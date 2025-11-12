#include "erl_nif.h"
#include <atomic>
#include <type_traits>

// 编译期自检：确保ERL_NIF_TERM是可平凡复制的类型
static_assert(std::is_trivially_copyable<ERL_NIF_TERM>::value, "ERL_NIF_TERM must be trivially copyable");

const int LockSize = 2097152;
std::atomic<ERL_NIF_TERM> LockSlot[LockSize];

ERL_NIF_TERM atomTrue;
ERL_NIF_TERM atomFalse;
ERL_NIF_TERM atomUndefined;

int nifLoad(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info) {
    atomTrue = enif_make_atom(env, "true");
    atomFalse = enif_make_atom(env, "false");
    atomUndefined = enif_make_atom(env, "undefined");
    return 0;
}

inline bool lockOne(ErlNifEnv *env, ErlNifPid *ThePid, int KeyIx, ERL_NIF_TERM Val) {
    ERL_NIF_TERM Expected = 0;
    if (LockSlot[KeyIx].compare_exchange_strong(Expected, Val, std::memory_order_acquire, std::memory_order_relaxed)) {
        return true;
    } else {
        ThePid->pid = Expected;
        if (enif_is_process_alive(env, ThePid)) {
            return false;
        } else {
            return LockSlot[KeyIx].compare_exchange_strong(Expected, Val, std::memory_order_acquire, std::memory_order_relaxed);
        }
    }
}

ERL_NIF_TERM tryLock(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    int KeyIx;
    enif_get_int(env, argv[0], &KeyIx);
    ErlNifPid ThePid;
    enif_self(env, &ThePid);
    ERL_NIF_TERM Val = ThePid.pid;

    return lockOne(env, &ThePid, KeyIx, Val) ? atomTrue : atomFalse;
}

ERL_NIF_TERM tryLocks(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ERL_NIF_TERM allList = argv[0];
    ERL_NIF_TERM head;
    ErlNifPid ThePid;
    enif_self(env, &ThePid);
    const ERL_NIF_TERM Val = ThePid.pid;
    int KeyIx;
    int cnt = -1;

    while (enif_get_list_cell(env, allList, &head, &allList)) {
        enif_get_int(env, head, &KeyIx);
        if (lockOne(env, &ThePid, KeyIx, Val)) {
            cnt++;
        } else {
            allList = argv[0];
            ERL_NIF_TERM Expected;
            for (int i = 0; i <= cnt; i++) {
                enif_get_list_cell(env, allList, &head, &allList);
                enif_get_int(env, head, &KeyIx);
                Expected = Val;
                LockSlot[KeyIx].compare_exchange_strong(Expected, 0, std::memory_order_release, std::memory_order_relaxed);
            }
            return atomFalse;
        }
    }
    return atomTrue;
}

ERL_NIF_TERM releaseLock(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    int KeyIx;
    enif_get_int(env, argv[0], &KeyIx);
    ErlNifPid ThePid;
    enif_self(env, &ThePid);
    ERL_NIF_TERM Expected = ThePid.pid;
    return LockSlot[KeyIx].compare_exchange_strong(Expected, 0, std::memory_order_release, std::memory_order_relaxed) ? atomTrue : atomFalse;
}

ERL_NIF_TERM releaseLocks(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ERL_NIF_TERM allList = argv[0];
    ERL_NIF_TERM head;
    ErlNifPid ThePid;
    enif_self(env, &ThePid);
    const ERL_NIF_TERM Expected = ThePid.pid;
    ERL_NIF_TERM RExpected;
    int KeyIx;
    int isAllOk = 1;

    while (enif_get_list_cell(env, allList, &head, &allList)) {
        enif_get_int(env, head, &KeyIx);
        RExpected = Expected;
        if (!LockSlot[KeyIx].compare_exchange_strong(RExpected, 0, std::memory_order_release, std::memory_order_relaxed)) {
            isAllOk = 0;
        }
    }
    return isAllOk > 0 ? atomTrue : atomFalse;
}

ERL_NIF_TERM getLockPid(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    int KeyIx;
    enif_get_int(env, argv[0], &KeyIx);
    ErlNifPid ThePid;

    ERL_NIF_TERM Var = LockSlot[KeyIx].load(std::memory_order_acquire);
    if (Var != 0) {
        ThePid.pid = Var;
        return enif_make_pid(env, &ThePid);
    } else {
        return atomUndefined;
    }
}

static ErlNifFunc nifFuncs[] = {
    {"tryLock", 1, tryLock},
    {"tryLocks", 1, tryLocks},
    {"releaseLock", 1, releaseLock},
    {"releaseLocks", 1, releaseLocks},
    {"getLockPid", 1, getLockPid}
};

ERL_NIF_INIT(eNifLock, nifFuncs, &nifLoad, NULL, NULL, NULL)
