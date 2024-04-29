#include "erl_nif.h"
#include <atomic>
using namespace std;

const int LockSize = 2097152;
const int HashSalt = 786234121;
atomic<uint64_t> LockSlot[LockSize];

ERL_NIF_TERM atomTrue;
ERL_NIF_TERM atomFalse;
ERL_NIF_TERM atomUndefined;

typedef struct KeyNode_r{
    int KeyIx;
    struct KeyNode_r *next;
} KeyNode;

bool isNotCurLocked(KeyNode *LockedHead, int KeyIx){
    KeyNode *temp = LockedHead;
    while (temp != NULL){
        if (temp->KeyIx == KeyIx)
            return false;
        temp = temp->next;
    }
    return true;
}

int nifLoad(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM){
    atomTrue = enif_make_atom(env, "true");
    atomFalse = enif_make_atom(env, "false");
    atomUndefined = enif_make_atom(env, "undefined");
    return 0;
}

bool lockOne(ErlNifEnv *env, ErlNifPid *ThePid, int KeyIx, uint64_t Val){
    uint64_t Expected = 0;
    if (LockSlot[KeyIx].compare_exchange_strong(Expected, Val)){
        return true;
    }else{
        ThePid->pid = (ERL_NIF_TERM)Expected;
        if (enif_is_process_alive(env, ThePid)){
            return false;
        }else{
            if (LockSlot[KeyIx].compare_exchange_strong(Expected, Val)){
                return true;
            }else{
                return false;
            }
        }
    }
}

ERL_NIF_TERM tryLock(ErlNifEnv *env, int, const ERL_NIF_TERM argv[]){
    if (enif_is_list(env, argv[0])){
        ERL_NIF_TERM allList = argv[0];
        ERL_NIF_TERM head;
        ErlNifPid ThePid;
        enif_self(env, &ThePid);
        uint64_t Val = (uint64_t)(ThePid.pid);
        int KeyIx;
        KeyNode *LockedHead = NULL;
        while (enif_get_list_cell(env, allList, &head, &allList)){
            KeyIx = enif_hash(ERL_NIF_INTERNAL_HASH, head, HashSalt) % LockSize;
            KeyNode OneKeyNode = {KeyIx, LockedHead};
            if (isNotCurLocked(LockedHead, KeyIx)){
                if (lockOne(env, &ThePid, KeyIx, Val)){
                    LockedHead = &OneKeyNode;
                }else{
                    uint64_t RExpected;
                    KeyNode *temp = LockedHead;
                    while (temp != NULL){
                        RExpected = Val;
                        LockSlot[temp->KeyIx].compare_exchange_strong(RExpected, 0);
                        temp = temp->next;
                    }
                }
            }
        }
        return atomTrue;
    }else{
        int KeyIx;
        KeyIx = enif_hash(ERL_NIF_INTERNAL_HASH, argv[0], HashSalt) % LockSize;
        ErlNifPid ThePid;
        enif_self(env, &ThePid);
        uint64_t Val = (uint64_t)(ThePid.pid);

        if (lockOne(env, &ThePid, KeyIx, Val)){
            return atomTrue;
        }else{
            return atomFalse;
        }
    }
}

ERL_NIF_TERM releaseLock(ErlNifEnv *env, int, const ERL_NIF_TERM argv[]){
    if (enif_is_list(env, argv[0])){
        ERL_NIF_TERM allList = argv[0];
        ERL_NIF_TERM head;
        ErlNifPid ThePid;
        enif_self(env, &ThePid);
        uint64_t Expected = (uint64_t)(ThePid.pid);
        uint64_t RExpected;
        int KeyIx;
        int isAllOk = 1;

        while (enif_get_list_cell(env, allList, &head, &allList)){
            KeyIx = enif_hash(ERL_NIF_INTERNAL_HASH, head, HashSalt) % LockSize;
            RExpected = Expected;
            if (!LockSlot[KeyIx].compare_exchange_strong(RExpected, 0)){
                isAllOk = 0;
            }
        }
        return isAllOk > 0 ? atomTrue : atomFalse;
    }else{
        int KeyIx;
        KeyIx = enif_hash(ERL_NIF_INTERNAL_HASH, argv[0], HashSalt) % LockSize;
        ErlNifPid ThePid;
        enif_self(env, &ThePid);
        uint64_t Expected = (uint64_t)(ThePid.pid);

        if (LockSlot[KeyIx].compare_exchange_strong(Expected, 0)){
            return atomTrue;
        }else{
            return atomFalse;
        }
    }
}

ERL_NIF_TERM getLockPid(ErlNifEnv *env, int, const ERL_NIF_TERM argv[]){
    int KeyIx;
    KeyIx = enif_hash(ERL_NIF_INTERNAL_HASH, argv[0], HashSalt) % LockSize;
    ErlNifPid ThePid;

    uint64_t Var = LockSlot[KeyIx].load();
    if (Var > 0){
        ThePid.pid = (ERL_NIF_TERM)Var;
        return enif_make_pid(env, &ThePid);
    }else{
        return atomUndefined;
    }
}

static ErlNifFunc nifFuncs[] = {
    {"tryLock", 1, tryLock},
    {"releaseLock", 1, releaseLock},
    {"getLockPid", 1, getLockPid}
};

ERL_NIF_INIT(eNifLock, nifFuncs, &nifLoad, NULL, NULL, NULL)