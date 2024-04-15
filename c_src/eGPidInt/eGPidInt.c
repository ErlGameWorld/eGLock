#include "erl_nif.h"

static ERL_NIF_TERM pidToInt(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifUInt64 TermInt = (ErlNifUInt64)argv[0];
    return enif_make_uint64(env, TermInt);
}

static ERL_NIF_TERM intToPid(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifUInt64 Pid;
    if (!enif_get_uint64(env, argv[0], &Pid))
        return enif_make_badarg(env);
    return (ERL_NIF_TERM)Pid;
}

static ErlNifFunc nif_funcs[] = {
    {"pidToInt", 1, pidToInt},
    {"intToPid", 1, intToPid}
};

ERL_NIF_INIT(eGPidInt, nif_funcs, NULL, NULL, NULL, NULL);