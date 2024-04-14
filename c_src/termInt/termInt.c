#include "erl_nif.h"
#include <stdio.h>

static ERL_NIF_TERM termInt(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifUInt64 TermInt = (ErlNifUInt64)argv[0];
    return enif_make_uint64(env, TermInt);
}
static ErlNifFunc nif_funcs[] = {
    {"termInt", 1, termInt}
};

ERL_NIF_INIT(termInt, nif_funcs, NULL, NULL, NULL, NULL);