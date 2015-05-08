#include "erl_nif.h"

int foo(int x);
int bar(int y);

static ERL_NIF_TERM foo_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int x, ret;
    if (!enif_get_int(env, argv[0], &x)) {
	return enif_make_badarg(env);
    }
    ret = foo(x);
    return enif_make_int(env, ret);
}

static ERL_NIF_TERM bar_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int y, ret;
    char *queryString = "hello";
    if (!enif_get_int(env, argv[0], &y)) {
	return enif_make_badarg(env);
    }
    ret = bar(y);

    yy_scan_string(queryString);

    return enif_make_tuple1(env,
		enif_make_int(env, ret) );
}

static ErlNifFunc nif_funcs[] = {
    {"foo", 1, foo_nif},
    {"bar", 1, bar_nif}
};

ERL_NIF_INIT(parser, nif_funcs, NULL, NULL, NULL, NULL)

int foo (int x) {
	return 100;
}

int bar (int y) {
	return y*2;
}
