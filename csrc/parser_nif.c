#include "erl_nif.h"
#include "grammar.tab.h"
#include "scanner.h"
#include <string.h>
#include <malloc.h>

#define MAXBUFLEN 1024

int foo(int x);
queryNode * parseQuery (char *queryText);

static ERL_NIF_TERM foo_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int x, ret;
    if (!enif_get_int(env, argv[0], &x)) {
	return enif_make_badarg(env);
    }
    ret = foo(x);
    return enif_make_int(env, ret);
}

static ERL_NIF_TERM parseQuery_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    queryNode * ret;
    char queryText[MAXBUFLEN];

    //consider static allocation here
    //determine better way to get the length of the query string at runtime
    //determine proper error handling method on malloc failure

   (void) memset (queryText, '\0', sizeof(queryText));

    if (enif_get_string(env, argv[0], queryText, MAXBUFLEN, ERL_NIF_LATIN1) < 1) {
	return enif_make_badarg(env);
    }
    ret = parseQuery(queryText);
    return enif_make_tuple(env, 2,
		enif_make_atom(env, "hello"),
		enif_make_atom(env, "goodbye" ));
}

static ErlNifFunc nif_funcs[] = {
    {"foo", 1, foo_nif},
    {"parseQuery", 1, parseQuery_nif}
};

ERL_NIF_INIT(parser, nif_funcs, NULL, NULL, NULL, NULL)

int foo (int x) {
	return 100;
}

queryNode * parseQuery (char *queryText) {
    YY_BUFFER_STATE buf;
    queryNode *qry;
    yyscan_t scanner;

    yylex_init(&scanner);
    buf = yy_scan_string(queryText, scanner); 
    yyparse(scanner);
    yy_delete_buffer(buf, scanner);
    yylex_destroy(scanner);
    return (qry);
}
