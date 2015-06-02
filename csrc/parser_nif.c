#include "erl_nif.h"
#include "grammar.tab.h"
#include "scanner.h"
#include <string.h>
#include <malloc.h>

#define MAXBUFLEN 1024

int foo(int x);
int parseQuery (char *queryText);

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
    int ret;
    char queryText[MAXBUFLEN];
//	yyscan_t scanner;

    //consider static allocation here
    //determine better way to get the length of the query string at runtime
    //determine proper error handling method on malloc failure
//   queryText = (char *) malloc ( MAXBUFLEN * sizeof(char));
/*
   if (queryText == (char *) NULL) {
	printf("bad malloc\n");
	return enif_make_badarg(env); 
   }
*/

   (void) memset (queryText, '\0', sizeof(queryText));

    if (enif_get_string(env, argv[0], queryText, MAXBUFLEN, ERL_NIF_LATIN1) < 1) {
	return enif_make_badarg(env);
    }
    ret = parseQuery(queryText);
    return enif_make_tuple1(env,
		enif_make_int(env, ret) );
}

static ErlNifFunc nif_funcs[] = {
    {"foo", 1, foo_nif},
    {"parseQuery", 1, parseQuery_nif}
};

ERL_NIF_INIT(parser, nif_funcs, NULL, NULL, NULL, NULL)

int foo (int x) {
	return 100;
}

int parseQuery (char *queryText) {
    YY_BUFFER_STATE buf;
    yyscan_t scanner;
    yylex_init(&scanner);
 //  yylex(scanner);
	

    buf = yy_scan_string(queryText, scanner); 
  //  yylex(scanner);
    yyparse();
    yy_delete_buffer(buf, scanner);
    yylex_destroy(scanner);
    return (9);
}
