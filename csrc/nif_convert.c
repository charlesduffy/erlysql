#include <string.h>
#include <malloc.h>
#include <stdbool.h>
#include "erl_nif.h"
#include "grammar.tab.h"
#include "scanner.h"
#include "dbglog.h"
#include "structures.h"

#define MAXBUFLEN 1024

//static ERL_NIF_TERM process_tuplist2(tuple *t, ErlNifEnv *env, const ERL_NIF_TERM n);
static ERL_NIF_TERM process_tuplist2(tuple *, ErlNifEnv *);
static ERL_NIF_TERM process_s_expr(s_expr *, ErlNifEnv *);
/* Node to Erlang NIF term converters */

/* NIF functions callable from erlang */

static ERL_NIF_TERM parseQuery_nif(ErlNifEnv *, int, const ERL_NIF_TERM[]);

static ErlNifFunc nif_funcs[] = {
  {"parseQuery", 1, parseQuery_nif}
};

ERL_NIF_INIT(parser, nif_funcs, NULL, NULL, NULL, NULL);


static ERL_NIF_TERM parseQuery_nif(ErlNifEnv * env, int argc,
                                   const ERL_NIF_TERM argv[])
{
  tuple *parseTree;
  char queryText[MAXBUFLEN];
  static ERL_NIF_TERM erlParseTree;
  YY_BUFFER_STATE buf;
  parseTree=malloc((size_t)sizeof(tuple));
  yyscan_t scanner;

  //consider static allocation here
  //determine better way to get the length of the query string at runtime
  //determine proper error handling method on malloc failure

  (void) memset(queryText, '\0', sizeof(queryText));

  //consider using the "load" params to create cross-call memory buffer  
    

  if (enif_get_string(env, argv[0], queryText, MAXBUFLEN, ERL_NIF_LATIN1) < 1) {
    return enif_make_badarg(env);
  }

  yylex_init(&scanner);
  buf = yy_scan_string(queryText, scanner);
  yyparse(scanner, parseTree);
  yy_delete_buffer(buf, scanner);
  yylex_destroy(scanner);

  erlParseTree = process_tuplist2(parseTree,env);

  return (erlParseTree);
}

//--process_tuplist(p, fn)
//- for a list of tuples, recursively process each element. Apply fn to each element

static ERL_NIF_TERM process_tuplist2(tuple *t, ErlNifEnv *env) {
    tuple *d;
    ERL_NIF_TERM z,x;

    x = enif_make_list(env, (unsigned int) 0);

    list_ff(t);
    
    list_foreach_r (t, tuple, d) {
	tuple_to_nif (z , d);
	x = enif_make_list_cell(env, z, x);
    }

    return(x);
}

//--process_sexpr(p, fn)
//- for an s_expr, recursively process the tree, applying fn to each element

static ERL_NIF_TERM process_s_expr(s_expr *s, ErlNifEnv *env) {
  /* traverse the scalarExpr and produce nested Erlang tuple 
     representation of it 
     ( 1 + ( foo * 4))
     { + , 1, { * , foo , 4 }}
     is oper?
     yes:is left null?
     yes:is right null?
     yes:return value
     no:recurse right
     no:recurse left
     no:return tuple of value
   */

  ERL_NIF_TERM left, right, value;
  
  if (s->left != NULL) 
    left = process_s_expr(s->left, env);
   else 
    left = enif_make_atom(env, "null");
  

  if (s->right != NULL) 
    right = process_s_expr(s->right, env);
   else 
    right = enif_make_atom(env, "null");

  //tuple_to_nif(value,s->value);
  value = process_tuplist2(s->value,env);

  return(enif_make_tuple3(env, value, left, right)); 

}
