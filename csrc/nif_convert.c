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
static ERL_NIF_TERM process_tuplist2(tuple *t, ErlNifEnv *env);
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
  ERL_NIF_TERM erlParseTree;
  static ERL_NIF_TERM erlParseTree2;
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

//  parseTree = goparse(queryText);

  yylex_init(&scanner);
  buf = yy_scan_string(queryText, scanner);
  yyparse(scanner, parseTree);
  //yy_delete_buffer(buf, scanner);
  //yylex_destroy(scanner);

  erlParseTree2 = process_tuplist2(parseTree,env);//, env, erlParseTree);

  return (erlParseTree2);
}

//--process_tuplist(p, fn)
//- for a list of tuples, recursively process each element. Apply fn to each element

//--process_sexpr(p, fn)
//- for an s_expr, recursively process the tree, applying fn to each element


/*
     x = enif_make_list(env, (unsigned int) 0);
	x = process_tuplist2(tuplist_next(t), env, n);
	n = enif_make_list_cell(env, x, n);
*/
static ERL_NIF_TERM process_tuplist2(tuple *t, ErlNifEnv *env) {
    tuple *d;
    ERL_NIF_TERM z,x;

    x = enif_make_list(env, (unsigned int) 0);

    list_ff(t);
    
    list_foreach_r (t, tuple, d) {

    //printf("list item!\n");

    switch(d->type) {
	case v_int: 
			//printf("v_int:{%s:%d} ", d->tag, d->v_int);	
			z = enif_make_tuple2(env, 
					    enif_make_atom(env, d->tag),
					    enif_make_int(env, d->v_int)
					);
			break;
	case v_text:	
			//printf("v_text:{%s:%s} ", d->tag, d->v_text);
			z = enif_make_tuple2(env, 
					    enif_make_atom(env, d->tag),
					    enif_make_string(env, d->v_text, ERL_NIF_LATIN1)
					);
			 break; 
	case v_float:
			//printf("v_float:{%s:%f} ", d->tag, d->v_float);
			z = enif_make_tuple2(env, 
					    enif_make_atom(env, d->tag),
					    enif_make_double(env, d->v_float )
					);
			break; 
	case v_tuple:
			//printf("v_tuple:{%s:tuple} ", d->tag);
			//z = process_tuplist2(d->v_tuple, env);
			z = enif_make_tuple2(env, 
					    enif_make_atom(env, d->tag),
					    process_tuplist2(d->v_tuple, env)
					);
			break; 
	case v_sexpr:   
			//printf("v_sexpr:{%s:sexpr} ", d->tag);
			z = enif_make_tuple2(env, 
					    enif_make_atom(env, d->tag),
					    enif_make_atom(env, "s-expression")
					);
			break; 
   }
    
    x = enif_make_list_cell(env, z, x);
}

    return(x);

}

