#include <string.h>
#include <malloc.h>
#include <stdbool.h>
#include "erl_nif.h"
#include "grammar.tab.h"
#include "scanner.h"
#include "dbglog.h"
#include "structures.h"

#define MAXBUFLEN 1024

static ERL_NIF_TERM process_tuplist2(tuple *t, ErlNifEnv *env, const ERL_NIF_TERM n);
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

  //consider static allocation here
  //determine better way to get the length of the query string at runtime
  //determine proper error handling method on malloc failure

  (void) memset(queryText, '\0', sizeof(queryText));

  //consider using the "load" params to create cross-call memory buffer  
    

  if (enif_get_string(env, argv[0], queryText, MAXBUFLEN, ERL_NIF_LATIN1) < 1) {
    return enif_make_badarg(env);
  }

  parseTree = goparse(queryText);

  erlParseTree2 = process_tuplist2(parseTree, env, erlParseTree);

  return (erlParseTree2);
}

static ERL_NIF_TERM tup_to_nif_term(tuple *t, ErlNifEnv *env) {

//    if (t->list.prev == NULL ) { 
//     x = enif_make_list(env, (unsigned int) 0);
//     n = enif_make_list_cell(env, x, n); 
//    }
printf("inside tup to nif\n");

    switch(t->type) {
	case v_int: 
		    printf("v_int:{%s:%d} ", t->tag, t->v_int);
		    return( enif_make_tuple2(env, 
			    enif_make_atom(env, t->tag),
			    enif_make_int(env, t->v_int )));
		    break;
	case v_text: 
		    printf("v_text:{%s:%s} ", t->tag, t->v_text);
		    return(enif_make_tuple2(env, 
			    enif_make_atom(env, t->tag),
			    enif_make_string(env, t->v_text, ERL_NIF_LATIN1 )));
		    break; 
	case v_float:
		    printf("v_float:{%s:%f} ", t->tag, t->v_float);
		    return(enif_make_tuple2(env, 
			    enif_make_atom(env, t->tag),
			    enif_make_double(env, t->v_float )));
		    break; 
	case v_tuple:
		    printf("v_tuple:{%s:tuple} ", t->tag);
		    return(enif_make_tuple2(env, 
			    enif_make_atom(env, t->tag),
			    //process_tuplist2(t->v_tuple, env));
			    enif_make_atom(env, "hello")));
		    break; 
	case v_sexpr: 
		    printf("v_sexpr:{%s:sexpr} ", t->tag);
		    break; 
   }
}
//--process_tuplist(p, fn)
//- for a list of tuples, recursively process each element. Apply fn to each element

//--process_sexpr(p, fn)
//- for an s_expr, recursively process the tree, applying fn to each element

static ERL_NIF_TERM process_tuplist2(tuple *t, ErlNifEnv *env, ERL_NIF_TERM n) {

    ERL_NIF_TERM x;
    tuple *d;

    printf("entering process_tuplist2\n");

    /*

	n is current list

	is head of list? 
	    make new list.
	    append new list to n
	    new list is 'current list'

	

    */


    x = enif_make_list(env, (unsigned int) 0);
    list_foreach(t, tuple, d) {
	printf("call tup to nif\n");
	x = enif_make_list_cell(env, 
			    tup_to_nif_term(d,env), 
			    x);
    }

/*
     x = enif_make_list(env, (unsigned int) 0);
	x = process_tuplist2(tuplist_next(t), env, n);
	n = enif_make_list_cell(env, x, n);
*/

   return(x); 

}

