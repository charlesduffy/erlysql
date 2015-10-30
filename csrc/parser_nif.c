#include "erl_nif.h"
#include "grammar.tab.h"
#include "scanner.h"
#include <string.h>
#include <malloc.h>

#define MAXBUFLEN 1024

#define TREESEP() (printf("\t|\n\t+-"))

/* Node to Erlang NIF term converters */

static ERL_NIF_TERM nodeToNifTerm(ErlNifEnv *, queryNode *);
static ERL_NIF_TERM sExprToNifTerm (ErlNifEnv *, scalarExpr *) ;



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
    ERL_NIF_TERM erlParseTree;

    //consider static allocation here
    //determine better way to get the length of the query string at runtime
    //determine proper error handling method on malloc failure

   (void) memset (queryText, '\0', sizeof(queryText));

    if (enif_get_string(env, argv[0], queryText, MAXBUFLEN, ERL_NIF_LATIN1) < 1) {
	return enif_make_badarg(env);
    }
    ret = parseQuery(queryText);

    erlParseTree = nodeToNifTerm(env, ret);	

/*
    return enif_make_tuple(env, 2,
		enif_make_atom(env, "hello"),
		enif_make_atom(env, "goodbye" ));
	*/

   printf("returning from parseQuery_nif\n");

   return(erlParseTree);
}

static ERL_NIF_TERM nodeToNifTerm(ErlNifEnv *env, queryNode *qry) {


	selectListNode *sellist = qry->selnode->selectList;
	ERL_NIF_TERM nifList;
	ERL_NIF_TERM nifItem;
	/* iterate over the array of pointers-to-sExpr 
	   and print each one */	

	printf("Select list: %d elements\n",sellist->nElements);
	
	int i;	
	scalarExpr *sExpr ;
	for (i=0; i < sellist->nElements; i++) {
		sExpr = *(sellist->sExpr+i);
//		prettyPrintSexpr(sExpr);
		nifItem = sExprToNifTerm(env, sExpr);
		nifList = enif_make_list_cell( env, nifItem , nifList );
		printf("+++++\n\r");
		//sExpr++;
	}

printf("returning from nodeToNifTerm\n");

	return nifList;
    

}

static ERL_NIF_TERM sExprToNifTerm (ErlNifEnv *env , scalarExpr *sExpr) {
	/* traverse the scalarExpr and produce nested Erlang tuple 
	   representation of it 

	Eg:
	
	( 1 + ( foo * 4))

	{ 1 , + , { * , foo , 4 }}

	algo:
 		(if an oper, then we have a complex sExpr. 
		 if not, it'll be relatively simple.)
	1. if left node not null, recurse call self with left
	2. keep going left...
	3. end-condition is left isnull
	
					 


	*/

	//print current node val
	

/*

	each call returns either
		a literal / colref
		an oper
		a tuple

	1. generalise all as ETERM ? ie, functions always call the appropriate 'erl_mk' and return ETERM 

*/
	ERL_NIF_TERM lNode,rNode,cNode;
	//is current node oper ?

	if (sExpr->value.type != OPER) {
		printf("literal\n\r");
		printf("returning\n\t");
		//replace with table-driven method + fn pointers
		if (sExpr->value.type == INT) {

			printf (" %d\n\r", sExpr->value.value.integer_val);
			cNode = enif_make_int(env, sExpr->value.value.integer_val);
			return(cNode);
			}
		else if (sExpr->value.type == COLREF || TEXT) {
			 printf (" %s\n\r", sExpr->value.value.colName);
			 cNode = enif_make_atom(env, sExpr->value.value.colName);
			 return(cNode);
			}
	} 
	
	if (sExpr->value.type == OPER) {
		printf("oper\n\r%d\n\r", sExpr->value.value.oper_val);
	}	
	
	if (sExpr->left != NULL) {
		printf ("going left\n\r");	
		lNode = sExprToNifTerm(env, sExpr->left);	
	}
	
	if (sExpr->right != NULL) {
		printf ("going right\n\r");	
		rNode = sExprToNifTerm(env, sExpr->right);	
	}

	printf("enf returning\n\r");	
	cNode = enif_make_tuple( env, 3 , enif_make_int(env, sExpr->value.value.oper_val) , lNode , rNode );
}

static ErlNifFunc nif_funcs[] = {
    {"foo", 1, foo_nif},
    {"parseQuery", 1, parseQuery_nif}
};

ERL_NIF_INIT(parser, nif_funcs, NULL, NULL, NULL, NULL)

int foo (int x) {
	return 100;
}
