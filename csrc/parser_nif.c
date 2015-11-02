#include "erl_nif.h"
#include "grammar.tab.h"
#include "scanner.h"
#include "dbglog.h"
#include <string.h>
#include <malloc.h>

#define MAXBUFLEN 1024

/* Node to Erlang NIF term converters */

static ERL_NIF_TERM nodeToNifTerm(ErlNifEnv *, queryNode *);
static ERL_NIF_TERM sExprToNifTerm (ErlNifEnv *, scalarExpr *) ;
static ERL_NIF_TERM valueExprToNifTerm(ErlNifEnv *, valueExprNode );

/* NIF function callable from erlang */


static ERL_NIF_TERM parseQuery_nif(ErlNifEnv* , int , const ERL_NIF_TERM[]);

queryNode * parseQuery (char *queryText);

static ErlNifFunc nif_funcs[] = {
    {"parseQuery", 1, parseQuery_nif}
};

ERL_NIF_INIT(parser, nif_funcs, NULL, NULL, NULL, NULL);


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
debug("returned from parseQuery");
    erlParseTree = nodeToNifTerm(env, ret);	

/*
   erlParseTree = enif_make_tuple2(env, 
	enif_make_int(env, 222),
	enif_make_int(env, 333)
	);
*/
   debug("returning from parseQuery_nif\n");

   return(erlParseTree);
}

static ERL_NIF_TERM nodeToNifTerm(ErlNifEnv *env, queryNode *qry) {


	selectListNode *sellist = qry->selnode->selectList;
	ERL_NIF_TERM nifList;
	ERL_NIF_TERM nifItem;
	/* iterate over the array of pointers-to-sExpr 
	   and print each one */	

	debug("Select list: %d elements\n",sellist->nElements);
	
	int i;	
	scalarExpr *sExpr ;
	for (i=0; i < sellist->nElements; i++) {
		sExpr = *(sellist->sExpr+i);
//		prettyPrintSexpr(sExpr);
		nifItem = sExprToNifTerm(env, sExpr);
		nifList = enif_make_list_cell( env, nifItem , nifList );
		debug("+++++\n\r");
		//sExpr++;
	}

debug("returning from nodeToNifTerm");

	return nifList;
}

static ERL_NIF_TERM sExprToNifTerm (ErlNifEnv *env , scalarExpr *sExpr) {
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


	ERL_NIF_TERM lNode,rNode,cNode;
	//is current node oper ?


   cNode = enif_make_tuple2(env, 
	enif_make_int(env, 222),
	enif_make_int(env, 333)
	);

/*

			cNode = enif_make_int(env, sExpr->value.value.integer_val);
			 cNode = enif_make_atom(env, sExpr->value.value.colName);
	
*/
	debug("Entering sExprToNifTerm");
	
	if (sExpr->value.type != OPER) {
		cNode = valueExprToNifTerm(env, sExpr->value);
		return (cNode);
	}

	if (sExpr->left != NULL) {
	  lNode = sExprToNifTerm (env , sExpr->left);
	}
	
	if (sExpr->right != NULL) {
	  rNode = sExprToNifTerm (env , sExpr->right);
	}

	//make my cNode
	cNode = enif_make_tuple2(env, lNode, rNode);

	return (cNode);

}

ERL_NIF_TERM valueExprToNifTerm(ErlNifEnv *env, valueExprNode value) {
//***TODO consider adding tags with SQL datatype to tuple	

	ERL_NIF_TERM cNode;
debug("entering valueExprToNifTerm");
	
	valueExpr v = value.value;

	switch (value.type) {

   	 case UNDEFINED:
	    debug("undefined value in s expression");
	    cNode = (ERL_NIF_TERM) NULL;
	    break;
	 case COLREF:
	    debug("add Colref to tuple...");
	    //consider making this text
	    cNode = enif_make_atom(env, v.colName);
	    break;
	 case INT:
	    debug("add integer to tuple...");
	    cNode = enif_make_int(env, v.integer_val);
	    break;
	 case NUM:
	    debug("add float to tuple...");
	    cNode = enif_make_double(env, v.numeric_val);
	    break;
	 case TEXT:
	    debug("add text to tuple...");
	    cNode = enif_make_string(env, v.text_val, ERL_NIF_LATIN1);
	    break;
	 case OPER:
	    debug("add oper to tuple. Should never get here.");
	    cNode = (ERL_NIF_TERM) NULL;
            break;
	 case SEXPR:  		
	    debug("add SEXPR to tuple. Deprecated. To be removed");
	    cNode = (ERL_NIF_TERM) NULL;
	    break;
	 default:
	    debug("unknown value type!");
	    cNode = NULL;
	} 		

	return(cNode);	
}



