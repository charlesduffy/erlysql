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
    queryNode * qryTree;
    char queryText[MAXBUFLEN];
    ERL_NIF_TERM erlParseTree;

    //consider static allocation here
    //determine better way to get the length of the query string at runtime
    //determine proper error handling method on malloc failure

   (void) memset (queryText, '\0', sizeof(queryText));

    if (enif_get_string(env, argv[0], queryText, MAXBUFLEN, ERL_NIF_LATIN1) < 1) {
	return enif_make_badarg(env);
    }

    qryTree = parseQuery(queryText);

    debug("returned from parseQuery");
    
    erlParseTree = nodeToNifTerm(env, qryTree);	
    
    debug("returning from parseQuery_nif\n");

    //free(qryTree); //****TODO - this will cause a memory leak. Not a deep free. For testing only!

   return(erlParseTree);
   // return (enif_make_int (env, 1));
}

static ERL_NIF_TERM nodeToNifTerm(ErlNifEnv *env, queryNode *qry) {


	selectListNode *sellist = qry->selnode->selectList;
	fromClauseNode *fromclause = qry->selnode->fromClause;
	whereClauseNode *whereclause = qry->selnode->whereClause;
	ERL_NIF_TERM nifSelectList, nifFromClause, nifWhereClause;
	ERL_NIF_TERM nifItem;
	ERL_NIF_TERM nifMap;
	/* iterate over the array of pointers-to-sExpr 
	   and print each one */	

debug("Select list: %d elements\n",sellist->nElements);

	nifSelectList = enif_make_list(env, (unsigned int) 0);
	
	int i;	
	scalarExpr *sExpr ;
	for (i=0; i < sellist->nElements; i++) {
		sExpr = *(sellist->sExpr+i);
		nifItem = sExprToNifTerm(env, sExpr);
		nifSelectList = enif_make_list_cell( env, nifItem , nifSelectList );
	}

	debug("making enif string for from clause >>%s<< ", fromclause->item->tableName);
	//from clause
	nifFromClause = enif_make_string (env , (const char *) fromclause->item->tableName, ERL_NIF_LATIN1);
	//nifFromClause = enif_make_string (env , (const char *) "hello hello" , ERL_NIF_LATIN1);
	//where clause	
	debug("starting where clause");
	sExpr = whereclause->expr;
	debug("calling sExprtoNifTerm for Where clause");
 	nifWhereClause = sExprToNifTerm(env, sExpr);			

	//make map object
	nifMap = enif_make_new_map(env);

	if ( enif_make_map_put ( env , nifMap , 
					enif_make_atom ( env , (const char *) "select_list") ,
					nifSelectList , &nifMap ) != 0) { 

	debug("make map failed");

	}


	enif_make_map_put (env, nifMap, enif_make_atom (env , (const char *) "from_clause") , nifFromClause, &nifMap);
	enif_make_map_put (env, nifMap, enif_make_atom (env , (const char *) "where_clause") , nifWhereClause, &nifMap);
	
	
	return (nifMap);
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
	
	debug("Entering sExprToNifTerm");
	
	if (sExpr->value.type != OPER) {
		debug("node is NOT oper");
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
	cNode = enif_make_tuple3(env, enif_make_int(env, sExpr->value.value.oper_val), lNode, rNode);

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
	    cNode = (ERL_NIF_TERM) NULL;
	} 		

	return(cNode);	
}



