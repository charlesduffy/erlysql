#include "erl_nif.h"
#include "grammar.tab.h"
#include "scanner.h"
#include "dbglog.h"
#include <string.h>
#include <malloc.h>
#include <stdbool.h>

#define MAXBUFLEN 1024

/* Node to Erlang NIF term converters */

static ERL_NIF_TERM nodeToNifTerm(ErlNifEnv *, queryNode *);
static ERL_NIF_TERM sExprToNifTerm(ErlNifEnv *, scalarExpr *, int);
static ERL_NIF_TERM valueExprToNifTerm(ErlNifEnv *, valueExprNode);

/* External function to translate Oper codes to symbols */
char *operSyms[] = { "/", "*", "+", "-", "%", ">", "<", ">=", "<=", "OR", "AND", "NOT", "=", "!=" };      //****TODO fix this nonsense!

/* NIF functions callable from erlang */

static ERL_NIF_TERM parseQuery_nif(ErlNifEnv *, int, const ERL_NIF_TERM[]);

queryNode *parseQuery(char *queryText);

static ErlNifFunc nif_funcs[] = {
  {"parseQuery", 1, parseQuery_nif}
};

ERL_NIF_INIT(parser, nif_funcs, NULL, NULL, NULL, NULL);


static ERL_NIF_TERM parseQuery_nif(ErlNifEnv * env, int argc,
                                   const ERL_NIF_TERM argv[])
{
  queryNode *qryTree;
  char queryText[MAXBUFLEN];
  ERL_NIF_TERM erlParseTree;

  //consider static allocation here
  //determine better way to get the length of the query string at runtime
  //determine proper error handling method on malloc failure

  (void) memset(queryText, '\0', sizeof(queryText));

  if (enif_get_string(env, argv[0], queryText, MAXBUFLEN, ERL_NIF_LATIN1) < 1) {
    return enif_make_badarg(env);
  }

  qryTree = parseQuery(queryText);

  debug("returned from parseQuery");


  /* check for error cond */

  if (qryTree->errFlag == 1) {
	debug("parser returned error");
	return(enif_make_atom(env, (const char *) "error"));
  }

  erlParseTree = nodeToNifTerm(env, qryTree);

  debug("returning from parseQuery_nif\n");

  //free(qryTree); //****TODO - this will cause a memory leak. Not a deep free. For testing only!

  return (erlParseTree);
}


/* 
	Translate a QueryNode to an Erlang NIF term 

	TODO - document erlang term structure here
*/

static ERL_NIF_TERM nodeToNifTerm(ErlNifEnv * env, queryNode * qry)
{

 


 /* Declare pointers to QueryNode data structure elements */

  

//  selectListNode *sellist = qry->selnode->selectList;
  selectListNode *sellist = qry->get_select_list(qry);


  fromClauseNode *fromclause = qry->selnode->tableExpr->fromClause;
  tableRefNode **tableRef =
    qry->selnode->tableExpr->fromClause->refList->tables;
  whereClauseNode *whereclause = qry->selnode->tableExpr->whereClause;

  /* Declare Erlang NIF terms for use */

  ERL_NIF_TERM nifSelectList, nifFromClause, nifWhereClause;
  ERL_NIF_TERM nifItem, nifItem1;
  ERL_NIF_TERM nifMap;

  debug("Select list: %d elements\n", sellist->nElements);

  /* 
     Construct select list element of Erlang parse tree
   */	

  int i;
  scalarExpr *sExpr;
  selectListItemNode *sItem;

     //Iterate over SELECT list items in QueryNode and push them on to 
     //an Erlang list as S-expressions constructed from Erlang tuples

  nifSelectList = enif_make_list(env, (unsigned int) 0);

  //Replace all this with proper use of iterator functions
  for (i = sellist->nElements - 1; i >= 0; i--) {
    sItem = *(sellist->sItems + i);
    nifItem = sExprToNifTerm(env, sItem->sExpr, 0);
    nifSelectList = enif_make_list_cell(env, nifItem, nifSelectList);
  }

  /* 
     Construct list of table references from the FROM clause 
   */

  nifFromClause = enif_make_list(env, (unsigned int) 0);

  debug("Decoding from clause, nElements: %d", fromclause->refList->nElements);

  tableRefNode *tref;

  for (i = fromclause->refList->nElements - 1; i >= 0; i--) {

    tref = *(tableRef + i);

    debug("from clause table >>%s<< ", (tref->tableName));

    nifItem = enif_make_list1( 	     env, 
    				     enif_make_tuple2 ( env, 
						        enif_make_atom(env, (const char *) "name"), 
				     			enif_make_string(env, (const char *) tref->tableName, ERL_NIF_LATIN1)
						      )
				   );

    /* Add the table alias to the proplist if one is present */ 
    if (tref->tableAlias != NULL) 
    	nifItem = enif_make_list_cell(
					    env, 
      					    enif_make_tuple2 (env, 
								enif_make_atom(env, (const char *) "alias"),
        				    			enif_make_string(env, (const char *) tref->tableAlias, ERL_NIF_LATIN1)
							     ),
					    nifItem
					    );

    /* Add the proplist to the from clause list */

    nifFromClause = enif_make_list_cell(env, nifItem , nifFromClause );

  }

  /* Handle WHERE clause creation */

    if (whereclause != NULL) {
      sExpr = whereclause->expr;
      nifWhereClause = sExprToNifTerm(env, sExpr, 0);
    }

    nifMap = enif_make_new_map(env);

  /* Handle SELECT list creation */

  if (!enif_make_map_put(env,
                         nifMap,
                         enif_make_atom(env, (const char *) "select_list"),
                         nifSelectList, &nifMap)
    ) {

    debug("make map failed");
    //return error condition here. 

  }


  if (!enif_make_map_put(env,
                         nifMap,
                         enif_make_atom(env, (const char *) "from_clause"),
                         nifFromClause, &nifMap)
    ) {

    debug("make map failed");
    //return error condition here
  }

  if (whereclause != NULL)
    if (!enif_make_map_put(env,
                           nifMap,
                           enif_make_atom(env, (const char *) "where_clause"),
                           nifWhereClause, &nifMap)
      ) {

      debug("make map failed");

    }

  return (nifMap);
}

static ERL_NIF_TERM sExprToNifTerm(ErlNifEnv * env, scalarExpr * sExpr, int depth)
{
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

  ERL_NIF_TERM lNode, rNode, cNode;
  ERL_NIF_TERM cList; //TODO rename these variables

  debug("Entering sExprToNifTerm");

  if (sExpr->value.type != OPER) {
    debug("node is NOT oper");
    cNode = valueExprToNifTerm(env, sExpr->value);
    //if (depth == 0) 
      //cNode = enif_make_tuple1(env, cNode);
    return (cNode);
  }

  if (sExpr->left != NULL) {
    lNode = sExprToNifTerm(env, sExpr->left, depth++);
  }

  if (sExpr->right != NULL) {
    rNode = sExprToNifTerm(env, sExpr->right, depth++);
  }

  cList = enif_make_list2(
				env, 
				enif_make_tuple2( env, 
						  enif_make_atom(env, (const char *) "type"), 
						  enif_make_atom(env, (const char *) "operator")
						),
				enif_make_tuple2( env,
						  enif_make_atom( env, (const char *) "value"), 
						  enif_make_string( env, operSyms[sExpr->value.value.oper_val], ERL_NIF_LATIN1 )
						)
			);

  cNode = enif_make_tuple3(env, cList, lNode, rNode); 

  return (cNode);
}

ERL_NIF_TERM valueExprToNifTerm(ErlNifEnv * env, valueExprNode value)
{

  ERL_NIF_TERM nodeVal, nodeType;
  ERL_NIF_TERM valNode;
  ERL_NIF_TERM colRefTuple;     	/* Holds table alias value if present in a column reference */
  bool tableRefFlag = false;		/* Set to true if a table reference is present for a column reference */

  debug("entering valueExprToNifTerm");

  valueExpr v = value.value;

  //TODO consider replacing with table driven method
  switch (value.type) {

    case UNDEFINED:
      debug("undefined value in s expression");
      nodeVal = (ERL_NIF_TERM) NULL;
      nodeType = (ERL_NIF_TERM) NULL;
      break;
    case COLREF:
      debug("add Colref to tuple...");
      nodeType = enif_make_atom(env, (const char *) "colref");

      /*
	 Special case: for column references we also need to encode the table reference
	 in the proplist, if one is present. 
      */

      if (v.column_val->colReference != NULL) {
		tableRefFlag = true;
    	        colRefTuple = enif_make_tuple2(
					env, 
					enif_make_atom(env, (const char *) "reference"),
	        	 		enif_make_string(env, v.column_val->colReference, ERL_NIF_LATIN1)
					);
      } 
      nodeVal = enif_make_string(env, v.column_val->colName, ERL_NIF_LATIN1);
      break;
    case INT:
      debug("add integer to tuple...");
      nodeVal = enif_make_int(env, v.integer_val);
      nodeType = enif_make_atom(env, "int");
      break;
    case NUM:
      debug("add float to tuple...");
      nodeVal = enif_make_double(env, v.numeric_val);
      nodeType = enif_make_atom(env, "int");
      break;
    case TEXT:
      debug("add text to tuple...");
      nodeVal = enif_make_string(env, v.text_val, ERL_NIF_LATIN1);
      nodeType = enif_make_atom(env, "text");
      break;
    case OPER:
      debug("add oper to tuple. Should never get here.");
      nodeVal = (ERL_NIF_TERM) NULL;
      nodeType = enif_make_atom(env, "oper_bad");
      break;
    default:
      debug("unknown value type!");
      nodeVal = (ERL_NIF_TERM) NULL;
      //TODO - handle this situation properly!
  }


  valNode = enif_make_list2(
			    env,
  			    enif_make_tuple2(
    					      env, 
    					      enif_make_atom(env, (const char *) "type"),
       	                  		      nodeType
					    ),
 			     enif_make_tuple2(
					       env,	  
                         		       enif_make_atom(env, (const char *) "value"),
                         		       nodeVal
					     )
			  );
   
  /* If a table alias is present, we push it on to the proplist */
 
  if (tableRefFlag == true)  
	valNode = enif_make_list_cell(env, colRefTuple , valNode );

  return (valNode);
}
