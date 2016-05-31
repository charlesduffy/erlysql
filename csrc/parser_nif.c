#include <string.h>
#include <malloc.h>
#include <stdbool.h>
#include "erl_nif.h"
#include "grammar.tab.h"
#include "scanner.h"
#include "dbglog.h"
#include "structures.h"

#define MAXBUFLEN 1024

/* Node to Erlang NIF term converters */

static ERL_NIF_TERM parseTreeToNifTerm(ErlNifEnv * , multiQueryNode * ); 
static ERL_NIF_TERM queryNodeToNifTerm(ErlNifEnv *, queryNode *);
static ERL_NIF_TERM sExprToNifTerm(ErlNifEnv *, scalarExpr *, int);
static ERL_NIF_TERM valueExprToNifTerm(ErlNifEnv *, valueExprNode);

/* External function to translate Oper codes to symbols */
char *operSyms[] = { "/", "*", "+", "-", "%", ">", "<", ">=", "<=", "OR", "AND", "NOT", "=", "!=" };      //****TODO fix this nonsense!

/* NIF functions callable from erlang */

static ERL_NIF_TERM parseQuery_nif(ErlNifEnv *, int, const ERL_NIF_TERM[]);

multiQueryNode * parseQuery(char *queryText);

static ErlNifFunc nif_funcs[] = {
  {"parseQuery", 1, parseQuery_nif}
};

ERL_NIF_INIT(parser, nif_funcs, NULL, NULL, NULL, NULL);


static ERL_NIF_TERM parseQuery_nif(ErlNifEnv * env, int argc,
                                   const ERL_NIF_TERM argv[])
{
  multiQueryNode *parseTree;
  char queryText[MAXBUFLEN];
  ERL_NIF_TERM erlParseTree;

  //consider static allocation here
  //determine better way to get the length of the query string at runtime
  //determine proper error handling method on malloc failure

  (void) memset(queryText, '\0', sizeof(queryText));

  if (enif_get_string(env, argv[0], queryText, MAXBUFLEN, ERL_NIF_LATIN1) < 1) {
    return enif_make_badarg(env);
  }

  parseTree = parseQuery(queryText);

  debug("returned from parseQuery");


  /* check for error cond */

  if (parseTree->errFlag == 1) {
	debug("parser returned error");
	return(enif_make_atom(env, (const char *) "error"));
  }

  erlParseTree = parseTreeToNifTerm(env, parseTree);

  return (erlParseTree);
}


/* 
	Translate a QueryNode to an Erlang NIF term 

	TODO - document erlang term structure here
*/

static ERL_NIF_TERM parseTreeToNifTerm(ErlNifEnv * env, multiQueryNode * parseTree) 
{



  ERL_NIF_TERM nifItem, nifQueryList;
  queryNode * query;

  nifQueryList = enif_make_list(env, (unsigned int) 0);

  list_foreach(parseTree->query, queryNode, query) {

    nifItem = enif_make_tuple2 (
				 env,
				 enif_make_atom(env, (const char *) "query"), 
				 queryNodeToNifTerm(env, query)
				);

/*

    for (llist *l = &parseTree->query->list; l !=NULL; l=l->next) {
	query = container_of(l, queryNode, list);	
        nifItem = enif_make_tuple2 (
				 env,
				 enif_make_atom(env, (const char *) "query"), 
				 queryNodeToNifTerm(env, query)
				);
    }
*/

    nifQueryList = enif_make_list_cell(env, nifItem, nifQueryList);
  }
  
  return(nifQueryList);
}

//--fn(selectStmtNode)-> [ select stmt ]
static ERL_NIF_TERM selectStmtNodeToNIFTerm(ErlNifEnv * env, selectStmtNode * node) {

}

//--fn(selectList) -> [ select list ]

static ERL_NIF_TERM selectListNodeToNIFTerm(ErlNifEnv * env, selectListNode * node) {

}

//--fn(selectListItem) -> [ select list item ]
static ERL_NIF_TERM selectListItemToNifTerm(ErlNifEnv * env, selectListItem * node) {

}


//--fn(fromClause) -> [ from clause list ]
static ERL_NIF_TERM fromClauseNodeToNIFTerm(ErlNifEnv * env, fromClauseNode * node) {

}

//--fn(tableRefNode) -> [ table expr ]
static ERL_NIF_TERM tableRefNodeToNIFTerm(ErlNifEnv * env, tableRefNode * node) {

}


//--fn(whereClauseNode) -> [ sexpr ]
static ERL_NIF_TERM whereClauseNodeToNIFTerm(ErlNifEnv * env, whereClauseNode * node) {

}

static ERL_NIF_TERM queryNodeToNifTerm(ErlNifEnv * env, queryNode * qry)
{

    /*

    - determine query type
    - call appropriate statment type conv func


    */


 /* Declare pointers to QueryNode data structure elements */

  selectListItemNode *selectList = get_select_list(qry);

  fromClauseNode *fromclause = get_from_clause(qry);

  tableRefNode **tableRef =
    qry->selnode->tableExpr->fromClause->refList->tables;

  whereClauseNode *whereclause = qry->selnode->tableExpr->whereClause;

  /* Declare Erlang NIF terms for use */

  ERL_NIF_TERM nifSelectList, nifFromClause, nifWhereClause;
  ERL_NIF_TERM nifItem, nifItem1;
  ERL_NIF_TERM nifMap;

  debug("Select list: %d elements\n", get_num_elements(selectList));

  /* 
     Construct select list element of Erlang parse tree
   */	

  int i;
  scalarExpr *sExpr;
  selectListItemNode *sItem;

  nifSelectList = enif_make_list(env, (unsigned int) 0);
  
  list_foreach(selectList, selectListItemNode, sItem) {
    nifItem = sExprToNifTerm(env, sItem->sExpr, 0);   //consider replacement with function like get_sexpr
    nifSelectList = enif_make_list_cell(env, nifItem, nifSelectList);
  }


  /* 
     Construct list of table references from the FROM clause 
   */

  nifFromClause = enif_make_list(env, (unsigned int) 0);

//  debug("Decoding from clause, Elements: %d", fromclause->refList->nElements);

  tableRefNode *tref;

//  for (i = fromclause->refList->nElements - 1; i >= 0; i--) {
  for (i = get_num_elements(fromclause->refList) - 1; i >= 0; i--) {

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
// TODO break these out to functions
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
printf("\n\rDepth is: %d\t ", depth);
  if (sExpr->value.type != OPER) {
    cNode = valueExprToNifTerm(env, sExpr->value);
    if (depth == 0) {
      /* This is to ensure that single-element s-expressions are returned properly wrapped in a tuple */
      cNode = enif_make_tuple1(env, cNode);	
    }
    return (cNode);
  }

  if (sExpr->left != NULL) {
    lNode = sExprToNifTerm(env, sExpr->left, depth+1);
  }

  if (sExpr->right != NULL) {
    rNode = sExprToNifTerm(env, sExpr->right, depth+1);
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
	printf("colref value: %s\n\r", v.column_val->colReference);

	fflush(stdout);
      /*
	 Special case: for column references we also need to encode the table reference
	 in the proplist, if one is present. 
      */

      if (v.column_val->colReference != NULL) {    //replace with some func like has_tabref
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
	printf("integer value: %d\n\r", v.integer_val);
	fflush(stdout);
      nodeVal = enif_make_int(env, v.integer_val);
      nodeType = enif_make_atom(env, "int");
      break;
    case NUM:
      debug("add float to tuple...");
      nodeVal = enif_make_double(env, v.numeric_val);
      nodeType = enif_make_atom(env, "int");
      break;
    case _TEXT:
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
