#include "erl_nif.h"
#include "grammar.tab.h"
#include "scanner.h"
#include "dbglog.h"
#include <string.h>
#include <malloc.h>

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

  erlParseTree = nodeToNifTerm(env, qryTree);

  debug("returning from parseQuery_nif\n");

  //free(qryTree); //****TODO - this will cause a memory leak. Not a deep free. For testing only!

  return (erlParseTree);
}

static ERL_NIF_TERM nodeToNifTerm(ErlNifEnv * env, queryNode * qry)
{

  selectListNode *sellist = qry->selnode->selectList;
  fromClauseNode *fromclause = qry->selnode->tableExpr->fromClause;
  tableRefNode **tableRef =
    qry->selnode->tableExpr->fromClause->refList->tables;
  whereClauseNode *whereclause = qry->selnode->tableExpr->whereClause;
  ERL_NIF_TERM nifSelectList, nifFromClause, nifWhereClause;
  ERL_NIF_TERM nifItem, nifItem1;
  ERL_NIF_TERM nifMap;
  /* iterate over the array of pointers-to-sExpr 
     and print each one */

  debug("Select list: %d elements\n", sellist->nElements);

  nifSelectList = enif_make_list(env, (unsigned int) 0);

  int i;
  scalarExpr *sExpr;
  for (i = sellist->nElements - 1; i >= 0; i--) {
    sExpr = *(sellist->sExpr + i);
    nifItem = sExprToNifTerm(env, sExpr, 0);
    nifSelectList = enif_make_list_cell(env, nifItem, nifSelectList);
  }

  nifFromClause = enif_make_list(env, (unsigned int) 0);

  //from clause

  debug("Decoding from clause, nElements: %d", fromclause->refList->nElements);

  tableRefNode *tref;

  for (i = fromclause->refList->nElements - 1; i >= 0; i--) {

    tref = *(tableRef + i);

    debug("from clause table >>%s<< ", (tref->tableName));

    nifMap = enif_make_new_map(env);
    //TODO: include better checking here. 
    nifItem1 =
      enif_make_string(env, (const char *) tref->tableName, ERL_NIF_LATIN1);

    if (!enif_make_map_put
        (env, nifMap, enif_make_atom(env, (const char *) "name"), nifItem1,
         &nifMap)) {
      debug("make map failed");
      //handle error
    }

    if (tref->tableAlias != NULL) {

      nifItem =
        enif_make_string(env, (const char *) tref->tableAlias, ERL_NIF_LATIN1);

      if (enif_make_map_put
          (env, nifMap, enif_make_atom(env, (const char *) "alias"), nifItem,
           &nifMap)) {
        debug("make map failed");
        //handle error
      }
    }
    nifFromClause = enif_make_list_cell(env, nifMap, nifFromClause);
  }

  //where clause  

  if (whereclause != NULL) {
    sExpr = whereclause->expr;
    nifWhereClause = sExprToNifTerm(env, sExpr, 0);
  }

  nifMap = enif_make_new_map(env);

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
  ERL_NIF_TERM cMap;

  debug("Entering sExprToNifTerm");

  if (sExpr->value.type != OPER) {
    debug("node is NOT oper");
    cNode = valueExprToNifTerm(env, sExpr->value);
    if (depth == 0) 
      cNode = enif_make_tuple1(env, cNode);
    return (cNode);
  }

  if (sExpr->left != NULL) {
    lNode = sExprToNifTerm(env, sExpr->left, depth++);
  }

  if (sExpr->right != NULL) {
    rNode = sExprToNifTerm(env, sExpr->right, depth++);
  }
  //make my cNode
  cMap = enif_make_new_map(env);
  enif_make_map_put(env, cMap, enif_make_atom(env, (const char *) "type"),
                    enif_make_atom(env, (const char *) "OPER"), &cMap);
  enif_make_map_put(env, cMap, enif_make_atom(env, (const char *) "value"),
                    enif_make_string(env, operSyms[sExpr->value.value.oper_val], ERL_NIF_LATIN1 ), &cMap);
  cNode = enif_make_tuple3(env, cMap, lNode, rNode);    //modify to get the text value of oper

  return (cNode);
}

ERL_NIF_TERM valueExprToNifTerm(ErlNifEnv * env, valueExprNode value)
{

  ERL_NIF_TERM nodeVal, nodeType;
  ERL_NIF_TERM nodeMap;



  debug("entering valueExprToNifTerm");

  valueExpr v = value.value;
  nodeMap = enif_make_new_map(env);

  //consider replacing with table driven method
  switch (value.type) {

    case UNDEFINED:
      debug("undefined value in s expression");
      nodeVal = (ERL_NIF_TERM) NULL;
      nodeType = (ERL_NIF_TERM) NULL;
      break;
    case COLREF:
      debug("add Colref to tuple...");
      nodeType = enif_make_atom(env, (const char *) "COLREF");

      /*
	 Special case: for column references we also need to encode the table reference
	 in the map, if one is present. We re-use the nodeVal enif variable for this here. 
	 Consider a better solution.
      */
      if (v.column_val->colReference != NULL) {
      		nodeVal = enif_make_string(env, v.column_val->colReference, ERL_NIF_LATIN1);
    	        if (!enif_make_map_put(env,
                	         nodeMap,
                        	 enif_make_atom(env, (const char *) "reference"),
                	         nodeVal, &nodeMap)) {
        	debug("make map failed");
        	//return (ERL_NIF_TERM) NULL; 
      	}
      }
      nodeVal = enif_make_string(env, v.column_val->colName, ERL_NIF_LATIN1);
      break;
    case INT:
      debug("add integer to tuple...");
      nodeVal = enif_make_int(env, v.integer_val);
      nodeType = enif_make_atom(env, "INT");
      break;
    case NUM:
      debug("add float to tuple...");
      nodeVal = enif_make_double(env, v.numeric_val);
      nodeType = enif_make_atom(env, "NUM");
      break;
    case TEXT:
      debug("add text to tuple...");
      nodeVal = enif_make_string(env, v.text_val, ERL_NIF_LATIN1);
      nodeType = enif_make_atom(env, "TEXT");
      break;
    case OPER:
      debug("add oper to tuple. Should never get here.");
      nodeVal = (ERL_NIF_TERM) NULL;
      nodeType = enif_make_atom(env, "OPER");
      break;
    default:
      debug("unknown value type!");
      nodeVal = (ERL_NIF_TERM) NULL;
  }


  if (!enif_make_map_put(env,
                         nodeMap,
                         enif_make_atom(env, (const char *) "type"),
                         nodeType, &nodeMap)) {
    debug("make map failed");
    //  return (ERL_NIF_TERM) NULL; 
  }

  if (!enif_make_map_put(env,
                         nodeMap,
                         enif_make_atom(env, (const char *) "value"),
                         nodeVal, &nodeMap)) {
    debug("make map failed");
    //return (ERL_NIF_TERM) NULL; 
  }

  return (nodeMap);
}
