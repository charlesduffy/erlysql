#include "grammar.tab.h"
#include "scanner.h"
#include "dbglog.h"
#include "parsetree.h"
#include <string.h>
#include <malloc.h>
#include <stddef.h>
#include <stdbool.h>

#define MAXBUFLEN 1024 //TODO get rid of this sort of thing

/* get list sizes */


/* get number of elements in a list node */
#define get_num_elements(node) (node->listInfo->nElements)

/* new macro 

* if 'hasconstr' is defined for the type, call the appropriate constructor fn
* else, just call malloc() 

* or, we use some sort of preprocessing macro system to scan the source file,
  determine if constructors are defined for a given node type, 
  and define the appropriate macro


Can't put #ifdef inside a function-like macro, so thinking about this

*/

#define new(nodetype) new_##nodetype ( ( nodetype * ) malloc ((size_t) sizeof(nodetype)))


/* constructor function declarations */

queryNode * new_queryNode ( queryNode *); 
		      
/* TODO put these in a header file */

selectListNode * get_select_list1 (selectStmtNode *);
selectListNode * get_select_list0 (queryNode *);
tableRefNode ** get_table_list1 (selectStmtNode *); 
tableRefNode ** get_table_list0 (queryNode *); 

// Accessor functions

// TODO: consider dropping the below funcs / function-ptr concept

selectListNode * get_select_list1 (selectStmtNode *selectStmt) {
	return(selectStmt->selectList);
}

selectListNode * get_select_list0 (queryNode *query) {
	return(get_select_list1(query->selnode));
}

tableRefNode ** get_table_list0 (queryNode *query) {
	return(get_table_list1 (query->selnode));
}

tableRefNode ** get_table_list1 (selectStmtNode *selectStmt) {
	return(selectStmt->tableExpr->fromClause->refList->tables);
} 



// Constructor functions

queryNode * new_queryNode ( queryNode *node) {
//initialise query Node
	
	//initialse pointer-to-function
	node->get_select_list = get_select_list0;
	//
	node->errNode = NULL;
	node->errFlag = 0;
	return(node);
	
}

selectStmtNode * new_selectStmtNode ( selectStmtNode *node ) {
//initialise selectStmtNode 
	return(node);
}

// Destructor functions


// Test functions

bool sexpr_is_boolean( scalarExpr *sExpr) {

if (sExpr->value.type == OPER)
	if ( sExpr->value.value.oper_val == _AND ||
	     sExpr->value.value.oper_val == _OR  ||
	     sExpr->value.value.oper_val == _NOT ||
	     sExpr->value.value.oper_val == _BETWEEN ||
	     sExpr->value.value.oper_val == _NOT_BETWEEN ||
	     sExpr->value.value.oper_val == _IN ||
	     sExpr->value.value.oper_val == _NOT_IN ||
	     sExpr->value.value.oper_val == _LT ||
	     sExpr->value.value.oper_val == _GT ||
	     sExpr->value.value.oper_val == _EQ ||
	     sExpr->value.value.oper_val == _NE ||
	     sExpr->value.value.oper_val == _GTE ||
	     sExpr->value.value.oper_val == _LTE ) return (true);
else return false;

}


// miscellaneous functions

char *preprocQuery(char *queryText)
{
/* placeholder for proposed query string preprocessor

might strip whitespace, find basic problems (corruption, too long, too short etc).

 */

  return (queryText);
}


// parser entry point

queryNode *parseQuery(char *queryText)
{
  YY_BUFFER_STATE buf;
//  queryNode *qry = malloc(sizeof(queryNode));
  queryNode *qry = new(queryNode);

  yyscan_t scanner;

  yylex_init(&scanner);
  buf = yy_scan_string(queryText, scanner);
  yyparse(scanner, qry);
  yy_delete_buffer(buf, scanner);
  yylex_destroy(scanner);
//    prettyPrintParseTree(qry);
  return (qry);
}

