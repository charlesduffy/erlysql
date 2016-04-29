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

if (sExpr->value.type == OPER) {
	 switch(sExpr->value.value.oper_val) {
	     case   _AND:
	     case   _OR:
	     case   _NOT:
	     case   _BETWEEN:
	     case   _NOT_BETWEEN:
	     case   _IN:
	     case   _NOT_IN:
	     case   _LT:
	     case   _GT:
	     case   _EQ:
	     case   _NE:
	     case   _GTE:
	     case   _LTE: 
		printf("Sexpr is BOOLEAN");
		return (true);
		break;
	     default: 
	
		printf("Sexpr is NOT BOOLEAN");
		return(false);
	} 
		printf("Sexpr is NOT BOOLEAN");
}
else  {
	return false;
}

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

