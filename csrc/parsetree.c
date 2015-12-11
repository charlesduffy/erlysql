#include "grammar.tab.h"
#include "scanner.h"
#include "dbglog.h"
#include <string.h>
#include <malloc.h>

#define MAXBUFLEN 1024 //TODO get rid of this sort of thing

#define new(nodetype) new_##nodetype ( ( nodetype * ) malloc ((size_t) sizeof(nodetype)))

/* constructor function definitiions */

queryNode * new_queryNode ( queryNode *); 
		      
/* TODO put these in a header file */

selectListNode * get_select_list1 (selectStmtNode *);
selectListNode * get_select_list0 (queryNode *);

//void gv_SelectList(selectListNode *);
//void gv_SelectNode(selectStmtNode *);
//void gv_ParseTree(queryNode *);
void gv_Sexpr(scalarExpr *, int depth);
/* other fun decl */


// Accessor functions

selectListNode * get_select_list1 (selectStmtNode *selectStmt) {
	return(selectStmt->selectList);
}

selectListNode * get_select_list0 (queryNode *query) {
	return(get_select_list1(query->selnode));
}

// Constructor functions

queryNode * new_queryNode ( queryNode *node) {
//initialise query Node
	node->get_select_list = get_select_list0;
	
}

// Destructor functions




char *preprocQuery(char *queryText)
{
/* placeholder for proposed query string preprocessor

might strip whitespace, find basic problems (corruption, too long, too short etc).

 */

  return (queryText);
}

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

