#include "grammar.tab.h"
#include "scanner.h"
#include "dbglog.h"
#include <string.h>
#include <malloc.h>

#define MAXBUFLEN 1024 //TODO get rid of this sort of thing

#define new(nodetype) constr_##( ( nodetype * ) malloc ((size_t) sizeof(nodetype)))
		      

/* pretty printer fun decls. Farm these out to own files eventually */


void prettyPrintSelectList(selectListNode *);
void prettyPrintSelectNode(selectStmtNode *);
void prettyPrintParseTree(queryNode *);
void prettyPrintSexpr(scalarExpr *, int depth);

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
	return(get_select_list1(query->query_stmt.selnode));
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
  queryNode *qry = malloc(sizeof(queryNode));
  yyscan_t scanner;

  yylex_init(&scanner);
  buf = yy_scan_string(queryText, scanner);
  yyparse(scanner, qry);
  yy_delete_buffer(buf, scanner);
  yylex_destroy(scanner);
//    prettyPrintParseTree(qry);
  return (qry);
}

void gv_Sexpr(scalarExpr * sExp, int depth)
{

  /* scalar expressions are tree-like structures. 
     Basic recursive tree traversal algorithm here.
   */
  valueExprNode v;
  char *oper[] = { "/", "*", "+", "-", "%", ">", "<", ">=", "<=", "OR", "AND", "NOT", "=", "!=" };      //****TODO fix this nonsense!

  //print three dashes, then the value of this sexpr

  printf("---");

  switch (sExp->value.type) {

    case UNDEFINED:
      printf("[?:undefined]");
      break;
    case COLREF:
      printf("[%s:colref]", sExp->value.value.column_val->colName);
      break;
    case TEXT:
      printf("[%s:text]", sExp->value.value.text_val);
      break;
    case INT:
      printf("[%d:integer]", sExp->value.value.integer_val);
      break;
    case NUM:
      printf("[%f:numeric]", sExp->value.value.numeric_val);
      break;
    case OPER:
      printf("[%s:operator]", *(oper + sExp->value.value.oper_val));
      break;
    case SEXPR:
      printf("[SEXPR]");
      break;
  }

  if (sExp->left != NULL) {
    printf("\n");
    drawbranch(depth + 1, ' ');
    printf("|\n");
    drawbranch(depth + 1, ' ');
    printf("+");
    prettyPrintSexpr(sExp->left, depth + 1);
  }

  if (sExp->right != NULL) {
    printf("\n");
    drawbranch(depth + 1, ' ');
    printf("|\n");
    drawbranch(depth + 1, ' ');
    printf("\\");
    prettyPrintSexpr(sExp->right, depth + 1);

  }



}


