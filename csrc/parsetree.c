#include "grammar.tab.h"
#include "scanner.h"
#include "dbglog.h"
#include <string.h>
#include <malloc.h>

#define MAXBUFLEN 1024
#define BRLEN 3
#define LEADSET "\n   |\n   +"



/* pretty printer fun decls. Farm these out to own files eventually */


void prettyPrintSelectList(selectListNode *);
void prettyPrintSelectNode(selectStmtNode *);
void prettyPrintParseTree(queryNode *);
void prettyPrintSexpr(scalarExpr *, int depth);


//void gv_SelectList(selectListNode *);
//void gv_SelectNode(selectStmtNode *);
//void gv_ParseTree(queryNode *);
void gv_Sexpr(scalarExpr *, int depth);
/* other fun decl */

void drawbranch(int depth, char c)
{

  int i;

  i = depth * BRLEN;

  printf("depth: %d", depth);

  while (i-- > 1) {
    if (i % BRLEN == 0)
      putc('|', stdout);
    else
      putc(c, stdout);
  }
}


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


void prettyPrintSexpr(scalarExpr * sExp, int depth)
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

void prettyPrintSelectList(selectListNode * sellist)
{

  /* iterate over the array of pointers-to-sExpr 
     and print each one */

  debug("Select list: %d elements\n", sellist->nElements);

  int i;
  char c;
  scalarExpr *sExpr;
  for (i = 0; i < sellist->nElements; i++) {
    //printf("i is: %d\n", i);
    if (i == sellist->nElements - 1) {
      c = '\\';
    } else {
      c = '+';
    }
    printf("\n");
    drawbranch(1, ' ');
    printf("|\n");
    drawbranch(1, ' ');
    printf("%c", c);
    sExpr = *(sellist->sExpr + i);
    prettyPrintSexpr(sExpr, 1);
  }
}


void prettyPrintSelectNode(selectStmtNode * selnode)
{

  printf("SELECT");
  //traverse select list and print
  prettyPrintSelectList(selnode->selectList);
  //


}

void prettyPrintParseTree(queryNode * qry)
{

  /* scan queryNode
     -- check query node type enum
     -- pass to select query handler 
   */
  //check node type
  printf("\n=====================\n\n");

  switch (qry->statType) {
    case SELECT_STMT:
      prettyPrintSelectNode(qry->query_stmt.selnode);
      break;
    case UPDATE_STMT:
    case DELETE_STMT:
    case INSERT_STMT:
      //not yet supported
      printf("Unsupported statement type\n");

  }

  printf("\n\n=====================\n\n");
}
