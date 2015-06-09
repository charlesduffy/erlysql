#include <stdio.h>
#include <malloc.h>
#include <stdlib.h>
#include "grammar.tab.h"
#include "simtree.h"

/* 

Node types
==========
sql query_statement select_statement scalar_expr value_expr from_clause



*/

/* SELECT statment nodes */


selectStmtNode * mkSelectStmtNode() {

	int i;
	printf("select stmt node hello world!\n");
	return (selectStmtNode*) malloc(sizeof(selectStmtNode));
	

}

selectListNode * mkSelectListNode() {

	int i;
	printf("select list node hello world!\n");
	return (selectListNode*) malloc(sizeof(selectListNode));
	

}
fromClauseNode * mkFromClauseNode() {

	int i;
	printf("form clause node hello world!\n");
	return (fromClauseNode*) malloc(sizeof(fromClauseNode));
	

}

valueExprNode * mkValueExpr() {
	
	valueExprNode *node = (valueExprNode *) malloc((size_t) sizeof(valueExprNode));

	if (node == NULL)  yyerror("oops. Malloc failed\n");

	node->type = UNDEFINED;
	node->value.integer_val = 0;	

	return(node);
}
