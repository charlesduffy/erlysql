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

	selectListNode *node = (selectListNode *) malloc((size_t) sizeof(selectListNode));

	if (node == NULL)  yyerror("oops. Malloc failed\n");

	node->type = UNDEFINED;
	node->item = NULL;

	return(node);

}
fromClauseNode * mkFromClauseNode() {

	fromClauseNode *node = (fromClauseNode *) malloc((size_t) sizeof(fromClauseNode));

	if (node == NULL)  yyerror("oops. Malloc failed\n");

	node->type = UNDEFINED;
	node->item = NULL;

	return(node);
}

valueExprNode * mkValueExpr() {
	
	valueExprNode *node = (valueExprNode *) malloc((size_t) sizeof(valueExprNode));

	if (node == NULL)  yyerror("oops. Malloc failed\n");

	node->type = UNDEFINED;
	node->value.integer_val = 0;	

	return(node);
}

whereClauseNode * mkWhereClauseNode() {
	
	whereClauseNode *node = (whereClauseNode*) malloc((size_t) sizeof(whereClauseNode));

	if (node == NULL)  yyerror("oops. Malloc failed\n");

	node->type = UNDEFINED;
	node->value.integer_val = 0;	

	return(node);
}
