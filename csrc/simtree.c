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

	
	valueExprNode *n;

	printf("value expression hello world!\n");
	printf("size of int is: %d\n", sizeof(int));
	printf("size of value expression node is: %d\n", sizeof(valueExprNode));
	n = (valueExprNode *) malloc((size_t) sizeof(valueExprNode));
	if (n == NULL) {
		printf("oops. Malloc failed\n");
		n = NULL;
	}
	n->type = UNDEFINED;
	n->value.integer_val = 0;	

	return(n);

}
