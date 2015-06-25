#ifndef _SIMTREE_H
#define _SIMTREE_H

/* Helper Enums for parse nodes */

typedef enum { UNDEFINED, COLREF , INT , NUM , TEXT, OPER, SEXPR } valueExprType;

typedef enum { DIVISION , MULTIPLICATION , ADDITION , SUBTRACTION , MODULO } operVal;

union value_expr {
	char * colName;
	int integer_val;
	char * text_val;
	float numeric_val;
	operVal oper_val;
} ;

typedef union value_expr valueExpr;

/* Parse Nodes */

struct value_expr_node {
	valueExprType type;
	valueExpr value;
} ;

typedef struct value_expr_node valueExprNode;

/* scalar expression */

typedef struct s_expr scalarExpr;

struct s_expr {

	valueExprType type;
	valueExprNode * value;
	scalarExpr * left;	
	scalarExpr * right;	

} ;


/*-------------------*/

struct select_list_node {
	int type;
	valueExpr *item;
} ;

typedef struct select_list_node selectListNode;

struct table_ref_node {
	char *tableName;
} ;

typedef struct table_ref_node tableRefNode;

struct from_clause_node {
	int type;
	tableRefNode *item;
} ;

typedef struct from_clause_node fromClauseNode;

struct where_clause_node {
	int type;
	scalarExpr *expr;	
} ;

typedef struct where_clause_node whereClauseNode;



struct select_stmt_node {
	selectListNode *selectList;
	fromClauseNode *fromClause;
	whereClauseNode *whereClause;
} ;

typedef struct select_stmt_node selectStmtNode;

struct query_node {
	//this should be a linked list of statement types (probably defined as a linked list of unions
	selectStmtNode *selnode;	
	int foo;
};

typedef struct query_node queryNode;

#endif
