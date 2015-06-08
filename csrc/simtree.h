#ifndef _SIMTREE_H
#define _SIMTREE_H


/* Helper Enums for parse nodes */


//typedef enum { UNDEFINED, COLREF , INT , NUM , TEXT } valueExprType;

/*
union value_expr {
	char * colName;
	int integer_val;
	char * text_val;
	float numeric_val;
} ;
*/


typedef union value_expr valueExpr;

/* Parse Nodes */

typedef char * colName0;

struct select_stmt_node {
	int type;
} ;

typedef struct select_stmt_node selectStmtNode;

struct select_list_node {
	int type;
} ;

typedef struct select_list_node selectListNode;

struct from_clause_node {
	int type;
} ;

typedef struct from_clause_node fromClauseNode;

struct value_expr_node {
	enum YYTOKENTYPE type;
	union YYSTYPE value;
} ;

typedef struct value_expr_node valueExprNode;

struct scalar_expr_node {
	int type;
} ;

typedef struct scalar_expr_node scalarExprNode;

/* node function prototypes */
selectStmtNode * mkSelectStmtNode ();
selectListNode * mkSelectListNode ();
fromClauseNode * mkFromClauseNode ();
valueExprNode * mkValueExpr();

#endif
