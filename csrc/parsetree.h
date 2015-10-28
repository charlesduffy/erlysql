#ifndef _PARSETREE_H
#define _PARSETREE_H

/* Helper Enums for parse nodes */

typedef enum { UNDEFINED, COLREF , INT , NUM , TEXT, OPER, SEXPR } valueExprType;

typedef enum { DIVISION , MULTIPLICATION , ADDITION , SUBTRACTION , MODULO, GREATERTHAN, LESSTHAN, GREATERTHANOE, LESSTHANOE, BOOLOR, BOOLAND, BOOLNOT, EQUAL, NOTEQUAL } operVal;

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

	valueExprNode value;
	scalarExpr * left;	
	scalarExpr * right;	

} ;


/*-------------------*/

struct select_list_node {
	int nElements;
	scalarExpr **sExpr;
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

enum statement_type { SELECT_STMT, INSERT_STMT, UPDATE_STMT, DELETE_STMT };

typedef enum statement_type statementType;


struct query_node {
	statementType statType;
	//union statement type
	//*****FIX this
	selectStmtNode *selnode;	
	//insertStmtNode *insnode;
	//updateStmtNode *updnode;
};

typedef struct query_node queryNode;

#endif
