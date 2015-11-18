#ifndef _PARSETREE_H
#define _PARSETREE_H

/* Helper Enums for parse nodes */

typedef enum { UNDEFINED, COLREF, INT, NUM, TEXT, OPER, SEXPR } valueExprType;

typedef enum { DIVISION, MULTIPLICATION, ADDITION, SUBTRACTION, MODULO,
    GREATERTHAN, LESSTHAN, GREATERTHANOE, LESSTHANOE, BOOLOR, BOOLAND, BOOLNOT,
    EQUAL, NOTEQUAL } operVal;

/* column reference */

struct column_ref {
  char *colName;
  char *colReference;
};

typedef struct column_ref colRef;

/* value expression */

union value_expr {
  colRef *column_val;
  int integer_val;
  char *text_val;
  double numeric_val;
  operVal oper_val;
};

typedef union value_expr valueExpr;

/* Parse Nodes */

struct value_expr_node {
  valueExprType type;
  valueExpr value;
};

typedef struct value_expr_node valueExprNode;

/* scalar expression */

typedef struct s_expr scalarExpr;

struct s_expr {

  valueExprNode value;
  scalarExpr *left;
  scalarExpr *right;

};


/*
	Select list node
*/

struct select_list_node {
  int nElements;
  scalarExpr **sExpr;
};

typedef struct select_list_node selectListNode;



/*	
	Table reference node 
*/

struct table_ref_node {
  char *tableName;
  char *tableAlias;
};

typedef struct table_ref_node tableRefNode;

/*
	Table reference list node	
*/

struct table_ref_list_node {
  int nElements;
  tableRefNode **tables;
};

typedef struct table_ref_list_node tableRefListNode;

/*
	FROM clause node
*/

struct from_clause_node {
  int type;
  tableRefListNode *refList;
};

typedef struct from_clause_node fromClauseNode;

/*
	WHERE clause node
*/

struct where_clause_node {
  int type;
  scalarExpr *expr;
};

typedef struct where_clause_node whereClauseNode;

/*
	Table expression node
*/
struct table_expression_node {

  fromClauseNode *fromClause;
  whereClauseNode *whereClause;

};

typedef struct table_expression_node tableExprNode;

/*
	Statement type enum
*/

enum statement_type { SELECT_STMT, INSERT_STMT, UPDATE_STMT, DELETE_STMT };

typedef enum statement_type statementType;

/*
	SELECT statement node
*/

struct select_stmt_node {
  selectListNode *selectList;
  tableExprNode *tableExpr;
};

typedef struct select_stmt_node selectStmtNode;


/*
	Query statement node
*/

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
