#ifndef _PARSETREE_H
#define _PARSETREE_H

/* Helper Enums for parse nodes */

typedef enum { UNDEFINED, COLREF, INT, NUM, TEXT, OPER, SEXPR } valueExprType;

typedef enum { DIVISION, MULTIPLICATION, ADDITION, SUBTRACTION, MODULO,
    GREATERTHAN, LESSTHAN, GREATERTHANOE, LESSTHANOE, BOOLOR, BOOLAND, BOOLNOT,
    EQUAL, NOTEQUAL } operVal;

/* operator symbols */
extern char *operSyms[];

/* column reference */

typedef struct {
  char *colName;
  char *colReference;
} colRef;

/* value expression */

typedef union {
  colRef *column_val;
  int integer_val;
  char *text_val;
  double numeric_val;
  operVal oper_val;
} valueExpr;

/* Parse Nodes */

typedef struct {
  valueExprType type;
  valueExpr value;
} valueExprNode;

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

typedef struct {
  int nElements;
  scalarExpr **sExpr;
} selectListNode;

/*	
	Table reference node 
*/

typedef struct {
  char *tableName;
  char *tableAlias;
} tableRefNode;

/*
	Table reference list node	
*/

typedef struct {
  int nElements;
  tableRefNode **tables;
} tableRefListNode;

/*
	FROM clause node
*/

typedef struct {
  int type;
  tableRefListNode *refList;
} fromClauseNode;

/*
	WHERE clause node
*/

typedef struct {
  int type;
  scalarExpr *expr;
} whereClauseNode;

/*
	Table expression node
*/
typedef struct _tableExprNode tableExprNode;

struct _tableExprNode {

  
  tableExprNode * (*get_table_expr_node2)(tableExprNode *);
  whereClauseNode * (*get_where_clause_node2)(tableExprNode *);

  fromClauseNode *fromClause;
  whereClauseNode *whereClause;

};

/*
	Statement type enum
*/

typedef enum { SELECT_STMT, INSERT_STMT, UPDATE_STMT, DELETE_STMT, CREATE_TABLE_STMT, DROP_TABLE_STMT } statementType;

/*
	SELECT statement node
*/
typedef struct select_statement selectStmtNode;

struct select_statement {

/* function pointers */

  selectListNode * (*get_select_list1)(selectStmtNode *);
  tableRefNode ** (*get_table_expr_node1)(selectStmtNode *);

  selectListNode *selectList;
  tableExprNode *tableExpr;
};

/* data definition language nodes */

typedef struct {
	char *tableName;
	char *tableSchema;
} ddlTableRefNode;

typedef struct {
	char *colName;
	valueExprType colType;
	//TODO - here we have to add a sExpr to describe the 
	// column value restriction for constraints			
} columnDefNode;

typedef struct {
	columnDefNode **colDef;	
	int nElements;
} columnDefListNode;

/* CREATE TABLE node */

typedef struct {
	ddlTableRefNode *createTable;
	columnDefListNode *colDefList;
} createTableStmtNode;

/* DROP TABLE node */

typedef struct {
	ddlTableRefNode *dropTable;
} dropTableStmtNode;

/* INSERT STATEMENT */

typedef struct _insertStmtNode {

} insertStmtNode;

/* UPDATE STATEMENT */

typedef struct _updateStmtNode {

} updateStmtNode;

/* DELETE STATEMENT */

typedef struct _deleteStmtNode {

} deleteStmtNode;


/*
	Query statement node
*/
typedef struct _queryNode queryNode;

struct _queryNode {

/* 

	Useful function pointers
	
	- destroy. Free all nodes below this
	- get_node. Return the statement contained in the node.
	- create. Initialise the node with default values.
*/

  selectListNode * (*get_select_list)(queryNode *); //= get_select_list0;
  fromClauseNode * (*get_from_clause)(queryNode *);
  whereClauseNode * (*get_where_clause)(queryNode *);  

  statementType statType;

  union {
    selectStmtNode *selnode;
    insertStmtNode *insnode;
    updateStmtNode *updnode;
    createTableStmtNode *crTabNode;
    dropTableStmtNode *drTabNode;
  };
};

#endif
