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

//typedef struct value_expr_node valueExprNode;

/* scalar expression */

typedef struct s_expr scalarExpr;

struct s_expr {

  valueExprNode value;
  scalarExpr *left;
  scalarExpr *right;

} ;


/*
	Select list node
*/

typedef struct {
  int nElements;
  scalarExpr **sExpr;
} selectListNode;

//typedef struct select_list_node selectListNode;



/*	
	Table reference node 
*/

typedef struct {
  char *tableName;
  char *tableAlias;
} tableRefNode;

//typedef struct table_ref_node tableRefNode;

/*
	Table reference list node	
*/

typedef struct {
  int nElements;
  tableRefNode **tables;
} tableRefListNode;

//typedef struct table_ref_list_node tableRefListNode;

/*
	FROM clause node
*/

typedef struct {
  int type;
  tableRefListNode *refList;
} fromClauseNode;

//typedef struct from_clause_node fromClauseNode;

/*
	WHERE clause node
*/

typedef struct {
  int type;
  scalarExpr *expr;
} whereClauseNode;

//typedef struct where_clause_node whereClauseNode;

/*
	Table expression node
*/
typedef struct {

  fromClauseNode *fromClause;
  whereClauseNode *whereClause;

} tableExprNode;

//typedef struct table_expression_node tableExprNode;

/*
	Statement type enum
*/

typedef enum { SELECT_STMT, INSERT_STMT, UPDATE_STMT, DELETE_STMT, CREATE_TABLE_STMT } statementType;

//typedef enum statement_type statementType;

/*
	SELECT statement node
*/

typedef struct {
  selectListNode *selectList;
  tableExprNode *tableExpr;
} selectStmtNode;

//typedef struct select_stmt_node selectStmtNode;

/* data definition language nodes */

typedef struct {
	char *tableName;
	char *tableSchema;
} createTableRefNode;

//typedef struct create_table_ref_node createTableRefNode;

typedef struct {
	char *colName;
	valueExprType colType;
	//TODO - here we have to add a sExpr to describe the 
	// column value restriction for constraints			
} columnDefNode;

//typedef struct column_definition_node columnDefNode;

typedef struct {
	columnDefNode **colDef;	
	int nElements;
} columnDefListNode;

//typedef struct column_definition_list_node columnDefListNode;

/* CREATE TABLE node */

typedef struct {
	createTableRefNode *createTable;
	columnDefListNode *colDefList;
} createTableStmtNode;

//typedef struct create_table_statement_node createTableStmtNode;


/*
	Query statement node
*/

typedef struct {
//add fn pointers to return the subcomponents here
  statementType statType;
  union {
    selectStmtNode *selnode;
    //insertStmtNode *insnode;
    //updateStmtNode *updnode;
    createTableStmtNode *crTabNode;
  } query_stmt;
} queryNode;

//typedef struct query_node queryNode;



#endif
