#ifndef _PARSETREE_H
#define _PARSETREE_H

/* Default list node allocation chunk sizes */
#define selectListItemNode_allocnmemb 20
#define queryNode_allocnmemb 5

/* get number of elements in a list node */
#define get_num_elements(node) node->list.nElements

/* Helper Enums for parse nodes */

typedef enum { UNDEFINED, COLREF, INT, NUM, _TEXT, OPER, SEXPR, WILDCARD, IN_LIST, BETWEEN_PREDICATE } valueExprType;

typedef enum { _DIV, _MUL, _ADD, _SUB, _MOD, _GT, _LT, _GTE, _LTE, _OR, _AND, _NOT, _EQ, _NE, _IN, _NOT_IN,
		_BETWEEN, _NOT_BETWEEN } operVal;

/* list node information block */

typedef struct list_info_block listInfoBlock;

struct list_info_block {
	int nElements;
	unsigned long currentSize;
	int index;
	listInfoBlock *next;	
};

/* operator symbols */
extern char *operSyms[];

typedef struct s_expr scalarExpr;

/*
	BETWEEN predicate node

*/

typedef struct {
	scalarExpr *rangeStart;
	scalarExpr *rangeEnd;
} betweenPredNode;

/*
	IN list node
*/

typedef struct {
  int nElements;
  listInfoBlock list;
  scalarExpr **sItems;
} inListNode;

/* column reference */

typedef struct {
  char *colName;
  char *colReference;
} colRef;

/* value expression */

typedef union {
  colRef *column_val;  //consider static allocation
  int integer_val;
  char *text_val;
  double numeric_val;
  operVal oper_val;
  inListNode *in_list_val;
  betweenPredNode *between_pred_val;
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
	Select list item node
*/

typedef struct {
  char hasAlias;
  char isWildcard;
  scalarExpr *sExpr;
  char *sAlias;
} selectListItemNode;


/*
	Generic list node
*/

/*
	Select list node
*/

typedef struct {
  int nElements;
  listInfoBlock list; 
  selectListItemNode **sItems;
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
  listInfoBlock list;
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
	Error type enum

	TODO: place in seperate header
*/


typedef enum { SYNTAX_ERROR,  } errorType;

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
  	listInfoBlock list;
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

/* optional column list for insert statement */

typedef struct _insertColListNode {
	int nElements;
  	listInfoBlock list;
	char **sItems;		
} insertColListNode;

typedef struct _insertValListNode {
	int nElements;
	listInfoBlock list;
	scalarExpr **sItems;	
} insertValListNode;

typedef struct _insertStmtNode {
	ddlTableRefNode *table;
	insertColListNode *collist;
	insertValListNode *vallist;
	selectStmtNode *selnode;	
} insertStmtNode;

/* UPDATE STATEMENT */

typedef struct _updateStmtNode {

} updateStmtNode;

/* DELETE STATEMENT */

typedef struct _deleteStmtNode {

} deleteStmtNode;

/*
	Error report node

*/

typedef struct _errorNode errorNode;

struct _errorNode {

	int line;
	int column;	

};

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

  char errFlag;

  selectListNode * (*get_select_list)(queryNode *); //= get_select_list0;
  fromClauseNode * (*get_from_clause)(queryNode *);
  whereClauseNode * (*get_where_clause)(queryNode *);  

  statementType statType;

  errorNode *errNode;

  union {
    selectStmtNode *selnode;
    insertStmtNode *insnode;
    updateStmtNode *updnode;
    createTableStmtNode *crTabNode;
    dropTableStmtNode *drTabNode;
  };
};

/* Handle multiple-statement containing query string 
	top level parser element
*/

typedef struct _multiQueryNode multiQueryNode;

struct _multiQueryNode {
	listInfoBlock list;
	char errFlag;
	int nElements;
	queryNode **sItems;	
	queryNode *sItem;
};

/* Node constructor function prototypes */

queryNode * new_queryNode ( queryNode * );
multiQueryNode * new_multiQueryNode ( multiQueryNode *);

/* Accessor function prototypes */

selectListNode * get_select_list1 (selectStmtNode *);
selectListNode * get_select_list0 (queryNode *);
tableRefNode ** get_table_list1 (selectStmtNode *); 
tableRefNode ** get_table_list0 (queryNode *); 

/* Iterator function prototypes */

void * iter_next(void *);	
void * iter_prev(void *);	
void * iter_first(void *);	
void * iter_last(void *);	
void * iter_current(void *);	
void * iter_inext(void *);	
void * iter_iprev(void *);	
void * iter_ifirst(void *);	
void * iter_ilast(void *);	

#endif
