%code requires {
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "parsetree.h"
#include "dbglog.h"

#define MAKENODE(nodetype) malloc ((size_t) sizeof( nodetype ))

#define MAKESCALAR(optype) \
		$$ = MAKENODE(scalarExpr);\
		$$->value = 

typedef void *yyscan_t;

}

/* parser options */


%define api.pure full
%lex-param {yyscan_t scanner}
%parse-param {yyscan_t scanner} {queryNode * ptree}

%locations
/* semantic value */

%union 
	{
	int			integer_val;
	char 			*text_val;
	float  			float_val;
	char			*keyword;
	char    		*identifier_val;

	/* Query Node */
	queryNode 		*query;

	/* DML Nodes - SELECT */

	selectStmtNode 		*selectStmt;
	selectListNode 		*selectList;
	selectListItemNode 	*selectListItem;
	fromClauseNode 		*fromClause;
	tableRefNode   		*tableRef;
	tableRefListNode 	*tableRefList;
	tableExprNode  		*tableExpr;
	valueExprNode  		valueExpr;
	scalarExpr	 	*sExpr;
	whereClauseNode 	*whereClause;
	colRef			*columnRef;

	/* DML Nodes - INSERT */

	insertStmtNode		*insertStmt;
	insertColListNode	*insertColList;
	insertValListNode	*insertValList;

	/* DDL Nodes - CREATE TABLE */

	columnDefNode             *columnDef;
	columnDefListNode         *columnDefList;
	ddlTableRefNode        *createTableRef;
	createTableStmtNode	  *createTableStmt;
	dropTableStmtNode	  *dropTableStmt;
	valueExprType		  dataType;
	
}	

%code{

  void yyerror (YYLTYPE *l, yyscan_t scanner, queryNode *qry, char const *s) {
       qry->selnode = NULL;
       qry->errFlag = 1;
       fprintf (stderr, "ERROR: %s -- %d %d %d %d \n", s, l->first_line, l->first_column, l->last_line, l->last_column);  
  }

}

/* SQL keywords */
%token <keyword> SELECT INSERT UPDATE DELETE WHERE FROM VALUES CREATE DROP SUM COUNT SET INTO TABLE

/* SQL Datatypes */

%token <keyword> INTEGER BIGINT SMALLINT NUMERIC CHAR

/* Literal values */
%token <integer_val> INT_LIT
%token <float_val> NUM_LIT
%token <text_val> STRING 

/* punctuation */
%token <keyword> QUOTE COMMA NEWLINE 

/* operators */
%left           OR
%left           AND
%left		NE
%right		EQ
%nonassoc	LT GT
%nonassoc	LE GE
%left           ADD SUB
%left           MUL DIV MOD
%left           EXP
/* Unary Operators */
%right          UMINUS
%left		LPAREN RPAREN
%left		SEMICOLON COMMA
//%left         TYPECAST
%left           POINT
%left 		AS

%type	<query>			query_statement
%type 	<selectStmt> 		select_statement
%type 	<selectList> 		select_list
%type 	<selectListItem>	select_list_item
%type 	<fromClause> 		from_clause
%type 	<tableRef> 		table_ref
%type 	<tableRefList> 		table_ref_list
%type 	<valueExpr> 		value_expr
%type 	<sExpr> 		scalar_expr
%type   <columnRef> 		colref
%type   <whereClause> 		where_clause
%type 	<tableExpr> 		table_expr
%type   <columnDef>   		column_definition
%type   <columnDefList>		column_definition_list
%type   <dataType>		data_type
%type   <insertStmt>		insert_statement
%type   <insertValList>		insert_value_list
%type   <insertColList>		column_list
%type   <createTableRef>	ddl_table_ref
%type 	<createTableStmt>	create_table_stmt
%type 	<dropTableStmt>		drop_table_stmt

%token  <identifier_val>  IDENTIFIER


%%


/* this node is a multi-statement submission delimited by semicolon */

sql:
	query_statement	SEMICOLON |
	query_statement SEMICOLON query_statement

;

/* this node is a single query statement */

query_statement:
	select_statement	
			{ 
			  $$ = ptree;
			  $$->statType = SELECT_STMT;
			  $$->selnode = $1;
			} 
			|
	insert_statement	
			{ 
			  $$ = ptree;
			  $$->statType = INSERT_STMT;
			  $$->insnode = $1;
			} 
			|
	create_table_stmt
			{
			  $$ = ptree;
			  $$->statType = CREATE_TABLE_STMT;
			  $$->crTabNode = $1;
			}
			|
	drop_table_stmt
			{
			  $$ = ptree;
			  $$->statType = DROP_TABLE_STMT;
			  $$->drTabNode = $1;
			}

//TODO consider using a more generic "DDL stmt" rather than explicitly identifying every kind of DDL operation
		
;

/* 

INSERT STATEMENT


*/


insert_statement:

	INSERT INTO ddl_table_ref LPAREN column_list RPAREN VALUES LPAREN insert_value_list RPAREN { 
		$$ = MAKENODE(insertStmtNode);
		$$->table = $3;
		$$->collist = $5;
		$$->vallist = $9;	
		$$->selnode = NULL;
	}
	|
	INSERT INTO ddl_table_ref VALUES LPAREN insert_value_list RPAREN {  //todo - seperate out the 'values' table
		$$ = MAKENODE(insertStmtNode);
		$$->table = $3;
		$$->collist = NULL;
		$$->vallist = $6;	
		$$->selnode = NULL;
	}
	|
	INSERT INTO ddl_table_ref select_statement {
		$$ = MAKENODE(insertStmtNode);
		$$->table = $3;
		$$->collist = NULL;
		$$->vallist = NULL;	
		$$->selnode = $4;
	}
;

column_list:
	IDENTIFIER {
		$$ = MAKENODE(insertColListNode);
		$$->cItems = malloc ((size_t) sizeof (insertColListNode*) * 20); //TEMP fixed size of 20 here, to debug issues with this
		$$->nElements = 1;
		*($$->cItems) = $1;
	} 
	|
	column_list COMMA IDENTIFIER {
		*($$->cItems + ($$->nElements)) = $3; //remove redundant parentheses
		$$->nElements++;
	}
;	

//should take generic table expression for INSERT source

insert_value_list:
	scalar_expr { 
		$$ = MAKENODE(insertValListNode);
		$$->vItems = malloc ((size_t) sizeof (insertValListNode*) * 20); //TEMP fixed size of 20 here, to debug issues with this
		$$->nElements = 1;
		*($$->vItems) = $1;
	}	
	|		
	insert_value_list COMMA scalar_expr {	
		*($$->vItems + ($$->nElements)) = $3; //remove redundant parentheses
		$$->nElements++;
	}
;


	
/*

SELECT STATEMENT

*/

select_list:
	select_list_item {
				//dangerous assumption here, that we can allocate the value of the select_list
				//at the first instance of scalar_expr.
				
				//Consider hiding this preallocation code in selectListNode's constructor function
		 		$$ = MAKENODE(selectListNode);
		 		$$->sItems = malloc ((size_t) sizeof (selectListItemNode *) * 20); //TEMP fixed size of 20 here, to debug issues with this

			 	debug("First Scalar expr in select list!");
				$$->nElements = 1;
				*($$->sItems) = $1;
				
			  }
			  |
	select_list COMMA select_list_item { 
				debug("recursive scalar expr!");
				*($$->sItems + ($$->nElements)) = $3; //remove redundant parentheses
				$$->nElements++;

					/*----------------------
					|  this all kind of sux, of course.
					|  suggestions for replacement:
					|  ^ use a linked list instead of **sExpr
					|  ^ preallocate a large number of select list items, and realloc in the event the number exceeds. Suggest say 2000 items. 
					|    -- problem with that is that the sExprs can be arbitrarily complex...how deep to pre-allocate them?
					|----------------------*/
					  
					} 
;





select_list_item:
	scalar_expr {

			$$ = MAKENODE(selectListItemNode);
			$$->isWildcard = 0;
			$$->sExpr = $1;	
			$$->hasAlias = 0;
			$$->sAlias = NULL;
			debug("select_list_item: no ALIAS reduced\n");
		    }
		    |
	MUL	    {
			$$ = MAKENODE(selectListItemNode);
			$$->isWildcard = 1;
			$$->hasAlias = 0;
			$$->sAlias = NULL;
		    }
		    |
	scalar_expr AS IDENTIFIER {
		   	 
			$$ = MAKENODE(selectListItemNode);
			$$->isWildcard = 0;
			$$->sExpr = $1;
			$$->hasAlias = 1;
			$$->sAlias = $3;
			debug("ALIAS in select list item: %s ", $3);
		}
;

select_statement:
	SELECT select_list table_expr {
		$$ = MAKENODE(selectStmtNode);
		$$->selectList = $2;
		$$->tableExpr = $3;
	};

where_clause:
	WHERE scalar_expr 
			{
				$$ = MAKENODE(whereClauseNode);
				$$->expr = $2;
			}
;

from_clause:
	FROM table_ref_list { 
				$$ = MAKENODE(fromClauseNode); 
				$$->refList = $2;
			    }
;

table_ref:
	IDENTIFIER {
		$$ = MAKENODE(tableRefNode);
		$$->tableName = $1;
		$$->tableAlias = NULL;
	}
	|
	IDENTIFIER IDENTIFIER {
		$$ = MAKENODE(tableRefNode);
		$$->tableName = $1;
		$$->tableAlias = $2;
	}
	|
	IDENTIFIER AS IDENTIFIER {
		$$ = MAKENODE(tableRefNode);
		$$->tableName = $1;
		$$->tableAlias = $3;

	}
;

table_ref_list:
	table_ref {
			$$ = MAKENODE(tableRefListNode);
			$$->tables =  malloc ( sizeof(tableRefNode) * 20); //TEMPORARY! FIX ASAP. 
			*($$->tables) = $1;
			$$->nElements = 1;
		  }
		  |
	table_ref_list COMMA table_ref {
			*($$->tables + ($$->nElements)) = $3;			
			$$->nElements++;
	}
;

table_expr:
	from_clause {
		$$ = MAKENODE(tableExprNode);
		$$->fromClause = $1;	
		$$->whereClause = NULL;
	}
	|
	from_clause where_clause {
		$$ = MAKENODE(tableExprNode);
		$$->fromClause = $1;
		$$->whereClause = $2;
	}
;


/*

EXPRESSIONS

*/


scalar_expr:
	value_expr  { 
		      $$ = MAKENODE(scalarExpr);
		      $$->value = $1;
		      $$->left = NULL;
		      $$->right = NULL;
		    }
	        	|
	LPAREN scalar_expr RPAREN
				{ 
				  $$ = MAKENODE(scalarExpr);
				  $$->left = $2;
				  $$->right = NULL;			  
				}		|
	scalar_expr ADD scalar_expr 
				{
				  $$ = MAKENODE(scalarExpr);
				  $$->left = $1;
				  $$->right = $3;
				  $$->value.type = OPER;
				  $$->value.value.oper_val = ADDITION;	
				}		|
				
	scalar_expr MUL scalar_expr 		
				{
				  $$ = MAKENODE(scalarExpr);
				  $$->left = $1;
				  $$->right = $3;
				  $$->value.type = OPER;
				  $$->value.value.oper_val = MULTIPLICATION;	
			
				}		|
	
	scalar_expr DIV scalar_expr 		
				{
				  $$ = MAKENODE(scalarExpr);
				  $$->left = $1;
				  $$->right = $3;
				  $$->value.type = OPER;
				  $$->value.value.oper_val = DIVISION;	
			
				}		|

	scalar_expr MOD scalar_expr 		
				{
				  $$ = MAKENODE(scalarExpr);
				  $$->left = $1;
				  $$->right = $3;
				  $$->value.type = OPER;
				  $$->value.value.oper_val = MODULO;	
			
				}		|

	scalar_expr AND scalar_expr 		
				{
				  $$ = MAKENODE(scalarExpr);
				  $$->left = $1;
				  $$->right = $3;
				  $$->value.type = OPER;
				  $$->value.value.oper_val = BOOLAND;	
			
				}		|

	scalar_expr OR scalar_expr 		
				{
				  $$ = MAKENODE(scalarExpr);
				  $$->left = $1;
				  $$->right = $3;
				  $$->value.type = OPER;
				  $$->value.value.oper_val = BOOLOR;	
			
				}		|

	scalar_expr EQ scalar_expr 		
				{
				  $$ = MAKENODE(scalarExpr);
				  $$->left = $1;
				  $$->right = $3;
				  $$->value.type = OPER;
				  $$->value.value.oper_val = EQUAL ;	
			
				}		|

	scalar_expr NE scalar_expr 		 
				{
				  $$ = MAKENODE(scalarExpr);
				  $$->left = $1;
				  $$->right = $3;
				  $$->value.type = OPER;
				  $$->value.value.oper_val = NOTEQUAL;	
			
				}		|

	scalar_expr GT scalar_expr 		
				{
				  $$ = MAKENODE(scalarExpr);
				  $$->left = $1;
				  $$->right = $3;
				  $$->value.type = OPER;
				  $$->value.value.oper_val = GREATERTHAN;	
			
				}		|

	scalar_expr LT scalar_expr 		
				{
				  $$ = MAKENODE(scalarExpr);
				  $$->left = $1;
				  $$->right = $3;
				  $$->value.type = OPER;
				  $$->value.value.oper_val = LESSTHAN;	
			
				}		|

	scalar_expr GE scalar_expr 		
				{
				  $$ = MAKENODE(scalarExpr);
				  $$->left = $1;
				  $$->right = $3;
				  $$->value.type = OPER;
				  $$->value.value.oper_val = GREATERTHANOE;	
			
				}		|
	scalar_expr LE scalar_expr 	
				{
				  $$ = MAKENODE(scalarExpr);
				  $$->left = $1;
				  $$->right = $3;
				  $$->value.type = OPER;
				  $$->value.value.oper_val = LESSTHANOE;	
			
				}		|

	scalar_expr SUB scalar_expr 	
				{
				  $$ = MAKENODE(scalarExpr);
				  $$->left = $1;
				  $$->right = $3;
				  $$->value.type = OPER;
				  $$->value.value.oper_val = SUBTRACTION;	
				}		
;

value_expr:
	colref  { 
		  $$.type = COLREF;
		  $$.value.column_val = $1;
		}
		|
	INT_LIT {
		  $$.type = INT;
		  $$.value.integer_val = $1;
		}
		|	
	NUM_LIT {
		  $$.type = NUM;
		  $$.value.numeric_val = $1;
		}
		|	
	STRING  {
		  $$.type = TEXT;
		  $$.value.text_val = $1;
		}
;

colref:
	IDENTIFIER 
	{ 
		$$=MAKENODE(colRef);
		$$->colName = $1;
		$$->colReference = NULL; 
	}
	|
	IDENTIFIER POINT IDENTIFIER  
	{
		$$=MAKENODE(colRef);
		$$->colName = $3;
		$$->colReference = $1; 
	}
;

/* Data definition language commands */

/* Drop Table */

drop_table_stmt:
	DROP TABLE ddl_table_ref
	{
		$$=MAKENODE(dropTableStmtNode);
		$$->dropTable = $3;
	}
;

/* Create Table */

ddl_table_ref:
	IDENTIFIER 
	{
		$$=MAKENODE(ddlTableRefNode);
		$$->tableName = $1;
		$$->tableSchema = NULL; 
	}
	|
	IDENTIFIER POINT IDENTIFIER  
	{
		$$=MAKENODE(ddlTableRefNode);
		$$->tableName = $1;
		$$->tableSchema = $3; 
	}
;

data_type:
	INTEGER	
		{
			$$ = INT;	
		}
		|
	NUMERIC 
		{
			$$ = NUM;
		}
		|
	CHAR	{
			$$ = TEXT;	
		}
;

create_table_stmt:
	CREATE TABLE ddl_table_ref LPAREN column_definition_list RPAREN
	{
		debug ("create table statement");
		$$=MAKENODE(createTableStmtNode);
		$$->createTable = $3;
		$$->colDefList = $5;
		
	}
;

column_definition_list:
	column_definition
	{
			$$ = MAKENODE(columnDefListNode);
			//Again we hardcode and preallocate a limited number of column definition nodes
			$$->colDef = malloc ( (size_t)sizeof(columnDefNode) * 20); //TEMPORARY! FIX ASAP. 
			*($$->colDef) = $1;
			$$->nElements = 1;
	} 
	|
	column_definition_list COMMA column_definition
	{
			*($$->colDef + ($$->nElements)) = $3;			
			$$->nElements++;
	}
;

column_definition: 
	IDENTIFIER data_type
	{
		$$ = MAKENODE(columnDefNode);
		$$->colName = $1;
		$$->colType = $2;	
	}

;

%%
