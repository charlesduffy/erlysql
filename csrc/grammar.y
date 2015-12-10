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
	fromClauseNode 		*fromClause;
	tableRefNode   		*tableRef;
	tableRefListNode 	*tableRefList;
	tableExprNode  		*tableExpr;
	valueExprNode  		valueExpr;
	scalarExpr	 	*sExpr;
	whereClauseNode 	*whereClause;
	colRef			*columnRef;

	/* DDL Nodes - CREATE TABLE */

	columnDefNode             *columnDef;
	columnDefListNode         *columnDefList;
	ddlTableRefNode        *createTableRef;
	createTableStmtNode	  *createTableStmt;
	dropTableStmtNode	  *dropTableStmt;
	valueExprType		  dataType;
	
}	

%code{
void yyerror (yyscan_t scanner, queryNode *qry, char const *s) {
     fprintf (stderr, ">>> %s <<<\n", s); }
}

/* SQL keywords */
%token <keyword> SELECT INSERT UPDATE DELETE WHERE FROM VALUES CREATE DROP SUM COUNT SET INTO AS TABLE

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

%type	<query>			query_statement
%type 	<selectStmt> 		select_statement
%type 	<selectList> 		select_list
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
			  $$->query_stmt.selnode = $1;
			} 
			|
	create_table_stmt
			{
			  $$ = ptree;
			  $$->statType = CREATE_TABLE_STMT;
			  $$->query_stmt.crTabNode = $1;
			}
			|
	drop_table_stmt
			{
			  $$ = ptree;
			  $$->statType = DROP_TABLE_STMT;
			  $$->query_stmt.drTabNode = $1;
			}

//TODO consider using a more generic "DDL stmt" rather than explicitly identifying every kind of DDL operation
		
;

/* 

INSERT STATEMENT

----UNSUPPORTED

*/

/*

insert_statement:
	INSERT INTO IDENTIFIER VALUES LPAREN value_list RPAREN
;

value_list:
	LPAREN scalar_expr RPAREN   		|
	scalar_expr scalar_operator scalar_expr |
	scalar_expr COMMA scalar_expr		|
	scalar_expr			
;

*/


	
/*

SELECT STATEMENT

*/

select_list:
	scalar_expr	  {
				//dangerous assumption here, that we can allocate the value of the select_list
				//at the first instance of scalar_expr.
		 		$$ = MAKENODE(selectListNode);
		 		$$->sExpr = malloc ((size_t) sizeof (scalarExpr*) * 20); //TEMP fixed size of 20 here, to debug issues with this

			 	debug("First Scalar expr in select list!");
				$$->nElements = 1;
				*($$->sExpr) = $1;
				
			  } |
	select_list COMMA scalar_expr  { debug("recursive scalar expr!");
					  
					*($$->sExpr + ($$->nElements)) = $3; //remove redundant parentheses
					  
					$$->nElements++;

				//	printf("scalar_expr: integer_value: %d\n\r" , SK->value.value.integer_val);
					/*----------------------
					|  this all kind of sux, of course.
					|  suggestions for replacement:
					|  ^ use a linked list instead of **sExpr
					|  ^ preallocate a large number of select list items, and realloc in the event the number exceeds. Suggest say 2000 items. 
					|    -- problem with that is that the sExprs can be arbitrarily complex...how deep to pre-allocate them?
					|----------------------*/
					  
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
		debug("table_ref: table name is %s alias is not present", $1);
		$$->tableName = strdup($1);
		$$->tableAlias = NULL;
	}
	|
	IDENTIFIER IDENTIFIER {
		$$ = MAKENODE(tableRefNode);
		debug("table_ref: table name is %s alias is %s ", $1, $2);
		$$->tableName = strdup($1);
		$$->tableAlias = strdup($2);
	}
	|
	IDENTIFIER AS IDENTIFIER {
		$$ = MAKENODE(tableRefNode);
		debug("table_ref: table name is %s alias is %s", $1, $3);
		$$->tableName = strdup($1);
		$$->tableAlias = strdup($3);

	}
;

table_ref_list:
	table_ref {
			debug("table_expr: first item: ");

			$$ = MAKENODE(tableRefListNode);
			// Assume first table reference in from clause
			$$->tables =  malloc ( sizeof(tableRefNode) * 20); //TEMPORARY! FIX ASAP. 
		
			*($$->tables) = $1;

			$$->nElements = 1;
				
		   } |

	table_ref_list COMMA table_ref {
			tableRefListNode *foo; //TODO clean up this cruft
			tableRefNode *bar;
			debug("table_expr: next item. Elements (before incr): %d || %s", $$->nElements, $3->tableName);
			*($$->tables + ($$->nElements)) = $3;			
			foo = $$;
			bar = *(foo->tables + $$->nElements);
			debug("table_expr: next item. After assignment: %s", bar->tableName);
			$$->nElements++;
	}
;

table_expr:
	from_clause {
		$$ = MAKENODE(tableExprNode);
		$$->fromClause = $1;	
		$$->whereClause = NULL;
	} |
	from_clause where_clause {
		$$ = MAKENODE(tableExprNode);
		$$->fromClause = $1;
		$$->whereClause = $2;
	}
;

/*

EXPRESSIONS

Todo: rewrite all the scalar_expr OPER scalar_expr rules.

*/


scalar_expr:
	value_expr  { $$ = MAKENODE(scalarExpr);
		      $$->value = $1;
		      $$->left = NULL;
		      $$->right = NULL;
		      debug("Single value_expr in scalar_expr");
		    }				|
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
	colref
	{ 
		$$.type = COLREF;
		$$.value.column_val = $1;
		debug("value_expr in parser. Colref value ");
	}
	|
	INT_LIT {
			$$.type = INT;
			$$.value.integer_val = $1;
			debug("value_expr in parser. Integer value is: %d", $$.value.integer_val);
		}	 |	
	NUM_LIT {
			
			$$.type = NUM;
			$$.value.numeric_val = $1;
			
		}	|	
	STRING  {
			
			$$.type = TEXT;
			$$.value.text_val = $1;
			debug("value_expr in parser. Text value : %s", $$.value.text_val);
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
		debug("colref with reference. Ref: %s , Colname: %s", $1 , $3); 
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
	STRING  {
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

column_definition: IDENTIFIER data_type
	{
		$$ = MAKENODE(columnDefNode);
		$$->colName = $1;
		$$->colType = $2;	
	}

;

%%
