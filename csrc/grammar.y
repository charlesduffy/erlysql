%code requires {
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdbool.h>
#include "parsetree.h"
#include "dbglog.h"



#define MAKENODE(nodetype) malloc((size_t) sizeof( nodetype ))

#define new(nodetype) new_##nodetype(malloc((size_t) sizeof( nodetype )))

#define add_list_item(nodetype,node,item) {                                                     		\
      listInfoBlock list = (node)->list;                                                      		\
      nodetype **resizePtr;                                                                     		\
        size_t nodeAllocSize = (size_t) sizeof(nodetype *) * nodetype##_allocnmemb;             		\
          if (list.nElements == 0) {                                                           			\
                node->sItems = malloc(nodeAllocSize);                                           		\
                if (node->sItems == NULL) yyerror (&yylloc, scanner, ptree, YY_("can't allocate list item"));   \
          } else if (list.nElements > 0) { 									\
		 if (list.nElements % nodetype##_allocnmemb == 0) {                                             \
                 resizePtr = realloc(node->sItems, list.currentSize + nodeAllocSize);           		\
                 if (resizePtr == NULL) {                                                        		\
                 	yyerror (&yylloc, scanner, ptree, YY_("can't allocate list item")); 			\
                 } else {                                                                        		\
                         list.currentSize = list.currentSize + nodeAllocSize;                  			\
                         node->sItems = resizePtr;                                               		\
                 }   												\
	      }                                                                            			\
          }                                                                                     		\
          *(node->sItems + list.nElements) = item;                                             			\
          (node)->list.nElements++;       									\
}
 
typedef void *yyscan_t;

}

/* parser options */


%define api.pure full
%lex-param {yyscan_t scanner}
%parse-param {yyscan_t scanner} {multiQueryNode * ptree}

%locations
/* semantic value */

%union 
	{
	int			integer_val;
	char 			*text_val;
	float  			float_val;
	char			*keyword;
	char    		*identifier_val;


	/* Query Nodes */
	multiQueryNode		*mquery;
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
	ddlTableRefNode        	  *createTableRef;
	createTableStmtNode	  *createTableStmt;
	dropTableStmtNode	  *dropTableStmt;
	valueExprType		  dataType;
	
}	

%code{

  void yyerror (YYLTYPE *l, yyscan_t scanner, multiQueryNode *mqry, char const *s) {
       mqry->errFlag = 1;
       fprintf (stderr, "ERROR: %s -- %d %d %d %d \n", s, l->first_line, l->first_column, l->last_line, l->last_column);  
  }

}

/* SQL keywords */
%token <keyword> SELECT INSERT UPDATE DELETE WHERE FROM VALUES CREATE DROP SUM 
%token <keyword> COUNT SET INTO TABLE WITH

/* SQL Datatypes */

%token <keyword> INTEGER BIGINT SMALLINT INT2 INT4 INT8 NUMERIC REAL DOUBLE 
%token <keyword> BIT DATE TIME TIMESTAMP ZONE INTERVAL PRECISION FLOAT TEXT CHAR VARCHAR

/* Literal values */
%token <integer_val> INT_LIT
%token <float_val> NUM_LIT
%token <text_val> STRING 

/* punctuation */
%token <keyword> QUOTE COMMA NEWLINE 

/* operators */

/* fix precedence */

%left           OR
%left           AND
%left		NE
%left 		IN
%right		NOT
%right		EQ
%nonassoc	LT GT
%nonassoc	LE GE
%right		BETWEEN
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

%type	<mquery>		sql
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
%type	<sExpr>			in_predicate
%type	<sExpr>			between_predicate

%token  <identifier_val>  IDENTIFIER


%%


/* this node is a multi-statement submission delimited by semicolon */

sql:
	query_statement	SEMICOLON 
				{
				  $$ = ptree;
				  $$->nElements = 1;
				  add_list_item(queryNode, $$, $1 );
				}
	|
	sql SEMICOLON query_statement SEMICOLON

;

/* this node is a single query statement */

query_statement:
	select_statement	
			{ 
			  $$ = MAKENODE(queryNode);
			  $$->statType = SELECT_STMT;
			  $$->selnode = $1;
			} 
			|
	insert_statement	
			{ 
			  $$ = new(queryNode);
			  $$->statType = INSERT_STMT;
			  $$->insnode = $1;
			} 
			|
	create_table_stmt
			{
			  $$ = new(queryNode);
			  $$->statType = CREATE_TABLE_STMT;
			  $$->crTabNode = $1;
			}
			|
	drop_table_stmt
			{
			  $$ = new(queryNode);
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
		$$->sItems = malloc ((size_t) sizeof (insertColListNode*) * 20); //TEMP fixed size of 20 here, to debug issues with this
		$$->nElements = 1;
		*($$->sItems) = $1;
	} 
	|
	column_list COMMA IDENTIFIER {
		*($$->sItems + ($$->nElements)) = $3; //remove redundant parentheses
		$$->nElements++;
	}
;	

//should take generic table expression for INSERT source

insert_value_list:
	scalar_expr { 
		$$ = MAKENODE(insertValListNode);
		$$->sItems = malloc ((size_t) sizeof (insertValListNode*) * 20); //TEMP fixed size of 20 here, to debug issues with this
		$$->nElements = 1;
		*($$->sItems) = $1;
	}	
	|		
	insert_value_list COMMA scalar_expr {	
		*($$->sItems + ($$->nElements)) = $3; //remove redundant parentheses
		$$->nElements++;
	}
;


	
/*

SELECT STATEMENT

*/

select_list:
	select_list_item {
		 		$$ = MAKENODE(selectListNode);
			
				$$->list.nElements = 0; //temporary until proper constructor code is written		
	
			 	debug("First Scalar expr in select list!");
				//$$->nElements = 1;
				//*($$->sItems) = $1;
				int x;
				add_list_item(selectListItemNode, $$, $1 );
				
			  }
			  |
	select_list COMMA select_list_item { 
				debug("recursive scalar expr!");
				//*($$->sItems + ($$->nElements)) = $3; //remove redundant parentheses
				//$$->nElements++;
				add_list_item(selectListItemNode , $$ , $3 );

				//proposed call structure:
				// add_list_item( <base pointer> , <type> , <item pointer> )
				//
				// add_list_item( $$ , selectListItemNode , $3 );

					/*----------------------
					|  this all kind of sux, of course.
					|  suggestions for replacement:
					|  ^ use a linked list instead of **sExpr
					|  ^ preallocate a large number of select list items, and realloc in the event the number exceeds. Suggest say 2000 items. 
					|    -- problem with that is that the sExprs can be arbitrarily complex...how deep to pre-allocate them?
					|----------------------*/
			
					/* idea
						use the makenode() functions as constructors, 	
						replace explicit calls to malloc() here with something like "addnode()" 
						which handles all the allocation, does pre-allocation for performance, etc
					*/
		  
					} 
;





select_list_item:
	scalar_expr {

			$$ = MAKENODE(selectListItemNode);
			$$->isWildcard = 0;
			$$->sExpr = $1;	
			$$->hasAlias = 0;
			$$->sAlias = NULL;
		    }	 |
	MUL	    {
			$$ = MAKENODE(selectListItemNode);
			$$->isWildcard = 1;
			$$->hasAlias = 0;
			$$->sAlias = NULL;
		    }	 |
	scalar_expr AS IDENTIFIER {
		   	 
			$$ = MAKENODE(selectListItemNode);
			$$->isWildcard = 0;
			$$->sExpr = $1;
			$$->hasAlias = 1;
			$$->sAlias = $3;
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
		/* 	This test enforces the root node of the 
			s-expression supplied to WHERE to be one with a boolean value output.
			
			TODO: investigate better ways of enforcing this
		*/
				if ( sexpr_is_boolean($2) == true ) {
				$$ = MAKENODE(whereClauseNode);
				$$->expr = $2;
				} else {
					printf("Can't supply non-boolean value to WHERE");
					YYERROR;
				}
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
			$$->nElements = 1;
			$$->list.nElements = 1;
			$$->tables =  malloc ( sizeof(tableRefNode) * 20); //TEMPORARY! FIX ASAP. 
			*($$->tables) = $1;
		  }
		  |
	table_ref_list COMMA table_ref {
			*($$->tables + ($$->nElements)) = $3;			
			$$->nElements++;
			$$->list.nElements++;
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
				  $$->value.value.oper_val = _ADD;	
				}		|
				
	scalar_expr MUL scalar_expr 		
				{
				  $$ = MAKENODE(scalarExpr);
				  $$->left = $1;
				  $$->right = $3;
				  $$->value.type = OPER;
				  $$->value.value.oper_val = _MUL;	
			
				}		|
	
	scalar_expr DIV scalar_expr 		
				{
				  $$ = MAKENODE(scalarExpr);
				  $$->left = $1;
				  $$->right = $3;
				  $$->value.type = OPER;
				  $$->value.value.oper_val = _DIV;	
			
				}		|

	scalar_expr MOD scalar_expr 		
				{
				  $$ = MAKENODE(scalarExpr);
				  $$->left = $1;
				  $$->right = $3;
				  $$->value.type = OPER;
				  $$->value.value.oper_val = _MOD;	
			
				}		|

	scalar_expr AND scalar_expr 		
				{
				  $$ = MAKENODE(scalarExpr);
				  $$->left = $1;
				  $$->right = $3;
				  $$->value.type = OPER;
				  $$->value.value.oper_val = _AND;	
			
				}		|

	scalar_expr OR scalar_expr 		
				{
				  $$ = MAKENODE(scalarExpr);
				  $$->left = $1;
				  $$->right = $3;
				  $$->value.type = OPER;
				  $$->value.value.oper_val = _OR;	
			
				}		|

	scalar_expr EQ scalar_expr 		
				{
				  $$ = MAKENODE(scalarExpr);
				  $$->left = $1;
				  $$->right = $3;
				  $$->value.type = OPER;
				  $$->value.value.oper_val = _EQ ;	
			
				}		|

	scalar_expr NE scalar_expr 		 
				{
				  $$ = MAKENODE(scalarExpr);
				  $$->left = $1;
				  $$->right = $3;
				  $$->value.type = OPER;
				  $$->value.value.oper_val = _NE;	
			
				}		|

	scalar_expr GT scalar_expr 		
				{
				  $$ = MAKENODE(scalarExpr);
				  $$->left = $1;
				  $$->right = $3;
				  $$->value.type = OPER;
				  $$->value.value.oper_val = _GT;	
			
				}		|

	scalar_expr LT scalar_expr 		
				{
				  $$ = MAKENODE(scalarExpr);
				  $$->left = $1;
				  $$->right = $3;
				  $$->value.type = OPER;
				  $$->value.value.oper_val = _LT;	
			
				}		|

	scalar_expr GE scalar_expr 		
				{
				  $$ = MAKENODE(scalarExpr);
				  $$->left = $1;
				  $$->right = $3;
				  $$->value.type = OPER;
				  $$->value.value.oper_val = _GTE;	
			
				}		|
	scalar_expr LE scalar_expr 	
				{
				  $$ = MAKENODE(scalarExpr);
				  $$->left = $1;
				  $$->right = $3;
				  $$->value.type = OPER;
				  $$->value.value.oper_val = _LTE;	
			
				}		|

	scalar_expr SUB scalar_expr 	
				{
				  $$ = MAKENODE(scalarExpr);
				  $$->left = $1;
				  $$->right = $3;
				  $$->value.type = OPER;
				  $$->value.value.oper_val = _SUB;	
				}		|
	scalar_expr IN LPAREN in_predicate RPAREN
				{
				  $$ = MAKENODE(scalarExpr);
				  $$->left = $1;
				  $$->right = $4;
				  $$->value.type = OPER;
				  $$->value.value.oper_val = _IN;	
				}		|	
	scalar_expr NOT IN LPAREN in_predicate RPAREN
				{
				  $$ = MAKENODE(scalarExpr);
				  $$->left = $1;
				  $$->right = $5;
				  $$->value.type = OPER;
				  $$->value.value.oper_val = _NOT_IN;	
				}		|	
	scalar_expr BETWEEN scalar_expr AND scalar_expr
				{
				/*
				   This action rewrites the BETWEEN construct as a boolean
				   expression. 

				   A BETWEEN R1 AND R2 

				   becomes

				   (A >= R1) AND (A <= R2)

				 */
			

				/* We can also consider replacing this with a construct like:
			
				scalar_expr BETWEEN scalar_expr

				checking in the action that the $3 scalar_expr is an operator
				node with the value AND (in a similar manner to the way the 
				check for boolean value for the WHERE clause predicate works.)

				This might need some fancy mid rule action or token pushback in the
				normal AND rule.

				*/
	
				  debug("BETWEEN statement found");
				  $$ = MAKENODE(scalarExpr);
				  $$->value.type = OPER;
				  $$->value.value.oper_val = _AND;	

				  $$->left = MAKENODE(scalarExpr);
				  $$->left->value.type = OPER;
				  $$->left->value.value.oper_val = _GTE;

				  $$->left->left = $1;
				  $$->left->right = $3;

				  $$->right = MAKENODE(scalarExpr);
				  $$->right->value.type = OPER;
				  $$->right->value.value.oper_val = _LTE;
	
				  $$->right->left = $1; //Are actions repeated when $1 is referred to again?
				  $$->right->right = $5;  
				  	

				}		|	
	scalar_expr NOT BETWEEN between_predicate
				{
				  $$ = MAKENODE(scalarExpr);
				  $$->left = $1;
				  $$->right = $4;
				  $$->value.type = OPER;
				  $$->value.value.oper_val = _NOT_BETWEEN;	
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
		  $$.type = _TEXT;
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

/* Cheating slightly, we treat the in_predicate as a specialised s_expression
	It seems to make more sense to do this than to make the in_predicate a value
	expression itself in the grammar, even though we store the in_predicate data
	in a C valueExprNode

	IN-list can be a list of literal items, or a query expression
 */

in_predicate:
	scalar_expr {
				  $$ = MAKENODE(scalarExpr);
				  $$->left = NULL;
				  $$->right = NULL;
				  $$->value.type = IN_LIST;
				  $$->value.value.in_list_val = MAKENODE(inListNode);
				  $$->value.value.in_list_val->sItems = malloc ((size_t) sizeof (selectListItemNode *) * 20); 
					//TEMP fixed size of 20 here, to debug issues with this;

				/* all the below messing around with long struct references are silly,
				   should all be replaced with some getter/setter function
				   or encapsulated "nodeadd()"  */
			
			 	debug("First Scalar expr in IN list!");
				$$->value.value.in_list_val->nElements = 1; 
				*($$->value.value.in_list_val->sItems) = $1;
				
		} |
	in_predicate COMMA scalar_expr {
				debug("IN list recursive scalar expr!");
				*($$->value.value.in_list_val->sItems + ($$->value.value.in_list_val->nElements)) = $3; //remove redundant parentheses
				$$->value.value.in_list_val->nElements++;
	}
;

between_predicate:
	scalar_expr AND scalar_expr {
				$$ = MAKENODE(scalarExpr);
				$$->left = NULL;
				$$->right = NULL;	
				$$->value.type = BETWEEN_PREDICATE;
				$$->value.value.between_pred_val = MAKENODE(betweenPredNode);
				$$->value.value.between_pred_val->rangeStart = $1;
				$$->value.value.between_pred_val->rangeEnd = $3;
};

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
			$$ = _TEXT;	
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
