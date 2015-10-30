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
	int	integer_val;
	char 	*text_val;
	float  float_val;
	/* keyword */
	char	*keyword;
	/* identifier */
	char    *identifier_val;

	/* node types */
	queryNode *query;
	selectStmtNode *selectStmt;
	selectListNode *selectList;
	fromClauseNode *fromClause;
	tableRefNode   *tableExpr;
	valueExprNode  valueExpr;
	scalarExpr *sExpr;
	whereClauseNode *whereClause;
	char 	       *columnName;
}	

%code{
void yyerror (yyscan_t scanner, queryNode *qry, char const *s) {
     fprintf (stderr, ">>> %s <<<\n", s); }
}

/* SQL keywords */
%token <keyword> SELECT INSERT UPDATE DELETE WHERE FROM VALUES CREATE DROP SUM COUNT SET INTO

/* values and identifiers */
%token <keyword> BIGINT 
%token <integer_val> INTEGER 
%token <float_val> NUMERIC
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
//%left         '.'

%type	<query>	query_statement
%type 	<selectStmt> select_statement
%type 	<selectList> select_list
%type 	<fromClause> from_clause
%type 	<valueExpr> value_expr
%type 	<sExpr> scalar_expr
%type   <columnName> colref
%type   <whereClause> where_clause
%type 	<tableExpr> table_expr

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
		 		//here we allocate enough memory for a single pointer to a pointer-to-sExp.
		 		//we realloc it later for every new select list element encountered. Kinda inefficent.
		 		$$->sExpr = malloc ((size_t) sizeof (scalarExpr*) * 20); //TEMP fixed size of 20 here, to debug issues with this

			 	debug("First Scalar expr in select list!");
				$$->nElements = 1;
				*($$->sExpr) = $1;
				//scalarExpr *SK = *($$->sExpr);				
				//printf("scalar_expr: integer_value: %d\n\r" , SK->value.value.integer_val);
			  } |
	select_list COMMA scalar_expr  { debug("recursive scalar expr!");

					  /* here's where it gets tricky...
					 
					  1. We increment the element count */

					  $$->nElements++;

					  /* 

					  2. Then we enlarge the size of the array of pointers-to-sExprs by one. 

				 	     Consider doing this by getting the current size of the block of memory
					     with sizeof() then adding the size of a *scalarExpr rather than using
					     the element count.
					 */

			//		  $$->sExpr = realloc ($$->sExpr, (size_t) sizeof(scalarExpr*) * $$->nElements);
					  //temporarily using fixed size array to debug
	
					  /*then we use the element count (adjusted by minus one) as the offset into the array, and
					  assign the new sExpr (which comes from the third rule element) */
	
					  *($$->sExpr + ($$->nElements - 1)) = $3;

				//	scalarExpr *SK = *($$->sExpr + ($$->nElements - 1));				
				//	printf("scalar_expr: integer_value: %d\n\r" , SK->value.value.integer_val);
					/*----------------------
					|  this all kind of sux, of course.
					|  suggestions for replacement:
					|  ^ use a linked list instead of **sExpr
					|----------------------*/
					  
					} 
;
	
table_expr:
	IDENTIFIER {
			$$ = MAKENODE(tableRefNode);
			$$->tableName = $1;
		   } |
	table_expr COMMA IDENTIFIER 
;

select_statement:
	SELECT select_list from_clause where_clause { 
				$$ = MAKENODE(selectStmtNode); 
				$$->selectList = $2;
				$$->fromClause = $3;
				$$->whereClause = $4;
}
						      
;

where_clause:
			{ 
				$$ =  NULL; 
			}
			|
	WHERE scalar_expr 
			{
				$$ = MAKENODE(whereClauseNode);
				$$->expr = $2;
			 }
;

from_clause:
				|
	FROM table_expr { 
				$$ = MAKENODE(fromClauseNode); 
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
				{ $$ = MAKENODE(scalarExpr);
				  $$->left = $2;
				  $$->right = NULL;			  
				}		|
	scalar_expr ADD scalar_expr 
				{
				  //printf("I am adding\n");
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
	colref	{ 
			$$.type = COLREF;
			$$.value.colName = (char *) $1;
			debug("value_expr in parser. Colref value is: %s", $$.value.colName);
		}				|
	
	INTEGER	{
			$$.type = INT;
			$$.value.integer_val = $1;
			debug("value_expr in parser. Integer value is: %d", $$.value.integer_val);
		}	 |	
	NUMERIC{
			
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
	IDENTIFIER { $$ = (char *) $1; }
;

%%
