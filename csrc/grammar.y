%code requires {
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "simtree.h"

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
			  queryNode *node = MAKENODE(queryNode);
			  $$ = node;
			  $$->selnode = $1;
			  
			 } 
		
;

/* 

INSERT STATEMENT

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
	scalar_expr		       |
	COMMA scalar_expr  |   
;
	

table_expr:
	table_expr COMMA table_expr |
	IDENTIFIER
;

select_statement:
	SELECT select_list from_clause where_clause { 
				selectStmtNode *node = MAKENODE(selectStmtNode); 
				node->selectList = $2;
				node->fromClause = $3;
				node->whereClause = $4;
				$$=node;	
}
						      
;

where_clause:
			{ 
				$$ =  NULL; 
			}
			|
	WHERE scalar_expr 
			{
				whereClauseNode *node = MAKENODE(whereClauseNode);
				node->expr = $2;
				$$ = (whereClauseNode *) node;			  	
			 }
;

from_clause:
				|
	FROM table_expr { $$ = MAKENODE(fromClauseNode); }
;


/*

EXPRESSIONS

*/


scalar_expr:
	value_expr  { $$ = MAKENODE(scalarExpr);
		      $$->value = $1;
		      $$->left = NULL;
		      $$->right = NULL;
		    }				|
	LPAREN scalar_expr RPAREN
				{ $$ = MAKENODE(scalarExpr);
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
			
				}		
;

value_expr:
	colref	{ 
			$$.type = COLREF;
			$$.value.colName = (char *) $1;
		}				|
	
	INTEGER	{
			
			$$.type = INT;
			$$.value.integer_val = $1;
			printf("INTNODE!!! %d\n", $1);
			
		}	 |	
	NUMERIC{
			
			$$.type = NUM;
			$$.value.numeric_val = $1;
			
		}	|	
	STRING  {
			
			$$.type = TEXT;
			$$.value.text_val = $1;
		}		
;



colref:
	IDENTIFIER { $$ = (char *) $1; }
;

%%
