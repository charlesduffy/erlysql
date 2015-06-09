%code requires {
#include <stdio.h>
#include <string.h>
#include "simtree.h"
typedef void *yyscan_t;
}

/* parser options */


%define api.pure full
%lex-param {yyscan_t scanner}
%parse-param {yyscan_t scanner}

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
	selectStmtNode *selectStmt;
	selectListNode *selectList;
	fromClauseNode *fromClause;
	valueExprNode  *valueExpr;
	colName0       *columnName;
}	

%code{
void yyerror (yyscan_t scanner, char const *s) {
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

%type	<node> 	sql query_statement 

%type 	<selectStmt> select_statement
%type 	<selectList> select_list
%type 	<fromClause> from_clause
%type 	<valueExpr> value_expr
%type 	<scalarExpr> scalar_expr
%type   <columnName> colref
%token  <identifier_val>  IDENTIFIER

%%

sql:
	|
	query_statement SEMICOLON query_statement 
;

query_statement:
	select_statement  	|
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
	select_list COMMA select_list |
	scalar_expr
;
	

table_expr:
	table_expr COMMA table_expr |
	IDENTIFIER
;

select_statement:
	SELECT select_list from_clause where_clause { $$ = mkSelectStmtNode(); }
						      
;

where_clause:
				|
	WHERE scalar_expr
;

from_clause:
				|
	FROM table_expr { $$ = mkFromClauseNode(); }
;

/*

EXPRESSIONS

*/


scalar_expr:
	value_expr				|
	LPAREN scalar_expr RPAREN		|
	scalar_expr ADD scalar_expr 		|
	scalar_expr MUL scalar_expr 		|
	scalar_expr DIV scalar_expr 		|
	scalar_expr MOD scalar_expr 		|
	scalar_expr AND scalar_expr 		|
	scalar_expr OR scalar_expr 		|
	scalar_expr EQ scalar_expr 		|
	scalar_expr NE scalar_expr 		| 
	scalar_expr GT scalar_expr 		|
	scalar_expr LT scalar_expr 		|
	scalar_expr GE scalar_expr 		|
	scalar_expr LE scalar_expr 	
;

value_expr:
	colref	{ 
			valueExprNode *n = mkValueExpr(); 
			//printf("parser. size of value expression N: %ld\n", sizeof(n));
			n->type = COLREF;
			n->value.colName = (char *) $1;
			//n->value.colName = strdup((const char *) yylval);
			$$ = (valueExprNode *) n; 
		}				|
	
	INTEGER	{
			
			valueExprNode *node = mkValueExpr();
			node->type = INT;
			node->value.integer_val = $1;
			
		}	 |	
	NUMERIC{
			
			valueExprNode *node = mkValueExpr();
			node->type = NUM;
			node->value.numeric_val = $1;
			$$ = (valueExprNode *) node;
			
		}	|	
	STRING  {
			
			valueExprNode *node = mkValueExpr();
			node->type = TEXT;
			node->value.text_val = $1;
			$$ = (valueExprNode *) node;
			
		}		
;



colref:
	IDENTIFIER { $$ = (char *) $1; }
;

%%
