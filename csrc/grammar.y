%code requires {

#define YYERROR_VERBOSE 1
#include <stdio.h>
#include "simtree.h"



  void yyerror (char const *s) {
     fprintf (stderr, ">>> %s <<<\n", s);
  }
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
	/* keyword */
	char	*keyword;
	/* node types */
	ParseNode *node;
}	

/* SQL keywords */
%token <keyword> SELECT INSERT UPDATE DELETE WHERE FROM VALUES CREATE DROP SUM COUNT SET INTO

/* values and identifiers */
%token <keyword> INT BIGINT NUMERIC STRING IDENTIFIER

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

%type	<node> 	sql query_statement select_statement scalar_expr value_expr from_clause

%%

sql:
	|
	query_statement SEMICOLON query_statement 
;

query_statement:
	select_statement 	|
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
	SELECT select_list from_clause where_clause
;

where_clause:
				|
	WHERE scalar_expr
;

from_clause:
				|
	FROM table_expr
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
	colref					|
	literal		
;

literal:
	number |
	string 
;

number:
	INT | 
	BIGINT | 
	NUMERIC
;

string:
	STRING
;

colref:
	IDENTIFIER
;

%%
