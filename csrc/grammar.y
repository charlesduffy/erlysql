/* Infix notation calculator--calc */

%{
#define YYSTYPE int //fix!
#define YYDEBUG 1
#define YYERROR_VERBOSE 1
#include <stdio.h>
//yydebug=1;
void yyerror (char const *s) {
   fprintf (stderr, ">>> %s <<<\n", s);
 }

%}



/* SQL keywords */
%token SELECT INSERT UPDATE DELETE WHERE FROM VALUES CREATE DROP AND OR SUM COUNT SET INTO
/* operators */
%token ADD MUL DIV MOD EXP EQ LT GT NE GE LE
/* values and identifiers */
%token INT BIGINT NUMERIC STRING IDENTIFIER

/* punctuation */
%token QUOTE SEMICOLON COMMA LPAREN RPAREN NEWLINE ASTERISK


%left OR
%left AND
%left NOT
%left EQ LT GT NE GE LE
%left ADD SUB
%left MUL DIV
%right EXP    /* exponentiation */

/* Grammar follows */

%%

sql:
	query_statement SEMICOLON  	  	|
	query_statement SEMICOLON query_statement 
;

query_statement:
	select_statement 	|
	insert_statement
;

insert_statement:
	INSERT INTO IDENTIFIER VALUES LPAREN value_list RPAREN
;

value_list:
	LPAREN scalar_expr RPAREN   		|
	scalar_expr scalar_operator scalar_expr |
	scalar_expr COMMA scalar_expr		|
	scalar_expr			
;
	



select_list:
	select_list COMMA select_list |
	ASTERISK		      |
	scalar_expr
;
	

table_expr:
	table_expr COMMA table_expr |
	IDENTIFIER
;

select_statement:
	SELECT select_list 		   |
	SELECT select_list FROM table_expr where_clause
;

where_clause:
				|
	WHERE predicate_expr
;

comparison_operator:
	GT |
	LT | 
	EQ |
	NE 
;

boolean_operator:
	AND 	|
	OR	|	
	NOT
;

scalar_operator:
	ADD |
	SUB |
	MUL |
	DIV 
;

predicate_expr:
	LPAREN predicate_expr RPAREN 				|
	predicate_expr comparison_operator predicate_expr 	|
	predicate_expr	boolean_operator predicate_expr		|
	atom							|
	colref
;

scalar_expr:
	LPAREN scalar_expr RPAREN   		|
	scalar_expr scalar_operator scalar_expr |
	colref 			   		|
	atom
;

atom:
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
