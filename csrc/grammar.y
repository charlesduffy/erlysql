%code requires {
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdbool.h>
#include "structures.h"
#include "dbglog.h"
 
typedef void *yyscan_t;

}

/* parser options */

%define api.pure full
%lex-param {yyscan_t scanner}
%parse-param {yyscan_t scanner} {tuple * ptree}

%locations

%union 
{
	int			integer_val;
	char 			*text_val;
	float  			float_val;
	char			*keyword;
	char    		*identifier_val;

	s_expr			*sExpr;	
	tuple			*Tuple;
}	

%code{

  void yyerror (YYLTYPE *l, yyscan_t scanner, tuple *mqry, char const *s) {
       //mqry->errFlag = 1;
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

%type <Tuple>	sql query_statement select_statement select_list select_list_item from_clause table_ref
		table_ref_list value_expr colref where_clause table_expr
		column_definition column_definition_list data_type insert_statement insert_value_list column_list
		ddl_table_ref create_table_stmt drop_table_stmt in_predicate

%type <sExpr>	scalar_expr 	

%token  <identifier_val>  IDENTIFIER

%%

/* this node is a multi-statement submission delimited by semicolon */

sql:
    query_statement SEMICOLON
    {
	new_tuple($$, v_tuple, "query", $1);
	ptree = $$;
    }
    |
    sql query_statement SEMICOLON
    {
	tuple_append($$, v_tuple, "query", $2);
    }
;

/* this node is a single query statement */

query_statement:
    select_statement 
    { 
	new_tuple($$, v_text, "statement_type", "select_statement");
	tuple_append($$, v_tuple, "select_statement", $1);
    } 
    |
    insert_statement	
    { 
	new_tuple($$, v_text, "statement_type", "insert_statement");
	tuple_append($$, v_tuple, "insert_statement", $1);
    } 
    |
    create_table_stmt
    {
	new_tuple($$, v_text, "statement_type", "create_table_statement");
	tuple_append($$, v_tuple, "create_table_statement", $1);
    }
    |
    drop_table_stmt
    {
	new_tuple($$, v_text, "statement_type", "drop_table_statement");
	tuple_append($$, v_tuple, "drop_table_statement", $1);
    }
//TODO consider using a more generic "DDL stmt" rather than explicitly identifying every kind of DDL operation
;

/* 

INSERT STATEMENT


*/



insert_statement:
    INSERT INTO ddl_table_ref LPAREN column_list RPAREN VALUES LPAREN insert_value_list RPAREN 
    { 
    }
    |
    INSERT INTO ddl_table_ref VALUES LPAREN insert_value_list RPAREN 
    {
    }
    |
    INSERT INTO ddl_table_ref select_statement 
    {
    }
;

column_list:
    IDENTIFIER
    {
    } 
    |
    column_list COMMA IDENTIFIER
    {
    }
;	

insert_value_list:
	scalar_expr { 
	}	
	|		
	insert_value_list COMMA scalar_expr {	
	}
;


	
/*

SELECT STATEMENT

*/

select_list:
    select_list_item
    {
	new_tuple($$, v_tuple, "select_list_item", $1);
    }
    |
    select_list COMMA select_list_item
    { 
	list_append($$,$3);
    } 
;

select_list_item:
    scalar_expr
    {
	new_tuple($$, v_sexpr, "value", $1);	
    }
    |
    MUL
    {
	new_tuple($$, v_text, "value", "wildcard");
    }	 
    |
    scalar_expr AS IDENTIFIER
    {
	new_tuple($$, v_sexpr, "value", $1);	
	tuple_append($$, v_text, "alias", $3); 
    }
;

select_statement:
    SELECT select_list table_expr
    {
	new_tuple($$, v_tuple, "select_list", $2);
	tuple_append($$, v_tuple, "table_expr", $3);
    }
;

where_clause:
    WHERE scalar_expr 
    {
    //TODO replace check for boolean expression 
    new_tuple($$, v_sexpr, "where_clause", $2); 
    }
;

from_clause:
	FROM table_ref_list { 
				new_tuple($$, v_tuple, "from_clause", $2);
			    }
;

table_ref:
    IDENTIFIER
    {
	new_tuple($$, v_text, "name", $1);
    }
    |
    IDENTIFIER IDENTIFIER
    {
	new_tuple($$, v_text, "name", $1);
	tuple_append($$, v_text, "alias", $2);
    }
    |
    IDENTIFIER AS IDENTIFIER
    {
	new_tuple($$, v_text, "name", $1);
	tuple_append($$, v_text, "alias", $3);
    }
;

table_ref_list:
    table_ref
    {
	new_tuple($$, v_tuple, "table", $1);
    }
    |
    table_ref_list COMMA table_ref
    {
	list_append($$,$3);	
    }
;

table_expr:
    from_clause
    {
	//possib from lower element
	new_tuple($$, v_tuple, "from_clause", $1);
    }
    |
    from_clause where_clause
    {
	new_tuple($$, v_tuple, "from_clause", $1);
	tuple_append($$, v_tuple, "where_clause", $2); 
    }
;


/*

EXPRESSIONS

*/


scalar_expr:
    value_expr
    { 
	$$ = MAKENODE(s_expr);
	$$->value = $1;
	$$->left = NULL;
	$$->right = NULL;
	$$->list.next = NULL;
    }
    |
    LPAREN scalar_expr RPAREN
    { 
	$$ = MAKENODE(s_expr);
	$$->left = $2;
	$$->right = NULL;	 //huh?? re-think the logic of this rule		  
    }
    |
    scalar_expr ADD scalar_expr 
    {
	mk_s_expr_oper($$, "ADD", $1, $3);
    }
    |
    scalar_expr MUL scalar_expr 		
    {
	mk_s_expr_oper($$, "MUL", $1, $3);
    }
    |
    scalar_expr DIV scalar_expr 		
    {
	mk_s_expr_oper($$, "DIV", $1, $3);
    }
    |
    scalar_expr MOD scalar_expr 		
    {
	mk_s_expr_oper($$, "MOD", $1, $3);
    }
    |
    scalar_expr AND scalar_expr 		
    {
	mk_s_expr_oper($$, "AND", $1, $3);
    }
    |
    scalar_expr OR scalar_expr 		
    {
	mk_s_expr_oper($$, "OR", $1, $3);
    }
    |
    scalar_expr EQ scalar_expr 		
    {
	mk_s_expr_oper($$, "EQ", $1, $3);
    }
    |
    scalar_expr NE scalar_expr 		 
    {
	mk_s_expr_oper($$, "NE", $1, $3);
    }
    |
    scalar_expr GT scalar_expr 		
    {
	mk_s_expr_oper($$, "GT", $1, $3);
    }
    |
    scalar_expr LT scalar_expr 		
    {
	mk_s_expr_oper($$, "LT", $1, $3);
    }
    |
    scalar_expr GE scalar_expr 		
    {
	mk_s_expr_oper($$, "GE", $1, $3);
    }
    |
    scalar_expr LE scalar_expr 	
    {
	mk_s_expr_oper($$, "LE", $1, $3);
    }
    |
    scalar_expr SUB scalar_expr 	
    {
	mk_s_expr_oper($$, "SUB", $1, $3);
    }
    |
    scalar_expr IN LPAREN in_predicate RPAREN
    {
    }
    |	
    scalar_expr NOT IN LPAREN in_predicate RPAREN
    {
    }
    |	
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

	
				  $$ = MAKENODE(s_expr);
				  $$->value.type = OPER;
				  $$->value.value.oper_val = _AND;	

				  $$->left = MAKENODE(s_expr);
				  $$->left->value.type = OPER;
				  $$->left->value.value.oper_val = _GTE;

				  $$->left->left = $1;
				  $$->left->right = $3;

				  $$->right = MAKENODE(s_expr);
				  $$->right->value.type = OPER;
				  $$->right->value.value.oper_val = _LTE;
	
				  $$->right->left = $1; 
				  $$->right->right = $5;  
				*/

				}	
;

value_expr:
	colref  { 
		    $$ = $1;
		}
		|
	INT_LIT {
		    mk_tuplist_lit($$, v_int, "INT", $1);
		}
		|	
	NUM_LIT {
		    mk_tuplist_lit($$, v_float, "NUM", $1);
		}
		|	
	STRING  {
		    mk_tuplist_lit($$, v_text, "INT", $1);
		}
;

colref:
	IDENTIFIER 
	{ 
	        mk_tuplist_ident($$, NULL, $1);
	}
	|
	IDENTIFIER POINT IDENTIFIER  
	{
		mk_tuplist_ident($$, $1, $3);
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
		} |
	in_predicate COMMA scalar_expr {
	}
;


/* Data definition language commands */

/* Drop Table */

drop_table_stmt:
	DROP TABLE ddl_table_ref
	{
	}
;

/* Create Table */

ddl_table_ref:
	IDENTIFIER 
	{
	}
	|
	IDENTIFIER POINT IDENTIFIER  
	{
	}
;

data_type:
	INTEGER	
		{
		}
		|
	NUMERIC 
		{
		}
		|
	CHAR	{
		}
;

create_table_stmt:
	CREATE TABLE ddl_table_ref LPAREN column_definition_list RPAREN
	{
	}
;

column_definition_list:
	column_definition
	{
	} 
	|
	column_definition_list COMMA column_definition
	{
	}
;

column_definition: 
	IDENTIFIER data_type
	{
	}

;

%%
