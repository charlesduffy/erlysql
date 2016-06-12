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
%token <keyword> COUNT SET INTO TABLE WITH ORDER BY HAVING GROUP CASE WHEN THEN END
%token <keyword> ELSE DESC ASC FIRST LAST NULLS _NULL TRUE FALSE IS NOT UNKNOWN

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

%left           OR
%left           AND
%left		NE
%left 		IN
%right		NOT
%right		EQ
%nonassoc	LT GT
%nonassoc	LE GE
%nonassoc	BETWEEN
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

%type <Tuple>	sql query_statement select_statement select_list u_select_list_item select_list_item table_ref
		table_ref_list value_expr colref table_expr
		function case_expr case_expr_when_list case_expr_when from_clause 
		order_by_list order_by_list_item order_by_clause
		column_definition column_definition_list data_type insert_statement insert_value_list column_list
		ddl_table_ref create_table_stmt drop_table_stmt in_predicate

%type <sExpr>	scalar_expr group_by_clause having_clause where_clause 

%type <keyword>	order_by_direction order_by_nulls boolean sqlval

%token  <identifier_val>  IDENTIFIER 

%%

/* this node is a multi-statement submission delimited by semicolon */

sql:
    query_statement SEMICOLON
    {
	$$ = ptree;
	$$->type = v_tuple;
	$$->v_tuple = $1;
	$$->tag = "query";
	$$->list.next = NULL;	
	$$->list.prev = NULL;	
    }
    |
    sql query_statement SEMICOLON
    {
	tuple_append(ptree , v_tuple, "query", $2);
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
	tuple_append($$,v_tuple, "select_list_item", $3);
    } 
;

select_list_item:
    u_select_list_item
    {
	$$=$1;
    }
    |
    u_select_list_item AS IDENTIFIER
    {
	$$=$1;
	tuple_append($$, v_text, "alias", $3); 
    }
;

u_select_list_item:
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
    LPAREN select_statement RPAREN
    {
	new_tuple($$, v_tuple, "subquery", $2);
    }
;

select_statement:
    SELECT select_list table_expr
    {
	new_tuple($$, v_tuple, "select_list", $2);
	tuple_append($$, v_tuple, "table_expr", $3);
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
	tuple_append($$,v_tuple, "table", $3);	
    }
;

from_clause:
    FROM table_ref_list
    {
	$$=$2;
    }
;

where_clause:
    %empty
    {
	$$=NULL;
    }
    |
    WHERE scalar_expr
    {
	$$=$2;
    }
;

having_clause:
    %empty
    {
	$$=NULL;
    }
    |
    HAVING scalar_expr
    {
	$$=$2;
    }
;

order_by_clause:
    %empty
    {
	$$=NULL;
    }
    |
    ORDER BY order_by_list
    {
	$$=$3;
    }
;

order_by_list:
    order_by_list_item
    {
	new_tuple($$, v_tuple, "order_by_expression", $1);
    }
    |
    order_by_list COMMA order_by_list_item
    {
	tuple_append($$, v_tuple, "order_by_expression", $3);
    }
;

order_by_list_item:
    scalar_expr order_by_direction order_by_nulls  
    {
	new_tuple($$, v_sexpr, "value", $1);	
	if ($2 != NULL) tuple_append($$, v_text, "direction", $2); 
	if ($3 != NULL) tuple_append($$, v_text, "nulls", $3); 
    }
;

order_by_direction:
    %empty
    {
	$$=NULL;
    }
    |
    ASC
    {
	$$="asc";
    }
    |
    DESC
    {
	$$="desc";
    }
;

order_by_nulls:
    %empty
    {
	$$=NULL;
    }
    |
    NULLS FIRST
    {
	$$="first";
    }
    |
    NULLS LAST
    {
	$$="last";
    }
; 

group_by_clause:
    %empty
    {
	$$=NULL;
    }
    |
    GROUP BY scalar_expr
    {
	$$=$3;
    }
;

table_expr:
    from_clause where_clause group_by_clause having_clause order_by_clause
    {
	new_tuple($$, v_tuple, "from_clause", $1);
	if ($2 != NULL) tuple_append($$, v_sexpr, "where_clause", $2); 
	if ($3 != NULL) tuple_append($$, v_sexpr, "group_by_clause", $3); 
	if ($4 != NULL) tuple_append($$, v_sexpr, "having_clause", $4); 
	if ($5 != NULL) tuple_append($$, v_tuple, "order_by_clause", $5); 
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
	$$->list.prev = NULL;
    }
    |
    LPAREN scalar_expr RPAREN
    { 
	$$ = $2;
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
    scalar_expr BETWEEN scalar_expr
    {
    }
    |
    scalar_expr NOT BETWEEN scalar_expr
    {
    }
    |
    scalar_expr IS scalar_expr
    {
	mk_s_expr_oper($$, "IS", $1, $3);
    }
    |
    scalar_expr IS NOT scalar_expr
    {
	mk_s_expr_oper($$, "ISNOT", $1, $4);
    }
;

value_expr:
	colref
	{ 
	    $$=$1;
	}
	|
	boolean
	{
	    mk_tuplist_lit($$, v_text, "BOOL", $1);
	}
	|
	sqlval
	{
	    mk_tuplist_lit($$, v_text, "SQLV", $1);
	}
	|
	INT_LIT
	{
	    mk_tuplist_lit($$, v_int, "INT", $1);
	}
	|	
	NUM_LIT 
	{
	    mk_tuplist_lit($$, v_float, "NUM", $1);
	}
	|	
	STRING
	{
	    mk_tuplist_lit($$, v_text, "TEXT", $1);
	}
	|
	function
	{
	    $$=$1;
	}
;

boolean:
    TRUE    
    {
	$$="true";	
    }
    |
    FALSE
    {
	$$="false";	
    }
;

sqlval:
    _NULL
    {
	$$="sqlnull";	
    }
    |
    UNKNOWN
    {
	$$="unknown";	
    }
;

function:
    case_expr 	
    {
	$$=$1;
    };
//    |
//    builtin_func
//    {
//    }


case_expr:
    CASE case_expr_when_list ELSE scalar_expr END 
    {
	new_tuple($$, v_tuple, "when_list", $2);
	tuple_append($$, v_sexpr, "else", $4);
    }
    |
    CASE case_expr_when_list END
    {
	new_tuple($$, v_tuple, "when_list", $2);
    }
;

case_expr_when_list:
    case_expr_when
    {
	new_tuple($$, v_tuple, "when", $1);
    }
    |
    case_expr_when_list case_expr_when
    {
	tuple_append($$, v_tuple, "when", $2);
    }
;

case_expr_when:
    WHEN scalar_expr THEN scalar_expr
    {
	new_tuple($$, v_sexpr, "condition", $2);
	tuple_append($$, v_sexpr, "result", $4);	
    }
; 

//builtin_func:
//;


colref:
	IDENTIFIER 
	{ 
                new_tuple($$,v_text,"class","identifier");  
		tuple_append($$, v_text, "value", $1);
	}
	|
	IDENTIFIER POINT IDENTIFIER  
	{
		mk_tuplist_ident($$, $1, $3);
	}
;

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
