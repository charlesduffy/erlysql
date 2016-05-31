%code requires {
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stddef.h>
#include <stdbool.h>
#include "collections.h"
#include "parsetree.h"
#include "dbglog.h"



#define MAKENODE(nodetype) malloc((size_t) sizeof( nodetype ))

#define new(nodetype) new_##nodetype(malloc((size_t) sizeof( nodetype )))

#define mk_s_expr_val(p, v) { p=MAKENODE(s_expr);	\
			      p->value = v;		\
			      p->left = NULL;		\
			      p->right = NULL;		\
			      p->list = NULL;		\
			}

#define mk_s_expr_oper(p, v, l, r) { p=MAKENODE(s_expr);	\
			      mk_tuplist_oper(p->value, v);	\
			      p->left = l;			\
			      p->right = r;			\
			      p->list = NULL;			\
			}

//new_tuple(tuple *, tag, type, value)
#define new_tuple(p , t, T, v) { p=MAKENODE(tuple);	\
				 p->tag=t;		\
				 p->T=v;		\
			       }			\

		//here we need 2 macros
		// 1)   mk_tuplist_lit(sqltype, value)
		// 2)   mk_tuplist_ident(nametype, value)

#define mk_tuplist_lit(p, t, T, v) { x * tuple;				    \
				     y * tuple;				    \
				     new_tuple(p,t,"value",v);		    \
				     new_tuple(x,v_char,"class","literal"); \
				     new_tuple(y,v_char,"sqltype",T);	    \
				     list_append(p,x);			    \ 
				     list_append(x,y);			    \ 
				    }					    
/*
    mk_tuplist_ident (<pointer>, <alias>, <value>)

    make a tuplist describing a column reference

    <pointer>	    pointer to a struct tuple.
    <alias>	    column table reference or NULL if not present
    <value>	    name of the column, char *

*/

#define mk_tuplist_ident(p, A, v) {  x * tuple;					\
				     y * tuple;					\
				     new_tuple(p,v_char,"value",v);		\
				     new_tuple(x,v_char,"class","identifier");	\
				     new_tuple(y,v_char,"reference",A);		\
				     list_append(p,x);				\	 
				     list_append(x,y);				\ 
				    }						
				    
/*
    mk_tuplist_oper (<pointer>, <alias>, <value>)

    make a tuplist describing an operator 

    <pointer>	    pointer to a struct tuple.
    <value>	    name of the operator

*/

#define mk_tuplist_oper(p, v)	    {   x * tuple;				\
				     new_tuple(p,v_char,"value",v);		\
				     new_tuple(x,v_char,"class","operator");	\
				     list_append(p,x);				\	 
				    }						

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
	multiQueryNode		*multiQuery;
	queryNode 		*query;

	/* DML Nodes - SELECT */

	selectStmtNode 		*selectStmt;
	//selectListNode 		*selectListItem;
	selectListItemNode 	*selectListItem;
	fromClauseNode 		*fromClause;
	tableRefNode   		*tableRef;
	//tableRefListNode 	*tableRefList;
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

%type	<multiQuery>			sql
%type	<query>			query_statement
%type 	<selectStmt> 		select_statement
//%type 	<selectList> 		select_list
%type 	<selectListItem> 	select_list
%type 	<selectListItem>	select_list_item
%type 	<fromClause> 		from_clause
%type 	<tableRef> 		table_ref
//%type 	<tableRefList> 		table_ref_list
%type 	<tableRef> 		table_ref_list
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

%token  <identifier_val>  IDENTIFIER

%%

/* this node is a multi-statement submission delimited by semicolon */

sql:
    query_statement SEMICOLON
    {
	$$ = ptree;
	$$->query = $1;
	$$->query->list.next = NULL;					
    }
    |
    sql query_statement SEMICOLON
    {
	list_append($$->query, $2);
    }
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
	$$ = MAKENODE(queryNode); 	
	$$->statType = INSERT_STMT;
	$$->insnode = $1;
    } 
    |
    create_table_stmt
    {
	$$ = MAKENODE(queryNode); 	
	$$->statType = CREATE_TABLE_STMT;
	$$->crTabNode = $1;
    }
    |
    drop_table_stmt
    {
	$$ = MAKENODE(queryNode); 	
	$$->statType = DROP_TABLE_STMT;
	$$->drTabNode = $1;
    }
//TODO consider using a more generic "DDL stmt" rather than explicitly identifying every kind of DDL operation
;

/* 

INSERT STATEMENT


*/



insert_statement:
    INSERT INTO ddl_table_ref LPAREN column_list RPAREN VALUES LPAREN insert_value_list RPAREN 
    { 
	$$ = MAKENODE(insertStmtNode);
	$$->table = $3;
	$$->collist = $5;
	$$->vallist = $9;	
	$$->selnode = NULL;
    }
    |
    INSERT INTO ddl_table_ref VALUES LPAREN insert_value_list RPAREN 
    {
	$$ = MAKENODE(insertStmtNode);
	$$->table = $3;
	$$->collist = NULL;
	$$->vallist = $6;	
	$$->selnode = NULL;
    }
    |
    INSERT INTO ddl_table_ref select_statement 
    {
	$$ = MAKENODE(insertStmtNode);
	$$->table = $3;
	$$->collist = NULL;
	$$->vallist = NULL;	
	$$->selnode = $4;
    }
;

column_list:
    IDENTIFIER
    {
	$$ = MAKENODE(insertColListNode);
	$$->sItems = malloc ((size_t) sizeof (insertColListNode*) * 20);
	$$->nElements = 1;
	*($$->sItems) = $1;
    } 
    |
    column_list COMMA IDENTIFIER
    {
	*($$->sItems + ($$->nElements)) = $3;
	$$->nElements++;
    }
;	

insert_value_list:
	scalar_expr { 
		$$ = MAKENODE(insertValListNode);
		$$->sItems = malloc ((size_t) sizeof (insertValListNode*) * 20);
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
    select_list_item
    {
	$$ = $1;
	$$->list.next = NULL;
    }
    |
    select_list COMMA select_list_item
    { 
	list_append($$,$3);
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
    IDENTIFIER
    {
	$$ = MAKENODE(tableRefNode);
	$$->tableName = $1;
	$$->tableAlias = NULL;
    }
    |
    IDENTIFIER IDENTIFIER
    {
	$$ = MAKENODE(tableRefNode);
	$$->tableName = $1;
	$$->tableAlias = $2;
    }
    |
    IDENTIFIER AS IDENTIFIER
    {
	$$ = MAKENODE(tableRefNode);
	$$->tableName = $1;
	$$->tableAlias = $3;
    }
;

table_ref_list:
    table_ref
    {
	$$ = $1;
	$$->list.next = NULL;
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
	$$ = MAKENODE(tableExprNode);
	$$->fromClause = $1;	
	$$->whereClause = NULL;
    }
    |
    from_clause where_clause
    {
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
		      $$ = MAKENODE(s_expr);
		      $$->value = $1;
		      $$->left = NULL;
		      $$->right = NULL;
		      $$->list = NULL;
		    }
	        |
	LPAREN scalar_expr RPAREN
				{ 
				  $$ = MAKENODE(s_expr);
				  $$->left = $2;
				  $$->right = NULL;	 //huh?? re-think the logic of this rule		  
				}		|
	scalar_expr ADD scalar_expr 
				{
				 //#define mk_s_expr_oper(p, v, l, r) { p=MAKENODE(s_expr);	\
				 // $$ = MAKENODE(s_expr);
				 mk_s_expr_oper($$, "ADD", $1, $3);
				}		|
				
	scalar_expr MUL scalar_expr 		
				{
				  $$ = MAKENODE(s_expr);
				 mk_s_expr_oper($$, "MUL", $1, $3);
				}		|
	
	scalar_expr DIV scalar_expr 		
				{
				  $$ = MAKENODE(s_expr);
				 mk_s_expr_oper($$, "DIV", $1, $3);
				}		|

	scalar_expr MOD scalar_expr 		
				{
				  $$ = MAKENODE(s_expr);
				 mk_s_expr_oper($$, "MOD", $1, $3);
				}		|

	scalar_expr AND scalar_expr 		
				{
				  $$ = MAKENODE(s_expr);
				 mk_s_expr_oper($$, "AND", $1, $3);
				}		|

	scalar_expr OR scalar_expr 		
				{
				 mk_s_expr_oper($$, "OR", $1, $3);
				}		|

	scalar_expr EQ scalar_expr 		
				{
				 mk_s_expr_oper($$, "EQ", $1, $3);

				}		|

	scalar_expr NE scalar_expr 		 
				{
				 mk_s_expr_oper($$, "NE", $1, $3);
				}		|

	scalar_expr GT scalar_expr 		
				{
				 mk_s_expr_oper($$, "GT", $1, $3);
				}		|

	scalar_expr LT scalar_expr 		
				{
				 mk_s_expr_oper($$, "LT", $1, $3);
				}		|

	scalar_expr GE scalar_expr 		
				{
				 mk_s_expr_oper($$, "GE", $1, $3);
				}		|
	scalar_expr LE scalar_expr 	
				{
				 mk_s_expr_oper($$, "LE", $1, $3);
				}		|

	scalar_expr SUB scalar_expr 	
				{
				 mk_s_expr_oper($$, "SUB", $1, $3);
				}		|
	scalar_expr IN LPAREN in_predicate RPAREN
				{
				  $$ = MAKENODE(s_expr);
				  $$->left = $1;
				  $$->right = $4;
				  $$->value.type = OPER;
				  $$->value.value.oper_val = _IN;	
				}		|	
	scalar_expr NOT IN LPAREN in_predicate RPAREN
				{
				  $$ = MAKENODE(s_expr);
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

				}	
;

value_expr:
	colref  { 

		//here we need 2 macros
		// 1)   mk_tuplist_lit(sqltype, value)
		// 2)   mk_tuplist_ident(nametype, value)

//macros:

//new_tuple(tuple *, tag, type, value)
//append_tuple(list, tuple)
//append_tuple2(list, tag, type, value)

		$$ = $1;
		}
		|
	INT_LIT {
		  //what's emitted here is a tuple (actually, a list-of-tuples)
		  //for the time being, a single tuple
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
				  $$ = MAKENODE(scalarExpr);
				  $$->left = NULL;
				  $$->right = NULL;
				  $$->value.type = IN_LIST;
				  $$->value.value.in_list_val = MAKENODE(inListNode);
				  $$->value.value.in_list_val->inListValue = $1;
				  $$->value.value.in_list_val->list.next = NULL;

				
		} |
	in_predicate COMMA scalar_expr {
				  inListNode *inListVal = MAKENODE(inListNode);
				  inListVal->inListValue = $1;
				  list_append($$->value.value.in_list_val, inListVal);
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
