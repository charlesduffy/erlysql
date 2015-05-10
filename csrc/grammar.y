/* Infix notation calculator--calc */

%{
#define YYSTYPE double
#include <math.h>
#include <string.h>
#include <stdio.h>

void yyerror (char const *s) {
   fprintf (stderr, "%s\n", s);
 }
/*
int parseQuery (char const *queryText) {
	yyscan_t scanner;
    YY_BUFFER_STATE buf;
    yylex_init(&scanner);
    buf = yy_scan_string(queryString, scanner);
    yylex(scanner);
    yy_delete_buffer(buf, scanner);
    yylex_destroy(scanner);
	yyparse();
	return(0);	
}
*/

%}

/* BISON Declarations */
%token WHITESPACE
%token WORD
%token QUOTE
%token OPER
%token NEWLINE
%token NUMBER

%left '-' '+'
%left '*' '/'
%left NEG     /* negation--unary minus */
%right '^'    /* exponentiation        */

/* Grammar follows */
%%
input:    /* empty string */
        | input line
;

line:     '\n'
        | exp '\n'  { printf ("\t%.10g\n", $1); }
;

exp:      NUMBER                { $$ = $1;         }
        | exp '+' exp        { $$ = $1 + $3;    }
        | exp '-' exp        { $$ = $1 - $3;    }
        | exp '*' exp        { $$ = $1 * $3;    }
        | exp '/' exp        { $$ = $1 / $3;    }
        | '-' exp  %prec NEG { $$ = -$2;        }
        | exp '^' exp        { $$ = pow ($1, $3); }
        | '(' exp ')'        { $$ = $2;         }
;
%%
