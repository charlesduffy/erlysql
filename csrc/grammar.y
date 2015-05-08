/* Infix notation calculator--calc */

%{
#define YYSTYPE double
#include <math.h>
#include <string.h>

void yyerror (char const *s) {
   fprintf (stderr, "%s\n", s);
 }


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
