%{

#include <string.h>
#include "grammar.tab.h"
#define YY_USER_ACTION yylloc->first_line = yylloc->last_line; \
		       yylloc->first_column = yylloc->last_column; \
		       yylloc->last_column++;
		       

%}

%option header-file="csrc/scanner.h"
%option noyywrap
%option reentrant 
%option bison-bridge
%option bison-locations
%option yylineno

space	[\t ]
newline [\r\n]
digit 	[0-9]
quote	'
doublequote ["]
alpha	[a-zA-Z]	
alphanum [a-zA-Z_0-9]
add	[+]
sub	[-]
mul 	[*]
div 	[/]
gt	[>]
lt	[<]
eq	[=]
semicolon [;]
comma [,]
lparen [(]
rparen [)]
point  [.]
lead_ident [a-zA-Z_]

%%

 
(?i:select)		{ return SELECT; } /* add a rule to create a state for "quoted" so we don't match these in strings */
(?i:insert)		{ return INSERT; }
(?i:update)		{ return UPDATE; }
(?i:delete)		{ return DELETE; }
(?i:where)		{ return WHERE; }
(?i:from)		{ return FROM; }
(?i:values)		{ return VALUES; }
(?i:create)		{ return CREATE; }
(?i:drop)		{ return DROP; }
(?i:and)		{ return AND; }
(?i:or)			{ return OR; }
(?i:in)			{ return IN; }
(?i:not)		{ return NOT; }
(?i:sum)		{ return SUM; }
(?i:count)		{ return COUNT; }
(?i:set)		{ return SET; }
(?i:into)		{ return INTO; }
(?i:as)			{ return AS; }
(?i:table)		{ return TABLE; }

(?i:int)		{ return INTEGER; }
(?i:integer)		{ return INTEGER; }
(?i:text)		{ return CHAR; }
(?i:numeric)		{ return NUMERIC; }

{space}+ 		{ } 

({lead_ident}{alphanum}*) { yylval->text_val = strdup(yytext); return IDENTIFIER;}  /* Identifiers must start with a letter, improve defn. Refactor memory handling */
({doublequote}{alphanum}+{doublequote})     { yylval->text_val = strdup(yytext); return IDENTIFIER;}  

{quote}  		{ return QUOTE;}
({quote}{alpha}+{quote}) { return STRING; }

{newline} 		{ return NEWLINE;}
{digit}+ 		{ yylval->integer_val = atoi(yytext); return INT_LIT; } 
({digit}+\.{digit}+)  	{ yylval->float_val = atof(yytext); return NUM_LIT; }		

{add}			{ return ADD; } /* operators */
{sub}			{ return SUB; }
{div}			{ return DIV; }
{mul}			{ return MUL; }
{gt} 			{ return GT; }
{lt}			{ return LT; }
{eq}			{ return EQ; }
({gt}{eq})		{ return GE; }
({lt}{eq})		{ return LE; }
{semicolon}		{ return SEMICOLON; }
{comma}			{ return COMMA; }
{lparen}		{ return LPAREN; }
{rparen}		{ return RPAREN; }
{point}			{ return POINT; }

%%
