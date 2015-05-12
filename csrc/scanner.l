%{

#include "grammar.tab.h"

%}

%option noyywrap


space	[\t ]
newline [\r\n]
digit 	[0-9]
quote	'
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
(?i:sum)		{ return SUM; }
(?i:count)		{ return COUNT; }
(?i:set)		{ return SET; }
(?i:into)		{ return INTO; }

{space}+ 		{ } 
({alpha}+{alphanum}*) 	{ return IDENTIFIER;} /* Identifiers must start with a letter. Fix this definition */
{quote}  		{ return QUOTE;}
({quote}{alpha}+{quote}) { return STRING; }

{newline} 		{ return NEWLINE;}
{digit}+ 		{ return INT; }	
({digit}+\.{digit}+)  	{ return NUMERIC; }		

{add}			{ return ADD; }
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

%%
