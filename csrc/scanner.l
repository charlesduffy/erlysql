%{
#include <string.h>
#include "grammar.tab.h"
#define YY_USER_ACTION yylloc->first_line = yylloc->last_line; \
		       yylloc->first_column = yylloc->last_column; \
		       yylloc->last_column++;
		       
#define MAX_STRING_BUFFER 1024

char stringBuffer[MAX_STRING_BUFFER];
char *strBuf;

%}

%option header-file="csrc/scanner.h"
%option noyywrap
%option reentrant 
%option bison-bridge
%option bison-locations
%option yylineno

%x string

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
excl   [!]
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
(?i:between)		{ return BETWEEN; }
(?i:not)		{ return NOT; }
(?i:sum)		{ return SUM; }
(?i:count)		{ return COUNT; }
(?i:set)		{ return SET; }
(?i:into)		{ return INTO; }
(?i:as)			{ return AS; }
(?i:table)		{ return TABLE; }
(?i:with)		{ return WITH; }
(?i:case)		{ return CASE; }
(?i:else)		{ return ELSE; }
(?i:end)		{ return END; }
(?i:when)		{ return WHEN; }
(?i:then)		{ return THEN; }


(?i:int)		{ return INT4; }
(?i:integer)		{ return INT4; }
(?i:bigint)		{ return INT8; }
(?i:smallint)		{ return INT2; }

(?i:numeric)		{ return NUMERIC; }
(?i:decimal)		{ return NUMERIC; }

(?i:real)		{ return REAL; }
(?i:double)		{ return DOUBLE; }
(?i:precision)		{ return PRECISION; }
(?i:float)		{ return FLOAT; }

(?i:text)		{ return TEXT; }
(?i:char)		{ return CHAR; }
(?i:varchar)		{ return VARCHAR; }

(?i:bit)		{ return BIT; }

(?i:date)		{ return DATE; }
(?i:time)		{ return TIME; }
(?i:timestamp)		{ return TIMESTAMP; }
(?i:zone)		{ return ZONE; }
(?i:interval)		{ return INTERVAL; }

{space}+ 		{ } 

{add}			{ return ADD; } /* operators */
{sub}			{ return SUB; }
{div}			{ return DIV; }
{mul}			{ return MUL; }
{gt} 			{ return GT; }
{lt}			{ return LT; }
{eq}			{ return EQ; }
({gt}{eq})		{ return GE; }
({lt}{eq})		{ return LE; }
({excl}{eq})		{ return NE; }
({lt}{gt})		{ return NE; }
{semicolon}		{ return SEMICOLON; }
{comma}			{ return COMMA; }
{lparen}		{ return LPAREN; }
{rparen}		{ return RPAREN; }
{point}			{ return POINT; }

({lead_ident}{alphanum}*) { yylval->text_val = strdup(yytext); return IDENTIFIER;}  /* Identifiers must start with a letter, improve defn. Refactor memory handling */
({doublequote}{alphanum}+{doublequote})     { yylval->text_val = strdup(yytext); return IDENTIFIER;}  

{quote}			{
				/* 
					Entering string state from INITIAL 
					
					For the time being, we have only very basic string literal 
					handling. This needs to be extended to include embedded escapes
					and concatenated quoted strings per the SQL standard (and many 
					other things.)
				*/
					strBuf = stringBuffer;
					printf("Entering STRING state\n");
					BEGIN(string);
				
			}

<string>{quote}		{	/* end of string */
				BEGIN(INITIAL);
				*strBuf = '\0';
				yylval->text_val = stringBuffer;
				printf("Leaving STRING state: >%s<\n", stringBuffer);
				return STRING;
			}	
		

<string>{alphanum}*		{
					char *text = yytext;
					printf("Adding STRING data\n");
                 			while ( *text ) *strBuf++ = *text++;	
			}


{newline} 		{ return NEWLINE;}
{digit}+ 		{ yylval->integer_val = atoi(yytext); return INT_LIT; } 
({digit}+\.{digit}+)  	{ yylval->float_val = atof(yytext); return NUM_LIT; }		
%%
