%{

#include "grammar.tab.h"

%}


%option noyywrap
%option reentrant

space	[\t ]
newline [\r\n]
digit 	[0-9]
quote	'
alpha	[a-zA-Z]	
oper	[!+-=<>/]

%%

{space}+ {return WHITESPACE;}
{alpha}+ {return WORD;} /* modify to permit identifiers with numbers, etc */
{quote}  {return QUOTE;}
{oper}   {return OPER;}
{newline} {return NEWLINE;}
{digit}+ {return NUMBER;}

%%
