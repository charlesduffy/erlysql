#include "grammar.tab.h"
#include "scanner.h"
#include "dbglog.h"
#include "structures.h"
#include <string.h>
#include <malloc.h>
#include <stddef.h>
#include <stdbool.h>

/*
bool sexpr_is_boolean( s_expr * sExpr) {

if (sExpr->value.type == OPER) {
	 switch(sExpr->value.value.oper_val) {
	     case   _AND:
	     case   _OR:
	     case   _NOT:
	     case   _BETWEEN:
	     case   _NOT_BETWEEN:
	     case   _IN:
	     case   _NOT_IN:
	     case   _LT:
	     case   _GT:
	     case   _EQ:
	     case   _NE:
	     case   _GTE:
	     case   _LTE: 
		return (true);
		break;
	     default: 
		return(false);
	} 
}
else  {
	return false;
}

}

*/

// miscellaneous functions

char *preprocQuery(char *queryText)
{
/* placeholder for proposed query string preprocessor

might strip whitespace, find basic problems (corruption, too long, too short etc).

 */

  return (queryText);
}


// parser entry point

tuple * parseQuery(char * queryText)
{
  YY_BUFFER_STATE buf;
  tuple *mqry;
  yyscan_t scanner;
  yylex_init(&scanner);
  buf = yy_scan_string(queryText, scanner);
  yyparse(scanner, mqry);
  yy_delete_buffer(buf, scanner);
  yylex_destroy(scanner);
  return (mqry);
}

