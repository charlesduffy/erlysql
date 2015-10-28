#include "simtree.h"
#include "grammar.tab.h"
#include "scanner.h"
#include <string.h>
#include <malloc.h>    

YY_BUFFER_STATE buf;
queryNode *qry;
yyscan_t scanner;
char *queryText = "select foo from bar;";

int main (int argc, char **argv) {

/* Instantiate new parser for testing */
    qry = malloc(sizeof(queryNode));
    yylex_init(&scanner);
    buf = yy_scan_string(queryText, scanner);
    yyparse(scanner, qry);
    yy_delete_buffer(buf, scanner);
    yylex_destroy(scanner);

}


