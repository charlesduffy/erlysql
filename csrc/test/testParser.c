#include "simtree.h"
#include "unity.h"
#include "unity_fixture.h"
#include "grammar.tab.h"
#include "scanner.h"
#include <string.h>
#include <malloc.h>    

YY_BUFFER_STATE buf;
//queryNode *qry;
yyscan_t scanner;

TEST_GROUP(basicParser);

TEST_SETUP(basicParser) {

/* Instantiate new parser for testing */
   // qry = malloc(sizeof(queryNode));
//    yylex_init(&scanner);
//    buf = yy_scan_string(queryText, scanner);
//    yyparse(scanner, qry);

}

TEST_TEAR_DOWN(basicParser) {
    
//   yy_delete_buffer(buf, scanner);
//   yylex_destroy(scanner);

}
