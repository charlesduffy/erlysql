#include "simtree.h"
#include "unity.h"
#include "unity_fixture.h"
#include "grammar.tab.h"
#include "scanner.h"
#include <stdio.h>
#include <string.h>
#include <malloc.h>    



YY_BUFFER_STATE buf;
queryNode *qry;
yyscan_t scanner;

/*
int main (int argc, char **argv) {
	printf("hello\n");
}
*/

TEST_GROUP(basicParser);

TEST_SETUP(basicParser) {

    qry = malloc(sizeof(queryNode));
    yylex_init(&scanner);
    //buf = yy_scan_string(queryText, scanner);
//    yyparse(scanner, qry);

}

TEST_TEAR_DOWN(basicParser) {

   free(qry);    
   yy_delete_buffer(buf, scanner);
   yylex_destroy(scanner);

}

TEST(basicParser, elderberry)
{
	int a = 1;
TEST_ASSERT_EQUAL( a , 1 ); //this one will pass

}

/*

queryNode * parseQuery (char *queryText) {
    YY_BUFFER_STATE buf;
    queryNode *qry = malloc(sizeof(queryNode));
    yyscan_t scanner;

    yylex_init(&scanner);
    buf = yy_scan_string(queryText, scanner);
    yyparse(scanner, qry);
    yy_delete_buffer(buf, scanner);
    yylex_destroy(scanner);
    prettyPrintParseTree(qry);
    return (qry);
}

*/



TEST(basicParser, herring)
{
    char * queryText = "select foo from bar;";	
    buf = yy_scan_string(queryText, scanner);
    yyparse(scanner, qry);
}

TEST_GROUP_RUNNER (basicParser) {

	RUN_TEST_CASE(basicParser, elderberry);
	RUN_TEST_CASE(basicParser, herring);

}

static void RunAllTests(void) {
    RUN_TEST_GROUP(basicParser);
}

int main(int argc, const char **argv) {
return UnityMain(argc, argv, RunAllTests); }
