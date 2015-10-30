#include "parsetree.h"
#include "unity.h"
#include "unity_fixture.h"
#include "grammar.tab.h"
#include "scanner.h"
#include <stdio.h>
#include <string.h>
#include <malloc.h>    



YY_BUFFER_STATE buf1, buf2;
queryNode *qry1, *qry2;
yyscan_t scanner1 , scanner2;

/*
int main (int argc, char **argv) {
	printf("hello\n");
}
*/

TEST_GROUP(basicParser);

TEST_SETUP(basicParser) {

    qry1 = malloc(sizeof(queryNode));
    qry2 = malloc(sizeof(queryNode));
    yylex_init(&scanner1);
    yylex_init(&scanner2);
    //buf = yy_scan_string(queryText, scanner);
//    yyparse(scanner, qry);

}

TEST_TEAR_DOWN(basicParser) {

   free(qry1);    
   printf("freeing ...\n");
   free(qry2);    
   yy_delete_buffer(buf1, scanner1);
   yy_delete_buffer(buf2, scanner2);
   yylex_destroy(scanner1);
   yylex_destroy(scanner2);

}

TEST(basicParser, elderberry)
{
   
    int result;
    char * queryText = "select 1+x , 2-4 , hello from bar;";	
    printf("query is: %s\n", queryText);
    buf1 = yy_scan_string(queryText, scanner1);
    yyparse(scanner1, qry1);
    result = compareSelectListNode( qry1->selnode->selectList , qry1->selnode->selectList );
    TEST_ASSERT_EQUAL(1 , result);
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
    int result;
    char * queryText1 = "select foo , bar from bar;";	
    char * queryText2 = "select foo from bar;";	
    buf1 = yy_scan_string(queryText1, scanner1);
    buf2 = yy_scan_string(queryText2, scanner2);
    yyparse(scanner1, qry1);
    yyparse(scanner2, qry2);
	printf("\n** nElements %d\n", qry1->selnode->selectList->nElements);
    result = compareSelectListNode(qry1->selnode->selectList, qry2->selnode->selectList); 
    TEST_ASSERT_EQUAL(1 , result);
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
