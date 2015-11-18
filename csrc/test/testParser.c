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
	debug("hello");
}
*/

TEST_GROUP(basicParser);

TEST_SETUP(basicParser) {

    qry1 = malloc(sizeof(queryNode));
   // qry2 = malloc(sizeof(queryNode));
    yylex_init(&scanner1);
    //yylex_init(&scanner2);

}

TEST_TEAR_DOWN(basicParser) {

   free(qry1);    
   yy_delete_buffer(buf1, scanner1);
   yylex_destroy(scanner1);

}

TEST(basicParser, elderberry)
{
   
    int result;
    char * queryText = "RHUBARB select a , b , c from bar , baz , bee as tab3;";	
    debug("query is: %s", queryText);
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
    char * queryText1 = "select foo , bar from tab1 , tab2 , tab3;";	
    buf1 = yy_scan_string(queryText1, scanner1);
    yyparse(scanner1, qry1);
	debug("** nElements %d", qry1->selnode->selectList->nElements);
    result = compareSelectListNode(qry1->selnode->selectList, qry1->selnode->selectList); 
    TEST_ASSERT_EQUAL(1 , result);
}

TEST(basicParser, antioch)
{
    int result;
    char * queryText1 = "select  foo , bar , baz from table1 as a , table2 as b where a.foo < 1 and 1>2;";	
    buf1 = yy_scan_string(queryText1, scanner1);
    yyparse(scanner1, qry1);
    printf("%s\n", queryText1);
    prettyPrintParseTree(qry1);
    TEST_ASSERT_EQUAL(1 , 1);
}

TEST_GROUP_RUNNER (basicParser) {

//	RUN_TEST_CASE(basicParser, elderberry);
	RUN_TEST_CASE(basicParser, herring);
	RUN_TEST_CASE(basicParser, elderberry);
	RUN_TEST_CASE(basicParser, antioch);
//	RUN_TEST_CASE(basicParser, elderberry);

}

static void RunAllTests(void) {
    RUN_TEST_GROUP(basicParser);
}

int main(int argc, const char **argv) {
return UnityMain(argc, argv, RunAllTests); }
