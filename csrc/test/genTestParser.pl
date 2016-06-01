#!/usr/bin/perl

my $SqlInputPath = "/home/ccd/xndb-micro/csrc/test/";
my $SqlInputFileName = "$SqlInputPath/LimitedParser.sql";

open (my $fileHandle, '<' ,  $SqlInputFileName ) or die "Can't open SQL datafile $SqlInputFileName \n";

print "$Preamble1";

my $TestCount = 0;

while (my $SqlStmt = <$fileHandle>) {

  chomp($SqlStmt);

  next if ($SqlStmt =~ /^--/);

  my $bpTest = "BasicParser$TestCount";

  my $testRunner = << "TESTRUNNER";
    RUN_TEST_CASE(basicParser, $bpTest);
TESTRUNNER

  my $testDfn = << "TESTDFN";
    TEST(basicParser, $bpTest)
    {

        char result;
        char * queryText1 = "$SqlStmt";	
	printf( "TEST# $bpTest : '$SqlStmt' ");
        buf1 = yy_scan_string(queryText1, scanner1);
        yyparse(scanner1, qry1);
	process_tuplist(qry1);
        //result = qry1->errFlag;
	//printf("result: %c \t %s\\n", qry1->errFlag, queryText1);
        TEST_ASSERT_EQUAL_MESSAGE(0,result, queryText1);
    }
TESTDFN

# Add TEST and the reference to the TEST_RUNNER

  push @testDefns, $testDfn;
  push @testRunners, $testRunner;
	
  $TestCount = $TestCount + 1;
}

close($fileHandle);

$testRunnerText = join ("\n", @testRunners);
$testDefnText = join ("\n", @testDefns);

my $Preamble1 = << "PREAMBLE1";

#include "structures.h"
#include "unity.h"
#include "unity_fixture.h"
#include "grammar.tab.h"
#include "scanner.h"
#include <stdio.h>
#include <string.h>
#include <malloc.h>    
#include <stddef.h>
#include <stdlib.h>
#include <stdbool.h>

YY_BUFFER_STATE buf1, buf2;
tuple *qry1;
yyscan_t scanner1;

TEST_GROUP(basicParser);

TEST_SETUP(basicParser) {

    yylex_init(&scanner1);
}

TEST_GROUP_RUNNER (basicParser) {

	$testRunnerText

}

TEST_TEAR_DOWN(basicParser) {

   free(qry1);    
   yy_delete_buffer(buf1, scanner1);
   yylex_destroy(scanner1);
}

static void RunAllTests(void) {

    RUN_TEST_GROUP(basicParser);
}

int main(int argc, const char **argv) {
	return UnityMain(argc, argv, RunAllTests);
}

$testDefnText

PREAMBLE1

print $Preamble1;
