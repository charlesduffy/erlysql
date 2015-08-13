#include "erl_nif.h"
#include "grammar.tab.h"
#include "scanner.h"
#include <string.h>
#include <malloc.h>

#define MAXBUFLEN 1024

#define TREESEP() (printf("\t|\n\t+-"))

/* pretty printer fun decls. Farm these out to own files eventually */

void prettyPrintSelectList(selectListNode *) ;
void prettyPrintSelectNode (selectStmtNode *) ;
void prettyPrintParseTree (queryNode *) ; 
void prettyPrintSexpr(scalarExpr *) ;

/* other fun decl */

int foo(int x);
queryNode * parseQuery (char *queryText);

static ERL_NIF_TERM foo_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int x, ret;
    if (!enif_get_int(env, argv[0], &x)) {
	return enif_make_badarg(env);
    }
    ret = foo(x);
    return enif_make_int(env, ret);
}

static ERL_NIF_TERM parseQuery_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    queryNode * ret;
    char queryText[MAXBUFLEN];

    //consider static allocation here
    //determine better way to get the length of the query string at runtime
    //determine proper error handling method on malloc failure

   (void) memset (queryText, '\0', sizeof(queryText));

    if (enif_get_string(env, argv[0], queryText, MAXBUFLEN, ERL_NIF_LATIN1) < 1) {
	return enif_make_badarg(env);
    }
    ret = parseQuery(queryText);
    return enif_make_tuple(env, 2,
		enif_make_atom(env, "hello"),
		enif_make_atom(env, "goodbye" ));
}

static ErlNifFunc nif_funcs[] = {
    {"foo", 1, foo_nif},
    {"parseQuery", 1, parseQuery_nif}
};

ERL_NIF_INIT(parser, nif_funcs, NULL, NULL, NULL, NULL)

int foo (int x) {
	return 100;
}

char * preprocQuery (char *queryText) {
/* placeholder for proposed query string preprocessor

might strip whitespace, find basic problems (corruption, too long, too short etc).

 */

return (queryText);
}

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

void prettyPrintSexpr(scalarExpr *sExp) {

	/* scalar expressions are tree-like structures. 
           Basic recursive tree traversal algorithm here.
	*/
	valueExprNode v;
	char *oper[] = { "/" , "*" , "+" , "-" , "%" , ">" , "<" , ">=" , "<=" , "OR" , "AND" , "NOT" , "=" , "!=" };

	//printf("entering pretty print sexpr\n");

	switch(sExp->value.type) {
		TREESEP();
	 case UNDEFINED:
		printf(" [UNDEFINED:<>\n");
		break;
	 case COLREF:
		printf(" [COLREF]:%s \n", sExp->value.value.colName);
		break;
	 case TEXT:
		printf(" [TEXT]:%s \n", sExp->value.value.text_val);
		break;
	 case INT:
		printf(" [INT]:%d \n", sExp->value.value.integer_val);
		break;
	 case NUM:
		printf(" [NUM]:%f \n", sExp->value.value.numeric_val);
		break;
	 case OPER:
		printf(" [OPER]:%s \n" , *(oper + sExp->value.value.oper_val));	
		break;
	 case SEXPR:
		printf(" [SEXPR] \n");
		break;
	}

	if (sExp->left != NULL) prettyPrintSexpr(sExp->left);
	
	if (sExp->right != NULL) prettyPrintSexpr(sExp->right);
	


}

void prettyPrintSelectList(selectListNode *sellist) {

	/* iterate over the array of pointers-to-sExpr 
	   and print each one */	

	printf("Select list: %d elements\n",sellist->nElements);

	int i;	
	scalarExpr *sExpr ;
	for (i=0; i < sellist->nElements; i++) {
		sExpr = *(sellist->sExpr+i);
		prettyPrintSexpr(sExpr);
		printf("+++++\n\r");
		//sExpr++;
	}
}


void prettyPrintSelectNode (selectStmtNode *selnode ) {

	printf("[SELECT]\n");	
	//traverse select list and print
	prettyPrintSelectList(selnode->selectList);
	//
		

}

void prettyPrintParseTree (queryNode *qry) {

	/* scan queryNode
		-- check query node type enum
		-- pass to select query handler 
	*/
printf("Parse tree prettyprint\r\n======================\n\r\n\r");
	//check node type
	switch (qry->statType) {
	  case SELECT_STMT:
		 prettyPrintSelectNode(qry->selnode);	
		break;	
	  case UPDATE_STMT:
	  case DELETE_STMT:
	  case INSERT_STMT:
		//not yet supported
		printf("Unsupported statement type\n");

	}

}

static ERL_NIF_TERM parseTreeToErl (queryNode *qry) {
	
}
