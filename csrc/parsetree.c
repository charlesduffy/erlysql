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
	char *oper[] = { "/" , "*" , "+" , "-" , "%" , ">" , "<" , ">=" , "<=" , "OR" , "AND" , "NOT" , "=" , "!=" }; //****TODO fix this nonsense!

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
