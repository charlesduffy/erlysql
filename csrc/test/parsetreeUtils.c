/*******

parsetree utilities for the test harness only. Nested struct comparison functions 

********/

#include "parsetree.h"
#include "dbglog.h"
#include <string.h>
#include <unistd.h>
#include <malloc.h>    

/***** TODO remember to consider adding generic iterator for the parsetree structure */

/* compare select list node */

int compareSelectListNode(selectListNode *A , selectListNode *B) {
 
 int i = 0;
 int nElems = 0; 
 int c;
 scalarExpr *sExprA, *sExprB;

debug("Entering compareSelectListNode");
fflush(stdout);
  
 /* test that the number of elements in the list is the same */

 if (A->nElements != B->nElements) {
	debug( "nElements not equal");
	fflush(stdout);
	return 0;
}

 nElems = A->nElements;

 debug("Number of elements: %d", nElems);

 /* check all the select list elements */

 for (i = 0; i < nElems; i++) {

    debug("Testing element %d of %d", i , nElems);
   
    sExprA = *(A->sExpr+i);    
    sExprB = *(B->sExpr+i);    

    if (sExprCmp(sExprA , sExprB) == 0) {
     debug("returning 0 from sExprComp");
     return 0;
	 }


 }   

return 1;
  
}

int sExprCmp(scalarExpr *A , scalarExpr *B) {

 /* recursively check each sExpr */

 /* check the value type marker */

 if ( A->value.type != B->value.type) {

   debug("Value types not equal A %d  |  B %d", A->value.type, B->value.type); 
   return 0; 
}


debug("Value type: %d %d", A->value.type, B->value.type);
fflush(stdout);
 
	switch(A->value.type) {
		TREESEP();
	 case UNDEFINED:
		debug(" [UNDEFINED:<>");
		break;
	 case COLREF:
		if (strcmp(A->value.value.colName,B->value.value.colName) != 0 )  { 
		  	debug("Colref not equal: %s | %s" , A->value.value.colName , B->value.value.colName );
			return 0;
		}
		break;
	 case TEXT:
		if (strcmp(A->value.value.text_val,B->value.value.text_val) != 0 ) {
		  debug("Text not equal: %s | %s" , A->value.value.text_val , B->value.value.text_val );
		  return 0;
		}
		break;
	 case INT:
		if (A->value.value.integer_val != B->value.value.integer_val) {
		  debug("Int not equal: %d | %d" , A->value.value.integer_val , B->value.value.integer_val );
		  return 0;
		}
		break;
	 case NUM:
		if (A->value.value.integer_val != B->value.value.integer_val) {
		  debug("Int not equal: %d | %d" , A->value.value.integer_val , B->value.value.integer_val );
		  return 0;
		}
		break;
	 case OPER:
		if (A->value.value.oper_val != B->value.value.oper_val) return 0;
		break;
	 case SEXPR:
		debug(" [SEXPR] ");
		break;

	}
	
	if (A->left != NULL && B->left != NULL) 
	  if (sExprCmp(A->left, B->left) == 0) {
	     debug("returning zero from recursive call left side ");
             return 0;
	} 
		
	
	if (A->right != NULL && B->right != NULL)
	  if (sExprCmp(A->right, B->right) == 0) {
	     debug("returning zero from recursive call right side ");
	     return 0;
	  }

	return 1;
	
}
