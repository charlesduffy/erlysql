#include "dbglog.h"
#include "structures.h"
#include <string.h>
#include <malloc.h>
#include <stddef.h>
#include <stdbool.h>

#include "structures.h"
//--process_tuplist(p, fn)
//- for a list of tuples, recursively process each element. Apply fn to each element

//--process_sexpr(p, fn)
//- for an s_expr, recursively process the tree, applying fn to each element

void * process_tuplist(tuple *,(void *) (*)(tuple *));

//v1 just prints.
void * process_tuplist(tuple *t, void * (*fn)(tuple *)) {
   
    if (t->list.next != NULL) {
	process_tuplist(tuplist_next(t), fn);
    }

    //if value is a sexpr, process that
    if (t->type == v_sexpr) {	
	process_sexpr(t->value, fn);
    } else {
	fn(t);
    }
    //aggregate r+fn(t)
}

void * process_sexpr (s_expr *s, void * (*fn)(s_expr *)) {

    if (s->left != NULL) {
	process_sexpr(s->left);
    } 

    if (s->right != NULL) {
	process_sexpr(s->right);
    }
    fn(s);
}

void print_tupval(tuple *t) {
    printf("{%s:", t->tag);
    switch(t->type) {
	case v_int: printf("%d}\n", t->v_int);break;
	case v_text: printf("%s}", t->v_text);break;
	case v_float: printf("%f}", t->v_float);break;
//	case v_sexpr: print_sexpr(t);
   }
}

