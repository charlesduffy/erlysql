#include "dbglog.h"
#include "structures.h"
#include <string.h>
#include <malloc.h>
#include <stddef.h>
#include <stdbool.h>
#include <string.h>

#include "structures.h"
//--process_tuplist(p, fn)
//- for a list of tuples, recursively process each element. Apply fn to each element

//--process_sexpr(p, fn)
//- for an s_expr, recursively process the tree, applying fn to each element

void * process_tuplist (tuple *,void * (*)(tuple *), int);
void * process_sexpr   (s_expr *, void * (*)(s_expr *));


tuple * tup_container(p) {
    return((tuple *)container_of(p, tuple, list));
}

//v1 just prints.
void * process_tuplist(tuple *t, void * (*fn)(tuple *), int d) {

    if (t->list.prev == NULL ) { 
//	    printf("\n"); 
//	    for (int i=0;i<d;i++) printf(" ");
	    printf("[");
    }
    switch(t->type) {
	case v_int: printf("v_int:{%s:%d} ", t->tag, t->v_int);break;
	case v_text: printf("v_text:{%s:%s} ", t->tag, t->v_text);break; 
	case v_float:printf("v_float:{%s:%f} ", t->tag, t->v_float);break; 
	case v_tuple:printf("v_tuple:{%s:tuple} ", t->tag);break; 
	case v_sexpr: printf("v_sexpr:{%s:sexpr} ", t->tag);break; 
   }
    if (t->type == v_tuple) {
	//pass "start list flag"
	//printf ("[");
	process_tuplist(t->v_tuple, fn(t->v_tuple), d+1);
	//printf ("]\n");
	//end list
    }    

    if (t->list.next != NULL) {
	process_tuplist(tuplist_next(t), fn(tuplist_next(t)), d);
    } else {
//	for (int i=0;i<d;i++) printf(" ");
	printf("]\n");
    }

}

void * process_sexpr (s_expr *s, void * (*fn)(s_expr *)) {

    if (s->left != NULL) {
	process_sexpr(s->left, fn(s->left));
    } 

    if (s->right != NULL) {
	process_sexpr(s->right, fn(s->right));
    }

 //   fn(s->value);
}
