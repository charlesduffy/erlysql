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

void * process_tuplist (tuple *,void * (*)(tuple *));
void * process_sexpr   (s_expr *, void * (*)(s_expr *));
void * print_tupval(tuple *);

tuple * tup_container(p) {
    return(container_of(p, tuple, list));
}

//v1 just prints.
void * process_tuplist(tuple *t, void * (*fn)(tuple *)) {

    switch(t->type) {
	case v_int: printf("v_int:     {%s:%d}\n", t->tag, t->v_int);break;
	case v_text: printf("v_text:   {%s:%s}\n", t->tag, t->v_text);break; 
	case v_float:printf("v_float:  {%s:%f}\n", t->tag, t->v_float);break; 
	case v_tuple:	printf("v_tuple:  {%s:tuple}\n\t\t", t->tag);
			//process_tuplist(t->v_tuple, fn(t->v_tuple));
			break; 
	case v_sexpr: printf("v_sexpr: {%s:sexpr}\n", t->tag);break; 
   }
    if (t->type == v_tuple) {
	process_tuplist(t->v_tuple, fn(t->v_tuple));
    }    

    if (t->list.next != NULL) {
	process_tuplist(tuplist_next(t), fn(tuplist_next(t)));
	//t = tuplist_next(t);
    }

   /* 
    //process depth-first (enter each nested list first)


    if (t->list.next != NULL) {
	process_tuplist(tuplist_next(t), fn(tuplist_next(t)));
    }

    //if value is a sexpr, process that
    if (t->type == v_sexpr) {	
	process_sexpr(t->v_sexpr, fn(t->v_sexpr));
    } else {
//	fn(t);
    }
    //aggregate r+fn(t)
*/
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

void * print_tupval(tuple *t) {
//    printf("{%s:", t->tag);
    switch(t->type) {
//	case v_int: printf("%d}\n", t->v_int);break;
//	case v_text: printf("%s}\n", t->v_text);break;
//	case v_float: printf("%f}\n", t->v_float);break;
   }
}

