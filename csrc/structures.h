#ifndef _STRUCTURES_H
#define _STRUCTURES_H

#include <stdbool.h>

extern char *operSyms[];

typedef enum { v_text , v_int , v_float , v_tuple , v_sexpr } ttype;

typedef struct ord_pair tuple;
typedef struct linked_list llist;
typedef struct s_expression s_expr;

struct linked_list {
    llist *next;
    llist *prev;
};

struct s_expression {
  tuple *value;
  llist list;
  s_expr *left;
  s_expr *right;
};

struct ord_pair {
    char *tag;
    ttype type;
    union {
	unsigned int v_long;
	int v_int;
	float v_float;
	char * v_text;
	tuple * v_tuple;
	s_expr * v_sexpr;
    };
    llist list;
};

#define LIST_TYPNAM llist
#define LIST_MEMB_NAME list

#define container_of(ptr, type, member) ((type *) ((char *)(ptr) - offsetof(type, member)))

#define list_append(p,n) {                                              \
                                LIST_TYPNAM *l = &p->LIST_MEMB_NAME;    \
                                while (l->next !=NULL) l=l->next;       \
                                l->next = &n->LIST_MEMB_NAME;           \
                                l->next->next = NULL;                   \
                                l->next->prev = l;                      \
                         }

#define list_foreach(p,T,d) for (LIST_TYPNAM *ll = &p->LIST_MEMB_NAME ; d = container_of(ll,T,LIST_MEMB_NAME), ll != NULL ; ll=ll->next )

#define list_ff(p)  { LIST_TYPNAM *ll = &p->LIST_MEMB_NAME; while ( ll->next != NULL) ll=ll->next; p = container_of(ll, tuple, LIST_MEMB_NAME); }

#define list_foreach_r(p,T,d) for ( LIST_TYPNAM *ll = &p->LIST_MEMB_NAME ; d = container_of(ll,T,LIST_MEMB_NAME), ll != NULL ; ll=ll->prev)

#define tuplist_next(p) container_of(p->list.next,tuple,LIST_MEMB_NAME)

#define MAKENODE(nodetype) malloc((size_t) sizeof( nodetype ))

#define new_tuple(p, t, T, v) { p=MAKENODE(tuple);	\
				 p->tag=T;		\
				 p->type = t;		\
				 p->t=v;		\
				 p->list.next = NULL;	\
				 p->list.prev = NULL;	\
			       }			\

#define mk_tuplist_lit(p, t, T, v) { tuple *x;				    \
				     tuple *y;				    \
				     new_tuple(p,t,"value",v);		    \
				     new_tuple(x,v_text,"class","literal"); \
				     new_tuple(y,v_text,"sqltype",T);	    \
				     list_append(p,x);			    \
				     list_append(x,y);			    \
				    }
/*
    mk_tuplist_ident (<pointer>, <alias>, <value>)

    make a tuplist describing a column reference

    <pointer>	    pointer to a struct tuple.
    <alias>	    column table reference or NULL if not present
    <value>	    name of the column, char *

*/

#define mk_tuplist_ident(p, A, v) {  tuple *x ;					\
				     tuple *y ;					\
				     new_tuple(p,v_text,"value",v);		\
				     new_tuple(x,v_text,"class","identifier");	\
				     new_tuple(y,v_text,"reference",A);		\
				     list_append(p,x);				\
				     list_append(x,y);				\
				    }
				    
/*
    mk_tuplist_oper (<pointer>, <alias>, <value>)

    make a tuplist describing an operator 

    <pointer>	    pointer to a struct tuple.
    <value>	    name of the operator

*/

#define mk_tuplist_oper(p, v)	    {tuple *x ;	 			\
				     new_tuple(p,v_text,"value",v);		\
				     new_tuple(x,v_text,"class","operator");	\
				     list_append(p,x);				\
				    }	

#define mk_s_expr_val(p, v) { p=MAKENODE(s_expr);	\
			      p->value = v;		\
			      p->left = NULL;		\
			      p->right = NULL;		\
			      p->list.next = NULL;	\
			      p->list.prev = NULL;	\
			}

#define mk_s_expr_oper(p, v, l, r) { p=MAKENODE(s_expr);	\
			      mk_tuplist_oper(p->value, v);	\
			      p->left = l;			\
			      p->right = r;			\
			      p->list.next = NULL;		\
			      p->list.prev = NULL;		\
			}

#define tuple_append(p, t, T, v) {  tuple *n;			\
				    new_tuple(n, t, T, v);	\
				    list_append(p, n);		\
				    }

#endif
