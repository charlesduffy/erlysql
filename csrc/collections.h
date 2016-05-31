#ifndef _COLLECTIONS_H
#define _COLLECTIONS_H

#define LIST_TYPNAM llist
#define LIST_MEMB_NAME list

#define container_of(ptr, type, member) ((type *) ((char *)(ptr) - offsetof(type, member)))

#define list_append(p,n) { 						\
				LIST_TYPNAM *l = &p->LIST_MEMB_NAME; 	\
				while (l->next !=NULL) l=l->next; 	\
				l->next = &n->LIST_MEMB_NAME; 		\
				l->next->next = NULL;			\
			 }

#define list_foreach(p,T,d) for (LIST_TYPNAM *ll = &p->LIST_MEMB_NAME ; d = container_of(ll,T,LIST_MEMB_NAME), ll != NULL ; ll=ll->next )
/*
typedef struct _list llist;

struct _list {
        llist *next;
};
*/
#endif
