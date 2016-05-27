#include <stdio.h>
#include <string.h>
#include <malloc.h>
#include <assert.h>
#include <stdlib.h>
#include <stddef.h>

#define DLIST dlist
#define LIST_MEMB_NAME list

#define container_of(ptr, type, member) ((type *) ((char *)(ptr) - offsetof(type, member)))

//#define container_of2(ptr, type, member) ({ const typeof( ((type *)0)->member ) *__mptr = (ptr);    (type *)( (char *)__mptr - offsetof(type,member) );})   

#define list_append(p,n) { 						\
				DLIST *l = &p->LIST_MEMB_NAME; 		\
				while (l->next !=NULL) l=l->next; 	\
				l->next = &n->LIST_MEMB_NAME; 		\
				l->next->next = NULL;			\
			 }

#define list_foreach(p,T,d) for (DLIST *ll = &p->LIST_MEMB_NAME; ll != NULL ; ll=ll->next , d = container_of(ll,T,LIST_MEMB_NAME))

typedef struct _list dlist;
typedef struct _list_info dlistInfo;

struct _list {
        dlist *next;
	dlistInfo *info;
        unsigned int index;
};

struct _list_info {
	unsigned int size;
	unsigned int allocnmemb;
	dlist *last;
};

typedef struct {
	char hasAlias;
	char isWildcard;
	//scalarExpr *sExpr;
	char *sAlias;
	dlist list;
	int data;
} selectListItemNode;

/*
typedef struct {
	int nElements;
	selectListItemNode *item;
} selectListNode;
*/

int main(void) {

//	pnode = container_of2(listp, node1, list);

//list POC

//allocate space for 20 selectListItemNodes
//for loop to set up list links
//take pointer to head from selectListNode
//test.
	selectListItemNode *item, *p,*Ditem;
	dlist *l, *head; 
	dlistInfo *inf;
	int i;

	p=item;

	
	for (i=0;i<20;i++) {
//first element "select list item" first occurrence
	  item = malloc((size_t) sizeof(selectListItemNode));

	  if (i==0) { 
		      //inf  = malloc((size_t) sizeof(dlistInfo));
		      l = &item->list;
		      //l->info = inf;
		      l->next = NULL;
			Ditem = item; //this is "$$", we always have reference to it
			head = l;
			item->data = i;
			printf("pointers: l: %p l->next: %p\n", l, l->next);
		    }

//subsequent occurrence "select list item"
	  else {
/*
		l = &Ditem->list;
		while (l->next != NULL ) { l=l->next; }
		l->next = &item->list;
		l->next->next = NULL ;	
		item->data = i;
*/
		list_append(Ditem, item);
		item->data = i;

	  }
 	       
		printf ("[%d] head is: %p l is: %p l->next is: %p data is: %d\n", i, head, l, l->next, item->data);		

	}
	
	head = &Ditem->list;
	i = 0;
/*
	for (l = &Ditem->list; l != NULL ; l=l->next ) {
		item = container_of(l, selectListItemNode, list);
		printf ("[%d] data is: %d\n", i++, item->data);		
	}	

*/

	list_foreach ( Ditem, selectListItemNode, p) {
			
		printf ("data is: %d\n", p->data);		
	}	
	
}

