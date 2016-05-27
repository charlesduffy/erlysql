#include <stdio.h>
#include <string.h>
#include <malloc.h>
#include <assert.h>
#include <stdlib.h>
#include <stddef.h>

#define container_of(ptr, type, member) ((type *) ((char *)(ptr) - offsetof(type, member)))

#define container_of2(ptr, type, member) ({ const typeof( ((type *)0)->member ) *__mptr = (ptr);    (type *)( (char *)__mptr - offsetof(type,member) );})   

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
	selectListItemNode *item, *p;
	dlist *l;
	dlistInfo *inf;
	int i;

	item = malloc((size_t) sizeof(selectListItemNode) * 20);
	inf = malloc( (size_t) sizeof(dlistInfo));	
	p=item;
	for (i=0; i<20; i++) {
		p->list.next = &(p+1)->list;
		p->list.info = inf;
		p->data = i;	
		printf("writing: [%d] p->data is: %d\n", i, p->data);
		p++;
	}	

	l = &item->list;
	for (i=0; i<20; i++) {
		p = container_of2(l, selectListItemNode, list);
		printf ("[%d] data is: %d\n", i, p->data);
		l = l->next;
	}

	//append test
	
	l = &item->list;
	l->info->last = l;
				

	
	
}

