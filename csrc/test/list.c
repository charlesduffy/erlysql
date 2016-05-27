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

struct datanode{

        int data;
        dlist list;

};

typedef struct datanode node1;

#define LIST_TYPE_NAME dlist
#define LIST_MEMB_NAME list
#define LIST_MEMB_OFFSET(T) (unsigned long)(&((T *)0)->LIST_MEMB_NAME)
#define ALIST_MEMB_OFFSET(T) offsetof(T,LIST_MEMB_NAME)
#define get_dlist_cell(T, p) ((T *)((LIST_TYPE_NAME *) p - LIST_MEMB_OFFSET(T)))
#define get_dlist_foreach(T, d, p) for(d=get_dlist_cell(T,p);p->next!=NULL;p=p->next,d=get_dlist_cell(T,p))

#define new_full_dlist( T, L, d, p ) {                          \
			int i;                                          \
			T *D;                                           \
			dlist *P;                                       \
			int i;                                          \
			T *D;                                           \
			dlist *P;                                       \
              		dlistInfo *li;                                  \
                 	d = malloc((size_t) sizeof(T) * L);             \
                 	li = malloc((size_t) sizeof(dlistInfo));        \
                 	li->allocnmemb = L;                             \
                 	li->size = 0;                                   \
              		li->last = p;                                   \
               		for (i=0; i<L; i++) {                          \
                         D=(d+i); P=(p+i);                       \
                         D->list=P;                              \
                         P->next = (i+1<=L ? (P+1) : NULL);      \
                         P->info = li;                           \
                  	 }                                              \
                 	}                                               \

 #define new_dlist( L, p ) {                                     \
                 int i;                                          \
                 dlist *P;                                       \
                 dlistInfo *li;                                  \
                 p = malloc((size_t) sizeof(dlist) * L);         \
                 li = malloc((size_t) sizeof(dlistInfo));        \
                 li->allocnmemb = L;                             \
                 li->size = 0;                                   \
		 li->last = p;					 \
                  for (i=0; i<L; i++) {                          \
                         P=(p+i);                                \
                         P->next = (i+1<=L ? (P+1) : NULL);      \
                         P->info = li;                           \
			 P->index = i;				 \
                  }                                              \
                 }                                               \

 #define get_list_size(p)        p->info->size

 #define inc_list_size(p)        {p->info->size++;p->info->last=p->next;}

 #define get_list_last(p)        p->info->last

 #define append_list_cell(p , d) {                               \
                 d->LIST_MEMB_NAME=get_list_last(p)              \
                 inc_list_size(p);                               \
                 }                                               \

 int main(int argc, char **argv) {

         dlist *mylist, *mylist2, *mylist3;
         struct datanode *mynode, *mynode2;
         int x = 0;

        new_dlist( 20, mylist);

                 mylist3 = mylist2 = mylist;
 //foreach working
         /*

                 get_dlist_foreach(struct datanode, mynode, mylist) {
                         mynode->data = x;
                         printf ("> x is: %d, mynode->data is: %d\n", x, mynode->data);
                         x++;
                 }
 */

                 mynode2 = malloc((size_t) sizeof(struct datanode));
                 mynode2->data = 1000;
                 mynode2->list = *mylist;
                 //mynode = get_dlist_cell(struct datanode,mylist);
		 mynode = NULL;
		 mynode = container_of2(*mylist, struct datanode , list);
		 //mynode = (struct datanode *)mylist - offsetof(struct datanode, list);
		 //printf("offset: %ld", offsetof(struct datanode, list));
                 //inc_list_size(mylist);
		 printf ("orig node: %p list ptr node: %p list member: %p\n" , mynode2, mynode, mylist);
//((struct datanode *)((char *)(mylist) - (unsigned long)(&((struct datanode *)0)->list)))

exit(0);

/*
         for ( x = 0; x<20V; x++) {
                 struct datanode *tmp;
                 dlist *p;
                 mynode2 = malloc((size_t) sizeof(struct datanode));
                 mynode2->data = x;
		//assert(mylist == get_list_last(mylist));
                 mynode2.ulist = get_list_last(mylist);
 //              append_list_cell( mylist , mynode2 );
                 tmp = get_dlist_cell(struct datanode,mylist);
                 inc_list_size(mylist);
                 printf("x is:  list size is:  node data via list is: %d \n", tmp->data);
         }
*/
 /*
                 //probably break these elements up into individual macros
                 do {
                   //mynode = ((struct datanode *)((dlist *) mylist - (unsigned long)(&((struct datanode *)0)->list)));
                   mynode = get_dlist_cell(struct datanode, mynode, mylist);
                   mynode->data = i;
                   mylist=mylist->next;
                   i++;
                 } while (mylist->next != NULL) ;
 */


                 do {
                   mynode = ((struct datanode *)((dlist *) mylist2 - (unsigned long)(&((struct datanode *)0)->list)));
                   //mynode = get_dlist_cell(struct datanode, mynode, mylist2);
                   printf("1 data is: %d\n", mynode->data);
                   mylist2=mylist2->next;
                 }
                 while (mylist2->next != NULL);

                 get_dlist_foreach(struct datanode, mynode, mylist3) {
                         printf("2 data is: %d\n", mynode->data);
                 }
}
