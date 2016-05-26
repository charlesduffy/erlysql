 #include <stdio.h>
#include <string.h>
#include <malloc.h>

typedef struct _list dlist;
typdef struct _list_info listInfo;

struct _list {
        listInfo *next;
        unsigned int index;
};

struct _list_info {
	unsigned int size;
	dlist *last;
};

typedef struct {

        dlist *list;
        char data;

} node1;



#define ALIST_MEMB_OFFSET(T) (unsigned long)(&((T *)0)->LIST_MEMB_NAME)
#define LIST_MEMB_OFFSET(T) offsetof(T,LIST_MEMB_NAME)
#define get_dlist_cell(T, d, p) ((T *)((LIST_TYPE_NAME *) p - LIST_MEMB_OFFSET(T)))
#define get_dlist_foreach(T, d, p) for(d=get_dlist_cell(T,d,p);p->next!=NULL;p=p->next,d=get_dlist_cell(T,d,p))

#define new_full_dlist( T, L, d, p ) {                          \
			int i;                                          \
			T *D;                                           \
			dlist *P;                                       \
			int i;                                          \
			T *D;                                           \
			dlist *P;                                       \
              		dlistInfo *li;                                  \
                 	d = malloc((size_t) sizeof(T) * L);             \
                	p = malloc((size_t) sizeof(dlist) * L);         \
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
                  for (i=0; i<L; i++) {                          \
                         P=(p+i);                                \
                         P->next = (i+1<=L ? (P+1) : NULL);      \
                         P->info = li;                           \
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
         node1 *mynode, *mynode2;
                   int x = 0;

        new_dlist( 20, mylist);

                 mylist3 = mylist2 = mylist;
 //foreach working
         /*

                 get_dlist_foreach(node1, mynode, mylist) {
                         mynode->data = x;
                         printf ("> x is: %d, mynode->data is: %d\n", x, mynode->data);
                         x++;
                 }
 */
         for ( x = 0; x<20; x++) {
                 node1 *tmp;
                 dlist *p;
                 int size = get_list_size(mylist);
                 mynode2 = malloc((size_t) sizeof(node1));
                 mynode2->data = x;
                 p = mylist->last;
 //              append_list_cell( mylist , mynode2 );
                 mynode2->list = p;
                 inc_list_size(mylist);


                 tmp = get_dlist_cell(node1,tmp,p);
                 printf("x is: %d list size is: %d node data via list is: %d \n", x, get_list_size(mylist),tmp->data);
         }

 /*
                 //probably break these elements up into individual macros
                 do {
                   //mynode = ((node1 *)((dlist *) mylist - (unsigned long)(&((node1 *)0)->list)));
                   mynode = get_dlist_cell(node1, mynode, mylist);
                   mynode->data = i;
                   mylist=mylist->next;
                   i++;
                 } while (mylist->next != NULL) ;
 */


                 do {
                   mynode = ((node1 *)((dlist *) mylist2 - (unsigned long)(&((node1 *)0)->list)));
                   //mynode = get_dlist_cell(node1, mynode, mylist2);
                   printf("1 data is: %d\n", mynode->data);
                   mylist2=mylist2->next;
                 }
                 while (mylist2->next != NULL);

                 get_dlist_foreach(node1, mynode, mylist3) {
                         printf("2 data is: %d\n", mynode->data);
                 }
}
