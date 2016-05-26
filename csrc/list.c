#include <stdio.h>
#include <string.h>
#include <malloc.h>

typedef struct _list listInfo;
struct _list {
	listInfo *next;
	unsigned int index;
};

typedef struct {

	listInfo *list;	
	char data;

} node1;

//need a macro to allocate a list of length n of data objects T
//T=data type, L=length, d=ptr to data obj, p=ptr to list obj
//- allocate n datatype items
//- allocate n list items
//- iterate, stringing pointers together
//-- n=0
//-- list_n = new(list)
//-- data_n = new(T)
//-- 
//-- data_n.list = list1
//-- list_(n+1) = new(list)
//-- list_n.next = list_(n+1)
//-- n++

#define new_list( T, L, d, p ) {            			\
		int i;						\
		T *D;						\
		listInfo *P;					\
		d = malloc((size_t) sizeof(T) * L); 		\
		p = malloc((size_t) sizeof(listInfo) * L);	\
		 for (i=0; i++; i<L) {				\
		  	D=(d+i); P=(p+i);			\
			D->list=P;				\
			P->index = i;				\
			P->next = (i+1<L ? (P+1) : NULL);	\
		 }						\
		}						\
			


//need a macro to get the next elem of a list

//need a macro to add an item to the list

//need a macro to perform foreach on list

//T=data type, d=data pointer, p=list pointer, c=callback function pointer
//- iterate across data pointers
//-- calculate list member offset value
//-- calculate address of enclosing struct
//-- expose in for loop body
#define foreach_list( T, d, p, c) 				\
	{ 							\

int main(int argc, char **argv) {

	listInfo *mylist, *mylist2;
	node1 *mynode, *mynode2;
		  char i = 0;

//	new_list( node1, 20, mynode, mylist);
	{ int i; 
	node1 *D; 
	listInfo *P; 
	mynode = malloc((size_t) sizeof(node1) * 20); 
	mylist = malloc((size_t) sizeof(listInfo) * 20); 
	for (i=0; i<20; i++) { 
		D=(mynode+i); 
		P=(mylist+i); 
		D->list=P; P->next = (     i+1<20 ? (P+1) : ((void *)0));
	 }
 };
		mylist2 = mylist;
//foreach working 
	
		//probably break these elements up into individual macros
		while (mylist->next != NULL) {
		  mynode = ((node1 *)((listInfo *) mylist - (unsigned long)(&((node1 *)0)->list)));
		  mynode->data = i;
		  mylist=mylist->next;
		  i++;
		}


		while (mylist2->next != NULL) {
		  mynode = ((node1 *)((listInfo *) mylist2 - (unsigned long)(&((node1 *)0)->list)));
		  printf("data is: %d\n", mynode->data);
		  mylist2=mylist2->next;
		}
}


