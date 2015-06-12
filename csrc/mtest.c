#include <stdio.h>
#include <stdlib.h>

#define FOOBAR(x) (x *) malloc ( (size_t) sizeof (x) ) 

#define MAKENODE(nodetype) malloc ((size_t) sizeof( nodetype ))

int main (void) {

	struct fooBar { int i ; char * p ;} ;
	typedef struct fooBar bazBar;
	int * pointer;
	bazBar * loiter;

	pointer = FOOBAR(int);
	loiter = MAKENODE(bazBar);

}
