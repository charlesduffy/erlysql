struct parsenode {
	int type;
	struct parsenode *node;
} ;

typedef struct parsenode ParseNode;

/*
typedef struct simtree_node {


	union {

		int int_lval; 	
		char * text_lval;

	} lval;


};
*/
