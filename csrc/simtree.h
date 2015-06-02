typedef struct simtree_node {

/* POC standard only. Union to hold the value; possibly an enum to hold the type */	

	union {

		int int_lval; 	
		char * text_lval;

	} lval;


};
