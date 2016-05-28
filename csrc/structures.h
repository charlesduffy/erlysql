#ifndef _STRUCTURES_H
#define _STRUCTURES_H

#include <stdbool.h>
#include "collections.h"


/* Helper Enums for parse nodes */
typedef enum { } tag;

typedef enum { UNDEFINED, COLREF, INT, NUM, _TEXT, OPER, SEXPR, WILDCARD, IN_LIST, BETWEEN_PREDICATE } valueExprType;

typedef enum { _DIV, _MUL, _ADD, _SUB, _MOD, _GT, _LT, _GTE, _LTE, _OR, _AND, _NOT, _EQ, _NE, _IN, _NOT_IN,
		_BETWEEN, _NOT_BETWEEN } operVal;

/* operator symbols */
extern char *operSyms[];

typedef struct s_expression s_expr;
typedef struct linked_list llist;
typedef struct ord_pair tuple;

struct linked_list {
    llist *next;
    llist *prev;
    llist list;
};

struct ord_pair {
    char *tag;
    typedef union {
	unsigned int v_long;
	int v_int;
	float v_float;
	char * v_text;
    };
    llist list;
};

/* s-expression */

typedef struct s_expr scalarExpr;

struct s_expr {
  tuple *value;
  llist list;
  scalarExpr *left;
  scalarExpr *right;
};


#endif
