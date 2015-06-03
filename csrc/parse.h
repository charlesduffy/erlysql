#ifdef __PARSE
#define __PARSE

void yyerror (yyscan_t scanner, char const *s) {
     fprintf (stderr, ">>> %s <<<\n", s);
}
#endif
