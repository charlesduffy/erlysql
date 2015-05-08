# makefile for xndbmicro
# compile Erlang and C components
# wraps rebar

ERL=$(shell which erl)
REBAR=$(shell which rebar)
EBIN=$(ebin/)
LEX=flex
YACC=bison -d
SRCDIR=csrc
NIFDIR=priv
##################


CFLAGS=-I/usr/lib/erlang/usr/include/
EFLAGS= -pa $(EBIN) -smp
YFLAGS=-d 

vpath %.l $(SRCDIR)
vpath %.c $(SRCDIR)
vpath %.y $(SRCDIR)

NIFSO=parser_nif.so
OBJECTS=parser_nif.o scanner.o

.PHONY: erl all 

all : grammar.tab.c scanner.c  $(NIFSO)  erl

$(SRCDIR)/scanner.c : scanner.l grammar.tab.c 
	$(LEX) -o $@ $<

$(SRCDIR)/grammar.tab.c : grammar.y
	$(YACC) $< -o $@

erl : 
	$(REBAR) co

clean:
	rm -f $(SRCDIR)/*.o
	rm -f $(NIFDIR)/*.so
	rm -f $(SRCDIR)/scanner.c
	rm -f $(SRCDIR)/grammar.tab.c
	$(REBAR) clean

$(OBJECTS): %.o: %.c
	gcc -fpic -c $(CFLAGS) $< -o $(SRCDIR)/$@

$(NIFSO):	$(OBJECTS)
	gcc -shared -fpic -lfl $(patsubst %.o, $(SRCDIR)/%.o, $(OBJECTS)) -o $(NIFDIR)/$@

errr:
	@echo $(OBJECTS)
