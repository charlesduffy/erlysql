# makefile for xndbmicro
# compile Erlang and C components
# wraps rebar

ERL=$(shell which erl)
REBAR=$(shell which rebar)
EBIN=$(ebin/)
LEX=flex
YACC=bison
SRCDIR=csrc
NIFDIR=priv
##################


CFLAGS=-I/usr/lib/erlang/usr/include/
EFLAGS= -pa $(EBIN) -smp

vpath %.l $(SRCDIR)
vpath %.c $(SRCDIR)

NIFSO=parser_nif.so
OBJECTS=parser_nif.o scanner.o

.PHONY: erl all 

all : scanner.c  $(NIFSO)  erl

$(SRCDIR)/scanner.c : scanner.l
	$(LEX) -o $@ $<

erl : 
	$(REBAR) co

clean:
	rm -f $(SRCDIR)/*.o
	rm -f $(NIFDIR)/*.so
	rm -f $(SRCDIR)/scanner.c
	$(REBAR) clean

$(OBJECTS): %.o: %.c
	gcc -fpic -c $(CFLAGS) $< -o $(SRCDIR)/$@

$(NIFSO):	$(OBJECTS)
	gcc -shared -fpic -lfl $(patsubst %.o, $(SRCDIR)/%.o, $(OBJECTS)) -o $(NIFDIR)/$@

errr:
	@echo $(OBJECTS)
