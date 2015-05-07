# makefile for xndbmicro
# compile Erlang and C components
# wraps rebar

ERL=$(shell which erl)
REBAR=$(shell which rebar)
EBIN=$(ebin/)
LEX=flex
YACC=bison
SRCDIR=c_src
NIFDIR=priv
##################


CFLAGS=-I/usr/lib/erlang/usr/include/
EFLAGS= -pa $(EBIN) -smp

vpath %.l $(SRCDIR)
vpath %.c $(SRCDIR)
#vpath %.o $(OBJDIR)

NIFSO=parser_nif.so
OBJECTS=$(SRCDIR:.c=.o)

.PHONY: erl all scanner

all : $(NIFSO) scanner.c

scanner.c : scanner.l
	$(LEX) -o $(SRCDIR)/$@ $<

erl : 
	$(REBAR) co

clean:
	rm -f $(SRCDIR)/*.o
	rm -f $(NIFDIR)/*.so
	rm -f $(SRCDIR)/scanner.c
	$(REBAR) clean

$(OBJECTS): 
	gcc -fpic  -c $(CFLAGS) $< -o $@

$(NIFSO):	$(OBJECTS)
	gcc -shared -fpic -lfl -o $(NIFDIR)/$@
