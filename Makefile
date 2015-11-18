# makefile for xndbmicro
# compile Erlang and C components
# wraps rebar

#indent -kr -ci2 -cli2 -i2 -l80 -nut

ERL=$(shell which erl)
REBAR=$(shell which rebar)
EBIN=$(ebin/)
LEX=flex
YACC=bison -d --report-file=grammar.output --verbose
SRCDIR=csrc
NIFDIR=priv
CC=gcc
##################


CFLAGS=-fpic -I/usr/lib/erlang/usr/include/ -Icsrc/ -g
EFLAGS= -pa $(EBIN) -smp
YFLAGS=-d 

vpath %.l $(SRCDIR)
vpath %.c $(SRCDIR)
vpath %.y $(SRCDIR)

NIFSO=parser_nif.so
OBJECTS=parser_nif.o scanner.o grammar.tab.o parsetree.o

##################
#test harness

UNITYROOT=$(HOME)/Unity
TESTDIR=$(SRCDIR)/test
TESTINC=-I$(SRCDIR) -I$(UNITYROOT)/src -I$(UNITYROOT)/extras/fixture/src
TESTSRC=$(UNITYROOT)/src/unity.c \
  	$(UNITYROOT)/extras/fixture/src/unity_fixture.c 
	
TESTTARGET=all_tests
TESTCFLAGS=-Icsrc/ -g

##################

.PHONY: erl all 


all : grammar.tab.c scanner.c  $(NIFSO)  erl

debug: CFLAGS += -DXDEBUG
debug: all

$(SRCDIR)/scanner.c : scanner.l grammar.tab.c 
	$(LEX) --header-file=$(SRCDIR)/scanner.h -o $@ $<

$(SRCDIR)/grammar.tab.c : grammar.y
	$(YACC) $< -o $@

erl : 
	$(REBAR) co

clean:
	rm -f $(SRCDIR)/*.o
	rm -f $(NIFDIR)/*.so
	rm -f $(SRCDIR)/scanner.c
	rm -f $(SRCDIR)/scanner.h
	rm -f $(SRCDIR)/grammar.tab.c
	$(REBAR) clean

$(OBJECTS): %.o: %.c
	$(CC) -c $(CFLAGS) $< -o $(SRCDIR)/$@

$(NIFSO):	$(OBJECTS)
	$(CC) -g -shared -fpic -lfl $(patsubst %.o, $(SRCDIR)/%.o, $(OBJECTS)) -Wl,--export-dynamic -o $(NIFDIR)/$@

errr:
	@echo $(OBJECTS)

test:	CFLAGS = $(TESTCFLAGS)
test:	grammar.tab.c scanner.c scanner.o grammar.tab.o parsetree.o
	$(CC) -DUNITY_FIXTURES $(CFLAGS) $(TESTINC) $(TESTSRC) $(SRCDIR)/test/testParser.c $(SRCDIR)/test/parsetreeUtils.c $(patsubst %.o, $(SRCDIR)/%.o, scanner.o grammar.tab.o parsetree.o) -lfl -o $(TESTTARGET)
	./$(TESTTARGET) -v	

testdebug: 	TESTCFLAGS += -DXDEBUG
testdebug:	test
