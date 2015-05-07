# makefile for xndbmicro
# compile Erlang and C components
# wraps rebar

ERL=$(shell which erl)
EFLAGS= -pa /ebin -smp
LEX=flex
YACC=bison
LFLAGS=-o src/scanner.c

CSOURCES=*.c
LSOURCES=*.l

vpath %.l src
vpath $.c src

.PHONY: deps erlco all



all:

		
scanner : scanner.c
	flex scanner.l 
