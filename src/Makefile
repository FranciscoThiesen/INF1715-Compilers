#Victor Nogueira - 1511043

LEXFILE=monga.l
SRCS=main.c lex.yy.c
CC=gcc
LEXHDR=lex.yy.h
MONGAC=mongac
LEX=lex
RM=rm -f
DARWIN=Darwin
UNAME=$(shell uname)

ifeq ($(UNAME),$(DARWIN))
LDFLAGS=-ll
else
LDFLAGS=-lfl
endif

$(MONGAC):$(LEX) $(SRCS)
	$(CC) -o $@ $(LDFLAGS) $(SRCS)

$(LEX): $(LEXFILE)
	$@ --header-file=$(LEXHDR) $<

clean:
	$(RM) $(LEXHDR) lex.yy.c $(MONGAC)
