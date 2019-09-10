#Victor Nogueira - 1511043

LEXFILE=monga.l
BISONFILE=monga.y
SRCS=main.c lex.yy.c monga.tab.c
CC=gcc
LEXHDR=lex.yy.h
MONGAC=mongac
LEX=lex
BISON=bison
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

$(BISON): $(BISONFILE)
	$@ -d $<

clean:
	$(RM) $(LEXHDR) lex.yy.c monga.tab.c $(MONGAC)
