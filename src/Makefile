#Victor Nogueira - 1511043 & Francisco Thiesen - 1611854

LEXFILE=monga.l
BISONFILE=monga.y
SRCS=main.c lex.yy.c monga.tab.c
CC=gcc
LEXHDR=lex.yy.h
MONGAHDR=monga.tab.h
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

$(MONGAC): $(LEX) $(BISON) $(SRCS)
	$(CC) -o $@ $(LDFLAGS) $(SRCS)

$(LEX): $(LEXFILE)
	$@ --header-file=$(LEXHDR) $<

$(BISON): $(BISONFILE)
	$@ -d $<

clean:
	$(RM) lex.yy.c $(LEXHDR) monga.tab.c $(MONGAHDR) $(MONGAC)
