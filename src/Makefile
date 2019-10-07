#Victor Nogueira - 1511043 & Francisco Thiesen - 1611854

LEXFILE=monga.l
BISONFILE=monga.y
CC=gcc
LEXSRC=lex.yy.c
BISONSRC=monga.tab.c
SRCS=main.c ast.c $(LEXSRC) $(BISONSRC) printtree.c aux.c semantic.c
MONGAC=mongac
RM=rm -f
DARWIN=Darwin
UNAME=$(shell uname)

ifeq ($(UNAME),$(DARWIN))
LDFLAGS=-ll
else
LDFLAGS=-lfl
endif
CFLAGS=-Wextra -Wall

$(MONGAC): $(SRCS)
	$(CC) $(CFLAGS) -o $@ $(SRCS) $(LDFLAGS)

$(LEXSRC): $(LEXFILE)
	lex --header-file=lex.yy.h $<

$(BISONSRC): $(BISONFILE)
	bison -d $<

clean:
	$(RM) $(LEXSRC) lex.yy.h $(BISONSRC) monga.tab.h $(MONGAC)
