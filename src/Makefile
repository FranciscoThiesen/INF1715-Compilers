#Victor Nogueira - 1511043 & Francisco Thiesen - 1611854

LEXFILE=monga.l
BISONFILE=monga.y
CC=gcc
LEXSRC=lex.yy.c
BISONSRC=monga.tab.c
SRCS=main.c ast.c $(LEXSRC) $(BISONSRC)
MONGAC=mongac
RM=rm -f
DARWIN=Darwin
UNAME=$(shell uname)

ifeq ($(UNAME),$(DARWIN))
LDFLAGS=-ll -Wextra
else
LDFLAGS=-lfl -Wextra
endif

$(MONGAC): $(SRCS)
	$(CC) -o $@ $(LDFLAGS) $(SRCS)

$(LEXSRC): $(LEXFILE)
	lex --header-file=lex.yy.h $<

$(BISONSRC): $(BISONFILE)
	bison -d $<

clean:
	$(RM) $(LEXSRC) lex.yy.h $(BISONSRC) monga.tab.h $(MONGAC)
