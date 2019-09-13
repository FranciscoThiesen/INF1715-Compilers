#Victor Nogueira - 1511043 & Francisco Thiesen - 1611854

LEXFILE=monga.l
BISONFILE=monga.y
SRCS=main.c lex.yy.c monga.tab.c
CC=gcc
LEXHDR=lex.yy.h
MONGAHDR=monga.tab.h
RM=rm -f
DARWIN=Darwin
UNAME=$(shell uname)

ifeq ($(UNAME),$(DARWIN))
LDFLAGS=-ll -Wextra
else
LDFLAGS=-lfl -Wextra
endif

all: lex bison mongac

mongac: $(SRCS)
	$(CC) -o $@ $(LDFLAGS) $(SRCS)

lex: $(LEXFILE)
	lex --header-file=$(LEXHDR) $<

bison: $(BISONFILE)
	bison -d $<

clean:
	$(RM) lex.yy.c $(LEXHDR) monga.tab.c $(MONGAHDR) $(MONGAC)
