//Victor Nogueira - 1511043 & Francisco Thiesen - 1611854

#ifndef _TK_H
#define _TK_H

#include <limits.h>

typedef enum {
   TK_INT = CHAR_MAX + 1,
   TK_FLOAT,
   TK_CHAR,
   TK_BOOL,
   TK_TRUE,
   TK_FALSE,
   TK_IF,
   TK_ELSE,
   TK_WHILE,
   TK_RET,
   TK_NEW,
   TK_AS,
   TK_ID,
   TK_PRINT,
   TK_EQUALS,
   TK_GEQUALS,
   TK_LEQUALS,
   TK_NEQUALS,
   TK_AND,
   TK_OR,
   TK_RAWFLOAT,
   TK_RAWINT,
   TK_STRING,
   TK_LITERAL,
} TK_EN;

typedef union {
   double d;
   int i;
   char *s;
} SemValue;

extern SemValue s;

#endif
