%option noyywrap
%option noinput
%option nounput
%{
//Victor Nogueira - 1511043 & Francisco Thiesen - 1611854

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <fcntl.h>
#include "monga.tab.h"
#include "aux.h"

static char *stringcpy();
static int literalcpy();
static int intcpy();
static int inthexcpy();
static float floatcpy();
static char *idcpy();

int line = 1;
%}

expdec          ([eE][+-]?[0-9]+)
exphex          ([pP][+-]?[0-9]+)
decdig          [0-9]
hexdig          ([0-9a-fA-F])
decfloat        ({decdig}+("."{decdig}*)?|("."{decdig})+){expdec}?
hexfloat        0[xX]({hexdig}+("."{hexdig}*)?|"."{hexdig}+){exphex}?
string          \"([^\\"\n]|\\.)*\"
literal         \'([^\\'\n]|\\.)?\'
id              [a-zA-Z_][a-zA-Z_0-9]*

%%
int                             { return TK_INT; }

float                           { return TK_FLOAT; }

char                            { return TK_CHAR; }

bool                            { return TK_BOOL; }

true                            {
                                    yylval.b = true;
                                    return TK_TRUE;
                                }

false                           {
                                    yylval.b = false;
                                    return TK_FALSE;
                                }

if                              { 
                                    yylval.line = line;
                                    return TK_IF;
                                }

else                            { return TK_ELSE; }

while                           { 
                                    yylval.line = line;
                                    return TK_WHILE;
                                }

return                          { 
                                    yylval.line = line;
                                    return TK_RET;
                                }

new                             { 
                                    yylval.line = line;
                                    return TK_NEW;
                                }

as                              { 
                                    yylval.line = line;
                                    return TK_AS;
                                }

"@"                             { 
                                    yylval.line = line;
                                    return TK_PRINT;
                                }

"=="                            {
                                    yylval.line = line;
                                    return TK_EQUALS;
                                }

">="                            {
                                    yylval.line = line;
                                    return TK_GEQUALS;
                                }

"<="                            { 
                                    yylval.line = line;
                                    return TK_LEQUALS;
                                }

"~="                            {
                                    yylval.line = line;
                                    return TK_NEQUALS;
                                }

"&&"                            {
                                    yylval.line = line;
                                    return TK_AND;
                                }

"||"                            {
                                    yylval.line = line;
                                    return TK_OR;
                                }

{id}                            {
                                    yylval.str = idcpy();
                                    return TK_ID;
                                }

{decdig}+                       {
                                    yylval.i = intcpy();
                                    return TK_RAWINT;
                                }

{decfloat}                      {
                                    yylval.f = floatcpy();
                                    return TK_RAWFLOAT;
                                }

0[xX]{hexdig}+                  {
                                    yylval.i = inthexcpy();
                                    return TK_RAWINT;
                                }

{hexfloat}                      {
                                    yylval.f = floatcpy();
                                    return TK_RAWFLOAT;
                                }

{string}                        {
                                    yylval.str = stringcpy();
                                    return TK_STRING;
                                }
{literal}                       {
                                   yylval.i = literalcpy();
                                   return TK_RAWCHAR;
                                }

\#[^\n]*\n                      { line++; }

\n                              { line++; }

[ \t]+                          {}

.                               { 
                                    yylval.line = line;
                                    return yytext[0];
                                }
%%

static char handleescape(char escaped) {
    switch(escaped) {
        case 'n':
            return '\n';
        case '\\':
            return '\\';
        case '"':
            return '"';
        case '\'':
            return '\'';
        default:
            fprintf(stderr, "There is no escaping this character: %c\n", escaped);
            exit(-1);
    }
}

static char *stringcpy() {
    char *newstr = (char *) tryalloc(yyleng - 1);
    int i, j;

    for (i = 0, j = 1; j < (int) yyleng - 1; i++, j++) {
        if (yytext[j] == '\\')
            newstr[i] = handleescape(yytext[++j]);
        else
            newstr[i] = yytext[j];
    }

    return newstr;
}

static int  literalcpy() {
    int  newchar;

    if (yytext[1] ==  '\\') {
        newchar = handleescape(yytext[2]);
        goto ret;
    }

    newchar = yytext[1];

    ret:
    return newchar;
}

static char *idcpy() {
    char *newstr = (char *) tryalloc(yyleng + 1);

    strncpy(newstr, yytext, yyleng + 1);

    return newstr;
}

static int intcpy() {
    return strtol(yytext, NULL, 10);
}

static int inthexcpy() {
    return strtol(yytext, NULL, 16);
}

static float floatcpy() {
    return strtof(yytext, NULL);
}
