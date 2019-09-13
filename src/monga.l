%{
//Victor Nogueira - 1511043 & Francisco Thiesen - 1611854

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include "monga.tab.h"
#include "aux.h"

static char *stringcpy();
static char literalcpy();
static int intcpy();
static int inthexcpy();
static double doublecpy();
static char *idcpy();

unsigned int line = 1;
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

true                            { return TK_TRUE; }

false                           { return TK_FALSE; }

if                              { return TK_IF; }

else                            { return TK_ELSE; }

while                           { return TK_WHILE; }

return                          { return TK_RET; }

new                             { return TK_NEW; }

as                              { return TK_AS; }

"@"                             { return TK_PRINT; }

"=="                            { return TK_EQUALS; }

">="                            { return TK_GEQUALS; }

"<="                            { return TK_LEQUALS; }

"~="                            { return TK_NEQUALS; }

"&&"                            { return TK_AND; }

"||"                            { return TK_OR; }

{id}                            {
                                    //yylval = idcpy();
                                    return TK_ID;
                                }

{decdig}+                       {
                                    //yylval = intcpy();
                                    return TK_RAWINT;
                                }

{decfloat}                      {
                                    //yylval = doublecpy();
                                    return TK_RAWFLOAT;
                                }

0[xX]{hexdig}+                  {
                                    //yylval = inthexcpy();
                                    return TK_RAWINT;
                                }

{hexfloat}                      {
                                    //yylval = doublecpy();
                                    return TK_RAWFLOAT;
                                }

{string}                        {
                                    //yylval = stringcpy();
                                    return TK_STRING;
                                }
{literal}                       {
                                   //yylval = literalcpy();
                                   return TK_LITERAL;
                                }

\#[^\n]*\n                      { line++; }

\n                              { line++; }

[ \t]+				{}

.                               { return yytext[0]; }
%%

static void *tryalloc(int size) {
    void *newptr = malloc(sizeof(char) * size);

    if (!newptr) {
        fprintf(stderr, "error: %s\n", strerror(ENOMEM));
        exit(-1);
    }

    return newptr;
}

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
    char *newstr = (char *) tryalloc(yyleng + 1);
    int i = 0, j = 0;

    while(yytext[i]) {
        if (yytext[i] == '\\') {
            newstr[j] = handleescape(yytext[i + 1]);
            i++;
        } else if (yytext[i] == '"') {
            goto nextchar;
        } else {
            newstr[j] = yytext[i];
        }
        j++;

        nextchar:
        i++;
    }

    return newstr;
}

static char literalcpy() {
    char newchar;

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

static double doublecpy() {
    return strtod(yytext, NULL);
}
