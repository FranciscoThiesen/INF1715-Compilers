//Victor Nogueira - 1511043

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <libgen.h>
#include "tk.h"
#include "lex.yy.h"

SemValue s;

char *strtoken[] = {
    "int",
    "float",
    "char",
    "bool",
    "true",
    "false",
    "if",
    "else",
    "while",
    "return",
    "new",
    "as",
    "id",
    "@",
    "==",
    ">=",
    "<=",
    "~=",
    "&&",
    "||",
};

static void usage(const char *prog) {
    fprintf(stderr,
            "usage: %s [OPTS] INPUTFILE\n"
            "\nOPTS:\n"
            "    -o OUTPUTFILE\n",
            prog);
}

char *gettoken(TK_EN token) {
    return strtoken[token - CHAR_MAX - 1];
}

int main(int argc, char *argv[]) {
    TK_EN token;
    int opt;
    FILE *fin;
    FILE *fout;
    const char *optstr = "o:";
    char inputfile[256];
    char outputfile[256];

    while ((opt = getopt(argc, argv, optstr)) != -1) {
        switch (opt) {
            case 'o':
                snprintf(outputfile, sizeof(outputfile),
                        "%s", optarg);
                break;
            default:
                usage(basename(argv[0]));
                return 1;
        }
    }

    if (optind == argc) {
        usage(basename(argv[0]));
        return 1;
    }
    snprintf(inputfile, sizeof(inputfile), "%s", argv[optind]);

    fin = fopen(inputfile, "r");
    fout = freopen(outputfile, "w", stdout);

    if (!fin) {
        fprintf(stderr, "error fopen input file: %s\n", strerror(errno));
        exit(-1);
    }

    if (!fout) {
        fprintf(stderr, "error fopen output file: %s\n", strerror(errno));
        exit(-1);
    }

    if (dup2(fileno(fout), 2) < 0) {
        fprintf(stderr, "couldn't redirect stderr: %s\n", strerror(errno));
    }

    yyrestart(fin);
    while((token = yylex())) {
        switch (token) {
            case TK_INT:
                printf("%s\n", gettoken(token));
                break;
            case TK_FLOAT:
                printf("%s\n", gettoken(token));
                break;
            case TK_CHAR:
                printf("%s\n", gettoken(token));
                break;
            case TK_BOOL:
                printf("%s\n", gettoken(token));
                break;
            case TK_TRUE:
                printf("%s\n", gettoken(token));
                break;
            case TK_FALSE:
                printf("%s\n", gettoken(token));
                break;
            case TK_IF:
                printf("%s\n", gettoken(token));
                break;
            case TK_ELSE:
                printf("%s\n", gettoken(token));
                break;
            case TK_WHILE:
                printf("%s\n", gettoken(token));
                break;
            case TK_RET:
                printf("%s\n", gettoken(token));
                break;
            case TK_NEW:
                printf("%s\n", gettoken(token));
                break;
            case TK_AS:
                printf("%s\n", gettoken(token));
                break;
            case TK_ID:
                printf("%s\n", s.s);
                free(s.s);
                break;
            case TK_PRINT:
                printf("%s\n", gettoken(token));
                break;
            case TK_EQUALS:
                printf("%s\n", gettoken(token));
                break;
            case TK_GEQUALS:
                printf("%s\n", gettoken(token));
                break;
            case TK_LEQUALS:
                printf("%s\n", gettoken(token));
                break;
            case TK_NEQUALS:
                printf("%s\n", gettoken(token));
                break;
            case TK_AND:
                printf("%s\n", gettoken(token));
                break;
            case TK_OR:
                printf("%s\n", gettoken(token));
                break;
            case TK_RAWINT:
                printf("%d\n", s.i);
                break;
            case TK_RAWFLOAT:
                printf("%f\n", s.d);
                break;
            case TK_STRING:
                printf("%s\n", s.s);
                free(s.s);
                break;
            case TK_LITERAL:
                printf("%c\n", s.i);
                break;
            default:
                if (token != ' ')
                    printf("%c\n", token);
        }
    }

    fclose(fin);
    fclose(fout);
}
