//Victor Nogueira - 1511043

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <libgen.h>
#include "monga.tab.h"
#include "lex.yy.h"

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

int main(int argc, char *argv[]) {
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

    if (yyparse()) {
	    printf("deu ruim\n");
	    exit(-1);
    }

    printf("deu certo\n");
    fclose(fin);
    fclose(fout);
}
