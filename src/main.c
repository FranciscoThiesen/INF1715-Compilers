//Victor Nogueira - 1511043 & Francisco Thiesen - 1611854

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <libgen.h>
#include "monga.tab.h"
#include "lex.yy.h"
#include "ast.h"
#include "typing.h"
#include "symbols.h"
#include "aux.h"

Def *GLOBAL_TREE;
State *global_state;

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
	bool accepted;
    const char *optstr = "o:";
    char inputfile[256];
    char outputfile[256];

    memset(outputfile, 0, 256);
    memset(inputfile, 0, 256);
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
    if (strcmp(outputfile, "\0")) {
        fout = freopen(outputfile, "w", stdout);

        if (!fout) {
            fprintf(stderr, "error fopen output file: %s\n", strerror(errno));
            exit(-1);
        }
    }

    if (!fin) {
        fprintf(stderr, "error fopen input file: %s\n", strerror(errno));
        exit(-1);
    }

    yyrestart(fin);

    if (yyparse())
        exit(-1);

    global_state = tryalloc(sizeof(State));
    init_symbols();
    accepted = type_tree();
	if (accepted)
		fprintf(stderr, "accepted\n");
	else
		fprintf(stderr, "rejected\n");

    fclose(fin);
    if (strcmp(outputfile, "\0"))
        fclose(fout);

    clean_symbols();
}
