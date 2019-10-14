//Victor Nogueira - 1511043 & Francisco Thiesen - 1611854

#include <string.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include "symbols.h"
#include "aux.h"
#include "ast.h"

#define MAX_SCOPES 40

static int current_scope = 0;

static Def *defs[MAX_SCOPES];

#define INSERT_DEF(t, p)	\
    def->t.def = p;		\
    def->t.next = defs[current_scope];

void clean_scope(int scope) {
    Def *def;
    Def *uaux;

    def = defs[scope];
    while(def) {
        switch(def->tag) {
            case DEFVAR:
                uaux = def;
                def = def->var.next;
                break;
            case DEFFUNC:
                uaux = def;
                def = def->func.next;
                break;
            default:
                fprintf(stderr, "unknown unary def type in scope array %d\n",
                        def->tag);
                exit(-1);
        }
        free(uaux);
    }

}

void enter_scope() {
    current_scope++;
    clean_scope(current_scope);
    defs[current_scope] = NULL;
}

void leave_scope() {
    current_scope--;
}

void init_symbols() {
    int scope;

    for (scope = 0; scope < MAX_SCOPES; scope++)
        defs[scope] = NULL;
}

void clean_symbols() {
    clean_scope(current_scope);
}

static Def *search_scope(char *id, int scope) {
    Def *def = defs[scope];

    while (def) {
        switch(def->tag) {
            case DEFVAR:
                if (!(strcmp(def->var.def->name, id)))
                    return def;

                def = def->var.next;
                break;
            case DEFFUNC:
                if (!(strcmp(def->func.def->name, id)))
                    return def;

                def = def->func.next;
                break;
            default:
                fprintf(stderr, "unknown unary def type in scope array\n");
                exit(-1);
        }
    }

    return NULL;
}

static int check_name(char *id) {
    if (search_scope(id, current_scope))
        return -1;

    return 0;
}

bool insert_var(Var *v) {
    Def *def;

    if(check_name(v->name) < 0)
        return true;

    def = tryalloc(sizeof(Def));
    def->tag = DEFVAR;
    INSERT_DEF(var, v);
    defs[current_scope] = def;

    return false;
}

bool insert_func(Func *f) {
    Def *def;

    if(check_name(f->name) < 0)
        return true;

    def = tryalloc(sizeof(Def));
    def->tag = DEFFUNC;
    INSERT_DEF(func, f);
    defs[current_scope] = def;

    return false;
}

Var *get_var(char *id, bool *error) {
    Def *def;
    int scope;

    *error = false;
    for (scope = current_scope; scope >= 0; scope--)
        if ((def = search_scope(id, scope)) && def->tag == DEFVAR)
            return def->var.def;
        else if (def && def->tag != DEFVAR)
            *error = true;

    return NULL;
}

Func *get_func(char *id, bool *error) {
    Def *def;
    int scope;

    *error = false;
    for (scope = current_scope; scope >= 0; scope--)
        if ((def = search_scope(id, scope)) && def->tag == DEFFUNC)
            return def->func.def;
        else if (def && def->tag != DEFFUNC)
            *error = true;

    return NULL;
}
