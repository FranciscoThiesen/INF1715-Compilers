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

typedef union unary_def Unary_def;
typedef enum unary_def_types Unary_def_types;

enum unary_def_types {
    UVAR,
    UFUNC,
};

union unary_def {
    Unary_def_types tag;
    struct {
        Unary_def_types tag;
        Var *def;
        Unary_def *next;
    } var;
    struct {
        Unary_def_types tag;
        Func *def;
        Unary_def *next;
    } func;
};

static Unary_def *udefs[MAX_SCOPES];

#define INSERT_DEF(t, p)	\
    udef->t.def = p;		\
    udef->t.next = udefs[current_scope];

void clean_scope(int scope) {
    Unary_def *udef;
    Unary_def *uaux;

    udef = udefs[scope];
    while(udef) {
        switch(udef->tag) {
            case UVAR:
                uaux = udef;
                udef = udef->var.next;
                break;
            case UFUNC:
                uaux = udef;
                udef = udef->func.next;
                break;
            default:
                fprintf(stderr, "unknown unary def type in scope array %d\n", udef->tag);
                exit(-1);
        }
        free(uaux);
    }

}

void enter_scope() {
    current_scope++;
    clean_scope(current_scope);
    udefs[current_scope] = NULL;
}

void leave_scope() {
    current_scope--;
}

void init_symbols() {
    int scope;

    for (scope = 0; scope < MAX_SCOPES; scope++)
        udefs[scope] = NULL;
}

void clean_symbols() {
    clean_scope(current_scope);
}

static Unary_def *search_scope(char *id, int scope) {
    Unary_def *udef = udefs[scope];

    while (udef) {
        switch(udef->tag) {
            case UVAR:
                if (!(strcmp(udef->var.def->name, id)))
                    return udef;

                udef = udef->var.next;
                break;
            case UFUNC:
                if (!(strcmp(udef->func.def->name, id)))
                    return udef;

                udef = udef->func.next;
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

void insert_var(Var *v) {
    Unary_def *udef;

    if(check_name(v->name) < 0) {
        fprintf(stderr, "redefinition of symbol %s\n", v->name);
        exit(-1);
    }

    udef = tryalloc(sizeof(Unary_def));
    udef->tag = UVAR;
    INSERT_DEF(var, v);
    udefs[current_scope] = udef;
}

void insert_func(Func *f) {
    Unary_def *udef;

    if(check_name(f->name) < 0) {
        fprintf(stderr, "redefinition of symbol %s\n", f->name);
        exit(-1);
    }

    udef = tryalloc(sizeof(Unary_def));
    udef->tag = UFUNC;
    INSERT_DEF(func, f);
    udefs[current_scope] = udef;
}

Var *get_var(char *id, bool *error) {
    Unary_def *udef;
    int scope;

    *error = false;
    for (scope = current_scope; scope >= 0; scope--)
        if ((udef = search_scope(id, scope)) && udef->tag == UVAR)
            return udef->var.def;
        else if (udef && udef->tag != UVAR)
            *error = true;

    return NULL;
}

Func *get_func(char *id, bool *error) {
    Unary_def *udef;
    int scope;

    *error = false;
    for (scope = current_scope; scope >= 0; scope--)
        if ((udef = search_scope(id, scope)) && udef->tag == UFUNC)
            return udef->func.def;
        else if (udef && udef->tag != UFUNC)
            *error = true;

    return NULL;
}


