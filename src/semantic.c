//Victor Nogueira - 1511043 & Francisco Thiesen - 1611854

#include <string.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include "semantic.h"
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

// Inicio das funcoes do sistema de tipos

/*
static int compare_type(Type *t1, Type *t2) {
    if(t1->tag != t2->tag) return 0;
    if(t1->tag == SEQ) return compareType(t1->seq.next, t2->seq.next );
    return ( t1->single.type == t2->single.type );
}
*/

static int is_float(Type *t) {
    if(t->tag == SINGLE) return (t->single.type == FLOAT);
    return is_float(t->seq.next);
}

static int is_int(Type  *t) {
    if(t->tag == SINGLE) return (t->single.type == INT);
    return is_int(t->seq.next);
}

static int is_char(Type *t) {
    if(t->tag == SINGLE) return (t->single.type == CHAR);
    return is_char(t->seq.next);
}

static int is_bool(Type *t) {
    if(t->tag == SINGLE) return (t->single.type == BOOL);
    return is_bool(t->seq.next);
}

static int is_array(Type *t) { return (t->tag == SEQ); }

/*
Type *get_exp_type(Exp *exp) {
    if(exp == NULL) {
        printf("Null expression has no type?!");
        exit(-1);
    }
    
    // A ideia eh que essa funcao vai ser chamada na printtree, para cada expressao
    switch(exp->tag) {
        case VARID:
            return get_type_varid(exp);
        case CALLEXP:
            return get_type_callexp(exp);
        case AS:
            return get_type_as(exp);
        case NEW:
            return get_type_new(exp);
        case SUM:
            return get_type_expsum(exp);
        case SUB:
            return get_type_expsub(exp);
        case MINUS:
            return get_type_unaryminus(exp);
        case MUL:
            return get_type_expmul(exp);
        case DIV:
            return get_type_expdiv(exp);
        case EXPINT:
            return get_type_expint(exp); // da pra retornar INT de cara?
        case EXPFLOAT:
            return get_type_expfloat(exp); // da pra retornar float de cara?
        case EXPCHAR:
            return get_type_expchar(exp);
        case EXPSTR:
            return get_type_expstr(exp);
        case EXPBOOL:
            return get_type_expbool(exp);
        case EXPATT:
            return get_type_expatt(exp);
        case EQ:
            return get_type_eq(exp);
        case NEQ:
            return get_type_neq(exp);
        case LEQ:
            return get_type_leq(exp);
        case GEQ:
            return get_type_geq(exp);
        case L:
            return get_type_less(exp);
        case G:
            return get_type_greater(exp);
        case NOT:
            return get_type_unarynot(exp);
        case AND:
            return get_type_and(exp);
        case OR:
            return get_Type_or(exp);
        default:
            fprintf(stderr, "Unknown expression %d!\n", exp->tag);
            exit(-1);
    }
    
}
*/


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

Var *get_var(char *id) {
    Unary_def *udef;
    int scope;

    for (scope = current_scope; scope >= 0; scope--)
        if ((udef = search_scope(id, scope)))
            return udef->var.def;

    return NULL;
}

Func *get_func(char *id) {
    Unary_def *udef;
    int scope;

    for (scope = current_scope; scope >= 0; scope--)
        if ((udef = search_scope(id, scope)))
            return udef->func.def;

    return NULL;
}


