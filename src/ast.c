#include "ast.h"
#include <string.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>

struct prog *GLOBAL_TREE;

static void *tryalloc(size_t size) {
    void *newptr = malloc(sizeof(char) * size);

    if (!newptr) {
        fprintf(stderr, "error: %s\n", strerror(ENOMEM));
        exit(-1);
    }

    return newptr;
}

struct var *vardef(char *name, union type *type) {
    struct var *var = tryalloc(sizeof(struct var));

    var->name = name;
    var->type = type;
    var->next = NULL;

    return var;
}

struct var *varseqdef(struct var *v1, struct var *v2) {
    struct var *vaux;

    if (!v1) {
        v2->next = v1;
        return v2;
    }

    vaux = v1;
    while (vaux->next) {
        vaux = vaux->next;
    }

    vaux->next = v2;
    return v1;
}

struct func *func(char *name, struct param *params, union type * type,
        struct stat *stat) {
    struct func *func = tryalloc(sizeof(struct func));

    func->name = name;
    func->param = params;
    func->type = type;
    func->stat = stat;
    func->next = NULL;

    return func;
}

struct func *funcseq(struct func *f1, struct func *f2) {
    f1->next = f2;

    return f1;
}

union type *newtype(enum native_types ntype) {
    union type *type = tryalloc(sizeof(union type));

    type->tag = SINGLE;
    type->single.type = ntype;

    return type;
}

union type *newseqtype(union type *t1) {
    union type *type = tryalloc(sizeof(union type));

    type->tag = SEQ;
    type->seq.next = t1;

    return type;
}

struct param *newparamseq(struct param *p1, struct param *p2) {
    p1->next = p2;

    return p1;
}

struct param *newparam(char *name, union type *type) {
    struct param *param = tryalloc(sizeof(struct param));

    param->name = name;
    param->type = type;

    return param;
}

struct stat *newstat(struct var *var, union cmd *cmd) {
    struct stat *stat = tryalloc(sizeof(struct stat));

    stat->vars = var;
    stat->cmds = cmd;

    return stat;
}

union cmd *newcmd(enum cmd_type tag, union exps *exp, struct stat *stat, struct stat *stat2) {
    union cmd *cmd = tryalloc(sizeof(union cmd));

    cmd->tag = tag;
    switch (tag) {
        case IF:
            cmd->cmd_if.exp = exp;
            cmd->cmd_if.stat = stat;
            cmd->cmd_if.next = NULL;
            break;
        case IFELSE:
            cmd->cmd_ifelse.exp = exp;
            cmd->cmd_ifelse.stat = stat;
            cmd->cmd_ifelse.stat2 = stat2;
            cmd->cmd_ifelse.next = NULL;
            break;
        case WHILE:
            cmd->cmd_while.exp = exp;
            cmd->cmd_while.stat = stat;
            cmd->cmd_while.next = NULL;
            break;
        case RETEXP:
            cmd->cmd_ret_exp.exp = exp;
            cmd->cmd_ret_exp.next = NULL;
            break;
        case RET:
            cmd->cmd_ret.next = NULL;
            break;
        case PRINT:
            cmd->print.exp = exp;
            cmd->print.next = NULL;
            break;
        default:
            fprintf(stderr, "unknown command type\n");
            exit(-1);
    }

    return cmd;
}

union cmd *callcmd(union exps *call) {
    union cmd *cmd = tryalloc(sizeof(union cmd));

    cmd->tag = CALLCMD;
    cmd->call.call = call;
    cmd->call.next = NULL;

    return cmd;
}

union cmd *statcmd(struct stat *stat) {
    union cmd *cmd = tryalloc(sizeof(union cmd));

    cmd->tag = STAT;
    cmd->stat.stat = stat;
    cmd->stat.next = NULL;

    return cmd;
}

union cmd *attcmd(union exps *att) {
    union cmd *cmd = tryalloc(sizeof(union cmd));

    cmd->tag = ATT;
    cmd->att.att = att;
    cmd->att.next;
}

union cmd *newseqcmd(union cmd *c1, union cmd *c2) {
    switch (c1->tag) {
        case IF:
            c1->cmd_if.next = c2;
            break;
        case IFELSE:
            c1->cmd_ifelse.next = c2;
            break;
        case WHILE:
            c1->cmd_while.next = c2;
            break;
        case RETEXP:
            c1->cmd_ret_exp.next = c2;
            break;
        case RET:
            c1->cmd_ret.next = c2;
            break;
        case PRINT:
            c1->print.next = c2;
            break;
        case CALLCMD:
            c1->call.next = c2;
            break;
        case ATT:
            c1->att.next = c2;
            break;
        default:
            fprintf(stderr, "unknown command type\n");
            exit(-1);
    }

    return c1;
}

union exps *unaryexp(enum exp_type tag, union exps *e1) {
    union exps *exp = tryalloc(sizeof(union exps));

    exp->tag = tag;
    exp->unary.exp = e1;
    exp->unary.next = NULL;

    return exp;
}

union exps *binaryexp(enum exp_type tag, union exps *e1, union exps *e2) {
    union exps *exp = tryalloc(sizeof(union exps));

    exp->tag = tag;
    exp->binary.e1 = e1;
    exp->binary.e2 = e2;
    exp->binary.next = NULL;

    return exp;
}

union exps *asexp(union exps *e1, union type *type) {
    union exps *exp = tryalloc(sizeof(union exps));

    exp->tag = AS;
    exp->as.type = type;
    exp->as.exp = e1;
    exp->as.next = NULL;

    return exp;
}

union exps *callexp(char *name, union exps *e1) {
    union exps *exp = tryalloc(sizeof(union exps));

    exp->tag = CALLEXP;
    exp->call.name = name;
    exp->call.exp = e1;
    exp->call.next = NULL;

    return exp;
}

union exps *newexp(union type *type, union exps *e1) {
    union exps *exp = tryalloc(sizeof(union exps));

    exp->tag = NEW;
    exp->new.type = type;
    exp->new.exp = e1;
    exp->new.next = NULL;

    return exp;
}

union exps *newvarid(char *name) {
    union exps *exp = tryalloc(sizeof(union exps));

    exp->tag = VAR;
    exp->var.name = name;
    exp->var.next = NULL;

    return exp;
}

union exps *listexp(union exps *e1, union exps *e2) {
    switch(e1->tag) {
        case VAR:
            e1->var.next = e2;
            break;
        case CALLEXP:
            e1->call.next = e2;
            break;
        case AS:
            e1->as.next = e2;
            break;
        case NEW:
            e1->new.next = e2;
            break;
        case SUM:
            e1->binary.next = e2;
            break;
        case SUB:
            e1->binary.next = e2;
            break;
        case MUL:
            e1->binary.next = e2;
            break;
        case DIV:
            e1->binary.next = e2;
            break;
        case EQ:
            e1->binary.next = e2;
            break;
        case NEQ:
            e1->binary.next = e2;
            break;
        case LEQ:
            e1->binary.next = e2;
            break;
        case GEQ:
            e1->binary.next = e2;
            break;
        case L:
            e1->binary.next = e2;
            break;
        case G:
            e1->binary.next = e2;
            break;
        case NOT:
            e1->unary.next = e2;
            break;
        case AND:
            e1->binary.next = e2;
            break;
        case OR:
            e1->binary.next = e2;
            break;
        case EXPINT:
            e1->expint.next = e2;
            break;
        case EXPFLOAT:
            e1->expfloat.next = e2;
            break;
        case EXPCHAR:
            e1->expchar.next = e2;
            break;
        case EXPSTR:
            e1->expstr.next = e2;
            break;
        case EXPBOOL:
            e1->expbool.next = e2;
            break;
        default:
            fprintf(stderr, "unknown expression type");
            exit(-1);
    }

    return e1;
}

union exps *newint(int i) {
    union exps *exp = tryalloc(sizeof(union exps));

    exp->tag = EXPINT;
    exp->expint.i = i;
    exp->expint.type = newtype(INT);
    exp->expint.next = NULL;

    return exp;
}

union exps *newfloat(double d) {
    union exps *exp = tryalloc(sizeof(union exps));

    exp->tag = EXPFLOAT;
    exp->expfloat.d = d;
    exp->expfloat.type = newtype(FLOAT);
    exp->expfloat.next = NULL;

    return exp;
}

union exps *newchar(char c) {
    union exps *exp = tryalloc(sizeof(union exps));

    exp->tag = EXPCHAR;
    exp->expchar.c = c;
    exp->expchar.type = newtype(CHAR);
    exp->expchar.next = NULL;

    return exp;
}

union exps *newstr(char *s) {
    union exps *exp = tryalloc(sizeof(union exps));

    exp->tag = EXPSTR;
    exp->expstr.str = s;
    exp->expstr.type = newseqtype(newtype(CHAR));
    exp->expstr.next = NULL;

    return exp;
}

union exps *newbool(bool b) {
    union exps *exp = tryalloc(sizeof(union exps));

    exp->tag = EXPBOOL;
    exp->expbool.b = b;
    exp->expbool.type = newtype(BOOL);
    exp->expbool.next = NULL;

    return exp;
}

struct prog *prognode(struct var *vars, struct func *funcs) {
    struct prog *node = tryalloc(sizeof(struct prog));

    node->vars = vars;
    node->funcs = funcs;

    return node;
}
