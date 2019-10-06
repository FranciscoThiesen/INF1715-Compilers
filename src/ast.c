//Victor Nogueira - 1511043 & Francisco Thiesen - 1611854

#include "ast.h"
#include <string.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>

static void *tryalloc(size_t size) {
    void *newptr = malloc(sizeof(char) * size);

    if (!newptr) {
        fprintf(stderr, "error: %s\n", strerror(ENOMEM));
        exit(-1);
    }

    return newptr;
}

static Def *defvar(Var *var) {
    Def *newdef = tryalloc(sizeof(Def));
    newdef->tag = DEFVAR;
    newdef->vars.vars = var;
    newdef->vars.next = NULL;
    return newdef;
}

static Def *deffunc(Func *func) {
    Def *newdef = tryalloc(sizeof(Def));
    newdef->tag = DEFFUNC;
    newdef->funcs.funcs = func;
    newdef->funcs.next = NULL;
    return newdef;
}

Def *def(Def_types type, Var *var, Func *func) {
    switch (type){
        case DEFVAR:
            return defvar(var);
        case DEFFUNC:
            return deffunc(func);
        default:
            fprintf(stderr, "unknown type\n");    
            exit(-1);
    }
    return NULL;
}

Def *defseq(Def *delem, Def *dlist) {
    if (!delem)
        return dlist;
    switch (delem->tag){
        case DEFVAR:
            delem->vars.next= dlist;
            break;
        case DEFFUNC:
            delem->funcs.next = dlist;
            break;
        default:
            fprintf(stderr, "unknown type\n");    
            exit(-1);
    }
    return delem;
}

Var *vardef(char *name, Type *type) {
    Var *var = tryalloc(sizeof(Var) );

    var->name = name;
    var->type = type;
    var->next = NULL;

    return var;
}

Var *varseqdef(Var *v1, Var *v2) {
    Var *vaux;
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

Func *func(char *name, Param *params, Type * type,
        Stat *stat) {
    Func *func = tryalloc(sizeof(Func));

    func->name = name;
    func->param = params;
    func->type = type;
    func->stat = stat;
    func->next = NULL;

    return func;
}

Func *funcseq(Func *f1, Func *f2) {
    f1->next = f2;

    return f1;
}

Type *newtype(Native_types ntype) {
    Type *type = tryalloc(sizeof(Type));

    type->tag = SINGLE;
    type->single.type = ntype;

    return type;
}

Type *newseqtype(Type *t1) {
    Type *type = tryalloc(sizeof(Type));

    type->tag = SEQ;
    type->seq.next = t1;

    return type;
}

Param *newparamseq(Param *p1, Param *p2) {
    p1->next = p2;

    return p1;
}

Param *newparam(char *name, Type *type) {
    Param *param = tryalloc(sizeof(Param));

    param->name = name;
    param->type = type;

    return param;
}

Stat *newstat(Var *var, Cmd *cmd) {
    Stat *stat = tryalloc(sizeof(Stat));

    stat->vars = var;
    stat->cmds = cmd;

    return stat;
}

static Cmd *newcmd_if(Exps *exp, Stat *stat) {
    Cmd *cmd = tryalloc(sizeof(Cmd));
    cmd->tag = IF;
    cmd->cmd_if.exp = exp;
    cmd->cmd_if.stat = stat;
    cmd->cmd_if.next = NULL;
    return cmd;
}

static Cmd *newcmd_ifelse(Exps *exp, Stat *stat, Stat *stat2) {
    Cmd *cmd = tryalloc(sizeof(Cmd));
    cmd->tag = IFELSE;
    cmd->cmd_ifelse.exp = exp;
    cmd->cmd_ifelse.stat = stat;
    cmd->cmd_ifelse.stat2 = stat2;
    cmd->cmd_ifelse.next = NULL;
    return cmd;
}

static Cmd *newcmd_while(Exps *exp, Stat *stat) {
    Cmd *cmd = tryalloc(sizeof(Cmd));
    cmd->tag = WHILE;
    cmd->cmd_while.exp = exp;
    cmd->cmd_while.stat = stat;
    cmd->cmd_while.next = NULL;
    return cmd;
}

static Cmd *newcmd_retexp(Exps *exp) {
    Cmd *cmd = tryalloc(sizeof(Cmd));
    cmd->tag = RETEXP;
    cmd->cmd_ret_exp.exp = exp;
    cmd->cmd_ret_exp.next = NULL;
    return cmd;
}

static Cmd *newcmd_ret() {
    Cmd *cmd = tryalloc(sizeof(Cmd));
    cmd->tag = RET;
    cmd->cmd_ret.next = NULL;
    return cmd;
}

static Cmd *newcmd_print(Exps *exp) {
    Cmd *cmd = tryalloc(sizeof(Cmd));
    cmd->tag = PRINT;
    cmd->print.exp = exp;
    cmd->print.next = NULL;
    return cmd;
}

Cmd *newcmd(Cmd_type tag, Exps *exp, Stat *stat, Stat *stat2) {
    switch (tag) {
        case IF:
            return newcmd_if(exp, stat);
        case IFELSE:
            return newcmd_ifelse(exp, stat, stat2);
        case WHILE:
            return newcmd_while(exp, stat);
        case RETEXP:
            return newcmd_retexp(exp);
        case RET:
            return newcmd_ret();
        case PRINT:
            return newcmd_print(exp);
        default:
            fprintf(stderr, "unknown command type\n");
            exit(-1);
    }
}

Cmd *callcmd(Exps *call) {
    Cmd *cmd = tryalloc(sizeof(Cmd));

    cmd->tag = CALLCMD;
    cmd->call.call = call;
    cmd->call.next = NULL;

    return cmd;
}

Cmd *statcmd(Stat *stat) {
    Cmd *cmd = tryalloc(sizeof(Cmd));

    cmd->tag = STAT;
    cmd->stat.stat = stat;
    cmd->stat.next = NULL;

    return cmd;
}

Cmd *attcmd(Exps *att) {
    Cmd *cmd = tryalloc(sizeof(Cmd));

    cmd->tag = ATTCMD;
    cmd->att.att = att;
    cmd->att.next = NULL;

    return cmd;
}

Cmd *newseqcmd(Cmd *c1, Cmd *c2) {
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
        case ATTCMD:
            c1->att.next = c2;
            break;
        default:
            fprintf(stderr, "unknown command type\n");
            exit(-1);
    }

    return c1;
}

Exps *unaryexp(Exp_type tag, Exps *e1) {
    Exps *exp = tryalloc(sizeof(Exps));

    exp->tag = tag;
    exp->unary.exp = e1;
    exp->unary.next = NULL;

    return exp;
}

Exps *binaryexp(Exp_type tag, Exps *e1, Exps *e2) {
    Exps *exp = tryalloc(sizeof(Exps));

    exp->tag = tag;
    exp->binary.e1 = e1;
    exp->binary.e2 = e2;
    exp->binary.next = NULL;

    return exp;
}

Exps *asexp(Exps *e1, Type *type) {
    Exps *exp = tryalloc(sizeof(Exps));

    exp->tag = AS;
    exp->as.type = type;
    exp->as.exp = e1;
    exp->as.next = NULL;

    return exp;
}

Exps *callexp(char *name, Exps *e1) {
    Exps *exp = tryalloc(sizeof(Exps));

    exp->tag = CALLEXP;
    exp->call.name = name;
    exp->call.listexp = e1;
    exp->call.next = NULL;

    return exp;
}

Exps *newexp(Type *type, Exps *e1) {
    Exps *exp = tryalloc(sizeof(Exps));

    exp->tag = NEW;
    exp->new.type = type;
    exp->new.exp = e1;
    exp->new.next = NULL;

    return exp;
}

Exps *newvarid(char *name) {
    Exps *exp = tryalloc(sizeof(Exps));

    exp->tag = VARID;
    exp->var.name = name;
    exp->var.next = NULL;

    return exp;
}

Exps *listexp(Exps *e1, Exps *e2) {
    switch(e1->tag) {
        case VARID:
            e1->var.next = e2;
            break;
        case VAR:
            e1->binary.next = e2;
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

Exps *newint(int i) {
    Exps *exp = tryalloc(sizeof(Exps));

    exp->tag = EXPINT;
    exp->expint.i = i;
    exp->expint.type = newtype(INT);
    exp->expint.next = NULL;

    return exp;
}

Exps *newfloat(double d) {
    Exps *exp = tryalloc(sizeof(Exps));

    exp->tag = EXPFLOAT;
    exp->expfloat.d = d;
    exp->expfloat.type = newtype(FLOAT);
    exp->expfloat.next = NULL;

    return exp;
}

Exps *newchar(char c) {
    Exps *exp = tryalloc(sizeof(Exps));

    exp->tag = EXPCHAR;
    exp->expchar.c = c;
    exp->expchar.type = newtype(CHAR);
    exp->expchar.next = NULL;

    return exp;
}

Exps *newstr(char *s) {
    Exps *exp = tryalloc(sizeof(Exps));

    exp->tag = EXPSTR;
    exp->expstr.str = s;
    exp->expstr.type = newseqtype(newtype(CHAR));
    exp->expstr.next = NULL;

    return exp;
}

Exps *newbool(bool b) {
    Exps *exp = tryalloc(sizeof(Exps));

    exp->tag = EXPBOOL;
    exp->expbool.b = b;
    exp->expbool.type = newtype(BOOL);
    exp->expbool.next = NULL;

    return exp;
}
