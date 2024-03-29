//Victor Nogueira - 1511043 & Francisco Thiesen - 1611854

#include "ast.h"
#include "aux.h"
#include <string.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>

static Def *defvar(Var *var) {
    Def *newdef = tryalloc(sizeof(Def));
    newdef->tag = DEFVAR;
    newdef->var.def = var;
    newdef->var.next = NULL;
    return newdef;
}

static Def *deffunc(Func *func) {
    Def *newdef = tryalloc(sizeof(Def));
    newdef->tag = DEFFUNC;
    newdef->func.def = func;
    newdef->func.next = NULL;
    return newdef;
}

static Def *def(Def_types type, Var *var, Func *func) {
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
            delem->var.next= dlist;
            break;
        case DEFFUNC:
            delem->func.next = dlist;
            break;
        default:
            fprintf(stderr, "unknown type\n");    
            exit(-1);
    }
    return delem;
}

Def *vardef(char *name, Type *type, int line) {
    Var *var = tryalloc(sizeof(Var) );

    var->name = name;
    var->type = type;
    var->line = line;
    var->is_global = false;

    return def(DEFVAR, var, NULL);
}

Def *varseqdef(Def *v1, Def *v2) {
    Def *vaux;
    if (!v1) {
        v2->var.next = v1;
        return v2;
    }

    vaux = v1;
    while (vaux->var.next) {
        vaux = vaux->var.next;
    }

    vaux->var.next = v2;
    return v1;
}

Def *funcdef(char *name, Def *params, Type * type,
        Stat *stat, int line) {
    Func *func = tryalloc(sizeof(Func));

    func->name = name;
    func->param = params;
    func->type = type;
    func->stat = stat;
    func->line = line;

    return def(DEFFUNC, NULL, func);
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

Var *newparam(char *name, Type *type) {
    Var *param = tryalloc(sizeof(Var));

    param->name = name;
    param->type = type;

    return param;
}

Stat *newstat(Def *dvar, Cmd *cmd) {
    Stat *stat = tryalloc(sizeof(Stat));

    stat->vars = dvar;
    stat->cmds = cmd;

    return stat;
}

static Cmd *newcmd_if(Exp *exp, int line, Stat *stat) {
    Cmd *cmd = tryalloc(sizeof(Cmd));
    cmd->tag = IF;
    cmd->cmd_if.exp = exp;
    cmd->cmd_if.stat = stat;
    cmd->cmd_if.next = NULL;
    cmd->cmd_if.line = line;
    return cmd;
}

static Cmd *newcmd_ifelse(Exp *exp, int line, Stat *stat, Stat *stat2) {
    Cmd *cmd = tryalloc(sizeof(Cmd));
    cmd->tag = IFELSE;
    cmd->cmd_ifelse.exp = exp;
    cmd->cmd_ifelse.stat = stat;
    cmd->cmd_ifelse.stat2 = stat2;
    cmd->cmd_ifelse.next = NULL;
    cmd->cmd_ifelse.line = line;
    return cmd;
}

static Cmd *newcmd_while(Exp *exp, int line, Stat *stat) {
    Cmd *cmd = tryalloc(sizeof(Cmd));
    cmd->tag = WHILE;
    cmd->cmd_while.exp = exp;
    cmd->cmd_while.stat = stat;
    cmd->cmd_while.next = NULL;
    cmd->cmd_while.line= line;
    return cmd;
}

static Cmd *newcmd_retexp(Exp *exp, int line) {
    Cmd *cmd = tryalloc(sizeof(Cmd));
    cmd->tag = RETEXP;
    cmd->cmd_ret_exp.exp = exp;
    cmd->cmd_ret_exp.next = NULL;
    cmd->cmd_ret_exp.line = line;
    return cmd;
}

static Cmd *newcmd_ret(int line) {
    Cmd *cmd = tryalloc(sizeof(Cmd));
    cmd->tag = RET;
    cmd->cmd_ret.next = NULL;
    cmd->cmd_ret.line = line;
    return cmd;
}

static Cmd *newcmd_print(Exp *exp, int line) {
    Cmd *cmd = tryalloc(sizeof(Cmd));
    cmd->tag = PRINT;
    cmd->print.exp = exp;
    cmd->print.next = NULL;
    cmd->print.line = line;
    return cmd;
}

Cmd *newcmd(Cmd_type tag, int line, Exp *exp, Stat *stat, Stat *stat2) {
    switch (tag) {
        case IF:
            return newcmd_if(exp, line, stat);
        case IFELSE:
            return newcmd_ifelse(exp, line, stat, stat2);
        case WHILE:
            return newcmd_while(exp, line, stat);
        case RETEXP:
            return newcmd_retexp(exp, line);
        case RET:
            return newcmd_ret(line);
        case PRINT:
            return newcmd_print(exp, line);
        default:
            fprintf(stderr, "unknown command type\n");
            exit(-1);
    }
}

Cmd *callcmd(Exp *call, int line) {
    Cmd *cmd = tryalloc(sizeof(Cmd));

    cmd->tag = CALLCMD;
    cmd->call.call = call;
    cmd->call.next = NULL;
    cmd->call.line = line;

    return cmd;
}

Cmd *statcmd(Stat *stat) {
    Cmd *cmd = tryalloc(sizeof(Cmd));

    cmd->tag = STAT;
    cmd->stat.stat = stat;
    cmd->stat.next = NULL;

    return cmd;
}

Cmd *attcmd(Exp *att, int line) {
    Cmd *cmd = tryalloc(sizeof(Cmd));

    cmd->tag = ATTCMD;
    cmd->att.att = att;
    cmd->att.next = NULL;
    cmd->att.line = line;

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

Exp *unaryexp(Exp_type tag, int line, Exp *e1) {
    Exp *exp = tryalloc(sizeof(Exp));

    exp->tag = tag;
    exp->unary.exp = e1;
    exp->unary.line = line;

    return exp;
}

Exp *attexp(RefVar *v, int line, Exp *e) {
    Exp *exp = tryalloc(sizeof(Exp));

    exp->tag = EXPATT;
    exp->att.v = v;
    exp->att.e = e;
    exp->att.line = line;

    return exp;
}

Exp *binaryexp(Exp_type tag, int line, Exp *e1, Exp *e2) {
    Exp *exp = tryalloc(sizeof(Exp));

    exp->tag = tag;
    exp->binary.e1 = e1;
    exp->binary.e2 = e2;
    exp->binary.line = line;

    return exp;
}

Exp *asexp(Exp *e1, int line, Type *type) {
    Exp *exp = tryalloc(sizeof(Exp));

    exp->tag = AS;
    exp->as.type = type;
    exp->as.exp = e1;
    exp->as.line = line;

    return exp;
}

Exp *callexp(char *name, int line, Exp_list *e1) {
    Exp *exp = tryalloc(sizeof(Exp));

    exp->tag = CALLEXP;
    exp->call.name = name;
    exp->call.explist= e1;
    exp->call.line = line;

    return exp;
}

Exp *newexp(Type *type, int line, Exp *e1) {
    Exp *exp = tryalloc(sizeof(Exp));

    exp->tag = NEW;
    exp->new.type = newseqtype(type);
    exp->new.exp = e1;
    exp->new.line = line;

    return exp;
}

RefVar *newvarid(char *name) {
    RefVar *ref = tryalloc(sizeof(RefVar));
    Var *v = tryalloc(sizeof(Var));
    v->name = name;

    ref->tag = REF_VAR;
    ref->refv.v = v;

    return ref;
}

RefVar *newvararr(Exp *var, Exp *index, int line) {
    RefVar *ref = tryalloc(sizeof(RefVar));

    ref->tag = REF_ARRAY;
    ref->refa.v = var;
    ref->refa.idx = index;
    ref->refa.line = line;

    return ref;
}

Exp *varexp(RefVar *v) {
    Exp *exp;

    exp = tryalloc(sizeof(Exp));
    exp->tag = VAR;
    exp->var.def = v;

    return exp;
}

Exp_list *listexp(Exp *e1, Exp_list *elist) {
    Exp_list *ehead;

    ehead = tryalloc(sizeof(Exp_list));
    ehead->exp = e1;
    ehead->next = elist;

    return ehead;
}

Exp *newint(int i) {
    Exp *exp = tryalloc(sizeof(Exp));

    exp->tag = EXPINT;
    exp->expint.i = i;
    exp->expint.type = newtype(INT);

    return exp;
}

Exp *newfloat(float f) {
    Exp *exp = tryalloc(sizeof(Exp));

    exp->tag = EXPFLOAT;
    exp->expfloat.f = f;
    exp->expfloat.type = newtype(FLOAT);

    return exp;
}

Exp *newchar(char c) {
    Exp *exp = tryalloc(sizeof(Exp));

    exp->tag = EXPCHAR;
    exp->expint.i = c;
    exp->expint.type = newtype(CHAR);

    return exp;
}

Exp *newstr(char *s) {
    Exp *exp = tryalloc(sizeof(Exp));

    exp->tag = EXPSTR;
    exp->expstr.str = s;
    exp->expstr.type = newseqtype(newtype(CHAR));

    return exp;
}

Exp *newbool(bool b) {
    Exp *exp = tryalloc(sizeof(Exp));

    exp->tag = EXPBOOL;
    exp->expbool.b = b;
    exp->expbool.type = newtype(BOOL);

    return exp;
}
