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

Func *func(char *name, Var *params, Type * type,
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

Var *newparamseq(Var *p1, Var *p2) {
    p1->next = p2;

    return p1;
}

Var *newparam(char *name, Type *type) {
    Var *param = tryalloc(sizeof(Var));

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

Exp *newvarid(char *name) {
    Exp *exp = tryalloc(sizeof(Exp));

    exp->tag = VARID;
    exp->var.name = name;

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

Exp *newfloat(double d) {
    Exp *exp = tryalloc(sizeof(Exp));

    exp->tag = EXPFLOAT;
    exp->expfloat.d = d;
    exp->expfloat.type = newtype(FLOAT);

    return exp;
}

Exp *newchar(char c) {
    Exp *exp = tryalloc(sizeof(Exp));

    exp->tag = EXPCHAR;
    exp->expchar.c = c;
    exp->expchar.type = newtype(CHAR);

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
