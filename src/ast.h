//Victor Nogueira - 1511043 & Francisco Thiesen - 1611854


#ifndef __AST
#define __AST
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>


typedef struct var Var;
typedef struct func Func;
typedef struct stat_monga Stat;
typedef union cmd Cmd;
typedef union type Type;
typedef union exps Exps;
typedef union def Def;
typedef enum native_types Native_types;
typedef enum def_types Def_types;
typedef enum exp_type Exp_type;
typedef enum types Types;
typedef enum cmd_type Cmd_type;

enum native_types {
    INT,
    CHAR,
    FLOAT,
    BOOL
};

enum def_types {
    DEFVAR,
    DEFFUNC
};

enum exp_type {
    VAR,
    VARID,
    CALLEXP,
    AS,
    NEW,
    EXPATT,
    MINUS,
    SUM,
    SUB,
    MUL,
    DIV,
    EQ,
    NEQ,
    LEQ,
    GEQ,
    L,
    G,
    NOT,
    AND,
    OR,
    EXPINT,
    EXPFLOAT,
    EXPCHAR,
    EXPSTR,
    EXPBOOL,
};

enum types {
    SINGLE,
    SEQ
};

union type {
    Types tag;
    struct {
        Types tag;
        Native_types type;
    } single;
    struct {
        Types tag;
        Type *next;
    } seq;
};

union exps {
    Exp_type tag;
    struct {
        Exp_type tag;
        char *name;
        Var *def;
        Exps *next;
    } var;
    struct {
        Exp_type tag;
        char *name;
        Func *funcdef;
        Exps *listexp;
        Exps *next;
    } call;
    struct {
        Exp_type tag;
        Exps *exp;
        Type *type;
        Exps *next;
    } as;
    struct {
        Exp_type tag;
        Type *type;
        Exps *exp;
        Exps *next;
    } new;
    struct {
        Exp_type tag;
        Exps *e1;
        Exps *e2;
        Exps *next;
    } binary;
    struct {
        Exp_type tag;
        Exps *exp;
        Exps *next;
    } unary;
    struct {
        Exp_type tag;
        int i;
        Type *type;
        Exps *next;
    } expint;
    struct {
        Exp_type tag;
        double d;
        Type *type;
        Exps *next;
    } expfloat;
    struct {
        Exp_type tag;
        char c;
        Type *type;
        Exps *next;
    } expchar;
    struct {
        Exp_type tag;
        char *str;
        Type *type;
        Exps *next;
    } expstr;
    struct {
        Exp_type tag;
        bool b;
        Type *type;
        Exps *next;
    } expbool;
};

enum cmd_type {
    IF,
    IFELSE,
    RET,
    RETEXP,
    WHILE,
    PRINT,
    CALLCMD,
    ATTCMD,
    STAT
};

struct var {
    char *name;
    Type *type;
    Var *next;
};

struct func {
    char *name;
    Var *param;
    Type *type;
    Stat *stat;
    Func *next;
};

union def {
    Def_types tag;
    struct {
        Def_types tag;
        Var *vars; 
        Def *next;
    } vars;
    struct {
        Def_types tag;
        Func *funcs;
        Def *next;
    } funcs;
};

union cmd {
    enum cmd_type tag;
    struct {
        Cmd_type tag;
        Exps *exp;
        Stat *stat;
        Cmd *next;
    } cmd_if;
    struct {
        Cmd_type tag;
        Exps *exp;
        Stat *stat;
        Cmd *next;
    } cmd_while;
    struct {
        Cmd_type tag;
        Exps *exp;
        Cmd *next;
    } cmd_ret_exp;
    struct {
        Cmd_type tag;
        Cmd *next;
    } cmd_ret;
    struct {
        Cmd_type tag;
        Exps *exp;
        Stat *stat;
        Stat *stat2;
        Cmd *next;
    } cmd_ifelse;
    struct {
        Cmd_type tag;
        Exps *exp;
        Cmd *next;
    } print;
    struct {
        Cmd_type tag;
        Exps *call;
        Cmd *next;
    } call;
    struct {
        Cmd_type tag;
        Stat *stat;
        Cmd *next;
    } stat;
    struct {
        Cmd_type tag;
        Exps *att;
        Cmd *next;
    } att;
};

struct stat_monga {
    Var *vars;
    Cmd *cmds;
};

extern Def *GLOBAL_TREE;
extern Def *def(Def_types type, Var *var, Func *func);
extern Def *defseq(Def *delem, Def *dlist);
extern Var *vardef(char *name, Type *type);
extern Var *varseqdef(Var *v1, Var *v2);
extern Func *func(char *name, Var *params, Type * type,
        Stat *stat);
extern Func *funcseq(Func *f1, Func *f2);
extern Type *newtype(Native_types ntype);
extern Type *newseqtype(Type *t1);
extern Var *newparamseq(Var *p1, Var *p2);
extern Var *newparam(char *name, Type *type);
extern Stat *newstat(Var *var, Cmd *cmd);
extern Cmd *newcmd(Cmd_type tag, Exps *exp, Stat *stat, Stat *stat2);
extern Cmd *newseqcmd(Cmd *c1, Cmd *c2);
extern Cmd *callcmd(Exps *call);
extern Cmd *attcmd(Exps *att);
extern Cmd *statcmd(Stat *stat);
extern Exps *unaryexp(Exp_type tag, Exps *e1);
extern Exps *binaryexp(Exp_type tag, Exps *e1, Exps *e2);
extern Exps *callexp(char *name, Exps *e1);
extern Exps *newexp(Type *type, Exps *e1);
extern Exps *newvarid(char *name);
extern Exps *listexp(Exps *e1, Exps *e2);
extern Exps *newint(int i);
extern Exps *newfloat(double d);
extern Exps *newchar(char c);
extern Exps *newstr(char *s);
extern Exps *newbool(bool b);
extern Exps *asexp(Exps *e1, Type *type);
#endif
