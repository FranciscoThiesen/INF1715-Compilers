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
typedef union exp Exp;
typedef struct exp_list Exp_list;
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

struct exp_list {
    Exp *exp;
    Exp_list *next;
    int line;
};

union exp {
    Exp_type tag;
    struct {
        Exp_type tag;
        char *name;
        Var *def;
    } var;
    struct {
        Exp_type tag;
        char *name;
        Func *funcdef;
        Exp_list *explist;
        int line;
    } call;
    struct {
        Exp_type tag;
        Exp *exp;
        Type *type;
        int line;
    } as;
    struct {
        Exp_type tag;
        Type *type;
        Exp *exp;
        int line;
    } new;
    struct {
        Exp_type tag;
        Exp *e1;
        Exp *e2;
        int line;
    } binary;
    struct {
        Exp_type tag;
        Exp *exp;
        int line;
    } unary;
    struct {
        Exp_type tag;
        int i;
        Type *type;
    } expint;
    struct {
        Exp_type tag;
        double d;
        Type *type;
    } expfloat;
    struct {
        Exp_type tag;
        char c;
        Type *type;
    } expchar;
    struct {
        Exp_type tag;
        char *str;
        Type *type;
    } expstr;
    struct {
        Exp_type tag;
        bool b;
        Type *type;
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
        Exp *exp;
        Stat *stat;
        Cmd *next;
        int line;
    } cmd_if;
    struct {
        Cmd_type tag;
        Exp *exp;
        Stat *stat;
        Cmd *next;
        int line;
    } cmd_while;
    struct {
        Cmd_type tag;
        Exp *exp;
        Cmd *next;
        int line;
    } cmd_ret_exp;
    struct {
        Cmd_type tag;
        Cmd *next;
        int line;
    } cmd_ret;
    struct {
        Cmd_type tag;
        Exp *exp;
        Stat *stat;
        Stat *stat2;
        Cmd *next;
        int line;
    } cmd_ifelse;
    struct {
        Cmd_type tag;
        Exp *exp;
        Cmd *next;
        int line;
    } print;
    struct {
        Cmd_type tag;
        Exp *call;
        Cmd *next;
        int line;
    } call;
    struct {
        Cmd_type tag;
        Stat *stat;
        Cmd *next;
    } stat;
    struct {
        Cmd_type tag;
        Exp *att;
        Cmd *next;
        int line;
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
extern Cmd *newcmd(Cmd_type tag, int line, Exp *exp, Stat *stat, Stat *stat2);
extern Cmd *newseqcmd(Cmd *c1, Cmd *c2);
extern Cmd *callcmd(Exp *call, int line);
extern Cmd *attcmd(Exp *att, int line);
extern Cmd *statcmd(Stat *stat);
extern Exp *unaryexp(Exp_type tag, int line, Exp *e1);
extern Exp *binaryexp(Exp_type tag, int line, Exp *e1, Exp *e2);
extern Exp *callexp(char *name, int line, Exp_list *e1);
extern Exp *newexp(Type *type, int line, Exp *e1);
extern Exp *newvarid(char *name);
extern Exp_list *listexp(Exp *e1, Exp_list *e2);
extern Exp *newint(int i);
extern Exp *newfloat(double d);
extern Exp *newchar(char c);
extern Exp *newstr(char *s);
extern Exp *newbool(bool b);
extern Exp *asexp(Exp *e1, int line, Type *type);
#endif
