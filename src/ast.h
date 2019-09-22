#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

enum native_types {
	INT,
	CHAR,
	FLOAT,
	BOOL
};

enum exp_type {
	VAR,
	CALLEXP,
	AS,
	NEW,
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
	enum types tag;
	struct {
		enum types tag;
		enum native_types type;
	} single;
	struct {
		enum types tag;
		union type *next;
	} seq;
};

union exps {
	enum exp_type tag;
	struct {
		enum exp_type tag;
		char *name;
		union exps *next;
	} var;
	struct {
		enum exp_type tag;
		char *name;
		union exps *exp;
		union exps *next;
	} call;
	struct {
		enum exp_type tag;
		union exps *exp;
		union type *type;
		union exps *next;
	} as;
	struct {
		enum exp_type tag;
		union type *type;
		union exps *exp;
		union exps *next;
	} new;
	struct {
		enum exp_type tag;
		union exps *e1;
		union exps *e2;
		union exps *next;
	} binary;
	struct {
		enum exp_type tag;
		union exps *exp;
		union exps *next;
	} unary;
	struct {
		enum exp_type tag;
		int i;
		union type *type;
		union exps *next;
	} expint;
	struct {
		enum exp_type tag;
		double d;
		union type *type;
		union exps *next;
	} expfloat;
	struct {
		enum exp_type tag;
		char c;
		union type *type;
		union exps *next;
	} expchar;
	struct {
		enum exp_type tag;
		char *str;
		union type *type;
		union exps *next;
	} expstr;
	struct {
		enum exp_type tag;
		bool b;
		union type *type;
		union exps *next;
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
	ATT,
	STAT
};

struct var {
	char *name;
	union type *type;
	struct var *next;
};

struct param {
	char *name;
	union type *type;
	struct param *next;
};

struct func {
	char *name;
	struct param *param;
	union type *type;
	struct stat *stat;
	struct func *next;
};

struct prog {
	struct func *funcs;
	struct var *vars;
};

union cmd {
	enum cmd_type tag;
	struct {
		enum cmd_type tag;
		union exps *exp;
		struct stat *stat;
		union cmd *next;
	} cmd_if;
	struct {
		enum cmd_type tag;
		union exps *exp;
		struct stat *stat;
		union cmd *next;
	} cmd_while;
	struct {
		enum cmd_type tag;
		union exps *exp;
		union cmd *next;
	} cmd_ret_exp;
	struct {
		enum cmd_type tag;
		union cmd *next;
	} cmd_ret;
	struct {
		enum cmd_type tag;
		union exps *exp;
		struct stat *stat;
		struct stat *stat2;
		union cmd *next;
	} cmd_ifelse;
	struct {
		enum cmd_type tag;
		union exps *exp;
		union cmd *next;
	} print;
	struct {
		enum cmd_type tag;
		union exps *call;
		union cmd *next;
	} call;
	struct {
		enum cmd_type tag;
		struct stat *stat;
		union cmd *next;
	} stat;
	struct {
		enum cmd_type tag;
		union exps *att;
		union cmd *next;
	} att;
};

struct stat {
	struct var *vars;
	union cmd *cmds;
};

extern struct prog *GLOBAL_TREE;

extern struct var *vardef(char *name, union type *type);
extern struct var *varseqdef(struct var *v1, struct var *v2);
extern struct func *func(char *name, struct param *params, union type * type,
						struct stat *stat);
extern struct func *funcseq(struct func *f1, struct func *f2);
extern union type *newtype(enum native_types ntype);
extern union type *newseqtype(union type *t1);
extern struct param *newparamseq(struct param *p1, struct param *p2);
extern struct param *newparam(char *name, union type *type);
extern struct stat *newstat(struct var *var, union cmd *cmd);
extern union cmd *newcmd(enum cmd_type tag, union exps *exp, struct stat *stat, struct stat *stat2);
extern union cmd *newseqcmd(union cmd *c1, union cmd *c2);
extern union cmd *callcmd(union exps *call);
extern union cmd *attcmd(union exps *att);
extern union cmd *statcmd(struct stat *stat);
extern union exps *unaryexp(enum exp_type tag, union exps *e1);
extern union exps *binaryexp(enum exp_type tag, union exps *e1, union exps *e2);
extern union exps *callexp(char *name, union exps *e1);
extern union exps *newexp(union type *type, union exps *e1);
extern union exps *newvarid(char *name);
extern union exps *listexp(union exps *e1, union exps *e2);
extern struct prog *prognode(struct var *vars, struct func *funcs);
extern union exps *newint(int i);
extern union exps *newfloat(double d);
extern union exps *newchar(char c);
extern union exps *newstr(char *s);
extern union exps *newbool(bool b);
extern union exps *asexp(union exps *e1, union type *type);
