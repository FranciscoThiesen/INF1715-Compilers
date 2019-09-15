#include <stdio.h>
#include <stdlib.h>

typedef union {
   double d;
   int i;
   char *s;
} SemValue;

enum native_types {
	INT,
	CHAR,
	FLOAT,
	BOOL
};

enum exp_type {
	VAR,
	BRACES,
	CALL,
	AS,
	NEW,
	MINUS,
	SUM,
	SUB,
	MUL,
	DIV,
	EQ,
	LE,
	GE,
	L,
	G,
	NOT,
	AND,
	OR
};

enum types {
	ATOMIC,
	SEQ
};

enum var_type {
	EXP,
	ID
};

struct type {
	enum native_types native_type;
	struct type *typer;
};

union exps {
	enum exp_type tag;
	struct {
		struct {
			enum var_type vartag;
			union {
				SemValue name;
				struct {
					union exps *exp1;
					union exps *exp2;
				};
			} name_exps;
		} value;
	} var ;
	struct {
		enum exp_type tag;
		union exps *exp;
	} braces;
	struct {
		enum exp_type tag;
		SemValue id;	
		union exps *exp;
	} call;
	struct {
		enum exp_type tag;
		union exps *exp;
		struct type *type;
	} as;
	struct {
		enum exp_type tag;
		struct type type;	
		union exps *exp;
	} new;
	struct {
		enum exp_type tag;
		union exps *expl;
		union exps *expr;
	} binary;
	struct {
		enum exp_type tag;
		union exps *exp;
	} unary;
};

enum cmd_type {
	IF,
	IFELSE,
	RET,
	RET_EXP,
	WHILE,
	PRINT
};

enum stat_type {
	DEFVARS,
	DEFFUNC
};

struct {
	SemValue id;
	struct type *type;
} def_var;

struct {
	struct def_var *var;
	struct def_vars *vars;	
} def_vars;

union {
	enum stat_type tag;	
	struct {
	};
} stat;

union cmd {
	enum cmd_type tag;	
	struct {
		enum cmd_type tag;	
		union exps *exp;
		union stat *stat;	
	} cmd_if;
	struct {
		enum cmd_type tag;	
		union exps *exp;
		union stat *stat;	
	} cmd_while;
	struct {
		enum cmd_type tag;	
		union exps *exp;
	} cmd_ret_exp;
	struct {
		enum cmd_type tag;	
	} cmd_ret;
	struct {
		enum cmd_type tag;	
		union exps *exp;
	} print;
};
