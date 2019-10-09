#ifndef __SEMANTIC_H
#define __SEMANTIC_H
#include "ast.h"

extern void insert_var(Var *var);
extern void insert_func(Func *func);
extern void init_symbols();
extern void enter_scope();
extern void leave_scope();
extern void clean_symbols();
extern Var *get_var(char *id);
extern Func *get_func(char *id);
extern Type *get_exp_type(Exp *exp);
#endif
