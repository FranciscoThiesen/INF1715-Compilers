#ifndef __SEMANTIC_H
#define __SEMANTIC_H
#include "ast.h"

extern bool insert_var(Var *var);
extern bool insert_func(Func *func);
extern void init_symbols();
extern void enter_scope();
extern void leave_scope();
extern void clean_symbols();
extern Var *get_var(char *id, bool *error);
extern Func *get_func(char *id, bool *error);
extern Type *get_exp_type(Exp *exp);
#endif
