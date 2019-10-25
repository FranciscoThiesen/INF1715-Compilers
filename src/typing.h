//Victor Nogueira - 1511043 & Francisco Thiesen - 1611854
#ifndef __PRINTTREE
#define __PRINTTREE
#include "ast.h"

typedef struct state State;

struct state {
    Type *cur_func_type;
    char *cur_func_name;
    int cur_line;
    int exp_count;
};

extern bool type_tree();
extern State * global_state;
extern Type *get_exp_type(Exp *exp);

#endif
