//Victor Nogueira - 1511043 & Francisco Thiesen - 1611854
#ifndef __PRINTTREE
#define __PRINTTREE
#include "ast.h"

typedef struct state State;

struct state {
    Type *cur_func_type;
    char *cur_func_name;
};

extern void print_tree();
extern State * global_state;

#endif