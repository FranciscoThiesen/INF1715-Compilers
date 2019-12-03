//Victor Nogueira - 1511043 & Francisco Thiesen - 1611854
#ifndef __PRINTTREE
#define __PRINTTREE
#include "ast.h"

typedef struct state State;

typedef struct string_info {
    char *str;
    int len;
} String_info;

struct state {
    Type *cur_func_type;
    char *cur_func_name;
    int cur_line;
    int temp_count;
    int label_count;
    int num_strs;
    String_info *strinfos;
};

extern bool type_tree();
extern int compare_type(Type *t1, Type *t2);
bool is_float(Type *t);
bool is_char(Type *t);
bool is_bool(Type *t);
bool is_array(Type *t);
bool is_void(Type *t);
extern State * global_state;

#endif
