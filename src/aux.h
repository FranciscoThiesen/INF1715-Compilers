//Victor Nogueira - 1511043 & Francisco Thiesen - 1611854

#ifndef _AUX_H
#define _AUX_H
#include <stdbool.h>
#include "ast.h"
extern int line;
extern void *tryalloc(size_t size);
extern bool is_int(Type  *t);
#endif
