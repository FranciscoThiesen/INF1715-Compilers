#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include "aux.h"
#include "typing.h"

void *tryalloc(size_t size) {
    void *newptr = malloc(sizeof(char) * size);

    if (!newptr) {
        fprintf(stderr, "error: %s\n", strerror(ENOMEM));
        exit(-1);
    }

    return newptr;
}

bool is_int(Type  *t) {
    if (t == NULL)
        return 0;

    if(t->tag == SINGLE) return (t->single.type == INT);
    return is_int(t->seq.next);
}
