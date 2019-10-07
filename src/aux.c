#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include "aux.h"

void *tryalloc(size_t size) {
	void *newptr = malloc(sizeof(char) * size);

	if (!newptr) {
		fprintf(stderr, "error: %s\n", strerror(ENOMEM));
		exit(-1);
	}

	return newptr;
}
