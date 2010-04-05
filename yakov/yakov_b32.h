/* $Id$ */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

void yakov_b32_encode_block(char *, char *);
void yakov_b32_decode_block(u_int8_t *, u_int8_t *);
char *yakov_b32_encode(char *, int, char *, int);
char *yakov_b32_decode(char *, int, char *, int, int *);
