#ifndef ERR_H
#define ERR_H 1

#include "tk.h"

#include <lt/lt.h>

LT_NORETURN
void ferr(char* fmt, tk_t tk, ...);

void werr(char* fmt, tk_t tk, ...);

#endif
