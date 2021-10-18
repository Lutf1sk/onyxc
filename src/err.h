#ifndef ERR_H
#define ERR_H 1

#include "fwd.h"
#include "tk.h"

#include <lt/lt.h>

LT_NORETURN
void ferr(char* fmt, lex_ctx_t* lex_c, tk_t tk, ...);

void werr(char* fmt, lex_ctx_t* lex_c, tk_t tk, ...);

#endif
