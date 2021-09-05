#ifndef LEX_H
#define LEX_H 1

#include "tk.h"

#include <lt/fwd.h>

typedef struct lex_ctx {
	lstr_t data;
	char* path;
} lex_ctx_t;

typedef usz (*lex_pfn)(lex_ctx_t* cx, tk_t* out_tk);

extern lex_pfn lex;

#endif
