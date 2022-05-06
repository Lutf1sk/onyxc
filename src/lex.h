#ifndef LEX_H
#define LEX_H 1

#include "tk.h"

#include <lt/fwd.h>

typedef struct lex_ctx {
	lstr_t data;
	char* path;

	tk_t* tk_data;
	usz count;
	usz it;
} lex_ctx_t;

lex_ctx_t* lex_file(lt_arena_t* arena, char* path, tk_t* path_tk);

typedef usz (*lex_pfn)(lex_ctx_t* cx, tk_t* out_tk);

extern lex_pfn lex;

#endif
