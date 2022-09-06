#ifndef PARSE_HELPERS_H
#define PARSE_HELPERS_H 1

#include "tk.h"
#include "interm.h"
#include "err.h"
#include "textattrib.h"
#include "lex.h"

static
tk_t* consume(parse_ctx_t* cx) {
	LT_ASSERT(cx->lex->it < cx->lex->count);
	return &cx->lex->tk_data[cx->lex->it++];
}

static
tk_t* consume_type(parse_ctx_t* cx, tk_stype_t stype, lstr_t err) {
	tk_t* tk = &cx->lex->tk_data[cx->lex->it];

	if (cx->lex->it >= cx->lex->count)
		ferr("unexpected end of file%S", cx->lex->tk_data[cx->lex->it - 1], err);
	if (tk->stype != stype)
		ferr("unexpected token "A_BOLD"'%S'"A_RESET"%S", *tk, tk->str, err);

	++cx->lex->it;
	return tk;
}

static
tk_t* peek(parse_ctx_t* cx, usz offs) {
	offs += cx->lex->it;
	if (offs >= cx->lex->count) {
		tk_t* eof = lt_arena_reserve(cx->arena, sizeof(tk_t));
		*eof = TK(cx->lex, TK_EOF, CLSTR("EOF"), 0);
		return eof;
	}
	return &cx->lex->tk_data[offs];
}

#endif
