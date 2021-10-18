#ifndef PARSE_HELPERS_H
#define PARSE_HELPERS_H 1

#include "tk.h"
#include "interm.h"
#include "err.h"
#include "textattrib.h"

static inline
tk_t* consume(parse_ctx_t* cx) {
	LT_ASSERT(cx->it < cx->count);
	return &cx->data[cx->it++];
}

static inline
usz line_count(parse_ctx_t* cx) {
	if (!cx->count)
		return 0;
	return cx->data[cx->count - 1].line_index;
}

static inline
tk_t* consume_type(parse_ctx_t* cx, tk_stype_t stype, lstr_t err) {
	tk_t* tk = &cx->data[cx->it];

	if (cx->it >= cx->count)
		ferr("unexpected end of file%S", cx->lex, cx->data[cx->it - 1], err);
	if (tk->stype != stype)
		ferr("unexpected token "A_BOLD"'%S'"A_RESET"%S", cx->lex, *tk, tk->str, err);

	++cx->it;
	return tk;
}

static LT_INLINE
tk_t* peek(parse_ctx_t* cx, usz offs) {
	static tk_t eof = TK_INIT(TK_EOF, CLSTR("EOF"), 0);

	offs += cx->it;
	if (offs >= cx->count)
		return &eof;
	return &cx->data[offs];
}

#endif
