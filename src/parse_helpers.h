#ifndef PARSE_HELPERS_H
#define PARSE_HELPERS_H 1

#include "tk.h"
#include "interm.h"

static inline
tk_t* consume(parse_ctx_t* cx) {
	if (cx->it >= cx->count)
		lt_ferrb(CLSTR("Attempted to read more data than available\n"));
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
		lt_ferrf("%s:%uz: Unexpected end of file%S", cx->path, line_count(cx) + 1, err);
	if (tk->stype != stype)
		lt_ferrf("%s:%uz: Unexpected '%S'%S", cx->path, tk->line_index + 1, tk->str, err);

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
