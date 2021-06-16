#include <stdlib.h>
#include <assert.h>

#include "tk.h"
#include "parse.h"
#include "intermediate.h"
#include "err.h"

static inline INLINE
const Token* peek(ParseCtx* cx, usz offs) {
    static Token eof_tk = (Token) { TK_INVALID, 0, 0, 0 };

    if (cx->tk_it + offs >= cx->tk_len)
        return &eof_tk;

    return &cx->tk_data[cx->tk_it + offs];
}

static inline INLINE
const Token* consume(ParseCtx* cx) {
    if (cx->tk_it >= cx->tk_len)
        err("%s: Unexpected end of file", cx->file_path);

    return &cx->tk_data[cx->tk_it++];
}

static inline INLINE
const Token* consume_type(ParseCtx* cx, TokenType type) {
    if (cx->tk_it >= cx->tk_len)
        err("%s: Unexpected end of file", cx->file_path);

    const Token* tk = &cx->tk_data[cx->tk_it++];
    if (tk->type != type)
        err("%s:%zu: Expected %s, got '%.*s'", cx->file_path, tk->line_index + 1,  tk_type_str(type), tk->len, &cx->char_data[tk->start]);
    return tk;
}

static inline INLINE
void emit(ParseCtx* cx, Instr instr) {
    if (cx->curr_func == -1)
        err("%s: Code can only be generated inside a function", cx->file_path);
    add_instr(&cx->funcs[cx->curr_func], instr);
}

static inline INLINE
b8 curr_scope_global(ParseCtx* cx) {
    return cx->syms->next != NULL;
}

static inline INLINE
unsigned long tk_to_int(ParseCtx* cx, const Token* tk) {
    const char* c_it = &cx->char_data[tk->start + tk->len - 1];

    unsigned long mult = 1;
    unsigned long val = 0;

    for (usz i = 0; i < tk->len; ++i) {
        val += (*(c_it--) - '0') * mult;
        mult *= 10;
    }

    return val;
}

static inline INLINE
double tk_to_float(ParseCtx* cx, const Token* tk) {
    (void)cx;
    (void)tk;
    return 5.5f; // TODO: Fix this
}

static inline INLINE
usz alloc_register(ParseCtx* cx) {
    assert(cx->curr_func != -1);
    return cx->funcs[cx->curr_func].registers++;
}

static inline INLINE
void free_register(ParseCtx* cx, usz reg) {
    assert(cx->curr_func != -1);
    assert(reg + 1 == cx->funcs[cx->curr_func].registers);
    cx->funcs[cx->curr_func].registers--;
}

static
usz add_intermediate_func(ParseCtx* cx, const IntermediateFunc* func) {
    if (cx->func_count >= cx->func_alloc_count) {
        // Double the size and reallocate the array
        // OR if the array has not been allocated yet, allocate space for 1024 instructions.
        if (cx->func_alloc_count)
            cx->func_alloc_count *= 2;
        else
            cx->func_alloc_count = 1024;

        cx->funcs = realloc(cx->funcs, cx->func_alloc_count * sizeof(IntermediateFunc));
        assert(cx->funcs);
    }

    cx->funcs[cx->func_count] = *func;
    return cx->func_count++;
}
