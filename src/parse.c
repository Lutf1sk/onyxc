// Copyright (C) 2021, Alex Edin <lutfisk@lutfisk.net>
// SPDX-License-Identifier: GPL-2.0+

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "parse.h"
#include "tk.h"
#include "ast.h"
#include "err.h"
#include "type.h"
#include "sym.h"
#include "intermediate.h"
#include "parse_helpers.h"

void parse_compound(ParseCtx* cx) {
    consume_type(cx, TK_LEFT_BRACE);

    Symbols sym_tab = make_sym_tab();
    sym_tab.next = cx->syms;
    cx->syms = &sym_tab;

    for (;;) {
        Token tk = *peek(cx, 0);

        if (tk.type == TK_INVALID)
            err("%s: Expected closing '}' before end of file", cx->file_path);
        if (tk.type == TK_RIGHT_BRACE) {
            consume(cx);
            break;
        }

        parse_stmt(cx);
    }

    cx->syms = sym_tab.next;
    free_sym_tab(&sym_tab);
}

static
void parse_var_def(ParseCtx* cx, TypeHandle type_hnd) {
    const Token* name_tk = consume_type(cx, TK_IDENTIFIER);

    isz last_func = cx->curr_func;

    b8 is_glob = cx->curr_func == -1;
    b8 is_const = peek(cx, 0)->type == TK_DOUBLE_COLON;

    if (is_const)
        consume_type(cx, TK_DOUBLE_COLON);
    else
        consume_type(cx, TK_EQUAL);

    if (is_glob || is_const) {
        IntermediateFunc glob_init_func = make_intermediate_func();
        cx->curr_func = add_intermediate_func(cx, &glob_init_func);
    }

    Expression expr = parse_expr(cx);
    isz reg = gen_expr(cx, &expr);
    free_expr_children(&expr);

    if (!type_handle_valid(type_hnd))
        type_hnd = expr.datatype;

    LenStr name = LSTR(&cx->char_data[name_tk->start], name_tk->len);

    SymbolType sym_type = is_const ? SM_CONSTANT : (is_glob ? SM_GLOBAL_VAR : SM_LOCAL_VAR);
    usz offs = add_symbol(cx->syms, sym_type, name, type_hnd);
    cx->syms->syms[offs].reg = reg;

    if (is_glob || is_const) {
        cx->syms->syms[offs].init_func = cx->curr_func;
        cx->curr_func = last_func;
    }
}

void parse_stmt(ParseCtx* cx) {
    Token tk = *peek(cx, 0);

    switch (tk.type) {
    case TK_KW_LET: {
        consume(cx);
        parse_var_def(cx, INVALID_TYPE);
    }   break;

    case TK_KW_RETURN: {
        consume(cx);
        if (cx->curr_func == -1)
            err("%s:%zu: Cannot return from global scope",
                cx->file_path, tk.line_index + 1, tk.len);

        if (peek(cx, 0)->type != TK_SEMICOLON) {
            Expression expr = parse_expr(cx);
            usz reg = gen_expr(cx, &expr);
            free_expr_children(&expr);
            emit(cx, make_instr(make_instr_op(IN_STORE_RETVAL, ISZ_64, 0), reg));
        }

        emit(cx, make_instr(make_instr_op(IN_RET, ISZ_64, 0), 0));

        consume_type(cx, TK_SEMICOLON);
    }   break;

    case TK_SEMICOLON: {
        consume(cx);
    }   break;

    case TK_LEFT_BRACE: {
        if (cx->curr_func == -1)
            err("%s:%zu: Nested global scopes are not supported",
                cx->file_path, tk.line_index + 1, tk.len);

        return parse_compound(cx);
    }   break;

    default: {
        TypeHandle type_hnd;
        if (parse_type(cx, &type_hnd)) {
            parse_var_def(cx, type_hnd);
            break;
        }

        if (cx->curr_func == -1)
            err("%s:%zu: Unexpected token '%.*s'",
                cx->file_path, tk.line_index + 1, tk.len, &cx->char_data[tk.start]);

        Expression expr = parse_expr(cx);
        gen_expr(cx, &expr);
        free_expr_children(&expr);
    }   break;
    }
}

b8 parse_type(ParseCtx* cx, TypeHandle* ret_hnd) {
    Token tk = *peek(cx, 0);

    switch (tk.type) {
    case TK_IDENTIFIER: {
        LenStr name = LSTR(&cx->char_data[tk.start], tk.len);
        TypeHandle type_hnd = find_type(cx->syms, name);
        if (!type_handle_valid(type_hnd))
            return 0;

        consume(cx);
        if (ret_hnd)
            *ret_hnd = type_hnd;
        return 1;
    }   break;

    default:
        return 0;
    }
}

void parse(ParseCtx* cx) {
    while (cx->tk_it < cx->tk_len)
        parse_stmt(cx);
}
