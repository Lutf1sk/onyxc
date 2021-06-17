// Copyright (C) 2021, Alex Edin <lutfisk@lutfisk.net>
// SPDX-License-Identifier: GPL-2.0+

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#include "scan.h"
#include "err.h"
#include "tk.h"

static inline INLINE
int is_digit(char c) {
    return c >= '0' && c <= '9';
}

static inline INLINE
int is_alpha(char c) {
    return (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z');
}

static inline INLINE FLATTEN
int is_ident_start(char c) {
    return is_alpha(c) || c == '_';
}

static inline INLINE FLATTEN
int is_ident_body(char c) {
    return is_ident_start(c) || is_digit(c);
}

static inline INLINE
int is_whitespace(char c) {
    return c == ' ' || c == '\t' || c == '\n' || c == '\r';
}

static inline INLINE
TokenType keyword(const char* str, usz len) {
    if (!len)
        return TK_INVALID;

    switch (*str) {
    case 'r':
        if (strncmp(str, "return", len) == 0) return TK_KW_RETURN;
        break;

    case 's':
        if (strncmp(str, "struct", len) == 0) return TK_KW_STRUCT;
        break;

    case 'l':
        if (strncmp(str, "let", len) == 0) return TK_KW_LET;
        break;
    }

    return TK_INVALID;
}

static inline INLINE
char peek(ScanCtx* cx, isz offs) {
    return cx->char_data[cx->char_it + offs];
}

static inline INLINE
char consume(ScanCtx* cx) {
    char c = cx->char_data[cx->char_it++];
    if (c == '\n')
        ++cx->line_index;
    return c;
}

static inline INLINE
void emit(ScanCtx* cx, TokenType type, usz tk_start) {
    usz tk_count = cx->token_count;
    usz avail_count = cx->token_avail_count;
    Token* tk_data = cx->token_data;

    if (tk_count >= avail_count) {
        // Double the size and reallocate the array
        // OR if the array has not been allocated yet, allocate space for 1024 tokens.
        if (avail_count)
            avail_count *= 2;
        else
            avail_count = 1024;

        tk_data = realloc(tk_data, avail_count * sizeof(Token));
        assert(tk_data);
    }

    tk_data[cx->token_count++] = (Token) { type, tk_start, cx->char_it - tk_start, cx->line_index };
    cx->token_data = tk_data;
    cx->token_avail_count = avail_count;
}

void scan(ScanCtx* cx) {
    for (;;) {
        // Skip any whitespace
        while (is_whitespace(peek(cx, 0)))
            consume(cx);

        // Break when the current char is the null-terminator
        char c = peek(cx, 0);
        if (!c)
            break;

        // Parse token
        usz tk_start = cx->char_it;

        switch (c) {
        case '+': consume(cx); emit(cx, TK_PLUS, tk_start); break;
        case '-': consume(cx); emit(cx, TK_MINUS, tk_start); break;
        case '*': consume(cx); emit(cx, TK_ASTERISK, tk_start); break;
        case '/': consume(cx); emit(cx, TK_SLASH, tk_start); break;

        case '=': consume(cx); emit(cx, TK_EQUAL, tk_start); break;

        case ':': consume(cx);
            if (peek(cx, 0) == ':') { consume(cx); emit(cx, TK_DOUBLE_COLON, tk_start); }
            else emit(cx, TK_COLON, tk_start);
            break;

        case ';': consume(cx); emit(cx, TK_SEMICOLON, tk_start); break;
        case ',': consume(cx); emit(cx, TK_COMMA, tk_start); break;
        case '.': consume(cx); emit(cx, TK_DOT, tk_start); break;

        case '(': consume(cx); emit(cx, TK_LEFT_PARENTH, tk_start); break;
        case ')': consume(cx); emit(cx, TK_RIGHT_PARENTH, tk_start); break;
        case '{': consume(cx); emit(cx, TK_LEFT_BRACE, tk_start); break;
        case '}': consume(cx); emit(cx, TK_RIGHT_BRACE, tk_start); break;
        case '[': consume(cx); emit(cx, TK_LEFT_BRACKET, tk_start); break;
        case ']': consume(cx); emit(cx, TK_RIGHT_BRACKET, tk_start); break;

        case '"': {
            consume(cx);

            usz start_line_index = cx->line_index;
            for (;;) {
                char sc = peek(cx, 0);
                if (!sc)
                    err("%s:%zu: String not terminated", cx->file_path, start_line_index + 1);

                consume(cx);

                if (sc == '"' && peek(cx, -2) != '\\')
                    break;
            }

            emit(cx, TK_STRING, tk_start);
        }   break;

        case '\'': {
            consume(cx);

            usz start_line_index = cx->line_index;
            for (;;) {
                char sc = peek(cx, 0);
                if (!sc)
                    err("%s:%zu: Character string not terminated", cx->file_path, start_line_index + 1);

                consume(cx);

                if (sc == '\'' && peek(cx, -2) != '\\')
                    break;
            }

            emit(cx, TK_CHARACTER, tk_start);
        }   break;

        default:
            if (is_ident_start(c)) {
                while (is_ident_body(peek(cx, 0)))
                    consume(cx);

                TokenType kw = keyword(&cx->char_data[tk_start], cx->char_it - tk_start);
                if (kw != TK_INVALID)
                    emit(cx, kw, tk_start);
                else
                    emit(cx, TK_IDENTIFIER, tk_start);
                break;
            }

            if (is_digit(c)) {
                while (is_digit(peek(cx, 0)))
                    consume(cx);
                emit(cx, TK_INTEGER, tk_start);
                break;
            }

            err("%s:%zu: Unexpected characted '%c'", cx->file_path, cx->line_index + 1, c);
            break;
        }
    }
}
