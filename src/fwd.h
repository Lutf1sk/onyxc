#ifndef FWD_H
#define FWD_H 1

typedef struct lex_ctx lex_ctx_t;

typedef struct expr expr_t;
typedef struct stmt stmt_t;
typedef struct type type_t;

typedef struct tk tk_t;

typedef struct parse_ctx parse_ctx_t;

typedef struct sympool sympool_t;
typedef struct symtab symtab_t;
typedef struct sym sym_t;

typedef struct icode icode_t;
typedef struct gen_ctx gen_ctx_t;

typedef struct seg_ent seg_ent_t;

typedef struct exec_ctx exec_ctx_t;

typedef struct amd64_ctx amd64_ctx_t;
typedef struct amd64_instr amd64_instr_t;
typedef struct amd64_ireg amd64_ireg_t;
typedef struct amd64_lbl amd64_lbl_t;

#endif
