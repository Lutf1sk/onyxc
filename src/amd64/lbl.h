#include "amd64.h"

amd64_lbl_t* new_lbl(amd64_ctx_t* cx, usz lbl_i);

amd64_lbl_t* find_lbl(amd64_ctx_t* cx, usz lbl_i);

void add_lbl_ref(amd64_ctx_t* cx, amd64_lbl_t* lbl, i32 i);

void resolve_lbls(amd64_ctx_t* cx, u32 i);

