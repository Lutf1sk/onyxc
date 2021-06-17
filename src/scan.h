#ifndef SCAN_H
#define SCAN_H

#include "common.h"
#include "fwd.h"

typedef
struct ScanCtx {
    const char* file_path;

    usz line_index;

    const char* char_data;
    usz char_count;
    usz char_it;

    Token* token_data;
    usz token_count;
    usz token_avail_count;
} ScanCtx;


void scan(ScanCtx* cx);

#endif // SCAN_H
