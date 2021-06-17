// Copyright (C) 2021, Alex Edin <lutfisk@lutfisk.net>
// SPDX-License-Identifier: GPL-2.0+

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

#include "err.h"

void NORETURN err(const char* fmt, ...) {
    va_list args;
    va_start(args, fmt);
    vprintf(fmt, args);
    putchar('\n');
    va_end(args);

    exit(1);
}
