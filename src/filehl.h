// Copyright (C) 2021, Alex Edin <lutfisk@lutfisk.net>
// SPDX-License-Identifier: GPL-2.0+

#ifndef FILEHL_H
#define FILEHL_H

#include <stdio.h>

#include "common.h"

FILE* hl_fopen_r(const char* path);
FILE* hl_fopen_w(const char* path);

usz hl_fsize(FILE* stream);

usz hl_fread(FILE* stream, void* data, usz len);
usz hl_fwrite(FILE* stream, const void* data, usz len);

#endif // FILEHL_H
