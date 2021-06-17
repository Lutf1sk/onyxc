// Copyright (C) 2021, Alex Edin <lutfisk@lutfisk.net>
// SPDX-License-Identifier: GPL-2.0+

#ifndef ERR_H
#define ERR_H

#include "common.h"

void NORETURN err(const char* fmt, ...);

#endif // ERR_H
