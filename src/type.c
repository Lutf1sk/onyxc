// Copyright (C) 2021, Alex Edin <lutfisk@lutfisk.net>
// SPDX-License-Identifier: GPL-2.0+

#include <string.h>
#include <stdlib.h>
#include <assert.h>

#include "type.h"
#include "parse.h"

#define ALLOC_CHUNK_SIZE 2048

void free_type_tab(Types* tab) {
    if (tab->name_arr)
        free(tab->name_arr);
    if (tab->info_arr)
        free(tab->info_arr);
}

usz add_type(Types* tab, const LenStr name, TypeInfo* info) {
    usz type_count = tab->type_count;
    LenStr* name_arr = tab->name_arr;
    TypeInfo* info_arr = tab->info_arr;

    if (type_count % ALLOC_CHUNK_SIZE == 0) {
        usz new_count = type_count + ALLOC_CHUNK_SIZE;
        name_arr = realloc(tab->name_arr, new_count * sizeof(LenStr));
        info_arr = realloc(tab->info_arr, new_count * sizeof(TypeInfo));
        assert(name_arr);
        assert(info_arr);
    }

    name_arr[type_count] = name;
    info_arr[type_count] = *info;

    ++tab->type_count;
    tab->name_arr = name_arr;
    tab->info_arr = info_arr;

    return type_count;
}

usz add_primitive(Types* tab, const LenStr name, TypeSType type) {
    TypeInfo inf = (TypeInfo) { TP_INVALID, INVALID_TYPE, 0, NULL };
    inf.stype = type;
    return add_type(tab, name, &inf);
}
