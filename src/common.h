#ifndef COMMON_H
#define COMMON_H

#include <stdint.h>
#include <stddef.h>

// Compiler-specific macros
#define NORETURN __attribute__((noreturn))
#define PACKED __attribute__((packed))
#define FLATTEN __attribute__((flatten))
#define INLINE __attribute__((always_inline))

// Less stupid primitive names
typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

typedef int8_t i8;
typedef int16_t i16;
typedef int32_t i32;
typedef int64_t i64;

typedef size_t usz;
typedef intmax_t isz;

typedef float f32;
typedef double f64;

typedef u8 b8;

typedef
struct LenStr {
    char* str;
    usz len;
} LenStr;

#define LSTR(str, len) ((LenStr) { str, len })

#endif // COMMON_H
