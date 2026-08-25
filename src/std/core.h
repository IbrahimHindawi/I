#pragma once

// #ifdef _MSC_VER
// #   define _CRT_SECURE_NO_WARNINGS
// #endif

#include <math.h>
#include <stddef.h>  // ptrdiff_t, for the `ptrdiff` alias below
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#define null NULL

typedef float f32;
typedef double f64;
typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;
typedef int8_t i8;
typedef int16_t i16;
typedef int32_t i32;
typedef int64_t i64;
typedef size_t usize;

/* `bool` is one byte, so a boolean family whose names carry a width could not be
   built on it -- b8 and b32 would have been the same type twice. Each is the
   matching fixed-width integer instead. b32 is the default boolean: it is what
   comparisons and `and`/`or` produce, and it matches Win32's BOOL. Note these do
   not normalise to 0/1 the way C's bool does. */
typedef uint8_t  b8;
typedef uint16_t b16;
typedef int32_t  b32;
typedef int64_t  b64;

/* ilang's name for C's char: whatever width and signedness the C compiler on
   this target gives it, which is exactly the point -- the language declines to
   have an opinion rather than pretending the question is settled. */
typedef char c8;

/* Spellings of C's own fixed names, so a program can say what it means without
   reaching for a cinclude. */
typedef intptr_t  intptr;
typedef uintptr_t uintptr;
typedef ptrdiff_t ptrdiff;
typedef intmax_t  intmax;
typedef uintmax_t uintmax;

typedef void *voidptr;
// typedef i8 *str;
// typedef const str *cstr;
typedef const char *str;
typedef str *strptr;

#define static_internal static
#define static_global static
#define static_local static

#define Kilobytes(N) ((u64)(N) * 1024)
#define Megabytes(N) ((u64)Kilobytes(N) * 1024)
#define Gigabytes(N) ((u64)Megabytes(N) * 1024)

#define stringify(s) #s
#define concat(a, b) a##b
// #define pi 3.141592f
#define sizeofarray(array) (sizeof(array) / sizeof(array[0]))
#define cast(T, V) (T)(V)

#define primdecl(primname)
#define enumdecl(enumname) typedef enum enumname enumname
#define enumdef(enumname) typedef enum enumname enumname; enum enumname
#define uniondecl(unionname) typedef union unionname unionname
#define uniondef(unionname) typedef union unionname unionname; union unionname
#define structdecl(structname) typedef struct structname structname
#define structdef(structname) typedef struct structname structname; struct structname

#define template(...)

#ifdef __cplusplus
    #define haikal_alignof(type) alignof(type)
#else
    #define haikal_alignof(type) _Alignof(type)
#endif
