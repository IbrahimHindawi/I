// The C runtime, declared for I.
//
// A `cinclude` arranges for a header to reach the C compiler; it does not bring
// any name into I. Naming a C function here is what makes it callable, and
// calling one that was never named is an error. See docs/name-resolution.md.
//
// An external proc emits call sites only, never a prototype, so these
// declarations describe the headers' functions to I without redeclaring
// anything to C.

cinclude "stdio.h"
cinclude "stdlib.h"
cinclude "string.h"

// Opaque on purpose: FILE's layout is implementation-defined, and std only ever
// holds a *FILE. Field access on it is rejected rather than passed through.
FILE: struct[external] = {}

printf: proc[external](fmt: *const char, ...) -> i32 = {}
fopen_s: proc[external](stream: **FILE, filename: *const char, mode: *const char) -> i32 = {}
fclose: proc[external](stream: *FILE) -> i32 = {}
// C spells these with `long`, which is 32-bit on Windows. Declaring them
// as i64 here disagreed with njinn's declaration of the same functions,
// which the compatible-redeclaration rule correctly rejected.
fseek: proc[external](stream: *FILE, offset: long, origin: i32) -> i32 = {}
ftell: proc[external](stream: *FILE) -> long = {}
fread: proc[external](buffer: *void, size: usize, count: usize, stream: *FILE) -> usize = {}

exit: proc[external](status: i32) -> void = {}

memcpy: proc[external](dst: *void, src: *const void, count: usize) -> *void = {}
memmove: proc[external](dst: *void, src: *const void, count: usize) -> *void = {}
memset: proc[external](dst: *void, value: i32, count: usize) -> *void = {}
memcmp: proc[external](a: *const void, b: *const void, count: usize) -> i32 = {}
strlen: proc[external](s: *const char) -> usize = {}
strcmp: proc[external](a: *const char, b: *const char) -> i32 = {}
