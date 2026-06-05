#pragma once
#include <core.h>

structdecl(memops_arena);

#line 1 "<generated>"
/* I monomorph: struct Array<T> -> Array_i32; declared at C:\devel\i\src\runtime\Array.i:3:1 */
#line 3 "C:\\devel\\i\\src\\runtime\\Array.i"
structdef(Array_i32) {
#line 4 "C:\\devel\\i\\src\\runtime\\Array.i"
    i32 * data;
#line 5 "C:\\devel\\i\\src\\runtime\\Array.i"
    u64 length;
};

#line 1 "<generated>"
/* I monomorph: proc Array<T>reserve -> Array_i32_reserve; declared at C:\devel\i\src\runtime\Array.i:9:1 */
#line 9 "C:\\devel\\i\\src\\runtime\\Array.i"
Array_i32 Array_i32_reserve(memops_arena * arena, u64 length);
