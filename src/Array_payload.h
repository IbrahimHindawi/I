#pragma once
#include <core.h>

structdecl(memops_arena);

structdecl(payload);

#line 1 "<generated>"
/* I monomorph: struct Array<T> -> Array_payload; declared at C:\devel\i\src\runtime\Array.i:3:1 */
#line 3 "C:\\devel\\i\\src\\runtime\\Array.i"
structdef(Array_payload) {
#line 4 "C:\\devel\\i\\src\\runtime\\Array.i"
    payload * data;
#line 5 "C:\\devel\\i\\src\\runtime\\Array.i"
    u64 length;
};

#line 1 "<generated>"
/* I monomorph: proc Array<T>reserve -> Array_payload_reserve; declared at C:\devel\i\src\runtime\Array.i:9:1 */
#line 9 "C:\\devel\\i\\src\\runtime\\Array.i"
Array_payload Array_payload_reserve(memops_arena * arena, u64 length);
