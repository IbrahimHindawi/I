#pragma once
#include <core.h>
#include <intrin.h>

struct vec3 {
    f32 x;
    f32 y;
    f32 z;
};

#pragma pack(push, 16)
typedef struct vec4 vec4;
struct vec4 {
    f32 x;
    f32 y;
    f32 z;
    f32 w;
};
#pragma pack(pop)

typedef __m128 f128;
