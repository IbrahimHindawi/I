#pragma once
#include <core.h>

structdef(Rec) {
    f32 amount;
    i32 *ids;
    // vec3 pos;
    // Array_Rec arr; // possible coz `T *`
    // HashMap_Rec hsh; // impossible coz `T`, infinite recursion
};
