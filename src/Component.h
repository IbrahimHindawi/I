#pragma once
#include <core.h>
#include "vec3.h"
//haikal@hkArray_i32
//haikal@hkArray_vec3
// #include <hkArray_i32.h>

structdef(Component) {
    i32 id;
    // hkArray_i32 ids;
    // hkArray_vec3 points;
};

Component Component_create(int id);
void Component_destroy(Component *comp);
