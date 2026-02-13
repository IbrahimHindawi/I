#include "Component.h"

Component Component_create(int id) {
    Component result = {0};
    result.id = id;
    // result.points = hkArray_i32_create(10);
    // result.points = hkArray_vec3_create(10);
    return result;
}

void Component_destroy(Component *comp) {
    // hkArray_i32_destroy(&comp->points);
    // hkArray_vec3_destroy(&comp->points);
}
