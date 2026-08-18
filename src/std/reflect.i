// Reflection records, mirroring std/reflect.h.
//
// The compiler generates a table of these for every struct, union and enum, so
// `Color<>` and `Point<>` hand you one. Until this file existed each project
// re-declared them as field-less `external` stubs, which meant every reflection
// access -- `meta[0].value_count`, `meta[0].fields[i].name` -- was unchecked and
// passed straight through to C. Declaring the fields alongside `external` opts
// the types back into checking while leaving their definitions in C.
//
// The layout here must match std/reflect.h exactly.

// How a field's type is spelled. Mirrors i_reflect_type_kind.
i_reflect_kind_name: i32 = 0;
i_reflect_kind_ptr: i32 = 1;
i_reflect_kind_generic: i32 = 2;
i_reflect_kind_array: i32 = 3;
i_reflect_kind_proc: i32 = 4;

i_reflect_field: struct = {
    external;
    name: *const char;
    type: *const char;
    attrs: *const char;
    offset: u64;
    size: u64;
    align: u64;
    kind: i32;
    array_count: u64;
    pointer_depth: u64;
    base_type: *const char;
    elem_type: *const char;
    generic_arg_type: *const char;
    is_const: u64;
}

i_reflect_type: struct = {
    external;
    name: *const char;
    size: u64;
    align: u64;
    field_count: u64;
    fields: *const i_reflect_field;
}

i_reflect_enum_value: struct = {
    external;
    name: *const char;
    value: i32;
}

i_reflect_enum: struct = {
    external;
    name: *const char;
    size: u64;
    align: u64;
    value_count: u64;
    values: *const i_reflect_enum_value;
}
