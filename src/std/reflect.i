// Reflection records, mirroring std/reflect.h.
//
// The compiler generates one table per struct, union and enum, so `Color<>` and
// `Point<>` both hand you a `reflect`. `kind` says which of the three it is and
// `variant` holds the payload only that kind has -- there is no longer a
// separate record per kind for a consumer to guess between.
//
// Declaring the fields alongside `external` is what opts these types back into
// checking: an `external` struct with no field list accepts any field name and
// passes it straight through to C, so `meta[0].value_kount` would have been a C
// error in generated code rather than a diagnostic here.
//
// The layout here must match std/reflect.h exactly. Both `kind` fields are i32
// rather than enums because C leaves an enum's underlying type
// implementation-defined, which would make the layout compiler-dependent on one
// side of a struct that has to agree on both.

// Each set is named for the record whose `kind` field it belongs to. They cannot
// reuse C's spellings (Reflect_Type_Name, Reflect_Struct, ...) because an I
// global lowers to a real C definition, which would redefine the enum constants
// reflect.h already declares under those names.

// Values for reflect_field.kind -- how a field's type is written.
reflect_field_kind_name: i32 = 0;
reflect_field_kind_ptr: i32 = 1;
reflect_field_kind_generic: i32 = 2;
reflect_field_kind_array: i32 = 3;
reflect_field_kind_proc: i32 = 4;

// Values for reflect.kind -- what the reflected type is. Struct and union are
// distinct kinds, not one kind plus a flag, so a walker that only knows structs
// cannot quietly treat a union's overlapping members as adjacent.
reflect_kind_struct: i32 = 0;
reflect_kind_union: i32 = 1;
reflect_kind_enum: i32 = 2;

reflect_field: struct = {
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
    // The record for this field's own type, so a walk can recurse. Null when the
    // field's type has no table: a builtin, an external type, a proc.
    info: *const reflect;
}

reflect_value: struct = {
    external;
    name: *const char;
    // i32, matching C. An unadorned C enum's members must fit in int, and I
    // permits negative members, so signed 32-bit carries every legal value.
    value: i32;
}

reflect_variant: union = {
    external;
    fields: *const reflect_field;
    values: *const reflect_value;
}

reflect: struct = {
    external;
    name: *const char;
    size: u64;
    align: u64;
    kind: i32;
    // Fields for a struct or union, values for an enum.
    count: u64;
    variant: reflect_variant;
}

// Helpers from std/reflect.h, declared so I can reach them.
//
// The variant is a plain union, so reading the wrong arm reinterprets a pointer
// rather than failing. reflect_fields and reflect_values are the checked way in:
// they return null when the kind does not match, which is what makes a `reflect`
// safe to pass around without every caller re-checking `kind` first.
reflect_fields: proc(type: *const reflect)->*const reflect_field = { external; }
reflect_values: proc(type: *const reflect)->*const reflect_value = { external; }
reflect_field_count: proc(type: *const reflect)->u64 = { external; }
reflect_value_count: proc(type: *const reflect)->u64 = { external; }

reflect_is_struct: proc(type: *const reflect)->i32 = { external; }
reflect_is_union: proc(type: *const reflect)->i32 = { external; }
reflect_is_enum: proc(type: *const reflect)->i32 = { external; }

reflect_kind_name: proc(kind: i32)->*const char = { external; }
reflect_type_kind_name: proc(kind: i32)->*const char = { external; }

reflect_find_field: proc(type: *const reflect, name: *const char)->*const reflect_field = { external; }
reflect_field_at: proc(type: *const reflect, index: u64)->*const reflect_field = { external; }
reflect_find_field_index: proc(type: *const reflect, name: *const char, fallback: u64)->u64 = { external; }

// The nested record for a field's own type, so a walk can recurse.
reflect_field_info: proc(field: *const reflect_field)->*const reflect = { external; }
reflect_find_field_info: proc(type: *const reflect, name: *const char)->*const reflect = { external; }

reflect_find_value_by_name: proc(type: *const reflect, name: *const char)->*const reflect_value = { external; }
reflect_find_value_by_value: proc(type: *const reflect, value: i32)->*const reflect_value = { external; }
reflect_value_at: proc(type: *const reflect, index: u64)->*const reflect_value = { external; }
reflect_name_from_value: proc(type: *const reflect, value: i32)->*const char = { external; }
reflect_value_from_name: proc(type: *const reflect, name: *const char, fallback: i32)->i32 = { external; }
