import "cstd.i"
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

// `const` is load-bearing, not decoration. These lower to C globals, and a
// mutable global cannot be constant-folded: the optimiser has to assume some
// other translation unit changed it. With `const` it folds, and an entire
// reflection lookup -- `reflect_find_field(Point<>.&, "y")[0].offset`, string
// compare and field walk included -- collapses to a single load of a constant.
// Without it, the same call leaves two runtime compares behind.
//
// Each set is named for the record whose `kind` field it belongs to. They cannot
// reuse C's spellings (Reflect_Type_Name, Reflect_Struct, ...) because an I
// global lowers to a real C definition, which would redefine the enum constants
// reflect.h already declares under those names.

// Values for reflect_field.kind -- how a field's type is written.
reflect_field_kind_name: const i32 = 0;
reflect_field_kind_ptr: const i32 = 1;
reflect_field_kind_generic: const i32 = 2;
reflect_field_kind_array: const i32 = 3;
reflect_field_kind_proc: const i32 = 4;

// Values for reflect.kind -- what the reflected type is. Struct and union are
// distinct kinds, not one kind plus a flag, so a walker that only knows structs
// cannot quietly treat a union's overlapping members as adjacent.
reflect_kind_struct: const i32 = 0;
reflect_kind_union: const i32 = 1;
reflect_kind_enum: const i32 = 2;

reflect_field: struct[external] = {
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

reflect_value: struct[external] = {
    name: *const char;
    // i32, matching C. An unadorned C enum's members must fit in int, and I
    // permits negative members, so signed 32-bit carries every legal value.
    value: i32;
}

reflect_variant: union[external] = {
    fields: *const reflect_field;
    values: *const reflect_value;
}

reflect: struct[external] = {
    name: *const char;
    size: u64;
    align: u64;
    kind: i32;
    // Fields for a struct or union, values for an enum.
    count: u64;
    variant: reflect_variant;
}

// The helpers below are written in I, not bound from C.
//
// They are pure logic -- null checks, kind compares, loops over the arrays the
// compiler emitted -- so there was never a reason for them to live in a C
// header. Keeping them here means they are type-checked, readable in the
// language they belong to, and cannot drift from a separate set of hand-written
// declarations the way an `external` binding can.
//
// The types above are a different matter and stay `external`: the compiler emits
// its tables as C initialisers of those types, so C has to own their layout.

// ---------------------------------------------------------------------------
// Kind
// ---------------------------------------------------------------------------

reflect_kind_name: proc(kind: i32) -> *const char = {
    if (kind == reflect_kind_struct) { return "struct"; }
    if (kind == reflect_kind_union) { return "union"; }
    if (kind == reflect_kind_enum) { return "enum"; }
    return "unknown";
}

reflect_type_kind_name: proc(kind: i32) -> *const char = {
    if (kind == reflect_field_kind_name) { return "name"; }
    if (kind == reflect_field_kind_ptr) { return "ptr"; }
    if (kind == reflect_field_kind_generic) { return "generic"; }
    if (kind == reflect_field_kind_array) { return "array"; }
    if (kind == reflect_field_kind_proc) { return "proc"; }
    return "unknown";
}

reflect_is_struct: proc(type: *const reflect) -> b32 = {
    return type != null and type[0].kind == reflect_kind_struct;
}

reflect_is_union: proc(type: *const reflect) -> b32 = {
    return type != null and type[0].kind == reflect_kind_union;
}

reflect_is_enum: proc(type: *const reflect) -> b32 = {
    return type != null and type[0].kind == reflect_kind_enum;
}

// ---------------------------------------------------------------------------
// Variant arms
//
// `variant` is a plain union, so reading the arm that is not live reinterprets a
// pointer instead of failing. These two are the checked way in: they hand back
// the arm only when the kind matches, and null otherwise. Where the kind is
// known at compile time -- any `Type<>` -- the compiler rejects the wrong arm
// outright and these are not needed.
// ---------------------------------------------------------------------------

reflect_fields: proc(type: *const reflect) -> *const reflect_field = {
    if (type == null) { return null; }
    if (type[0].kind != reflect_kind_struct and type[0].kind != reflect_kind_union) { return null; }
    return type[0].variant.fields;
}

reflect_values: proc(type: *const reflect) -> *const reflect_value = {
    if (type == null or type[0].kind != reflect_kind_enum) { return null; }
    return type[0].variant.values;
}

reflect_field_count: proc(type: *const reflect) -> u64 = {
    if (reflect_fields(type) == null) { return 0; }
    return type[0].count;
}

reflect_value_count: proc(type: *const reflect) -> u64 = {
    if (reflect_values(type) == null) { return 0; }
    return type[0].count;
}

// ---------------------------------------------------------------------------
// Fields
// ---------------------------------------------------------------------------

reflect_cstr_equal: proc(a: *const char, b: *const char) -> b32 = {
    i: u64 = 0;
    if (a == null or b == null) { return 0; }
    while (a[i] != 0 and b[i] != 0) {
        if (a[i] != b[i]) { return 0; }
        i += 1;
    }
    return a[i] == 0 and b[i] == 0;
}

reflect_field_at: proc(type: *const reflect, index: u64) -> *const reflect_field = {
    fields: *const reflect_field = reflect_fields(type);
    if (fields == null or index >= type[0].count) { return null; }
    return fields[index].&;
}

reflect_find_field: proc(type: *const reflect, name: *const char) -> *const reflect_field = {
    fields: *const reflect_field = reflect_fields(type);
    if (fields == null or name == null) { return null; }
    for (i: u64 = 0; i < type[0].count; i += 1) {
        if (reflect_cstr_equal(fields[i].name, name) != 0) { return fields[i].&; }
    }
    return null;
}

reflect_find_field_index: proc(type: *const reflect, name: *const char, fallback: u64) -> u64 = {
    fields: *const reflect_field = reflect_fields(type);
    if (fields == null or name == null) { return fallback; }
    for (i: u64 = 0; i < type[0].count; i += 1) {
        if (reflect_cstr_equal(fields[i].name, name) != 0) { return i; }
    }
    return fallback;
}

// The record for a field's own type, so a walk can recurse instead of stopping
// at the type-name string. Null when the field's type has no table: a builtin,
// an external type, a proc.
reflect_field_info: proc(field: *const reflect_field) -> *const reflect = {
    if (field == null) { return null; }
    return field[0].info;
}

reflect_find_field_info: proc(type: *const reflect, name: *const char) -> *const reflect = {
    return reflect_field_info(reflect_find_field(type, name));
}

// ---------------------------------------------------------------------------
// Enum values
// ---------------------------------------------------------------------------

reflect_value_at: proc(type: *const reflect, index: u64) -> *const reflect_value = {
    values: *const reflect_value = reflect_values(type);
    if (values == null or index >= type[0].count) { return null; }
    return values[index].&;
}

reflect_find_value_by_name: proc(type: *const reflect, name: *const char) -> *const reflect_value = {
    values: *const reflect_value = reflect_values(type);
    if (values == null or name == null) { return null; }
    for (i: u64 = 0; i < type[0].count; i += 1) {
        if (reflect_cstr_equal(values[i].name, name) != 0) { return values[i].&; }
    }
    return null;
}

reflect_find_value_by_value: proc(type: *const reflect, value: i32) -> *const reflect_value = {
    values: *const reflect_value = reflect_values(type);
    if (values == null) { return null; }
    for (i: u64 = 0; i < type[0].count; i += 1) {
        if (values[i].value == value) { return values[i].&; }
    }
    return null;
}

reflect_name_from_value: proc(type: *const reflect, value: i32) -> *const char = {
    found: *const reflect_value = reflect_find_value_by_value(type, value);
    return found != null ? found[0].name : null;
}

reflect_value_from_name: proc(type: *const reflect, name: *const char, fallback: i32) -> i32 = {
    found: *const reflect_value = reflect_find_value_by_name(type, name);
    return found != null ? found[0].value : fallback;
}

// Same as reflect_name_from_value, but never returns null.
//
// The plain version returns null when nothing matches, which is right for code
// that wants to test the result and wrong for the common use -- printing the
// name of a status enum. Passing null to a `%s` is undefined behaviour; clang's
// runtime happens to print `(null)` and MSVC's does not have to.
//
// This is the direction a status enum is usually read in:
//
//     e: mesh_status = mesh_load(mesh.&);
//     if (e != mesh_status.success) {
//         printfmt("load failed: {}\n",
//                  reflect_name_from_value_or(mesh_status<>.&, cast(e, i32), "?"));
//     }
//
// A miss is not necessarily a bug: an enum value can arrive from a file or a
// socket and hold anything that fits in the underlying type.
reflect_name_from_value_or: proc(type: *const reflect, value: i32, fallback: *const char) -> *const char = {
    found: *const reflect_value = reflect_find_value_by_value(type, value);
    return found != null ? found[0].name : fallback;
}


// ---------------------------------------------------------------------------
// Field shape
// ---------------------------------------------------------------------------

reflect_field_is_pointer: proc(field: *const reflect_field) -> b32 = {
    return field != null and field[0].pointer_depth > 0;
}

reflect_field_is_array: proc(field: *const reflect_field) -> b32 = {
    if (field == null) { return 0; }
    return field[0].kind == reflect_field_kind_array or field[0].array_count > 0;
}

reflect_field_is_generic: proc(field: *const reflect_field) -> b32 = {
    if (field == null) { return 0; }
    if (field[0].kind == reflect_field_kind_generic) { return 1; }
    return field[0].generic_arg_type != null and field[0].generic_arg_type[0] != 0;
}

reflect_count_fields_with_kind: proc(type: *const reflect, kind: i32) -> u64 = {
    fields: *const reflect_field = reflect_fields(type);
    count: u64 = 0;
    if (fields == null) { return 0; }
    for (i: u64 = 0; i < type[0].count; i += 1) {
        if (fields[i].kind == kind) { count += 1; }
    }
    return count;
}

reflect_find_field_with_kind: proc(type: *const reflect, kind: i32) -> *const reflect_field = {
    fields: *const reflect_field = reflect_fields(type);
    if (fields == null) { return null; }
    for (i: u64 = 0; i < type[0].count; i += 1) {
        if (fields[i].kind == kind) { return fields[i].&; }
    }
    return null;
}

// `after` walks the matches in order: pass null for the first, then the previous
// result for each one after it.
reflect_next_field_with_kind: proc(type: *const reflect, kind: i32, after: *const reflect_field) -> *const reflect_field = {
    fields: *const reflect_field = reflect_fields(type);
    start: u64 = 0;
    if (fields == null) { return null; }
    if (after != null) {
        for (i: u64 = 0; i < type[0].count; i += 1) {
            if (fields[i].& == after) { start = i + 1; break; }
        }
    }
    for (i: u64 = start; i < type[0].count; i += 1) {
        if (fields[i].kind == kind) { return fields[i].&; }
    }
    return null;
}

reflect_field_index: proc(type: *const reflect, field: *const reflect_field, fallback: u64) -> u64 = {
    fields: *const reflect_field = reflect_fields(type);
    if (fields == null or field == null) { return fallback; }
    for (i: u64 = 0; i < type[0].count; i += 1) {
        if (fields[i].& == field) { return i; }
    }
    return fallback;
}

// ---------------------------------------------------------------------------
// Offsets
// ---------------------------------------------------------------------------

reflect_field_end_offset: proc(field: *const reflect_field) -> u64 = {
    if (field == null) { return 0; }
    return field[0].offset + field[0].size;
}

reflect_find_field_by_offset: proc(type: *const reflect, offset: u64) -> *const reflect_field = {
    fields: *const reflect_field = reflect_fields(type);
    if (fields == null) { return null; }
    for (i: u64 = 0; i < type[0].count; i += 1) {
        if (fields[i].offset == offset) { return fields[i].&; }
    }
    return null;
}

// The field an arbitrary byte falls inside, rather than one that starts there.
reflect_find_field_containing_offset: proc(type: *const reflect, offset: u64) -> *const reflect_field = {
    fields: *const reflect_field = reflect_fields(type);
    if (fields == null) { return null; }
    for (i: u64 = 0; i < type[0].count; i += 1) {
        if (fields[i].offset <= offset and offset < reflect_field_end_offset(fields[i].&)) {
            return fields[i].&;
        }
    }
    return null;
}

// ---------------------------------------------------------------------------
// Reading and writing a field through its record
// ---------------------------------------------------------------------------

reflect_field_ptr: proc(base: *void, field: *const reflect_field) -> *void = {
    if (base == null or field == null) { return null; }
    return cast(cast(base, *u8) + field[0].offset, *void);
}

reflect_field_const_ptr: proc(base: *const void, field: *const reflect_field) -> *const void = {
    if (base == null or field == null) { return null; }
    return cast(cast(base, *const u8) + field[0].offset, *const void);
}

reflect_field_copy: proc(dst_base: *void, src_base: *const void, field: *const reflect_field) -> b32 = {
    dst: *void = reflect_field_ptr(dst_base, field);
    src: *const void = reflect_field_const_ptr(src_base, field);
    if (dst == null or src == null) { return 0; }
    memmove(dst, src, cast(field[0].size, usize));
    return 1;
}

reflect_field_zero: proc(base: *void, field: *const reflect_field) -> b32 = {
    dst: *void = reflect_field_ptr(base, field);
    if (dst == null) { return 0; }
    memset(dst, 0, cast(field[0].size, usize));
    return 1;
}

reflect_field_copy_by_name: proc(dst_base: *void, src_base: *const void, type: *const reflect, name: *const char) -> b32 = {
    return reflect_field_copy(dst_base, src_base, reflect_find_field(type, name));
}

reflect_field_zero_by_name: proc(base: *void, type: *const reflect, name: *const char) -> b32 = {
    return reflect_field_zero(base, reflect_find_field(type, name));
}

// ---------------------------------------------------------------------------
// Attributes
//
// A field's `attrs` is the raw string written after `@`. Queries treat it as a
// list separated by commas or whitespace, so `@ "save,editor"` and
// `@ "save editor"` both answer yes to "save".
// ---------------------------------------------------------------------------

reflect_attr_is_sep: proc(c: char) -> b32 = {
    return c == 0 or c == 44 or c == 32 or c == 9 or c == 13 or c == 10;
}

reflect_field_has_attr: proc(field: *const reflect_field, attr: *const char) -> b32 = {
    scan: u64 = 0;
    token: u64 = 0;
    a: u64 = 0;
    b: u64 = 0;
    attrs: *const char = null;

    if (field == null or field[0].attrs == null or attr == null or attr[0] == 0) { return 0; }
    attrs = field[0].attrs;
    while (attrs[scan] != 0) {
        while (attrs[scan] == 44 or attrs[scan] == 32 or attrs[scan] == 9 or attrs[scan] == 13 or attrs[scan] == 10) {
            scan += 1;
        }
        token = scan;
        while (attrs[scan] != 0 and reflect_attr_is_sep(attrs[scan]) == 0) {
            scan += 1;
        }
        a = token;
        b = 0;
        while (a < scan and attr[b] != 0 and attrs[a] == attr[b]) {
            a += 1;
            b += 1;
        }
        if (a == scan and attr[b] == 0) { return 1; }
    }
    return 0;
}

reflect_count_fields_with_attr: proc(type: *const reflect, attr: *const char) -> u64 = {
    fields: *const reflect_field = reflect_fields(type);
    count: u64 = 0;
    if (fields == null or attr == null or attr[0] == 0) { return 0; }
    for (i: u64 = 0; i < type[0].count; i += 1) {
        if (reflect_field_has_attr(fields[i].&, attr) != 0) { count += 1; }
    }
    return count;
}

reflect_find_field_with_attr: proc(type: *const reflect, attr: *const char) -> *const reflect_field = {
    fields: *const reflect_field = reflect_fields(type);
    if (fields == null or attr == null or attr[0] == 0) { return null; }
    for (i: u64 = 0; i < type[0].count; i += 1) {
        if (reflect_field_has_attr(fields[i].&, attr) != 0) { return fields[i].&; }
    }
    return null;
}

reflect_next_field_with_attr: proc(type: *const reflect, attr: *const char, after: *const reflect_field) -> *const reflect_field = {
    fields: *const reflect_field = reflect_fields(type);
    start: u64 = 0;
    if (fields == null or attr == null or attr[0] == 0) { return null; }
    if (after != null) {
        for (i: u64 = 0; i < type[0].count; i += 1) {
            if (fields[i].& == after) { start = i + 1; break; }
        }
    }
    for (i: u64 = start; i < type[0].count; i += 1) {
        if (reflect_field_has_attr(fields[i].&, attr) != 0) { return fields[i].&; }
    }
    return null;
}
