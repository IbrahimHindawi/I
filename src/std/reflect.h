#pragma once

#include <core.h>
#include <stddef.h>
#include <string.h>

#ifndef I_REFLECT_TYPES_DEFINED
#define I_REFLECT_TYPES_DEFINED

/* One record describes every reflected type. `kind` says which, and `variant`
   holds the payload that only that kind has. This replaces the older split
   between i_reflect_type and i_reflect_enum, which forced every consumer to
   know in advance which of two unrelated records it was going to be handed.

   The layout here must match std/reflect.i exactly. Both `kind` fields are
   spelled i32 rather than as C enums on purpose: C leaves an enum's underlying
   type implementation-defined, so a C enum here would make the struct's layout
   depend on the compiler while the I side mirrors it as a fixed i32. */

typedef struct reflect reflect;

/* How a field's type is written, not what the type is. */
typedef enum reflect_type_kind {
    Reflect_Type_Name,
    Reflect_Type_Ptr,
    Reflect_Type_Generic,
    Reflect_Type_Array,
    Reflect_Type_Proc,
} reflect_type_kind;

/* What a reflected type is. Struct and union are separate kinds rather than one
   kind plus a flag, so a consumer that handles only structs cannot silently
   treat a union's overlapping members as if they were adjacent. */
typedef enum reflect_kind {
    Reflect_Struct,
    Reflect_Union,
    Reflect_Enum,
} reflect_kind;

typedef struct reflect_field {
    const char *name;
    const char *type;
    const char *attrs;
    u64 offset;
    u64 size;
    u64 align;
    i32 kind; /* reflect_type_kind */
    u64 array_count;
    u64 pointer_depth;
    const char *base_type;
    const char *elem_type;
    const char *generic_arg_type;
    u64 is_const;
    /* The record for this field's own type, so reflection can recurse. Null when
       the field is a builtin, an external type, or anything with no table. For a
       pointer or array it is the record of the element type. */
    const reflect *info;
} reflect_field;

typedef struct reflect_value {
    const char *name;
    /* i32, matching C: an unadorned C enum's members must fit in int, and I
       permits negative members, so a signed 32-bit value carries all of them. */
    i32 value;
} reflect_value;

typedef union reflect_variant {
    const reflect_field *fields; /* Reflect_Struct, Reflect_Union */
    const reflect_value *values; /* Reflect_Enum */
} reflect_variant;

struct reflect {
    const char *name;
    u64 size;
    u64 align;
    i32 kind; /* reflect_kind */
    u64 count; /* fields for a struct or union, values for an enum */
    reflect_variant variant;
};

#if defined(__clang__) || defined(__GNUC__)
#define I_REFLECT_INLINE static inline __attribute__((unused))
#else
#define I_REFLECT_INLINE static inline
#endif

I_REFLECT_INLINE const char *reflect_type_kind_name(i32 kind) {
    switch (kind) {
        case Reflect_Type_Name: return "name";
        case Reflect_Type_Ptr: return "ptr";
        case Reflect_Type_Generic: return "generic";
        case Reflect_Type_Array: return "array";
        case Reflect_Type_Proc: return "proc";
    }
    return "unknown";
}

I_REFLECT_INLINE const char *reflect_kind_name(i32 kind) {
    switch (kind) {
        case Reflect_Struct: return "struct";
        case Reflect_Union: return "union";
        case Reflect_Enum: return "enum";
    }
    return "unknown";
}

I_REFLECT_INLINE int reflect_is_struct(const reflect *type) {
    return type && type->kind == Reflect_Struct;
}

I_REFLECT_INLINE int reflect_is_union(const reflect *type) {
    return type && type->kind == Reflect_Union;
}

I_REFLECT_INLINE int reflect_is_enum(const reflect *type) {
    return type && type->kind == Reflect_Enum;
}

/* The variant is a plain union, so reading the wrong arm reinterprets a pointer.
   These are the checked way in: they return null rather than the wrong arm when
   the kind does not match, which is what makes the merged record safe to hand
   around without every caller re-checking `kind` first. */
I_REFLECT_INLINE const reflect_field *reflect_fields(const reflect *type) {
    if (!type || (type->kind != Reflect_Struct && type->kind != Reflect_Union)) return 0;
    return type->variant.fields;
}

I_REFLECT_INLINE const reflect_value *reflect_values(const reflect *type) {
    if (!type || type->kind != Reflect_Enum) return 0;
    return type->variant.values;
}

I_REFLECT_INLINE u64 reflect_field_count(const reflect *type) {
    return reflect_fields(type) ? type->count : 0;
}

I_REFLECT_INLINE u64 reflect_value_count(const reflect *type) {
    return reflect_values(type) ? type->count : 0;
}

I_REFLECT_INLINE int reflect_field_is_pointer(const reflect_field *field) {
    return field && field->pointer_depth > 0;
}

I_REFLECT_INLINE int reflect_field_is_array(const reflect_field *field) {
    return field && (field->kind == Reflect_Type_Array || field->array_count > 0);
}

I_REFLECT_INLINE int reflect_field_is_generic(const reflect_field *field) {
    return field && (field->kind == Reflect_Type_Generic || (field->generic_arg_type && field->generic_arg_type[0]));
}

I_REFLECT_INLINE u64 reflect_count_fields_with_kind(const reflect *type, i32 kind) {
    const reflect_field *fields = reflect_fields(type);
    if (!fields) return 0;
    u64 count = 0;
    for (u64 i = 0; i < type->count; i++) {
        if (fields[i].kind == kind) count++;
    }
    return count;
}

I_REFLECT_INLINE const reflect_field *reflect_find_field_with_kind(const reflect *type, i32 kind) {
    const reflect_field *fields = reflect_fields(type);
    if (!fields) return 0;
    for (u64 i = 0; i < type->count; i++) {
        if (fields[i].kind == kind) return &fields[i];
    }
    return 0;
}

I_REFLECT_INLINE const reflect_field *reflect_next_field_with_kind(const reflect *type, i32 kind, const reflect_field *after) {
    const reflect_field *fields = reflect_fields(type);
    if (!fields) return 0;
    u64 start = 0;
    if (after) {
        for (u64 i = 0; i < type->count; i++) {
            if (&fields[i] == after) { start = i + 1; break; }
        }
    }
    for (u64 i = start; i < type->count; i++) {
        if (fields[i].kind == kind) return &fields[i];
    }
    return 0;
}

I_REFLECT_INLINE int reflect_cstr_equal(const char *a, const char *b) {
    if (!a || !b) return 0;
    while (*a && *b && *a == *b) { a++; b++; }
    return *a == 0 && *b == 0;
}

I_REFLECT_INLINE const reflect_field *reflect_find_field(const reflect *type, const char *name) {
    const reflect_field *fields = reflect_fields(type);
    if (!fields || !name) return 0;
    for (u64 i = 0; i < type->count; i++) {
        if (reflect_cstr_equal(fields[i].name, name)) return &fields[i];
    }
    return 0;
}

I_REFLECT_INLINE u64 reflect_field_index(const reflect *type, const reflect_field *field, u64 fallback) {
    const reflect_field *fields = reflect_fields(type);
    if (!fields || !field) return fallback;
    for (u64 i = 0; i < type->count; i++) {
        if (&fields[i] == field) return i;
    }
    return fallback;
}

I_REFLECT_INLINE u64 reflect_find_field_index(const reflect *type, const char *name, u64 fallback) {
    const reflect_field *field = reflect_find_field(type, name);
    return reflect_field_index(type, field, fallback);
}

I_REFLECT_INLINE const reflect_field *reflect_field_at(const reflect *type, u64 index) {
    const reflect_field *fields = reflect_fields(type);
    if (!fields || index >= type->count) return 0;
    return &fields[index];
}

/* The record for a field's own type, so a walk can recurse into nested structs.
   Null when the field's type has no table. */
I_REFLECT_INLINE const reflect *reflect_field_info(const reflect_field *field) {
    return field ? field->info : 0;
}

I_REFLECT_INLINE const reflect *reflect_find_field_info(const reflect *type, const char *name) {
    return reflect_field_info(reflect_find_field(type, name));
}

I_REFLECT_INLINE const reflect_field *reflect_find_field_by_offset(const reflect *type, u64 offset) {
    const reflect_field *fields = reflect_fields(type);
    if (!fields) return 0;
    for (u64 i = 0; i < type->count; i++) {
        if (fields[i].offset == offset) return &fields[i];
    }
    return 0;
}

I_REFLECT_INLINE u64 reflect_field_end_offset(const reflect_field *field) {
    if (!field) return 0;
    return field->offset + field->size;
}

I_REFLECT_INLINE const reflect_field *reflect_find_field_containing_offset(const reflect *type, u64 offset) {
    const reflect_field *fields = reflect_fields(type);
    if (!fields) return 0;
    for (u64 i = 0; i < type->count; i++) {
        u64 start = fields[i].offset;
        u64 end = reflect_field_end_offset(&fields[i]);
        if (start <= offset && offset < end) return &fields[i];
    }
    return 0;
}

I_REFLECT_INLINE void *reflect_field_ptr(void *base, const reflect_field *field) {
    if (!base || !field) return 0;
    return (void *)((unsigned char *)base + field->offset);
}

I_REFLECT_INLINE const void *reflect_field_const_ptr(const void *base, const reflect_field *field) {
    if (!base || !field) return 0;
    return (const void *)((const unsigned char *)base + field->offset);
}

I_REFLECT_INLINE int reflect_field_copy(void *dst_base, const void *src_base, const reflect_field *field) {
    void *dst = reflect_field_ptr(dst_base, field);
    const void *src = reflect_field_const_ptr(src_base, field);
    if (!dst || !src) return 0;
    memmove(dst, src, (size_t)field->size);
    return 1;
}

I_REFLECT_INLINE int reflect_field_zero(void *base, const reflect_field *field) {
    void *dst = reflect_field_ptr(base, field);
    if (!dst) return 0;
    memset(dst, 0, (size_t)field->size);
    return 1;
}

I_REFLECT_INLINE int reflect_field_copy_by_name(void *dst_base, const void *src_base, const reflect *type, const char *name) {
    return reflect_field_copy(dst_base, src_base, reflect_find_field(type, name));
}

I_REFLECT_INLINE int reflect_field_zero_by_name(void *base, const reflect *type, const char *name) {
    return reflect_field_zero(base, reflect_find_field(type, name));
}

I_REFLECT_INLINE int reflect_attr_is_sep(char c) {
    return c == 0 || c == ',' || c == ' ' || c == '\t' || c == '\r' || c == '\n';
}

I_REFLECT_INLINE int reflect_field_has_attr(const reflect_field *field, const char *attr) {
    if (!field || !field->attrs || !attr || !attr[0]) return 0;
    const char *scan = field->attrs;
    while (*scan) {
        while (*scan == ',' || *scan == ' ' || *scan == '\t' || *scan == '\r' || *scan == '\n') scan++;
        const char *token = scan;
        while (*scan && !reflect_attr_is_sep(*scan)) scan++;
        const char *a = token;
        const char *b = attr;
        while (a < scan && *b && *a == *b) { a++; b++; }
        if (a == scan && *b == 0) return 1;
    }
    return 0;
}

I_REFLECT_INLINE u64 reflect_count_fields_with_attr(const reflect *type, const char *attr) {
    const reflect_field *fields = reflect_fields(type);
    if (!fields || !attr || !attr[0]) return 0;
    u64 count = 0;
    for (u64 i = 0; i < type->count; i++) {
        if (reflect_field_has_attr(&fields[i], attr)) count++;
    }
    return count;
}

I_REFLECT_INLINE const reflect_field *reflect_find_field_with_attr(const reflect *type, const char *attr) {
    const reflect_field *fields = reflect_fields(type);
    if (!fields || !attr || !attr[0]) return 0;
    for (u64 i = 0; i < type->count; i++) {
        if (reflect_field_has_attr(&fields[i], attr)) return &fields[i];
    }
    return 0;
}

I_REFLECT_INLINE const reflect_field *reflect_next_field_with_attr(const reflect *type, const char *attr, const reflect_field *after) {
    const reflect_field *fields = reflect_fields(type);
    if (!fields || !attr || !attr[0]) return 0;
    u64 start = 0;
    if (after) {
        for (u64 i = 0; i < type->count; i++) {
            if (&fields[i] == after) { start = i + 1; break; }
        }
    }
    for (u64 i = start; i < type->count; i++) {
        if (reflect_field_has_attr(&fields[i], attr)) return &fields[i];
    }
    return 0;
}

I_REFLECT_INLINE const reflect_value *reflect_find_value_by_name(const reflect *type, const char *name) {
    const reflect_value *values = reflect_values(type);
    if (!values || !name) return 0;
    for (u64 i = 0; i < type->count; i++) {
        if (reflect_cstr_equal(values[i].name, name)) return &values[i];
    }
    return 0;
}

I_REFLECT_INLINE const reflect_value *reflect_find_value_by_value(const reflect *type, i32 value) {
    const reflect_value *values = reflect_values(type);
    if (!values) return 0;
    for (u64 i = 0; i < type->count; i++) {
        if (values[i].value == value) return &values[i];
    }
    return 0;
}

I_REFLECT_INLINE const reflect_value *reflect_value_at(const reflect *type, u64 index) {
    const reflect_value *values = reflect_values(type);
    if (!values || index >= type->count) return 0;
    return &values[index];
}

I_REFLECT_INLINE const char *reflect_name_from_value(const reflect *type, i32 value) {
    const reflect_value *found = reflect_find_value_by_value(type, value);
    return found ? found->name : 0;
}

I_REFLECT_INLINE i32 reflect_value_from_name(const reflect *type, const char *name, i32 fallback) {
    const reflect_value *found = reflect_find_value_by_name(type, name);
    return found ? found->value : fallback;
}

#undef I_REFLECT_INLINE

#endif
