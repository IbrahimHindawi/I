#pragma once

#include <core.h>
#include <stddef.h>
#include <string.h>

#ifndef I_REFLECT_TYPES_DEFINED
#define I_REFLECT_TYPES_DEFINED

typedef enum i_reflect_type_kind {
    I_Reflect_Type_Name,
    I_Reflect_Type_Ptr,
    I_Reflect_Type_Generic,
    I_Reflect_Type_Array,
    I_Reflect_Type_Proc,
} i_reflect_type_kind;

typedef struct i_reflect_field {
    const char *name;
    const char *type;
    const char *attrs;
    u64 offset;
    u64 size;
    u64 align;
    i_reflect_type_kind kind;
    u64 array_count;
    u64 pointer_depth;
    const char *base_type;
    const char *elem_type;
    const char *generic_arg_type;
    u64 is_const;
} i_reflect_field;

typedef struct i_reflect_type {
    const char *name;
    u64 size;
    u64 align;
    u64 field_count;
    const i_reflect_field *fields;
} i_reflect_type;

typedef struct i_reflect_enum_value {
    const char *name;
    i32 value;
} i_reflect_enum_value;

typedef struct i_reflect_enum {
    const char *name;
    u64 size;
    u64 align;
    u64 value_count;
    const i_reflect_enum_value *values;
} i_reflect_enum;

#if defined(__clang__) || defined(__GNUC__)
#define I_REFLECT_INLINE static inline __attribute__((unused))
#else
#define I_REFLECT_INLINE static inline
#endif

I_REFLECT_INLINE const char *i_reflect_type_kind_name(i_reflect_type_kind kind) {
    switch (kind) {
        case I_Reflect_Type_Name: return "name";
        case I_Reflect_Type_Ptr: return "ptr";
        case I_Reflect_Type_Generic: return "generic";
        case I_Reflect_Type_Array: return "array";
        case I_Reflect_Type_Proc: return "proc";
    }
    return "unknown";
}

I_REFLECT_INLINE int i_reflect_field_is_pointer(const i_reflect_field *field) {
    return field && field->pointer_depth > 0;
}

I_REFLECT_INLINE int i_reflect_field_is_array(const i_reflect_field *field) {
    return field && (field->kind == I_Reflect_Type_Array || field->array_count > 0);
}

I_REFLECT_INLINE int i_reflect_field_is_generic(const i_reflect_field *field) {
    return field && (field->kind == I_Reflect_Type_Generic || (field->generic_arg_type && field->generic_arg_type[0]));
}

I_REFLECT_INLINE u64 i_reflect_count_fields_with_kind(const i_reflect_type *type, i_reflect_type_kind kind) {
    if (!type) return 0;
    u64 count = 0;
    for (u64 i = 0; i < type->field_count; i++) {
        if (type->fields[i].kind == kind) count++;
    }
    return count;
}

I_REFLECT_INLINE const i_reflect_field *i_reflect_find_field_with_kind(const i_reflect_type *type, i_reflect_type_kind kind) {
    if (!type) return 0;
    for (u64 i = 0; i < type->field_count; i++) {
        if (type->fields[i].kind == kind) return &type->fields[i];
    }
    return 0;
}

I_REFLECT_INLINE const i_reflect_field *i_reflect_next_field_with_kind(const i_reflect_type *type, i_reflect_type_kind kind, const i_reflect_field *after) {
    if (!type) return 0;
    u64 start = 0;
    if (after) {
        for (u64 i = 0; i < type->field_count; i++) {
            if (&type->fields[i] == after) { start = i + 1; break; }
        }
    }
    for (u64 i = start; i < type->field_count; i++) {
        if (type->fields[i].kind == kind) return &type->fields[i];
    }
    return 0;
}

I_REFLECT_INLINE int i_reflect_cstr_equal(const char *a, const char *b) {
    if (!a || !b) return 0;
    while (*a && *b && *a == *b) { a++; b++; }
    return *a == 0 && *b == 0;
}

I_REFLECT_INLINE const i_reflect_field *i_reflect_find_field(const i_reflect_type *type, const char *name) {
    if (!type || !name) return 0;
    for (u64 i = 0; i < type->field_count; i++) {
        if (i_reflect_cstr_equal(type->fields[i].name, name)) return &type->fields[i];
    }
    return 0;
}

I_REFLECT_INLINE u64 i_reflect_field_index(const i_reflect_type *type, const i_reflect_field *field, u64 fallback) {
    if (!type || !field) return fallback;
    for (u64 i = 0; i < type->field_count; i++) {
        if (&type->fields[i] == field) return i;
    }
    return fallback;
}

I_REFLECT_INLINE u64 i_reflect_find_field_index(const i_reflect_type *type, const char *name, u64 fallback) {
    const i_reflect_field *field = i_reflect_find_field(type, name);
    return i_reflect_field_index(type, field, fallback);
}

I_REFLECT_INLINE const i_reflect_field *i_reflect_field_at(const i_reflect_type *type, u64 index) {
    if (!type || index >= type->field_count) return 0;
    return &type->fields[index];
}

I_REFLECT_INLINE const i_reflect_field *i_reflect_find_field_by_offset(const i_reflect_type *type, u64 offset) {
    if (!type) return 0;
    for (u64 i = 0; i < type->field_count; i++) {
        if (type->fields[i].offset == offset) return &type->fields[i];
    }
    return 0;
}

I_REFLECT_INLINE u64 i_reflect_field_end_offset(const i_reflect_field *field) {
    if (!field) return 0;
    return field->offset + field->size;
}

I_REFLECT_INLINE const i_reflect_field *i_reflect_find_field_containing_offset(const i_reflect_type *type, u64 offset) {
    if (!type) return 0;
    for (u64 i = 0; i < type->field_count; i++) {
        u64 start = type->fields[i].offset;
        u64 end = i_reflect_field_end_offset(&type->fields[i]);
        if (start <= offset && offset < end) return &type->fields[i];
    }
    return 0;
}

I_REFLECT_INLINE void *i_reflect_field_ptr(void *base, const i_reflect_field *field) {
    if (!base || !field) return 0;
    return (void *)((unsigned char *)base + field->offset);
}

I_REFLECT_INLINE const void *i_reflect_field_const_ptr(const void *base, const i_reflect_field *field) {
    if (!base || !field) return 0;
    return (const void *)((const unsigned char *)base + field->offset);
}

I_REFLECT_INLINE int i_reflect_field_copy(void *dst_base, const void *src_base, const i_reflect_field *field) {
    void *dst = i_reflect_field_ptr(dst_base, field);
    const void *src = i_reflect_field_const_ptr(src_base, field);
    if (!dst || !src) return 0;
    memmove(dst, src, (size_t)field->size);
    return 1;
}

I_REFLECT_INLINE int i_reflect_field_zero(void *base, const i_reflect_field *field) {
    void *dst = i_reflect_field_ptr(base, field);
    if (!dst) return 0;
    memset(dst, 0, (size_t)field->size);
    return 1;
}

I_REFLECT_INLINE int i_reflect_field_copy_by_name(void *dst_base, const void *src_base, const i_reflect_type *type, const char *name) {
    return i_reflect_field_copy(dst_base, src_base, i_reflect_find_field(type, name));
}

I_REFLECT_INLINE int i_reflect_field_zero_by_name(void *base, const i_reflect_type *type, const char *name) {
    return i_reflect_field_zero(base, i_reflect_find_field(type, name));
}

I_REFLECT_INLINE int i_reflect_attr_is_sep(char c) {
    return c == 0 || c == ',' || c == ' ' || c == '\t' || c == '\r' || c == '\n';
}

I_REFLECT_INLINE int i_reflect_field_has_attr(const i_reflect_field *field, const char *attr) {
    if (!field || !field->attrs || !attr || !attr[0]) return 0;
    const char *scan = field->attrs;
    while (*scan) {
        while (*scan == ',' || *scan == ' ' || *scan == '\t' || *scan == '\r' || *scan == '\n') scan++;
        const char *token = scan;
        while (*scan && !i_reflect_attr_is_sep(*scan)) scan++;
        const char *a = token;
        const char *b = attr;
        while (a < scan && *b && *a == *b) { a++; b++; }
        if (a == scan && *b == 0) return 1;
    }
    return 0;
}

I_REFLECT_INLINE u64 i_reflect_count_fields_with_attr(const i_reflect_type *type, const char *attr) {
    if (!type || !attr || !attr[0]) return 0;
    u64 count = 0;
    for (u64 i = 0; i < type->field_count; i++) {
        if (i_reflect_field_has_attr(&type->fields[i], attr)) count++;
    }
    return count;
}

I_REFLECT_INLINE const i_reflect_field *i_reflect_find_field_with_attr(const i_reflect_type *type, const char *attr) {
    if (!type || !attr || !attr[0]) return 0;
    for (u64 i = 0; i < type->field_count; i++) {
        if (i_reflect_field_has_attr(&type->fields[i], attr)) return &type->fields[i];
    }
    return 0;
}

I_REFLECT_INLINE const i_reflect_field *i_reflect_next_field_with_attr(const i_reflect_type *type, const char *attr, const i_reflect_field *after) {
    if (!type || !attr || !attr[0]) return 0;
    u64 start = 0;
    if (after) {
        for (u64 i = 0; i < type->field_count; i++) {
            if (&type->fields[i] == after) { start = i + 1; break; }
        }
    }
    for (u64 i = start; i < type->field_count; i++) {
        if (i_reflect_field_has_attr(&type->fields[i], attr)) return &type->fields[i];
    }
    return 0;
}

I_REFLECT_INLINE const i_reflect_enum_value *i_reflect_find_enum_value_by_name(const i_reflect_enum *type, const char *name) {
    if (!type || !name) return 0;
    for (u64 i = 0; i < type->value_count; i++) {
        if (i_reflect_cstr_equal(type->values[i].name, name)) return &type->values[i];
    }
    return 0;
}

I_REFLECT_INLINE const i_reflect_enum_value *i_reflect_find_enum_value_by_value(const i_reflect_enum *type, i32 value) {
    if (!type) return 0;
    for (u64 i = 0; i < type->value_count; i++) {
        if (type->values[i].value == value) return &type->values[i];
    }
    return 0;
}

I_REFLECT_INLINE const i_reflect_enum_value *i_reflect_enum_value_at(const i_reflect_enum *type, u64 index) {
    if (!type || index >= type->value_count) return 0;
    return &type->values[index];
}

I_REFLECT_INLINE const char *i_reflect_enum_name_from_value(const i_reflect_enum *type, i32 value) {
    const i_reflect_enum_value *found = i_reflect_find_enum_value_by_value(type, value);
    return found ? found->name : 0;
}

I_REFLECT_INLINE i32 i_reflect_enum_value_from_name(const i_reflect_enum *type, const char *name, i32 fallback) {
    const i_reflect_enum_value *found = i_reflect_find_enum_value_by_name(type, name);
    return found ? found->value : fallback;
}

#undef I_REFLECT_INLINE

#endif
