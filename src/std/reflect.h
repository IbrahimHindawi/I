#pragma once

#include <core.h>
#include <stddef.h>
#include <string.h>

#ifndef I_REFLECT_TYPES_DEFINED
#define I_REFLECT_TYPES_DEFINED

/* Every name here carries an `i_` prefix because this header's contents land in
   the C global namespace of every I program -- reflection tables are emitted
   unconditionally. `reflect` unprefixed is a plausible identifier for
   third-party C to claim: it is a GLSL builtin, and any vector-math library an
   engine links is fair game.

   The prefix is `i_`, not `__i_`: C reserves every identifier beginning with two
   underscores, or an underscore followed by an uppercase letter, to the
   implementation for any use. Buying namespace hygiene with undefined behaviour
   would contradict the one correctness claim this backend makes.

   I source keeps the short spelling -- `reflect`, `reflect_fields` -- and the
   compiler maps those onto the names below when it emits C. See
   reflect_runtime_c_name() in src/main.c, and std/reflect.i for the I side. */

/* One record describes every reflected type. `kind` says which, and `variant`
   holds the payload that only that kind has. This replaces the older split
   between the old separate type and enum records, which forced every consumer to
   know in advance which of two unrelated records it was going to be handed.

   The layout here must match std/reflect.i exactly. Both `kind` fields are
   spelled i32 rather than as C enums on purpose: C leaves an enum's underlying
   type implementation-defined, so a C enum here would make the struct's layout
   depend on the compiler while the I side mirrors it as a fixed i32. */

typedef struct i_reflect i_reflect;

/* How a field's type is written, not what the type is. */
typedef enum i_reflect_type_kind {
    I_Reflect_Type_Name,
    I_Reflect_Type_Ptr,
    I_Reflect_Type_Generic,
    I_Reflect_Type_Array,
    I_Reflect_Type_Proc,
} i_reflect_type_kind;

/* What a reflected type is. Struct and union are separate kinds rather than one
   kind plus a flag, so a consumer that handles only structs cannot silently
   treat a union's overlapping members as if they were adjacent. */
typedef enum i_reflect_kind {
    I_Reflect_Struct,
    I_Reflect_Union,
    I_Reflect_Enum,
} i_reflect_kind;

typedef struct i_reflect_field {
    const char *name;
    const char *type;
    const char *attrs;
    u64 offset;
    u64 size;
    u64 align;
    i32 kind; /* i_reflect_type_kind */
    u64 array_count;
    u64 pointer_depth;
    const char *base_type;
    const char *elem_type;
    const char *generic_arg_type;
    u64 is_const;
    /* The record for this field's own type, so reflection can recurse. Null when
       the field is a builtin, an external type, or anything with no table. For a
       pointer or array it is the record of the element type. */
    const i_reflect *info;
} i_reflect_field;

typedef struct i_reflect_value {
    const char *name;
    /* i32, matching C: an unadorned C enum's members must fit in int, and I
       permits negative members, so a signed 32-bit value carries all of them. */
    i32 value;
} i_reflect_value;

typedef union i_reflect_variant {
    const i_reflect_field *fields; /* I_Reflect_Struct, I_Reflect_Union */
    const i_reflect_value *values; /* I_Reflect_Enum */
} i_reflect_variant;

struct i_reflect {
    const char *name;
    u64 size;
    u64 align;
    i32 kind; /* i_reflect_kind */
    u64 count; /* fields for a struct or union, values for an enum */
    i_reflect_variant variant;
};

/* The helpers that used to live here -- i_reflect_fields, i_reflect_find_field,
   and the rest -- are now written in I, in std/reflect.i. They were pure logic
   over the data below, so a C header was the wrong home: there they could not be
   type-checked, could not be read in the language that uses them, and had to be
   mirrored by a hand-written set of `external` declarations that nothing kept in
   step.

   What stays here is what C has to own: the record layouts, because the compiler
   emits its reflection tables as C initialisers of these types. */


#endif
