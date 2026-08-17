# Reflection Issues

Problems with reflection as it stands. Separate from the module-system work in
`shape.md` and `stranger-with-generics.md` — none of these are caused by, or
blocked on, how modules are lowered.

Reflection is one of I's two distinguishing features and the reason the resource
layer in a real engine is better than its C original: 131 uses of
`<>.value_count`, 61 of `<>.&`, and a metadata generator a third smaller than
the C one it replaced. So these are notes on sharpening something that works,
not a case against it.

## What Works

Worth stating first, so none of it gets broken while fixing the rest.

- The header/source split is already correct: `extern const i_reflect_type
  Point_reflect;` in the header, the definition in the `.c`.
- It works through monomorphisation — `Array_i32_reflect` is generated
  correctly for each instantiation.
- Field records are richly decomposed: `kind` (`Name`/`Ptr`/`Generic`/`Array`/
  `Proc`), `offset`, `size`, `align`, `pointer_depth`, `array_count`,
  `base_type`, `elem_type`, `generic_arg_type`, `is_const`.
- It costs nothing at runtime. The tables are `static const` data.
- `std/reflect.h` ships useful helpers: `i_reflect_find_field`,
  `i_reflect_find_field_by_offset`, `i_reflect_find_field_with_kind`.

## 1. Every Reflection Access Is Unchecked

The reflect types are declared `external` on the I side:

    i_reflect_field: struct = { external; }
    i_reflect_type: struct = { external; }
    i_reflect_enum_value: struct = { external; }
    i_reflect_enum: struct = { external; }

An `external` struct accepts **any** field name with no verification — the
access passes straight through to C. So `meta[0].value_count` compiles because
C will resolve it later, not because I checked that the field exists.

That means every reflection accessor in every program rides the soundness hole
recorded in `shape.md` §3. A typo in a field name is not a diagnostic; it is a C
error pointing at generated code, or worse, a silent match against a different
field that happens to exist.

This is the most consequential item here, because it applies to a core language
feature rather than to an interop corner.

**Resolution.** Either give `external` structs a declared, checked field list —
the fix already proposed in `shape.md` — or make the reflect types known to the
compiler directly, since it generates them and knows their shape exactly.

## 2. Reflection Is Only One Level Deep

A field's type is recorded as a **mangled string**, not a link:

    Outer: struct = { inner: Inner; ptr: *Inner; arr: [4]i32; }

    Outer<>.fields[0].type  ->  "Inner"
    Outer<>.fields[1].type  ->  "ptr_Inner"
    Outer<>.fields[2].type  ->  "array_4_i32"

There is no way to get from `"Inner"` to `Inner`'s reflect record. The header
`std/reflect.h` has helpers to find a *field within a known type*, but
**nothing maps a type name to its record** — no registry, no
`i_reflect_find_type`.

So reflection can walk one struct's fields and cannot recurse into them. That
blocks the single most common thing reflection is wanted for: a generic
serialiser, a debug inspector that expands nested structs, a UI that edits a
struct tree. In a real engine this is exactly the wall you hit after the first
afternoon.

**Resolution.** Add a pointer to the nested `i_reflect_type` in
`i_reflect_field` where the field is a struct or enum, or emit a program-wide
registry the runtime can search by name. The first is cheaper to consume and
costs a pointer per field; the second is more flexible and costs a linear scan.

## 3. A Union Is Indistinguishable From A Struct

Unions reflect, and report like structs:

    U: union = { i: i32; f: f32; }
    U<>.field_count   ->  2

`i_reflect_type` has no kind flag, so nothing tells a consumer that these fields
**overlap**. A serialiser walking `U` would write both members as though they
were adjacent, producing output that is silently wrong rather than failing.

Every field would report `offset == 0`, which is the only available hint, and
inferring "union" from repeated zero offsets is guesswork — a struct whose first
field is at offset zero looks the same at index 0.

**Resolution.** A kind flag on `i_reflect_type` distinguishing struct from union.
One field, and it closes a silent-wrong-output hole.

## 4. Tables Are Emitted Unconditionally

Every struct and enum gets a reflect table whether or not anything reflects it:

    Unused: struct = { z: i32; }   // never reflected
    // Unused_reflect is emitted anyway

In the njinn engine that is **152 tables, about 304 lines of the 28,301
generated** — roughly 1%. Not a crisis, and it is `const` data a linker can
often drop. But it is unconditional, with no way to opt a type out.

**Resolution.** Three options, in increasing effort: leave it and document that
reflection data is always present; emit only for types actually reflected, which
needs a whole-program use analysis the compiler is already positioned to do; or
add an opt-in marker on the type. The middle option is the most likely to be
worth it, since the analysis pass is the same one the module work would need.

## 5. Enum Values Are `i32`, Consumers Take `i64`

    i_reflect_enum_value: { const char *name; i32 value; }

Every consumer signature widens:

    gin_reflect_enum_name: proc(meta: *const i_reflect_enum, value: i64)->*const char

so each call site carries `cast(action, i64)`. Harmless, but it means an enum
value outside `i32` range cannot round-trip through reflection — which connects
directly to the unresolved enum underlying type in `shape.md` §2.6. Whatever is
decided there should decide this too.

## 6. Two Spellings For The Accessor

`Type<>.value_count` reads a field directly; `Type<>.&` produces the address to
pass along. Both are in use and the relationship between them is not obvious
from the syntax — the first looks like member access on a value, the second like
taking the address of something that was never named.

Cosmetic, but this is the surface a student meets first, so it is worth deciding
whether the asymmetry is intended.

## Suggested Order

1. **§3, the union flag.** One field, closes a silent-wrong-output path.
2. **§2, nested type links.** This is what unblocks reflection actually being
   used for the thing it exists for.
3. **§1, checked access.** Larger, and tied to the `external` decision in
   `shape.md`; but it is the one that makes reflection trustworthy rather than
   merely functional.
4. **§5, the integer width** — decide alongside the enum underlying type.
5. **§4 and §6** whenever convenient.

Each of these should get a discriminating test in the execute suite before it is
called done, per `compiler-hardening.md`. `017-generics-and-reflection.i` is the
place they belong.
