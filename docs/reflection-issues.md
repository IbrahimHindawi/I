# Reflection Issues

Problems with reflection as it stands. Separate from the module-system work in
`shape.md` and `stranger-with-generics.md` — none of these are caused by, or
blocked on, how modules are lowered.

Reflection is one of I's two distinguishing features and the reason the resource
layer in a real engine is better than its C original: 153 uses of `<>.count`, 62
of `<>.&`, and a metadata generator a third smaller than the C one it replaced.
So these are notes on sharpening something that works, not a case against it.

## Settled

Recorded so they do not get re-litigated. Every item here has a discriminating,
mutation-checked test — the mutation is named in each entry, and reverting the
fix makes the named test fail.

### One record, kind-tagged, variant payload

`i_reflect_type` and `i_reflect_enum` are gone. One `reflect` record describes
every reflected type; `kind` says which, and `variant` holds the payload only
that kind has. The whole family dropped its `i_` prefix.

```
reflect_variant: union = {
    fields: *const reflect_field;   // Struct, Union
    values: *const reflect_value;   // Enum
}

reflect: struct = {
    name: *const char;
    size: u64;
    align: u64;
    kind: i32;      // reflect_kind_struct | _union | _enum
    count: u64;     // fields for a struct or union, values for an enum
    variant: reflect_variant;
}
```

Every language surveyed had converged on this: Go, C#, Java, Zig, Odin, C++26,
Rust. Of the two closest, **Odin's** shape is the one that ports — a common
header plus a `variant` union plus a kind discriminator, expressible in I today.
**Zig's** `union(enum)` is a language-level tagged union you `switch` on; I has
plain C unions only, and an inline `variant: union = {...}` does not even parse,
so adopting Zig's shape would mean building tagged unions into the language
first. That is a separate decision, not a prerequisite for this one.

**One deliberate deviation from Odin:** `count` is hoisted into the header rather
than living in the variant. Odin keeps it in the variant. Hoisting turned the
single largest migration cost — 151 uses of `value_count` against 0 of
`field_count` — into a one-word rename instead of a restructure.

### Nested type links (was §2)

`reflect_field` gained `info: *const reflect`, the record for the field's own
type. It resolves through a plain name, a pointer, and an array; it is null for
builtins, external types and procs; and it links a self-referential type back to
itself. This is what makes a recursive walk — a serialiser, a tree inspector —
writable at all. Before it, a walk stopped at the mangled type-name string with
no way from `"Inner"` to `Inner`'s record.

Because a table can be defined later in a file than one linking to it, every
table is now forward-declared. In module mode the headers already carried these,
so cross-module links work unchanged: njinn emits 94 of them.

*Mutation: suppress every link (`info` always `0`) — `017-generics-and-reflection`
fails on `recursive_field_sum`.*

### A union is its own kind (was §3)

Struct and union are distinct `kind` values rather than one kind plus a flag. A
consumer that only handles structs therefore cannot silently walk a union's
overlapping members as though they were adjacent — the case that produced output
that was wrong rather than absent.

*Mutation: report unions as `Reflect_Struct` — `017-generics-and-reflection`
fails on `kind_tags`.*

### Reflection access is checked (was §1)

`std/reflect.i` declares the fields alongside `external`, which opts the types
back into checking. `meta[0].value_kount` is now a type error at the access site
rather than a C error in generated code, or a silent match against a different
field. This also made the migration compiler-guided: every stale `value_count`
reported its own file and line.

### Enum values stay `i32` (was §5)

I permits negative enum members and reflection round-trips them correctly today.
A `u32` field would turn `None = -1` into `4294967295`, after which every lookup
by value misses a member that plainly exists — the silent wrong-value class this
project's torture suite exists to catch. The reverse risk does not balance it:
`u32` only buys values above 2³¹, which nothing has, and an unadorned C enum
could not hold one anyway.

This decision does not depend on how `shape.md` §2.6 resolves the enum
*underlying type*, because `i32` holds every value a C enum can legally have.

*Covered by `017-generics-and-reflection`: `Slot { Empty = -1 }` prints `-1`.*

## 1. The Merge Traded A Type Error For A Runtime Check

This is the cost of the collapse and it should be written down rather than
discovered.

Before, `*const i_reflect_enum` and `*const i_reflect_type` were different types,
so handing a struct's table to an enum consumer was a compile error. Now both are
`*const reflect` and the type system permits it. Reading `variant.values` on a
struct reinterprets the fields pointer.

The mitigation is in `std/reflect.h`: `reflect_fields()` and `reflect_values()`
return the arm only when `kind` matches, and null otherwise. njinn was migrated
to route every arm read through them, so a struct handed to
`gin_reflect_enum_name` reports `"unknown"` instead of garbage.

*Mutation: read the arm unchecked in `gin_reflect_enum_name` —
`resops_reflect_selftest` fails with `returned '', want 'unknown'`.*

**Open question.** Whether the compiler should enforce this rather than relying on
consumers using the right helper. A `reflect` whose `kind` is known statically —
which it always is at a `Type<>` site — could in principle reject
`Point<>.variant.values` outright. That would recover the compile-time error
without giving up the single record. Not implemented; worth deciding before much
more code is written against the variant.

## 2. Tables Are Emitted Unconditionally

Every struct, union and enum gets a table whether or not anything reflects it:

    Unused: struct = { z: i32; }   // never reflected
    // Unused_reflect is emitted anyway

In the njinn engine that is roughly 1% of generated lines. Not a crisis, and it
is `const` data a linker can often drop. But it is unconditional, with no way to
opt a type out — and the nested `info` links now make the set of live tables
harder to compute, since a table can be reachable only through another type's
field.

**Resolution.** Leave it and document that reflection data is always present;
emit only for types actually reflected, which needs a whole-program use analysis
including link reachability; or add an opt-in marker on the type. The middle
option is the most likely to be worth it, since the analysis pass is the same one
the module work would need.

## 3. Two Spellings For The Accessor

`Type<>.count` reads a field directly; `Type<>.&` produces the address to pass
along. Both are in use and the relationship between them is not obvious from the
syntax — the first looks like member access on a value, the second like taking
the address of something that was never named.

Cosmetic, but this is the surface a student meets first, so it is worth deciding
whether the asymmetry is intended.

## 4. `reflect` Occupies A Common Word In Every Program's C Namespace

The generated C defines `struct reflect`, `reflect_field`, `reflect_value` and
`reflect_variant` in every translation unit, since tables are emitted
unconditionally. `reflect` is a plausible identifier for third-party C to use.

Nothing has collided yet. If it does, the fix is to keep the C-side symbols
prefixed and map the I-side spelling onto them, which is a change to one emitter
and one header rather than to any user code.

## Suggested Order

1. **§1's open question** — whether static `kind` knowledge should make the wrong
   arm a compile error. This is the one place the merge is currently weaker than
   what it replaced.
2. **§2, unconditional tables** — the whole-program analysis overlaps the module
   work.
3. **§3 and §4** whenever convenient.

Each of these should get a discriminating test in the execute suite before it is
called done, per `compiler-hardening.md`. `017-generics-and-reflection.i` is the
place they belong, and every item under "Settled" above already has one.
