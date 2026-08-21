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
that kind has. In I the family is spelled without a prefix; in the emitted
C it keeps one, for the namespace reasons under "The C names carry an `i_`
prefix" below.

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

That was only conditionally true at first, and the condition was the wrong way
round: the diagnostic fired only for a type the program had *declared*, so it
required importing `std/reflect.i` — while `Type<>` needs no import at all. Code
that never imported it, which is the ordinary case, went unchecked. Found when
the i-learn lessons kept compiling against field names that no longer exist. The
reflect record's field set is compiler knowledge, not something the user
supplies, so the check no longer depends on the declaration being in scope.

*Mutation: drop `type_is_reflect_runtime_record` from the field-access branch —
`type_reflect_no_import` fails.*

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

### The wrong variant arm is a diagnostic again

Collapsing the records cost a type error. Before, `*const i_reflect_enum` and
`*const i_reflect_type` were different types, so handing a struct's table to an
enum consumer could not be written. Afterwards both were `*const reflect`, and
`Point<>.variant.values` compiled — and did not fail loudly, because both arms
begin with a `const char *name`: it reinterpreted the fields pointer and printed
`"x"`, a plausible wrong answer.

That is closed in three places:

- **Statically**, wherever the kind is known. `Type<>` names its owner, so the
  compiler resolves that owner's kind and rejects the arm which is not live. It
  stays silent where it cannot tell — a monomorphised table with no decl under
  that name, or any base that is not a `Type<>` chain — so it fires only when
  it is certain.
- **At run time**, for a `*const reflect` that was passed in and carries no
  static kind. `reflect_fields()` and `reflect_values()` in `std/reflect.h`
  return the arm only when `kind` matches, null otherwise. njinn routes every
  arm read through them.
- **In the editor**, completion after `Type<>.variant.` offers only the live arm,
  so the error is hard to reach by accident rather than merely recoverable once
  hit. The diagnostic also carries its fix as a note over JSON, not only in the
  terminal.

*Mutations: drop the static check — `type_reflect_variant_arm` fails; read the
arm unchecked in `gin_reflect_enum_name` — `resops_reflect_selftest` fails with
`returned '', want 'unknown'`; offer both arms in completion — `lsp_semantics`
fails.*

### Reflection data is always emitted — nothing is elided

Every struct, union and enum gets a table whether or not anything reflects it.
Measured in njinn: **155 tables over 1,737 of 29,542 generated lines (5.9%)**, and
about **558 bytes of binary per table**, so roughly 200 KB in total.

Two beliefs about this turned out to be wrong and are worth recording so they are
not repeated. The C compiler cannot eliminate an unused table: `const i_reflect
Point_reflect` has external linkage, so any single translation unit must assume
another references it. And the linker does not pick up the slack either —
measured, an unreferenced table is still in the binary at `-O0` **and** `-O2`.

Kept anyway, and not because it is free. The alternative is whole-program use
analysis, and reachability now runs through the nested `info` links, so a table
can be live only because another type's field points at it. Getting that wrong
means reflection silently missing at run time, which is the exact failure class
this project exists to prevent. Trading a correctness risk for 200 KB in a
program that ships textures and audio is a bad trade.

If size ever does matter there is a cheap escape that needs no compiler work:
`-fdata-sections` plus `/OPT:REF` lets the linker collect them. A build-flag
change, not a language change.

### The accessor pair is not an asymmetry — retracted

An earlier entry here claimed `Type<>.count` and `Type<>.&` were two
inconsistent spellings and a wart for newcomers. That was wrong, and it came from
pattern-matching "two spellings" without checking they were the language's
ordinary two.

`.&` is I's universal address-of postfix — `g_fx.&`, `desc.&`, `mapped.&` are
everywhere in real code. `.count` is universal member access. `Type<>` yields a
value, and both accessors then behave exactly as they do on any other value.
There is nothing special to learn.

The one genuine novelty is `<>` itself, and once it is known everything
downstream follows the normal rules. No change made, and none wanted.

### The C names carry an `i_` prefix

Reflection tables are emitted unconditionally, so `std/reflect.h` puts its
contents into the C global namespace of every I program. `reflect` unprefixed is
a plausible identifier for third-party C to claim — it is a GLSL builtin, and
any vector-math library an engine links is fair game. Nothing collided in
njinn's whole vendor set (cgltf, stb, miniaudio, jsmn, cglm, D3D11), but doing
this before third parties depend on the spelling is far cheaper than after.

So the C side is `i_reflect`, `i_reflect_field`, `i_reflect_fields()`,
`I_Reflect_Struct`, and so on, while **I source keeps the short spelling**. The
compiler maps between them when it emits.

Not `__i_`. C reserves every identifier beginning with two underscores, or an
underscore followed by an uppercase letter, to the implementation for any use.
Buying namespace hygiene with undefined behaviour would contradict the one
correctness claim the backend actually makes.

The mapping is a **closed list of 55 names**, not a "starts with `reflect`" rule.
A blanket rule would silently rewrite a user's own
`reflect_normal: proc = { external; }` into `i_reflect_normal` and break the link
against the C function they meant to bind — the language must never rename
someone's external bindings based on a prefix. A closed list can fall behind the
headers instead, so `reflect_runtime_names` re-derives it from `std/reflect.h`
and `std/reflect.i` on every run and fails on any difference in either
direction.

*Mutation: drop one name from the table — `reflect_runtime_names` fails with
"declared but not mapped".*

### The helpers are written in I, not bound from C

`std/reflect.h` used to define 41 `static inline` helpers, and `std/reflect.i`
mirrored a subset of them as `external` declarations. They are all pure logic
— null checks, kind compares, loops over the arrays the compiler emitted, one
small attribute tokeniser — so a C header was the wrong home for them. They are
now ordinary I procs in `std/reflect.i`, and the header holds record layouts
only.

Three things this bought. The hand-written `external` declarations are gone, and
with them the chance of a signature drifting from its definition with nothing to
catch it. The helpers are type-checked. And a reader who wants to know what
`reflect_fields` does can open the file and see it, in the language that uses it
— which matters most for i-learn, where the lesson is about reflection being
ordinary data and the tool that reads it was hidden in C.

C still owns the record layouts, and has to: the compiler emits its tables as C
initialisers of those types, so a definition on the I side would collide with the
header's.

*Covered by `enum_reflect_preprocessor`, which asserts on the results of all 41
helpers and passes unchanged against the I implementations; and by
`reflect_runtime_names`, which now also fails if the header grows a helper back.*

## Immutability -- settled, and it needed nothing

Reflection tables are compiler-generated and must never be mutated. Measured
against the current compiler, that is already enforced three ways over:

- **Deep `const` in the emitted C**: `static const i_reflect_value[]`,
  `static const i_reflect_field[]`, `const i_reflect`, `extern const i_reflect`,
  with every interior pointer const-qualified in `reflect.h`.
- **`.rdata`**: `llvm-nm` reports the tables as `R`/`r`. A program that forces a
  write through segfaults (exit 139).
- **A mutable `*reflect` is unconstructible**: `p: *reflect = Point<>.&` is a
  type error -- *expected `ptr_reflect`, got `ptr_const_reflect`*.

A magic lowering of `*reflect` to `*const reflect`, to save writing `const`, was
proposed and rejected. The short version: that type error *is* the enforcement,
so the magic would delete the check it was meant to strengthen. It would also
make `*T` mean different things for different `T` under `substitute_type_sub`,
and the tedium it saves is 11 spellings across all of njinn -- 75 of the 96 in
the tree are in `src/std/reflect.i`, one file written once. Full reasoning in
`shape.md` 9.2.

**The one real hole is not a reflection hole.** `cast` launders `const` away for
every type, silently:

    p: *reflect = cast(Point<>.&, *reflect);
    p[0].size = 999;                          // accepted

Tracked as `shape.md` 9.1, alongside the general fix (`cast` from `*const T` to
`*T` should be an error) and the general answer to the ergonomic complaint
(type aliases, 9.3). Nothing here needs a reflection-specific change.

*Wants a test asserting the `.rdata` placement, so it cannot silently regress
into `.data` if the emitter changes.*

## Nothing Open

Every item this document opened has been settled. The natural next questions, if
reflection is picked up again, are the ones it never raised: whether reflect
tables should be reachable by name at run time (there is still no
`i_reflect_find_type`, so a walk can recurse through `info` but cannot look a
type up from a string), and whether `<>` should extend to anything beyond
structs, unions and enums.

Immutability was raised and closed above without a language change; the general
`cast`/`const` hole it exposed lives in `shape.md` 9.1, not here.

Each would need a discriminating test in the execute suite before being called
done, per `compiler-hardening.md`. `017-generics-and-reflection.i` is where they
belong, and every item under "Settled" above already has one.
