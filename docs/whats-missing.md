# What Is Missing

A survey of the language and compiler, measured against njinn rather than
against a wish list. Everything here was probed against the compiler as it
stands; the numbers come from njinn's source.

## Where it already stands up

Worth stating first, because it changes what the gaps cost.

- **318 discriminating checks**, mutation-tested. The suite fails when a
  lowering is deliberately broken, which is the only property that means
  anything.
- **481 ms to compile njinn** -- 28k lines of ilang into 60k lines of C.
- **Order-independent declarations.** A type or proc may be used before it is
  declared. No forward declarations, no header discipline.
- **Nominal typing where it counts.** Structs do not interconvert, pointers do
  not interconvert without a cast, `const` is tracked through the type and
  through argument passing, arity is checked, and two different enums cannot be
  compared.
- **Reflection with verified layout** -- 952 `_Static_assert`s comparing
  declared external records against the real C ones.
- **Diagnostics report more than the first error** and map back to the `.i`
  line through `#line`.

## 1. Error handling, and discarded results

### The measurement

    268 procs return b32
    call sites that use the result:  665
    call sites that discard it:      145   (18%)

Including `dx11ops_create_bloom_targets(dx);` -- a GPU resource creation whose
failure signal goes nowhere. Some of those 145 are deliberate fire-and-forget.
Some are bugs. Nothing distinguishes them, and nothing will without a **must-use**
notion: a proc marked such that discarding its value is a diagnostic.

That much is a plain gap, independent of anything below, and it sharpens the
error model already in use rather than proposing a different one. The marker can
be an attribute (`proc[must_use]`), which the slot already supports.

### Error handling: decided

**A status enum per domain, the value through an out-parameter, the message from
reflection.** `Result<T>` is not the direction.

    mesh_status: enum = { success, file_not_found, bad_magic, truncated, }

    mesh_load: proc(mesh: *geo_mesh) -> mesh_status { ... }

    e: mesh_status = mesh_load(mesh.&);
    if (e != mesh_status.success) {
        printfmt("load failed: {}\n",
                 reflect_name_from_value_or(e<>.&, cast(e, i32), "?"));
        return -1;
    }

What it gets right:

- **The enums are nominally typed.** `mesh_status` and `file_status` cannot be
  compared -- the compiler rejects it. Better than C's `int` codes and better
  than the `b32` used 303 times in njinn, where every failure looks alike.
- **The message is derived, not written.** Reflection produces the member name,
  so adding `truncated` makes `"truncated"` print with nothing else to update.
  Every C codebase has a hand-written `switch (err)` returning strings that
  goes stale; this one cannot.
- **It costs nothing.** It is an int, and it compiles to an int.
- **The out-parameter avoids a copy.** `Result<geo_mesh, E>` returns the mesh by
  value, and ilang has no moves, so that is a copy of everything in it.

Two pieces of the pattern live in the language rather than in each project.
`reflect_name_from_value_or` is in `std/reflect.i` -- the plain version returns
null when nothing matches, which is right for code that tests the result and
wrong for the common use, since passing null to a `%s` is undefined. And `<>`
works on a *value*, not only a type, so the status variable names its own
record. Covered by `status_enum_errors` and `reflect_of_value`.

### Why not `Result<T, E>`

Worth writing down, because the obvious argument for it does not survive
contact.

`Result` does *not* remove cross-domain conversion. Rust's `?` requires
`impl From<FileError> for MeshError` -- you still write the mapping, `?` just
calls it. The gain is that it is declared once per pair rather than spelled at
each call site. Real, but modest.

What actually makes errors compose in Rust is **payload-carrying enums**
(`enum MeshError { File(FileError), BadMagic }`), which is a separate feature
from `Result`. And ilang can already express that by hand -- a struct with a
tag field and a payload:

    mesh_status: struct = {
        kind: mesh_kind;
        file: file_status;   // meaningful when kind == from_file
    }

More verbose, and nothing checks that `file` is read only when `kind` says so,
but it works today with no new feature.

So `Result` would buy three things, and two of them are not `Result`: value and
error travelling together (needs exhaustive matching to enforce), composition in
expressions (needs a propagation form), and forced handling (needs must-use).
Without those it is a struct whose value can be read after a failure -- which is
what `std/Result.i` is, and why it has three uses.

### What would strengthen the decided design

**Enum exhaustiveness in `switch`** -- item 1 in the order below, and now with a
concrete motivation rather than an abstract one. Handle a `mesh_status` with a
`switch`, add `truncated` to the enum, and every handler that does not cover it
fails to compile. That is the property people actually want from `Result` plus
`match`, on a plain enum, with no generics, no propagation operator and no
copies.

## 2. No `defer`

Manual memory, 199 abort sites, and no scoped cleanup. Arenas carry most of the
weight in njinn, which is why this has not bitten harder -- but every early
return that owns a resource is a hand-written unwind.

This is the single most commonly cited C ergonomic fix and both Go and Zig have
it. It is also cheap: `defer` lowers to statements emitted at each scope exit,
and ilang already computes those points for `break`/`continue` checking.

## 3. Naming: the module prefix tax

**1036 of njinn's 1130 top-level procs (91%) carry a hand-written module
prefix**, across 46 distinct prefixes -- `gin_` (205), `gops_` (201), `fxops_`
(106), `fxed_` (71), `guiops_` (59), `resops_` (52).

That is C-era bookkeeping the language could own, enforced by nothing but
habit.

**Methods are permanently out of scope.** Decided, not deferred: ilang will not
grow `P.get: proc(self: *P)` or any other form of a proc bound to a type. This
is recorded so it is not proposed again.

Which leaves **module namespaces** (§10 of `shape.md`, parked) as the only
lever on that 91%. Worth saying plainly, because it changes what §10 is: not a
cosmetic preference between `mem.arena` and `mem_arena`, but the sole remaining
answer to a tax every declaration in the codebase pays.

## 4. Missing loop and return forms

- **No `for (v in xs)`.** njinn has **238 index-style loops**. Iteration over an
  array whose length is in its type is decidable and mechanical.
- **No multiple return values.** Anything returning both a value and a status
  uses an out-parameter -- which is the decided error-handling shape (§1), so
  this is less a gap than it first looks. It still bites for a genuine pair of
  results that are not value-plus-status.

## 5. Cheap correctness checks that do not exist

Each of these is accepted today, and each is a known bug source:

| accepted | note |
|---|---|
| `switch` over an enum missing cases | **the valuable one** -- enums are already nominally typed, so the machinery to know the type exists |
| an unused local | clang warns at `-Wall`; ilang says nothing |
| a discarded return value | no must-use of any kind |
| assigning to a parameter | shadows the caller's intent silently |
| unreachable code after `return` | |
| `sizeof` on an opaque `external` | ilang accepts, clang then reports the name as undeclared |

Enum exhaustiveness is the one worth doing first. njinn has 34 enums, the type
of a switch subject is already known, and a missing case is the classic way a
new enum member silently does nothing.

## 6. Diagnostics cascade

Three undeclared types produce six errors -- each bad declaration also reports
a follow-on initializer mismatch. Reporting all errors is right; reporting
consequences of an error already reported is noise.

## 7. Compiler shape

`src/main.c` is **16,194 lines** in one file. Not a language problem, and not
urgent while one person works on it, but it is the reason a change like the
preprocessor rework touches lexer, parser, semantics and emitter in the same
file with no boundary between them.

There is also no self-hosting path, and there cannot be one until `std` grows an
OS layer -- see [`i-build-story.md`](i-build-story.md), which measures how far
off that is.

## Suggested order

1. **Enum exhaustiveness in `switch`** -- small, high value, machinery exists.
2. **Must-use for return values** -- 145 discarded status returns, and it
   sharpens the error model already in use.
3. **`defer`** -- small, and the ergonomic gap most visible in daily use.
4. **`for (v in xs)`** -- 238 sites say it pays for itself.
5. **Module namespaces** (§10) -- 91% of procs carry a hand-written prefix, and
   with methods ruled out this is the only thing that can remove it.

**Decided, not deferred:** no methods, in any form.

**Decided:** errors are a status enum per domain, the value comes back through
an out-parameter, and the message comes from reflection. `Result<T>` is not the
direction, and neither is a propagation form. Reasoning in §1.
