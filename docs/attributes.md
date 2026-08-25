# Declaration Attributes

> **Implemented.** `struct[external]`, `proc[external]`, `proc[external, WINCALL]`
> and `enum[external]` all parse and carry their meaning, the old spellings still
> work, and the tree has migrated -- 268 procs and 79 structs, unions and enums
> across 30 files. Covered by `decl_attributes`. **Layout verification is also
> implemented** -- 934 assertions now run on every njinn build.

What one attribute slot per declaration buys, and what it means for C interop.

## The slot already exists

`proc[...]` is already parsed, and already carries something:

    platform_add: proc[WINCALL](a: i32, b: i32)->i32 = { ... }

The parser reads a single identifier between the brackets and stores it as the
declaration's calling convention. Seven uses across the tree. Structs have no
such slot.

So this is not a new convention. It is making a slot that exists uniform, and
letting it hold more than one thing.

## The form

    timespec:       struct[external] = { tv_sec: i64; tv_nsec: long; }
    FILE:           struct[external] = {}
    timespec_alloc: proc[external](...)->i32 = {}
    printf:         proc[external, WINCALL](fmt: *const char, ...)->i32 = {}
    DXGI_FORMAT:    enum[external] = { UNKNOWN, }

replacing

    timespec:       struct = { external; tv_sec: i64; tv_nsec: long; }
    timespec_alloc: proc(...)->i32 = { external; }

One parser, `parse_decl_attributes`, reads the comma-separated list for all
three declaration kinds. It recognises `external` and `external_emit`; any other
identifier is taken as a calling convention, which is all the slot held before,
so `proc[WINCALL]` is unchanged. The `= {}` stays: `name : kind = value` holds
with no exceptions, and an empty body reads as "defined elsewhere".

**The old spellings still parse.** That is what let 347 declarations migrate a
file at a time rather than in one commit, and the test fixtures in
`run_tests.py` were deliberately left on the legacy form so it stays covered.

### Why the bracket is the right place

**It is where the truth lives.** (This one had teeth: before the slot took a
list, *any* identifier in it was read as a calling convention, so `proc[external]`
parsed cleanly on the old compiler and emitted
`i32 external printf(const char *, ...);` -- invalid C from a declaration that
looked fine. The test asserts against exactly that.) `external` says something about the
*declaration* -- do not emit it, C already has it. Inside the braces is where
**fields** go. Putting a non-field there is a category error, and it is exactly
why `FILE: struct = { external; }` reads as a struct with one strange member.

**It ends two mechanisms for one concept.** Procs mark it in the body; structs
mark it as a pseudo-field. Same idea, two spellings, for no reason.

**It composes.** An external proc that also needs a calling convention currently
has to wear both systems at once -- `proc[WINCALL] = { external; }`. One slot
handles it: `proc[external, WINCALL]`.

### The argument worth leading with

The slot unblocks three items in `shape.md` that have **no syntax at all** today.
All three are parse errors, verified against the current compiler:

    P:   struct packed = { ... }     // parse error: expected '=' after struct
    V:   struct align(16) = { ... }  // parse error
    Big: enum(u32) = { ... }         // parse error

Those are §7 (the C backend contract -- no way to state packing or alignment, in
a language driving a D3D11 renderer, where a wrong vertex layout is silent wrong
bytes on the GPU) and §2.6 (enum underlying type). With a general slot they
become `struct[packed]`, `struct[align(16)]`, `enum[u32]` and need no new
grammar. `static` is a bare keyword today and could fold in the same way.

That is the real case. Not tidying -- opening a slot the language needs at least
three more times.

### It dissolves the opaque-struct question

    timespec: struct[external] = { tv_sec: i64; tv_nsec: long; }   // layout known
    FILE:     struct[external] = {}                                // layout unspecified

Same construct; the difference is whether the field list is empty. "Opaque" stops
being a special case and becomes *data*, so there is no second keyword to argue
about and no separate form to teach. Field access on the empty one stays
rejectable exactly as it is now.

This replaces an earlier plan to ban `external` on structs. That plan was
proposed on the premise that the opaque form "accepts any field name and passes
it straight through to C" -- a line from a stale comment in `std/reflect.i`.
Measured against the current compiler, the opaque form is fully checked:

    Op: struct = { external; }
    o.bogus   ->   type error: cannot read field 'bogus': type 'Op' is external

The comment predates a fix and was never updated. It should be corrected.

## Settled while implementing

**`= {}` stays on external procs.** `name : kind = value` holds with no
exceptions, and one empty brace pair is cheaper than a special-case declaration
form.

**Attributes are comma-separated inside one bracket**, not stacked brackets.

## Where the slot sits

The declaration grammar is

    name : kind [attributes] <generics> (params) -> ReturnType = { body }

so attributes come **before** the type parameters, not after:

    f:   proc[external]<T>(x: T)->i32 = {}
    Box: struct[external]<T> = {}

**Variables have no attribute slot.** Of the six attribute names only
`external` ever meant anything on one -- `packed`, `align(N)`, `callconv(...)`,
`no_layout_check` and `external_emit` parsed and were silently dropped. And the
slot would have to sit directly after the colon, where `[` already begins an
array type, so `[external]` and `[Kind.Count]` needed a lookahead that existed
to support the single attribute that worked.

A global with **no initializer** says it instead:

    g_atlas:  const Atlas;      // C owns it -- nothing is emitted
    g_table:  [4]i32 = {};      // ilang owns it, zeroed
    g_x:      i32 = ?;          // ilang owns it, indeterminate

The form was an error until now, so nothing had to move. It reads the way the
rest of the language already works: if you did not say how it is initialized,
you are not the one defining it.

**The cost, stated plainly:** forgetting an initializer used to be an ilang
error and is now a declaration that C is expected to satisfy. If C does not,
clang says *use of undeclared identifier* at the use site. ilang cannot tell the
two apart, because a `cinclude` deliberately brings no names into scope -- the
same trade `struct[external]` already makes. Locals still require an
initializer, since a local cannot be owned by C.

## One spelling, not two

`external;` written inside a body is **rejected**. The attribute is the only
form:

    f: proc[external](fmt: *const char, ...)->i32 = {}
    X: struct[external] = {}
    E: enum[external] = { A, }

    // and with generic parameters, which come first:
    f: proc<T>[external](x: T)->i32 = {}
    Box: struct<T>[external] = {}

The body form existed so 347 declarations could migrate a file at a time rather
than in one commit. That migration is done, and keeping it meant four separate
parsers -- struct bodies, struct fields, enum items and proc bodies -- each
recognising the same thing independently and each able to drift. Covered by
`external_spelling`.

Globals are covered by the no-initializer rule above; `g: const T = external;`
is rejected.

## `external` is not C's `extern`

Worth stating plainly, because conflating them produced a real bug.

**`external` means: C owns this definition; here is its shape so ilang can
type-check against it.** It is a fact about where the thing lives, not a
linkage specifier. So an external declaration emits **nothing** -- the
declaration C needs already arrived through the header the program `cinclude`d.
That is true of `struct[external]`, of `enum[external]`, of `proc[external]`,
and now of `g: [external] T;` as well.

**C's `extern` is a linkage specifier**, and asserting it is a decision about
how the symbol is resolved. ilang emitted `extern T g;` for external globals,
which made them the only kind that emitted anything, and it asserted *external*
linkage over definitions C is free to have made `static`. njinn has exactly
that: `gui_atlas_meta.h` defines both atlases `static`. It happened to work,
because a prior internal-linkage declaration wins (C11 6.2.2p4) and the header
is included first -- but the ordering was load-bearing and nothing enforced it,
and clang diagnoses neither arrangement.

Emitting nothing removes the question: whatever linkage C gave the definition is
the linkage the reference gets. Covered by `external_globals`, which asserts the
declaration is *absent* from both the generated C and the header.

## Where an attribute may appear

> **Implemented.** Covered by `decl_attribute_rule`, which checks the rejected
> positions, checks the allowed one *aligns* rather than merely parsing, and is
> mutation-tested by dropping the emission.

**An attribute attaches to a declaration. It never attaches to a type use.**

That single rule decides every position, and it is what makes the two written
forms -- after a head keyword, or after a whole type -- one idea rather than two:

    x: i32[attrib] = 0                 ok     declares x
    x: [4]*const T[attrib] = {}        ok     still declares x; outermost
    P: struct[attrib]<T> = {}          ok     declares P
    P: struct = { f: i32[attrib]; }    ok     declares a field
    f: proc[attrib](a: i32)->i32 = {}  ok     declares f

    x: *proc(a: i32)->i32[attrib]      error  a return type is a use
    x: [4](i32[attrib])                error  an element type is a use
    x: *(T[attrib])                    error  a pointee is a use
    y: Box<i32[attrib]>                error  a generic argument is a use

So an attribute is always outermost in the thing it modifies, and there is
exactly one per declaration.

**On a value, `align(N)` is the only attribute that means anything.** `external`
is answered by a global having no initializer; `packed`, `no_layout_check` and
`callconv` describe records and procs. Each of those is rejected by name rather
than accepted and ignored -- `[align(16)] i32` used to parse and emit a plainly
unaligned `i32`, which is the failure this whole rule exists to prevent.

`align(N)` on a variable or a field lowers to C11 `_Alignas(N)`, which is the
declaration-level spelling; records keep `__attribute__((aligned))` on the tag,
because a tag is not a declaration.

### An alias takes no attribute

    Handle: alias = i32[attrib];       error

An alias does not declare a type; it gives an existing one a second name. There
is no declaration for the attribute to modify, and letting it through would
raise a question with no good answer -- whether `Handle` or `i32` is the thing
being modified.

### A parameter takes no attribute

    f: proc(a: i32[attrib])->i32 = {}  error
    x: *proc(a: i32[attrib])->i32      error

Both, in the end. A parameter would only ever want a *type qualifier* -- `const`,
`restrict`, nullability -- and those belong to the type, which already carries
them. The one real parameter attribute in C is `unused`, and that is warning
suppression, a build concern rather than a language one.

Ruling them out in both places also removes the only context-dependent case in
the rule: a parameter would otherwise have been a declaration inside a proc
declaration and a bare name inside a proc type, which is a distinction nobody
should have to be taught.

Worth noting the second line needs the `*`: C has no object of function type, so
a variable holding a proc is a pointer to one. A bare `x: proc(a: i32)->i32` is
an error on its own account -- read as a proc declaration it is missing its
`= { }` body, and read as a variable type it is an object of function type.

## Still open

Nothing. **Attributes take arguments**, which was the last question here:
`struct[align(16)]`, `enum[u32]` and `proc[callconv(NAME)]` all parse and carry
their meaning, and the name is checked against a closed list -- an unrecognised
one used to fall through to "calling convention", so `struct[externl]` silently
meant *not external*. Covered by `decl_attributes_known`, which asserts the
resulting sizes rather than only that the emitted C compiles: `packed` that does
not pack compiles perfectly well.

## What may go in the slot

Worth fixing while it is still empty, because both Rust's `#[...]` and C++'s
attributes sprawled:

> **An attribute may change how a declaration is lowered. It may not change what
> the declaration means in ilang.**

`external` (do not emit), `packed` and `align` (layout), `WINCALL` (ABI) all
satisfy this. `inline` and `deprecated` are the first two that would need
arguing about, and are deliberately out of scope for now -- the rule above is
what to argue with when they come up.

## What this means for C headers

**Nothing, directly.** The attribute change is a spelling change for a marker
that already exists. It neither costs nor buys any C transparency.

The transparency question was already settled separately: a `cinclude` brings no
names into ilang, so every C function is declared before it can be called. See
`name-resolution.md`. Types have not had the same treatment yet, and that is the
actual open gap:

| foreign types in njinn + std | count |
|---|---:|
| used **only** behind a pointer -- layout never needed | 31 |
| used **by value** -- layout genuinely needed | 85 |

All 116 are declared nowhere in ilang today; they arrive through `cinclude` and pass
through unexamined. That is the type-level twin of the undeclared-call hole, and
closing it is what "everything must be declared" actually costs.

### Who writes those declarations

**The translator already exists and is already in the build.** `src/ibind.c` is
1,314 lines built on **libclang** -- it parses real headers with the actual C
frontend and emits `.i` bindings, including `external` structs with field lists,
aliases and external procs:

    ibind <input.h> <output.i> [--preprocess] [--filter path-fragment]
          [--prefix symbol-prefix] [-- <clang args...>]

`njinn/src/bindings/cgltf.i` is its output -- 47 structs with full field lists,
regenerated by `njinn/scripts/bindgen_cgltf.py`. Pointing it at `windows.h` and
`d3d11.h` is a matter of running it with the right `--filter` and `--prefix`,
not of building anything.

**An LLM is the wrong tool for this specific job**, and the reason is not
snobbery. Field order and padding have to be exactly right: one transposed
member is not a compile error, it is silent memory corruption at a struct
boundary. libclang reports the layout *as the C compiler actually sees it*,
including SDK-version differences, `#ifdef` variants and `#pragma pack`. It is
also re-runnable -- the Windows SDK updates and a generated file updates with it,
while a chat transcript does not. The legitimate use is downstream of generation:
naming and ergonomics, or triaging what libclang could not express.

### The COM macro case, already solved

libclang can see a function-like macro but cannot type it, and `d3d11.h`'s COM
calls are macros: `ID3D11Device_CreateBuffer(dev, ...)`. njinn already handles
this by declaring them as external procs in `externs.i`. That works because **an
external proc emits call sites only, never a prototype**, so ilang type-checks the
call and cpp expands the macro underneath. The same trick covers `va_start`,
`va_end` and `_alloca`.

Separately, function-like `#define`s written in *ilang* source are callable with an
unknown signature; see `name-resolution.md`. That covers `gin_require` and
friends, not the header's macros.

### `external` vs `external_emit`

The two spellings differ only in whether a C prototype is emitted, and which one
to use is parked in `external-vs-external_emit.md` -- including the measurement
that njinn's build passes either way, the three ways `external_emit` breaks on
hand-written bindings, and a proposal to flip everything to `external` and keep
`external_emit` as an opt-in audit.

### Layout verification -- implemented

Nothing verified that a declared `external` layout matched C's real one. If a
header reordered a member, or a `long` was the wrong width on this target, ilang
type-checked field access happily against a layout that was a lie, and the
result was wrong bytes rather than a diagnostic. Procs at least have a prototype
C can compare against; records had nothing.

The compiler cannot assert `sizeof(X) == 24` -- it does not compute C layouts,
deferring to C everywhere including reflection. So it emits a **shadow record**
built from what ilang was told and asks C to compare the two:

    typedef struct { i32 x; i32 y; } i_layout_lc_point;
    _Static_assert(sizeof(lc_point) == sizeof(i_layout_lc_point), "...size");
    _Static_assert(_Alignof(lc_point) == _Alignof(i_layout_lc_point), "...alignment");
    _Static_assert(offsetof(lc_point, x) == offsetof(i_layout_lc_point, x), "...x offset");
    _Static_assert(sizeof(((lc_point *)0)->x) == sizeof(((i_layout_lc_point *)0)->x), "...x type");

C computes both layouts with the same rules, so member order, member type,
padding and alignment are all covered. Per-field offsets matter: reordering two
members of equal size passes a bare `sizeof` check and fails this one.

**Emitted once into the `.c`, never the header.** njinn has 30 translation units
and one proving the layout proves it for all of them; emitting into a shared
header would multiply the work thirtyfold for nothing.

**Skipped**, because none of them can be compared: bitfields and anonymous
members (`offsetof` cannot address them), generic records (no single C type),
records with an empty field list (they claim nothing), and records marked
`no_layout_check`.

**`no_layout_check`** exists for records that have no C type of that name at
all. `ibind` synthesises a name for a genuinely anonymous member -- cgltf's
`union { ... } data;` becomes `cgltf_camera_anon0` -- and C cannot be asked about
a type it cannot name. ibind now marks these automatically, and the suppression
is **transitive**: a record with a by-value field of an unnameable type is
itself unnameable, so `cgltf_camera` is skipped too. Pointers are exempt, since a
pointer is a pointer whatever it points at.

**Measured on njinn:** 934 assertions across 74 external records, all passing.
Cost was measured separately at ~1% of one translation unit's compile time, with
byte-identical object files -- `_Static_assert` is a frontend check with no
codegen.

*Covered by `layout_check`, which pairs a correct declaration against four wrong
ones -- wrong member types, reordered members, an extra member, a missing member
-- plus `no_layout_check` suppressing, an opaque record emitting nothing, and an
assertion that the checks are actually present, since zero errors would otherwise
be indistinguishable from zero checks.*

## Migration cost

| | count |
|---|---:|
| `external` structs | 61 |
| `external` procs -- njinn | 228 |
| `external` procs -- i/std | 17 |
| `external` procs -- test fixtures | 61 |
| `proc[CALLCONV]` already using the slot | 7 |

Mechanical, and all of it is in three repos under one owner. Accepting both
spellings for one release makes it painless; a single pass is also viable.
