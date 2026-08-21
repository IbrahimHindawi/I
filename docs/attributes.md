# Declaration Attributes

> **Implemented.** `struct[external]`, `proc[external]`, `proc[external, WINCALL]`
> and `enum[external]` all parse and carry their meaning, the old spellings still
> work, and the tree has migrated -- 268 procs and 79 structs, unions and enums
> across 30 files. Covered by `decl_attributes`. The layout-verification section
> below is still a proposal.

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

## Still open

**Do attributes take arguments?** The parser reads bare identifiers, so
`struct[align(16)]` and `enum[u32]` are not yet expressible. Those are the next
users of the slot -- see §7 and §2.6 in `shape.md` -- and they are what decides
how far it generalises.

## What may go in the slot

Worth fixing while it is still empty, because both Rust's `#[...]` and C++'s
attributes sprawled:

> **An attribute may change how a declaration is lowered. It may not change what
> the declaration means in I.**

`external` (do not emit), `packed` and `align` (layout), `WINCALL` (ABI) all
satisfy this. `inline` and `deprecated` are the first two that would need
arguing about, and are deliberately out of scope for now -- the rule above is
what to argue with when they come up.

## What this means for C headers

**Nothing, directly.** The attribute change is a spelling change for a marker
that already exists. It neither costs nor buys any C transparency.

The transparency question was already settled separately: a `cinclude` brings no
names into I, so every C function is declared before it can be called. See
`name-resolution.md`. Types have not had the same treatment yet, and that is the
actual open gap:

| foreign types in njinn + std | count |
|---|---:|
| used **only** behind a pointer -- layout never needed | 31 |
| used **by value** -- layout genuinely needed | 85 |

All 116 are declared nowhere in I today; they arrive through `cinclude` and pass
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
external proc emits call sites only, never a prototype**, so I type-checks the
call and cpp expands the macro underneath. The same trick covers `va_start`,
`va_end` and `_alloca`.

Separately, function-like `#define`s written in *I* source are callable with an
unknown signature; see `name-resolution.md`. That covers `gin_require` and
friends, not the header's macros.

### The thing that makes any of this trustworthy

Nothing currently verifies that a declared `external` layout matches C's real
one. If a header reorders a field, or a `long` is the wrong width on this target,
I checks field access happily against a layout that is a lie.

The compiler already emits `offsetof` for reflection tables, so the pieces are
there. For every `external` struct with fields, emit into the generated C:

    _Static_assert(sizeof(D3D11_BUFFER_DESC) == 24, "layout mismatch");
    _Static_assert(offsetof(D3D11_BUFFER_DESC, ByteWidth) == 0, "layout mismatch");

C then checks the declaration against the real header on every build, at zero
runtime cost. That turns `external` from "trust me" into "verified", and it is
what makes generating 116 declarations safe regardless of what generated them.

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
