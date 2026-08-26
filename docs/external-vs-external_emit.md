# `external` vs `external_emit`

Parked. The proposal at the bottom is to flip everything to `external` and keep
`external_emit` as an opt-in audit tool. Nothing here is implemented; the
measurements are.

## The model

Using a C library from ilang means two separate parties need to know its shapes:

1. **ilang needs to know**, to type-check your code. That is the `.i` declaration.
   Always required, and identical for both spellings.
2. **C needs to know**, so the generated C compiles. That comes from *either*
   the header (`cinclude`) *or* ilang emitting prototypes (`external_emit`).

`external` and `external_emit` differ in exactly one respect: whether a C
prototype is emitted. One flag, `emit_external_proto`, guarding two returns in
`emit_proc_proto` and `emit_proc_proto_mono`:

    if (decl->is_external && !decl->emit_external_proto) return;

Which gives four combinations, three of them valid:

| | C learns shapes from | declaration verified? |
|---|---|---|
| `cinclude` + `[external]` | the header | no |
| `cinclude` + `[external_emit]` | both, so C compares them | **yes, partly** |
| no `cinclude` + `[external_emit]` | your declaration | no |
| no `cinclude` + `[external]` | nothing | **does not compile** |

That last row is measured, not assumed: `use of undeclared identifier`. So
`[external]` without a `cinclude` is simply broken, and that is the one rule to
remember.

## What the verification actually catches

With the header present, the emitted prototype sits beside the real one and C
compares them. Measured with a deliberately wrong return type and correct arity
(`f64` where the real function returns `int32_t`):

| | wrong return type, right arity |
|---|---|
| `proc[external]` | **silent** -- accepted, wrong code generated |
| `proc[external_emit]` | `conflicting types for 'vendor_sum'` |

`external` catches a bad signature only when a call site happens to expose it --
wrong *arity* fails at the call, wrong *types* with matching arity sail through.

## What it costs

Flipping njinn's 173 hand-written `proc[external]` declarations in `externs.i`
to `external_emit` produced **20 C errors, with clang stopping early**, so 20 is
a floor. Three distinct failure modes:

**Macros break outright.** Five `expected ')'`. The emitted prototype gets
macro-expanded:

    #define tricky_max(a, b) ((a) > (b) ? (a) : (b))
    tricky_max: proc[external_emit](a: i32, b: i32) -> i32 = {}
    -> clang: expected ')'

This is why `external` is the *only* way to declare a C macro as a proc, which
njinn relies on for `va_start`, `va_end`, `_alloca` and every COM call macro.

**Equivalent type spellings are rejected.** `conflicting types` for
`RegisterClassA`, `LoadLibraryA`, `GetProcAddress`, `TranslateMessage` and
others. `long` and `int` are both 32 bits on Windows and interchangeable at the
ABI level, but C treats them as different types, so a declaration that is
functionally correct fails to build. Win32 is full of `DWORD`, `BOOL`, `ATOM`,
`HRESULT`.

**Opaque handle collisions.** `redefinition of 'IDXGISwapChain' as different
kind of symbol` -- the emitted struct tag clashes with the header's COM
interface declaration.

**Not a problem:** header-only `static inline` functions, tested specifically
because cglm is header-only. Those pass cleanly.

## Neither covers struct layouts

There is no struct equivalent of the prototype cross-check. A struct declared
`external` emits nothing, so C has nothing to compare against. Tested:

    // real: struct vendor_point { int32_t x; int32_t y; }
    vendor_point: struct[external_emit] = { x: f64; y: f64; z: i32; }

compiled **silently** when this was written. That gap is now closed: every
`external` record with a field list gets a shadow record and per-field
`_Static_assert`s comparing it against C's real layout -- 934 of them on an njinn
build. See `attributes.md`.

Which strengthens the proposal below rather than weakening it. Layout was the
half that could silently corrupt memory, and it is now checked regardless of
which spelling a proc uses. What `external_emit` still adds is a partial check on
*proc signatures*, with the false positives documented above.

## What njinn does today, and whether it needs to

`src/pch.i` already `cinclude`s every header njinn uses, including `cgltf.h`.
So `bindings/cgltf.i`'s 37 `external_emit` prototypes are **not load-bearing** --
they are purely a second opinion.

Measured: flipping all 37 to `[external]` and running njinn's real build (its
own compile command, its PCH) gives **exit 0, zero errors**.

So in njinn, `external_emit` buys only verification. Its unique capability --
making generated C compile with no header at all -- is a generality the project
does not use.

## The proposal

**Flip everything to `external`, keep the `external_emit` mechanism as an
opt-in audit.**

Reasons:

- **One rule instead of two**: declare it in ilang, `cinclude` the header in C. No
  decision when writing a binding, one mechanism to teach.
- **`external` never breaks.** Macros, type spellings, opaque handles all fine.
- **The verification is being lost exactly where it was least needed.** The only
  checked bindings today are cgltf's 37, which `ibind` generated with libclang
  *from the same header clang is reading* -- correct by construction. The 173
  hand-written declarations, the ones that genuinely could be wrong, were never
  checked and largely cannot be without false alarms.

**One condition.** `ibind` should emit a `cinclude` of the header it read.
Otherwise generated bindings stop being self-contained: you would import
`cgltf.i` and still have to remember the header elsewhere. ibind already knows
the path -- it is the input argument.

**One thing to do first.** Run the audit once, before flipping. The conflicts
the experiment surfaced (`RegisterClassA`, `LoadLibraryA`, `GetProcAddress`,
`TranslateMessage`) were *assumed* to be harmless spelling differences and never
checked. Some may be real -- a wrong `DWORD` vs `BOOL`, a missing `const`, a
pointer depth off by one -- and those would not show up as build errors today,
only as wrong behaviour. That is the one moment `external_emit` earns its keep,
and it is cheapest while the flip is a one-line experiment.

**Framing.** Calling `external_emit` "the verification mechanism" is thinner
than it sounds. Prototype comparison is a proxy that fails on macros and type
spellings. Better framed as an audit tool switched on deliberately -- especially
now that record layouts, the part that could silently corrupt memory, are checked
independently of it.

**One risk to be deliberate about.** After the flip, `external_emit` has zero
production users, and the test suite becomes the only thing keeping it working.
It needs to stay covered or it will rot and be useless the day the audit is
wanted.

## Corrections made while working this out

Recorded because each was stated confidently and was wrong:

- **"`external` + `cinclude` lets C verify your declaration."** No. With
  `external` no prototype is emitted, so there is nothing to compare. It is
  `external_emit` that gets the check, and only because the prototype is
  redundant with the header's.
- **"Flip `externs.i` to `external_emit` for 228 free checks."** It breaks the
  build -- see the three failure modes above.
- **A cgltf flip test was read as a result without running the control.** The
  same 20 errors appeared unmodified; the ad-hoc compile was missing the build's
  PCH. The real answer needed njinn's actual build command.
