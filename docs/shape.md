# The Shape Of I

Decisions still open before the language can be called finished.

`i-soul.md` sets the philosophy: keep C's physical model, use C as the backend
and ABI, and make the repeated C-era bookkeeping explicit and checkable. It also
draws a line — I should *not* become "C but safer" in the abstract. Several
questions below are answered by that stance, and where they are, this note says
so rather than re-opening them.

Each entry records what I does **today**, verified against the current compiler
rather than assumed, so the choice is between real alternatives instead of
imagined ones.

## How To Use This

Most of these are not blocking. A language ships with unspecified corners; C
did, and still does. What matters is knowing which corners are unspecified *on
purpose*. The switch bug happened because nobody had written down what a `case`
meant, so the emitter's behaviour became the answer by default. Every item here
is a place where the same thing could happen.

Two things worth deciding early because everything else leans on them: the
**safety bargain** and the **conversion rules**. The rest can be settled as they
come up.

## 1. The Safety Bargain

The biggest one, and the one that determines what kind of language I is.

C trusts the programmer and pays for it in undefined behaviour. I inherits that
by default, because it lowers to C and does nothing to intervene. The question
is how much of it is deliberate.

**Today.** All of these compile without a diagnostic:

    arr: [3]i32 = {};
    arr[7]                     // statically knowable, out of bounds
    ov: i32 = 2147483647;
    ov += 1;                   // signed overflow, UB in the emitted C
    u: i32 = ?;                // uninitialised, reading it is UB

**Options.** Three coherent positions, and the middle one is probably the one
that matches the stated philosophy:

1. *Full C bargain.* UB is inherited wholesale and documented as such. Cheapest,
   and consistent with "explicit memory, small runtime assumptions."
2. *Static diagnostics, no runtime cost.* Keep C's runtime model exactly, but
   reject at compile time what is provably wrong: a constant index outside a
   fixed-size array, a read of a `= ?` local before any assignment. This is "the
   common shape decisions made explicit and checkable" applied to safety, and it
   costs nothing at runtime.
3. *Runtime checks.* Bounds checking, overflow trapping. Contradicts the stated
   physical model; listed only for completeness.

**Bearing.** Option 2 fits the soul document best, and it is the one a teaching
language benefits from most — a student who indexes past the end gets a sentence
instead of a corrupted heap.

**Also needs an answer:** division by zero, null dereference, and whether
`= ?` should require the compiler to prove a write before any read.

## 2. Types And Conversions

### 2.1 Implicit conversions

**Today.** Everything is allowed, silently:

    big: i64 = 300;
    small: i32 = big;      // narrowing, no diagnostic
    s: i32 = -1;
    u: u32 = s;            // sign change, no diagnostic
    n: i32 = 2.7f;         // float to int, no diagnostic

**Options.** Keep C's rules; or require `cast` for anything lossy (narrowing,
sign change, float/int) while leaving widening implicit; or require `cast` for
every conversion.

**Bearing.** `cast` already exists and is used everywhere in real I code, so the
explicit form is established. The middle option is what Zig and Rust do and it
catches a genuine bug class. This is the decision most likely to change how I
code reads, so it is worth making deliberately rather than by inheritance.

### 2.2 `bool` versus `b32`

**Today.** Both exist. Real code uses `b32` throughout; `bool` is available and
unused.

**Options.** Keep both and document when each is for; drop `bool`; drop `b32`
and make `bool` the ABI-compatible 32-bit boolean.

Two spellings for one concept is exactly the kind of accumulated friction the
soul document objects to.

### 2.3 `string`

**Today.** `string` appears in the compiler's type-name table but is not a
declared type — `s: string = "hello"` reports *use of undeclared type*. It is
vestigial.

**Options.** Implement it as a first-class slice type; or remove the name so it
stops looking available. Leaving a half-present type is the worst of the three.

Related: string literals are currently `*const char`. Whether I wants a length-
carrying string is a real design question, and `std` already has `string8` and
`string8slice`, which suggests the answer may be "in the library, not the
language."

### 2.4 Missing widths

**Today.** The primitive set is `i8 i16 i32 i64 u8 u16 u32 u64 f32 f64 b32 bool
char usize void`.

There is `usize` but no `isize`. There is `b32` but no `b8`/`b64`. There is no
`uintptr` or opaque pointer-sized integer. Whether those gaps are deliberate
should be recorded either way.

### 2.5 `char` signedness

**Today.** Signed on this target, inherited from the C compiler — which means it
is implementation-defined and can differ per platform.

For a language that fixed operator precedence rather than inherit a C wart, this
is the same kind of question. Pick signed or unsigned and state it.

### 2.6 Enum underlying type

**Today.** Enums are 4 bytes, and nothing states the underlying type. A value
that does not fit in a signed 32-bit integer is accepted without complaint and
then depends on how it is read:

    Big: enum = { X = 3000000000, }

    cast(Big.X, u32)   // 3000000000
    cast(Big.X, i32)   // -1294967296

Both are the same four bytes. Neither reading is wrong, because nothing says
which one is correct.

**Needs.** A stated underlying type, whether it can be chosen per enum, and what
happens to values that do not fit.

## 3. Names And Scope

### 3.1 Shadowing an enclosing local

**Today.** Rejected. Shadowing a *global* is allowed, and sibling blocks may
reuse a name, so scopes are properly nested — it is specifically the enclosing
case that errors.

    v: i32 = 1;
    if (v != 0) {
        v: i32 = 20;   // semantic error: duplicate local declaration
    }

C, Go, Rust and Zig all permit this. The asymmetry with globals suggests the
restriction may not be deliberate.

**Options.** Keep it and document the rule; allow it, matching the family; or
keep it and forbid shadowing globals too, for consistency.

### 3.2 C keywords as identifiers

**Today.** Rejected with a diagnostic, added this week — `typedef: i32 = 1`
previously emitted `i32 typedef = 1;` and failed inside the C compiler.

**Options.** Keep the restriction, or mangle on emission so any I identifier is
legal regardless of what C reserves.

Rejection was chosen because it is reversible and keeps generated C readable,
which matters if the C is meant to be read as a teaching artifact. But it leaks
the backend into the language surface, and "why can't I name a variable `auto`?"
is a fair question from a student.

**If the restriction stays,** it currently covers only local variables and proc
parameters. Globals, proc names, struct and union names, field names, enum
members and type aliases are unchecked.

### 3.3 Visibility

**Today.** `static` and `extern` only. There is no module-level public/private
distinction; an imported file's symbols are all visible.

**Needs.** Whether I wants export control, and if so whether it is per-symbol or
per-file.

## 4. Operators And Expressions

### 4.1 Truthiness

**Today.** `and` and `or` accept arbitrary integers — `n and 1` compiles with
`n: i32`. Real I code writes `x != 0 and y != 0` by convention rather than by
requirement.

**Options.** Require boolean operands, or keep C-style truthiness. If real code
already writes the explicit form, requiring it costs nothing and removes a class
of confusion.

### 4.2 Arithmetic right shift

**Today.** Right-shifting a negative signed value is arithmetic. C leaves this
implementation-defined. `013-integer-conversion.i` records the behaviour but
that is documentation of what happens, not a decision that it should.

### 4.3 Emitted parentheses

**Today.** Every subexpression is parenthesised: `1 shl 2 + 3` becomes
`(1 << (2 + 3))`.

Since the bitwise group was moved above comparison this week, those parentheses
are **load-bearing** — stripping them would let C re-parse `6 & 4 == 4` back
into its old grouping, silently. An earlier idea to emit unparenthesised C for
legibility is therefore withdrawn for binary operators.

A narrower version remains open: omit parentheses only where I's precedence
agrees with C's, keeping them where the tables now differ. Cheap to state,
fiddly to maintain, and it would need the precedence test extended to cover
every operator pair.

## 5. Modules And Imports

**Today.** `import "path.i"` brings a file's symbols into scope. `std` is
resolved from beside the compiler executable.

**Needs.** What happens on a circular import; whether importing the same file
twice through different paths is one module or two; whether an import is
transitive — if A imports B and B imports C, does A see C's symbols; and what
the generated header is supposed to contain relative to what the file exports.

None of these have shown up as bugs yet, which usually means they have not been
exercised rather than that they are settled.

## 6. Generics And Reflection

**Today.** Generics are monomorphisation with no constraints. `proc<T>` accepts
any `T`; a body that assumes `T` supports `+` fails inside the instantiation
rather than at the call.

Reflection is one kind-tagged `reflect` record per struct, union and enum, with a
variant payload, checked field access, and a nested `info` link per field so a
walk can recurse. It works under monomorphisation. See `reflection-issues.md`
for what is settled and what is still open.

**Needs.** Whether generics get bounds — and note that adding them is the single
fastest way to become C++-shaped, so the honest answer may be a deliberate
"no, error messages inside instantiations are the price." Whether there is any
compile-time evaluation beyond enum values, `sizeof` and `Enum<>.count`.

## 7. The C Backend Contract

The soul document commits to C ABI interop and predictable layout. That implies
promises the language has not yet written down.

**Needs.** Whether struct layout is guaranteed to match the equivalent C
declaration, including padding. Whether there is control over packing and
alignment. Whether bitfields are supported and how they map — a torture test for
anonymous bitfields exists, so something is there. What calling convention procs
use and whether it can be specified. What the emitted C guarantees, given
"if the emitted C is free of undefined behaviour, it means what the I means" is
the strongest correctness claim available, and it is only worth something once
the lowering rules are written down.

## 8. Tooling And Diagnostics

**Today.** Preprocessor directives are hoisted to the top of the generated file,
so an inline `#ifdef` around a statement does not work. Real code works around
this with runtime flags.

**Needs.** Whether that is a limitation to fix or a restriction to document. More
broadly: whether I wants conditional compilation at all, or something better,
given that the soul document lists "macro accidents" among C's failures.

## Settled This Week

Recorded so they do not get re-litigated:

- **A switch case does not fall through.** It takes a block, so it is
  self-contained. Matches Go, Rust and Zig.
- **Bitwise operators bind tighter than comparison.** C's ordering exists only
  because early C had no `&&`; Ritchie called it a mistake. Verified safe by
  regenerating a 28,301-line engine with zero changed lines.
- **`if` requires a braced body**, which makes the dangling-else ambiguity
  unwritable. Already true; worth stating as intent rather than accident.
- **`const` is enforced** on assignment.
- **Reflection is one record, kind-tagged, with a variant payload.** Odin's
  shape, which ports to I as-is; Zig's needs language-level tagged unions first.
  This also settled nested type links and made a union its own kind rather than
  a flag. Details and the remaining open question in `reflection-issues.md`.
- **Reflected enum values are `i32`.** I permits negative members and they must
  round-trip; `u32` would turn `-1` into `4294967295` and break every lookup by
  value. Independent of how the enum underlying type (§2.6) resolves.

## Suggested Order

1. **The safety bargain** (§1) — it sets the language's character, and the
   static-diagnostic option is cheap and fits the stated philosophy.
2. **Implicit conversions** (§2.1) — the decision most likely to change how
   ordinary I code reads.
3. **`bool` vs `b32`, and `string`** (§2.2, §2.3) — small, and they remove
   visible inconsistency.
4. **Shadowing** (§3.1) — a one-line change either way, currently asymmetric.
5. Everything else as it comes up.

Whatever is decided, write it into the lowering table described in
`compiler-hardening.md` and give it a discriminating test. A decision that is
not written down becomes whatever the emitter happens to do, which is how this
document's motivating bug got in.
