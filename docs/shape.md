# The Shape Of ilang

Decisions still open before the language can be called finished.

`i-soul.md` sets the philosophy: keep C's physical model, use C as the backend
and ABI, and make the repeated C-era bookkeeping explicit and checkable. It also
draws a line — ilang should *not* become "C but safer" in the abstract. Several
questions below are answered by that stance, and where they are, this note says
so rather than re-opening them.

Each entry records what ilang does **today**, verified against the current compiler
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

The biggest one, and the one that determines what kind of language ilang is.

C trusts the programmer and pays for it in undefined behaviour. ilang inherits that
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

**Bearing.** `cast` already exists and is used everywhere in real ilang code, so the
explicit form is established. The middle option is what Zig and Rust do and it
catches a genuine bug class. This is the decision most likely to change how ilang
code reads, so it is worth making deliberately rather than by inheritance.

> **Parked.** ilang keeps C's rules for now -- deliberately dirty and unsafe.
> Written up with everything else in that bargain in
> [`type-safety.md`](type-safety.md), alongside the runtime half in
> [`safety.md`](safety.md).

### 2.2 `bool` versus `b32`

> **Resolved: `b32` stays, `bool` is gone.** `bool` no longer names a type and
> reports *use of undeclared type* like any other unknown name. std's duplicate
> `print<bool>` overload -- byte-identical to `print<b32>` -- was removed and the
> 36 remaining uses across std and the test suite were converted. njinn was
> already at zero, and ibind already mapped C's `bool` to `b32`, so the C
> boundary was unaffected. Covered by `b32_only`.
>
> **Since fixed, in 2.4.** `core.h` had `typedef bool b32;`, so `b32` was one
> byte rather than four. It is `int32_t` now. The measurements that made the
> change safe: no external struct in njinn has a `b32` field, so none of the 952
> `_Static_assert` layout checks depended on the old width; all njinn file IO is
> byte-oriented JSON rather than raw struct dumps, so no save format did either;
> and 122 internal struct fields grew 1 to 4 bytes, memory only.
>
> The one semantic change is worth knowing: C's `bool` normalises to 0/1 on
> assignment and `int32_t` does not, so `f: b32 = x & 4` now stores `4` where it
> used to store `1`, and `f == 1` would differ. `if (f)` is unaffected. No `b32`
> value in njinn is compared against 1 anywhere, so nothing there can observe
> it.

### 2.3 `string`

**Today.** `string` appears in the compiler's type-name table but is not a
declared type — `s: string = "hello"` reports *use of undeclared type*. It is
vestigial.

**Options.** Implement it as a first-class slice type; or remove the name so it
stops looking available. Leaving a half-present type is the worst of the three.

Related: string literals are currently `*const char`. Whether ilang wants a length-
carrying string is a real design question, and `std` already has `string8` and
`string8slice`, which suggests the answer may be "in the library, not the
language."

> **Parked, with a target.** The direction is to remove as much C string usage
> as possible, and the measurements say how much that is: 536 `*char` /
> `*const char` against 91 `string8` mentions in njinn, 167 `strcmp`, 51
> `snprintf`. See [`string.md`](string.md), which also covers `char`
> signedness and records that `usize` is the only size type -- there is
> deliberately no `isize`.

### 2.3b The type algebra *(implemented)*

Types compose recursively and read outside-in, with every constructor prefix:

    type := T | const type | volatile type | *type | [N]type | proc(...) -> type

so `x: static [4]*const Foo = {}` decomposes left to right: a static variable,
an array of 4, pointers to, const Foo. No spiral rule.

`const` binds to the type expression after it, which gives the three pointer
forms distinct and unambiguous spellings:

    *const T          pointer to const T
    const *T          const pointer to T
    const *const T    const pointer to const T

**`const [N]T` is rejected.** C says that qualifiers on an array type qualify
the *element*, not the array (C11 6.7.3p9), so it and `[N]const T` are the same
type once lowered. A distinction that evaporates in the backend is worse than no
distinction, given the correctness claim rests on the emitted C meaning what the
ilang means.

**Storage is not part of the type.** `static`, `thread_local` and friends say
where an object lives and who can see it; `const` and `volatile` say what the
type is and what may be done through it. Both are written before the type today
and that is fine while the word lists stay closed and disjoint, but they are
different categories and new words sort by which question they answer.
`external` is in neither: it says which *compiler* emits the definition. See
[`attributes.md`](attributes.md).

Slices (`[]T`) are deliberately absent -- that is the parked
[`string.md`](string.md) question, and it is a representation decision rather
than a syntax one.

### 2.4 Missing widths

> **Resolved.** The primitive set is now:
>
>     i8 i16 i32 i64   u8 u16 u32 u64   f32 f64
>     b8 b16 b32 b64   c8   usize   void
>     intptr uintptr ptrdiff intmax uintmax
>
> **Booleans carry their width, and `b32` is the default** -- what comparisons
> and `and`/`or` produce, and what matches Win32's `BOOL`. They are backed by
> `uint8_t`, `uint16_t`, `int32_t`, `int64_t`. This is the part that forced 2.2:
> `bool` is one byte, so a family built on it would have made `b8` and `b32` the
> same type wearing different names.
>
> **`intptr uintptr ptrdiff intmax uintmax`** are ilang spellings of C's own
> fixed names, so a program can say what it means without a `cinclude`. All five
> are 8 bytes on every target ilang builds for today; the `printfmt` specs
> assume that, and are where it would show up if that changed.
>
> `isize` remains deliberately absent -- reasoning in [`string.md`](string.md).
> Covered by `scalar_widths`, which asserts every width at runtime rather than
> only that the generated C compiles.

### 2.5 `char` signedness

**Today.** Signed on this target, inherited from the C compiler — which means it
is implementation-defined and can differ per platform.

> **Resolved: the type is `c8`, and it is whatever C's `char` is.** Rather than
> pick signed or unsigned and then have to defend the choice at every C
> boundary, ilang declines to have an opinion and names the thing accurately:
> `c8` *is* the C compiler's `char` on this target, width and signedness
> included. The name also stops implying "a character", which it never was.
>
> `char` remains a legal spelling and **normalises to `c8` at parse time**, so
> the two are one type rather than two that look alike -- `*const char` and
> `*const c8` can be passed between each other with no cast, and both emit
> `const c8 *`, where `c8` is a `typedef char` in `core.h`. Existing source
> needs no migration: njinn's 536 `char` sites and every ibind-generated binding
> keep working untouched.
>
> The visible consequence: the compiler and the LSP report `c8` back even when
> you wrote `char`. Covered by `c8_is_char`, which passes a value across both
> spellings and out to a real `strlen`.

### 2.6 Enum underlying type

**Today.** Enums are 4 bytes, and nothing states the underlying type. A value
that does not fit in a signed 32-bit integer is accepted without complaint and
then depends on how it is read:

    Big: enum = { X = 3000000000, }

    cast(Big.X, u32)   // 3000000000
    cast(Big.X, i32)   // -1294967296

Both are the same four bytes. Neither reading is wrong, because nothing says
which one is correct.

> **Resolved: an unattributed enum is `i32`,** and the emitted C says so --
> `typedef enum E : i32 { ... }`. It can be chosen per enum with the attribute
> (`enum[u32]`), which now reads as a change of mind rather than the only source
> of truth about what an enum is. All 34 enums in njinn state their type.
>
> **Overflow is checked too, in two halves.** Stating the underlying type is
> what gave this something to check against -- before, `Big: enum = { X =
> 3000000000, }` was accepted by ilang *and* by clang at every warning level,
> and read as `-1294967296` through `i32` and `3000000000` through `u32`.
>
> ilang reports a member that does not fit for plain literals, negative
> literals and implicit sequential values: 435 of the 442 members across njinn,
> `std` and the tests. The other 7 are constant expressions (`1 shl 2`, `~0`,
> references to siblings) and need an evaluator ilang does not have, so once a
> value is unknown the walk stops rather than guessing at the implicit values
> after it.
>
> The generated C carries
> `#pragma clang diagnostic error "-Wmicrosoft-enum-value"` after the includes,
> which covers the expression cases on the real `.i` line -- the same borrowing
> the external layout asserts already do. It is placed after the includes so it
> governs the enums ilang emits rather than whatever a third-party header
> contains.
>
> So: ilang answers at `check` time for the common case, which is what an
> editor sees; clang backstops the rest at build time. Covered by
> `enum_default` and `enum_ranges`.

## 3. Names And Scope

### 3.1 Shadowing an enclosing local *(fixed)*

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

**A concrete hole in the permitted case.** Shadowing a *proc* with a local is
accepted, and the failure surfaces as a C error in generated code rather than a
diagnostic:

    helper: proc(v: i32)->i32 = { return v * 2; }

    main: proc()->i32 = {
        helper: i32 = 7;
        n: i32 = helper(3);   // `i: checked` passes
        return helper + n;
    }

    // then, from the generated C:
    // error: called object type 'i32' is not a function or function pointer

This is the `compiler-hardening.md` failure shape exactly: the checker says yes,
the backend says no, and the message points at generated code. Whichever way
§3.1 is decided, this case wants a diagnostic of its own — either "a local
may not shadow a proc", or, if shadowing is allowed, an error at the *call*
saying the name now refers to a local. Found while auditing the reflect runtime's
`i_` prefix; it has nothing to do with reflection and reproduces with any proc.

C permits the shadow silently and reports it at the call, so the fix is to own
that diagnostic rather than to pick a new rule. See `name-resolution.md`.

### 3.2 C keywords as identifiers

**Today.** Rejected with a diagnostic, added this week — `typedef: i32 = 1`
previously emitted `i32 typedef = 1;` and failed inside the C compiler.

**Options.** Keep the restriction, or mangle on emission so any ilang identifier is
legal regardless of what C reserves.

Rejection was chosen because it is reversible and keeps generated C readable,
which matters if the C is meant to be read as a teaching artifact. But it leaks
the backend into the language surface, and "why can't I name a variable `auto`?"
is a fair question from a student.

> **Resolved: mangle on collision.** The restriction was losing. It was added
> for locals, then parameters, then proc names, then struct names, and globals,
> field names, enum names and aliases were still open after four rounds -- each
> one accepted by ilang and then rejected by clang, pointing at generated code.
> That is a class of bug, not four bugs, and checking positions one at a time
> could not close it.
>
> A reserved name is renamed on the way into C instead: `typedef` becomes
> `i_typedef`. Every identifier passes through one function on its way to the
> output, so declarations and references cannot disagree and there is no next
> position to miss. `_Static_assert` becomes `i_Static_assert`, not
> `i__Static_assert` -- C reserves any identifier containing a double
> underscore to the implementation, so the prefix absorbs the leading one.
>
> **Only on collision.** Mangling every name would cost the readability of the
> generated C, which is the reason for lowering to C at all. For the ~16 names
> affected the C symbol differs from the ilang name; for everything else the
> output is byte-identical.
>
> **`int`, `long`, `short`, `float`, `double`, `signed`, `unsigned` stay
> rejected**, in all eight positions now rather than four. Those are also ilang
> type spellings that pass through to C, so renaming them would make one token
> mean a type in one position and a variable in another. `sizeof` and `alignof`
> stay rejected too: they are operators, not identifiers.
>
> Covered by `sizeof_reserved` and `reserved_c_identifier`, both of which run
> the result -- a rename that missed a *use* would fail to link, but one that
> renamed an enum tag and not its members would build and quietly misbehave.

### 3.4 A call to an undeclared name is not an error *(fixed)*

**Today.** Not checked. The name resolves to nothing, and only the C compiler
objects:

    main: proc()->i32 = {
        return totally_not_declared_anywhere(3);   // `i: checked` passes
    }

    // error: call to undeclared function 'totally_not_declared_anywhere';
    // ISO C99 and later do not support implicit function declarations

This is the most basic resolution check a compiler does, and it is absent. It is
also the third instance of the same shape found in one sitting — alongside
§3.1's proc shadowing and, before it was fixed, every reflection field access.
The pattern is consistent: where ilang declines to resolve a name, the error
reappears in generated code with a message pointing at C.

Presumably it exists so a C function can be called without declaring it. That is
not a bargain real code takes: njinn declares every external explicitly
(`printf: proc(fmt: *const char, ...)->i32 = { external; }`), so the hole buys
nothing and costs the diagnostic. It is the same trade `external` structs used to
make before they were given a field list.

**Options.** Resolve calls and error on an unknown name, which is the obvious
one; or keep implicit calls and require a flag to allow them, so the permissive
mode is opt-in rather than the default. Either way the fix is worth more than it
costs — this is the cheapest possible class of bug to catch and it is currently
escaping to the backend.

Measured and written up in `name-resolution.md`: where the check goes, what
actually breaks (16 names, not the 407 call sites it first looks like), and the
one genuine language question buried in it.

### 3.3 Visibility

**Today.** `static` and `extern` only. There is no module-level public/private
distinction; an imported file's symbols are all visible.

**Needs.** Whether ilang wants export control, and if so whether it is per-symbol or
per-file.
> **Later.** Deferred deliberately; no work planned this round.


## 4. Operators And Expressions

### 4.1 Truthiness

**Today.** `and` and `or` accept arbitrary integers — `n and 1` compiles with
`n: i32`. Real ilang code writes `x != 0 and y != 0` by convention rather than by
requirement.

**Options.** Require boolean operands, or keep C-style truthiness. If real code
already writes the explicit form, requiring it costs nothing and removes a class
of confusion.

> **Decided: keep C's truthiness.** `n and 1` stays legal. The convention of
> writing `x != 0 and y != 0` stays a convention. Listed with the rest of the
> deliberately-unsafe inheritances in [`safety.md`](safety.md).

### 4.2 Arithmetic right shift

**Today.** Right-shifting a negative signed value is arithmetic. C leaves this
implementation-defined. `013-integer-conversion.i` records the behaviour but
that is documentation of what happens, not a decision that it should.
> **Later.** Deferred deliberately; no work planned this round.


### 4.3 Emitted parentheses

**Today.** Every subexpression is parenthesised: `1 shl 2 + 3` becomes
`(1 << (2 + 3))`.

Since the bitwise group was moved above comparison this week, those parentheses
are **load-bearing** — stripping them would let C re-parse `6 & 4 == 4` back
into its old grouping, silently. An earlier idea to emit unparenthesised C for
legibility is therefore withdrawn for binary operators.

A narrower version remains open: omit parentheses only where ilang's precedence
agrees with C's, keeping them where the tables now differ. Cheap to state,
fiddly to maintain, and it would need the precedence test extended to cover
every operator pair.

## 5. Modules And Imports

**Today.** `import "path.i"` brings a file's symbols into scope. `std` is
resolved from beside the compiler executable.

**Measured, not assumed** -- three of the four questions here already have
answers in the implementation:

- **Circular imports are diagnosed**, with the full chain:
  `semantic error: import cycle: a.i -> b.i -> a.i`.
- **Imports are transitive.** If A imports B and B imports C, A sees C's
  symbols. Verified with a *discriminating* test: a plain transitive call
  proves nothing while 3.4 is open, since an unresolved callee is accepted
  anyway. Passing the wrong argument count through two levels does produce
  `proc 'deep' expects 0 args, got 3`, so the visibility is real.
- **Transitive types work too**, not just procs.

**Still open.** Whether importing the same file twice through different paths
is one module or two, and what the generated header is supposed to contain
relative to what the file exports.
> **Later.** Deferred deliberately; no work planned this round.


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
> **Later.** Deferred deliberately; no work planned this round.


## 7. The C Backend Contract

The soul document commits to C ABI interop and predictable layout. That implies
promises the language has not yet written down.

**Needs.** Whether struct layout is guaranteed to match the equivalent C
declaration, including padding. Whether there is control over packing and
alignment. Whether bitfields are supported and how they map — a torture test for
anonymous bitfields exists, so something is there. What calling convention procs
use and whether it can be specified. What the emitted C guarantees, given
"if the emitted C is free of undefined behaviour, it means what the ilang means" is
the strongest correctness claim available, and it is only worth something once
the lowering rules are written down.
> **Later.** Deferred deliberately; no work planned this round.


## 8. Tooling And Diagnostics

**Today.** ~~Preprocessor directives are hoisted to the top of the generated
file, so an inline `#ifdef` around a statement does not work. Real code works
around this with runtime flags.~~

> **Resolved: inline `#ifdef` works.** A directive at file scope still hoists --
> that is what makes `cinclude` and a top-level `#define` work at all. One
> written *inside* a body is now lexed into a `Token_Directive`, parsed as a
> `Stmt_Directive`, and emitted verbatim where it was written; the collector
> tracks brace depth so the same line is never both hoisted and placed. ilang
> does not evaluate the condition, so both arms are parsed and type-checked and
> neither is folded away. Covered by `inline_directives`, which runs the result
> under both settings of a flag rather than only checking that the emitted C
> compiles -- it compiles either way.

> **Resolved: ilang evaluates conditionals; C keeps everything else.**
>
> The `#if` family is evaluated by ilang and never emitted. A dead arm is
> skipped in the lexer, so it is not parsed, not type-checked, and does not
> exist -- the same deal C makes, and what Rust's `cfg` and Zig's `comptime`
> both settle for.
>
> `#define`, `#include`, `#pragma` and the rest still pass through untouched.
> ilang records *which names are defined* so it can answer `#ifdef`, but it does
> not expand macros; that stays C's job.
>
> **What it buys.** Two things that were impossible while directives were passed
> through: omitting a whole declaration, and `#else` at file scope. The second
> is the interesting one -- two arms would otherwise be two declarations of one
> name, and ilang would reject the pair before C ever saw it. Evaluating the
> condition is what makes only one of them exist.
>
> **What it costs, deliberately.** The arm you are not building can rot. Body
> level previously had both arms parsed *and* type-checked, which is stronger
> than either Rust or Zig manage, and that is now given up for consistency and
> for the ability to guard declarations. It was a considered trade, not an
> oversight.
>
> **Defines are seeded program-wide before anything is lexed.** `import` is not
> `#include`, and the entry file is lexed before its imports are known, so a
> `#define` in an imported file would otherwise be invisible to a conditional in
> the file importing it -- which is exactly how njinn's `#define gin_debug_draw`
> lives in `pch.i` and is tested in `gin.i`. A pre-pass walks the entry and its
> imports textually and collects the *unconditional* file-scope defines. Only
> unconditional ones: resolving a `#define` inside an `#ifdef` needs the table
> the pass is building. The rule is that an unconditional file-scope define is
> visible program-wide and a conditional one is visible from where it appears
> onward in its own file, and the approximation errs toward not defining.
>
> **The condition language is small on purpose:** `defined(X)`, `!`, `&&`,
> `||`, parentheses and integer literals. A bare `#if FOO` is refused rather
> than guessed at, because ilang does not expand macros and would have to read
> defined-ness as the value -- so `#define FOO 0` would be true here and false
> in C. Comparison and arithmetic are refused for the same reason.
>
> Covered by `conditionals`, which builds and runs twelve shapes including
> `#else` at file scope, nesting, `#elif`, `#undef`, a dead arm that is not
> valid ilang at all, and a `#define` inside a dead arm that must not take.

**Needs.** The larger question stands:**Needs.** The larger question stands: whether ilang wants conditional
compilation *as its answer*, or something better, given that the soul document
lists "macro accidents" among C's failures. What exists now is C's mechanism,
placed correctly.

## 9. Const And Immutability

### 9.1 `cast` launders `const` away

**Today.** `const` is enforced on assignment, but `cast` is a hole straight
through it, with no diagnostic:

    p: *reflect = cast(Point<>.&, *reflect);
    p[0].size = 999;                          // i: generated

The emitted C is `((i_reflect *)(&(Point_reflect)))`. clang catches it only
under `-Wcast-qual`, which is not on by default. The write does fault at
runtime -- see 9.2 -- so the failure mode is a crash rather than corruption, but
the compile-time gap is real.

This is not a reflection bug. It is general: `cast` from `*const T` to `*T` is
accepted for every `T`. The Settled list below claims `const` is enforced, and
through `cast` it is not.

**Bearing.** Make `cast` from `*const T` to `*T` an error, or require a named,
ugly, greppable escape hatch for the rare case that wants it. Fixing it here
covers reflection for free and repairs a claim this document currently makes
falsely.

> **Parked.** `cast` keeps laundering `const` for now. The named escape hatch is
> the shape the fix would take -- a separate `bitcast` for "reinterpret these
> bits", leaving `cast` able to keep the qualifier. Listed with the rest of the
> deferred checks in [`type-safety.md`](type-safety.md).

### 9.2 Reflection immutability -- settled, no language change

Reflection tables are compiler-generated and must never be mutated. Measured
against the current compiler, this is already true at every level:

- **Deep `const` in the emitted C.** `static const i_reflect_value[]`,
  `static const i_reflect_field[]`, `const i_reflect`, `extern const i_reflect`,
  and every interior pointer const-qualified in `reflect.h` (`const char *name`,
  `const i_reflect *info`).
- **Read-only section.** `llvm-nm` reports the tables as `R`/`r` -- `.rdata`. A
  program that forces a write through segfaults (exit 139). The pages are
  read-only at runtime already.
- **A mutable `*reflect` is unconstructible.** `p: *reflect = Point<>.&` is a
  type error today: *initializer expected `ptr_reflect`, got
  `ptr_const_reflect`*.

**Rejected: lowering `*reflect` to `*const reflect` as a magic exception.**
Proposed to save writing `const`. Four reasons not to:

1. **It deletes the check it is trying to strengthen.** That type error on
   `p: *reflect = Point<>.&` *is* the enforcement. Make `*reflect` silently mean
   `*const reflect` and the declaration compiles; whatever catches the
   subsequent write fires later, further from the cause, or not at all.
2. **Generics.** `proc<T>(p: *T)` instantiated with `T = reflect` -- does the
   magic apply? If yes, `substitute_type_sub` becomes type-dependent and `*T`
   means different things for different `T`. If no, two spellings diverge based
   on how you arrived at them.
3. **It moves magic from the producer into the type system.** `Type<>` is magic
   at one site: the compiler manufactures a table. Once manufactured, `reflect`
   is an ordinary struct with ordinary rules, and that containment is why the
   magic has cost nothing so far. `*T` meaning different things for different
   `T` is not contained -- it is in every signature, every instantiation, the
   LSP, and every error message.
4. **The tedium is smaller than it feels.** 96 `*const reflect*` spellings
   exist; **75 are in `src/std/reflect.i`**, one file written once. All of njinn
   (28K lines) has 11. i-learn has 6. That is a permanent type-system exception
   to save eleven `const` keywords in an entire engine.

The ergonomic complaint is real but wants the general fix in 9.3, not an
exception.

**Still worth doing:** assert the `.rdata` placement in `run_tests.py`, so it
cannot silently regress into `.data` if the emitter changes.

### 9.3 Type aliases exist, spelled `alias`

**Correction.** An earlier draft of this section claimed there was no alias form
at all. That was wrong: it was based on testing `myint: type = i32`, which fails
because the keyword is `alias`, not `type`.

    myint: alias = i32;
    reflectref: alias = *const reflect;

Both check. `bindings/cglm.i` has been using it all along -- `vec2: alias = [2]f32;`.

So the ergonomic complaint behind the rejected `*reflect` magic in 9.2 already
has its general answer in the language, and it needs no new feature:
`reflectref: alias = *const reflect;` covers `*const reflect_field` and
`*const reflect_value` too.

**Still worth recording.** Whether aliases are transparent (a second spelling of
the same type) or distinct (a new type that does not interconvert). Transparent
appears to be what happens today -- `vec3` and `[3]f32` are used
interchangeably -- but it is nowhere written down, which is exactly the condition
this document exists to remove.

## 10. Module Namespaces

**The proposal.** `mem.arena` in ilang source, lowering to `mem_arena` in C.

**Today.** There is no namespace form of any kind. Every spelling of one is a
parse error:

    import "mem.i" as mem      // parse error: expected ':' after identifier
    module mem;                // parse error: expected ':' after identifier

So symbols are namespaced by hand, in C's manner. In njinn, **887 of 1035
top-level procs (86%) carry a hand-written module prefix**, across **29 distinct
prefixes** -- `gin_` (204), `gops_` (201), `fxops_` (106), `guiops_` (59),
`resops_` (52), and so on down to `ma_` and `resio_`. That is precisely the
"repeated C-era bookkeeping" `i-soul.md` says the language should make explicit
and checkable rather than leave to discipline, and it is currently enforced by
nothing but habit.

**A crash, found while checking the above -- now fixed.** Dotted call syntax did
not merely fail to parse, it segfaulted the compiler:

    main: proc()->i32 = { return nosuch.method(); }     // exit 139

The first guess -- "calls through an unresolved receiver" -- was wrong. Six
shapes reached it, including `n.g()` with `n: i32`, whose receiver is perfectly
well declared. The real condition was *any* callee expression with no inferable
type: `type_check_call` passed a null `TypeExpr` to `type_error_call_non_proc`,
which dereferenced it in `type_mangle_impl`.

Fixed, along with two defects found underneath it:

- `type_error_field_access` printed its "cannot resolve base type" diagnostic and
  then fell through to dereference the null it had just reported.
- The pointer arm also fell through, so every pointer field error printed twice
  -- once with the useful `use q[0].bogus` hint, then again as
  `type 'ptr_P' has no field 'bogus'`, which is misleading since pointers have no
  fields at all.
- Field access was checked only on pointers, declared aggregates and reflect
  records, so `n.bogus` with `n: i32` was **accepted silently** and became a
  clang error about generated code. Now reported, but only for types that
  provably have no fields (ilang's scalars and arrays): a name the compiler has never
  seen is a foreign C type from a `cinclude`, whose fields are genuinely unknown,
  and reporting on those rejects njinn. That silence is the type-level twin of
  3.4 and goes away with the same fix.

*Covered by `call_untyped_base` (six shapes, asserting exit 1 rather than a
crash) and `field_access_fieldless` (scalars and arrays report, a `cinclude`d C
type does not, a pointer reports exactly once, a real field still resolves). Both
verified against the pre-fix compiler: the crash cases exit 139, the scalar cases
exit 0, and the pointer case reports twice.*

**Why this is a better kind of magic than the one rejected in 9.2.** The
distinction is where the magic lives. `*reflect` meaning `*const reflect` puts it
in the *type system*, where `*T` starts meaning different things for different
`T` and the effect reaches every signature, every instantiation and every error
message. `mem.arena` puts it in the *surface syntax*: resolved once at name
resolution, lowering one-to-one to a symbol you could have written by hand. It is
also the naming scheme already committed to -- `proc<T>` becomes `proc_T` and
`Pair<i32, f32>` becomes `Pair_i32_f32`, so `mem.arena` becoming `mem_arena` is
the same rule applied to modules instead of type arguments.

**Three things to decide.**

1. **Is the prefix mandatory or optional at the call site?** Required gives
   C++/Rust-grade clarity and changes every existing njinn call site. Optional --
   bare `arena` still resolves -- makes it an alias, and then two modules
   exporting `arena` need a rule for which wins. Optional-with-ambiguity-error is
   the likely answer, but note it **interacts with 3.4**: while an unresolved
   callee is silently accepted, the checker cannot distinguish "ambiguous" from
   "undeclared", so this decision is partly blocked on that one.
2. **Where does the module name come from?** Filename (`mem.i` gives `mem`), an
   explicit `module` declaration in the file, or the import site
   (`import "std/mem.i" as mem`). Filename is the least ceremony; explicit is the
   only one that survives a file rename without breaking callers.
3. **`.` is already field access.** `mem.arena` and `player.health` are the same
   token sequence, separated only by whether the left side resolves to a module
   or a value. Go does exactly this, so it is workable, but the parser cannot
   decide it and the resolver must -- which affects the LSP, and it means every
   "no such field" and "no such module" diagnostic has to know which one the user
   meant. Worth signing up for deliberately rather than discovering.

**Bearing -- weaker than it first looks.** The 86% figure proves the prefixes get
written, not that a feature is needed. Checked for drift and there is almost
none: `fxops.i` is 106 of 111 consistent, `gops.i` 199 of 218, `gin.i` 191 of
212. The files that look like total violations -- `externs.i` (34 of 34),
`os.i` (53 of 58), `pch.i` (7 of 7) -- exist to declare *C* symbols, which no
namespace feature would touch. The discipline is not failing.

What it actually buys, ranked:

1. Renaming a module is one edit instead of 201. Real, rare, and `sed` does it.
2. **Import-site aliasing** (`import "long/path.i" as g`). Manual prefixes cannot
   do this at all, because the prefix is baked into the symbol.
3. **Two libraries you do not control that both prefix `str_`.** With flat names
   you fork one. This is the only decisive argument, and it is
   `stranger-with-generics.md`'s problem, not njinn's.

What it costs:

4. **Grep.** Today `grep gops_update` finds exactly one thing. Under namespaces
   the definition reads `update:` and callers read `gops.update`, so the string
   `gops_update` exists nowhere in the source -- only in generated C. For a
   codebase navigated by grep this is a daily cost against a rare benefit.
5. **A translation tax downstream.** Debugger frames, profiler rows, linker
   errors and crash dumps all say `mem_arena` while the source says `mem.arena`.
   Mechanical and cheap, but "the C you get is the C you would have written" is a
   selling point and this chips at it.
6. The `.` overload: resolver complexity, LSP work, and every "no such field" /
   "no such module" diagnostic has to infer which was meant.

**The reframe.** `std.vec` versus `std_vec` is cosmetic. The question underneath
is whether two strangers' libraries can coexist in one program. If that is the
goal, the feature is (2) and (3) -- import aliasing and module identity separate
from symbol spelling -- and the dotted spelling is incidental. That makes this a
library-ecosystem feature in ergonomics costume, and there is no ecosystem yet.

**So: below 3.4, 9.1 and `true`/`false`.** Build import aliasing when there is a
second author. The segfault above stands on its own and should be fixed either
way.

## 11. Declaration Attributes *(implemented)*

**Today.** `proc[...]` is already parsed and carries a calling convention
(`platform_add: proc[WINCALL](...)`), seven uses across the tree. Structs have no
such slot, and `external` is spelled two different ways -- a pseudo-field inside a
struct body, a statement inside a proc body.

**Proposal.** One attribute slot per declaration:
`struct[external]`, `proc[external, WINCALL]`, `struct[packed]`,
`struct[align(16)]`, `enum[u32]`.

The case is not tidiness. The last three are **parse errors today** with no
syntax at all -- they are §7 (packing and alignment, unstatable in a language
driving a D3D11 renderer) and §2.6 (enum underlying type). One slot answers all
of them. It also dissolves the opaque-struct question: `struct[external] = {}`
with an empty field list *is* the opaque form, so there is no second construct.

**Implemented**, and the tree has migrated: 268 procs and 79 structs, unions and
enums across 30 files. The old spellings still parse, and the test fixtures were
left on them so the legacy form stays covered. `= {}` stays on external procs
(`name : kind = value` with no exceptions) and attributes are comma-separated in
one bracket. Still open: whether attributes take *arguments*, which is what
`struct[align(16)]` and `enum[u32]` need. Full account in `attributes.md`.

## Settled This Week

Recorded so they do not get re-litigated:

- **There will be no methods.** No `P.get: proc(self: *P)`, no receiver syntax,
  no proc bound to a type in any form. Permanent, not deferred. The consequence
  is that module namespaces (§10) are the only remaining answer to njinn's 91%
  hand-written prefix rate, which raises §10 from a cosmetic question to the
  only one on the table.

- **A switch case does not fall through.** It takes a block, so it is
  self-contained. Matches Go, Rust and Zig.
- **Bitwise operators bind tighter than comparison.** C's ordering exists only
  because early C had no `&&`; Ritchie called it a mistake. Verified safe by
  regenerating a 28,301-line engine with zero changed lines.
- **`if` requires a braced body**, which makes the dangling-else ambiguity
  unwritable. Already true; worth stating as intent rather than accident.
- **`const` is enforced** on assignment -- but *not* through `cast`, which
  launders it silently. See 9.1; the claim is only half true today.
- **Reflection is one record, kind-tagged, with a variant payload.** Odin's
  shape, which ports to ilang as-is; Zig's needs language-level tagged unions first.
  This also settled nested type links and made a union its own kind rather than
  a flag. Details in `reflection-issues.md`, which has nothing open.
- **The reflect runtime's C names carry an `i_` prefix**, while ilang source keeps
  the short spelling. Tables are emitted unconditionally, so `reflect` would
  otherwise squat on a common word in every program's C namespace. Not `__i_`:
  C reserves the double underscore to the implementation.
- **Reflected enum values are `i32`.** ilang permits negative members and they must
  round-trip; `u32` would turn `-1` into `4294967295` and break every lookup by
  value. Independent of how the enum underlying type (§2.6) resolves.
- **A `cinclude` brings no names into ilang.** It sends a header to the C compiler
  and nothing else; every C function is declared in ilang before it can be called.
  Implemented, with three consequent rules -- function-like macros are callable
  with an unknown signature, identical `external` redeclarations merge, and
  builtins spelled like calls are exempt. Full account in `name-resolution.md`.
- **A name resolves to its nearest binding, as in C.** A local or parameter
  shadows a proc, and calling it is an error at the call site rather than a clang
  message about generated code. The shadow itself stays legal and silent, which
  is also what C does. Implemented; see `name-resolution.md` 3.1, including why
  the first analysis of this one was wrong.
- **Reflection data is immutable, and needs no new language rule to be.**
  Deep `const`, `.rdata` placement and an unconstructible mutable
  `*reflect` are all already in place. A proposed magic lowering of
  `*reflect` to `*const reflect` was rejected: it would delete the very
  diagnostic that enforces this. Full reasoning in 9.2.

## Suggested Order

Types (§2.2, §2.4, §2.5, §2.6), shadowing (§3.1), undeclared calls (§3.4),
attributes (§11) and inline `#ifdef` (§8) are done. §1, §2.1, §2.3, §4.1 and
§9.1 are decided-and-parked. What is left, in the order worth doing it:

1. ~~Reject enum members that do not fit~~ *(done)* — ilang checks literals and
   implicit sequential values; the generated C carries a pragma that asks clang
   for the constant-expression cases it cannot evaluate.
2. ~~`true`/`false`~~ *(done)* — keywords producing `1` and `0`, so nothing
   downstream had to learn about them.
3. ~~File-scope guards around declarations~~ *(done)* — see §8. ilang evaluates
   conditionals now, which resolved this and `#else` at file scope together.
4. ~~Constructs ilang accepts that clang then rejects~~ — self-referential
   types and zero-length arrays are fixed. A non-void proc that can finish
   without returning stays accepted: that one is C's rule, kept deliberately
   with the rest of [`safety.md`](safety.md), and clang warns about it by
   default. Written up in [`compiler-hardening.md`](compiler-hardening.md).
5. Everything else as it comes up: §4.2, §3.3, §5, §6, §7 are all marked
   **Later**.

Whatever is decided, write it into the lowering table described in
`compiler-hardening.md` and give it a discriminating test. A decision that is
not written down becomes whatever the emitter happens to do, which is how this
document's motivating bug got in.
