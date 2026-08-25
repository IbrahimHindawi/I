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

## 1. Discarded results, and the open question behind them

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

### `Result<T>`: open, not decided

`std` has one. njinn uses it three times, against 303 procs returning `b32` and
199 `gin_fatal` / `gin_require` sites. The temptation is to read that as the
language failing the codebase. It is at least as likely to be the opposite:
`i-soul.md` says ilang keeps C's directness, C's error model *is* bool, sentinel
or abort, and for a game where a missing mesh is unrecoverable, aborting with a
message is the right answer -- wrapping it in a `Result` you would unwrap and
then abort on is ceremony.

So njinn is weak evidence either way. It is one codebase in one domain, and that
domain happens to be the one where C's model works best. Code where failures are
*recoverable and need context* -- tools, servers, anything with a caller that can
do something useful with "why" -- is where the answer would differ, and ilang has
none of that yet.

**Why it is unused today**, regardless of which way the question falls:

- **Nothing forces a check.** `is_ok` is a function you may skip; `.value` reads
  fine without looking at `.ok`. Without must-use, `Result` is a convention,
  which is most of what separates it from C.
- **No propagation form.** Rust's `?` and Zig's `try` are what make this survive
  contact with real code. Without one, every call site is four lines -- and 303
  procs chose a bool instead.
- **`unwrap` calls `exit(1)`**, which is fine in a program and unusable in a
  library.
- **The error is an `i32`.** No message, no context, no chain.

### The cheap way to find out

Do **must-use** first. It is worth having on its own evidence -- 145 discarded
statuses -- and it happens to remove the first and largest of the four blockers
above. Then look again: if `Result` is still unused with checking enforced, that
is a real answer rather than a guess. If it starts getting used, the propagation
form has earned its argument.

That ordering costs nothing and decides the question with evidence instead of
taste. The expensive piece is the propagation form, because it is a control-flow
construct and changes the language's character; it is also the piece that should
be decided last.

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
- **No multiple return values.** Combined with no `Result`, anything returning
  both a value and a status uses an out-parameter.

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

**Open, and deliberately last:** whether `Result<T>` becomes the way errors are
done, and whether that brings a propagation form and a richer error type. Item 2
is the cheap experiment that would answer it -- see §1.
