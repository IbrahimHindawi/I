# Compiler Hardening

Working notes for the effort to make wrong-code bugs findable. Companion to
`docs/verification.md`, which covers the theory; this one records what has
actually been done and what is still open.

Background: a switch lowering emitted cases without `break`, so an enemy AI ran
its approach case, fell through into retreat, and negated its own movement
vector. It compiled, it ran, and the source read correctly. That is a
**wrong-code bug**, and the suite at the time could not have caught it — the
checks were about whether things compiled, not what they computed.

## Method

Every test targets a **hypothesised fault**, not a feature. "Does switch work"
would have passed the entire time the bug existed: `002-enum-values.i` had a
switch and passed, because every case returned and fall-through was invisible.

So each case is chosen so that the plausible wrong lowering **changes the
answer**. `Mode.Run` hits `default`, which is last, so nothing falls into it and
the bug hides; `Mode.Idle` exposes it. Where a test cannot name the fault it
guards against, it is not earning its place.

Expected values are **hand-computed from first principles**, never blessed.
`--bless` records whatever the compiler currently does, which is exactly how a
wrong-code bug gets enshrined as correct.

The suite is validated by **mutation**: break the emitter deliberately and
confirm the suite goes red. Disabling the `break` emission makes all six
assertions in `008-switch-semantics.i` fire. A suite that stays green against an
injected fault has a hole.

## Done

Execute-suite cases live in `tests/i-torture/execute`, which compiles, links,
runs, and diffs against a recorded `.expected`.

| test | covers |
| --- | --- |
| `008-switch-semantics` | fall-through, default in the middle, `break` inside a loop, empty case, nested switch, sparse and negative labels |
| `009-evaluation-order` | `and`/`or` short-circuiting, one-arm ternary, compound assignment evaluating its target once, argument evaluation counts |
| `010-loop-semantics` | `continue` running the increment, `break` depth, loop-variable scoping, do-while running once, while running never |
| `011-aggregate-value-semantics` | struct assignment copying, deep nested copy through arrays, pass-by-value, return-by-value, struct-in-array, pointer aliasing |
| `012-precedence-and-parens` | explicit grouping surviving lowering, shift vs addition, the bitwise group vs comparison, logical vs conditional, the pack/unpack shape |
| `013-integer-conversion` | narrowing truncation, signed-to-unsigned modularity, unsigned wrap, logical vs arithmetic shift, sign-preserving widening |
| `014-names-and-emission-order` | mutual recursion, forward struct references, sibling-scope name reuse, locals shadowing globals |
| `015-initialization` | partial init zeroing the tail, designated initialisers, `= {}` through nested aggregates, arrays of structs |
| `016-type-surface` | union member aliasing and size, transparent aliases, `sizeof` on an array not decaying, empty `for`, else binding |
| `017-generics-and-reflection` | instantiation independence, repeated and nested instantiation, enum and field reflection, enum-sized arrays |

Negative checks in `tests/run_tests.py`: `switch_no_fallthrough`,
`reserved_c_identifier`, `mangle_collision`.

### Fixes made

**Switch cases no longer fall through.** A `case` takes a block, so it is
self-contained. The emitter appends `break` unless the body already ends in a
jump, which keeps returning cases free of unreachable code. Go, Rust and Zig
make the same call. *Ratified.*

**Bitwise operators now bind tighter than comparison.** C binds `&` `^` `|`
looser than `==` only because early C had no `&&`; by the time it did, changing
it would have broken existing code, and Ritchie wrote it up as a known mistake.
Rust, Zig, Go and Python all fixed it. ilang does not consume C source, so it owed
nothing to that compatibility. Verified safe by regenerating the whole njinn
engine and diffing: **0 changed lines across 28,301** — nothing relied on the old
grouping, because people parenthesise `(a & b) == c` by reflex. *Ratified.*

**C keywords are rejected as identifiers.** `typedef: i32 = 1;` used to emit
`i32 typedef = 1;` and fail in the C compiler, pointing at generated code. Now a
diagnostic on the real source line. Currently enforced for local variables and
proc parameters.

### Verified already correct

Mangling collisions between enum members and globals are caught. Mutual
recursion and forward struct references emit correct declarations. Locals may
shadow globals. Initialisation zeroes unmentioned fields at every depth. Unions
alias their members and size to the largest. `sizeof` on an array does not decay
to a pointer. Generic instantiations are independent. The emitter fully
parenthesises subexpressions. The dangling-else ambiguity is unwritable, because
`if` requires a braced body.

## Pending

Open questions, each of which is a decision rather than a defect.

### Shadowing an enclosing local is rejected; shadowing a global is allowed

Sibling blocks may reuse a name, so scopes are properly nested, but an inner
block declaring a name that an enclosing block already declared is reported as a
duplicate local declaration.

    v: i32 = 1;
    if (v != 0) {
        v: i32 = 20;   // semantic error: duplicate local declaration
    }

C, Go, Rust and Zig all permit this. The asymmetry with globals — which *can* be
shadowed — suggests it may not be deliberate. Three options:

1. keep it, and document "no shadowing an enclosing local" as a rule
2. allow it, matching every other language in the family
3. keep it *and* forbid shadowing globals, for consistency

`014-names-and-emission-order.i` currently pins only the settled part.

### Identifier restriction versus mangling

C keywords are currently **rejected**. The alternative is to mangle them on
emission so that any ilang identifier is legal regardless of what C reserves.

Rejecting was chosen because it is reversible — relaxing a restriction later is
backward compatible, the reverse is not — and because mangling every identifier
would make the generated C harder to read, which matters given it is meant to be
read as a teaching artifact. But it does leak the backend into the language
surface, and "why can't I name a variable `auto`?" is a fair question from a
student.

The check also does not yet cover every declaration site. Locals and parameters
are checked; **globals, proc names, struct and union names, field names, enum
members and type aliases are not.** If the restriction stays, that coverage
should be completed.

### Emitted-parenthesis policy

The emitter parenthesises every subexpression, so `1 shl 2 + 3` becomes
`(1 << (2 + 3))`. With the precedence table now deliberately different from C's,
those parentheses are **load-bearing** rather than redundant — stripping them
would let C re-parse `6 & 4 == 4` back into the old grouping, silently.

An earlier proposal to emit unparenthesised C for legibility is therefore
withdrawn for binary operators. A narrower version remains possible: omit
parentheses only where ilang's precedence agrees with C's, keeping them where the
tables now differ. That is cheap to state and fiddly to maintain, and it would
need `012-precedence-and-parens.i` extended to cover every operator pair.

### Arithmetic right shift is implementation-defined

`013-integer-conversion.i` records that right-shifting a negative signed value
is arithmetic. C leaves this implementation-defined. The test documents the
current behaviour rather than a decision; it should be confirmed as deliberate
and written into the language docs, or changed.

### Documentation drift

`docs/verification.md` and `docs/i-soul.md` both describe ilang as following C's
operator precedence. That is now false and should be corrected before it
misleads a reader — including a student.

## Not yet covered

The largest gap is not a construct but a resolution rule: **a call to a name
nothing declares is not checked**, so it reaches clang and reports there. Three
instances were hit in a single sitting of ordinary porting. Measured and written
up separately in `name-resolution.md`, together with the proc-shadowing case,
which is the same shape.

Constructs with no discriminating test, roughly in the order worth doing:

- **field and enum-member name collisions** with C keywords, and the remaining
  declaration sites listed above
- **varargs** — forwarding, and the promotion rules at the call boundary
- **pointer arithmetic** — scaling by element size, differences, comparison
- **`goto` across scopes** — jumping backward over an initialisation, jumping
  out of a nested block; `003-goto-labels.i` covers the ordinary cases only
- **associativity** beyond the arithmetic cases, especially the right-associative
  conditional: `a ? b : c ? d : e`
- **index expressions with side effects** in multidimensional access
- **`volatile`** — does the qualifier survive lowering, or does the C compiler
  optimise away an access it should keep?
- **`static`** — internal linkage, and statics inside procs if they exist
- **recursion depth and large aggregates** returned by value
- **string and character literals** — escapes, embedded nulls, concatenation

## Still accepted, then rejected by clang

Found by probing rather than by a bug report, so none of these had bitten yet.
All are the same shape this document is about: ilang says the program is fine
and the C compiler disagrees, pointing at generated code. Two are fixed; one
turned out to be a deliberate inheritance rather than a defect, which is why
the entry is still here.

### ~~A type containing itself by value~~ *(fixed)*

> **Fixed.** One walk over type references, rooted at every struct and every
> alias, reporting the path it found:
>
>     error: type contains itself by value, so it has no size;
>            use a pointer to break the cycle 'B -> A'
>
> It turned out to be six shapes rather than the two that were obvious, all one
> root cause -- direct, through an array, mutual, through an alias, through an
> anonymous member, and generic. The walk follows whatever needs a *complete*
> type and stops at whatever does not.
>
> The half that matters as much: a self-pointer, a generic self-pointer, a
> nested generic argument (`Pair<Pair<i32, i32>, i32>`) and a type shared by two
> others all stay legal. A check that rejected a self-pointer would be worse
> than no check, since linked lists are the common case. njinn's 165 records and
> 24 aliases build unchanged.
>
> **Deliberate false negative:** generic type *arguments* are not walked, only
> the base name. Walking them would be unsound without substituting them into
> the generic's fields -- `P: struct = { items: Vec<P>; }` is legal whenever
> `Vec` holds its elements behind a pointer, and rejecting it would be a false
> positive on correct code. A generic that does hold its argument by value is
> still missed, which is where this already was.
>
> Covered by `type_cycles`, which asserts the accepted list as well as the
> rejected one, and checks the diagnostic names the path. Mutation-tested by
> disabling the call.

### A non-void proc with no return *(not a defect: decided)*

    f: proc()->i32 = { }
    f: proc(n: i32)->i32 = { if (n > 0) { return 1; } }
    f: proc(n: i32)->i32 = { switch (n) { case 0: { return 1; } } }

All three are accepted, and stay accepted. This was briefly implemented as an
error and reverted: ilang keeps C's rule here, like the rest of
[`safety.md`](safety.md). clang warns by default with `-Wreturn-type` and
`#line` puts the warning on the real source line, which is the whole bargain --
C does the checking ilang has deliberately not taken on.

Listed here only so it is not "found" a third time.

### ~~A zero-length array~~ *(fixed)*

> **Fixed.** `xs: [0]i32` is now *array length must be greater than zero*. Zero
> uses across njinn, `std` and the test suite, so nothing had to change; the C
> trick it exists for, a flexible array member, has its own spelling.
>
> The related case stands: guarding every field of a struct out with `#ifdef`
> leaves an empty struct, which ISO C also forbids and clang sizes at 4 here
> without a word. That one needs a program to go looking for it.

## Coverage criteria

Two, and the second is the one that means anything.

**Enumerative.** Every one of the 13 statement kinds and 17 expression kinds in
`main.c` has at least one discriminating test. This is a checklist that can be
ticked off against the compiler's own enums.

**Mutation.** Break a lowering deliberately and confirm the suite fails. If it
stays green, that construct has no discriminating coverage regardless of how
many tests mention it.
