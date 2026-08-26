# Verifying ilang

This note is about one question: how do we know the compiler produces code that
means what the source says?

It is written against a real failure. While porting a C game engine to ilang, the
compiler emitted switch cases without `break`. An enemy AI ran its "approach"
case, fell through into "retreat", and negated its own movement vector. Every
approaching enemy walked directly away from its target. It compiled without a
diagnostic, ran without a crash, and the ilang source read correctly — because the ilang
source *was* correct.

That bug survived four separate readings of the function against its C original.
Reading source cannot find it. That is what this note is about.

## What This Class Of Bug Is Called

The standard taxonomy for compiler defects:

- **crash bugs** — the compiler dies, hangs, or produces malformed output
- **rejects-valid / accepts-invalid** — the front end is wrong about legality
- **missed optimization** — the output is correct but slower than it should be
- **wrong-code bugs**, also called **miscompilation** — the compiler accepts the
  program, emits code, and that code behaves differently from what the source
  language says it means

The switch bug was a wrong-code bug. More precisely it was a **silent** one, in
that no diagnostic was produced anywhere, arising from an **unsound lowering**:
the ilang construct meant one thing and the C construct it was lowered to meant
another. The defect lived in the gap between the two.

Wrong-code bugs are the expensive category. A crash bug tells you where it is. A
wrong-code bug tells you nothing, and the cost is paid downstream by whoever is
debugging a game that misbehaves for no visible reason.

## Why They Are Hard: The Oracle Problem

To find a wrong-code bug you need an **oracle** — something that independently
knows what the program should do.

"It compiled" is not an oracle. "It did not crash" is not an oracle. Reading the
source is not an oracle, because in a wrong-code bug the source is right and the
compiler is wrong; there is nothing in the source to notice.

This is worth stating plainly because the current test machinery has this exact
shape. `tests/run_i_fuzz.py` documents its own invariants:

    the process exits 0 or 1, never a signal and never a timeout
    --diagnostics=json emits either nothing or one well-formed JSON array
    a failing exit is accompanied by at least one diagnostic

Every one of those is about the compiler's behaviour as a process. None of them
runs the generated program. The fuzzer is a robustness fuzzer, and it is a good
one, but it is structurally incapable of finding a wrong-code bug and always was.
Most of the 283 checks in `tests/run_tests.py` are in the same position: they
assert that something compiles, or that a string appears in the generated C.

An oracle is the thing that is missing, and everything below is a way of getting
one.

## The Root Cause Is A Missing Specification

Ask what `case` means in ilang. Today the only honest answer is: whatever
`emit_stmt` does with it.

When the emitter is the specification, miscompilation is definitionally
impossible — the compiler cannot disagree with itself — and surprises are
guaranteed, because the semantics are discovered by running programs rather than
stated in advance. The switch bug was not a deviation from a spec. There was no
spec. The behaviour was simply not what anyone would have written down if asked.

Verification means checking an implementation against an independent statement of
intent. Without that statement there is nothing to verify, and no amount of
tooling substitutes for it. This is the first thing to fix and everything else
depends on it.

## The Ladder

Approaches to compiler correctness, from cheapest to strongest.

### Testing with a real oracle

Not verification — it finds bugs and never proves their absence — but it is where
the return per hour is highest, and it is the rung the compiler is currently
missing entirely.

**A definitional interpreter.** Walk the typed AST and execute ilang's semantics
directly, independently of how they are lowered. Then run every test program
twice, interpreted and compiled, and compare. Any divergence is a compiler bug,
localised automatically.

This is worth building for a reason beyond testing: a definitional interpreter
*is* an executable semantics. Written this way, "what ilang means" becomes an
artifact that exists apart from `main.c`, can be executed, and cannot silently
rot. It would have caught the switch bug on its first run, because an interpreter
implements "a case is a block" directly and has no C fall-through to inherit.

It also retroactively upgrades every existing test into a semantic test, which is
why it is the single highest-leverage thing on this list.

**Random program generation.** With an oracle in place, generate random valid
programs and compare interpreted against compiled output. This is the Csmith
method, and it is how several hundred wrong-code bugs were found in GCC and LLVM.
The generator must produce programs that are deterministic and free of undefined
behaviour, or the comparison is meaningless.

**Equivalence modulo inputs (EMI).** Run a program, record which statements
executed, then delete or mutate statements that provably did not execute. The
output must be unchanged. Any difference is a miscompilation. The appeal is that
it needs no specification and no interpreter — the unmutated program is its own
oracle.

**A lowering table with adversarial tests.** Finite and systematic: for each
construct, write down the plausible *wrong* lowerings and a test that
distinguishes them.

- switch — does a case fall through, or not?
- for — is the loop variable scoped to the loop, or does it leak?
- and / or — short-circuit or eager? test with a side-effecting right operand
- struct assignment — copy or alias?
- call arguments — what is the evaluation order?
- shl / shr — what happens on signed operands, and past the width?
- integer promotion and conversion at assignment and call boundaries

Each has more than one plausible lowering and only one that matches the intended
semantics. A test that pins the choice is a few lines. The set is bounded.

### Translation validation

The first rung that is genuinely verification. Rather than proving the compiler
correct for all inputs, prove that *this* compilation preserved semantics: for a
given source program, establish that the emitted C is equivalent to it.

The practical form is symbolic execution of both sides with the equivalence
obligation discharged by an SMT solver. Alive2 does exactly this for LLVM IR and
has found a substantial number of real wrong-code bugs in a compiler that is
already among the most heavily tested software in existence.

Its great virtue is that it scales down. Validation of one construct, one
function, or bounded-depth programs yields a real guarantee over that fragment,
rather than nothing until the whole thing is done.

### A verified compiler

CompCert: a machine-checked proof in Coq that compilation preserves the semantics
of the source, for all inputs. This is why CompCert is the only C compiler in
which Csmith never found a wrong-code bug.

It is also person-decades of work, and even then the guarantee is narrower than
it sounds — CompCert's front end, the parsing and elaboration, is substantially
less verified than its back end.

This is not a reasonable target for this project and should not be attempted.

## What Compiling To C Changes

ilang lowers to C, which makes the verification obligation an unusual shape — cheaper
in one way and strictly harder in another.

Cheaper, because the target is a language with a written standard. The obligation
is not "prove correct machine code" but "prove this ilang construct and this C
construct mean the same thing," and there are on the order of forty constructs.
That is a document, not a research programme.

Harder, because **the correctness statement inherits C's undefined behaviour**.
The strongest claim available is:

> if the emitted C is free of undefined behaviour, it means what the ilang means

If a lowering can emit UB — signed overflow, a strict-aliasing violation, an
unsequenced modification, an out-of-bounds access — then the C compiler is
licensed to do anything at all, and every guarantee upstream evaporates.

So **proving UB-freedom of emitted code is part of the obligation**, not an
optional extra. For each lowering rule, the argument must include why that rule
cannot emit UB. This is concrete and checkable, and it is backed cheaply by
running the generated C under UBSan and ASan over the test corpus.

## The Soundness Hole That Exists Today

`external` structs accept arbitrary field names with no checking. This compiles
clean and passes straight through to C:

    Foo: struct = { external; }
    main: proc() -> i32 = {
        f: *Foo = null;
        return f[0].this_field_does_not_exist_anywhere;
    }

So the claim "well-typed ilang programs do not go wrong" is false as things stand,
and the exception is unbounded rather than narrow. This matters more than it
looks: a real engine binds D3D11, miniaudio, cgltf and cglm through this hole,
which is a large fraction of all field accesses in the program with no checking
whatsoever. During the port it allowed reading a field off an opaque struct that
had never been declared anywhere in ilang.

An escape hatch for binding C is necessary. An escape hatch with no declared
shape is not. `external` should carry a field list that is checked, even when the
definition still lives in C.

Verification claims are only worth what the type system underneath them is worth.

## Prevention Beats Detection

Two structural notes, both cheaper than any of the above.

**Lower to a boring subset.** The switch bug was possible only because the C
construct chosen had different default semantics from the ilang construct. Lowering
`switch` to an if/else chain makes C's fall-through unreachable regardless of
whether anyone remembers to emit `break`. The general rule: never depend on a
target construct meaning what the source construct means. Prefer lowerings that
are obviously correct over lowerings that are concise.

**Treat warnings on generated C as compiler bugs.** Clang has a diagnostic for
precisely the bug described at the top of this note:

    warning: unannotated fall-through between switch labels
    [-Wimplicit-fallthrough]
    note: insert 'break;' to avoid fall-through

One caveat worth recording, verified rather than assumed: this is *not* enabled
by `-Wall -Wextra` in C. It must be requested by name. Nothing in the engine
build suppresses it — the build runs at clang's default warning level, which
simply does not include it, and the one place warnings are narrowed carries the
comment "do not hide general generated-code warnings here." The intent is
already right; the flag was just never asked for.

Generated code is exactly where compiler warnings are most valuable, because no
human is reading it. The warning set over emitted C should be explicit and wide,
and any test harness that compiles generated C should not be passing `-w`.

## Order Of Work

1. Write the semantics, as a definitional interpreter, so it is executable and
   cannot rot away from the implementation.
2. Write the lowering table: per construct, the emitted C pattern and a short
   argument for why the two are equivalent. This alone would have prevented the
   switch bug, because the rule would have been written before the code.
3. Close the `external` soundness hole so the type system claims something true.
4. Establish UB-freedom per lowering rule; enforce with UBSan and with
   warnings-as-errors over generated C.
5. Add differential testing against the interpreter, then random program
   generation feeding it, then EMI.
6. Only then consider bounded equivalence checking with an SMT solver.

Steps 1 through 3 are what convert "the compiler is whatever it does" into "the
compiler has a stated intent that can be checked against." Everything that
deserves the name verification is downstream of that. Without them, the more
sophisticated machinery has nothing to verify.
