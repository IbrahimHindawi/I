# Name Resolution

Two open items from `shape.md` — §3.4 (a call to an undeclared name) and §3.1
(a local shadowing a proc) — with the measurements already taken, so the work
does not have to start by re-deriving them.

Both are the same failure shape, the one `compiler-hardening.md` is about:

> `i: checked` passes, then the C compiler produces the error, pointing at
> generated code the author never wrote.

This is the worst class of defect for a language whose pitch is predictable
lowering, and the worst for teaching: a student's first typo produces a message
about ISO C99 implicit declarations, in a file they did not write.

> **Both settled and implemented.** `cinclude` permits nothing: every C function
> is declared in ilang before it can be called (3.4). And a name resolves to its
> nearest binding, exactly as in C, so a local shadowing a proc is caught at the
> call (3.1). What they cost is recorded below.

## §3.4 — A call to an undeclared name is not checked *(fixed)*

```
main: proc()->i32 = {
    return totally_not_declared_anywhere(3);   // `i: checked` passes
}

// error: call to undeclared function 'totally_not_declared_anywhere';
// ISO C99 and later do not support implicit function declarations
```

Hit three times in one sitting while writing `njinn/src/fxed.i`, against
`sops_skin_state_deinit`, `pacops_character_name` and
`guiops_layout_content_height` — none of which exist. The checker accepted all
three.

### Where it is

`type_check_expr`, the `Expr_Call` branch, immediately after
`lookup_call_proc_decl` fails:

```c
TypeExpr *callee_type = type_scope_lookup(scope, call->name);
if (!callee_type) {
    for (i32 i = 0; i < call->args.length; i++) {
        type_check_expr((Expr *)call->args.data[i], scope, prog, arena);
    }
    return;                 // <- silently accepts an unknown callee
}
```

The diagnostic already exists one branch below: `type_error_call_non_proc`
handles "resolved, but not callable". Only "did not resolve at all" returns
quietly. Firing an error here is a few lines.

### What currently depends on the silence

Measured by instrumenting that branch to report rather than fail, then running
njinn through it. **407 calls resolve to nothing.** That number is the wrong unit
— it is **16 distinct names**, in three groups that want three different answers:

| group | calls | names |
|---|------:|-------|
| ~~language builtin~~ | ~~98~~ | ~~`sizeof`~~ **-- fixed, see below** |
| C preprocessor macros | 131 | `gin_require` (128), `gin_assert` (3) |
| implicit C interop | 178 | `printf` (138), `glm_rad` (14), `glm_vec3_normalize` (9), `glm_vec3_norm2` (3), `_alloca` (3), `va_start` (2), `va_end` (2), `strrchr` (2), `glm_vec2_scale`, `glm_vec2_norm`, `fopen_s`, `fmodf`, `abort` |

The migration cost is therefore **16 declarations, not 407 edits**. `printf`
alone covers 138 call sites and needs one line.

With `sizeof` fixed, **309 calls across 15 names remain** (arithmetic from the
measurement above, not a fresh instrumented run), and all of them are real
questions about C interop rather than compiler artifacts.

### The decision — settled

**A `cinclude` brings no names into ilang.** It arranges for a header to reach the C
compiler and nothing more; naming a C function in ilang is what makes it callable.
Chosen over the two permissive alternatives below, and the migration was smaller
than the raw call count suggested.

**What it actually cost.** The instrumented count was exact: 309 calls across 15
names once `sizeof` was fixed, and the fix was **one new std module plus 11
declarations in njinn**.

- `src/std/cstd.i` -- a new module declaring the C runtime std itself uses
  (`printf`, `fopen_s`, `fclose`, `fseek`, `ftell`, `fread`, `exit`, `memcpy`,
  `memmove`, `memset`, `memcmp`, `strlen`, `strcmp`) and the opaque `FILE`.
  Imported by `memops.i`, `Option.i`, `Print.i`, `Result.i`, `Equal.i` and
  `reflect.i`; transitivity covers the rest of std -- and njinn, whose 95 `printf`
  calls resolved through it without a single edit.
- `njinn/src/externs.i` -- 11 declarations, in the sections that already held
  their neighbours: five cglm entry points, `strrchr`, `fmodf`, `abort`,
  `_alloca`, `va_start`, `va_end`.

An external proc emits call sites only, never a prototype, which is why
declaring C *macros* like `va_start` costs nothing at the C level while letting ilang
check the calls.

### Three rules it forced

**1. A function-like `#define` is callable; an object-like one is not.**
131 of the 309 were `gin_require` (128) and `gin_assert` (3), which are `#define`s
in `pch.i`. Requiring a proc declaration for something that is not a proc is the
wrong answer, so a `#define` whose name is immediately followed by `(` -- cpp's
own rule -- now registers a callable. Its signature is genuinely unknown, since
cpp substitutes tokens and has no types, so it takes any argument list and its
result has no type: `#define TWICE(x) ((x) * 2)` yields a number while
`gin_require(...)` yields nothing, and both have to work. `#define PLAIN 128`
stays a value and is not callable. This removed all 131 with no source edits.

**2. Identical `external` redeclarations merge.** Once every C function must be
declared, two modules that both use `printf` must both declare it -- and a
program importing both would fail on a conflict neither author can see. C allows
a compatible redeclaration for exactly this reason. A redeclaration that
*disagrees* is still an error, and that is not hypothetical: `cstd.i` declared
`fseek`'s offset as `i64` while njinn used C's `long`, and this rule is what
caught it. Non-external procs are unaffected.

**3. Compiler builtins spelled like calls are exempt.** `printfmt` is lowered to
a `printf` during emission and never reaches the backend as a call, so it has no
declaration to find. `sizeof` and `alignof` used to need this too; they are now
their own node kind and cannot reach the call path at all.

*Covered by `call_undeclared`, `call_macro` and `external_redeclaration`. Each
was checked against the pre-change compiler: the undeclared call, the bare
`cinclude`, and the called object-like define were all accepted before.*

### The alternatives, for the record

- **Allow implicit calls behind a flag**, so the permissive mode is opt-in.
- **Parse `cinclude`d headers** to learn what is declared. Real work, and
  `ibind.exe` already exists for binding generation, so this is the expensive
  path for a small gain.

Both were rejected. `i-learn`'s lesson 06 now declares `printf` alongside the
`puts` it already declared, which is better teaching anyway -- that lesson's
subject *is* the C surface and `external`.

### Two smaller things found while measuring

**`sizeof` reached the call path at all — fixed.** 98 of the 407 were not
undeclared calls in any meaningful sense. The parser chose between "type" and
"expression" with a shape heuristic, and when the guess said "not a type" it had
nowhere to put the result, so it built an `Expr_Call` naming a proc that is
declared nowhere. A naive §3.4 would have produced 98 nonsense errors.

The guess could never be right: `sizeof(gin_vertex)` and `sizeof(line)` are the
same shape and only the symbol tables separate them — and njinn uses both
readings heavily, 347 of its 445 sites being a bare name. Because the guess was
unreliable, the operand was never validated at all, so `sizeof(Playr)` passed
`i: checked` and became a clang error about generated code.

Now `sizeof` and `alignof` are builtins with mandatory parens (already required,
so no source changed) producing one node kind, and the operand is resolved after
the symbol tables exist: a type, or a value, or an error. Values are consulted
first, so a local shadowing a type name resolves the way C resolves it, and a
SHOUTING_CASE variable is not mistaken for a foreign C type by the ALL_CAPS
heuristic in `semantic_builtin_type_name`. Compound operands (`*T`, `[N]T`) keep
the old silence; the ambiguity only ever existed for a bare name.

Two adjacent holes closed with it: `sizeof` was the last C keyword still usable
as an identifier, and proc and struct names were not checked against the reserved
list at all, so `typedef: proc()->i32` also passed and then failed in clang.

*Covered by `sizeof_operand` and `sizeof_reserved`.*

**A macro is not a proc — resolved by rule 1 above.** The original note guessed
that `gin_require` could be declared external-variadic. That turned out to be
blocked anyway: a `#define` already registers its name as a global, so an
explicit declaration collides with it. Making function-like macros callable in
their own right was both simpler and closer to the truth.

## §3.1 — A local may shadow a proc *(fixed)*

```
helper: proc(v: i32)->i32 = { return v * 2; }

main: proc()->i32 = {
    helper: i32 = 7;
    n: i32 = helper(3);   // `i: checked` passes
    return helper + n;
}

// error: called object type 'i32' is not a function or function pointer
```

### What C actually does

Measured, not recalled. Compiling the equivalent C:

- **The shadow itself is permitted, silently** — no diagnostic even under
  `-Wall -Wextra -Wshadow`.
- **The call is an error**: `called object type 'int' is not a function or
  function pointer`.

**That first reading was wrong, and worth recording as a mistake.** It said ilang
"has the same rule and is delegating the diagnostic to clang", and that §3.1 was
"largely subsumed" by §3.4 -- that once an unresolved callee was an error, this
would fall out nearly for free. Neither held. Checking after §3.4 landed, the
shadow case was still accepted, because **the name resolves perfectly well**; it
just resolves to the wrong thing. §3.4 never fires on it.

ilang did not have C's rule. `type_check_call` consulted the proc table *before* the
scope:

```c
ProcDecl *decl = lookup_call_proc_decl(prog, call, scope, ...);   // procs first
if (!decl) {
    TypeExpr *callee_type = type_scope_lookup(scope, call->name); // locals second
```

So ilang read `helper(3)` as a call to the proc, while C read the same text as
calling the `i32` local beside it. The emitted C was name-for-name identical to
the source, which makes this worse than a missing check: **the two languages
disagreed about what the program meant.** The claim the backend rests on -- "if
the emitted C is free of undefined behaviour, it means what the ilang means" -- was
false here, and the failure surfaced as clang complaining about generated code.

Today it fails loudly, which is survivable. The dangerous version is two
same-named bindings that are *both* callable, where ilang picks one and C picks the
other: silently wrong behaviour rather than an error.

**The fix.** Prefer the scope binding over the proc table. Procs are never
entered into the `TypeScope` -- it holds locals, parameters, globals,
function-like macros and reflection globals -- so a hit there is by construction
a nearer binding, which is exactly C's rule. If the nearest binding is not
callable, `type_error_call_non_proc` reports it on the `.i` line; if it is
callable (a proc-pointer local), it goes to the indirect-call path that already
existed. Explicit type arguments are left alone, since `f<i32>(x)` is
unambiguously a generic proc call and never a variable.

The shadow *itself* stays legal and silent, because that is what C does -- no
diagnostic even under `-Wall -Wextra -Wshadow`. Only the call is an error.

*Covered by `call_shadowed_proc`, which pairs the two rejections with four cases
that must still check: an unshadowed call, a proc-pointer local, a shadow that is
never called, and a call with explicit type arguments. Resolving the scope first
unconditionally passes the rejections and breaks all four.*

## Order

1. ~~**`sizeof` should not be an `Expr_Call`.**~~ **Done.** Removed 98 false
   positives from any §3.4 work.
2. ~~**§3.4**~~ **Done.** `cinclude` declares nothing; 309 calls migrated behind
   one std module and 11 njinn declarations.
3. ~~**§3.1**~~ **Done** -- and it was not free, nor a consequence of §3.4. The
   name resolved; it resolved to the wrong binding. See above.

Each wants a discriminating test before it is called done, per
`compiler-hardening.md`. The negative cases belong beside
`type_reflect_variant_arm` and `type_multi_param_generics` in
`tests/run_tests.py`; there is no execute-suite case here, since the whole point
is that nothing should reach the backend.

## Why this is worth doing

`i-learn`'s reflection lessons compiled for weeks against `field_count`, a field
that no longer existed, because reflect access was only checked when
`std/reflect.i` happened to be imported. That was the same class of hole, and it
was found by accident. §3.4 is the general case, and it is the single item that
most separates "a language I can hand to someone else" from "a language that
works if you already know what it wants".
