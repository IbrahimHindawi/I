# How To Stop Me Missing Things On Ports

Written after the fxops/fxed port, which took five rounds of "it's done" before
it worked. This is an honest account of what I got wrong and what would actually
prevent it, rather than a list of good intentions.

## What happened

Five times I reported a port complete. Each time a real defect was still there,
and each time I had "verified" it against a metric that could not see the defect.

| round | metric I used | the bug it could not see |
|---|---|---|
| 1 | every gin symbol exists in njinn | `fxops_step_fixed(dt, null)` -- function present, argument wrong |
| 2 | every `fxops_desc` field is editable | anything outside that struct |
| 3 | folds and widget counts match | a 132-line function ported as 46 |
| 4 | per-function body size | `fxed_theme_load` calling itself; scroll write-back in the wrong order |
| 5 | call sites all wired | fx mode never disabling the half-scale render target |

Every one of those metrics answers *"does the code exist?"*. Not one answers
*"does it run correctly?"*. The defects were, in order: a wrong argument, a
missing caller, a truncated body, infinite recursion, and missing initialisation.
Only the third is something a static comparison could have caught, and I only
found it by accident on the fourth attempt.

## The two things that would have caught nearly all of it

**1. Read the compiler warnings.** clang printed

    warning: all paths through this function will call itself [-Winfinite-recursion]

for `fxed_theme_load` across *multiple* rounds where I declared the port done. I
was checking the build's exit code and nothing else. That one line described a
guaranteed stack overflow on editor launch, which very likely meant none of the
editor work from the preceding rounds was even reachable.

Do this: `-Werror` on the njinn build, or at minimum have the build script fail
when the warning count is non-zero. A warning that is allowed to scroll past is
not a warning.

**2. Run the program.** `i: checked` and a successful link prove the code
compiles. They execute exactly zero frames. Three of the five defects would have
shown up in the first second of running it, and one of them -- the recursion --
would have crashed instantly.

I never ran njinn once during this port. That is the single biggest thing to fix,
and it is entirely on me.

## Why "port it properly" is not the fix

The instinct after a bad port is "be more thorough next time". That is not
actionable and it is not what went wrong. What went wrong is that **I chose a
new completion metric each round and trusted it**, when what I needed was a
metric that could *fail*.

A useful completion check has to be able to distinguish a working port from a
broken one. Symbol lists cannot. Field coverage cannot. Widget counts cannot.
Before using any check to declare something done, the question to answer is:
*what defect would this catch, and what defect would slip past it?* If I cannot
name the second, the check is decoration.

## The order to do a port in

1. **Inventory first.** Enumerate every function in the reference before writing
   any code, with its line count. That list is the checklist and it does not
   move. Building it takes a minute; I built it on attempt four, and it
   immediately showed `fxed_update` at 132 lines against njinn's 46.
2. **Port in file order**, working the checklist. Do not sample.
3. **Diff the call sites, not just the module.** A complete module wired up
   wrong is indistinguishable from a broken one at runtime. `fxops.i` was a
   perfect port for four rounds while three of its entry points were never
   called by anything and a fourth was called with `null`.
4. **Diff initialisation and mode setup.** The last defect was that fx mode
   never turned off the render-scale target. Nothing in either file was missing;
   the *caller's setup* differed. Bootstrap paths, resize handlers and mode
   switches deserve their own line-by-line pass.
5. **Build with zero warnings.**
6. **Run it.** Then say what you saw.

## What to say instead of "done"

The thing that wasted the most of your time was not any single bug -- it was me
reporting completion five times. Each report cost a round trip to disprove.

Better shape:

> Ported X. Verified: builds clean, N checks pass, symbol/field/call-site diffs
> clean. **Not verified: I have not run it.** The things most likely still wrong
> are the init path and anything order-dependent.

That is longer and less satisfying and it would have saved four rounds.

## Repo-specific traps hit during this port

- **fxops reads the global `memory` from `state.i`**, it does not use the one
  passed in. A test that initialises a local `os_memory` faults on the first
  push.
- **The scroll region has to be re-begun after the content is laid out**, with
  the real height, and `scroll_y` written back *after* the drawbar -- the bar
  moves it when dragged. Writing it back straight after `apply_wheel` silently
  discards every drag.
- **`gin_on_resize` reapplies the saved postprocess settings**, so any mode that
  wants different settings has to reapply its own afterwards.
- **A `#define`d name registers as a global** but is not callable unless it is
  function-like; an explicit declaration of the same name collides with it.
- **njinn is frequently better than gin**, and matching gin literally is the
  wrong move: reflection instead of X-macro tables, `resio` instead of `fopen`,
  generics instead of duplicated parsers, unload folded into load so a call site
  cannot forget it. A diff flagging these as "missing" is flagging an
  improvement. Check what the difference *does* before erasing it.

## Tooling that exists, and what is still missing

`tools/gin_fulldiff.py` compares numeric literals, string literals and callee
names per function. It is good and it found real bugs. Its blind spots, all hit
here: argument values, statement order, initialisation, and reachability.

Added during this port: `tests/fxops_emit_selftest.i`, which walks the chain an
effect must travel before a vertex is possible -- packages load, registry
rebuilds, anim resolves to a desc name, name resolves to an id, desc has effects
-- across every character and anim. Any break in that chain means nothing renders
regardless of how correct the drawing code is, which is precisely the failure
that kept getting reported as "no fx".

Still missing, and worth building: a headless frame runner. Something that
initialises the engine, steps N frames, and asserts that counters moved --
`draw_vert_count > 0`, `mesh_draw_count > 0`. That needs a device or a null
backend, which is real work, but it is the only check that would have caught
every defect in the table above.
