# Modules

How `--modules` lowers a program to one `.h`/`.c` pair per source file instead of
a single translation unit.

The single-file path is unchanged and remains the default. Module mode is
additive: `I.exe compile entry.i --modules <dir>`.

## What Gets Emitted

    i_types.h        every type declaration, module and monomorphised alike
    i_monomorphs.h   prototypes and reflect externs for each instantiation
    i_monomorphs.c   the one definition of each instantiated body and table
    i_all.h          every module header, in dependency order
    i_<module>.h     that module's public surface
    i_<module>.c     that module's definitions

Module headers carry the `i_` prefix because generated headers sit on the
include path ahead of the vendored C headers. Without it a module named
`cgltf.i` emits a `cgltf.h` that shadows the real one — which is exactly what
happened the first time this ran against a real project.

## Why Types Share One Header

A monomorphised generic can embed a module's type by value: `Box<Point>` holds a
`Point`, so it needs `Point`'s full definition. That module in turn needs
`Box<Point>`. Split per module, that is a cycle.

Keeping every type in `i_types.h` removes it, and costs nothing, because the
compiler already orders struct definitions by dependency for the single-file
build. `emit_concrete_struct_defs_sorted` covers monomorphised structs too.

## Why Monomorphs Have External Linkage

There is exactly one definition of each instantiation, in `i_monomorphs.c`.
Every other unit sees only the prototype. That is what makes the split safe:
no translation unit emits a second copy, so there is nothing for the linker to
collide with.

`static` would be the answer to a different design — one where each unit emits
its own copy — and it would make the definitions unreachable from anywhere else.
The earlier note about `static` monomorph reflect tables applies to that
alternative, not to this one.

## Why `i_all.h` Exists

Module headers include `i_types.h`, `i_monomorphs.h`, and the headers of every
module ordered before them. That is acyclic, because imports are appended ahead
of the file that imports them.

It is not sufficient. Real code has **backward** references between modules: in
the engine this was written against, `memops_pool.i` calls `gin_fatal`, which is
declared in `gin.i` — a module that comes later. No acyclic per-module include
can express that.

`i_all.h` includes every module header in order, and is included only by module
*sources*, never by module headers, so it cannot form a cycle. Module `.c` files
therefore see the whole declaration surface, exactly as the single translation
unit provided.

The consequence is that module mode is a **build change, not a source change**.
A stricter rule — a module may only see what it imports — is the better design
and would have caught those backward references as errors. It also requires the
author to restructure real code before anything compiles. That trade is recorded
in `shape.md` rather than made here.

## What Had To Change In The Compiler

**Import edges are recorded.** `expand_i_imports` flattens depth-first and
discarded the edges. `Program` now carries `module_edge_from`/`module_edge_to`,
recorded before the already-visited check, since the edge is real whether or not
the target was parsed through another importer.

**Static procs get forward prototypes in their module's source.** The public
ones are declared in the header, but a `static` proc called before its own
definition had nowhere to be declared. In the single-file build every prototype
preceded every body, which hid the need.

**Module names may not collide with the generated files.** A module called
`types.i` or `monomorphs.i` is rejected rather than silently overwriting.

## What Had To Change In The Project

One thing, and it was a latent bug rather than a porting cost: `cryptops.i`
declared `cryptops_seek_set` and `cryptops_seek_end` as `static i32` and
`resio.i` used them. Internal linkage read across a module boundary only works
when everything is one translation unit. They are now non-static, which is what
they should always have been — they are shared constants.

## Measurements

Against a 20,000-line engine, 26 modules, on this machine.

| | time |
| --- | --- |
| full rebuild, unity | 2.55s |
| full rebuild, modules | 2.75s |
| incremental, unity | 1.59s |
| incremental, modules | 1.52s |

The end-to-end numbers barely move, and the reason is worth knowing:

| | time |
| --- | --- |
| compile + link after touching one source, unity | 0.77s |
| compile + link after touching one source, modules | 0.35s |
| bunyan's own per-invocation overhead | 0.69s |

**The compile step is 2.2x faster in module mode**, which is the win the split
was for. It is invisible end to end because the build driver's fixed cost —
resource staging, CMake, process startup — is now larger than the compile it is
driving. Optimising that overhead is where the next second lives, not in the
lowering.

Full rebuilds cost slightly more in module mode, as expected: 26 translation
units each pay the per-TU header cost. Ninja's parallelism absorbs most of it.

## Known Characteristics

**Header-only C libraries warn per translation unit.** `jsmn.h` defines `static`
functions; every unit that includes `i_types.h` but does not call them warns
about unused functions. Harmless, and about two warnings per unit. Suppressing
`-Wunused-function` over generated code would silence it, at the cost of hiding
genuinely dead static I procs.

**The whole-program parse still runs on every build.** Collecting instantiations
needs to see everything, so the front end reads the entire program even when one
module changed. At 0.19s for 20,000 lines that is not currently worth avoiding.

**A precompiled module cannot be consumed without its source** if a caller
instantiates a new generic against it. See `stranger-with-generics.md`; nothing
here changes that.
