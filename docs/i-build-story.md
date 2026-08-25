# The Build Story

What it actually takes to start an ilang project, measured rather than intended.
Written after walking a new project from an empty directory with nothing but the
package on `PATH`.

## The short version

The compiler's front door is clean: two commands, no project files. The building
around it is copy-paste: the build driver is not shipped, so every project
vendors its own copy, and there are three of them that have already drifted.

The destination is a build script written in ilang (`build.i`), which is a long
way off and blocked on something that is not yet on the open list -- ilang has
no error-handling story, and every call a build system makes can fail. The one
piece worth taking now is `I build`, which makes the simple case a single
command and is step one of the destination regardless. Details at the bottom.

## The minimum, and it works

With `I_HOME` pointing at the package directory and that directory on `PATH`:

    I.exe compile main.i -o main.c --no-header
    clang main.c -I %I_HOME% -I %I_HOME%\std -o main.exe

That is the whole thing. No CMake, no build script, no manifest. Worth
protecting -- a language you can try in two commands is a different proposition
from one that needs a project generator first.

Two details that are not obvious:

**`I.exe` never reads `I_HOME`.** It locates `std` relative to its own
executable (`GetModuleFileNameA`, then `exe_import_root`). Having the package on
`PATH` is enough for the compiler. `I_HOME` is read only by the build driver,
`scripts/bunyan.py`, to find `I.exe` and `ibind.exe`.

**You need both `-I` paths, and the second one only bites later.** The generated
C opens with

    #include <core.h>
    #include <reflect.h>

which `-I %I_HOME%\std` satisfies. But the moment a program does
`import "std/memops.i"`, the generated C also contains

    #include "std/memops_os.h"

which is relative to the *package root*, not to `std/`. With only the first
`-I`, that fails as `fatal error: 'std/memops_os.h' file not found` -- a header
the author never wrote, in a file they did not write either. It is the exact
failure shape `compiler-hardening.md` is about, wearing a build-configuration
hat.

## Imports need the `std/` prefix

`import "memops.i"` does not resolve; `import "std/memops.i"` does. Imports are
resolved relative to the importing file and to the compiler's own directory, and
the package puts the library under `std/`, so the prefix is part of the name.
The diagnostic is honest about it -- it prints the absolute path it tried -- but
the name still reads like it should work without the prefix.

## Where it stops being two commands

Any real project wants incremental builds, a link line, and probably a resource
step, so it reaches for `bunyan.py`. That is where the story degrades.

### The build driver is not shipped

The package is exactly four things:

    I.exe   ibind.exe   libclang.dll   std/

`scripts/bunyan.py` is not among them. So every project carries its own copy:

| copy | lines |
|---|---|
| `i/scripts/bunyan.py` | 452 |
| `njinn/scripts/bunyan.py` | 482 |
| `gin/scripts/bunyan.py` | 463 |

Three copies, three lengths, and they have genuinely diverged -- njinn's has the
`--modules` support (one `.h`/`.c` pair per module instead of a unity build)
that the ilang repo's copy does not. A fix in one does not reach the other two,
and nothing reports the drift.

### The CMake contract is hand-copied and written down nowhere

A project's `CMakeLists.txt` has to agree with the driver about five variables:

    BUNYAN_I_ENTRY          the .i file to compile
    BUNYAN_I_GEN_DIR        where generated C lands
    BUNYAN_I_GENERATED_C    the generated C to build
    BUNYAN_I_COMPILER_DIR   added to the include path
    BUNYAN_I_STD_DIR        added to the include path

and about a rule that is not visible from either side: `BUNYAN_I_GENERATED_C` is
a single path in unity mode and a `;`-separated *list* in module mode. That is
an interface between two files that both live in the user's project, described
in neither, and reconstructed by copying a project that already works.

### There is no `i init`

Starting a project means finding one that builds, copying its `scripts/`
directory and `CMakeLists.txt`, and editing them. Roughly 550 lines of
scaffolding before the first line of ilang. This is how the three drifted copies
came to exist -- not carelessness, just the only available path.

## Smaller things noticed on the way

**A bare `I.exe` compiles something.** Run with no arguments it does not print
usage; it produced `build/i_gen/main.c` and `build/i_gen/main.h`. `I.exe help`
prints a good usage message, so the no-argument case is the odd one.

**The legacy CLI is still live.** `I input.i output.c output.h` and
`--check` / `--symbols=json` / `--lsp=json` all still work alongside the
subcommand form. Two grammars for one tool, and the help text documents both.

## Where this is going

Four tiers, in the order a project grows:

| tier | what you write | status |
|---|---|---|
| `I compile` + `clang` | nothing | works today |
| `I build main.i` | nothing | **does not exist**; small |
| `build.i` | a build script, in ilang | does not exist; large |
| `bunyan.py` | a Python build script | works today; the thing being replaced |

### Tier 2 is `I build`, not `i init`

The obvious middle tier is a project generator, but generating scaffolding just
moves the problem into a file you now own. A "simple project" still needs
something to run compile-then-link and decide what is stale.

The better middle tier is **no file at all**: `I build main.i` transpiles,
invokes the C compiler with the two `-I` paths it already knows, links, and
produces an executable. That makes the simple case genuinely simple and removes
the include-path papercut above, which is the compiler asking the user to
re-derive something it had in hand.

`i init` then only earns a subcommand once you actually need a build *file*,
which is the serious tier -- and by then it is generating maybe eight lines.

### What `i init` would generate, when it does

    main.i          a program that compiles
    build.i         the build script
    .gitignore      build/

and nothing else. In particular the `CMakeLists.txt` should not be one of them:
generating it into `build/` at configure time is what stops the `BUNYAN_I_*`
contract above from being an interface between two files the user owns.

No dependency fetching -- there is no ecosystem, and `stranger-with-generics.md`
argues the library questions are premature anyway. No `git init`. One template,
refusing to overwrite anything that already exists.

### Why the build script should be a program, not a manifest

njinn is the argument. Its build runs a resource cooker, a bindgen pass over
`cgltf.h`, and a metadata generator, all before the ilang compile. A declarative
manifest would either not express that or grow into a bad programming language.
The escape hatch has to be there from the start.

## `build.i`: what it needs that does not exist

A build script written in ilang has to read a source tree, decide what is stale,
run a C compiler, and write output. Here is std's entire contact with the
outside world today:

    printf
    fopen  fclose  fseek  ftell  fread
    exit   abort
    memcpy memmove memset memcmp
    strlen strcmp
    malloc realloc
    a clock

So: **no process spawn, no directory listing, no stat or mtime, no mkdir, no
environment access -- and no file write at all.** There is `fread` and no
`fwrite`. A build system needs every one of those.

njinn proves the pattern works -- it declares 177 externals including
`CreateDirectoryA` -- but those are a Win32 dump living in a game, not a
portable surface living in `std`.

### The blocker is error handling, not the syscalls

Every one of those calls can fail, and ilang has no design for that. `Result<T>`
exists in `std` and nothing uses it. Writing the OS layer first would mean
settling the error story under pressure from a build system rather than on its
own terms, and that decision is not even on the open list in `shape.md` yet.

That is the honest reason not to start this now.

### The reason to do it eventually

Dogfooding, and a better argument than "self-hosting is nice." A build system is
the first ilang program that would be forced to have an opinion about errors,
strings (parked in `string.md`), file IO and process handling -- the four
weakest areas. It would surface real gaps faster than any number of torture
tests, because it is a real program with failure modes someone has to live with.

### Distance

- **`I build`** -- small. The compiler already knows where `std` is and what the
  generated C needs. Mostly plumbing, plus deciding how the C compiler is
  located and configured.
- **OS primitives in `std`** -- medium, and the cost is design rather than
  typing. Perhaps 20 calls behind an ilang-shaped API, gated on the error story.
- **`std/build.i` plus bootstrapping** -- large. bunyan's logic ported, plus the
  chicken-and-egg that a build script must be compiled and linked before it can
  run, plus a cache so that does not happen every invocation.

Note that `I build` is not a detour on the way to `build.i` -- compiling and
linking a build script into something runnable *is* the `I build` operation.

## Recommendation

**Take `I build`. Leave the rest.**

It is small, it is useful immediately, it removes the two-`-I` papercut, and it
commits to nothing.

**Do not ship `bunyan.py` in the package.** An earlier draft of this document
recommended exactly that, on the grounds that three drifted copies is a real
problem. That advice is withdrawn: if `build.i` is the destination, shipping
bunyan is investing in the layer being replaced. The three copies are annoying
rather than harmful while there is one real project, and they stop mattering
entirely when `build.i` lands.

Both halves of that get more expensive with a second real project, which is the
event that should reopen this.
