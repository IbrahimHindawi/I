# The Stranger With Generics

What happens when someone else uses an I library, and their code instantiates a
generic the library author never wrote down.

This is a **future** problem. Nothing in the current setup hits it, and the
reason is recorded below so the question does not get re-opened without cause.
It is written up because it is the first thing anyone will ask the day I has a
second user, and because the answer shapes the module design.

## The Problem

A library defines a generic container:

    // stack.i, written by someone else
    Stack: struct<T> = {
        items: [8]T;
        count: i32;
    }

    Stack<T>push: proc<T>(s: *Stack<T>, v: T)->void = { ... }

A stranger uses it with a type the library has never heard of:

    import "stack.i"

    Point: struct = { x: i32; y: i32; }

    numbers: Stack<i32> = {};
    points: Stack<Point> = {};

Monomorphisation means the compiler generates a concrete `Stack_i32_push` and a
concrete `Stack_Point_push`. Somebody has to decide where those land, and each
one has to exist exactly once in the final program.

If two separately compiled translation units both contain `Stack_i32_push`, the
link fails:

    lld-link: error: duplicate symbol: A32_reserve
    >>> defined at tu1.o
    >>> defined at tu2.o
    lld-link: error: duplicate symbol: A32_reflect

Note the second one. Reflection tables are ordinary `const` globals, so they
collide the same way the code does.

## Why C++ Has This Badly And I Does Not

C++ has no whole-program step. It inherited C's translation-unit model, which
was designed so a compiler could work on one file at a time on a machine with
64KB of memory. Each `.cpp` is compiled in isolation, so each one instantiates
`vector<int>` independently, and duplicates are unavoidable. C++ therefore needs
**COMDAT / `linkonce_odr` linkage** so the linker can silently fold them.

C++98 tried to fix this properly with the `export` keyword, which would have
allowed separately compiled templates. One implementation ever shipped it, it
was brutally complex, and it was removed in C++11.

I is in a different position. `import` already reads the entire program, and
20,000 lines parse in under 0.2 seconds. So the compiler can simply **collect
every instantiation and emit each one once**, into a single generated unit. No
folding, no special linkage, no COMDAT.

That is not the C++ bargain reproduced. It is the step C++ could not take.

## When It Actually Bites

Only in one situation: a library ships **precompiled** object code containing an
instantiation, and the consumer's program also instantiates the same thing. Then
there are two copies — one in the shipped `.obj`, one in the consumer's
generated monomorph unit — and the link fails.

Everything else is fine:

- one program, however many modules — the whole-program sweep sees all of it
- a library shipped as **source** — its generics are compiled as part of the
  consumer's program, so there is exactly one copy of each instantiation
- a library shipped as `.h` + `.obj` for its **non-generic** code — no
  instantiations in the object file, nothing to collide with

## Does It Apply Today

No, and it is worth being explicit about why, because the machinery for solving
it is not free.

- **A game** is one program. Whole-program compilation covers it entirely.
- **Teaching** hands students `.i` files. That is source.
- **Open-sourcing** a library hands over source too.

The problem requires distributing a *closed-source binary* I library that a
stranger compiles against. Until that exists, this document describes a
situation nobody is in.

## Options, If It Ever Applies

**A. Ship generics as source; ship everything else compiled.** The library
travels as `.h` + `.obj` for its ordinary code, plus the `.i` for its generic
definitions. The consumer's compiler generates every instantiation once. This
is the default and it needs no new machinery — it is what already happens.

It is also what Rust, Go and Zig do, and what C++ does in practice, since a
header containing a template *is* source.

**B. Declare provided instantiations in the shipped header.** The library states
that it already provides `Stack<i32>`, and the consumer's sweep skips emitting
it. This is `extern template` under another name. Precise and cheap at runtime;
the cost is that the author maintains the list, and a consumer who wants
`Stack<TheirType>` is out of luck unless the generic source ships too.

**C. Weak or COMDAT linkage.** `__attribute__((weak))`, `__declspec(selectany)`.
Reproduces C++'s behaviour exactly: duplicates are permitted and folded, and
every copy resolves to one address.

The portability objection is weaker than it first appears — the emitted C
already uses `__alignof__` in every reflect table, so it is not
strictly-conforming ISO C today. This would not be a new category of
compromise, only more of an existing one.

**D. Type erasure.** One compiled implementation over `void *` plus size and
alignment, in the shape of `qsort`. Any type works with no source and no
duplication, at the cost of monomorphisation's performance and of type safety at
the C boundary. Worth considering only as an opt-in container in `std`, never as
the default — adding erasure to the language proper is a step toward being
C++-shaped.

## Recommendation

Default to **A**. It requires nothing to be built, it matches what every modern
language actually does, and it degrades gracefully.

Keep **C** in reserve for the case where a library genuinely must ship compiled
generics. Reach for **B** only if a library has a small, fixed generic surface
and a strong reason to stay closed. Treat **D** as a library feature, not a
language one.

## The Related Decision, Already Made

Monomorphised reflect tables can use `static` linkage, which means duplicate
copies across translation units are harmless. This holds because **reflect
addresses are read through, never compared**.

Verified against a real codebase: 61 uses of `Type<>.&` across four modules, all
of them passing the address to a reader such as

    gin_reflect_enum_name: proc(meta: *const i_reflect_enum, value: i64)->*const char = {
        if (meta == null) {
            return "unknown";
        }
        for (i: u64 = 0; i < meta[0].value_count; i += 1) {
            if (meta[0].values[i].value == value) {
                return meta[0].values[i].name;
            }
        }
        return "unknown";
    }

The only pointer comparison is against null. Every other use dereferences and
reads. Duplicate copies hold identical contents, so no reader can distinguish
them.

**So `&Type<>` is not guaranteed unique across a program.** If that ever needs to
change — a runtime type tag compared by address would need it — the answer is
option **C**, and this note is where to start.
