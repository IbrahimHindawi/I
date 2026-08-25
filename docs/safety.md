# Safety

> **Parked, deliberately.** ilang is as dirty and unsafe as C for now. This
> records what that costs so the bill is legible when we come back to it, not to
> argue that it should be paid today.

Every item below is a way an ilang program can be wrong at runtime with the
compiler saying nothing. None of them is an oversight -- each is a place where
we chose C's behaviour, and each has a price we are choosing to defer.

## The list

### Out-of-bounds indexing

    xs: [4]i32 = {};
    xs[9] = 1;          // accepted; writes past the array

The length is in the type. `[4]i32` knows it is four elements, and the index is
a constant, so this particular case is decidable at compile time and we decline
to decide it. Non-constant indices need a runtime check, which is the part that
costs.

### Integer overflow

    a: i32 = 2147483647;
    b: i32 = a + 1;     // undefined behaviour, inherited from C

Signed overflow is UB in C, and ilang lowers to C, so it is UB here. Unsigned
wraps. Neither is diagnosed.

### Reading uninitialised memory

`= ?` is the explicit opt-out, and that part is fine -- it is a declaration that
the contents are indeterminate, which is exactly what it lowers to. The gap is
that reading such a variable afterwards is not tracked. The annotation marks the
declaration, not the reads.

### Division by zero

    n: i32 = d / 0;     // accepted

Constant zero divisors are decidable and undiagnosed. Non-constant ones need a
runtime check.

### Null dereference

    p: *Thing = null;
    p[0].field = 1;     // accepted

There is no non-nullable pointer type, so there is nothing to check against. A
pointer either points at something or it does not, and the type does not say.

### Lossy conversions

`cast(v, T)` is the C cast: it will silently drop the high bits of a `u64` on
its way into a `u8`, and silently reinterpret a negative `i32` as a large `u32`.
The cast is explicit, so this is at least *written down* at the point it
happens, which is more than C's implicit conversions give.

### Integer truthiness *(decided: keep it)*

`if (n)` on an integer is accepted and means `n != 0`, and so does `n and 1`.
This is C's rule and ilang keeps it deliberately. It is also how a mistyped
comparison turns into a condition that is always true -- the cost is accepted,
not overlooked. Real code writes `x != 0 and y != 0` by convention, and that
stays a convention rather than becoming a requirement.

## Why it is parked

The safe versions of most of these are not hard to implement -- they are hard to
implement *without changing what the language is for*. ilang exists to generate C
that a person can read, for a codebase (njinn) where the generated C is compiled
with the same expectations as hand-written C. Bounds checks and null checks are
runtime code that would have to appear in that output, and a `-fno-checks` flag
that turns them off is a second language, not a safety feature.

The order they would be worth doing in, if we do:

1. **Constant-index out-of-bounds, constant division by zero.** No runtime cost,
   no new types, no new syntax -- purely a compile-time check on cases that are
   already fully decided. This is the free one.
2. **Lossy `cast` diagnostics.** A warning, not an error, on casts that can lose
   information. Still no runtime cost.
3. **Non-nullable pointers.** A type-system change, so it costs a lot more, but
   it is the one that would catch the most real bugs.
4. **Bounds and overflow checking.** Runtime cost. Needs the debug/release split
   to be a real design decision rather than a flag.

## See also

- [`type-safety.md`](type-safety.md) -- the compile-time half of the same
  question: what the type checker declines to check.
- [`string.md`](string.md) -- `*const char` is where several of these meet at
  once.
