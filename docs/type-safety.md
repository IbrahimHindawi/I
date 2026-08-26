# Type Safety

> **Parked, deliberately.** The compile-time half of [`safety.md`](safety.md):
> what the type checker declines to check. Measured against the compiler as it
> stands, not assumed.

The type checker is stricter than the "it's basically C" reputation suggests.
Worth stating what it *does* catch first, because it changes which of the gaps
below are actually worth closing.

## What it already catches

Each of these is rejected today:

    p: *B = a.&;              // initializer expected 'ptr_B', got 'ptr_A'
    f(1);                     // proc 'f' expects 2 args, got 1
    b: B = a;                 // initializer expected 'B', got 'A'
    f(c);                     // expected 'ptr_i32', got 'ptr_const_i32'

So: pointers are not interconvertible without a cast, structs are nominally
typed rather than structurally, arity is checked, and `const` is part of the
type and is tracked through assignment and argument passing. That last one is
the interesting one -- it means const-correctness is real here, and the gap is
narrower than "there is no const".

## What it does not

### Implicit numeric conversion

    f: proc(x: f32) -> void = {}
    f(3);                     // accepted: integer literal into a float param

    d: f64 = 1.5;
    n: i32 = d;               // accepted: silently truncates to 1

This is C's implicit conversion rule, inherited wholesale. The second one is the
one that bites: it loses information with no cast written anywhere, which is
exactly the thing `cast(v, T)` exists to make visible.

The fix is not "ban implicit conversion" -- widening `i32` to `i64` is fine and
demanding a cast for it would be noise. The fix is to reject the *narrowing* and
*sign-changing* ones, which is a much smaller rule and would have caught the
`f64` case above.

### Enums convert to integers

    E: enum = { A, B, }
    n: i32 = E.A;             // accepted

Enums are integers that happen to have names. This is deliberate for C interop
-- an `enum[external]` has to match whatever C says a value of that type is --
but it means an enum is not a distinct type and cannot be used as one. There is
no "you passed an `E` where an `F` was expected".

### `cast` launders `const`

    c: *const i32 = v.&;
    f(cast(c, *i32));         // accepted

`const` is tracked, and then `cast` throws it away, because `cast` is the C
cast. This one is *decided* -- it stays for now -- but it is worth being clear
that it is the hole in an otherwise real guarantee. A separate `bitcast` for
"reinterpret these bits" would let `cast` keep const, which is the shape the fix
would take.

### `alias` is transparent

    Handle: alias = i32;
    f: proc(x: Handle) -> void = {}
    f(7);                     // accepted

By design: `alias` is a second name for a type, not a new one. It is listed here
because it is the thing people reach for when they want a distinct type, and it
will not give them one. A distinct-type declaration (`Handle: distinct = i32;`
or similar) would be a separate feature, not a change to `alias`.

### Variadic arguments are unchecked

    printf("%s", 12345);      // accepted

Nothing after `...` is checked against anything, and there is no format-string
attribute to check it with. C compilers do this with `__attribute__((format))`,
which ilang has no way to spell -- an obvious candidate for the attribute slot
that [`attributes.md`](attributes.md) already established.

## Priority, if we come back

1. **Narrowing and sign-changing implicit conversions.** Smallest rule, catches
   the `f64 -> i32` case, no new syntax, no runtime cost.
2. **A `format(...)` attribute on external procs.** The slot exists; this is
   plumbing a name through to a C attribute and gets clang to do the work.
3. **`bitcast`, so `cast` can stop laundering const.** Explicitly parked.
4. **Distinct types.** A real feature, not a tightening.

## See also

- [`safety.md`](safety.md) -- the runtime half.
- [`attributes.md`](attributes.md) -- where a `format` attribute would live.
- [`string.md`](string.md) -- the variadic hole and the string plan overlap.
