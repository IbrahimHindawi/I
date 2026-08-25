# Strings

> **Parked, deliberately.** The plan is to nuke as much C string usage out of
> ilang as possible. This records the target and the measured size of the job so
> the work does not start by re-counting. Nothing here is implemented.

## Where we are

Two string types already exist, in `src/std/string8.i`:

    string8:      struct = { data: *u8; length: u64; capacity: u64; }
    string8slice: struct = { data: *u8; length: u64; }

An owned buffer and a borrowed view, both length-carrying, neither
NUL-terminated by contract. This is the right shape and it is already in use --
the compiler itself is written against it.

And yet, in njinn:

| | count |
|---|---|
| `*c8` / `*const c8` in signatures and locals (spelled `char` in the source) | 536 |
| `string8` / `string8slice` mentions | 91 |
| `strcmp` | 167 |
| `snprintf` | 51 |
| `strlen` | 26 |
| `strncpy` | 17 |
| `strtok` | 11 |
| `strcpy` | 6 |
| `strncmp` | 6 |
| `strchr` / `strrchr` / `strstr` | 8 |
| `sscanf` | 5 |

So the good type exists and the codebase does not use it. Six to one against.

## Why it stayed that way

`string8.data` is `*u8`. Every C string function wants `*const char`. The two are
different types and ilang correctly refuses to interconvert them without a cast
(see [`type-safety.md`](type-safety.md)), so every crossing costs a `cast` --
and once a value has been cast to `*const c8` to call `strcmp`, keeping it
that way is the path of least resistance. The friction points the wrong way.

`strcmp` at 167 uses is not 167 hard problems; it is one easy problem 167 times.
`string8slice_equals` already exists and already does the job. What is missing is
that the values being compared are `*const c8` by the time they reach it.

## The target

1. **`string8slice` is the parameter type.** Anything that takes a string and
   does not own it takes a slice. This is where the 167 `strcmp`s go: they become
   `string8slice_equals`, which is already written, already tested, and does not
   walk off the end of an unterminated buffer.
2. **`string8` is the owned type.** Anything that builds a string builds one of
   these.
3. **`*const c8` survives only at the C boundary**, in `external`
   declarations, and conversion is explicit and one-directional-per-call.
4. **A literal is a `string8slice`.** Right now a string literal is a
   `*const c8`, which is what makes step 1 awkward -- every call site would
   need a wrapper. Changing what a literal *is* is the change that makes the
   rest fall out, and it is also the one that touches everything, which is why
   this is parked rather than half-done.
5. **`snprintf` is replaced, not wrapped.** 51 uses, and it is the one that
   most wants a real answer: a formatter that knows the types of its arguments
   rather than being told about them by a string. `printfmt` already exists in
   the language as a builtin; the question is whether it grows into this or
   whether something else does.

## `char` signedness *(resolved: the type is `c8`)*

The type is C's `char`, and its signedness is the platform's:

    c: c8 = 200;    // -56 here; clang warns about the conversion

Rather than pick signed or unsigned and defend it at every C boundary, ilang
declines to have an opinion and names the type accurately: **`c8` is whatever
the C compiler's `char` is on this target**, width and signedness included. The
name also stops implying "a character", which it never was. `char` remains a
legal spelling and normalises to `c8`, so the two are one type. See `shape.md`
2.5.

That resolves the *naming*, not the hazard: byte arithmetic on a `c8` is still
subject to the platform's signedness. Which is why the rest of this plan matters
more than the rename -- `string8` stores `*u8`, and that choice was already made
correctly. The rule stands: `c8` exists for the C boundary, `u8` is the byte
type, and code that is not talking to C should not touch `c8` at all.

## Related type decisions

**`usize` only.** There is a `usize` and there is deliberately no `isize`. A
size is not negative, a length is not negative, and an index into a buffer is
not negative; the cases that genuinely want a signed offset want `i64` and can
say so. `isize` exists in other languages mostly to paper over pointer
subtraction, which here already has a type: `p - q` yields `long`. Now that
`ptrdiff` exists as a primitive, that is arguably the honest name for the result
-- worth revisiting, but it is a separate change from adding the name.

## Sequencing

The literal type (step 4) gates everything else, and it is a change with a very
wide blast radius across njinn. Nothing before it is wasted, though -- migrating
`strcmp` to `string8slice_equals` at sites that already hold a slice is
independently correct and shrinks the eventual diff.

## See also

- [`safety.md`](safety.md) -- unterminated buffers and out-of-bounds reads are
  the same failure this is trying to remove.
- [`type-safety.md`](type-safety.md) -- variadics are unchecked, which is half of
  why `snprintf` is dangerous.
