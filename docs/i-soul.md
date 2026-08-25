# The Soul Of ilang

This note answers two related questions:

- what makes C feel like C
- what kind of small example suite and book should teach ilang

The short version: ilang should keep C's directness and remove the boring friction
around declarations, generics, reflection, diagnostics, and small project setup.

## What Makes C C

C is not just braces, semicolons, and pointers. Plenty of languages copied those
and still do not feel like C.

C feels like C because the program is close to the machine model:

- memory is bytes
- objects have addresses
- arrays are contiguous
- pointer arithmetic is real
- layout matters
- calling conventions matter
- the generated artifact can be understood as object files and symbols

C also feels like C because compilation is simple in shape. Source files become
translation units. Translation units become object files. Object files link. A
header is a promise between separately compiled pieces. There is not much hidden
runtime story unless the programmer chooses one.

C's abstraction style is lightweight. A `struct` is layout, an `enum` is a set of
integer names, a function is a callable symbol, and a pointer is the bridge
between code and data. The standard library is useful but not the language's
personality. The personality is that the language lets you build the missing
pieces yourself.

C also has a sharp bargain: it trusts the programmer. That trust is productive
when writing systems, tools, engines, runtimes, embedded code, bindings, and
portable libraries. It is painful when simple mistakes become undefined behavior,
macro accidents, header-order bugs, or weak diagnostics.

So the useful answer is:

> C is C because it exposes the machine, compiles as cooperating pieces, keeps
> abstractions inspectable, and treats the programmer as responsible for the
> program's shape.

The bad parts of C are not the soul. They are mostly accumulated friction:

- declarations are noisy and inconsistent
- generic code is usually macro code
- reflection has to be handwritten
- build layout is folklore
- headers can become dependency traps
- diagnostics often arrive after the C compiler sees generated or macro-expanded
  code

ilang should not become "C but safer" in the abstract. That is too vague. ilang should be
"C, but with the common shape decisions made explicit and checkable."

## What ilang Should Keep

ilang should keep these C qualities:

- explicit memory
- predictable layout
- cheap values
- visible addresses
- C ABI interop
- small runtime assumptions
- ordinary C toolchain output
- boring control flow
- source you can lower to C without mystery

ilang should improve these points:

- one declaration shape: `name: Type = value`
- checked imports before generated C is handed off
- generated headers that match the ilang-owned surface
- generics without macro text games
- reflection records without handwritten tables
- good diagnostics at the ilang source line
- a standard library that is just files beside `I.exe`
- a project story where user code runs the installed compiler

That gives ilang a simple identity:

> ilang is a small systems language that keeps C's physical model, uses C as the
> backend and ABI, and makes the repeated C-era bookkeeping explicit enough for
> tools to understand.

## The Cute Program Suite

We should have a small suite of ilang programs outside the compiler repo. The exact
folder can be decided later. The important rule is that the suite behaves like a
real user project: it runs `I.exe` from the installed `i-windows-x64` package and
imports `std` from beside that executable.

This suite should not be a stress test. The compiler repo already has tests. This
suite is for taste, teaching, and regression-by-example.

Suggested shape:

```text
i-examples/
    README.md
    bunyan.py
    src/
        00_hello.i
        01_temperature.i
        02_counting.i
        03_arrays.i
        04_strings.i
        05_structs.i
        06_enums_tables.i
        07_pointers.i
        08_arena.i
        09_array_std.i
        10_print_reflect.i
        11_c_interop.i
        12_tiny_grep.i
        13_tiny_calc.i
    expected/
        00_hello.txt
        ...
```

Each program should be:

- small enough to read in one sitting
- useful enough to teach one idea
- runnable on Windows from a clean shell
- boring about dependencies
- paired with expected output where output matters

The first pass should favor charm over coverage. A good example suite makes a
new language feel approachable before it feels impressive.

Candidate programs:

- `00_hello.i`: print text, return status
- `01_temperature.i`: Fahrenheit/Celsius table, K&R-style
- `02_counting.i`: count chars, lines, words
- `03_arrays.i`: fixed arrays, enum-sized tables
- `04_strings.i`: `string8`, slices, path helpers
- `05_structs.i`: a tiny `Point`, `Rect`, and `Sprite`
- `06_enums_tables.i`: enum members as indexes, `[Kind<>.value_count]`
- `07_pointers.i`: addresses, pointer fields, pointer parameters
- `08_arena.i`: allocate several records from `memops_arena`
- `09_array_std.i`: `Array<T>` reserve/append/use
- `10_print_reflect.i`: derive simple print behavior from reflection metadata
- `11_c_interop.i`: bind one tiny C function or header
- `12_tiny_grep.i`: scan lines and print matches
- `13_tiny_calc.i`: parse and evaluate simple integer expressions

The suite should be cute, but not toy-only. It should gently cross the same
ground C programmers care about: bytes, arrays, text, files, structs, pointers,
allocation, and separately compiled code.

## The K&R-Style ilang Book

The book should be small, practical, and example-led. K&R works because each
chapter gives the reader a working mental model and then reinforces it with
programs.

The ilang version should not try to be a full reference manual. It should teach the
reader how to think in ilang.

Working title:

```text
The ilang Programming Language
```

Possible subtitle:

```text
A small systems language in the C tradition
```

Chapter shape:

1. **A Tutorial Introduction**
   - hello world
   - build and run through installed `I.exe`
   - variables, procs, `printf`
   - temperature table

2. **Types, Operators, And Expressions**
   - integers, floats, chars, bools
   - casts
   - pointer/value distinctions
   - compound assignment
   - what maps directly to C

3. **Control Flow**
   - `if`, `while`, `for`, `switch`
   - case blocks and no accidental fallthrough
   - `break`, `continue`, `return`
   - labels and `goto` where honest

4. **Procedures And Program Structure**
   - declarations
   - parameters and return types
   - static helpers
   - imports
   - generated headers
   - separate C compilation

5. **Pointers, Arrays, And Memory**
   - addresses with `.&`
   - dereference/index rules
   - fixed arrays
   - array decay where applicable
   - enum-sized arrays
   - arenas and explicit allocation

6. **Structs, Enums, And Data Layout**
   - structs as layout
   - nested structs
   - enums and enum members
   - external C enums
   - tables indexed by enums
   - bitfields and anonymous members for C interop

7. **Generics**
   - `Array<T>`
   - generic procs
   - concrete instantiation
   - requirement-style constraints
   - what gets emitted to C

8. **Reflection And Formatting**
   - generated `Type_reflect`
   - field names, offsets, sizes, attrs
   - enum reflection
   - simple debug printers
   - why reflection is data, not magic

9. **Interfacing With C**
   - `cinclude`
   - `external`
   - `external_emit`
   - aliases for C proc pointers
   - calling through vtables and callback fields
   - using existing C headers without owning them

10. **Building Real Programs**
    - the installed compiler package
    - `std` beside `I.exe`
    - Bunyan `mode="i"`
    - generated C under `build`
    - when to inspect generated C
    - when to add project-local import dirs

11. **A Few Complete Programs**
    - tiny grep
    - tiny calc
    - small asset table
    - small C interop demo

The book and suite should be the same artifact from two angles:

- the suite gives runnable programs
- the book explains them in order

Each chapter should end with exercises in the old style:

- change this program
- remove this assumption
- make the table bigger
- add one enum member
- replace a fixed buffer with an arena allocation
- print the struct with reflection
- call a C function instead of an ilang function

The tone should be clear and direct. No mascot. No giant framework. No fake
enterprise app. The reader should finish the book knowing how to write a small
real program, read the generated C when needed, and understand what ilang is adding
on top of C.

## A Useful Standard For The Examples

An example belongs in the suite if it teaches one durable idea:

- how memory is shaped
- how declarations work
- how ilang lowers to C
- how imports and `std` work
- how to call C
- how generic code becomes concrete
- how reflection describes data

An example does not belong if it only demonstrates syntax without a problem.

That keeps the suite small, cute, and honest.

