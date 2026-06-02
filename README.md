# The I Systems Programming Language

## Synopsis
I is C-shaped code with a smaller syntax surface and compile-time metaprogramming. It transpiles to C, then the normal C toolchain takes over.

## How to Run

### Modules
- run `git submodule update --init --recursive`

### Bunyan
- run `python bunyan.py build debug` to build the `I.exe` compiler
- run `i.bat` to build `I.exe`, translate `src\main.i` to `build\i_gen\main.i.c`, compile that C, and run it
- run `i.bat path\to\file.i build\i_gen\file.c` to translate a different source file

### Compiler CLI
- `build\I.exe [input.i] [output.c]`
- defaults: `src\main.i` -> `src\main.i.c`
- the compiler also writes a companion header beside the output C file, for example `build\i_gen\main.i.h`
- `import "foo.i"` emits `#include "foo.h"` and lets the current file type-check against `foo.i` declarations
- `i.bat` keeps generated C in `build\i_gen` so normal editing stays in `.i` files

### Tests
- run `python tests\run_tests.py`

## Language Sketch
```i
import "stdio.h"
define("SAHA_IMPLEMENTATION")

# variable
num:i32 = 7;

# struct type
vec2:struct = { x:f32; y:f32; }

# enum type
Color:enum = { Red, Green, Blue }

# generic struct type
array:struct<T> = {
    length:u64;
    border:u64;
    data:*T;
}

# proc
make:proc(x:i32)->i32 = { return x; }

# generic proc
makeg:proc<T>(x:T)->T = { return x; }

# requirement implementation for i32
hash:proc<i32>(x:*i32)->u64 = { return hash_fnv1a(x, sizeof(x[0])); }

# constrained generic proc
makehash:proc<T:hash>(x:T)->u64 = { return hash<T>(&x); }

# usage
a:array<i32> = {};
y:i32 = makeg<i32>(3);
h:u64 = makehash<i32>(num);
printf("num = {}\n", num);

# boring C control flow/operators
Packet:struct = {
    values:[4]i32;
    flags:u32;
}

WinProc:proc[WINCALL](value:i32)->i32 = {
    while (value > 0) {
        value -= 1;
        value %= 8;
        if (value == 2) { continue; }
        switch (value) {
            case 1: break;
            default: break;
        }
    }
    return (value shl 1) | 1;
}
```

## License
MIT - see LICENSE file.
