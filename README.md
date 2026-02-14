# The I Systems Programming Language
## Synopsis
I is just C with modern syntax and meta-programming.
## how to run:
### Modules
- run `git submodule update --init --recursive`
### MSVC
- initalize `cl.exe` using `x64 Native Tools Command Prompt for VS 2019`
- run `scripts\build.bat -mb`
- run `scripts\build.bat -mc`
- run `scripts\build.bat -b`
- run `scripts\build.bat -cr`
### Clang
- run `scripts\build.bat -mb`
- run `scripts\build.bat -mc`
- run `scripts\build.bat -b`
- run `scripts\build.bat -cr`
# I language specification:
```
# variable
num:i32=7;

# struct type
vec2:struct={x:f32; y:f32;};

# generic struct type
arr:struct<T>={len:u64; cap:u64; data:ptr<T>;};

# proc
make:proc(x:i32)->i32={ret x;}

# generic proc
makeg:proc<T>(x:T)->T={ret x;}

# constrained generic proc
makehash:proc<T:hashable>(x:T)->u64={ret hash(&x);}

# usage
a:arr<i32>;
y:i32 = makeg<i32>(3);
h:u64 = makehash<i32>(num);
```
## License
MIT — see LICENSE file.
