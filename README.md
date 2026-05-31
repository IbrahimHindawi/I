# The I Systems Programming Language
## Synopsis
I is just C with modern syntax and meta-programming.
## how to run:
### Modules
- run `git submodule update --init --recursive`
### Bunyan
- run `python bunyan.py build debug`
- run `python bunyan.py run debug`
- run `i.bat` to build `I.exe`, generate `src\main.i.c`, compile it, and run it
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
