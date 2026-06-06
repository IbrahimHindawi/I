# I Stdlib Container Port

This document tracks the pure-I std/container port from haikal's `meta_arena` templates.

## Scope

- [x] Add `Option<T>` in `src/std/Option.i`.
- [x] Add `Result<T>` in `src/std/Result.i`.
- [x] Port `Array<T>` from haikal meta_arena to `src/std/Array.i`.
- [x] Port `Vec<T>` from haikal meta_arena to `src/std/Vec.i`.
- [x] Port `Node<T>` and `BiNode<T>`.
- [x] Port `List<T>` and `DList<T>`.
- [x] Port `Queue<T>` and `Stack<T>`.
- [x] Port string-key `Map<T>` and `MapIterator<T>`.
- [x] Add equality-based `List<T>remove` / `DList<T>remove` using bytewise generic equality.
- [ ] Decide whether `Result` should become `Result<T, E>` after multi-parameter generics exist.
- [ ] Decide whether `Map_T0_T1.h` should come back as `Map<K, V>` after multi-parameter generics exist; the existing haikal file is malloc-based/incomplete, not a meta_arena port target.
- [ ] Replace remaining haikal-generated meta_arena container usage in downstream projects with std imports.

## Current Language Notes

- I currently supports one generic type parameter for structs/procs. `Result<T>` uses `i32 error` for now; `Result<T, E>` requires multi-parameter generic support.
- The current initializer surface supports `{}`, positional initializer lists, `.field = value`, `[index] = value`, nested initializer lists, and typed expression-level compound initializers like `Payload{.x = 1}` / `Array<i32>{}`.
- Runtime ports should prefer arena-backed allocation and avoid malloc/free/_strdup.

## C Feature Cleanup Watchlist

- [ ] Multi-parameter generics for `Result<T, E>` and future map key/value containers.
- [ ] Decide whether `true`/`false` should be language literals or whether stdlib should consistently use `1`/`0` for `bool`.
- [x] Add typed compound initializer syntax for direct expression-level aggregate construction.
- [ ] Generic equality requirement spelling for value-based `remove` APIs.
- [ ] Replace bytewise `runtime_equal<T>` with opt-in/custom equality once the requirement story is stronger.
- [ ] Optional compiler-owned safe container diagnostics for null container pointers and bounds checks.
