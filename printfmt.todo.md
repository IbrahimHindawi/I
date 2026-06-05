# printfmt

## Spec

`printfmt` is I's formatted printing surface. It is statement-only for now:

```i
printfmt("payload x={} y={}\n", payload.x, payload.y);
```

The compiler requires the first argument to be a string literal, counts plain `{}` placeholders, and requires the placeholder count to match the remaining argument count.

`printfmt` lowers to ordinary I calls:

```i
print_cstr("payload x=");
print<i32>(payload.x);
print_cstr(" y=");
print<f32>(payload.y);
print_cstr("\n");
```

`printf` stays normal C interop. It does not receive `{}` rewriting.

`print<T>(value)` is the typed primitive. Runtime owns builtin printers, and user code owns custom struct printers:

```i
Payload: struct = {
    x: i32;
    y: f32;
}

print: proc<Payload>(value: Payload)->void = {
    printfmt("Payload{x: {}, y: {}}", value.x, value.y);
}
```

Missing printers are intentionally allowed to fall through to generated C errors for now. The compiler does not try to synthesize printers or emit polished missing-printer diagnostics yet.

## Todo

- [x] Add runtime `print_cstr` and builtin `print<T>` implementations for primitive types.
- [x] Lower `printfmt` statement calls into `print_cstr` plus `print<T>` calls.
- [x] Stop rewriting `printf("{}")`.
- [ ] Add escaped brace support if it becomes useful.
- [ ] Add compiler diagnostics for missing printers.
- [ ] Add richer LSP completion/hover for available `print<T>` overloads.
- [ ] Decide whether reflected structs should get optional generated debug printers.
