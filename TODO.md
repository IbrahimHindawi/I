# I TODO

## Current Target: Clangd-Style Ergonomics

The language is now viable enough to compile and run Gini. The next phase should focus on making I sharp to write, diagnose, and edit, not on expanding the language surface or making generated C pretty.

Generated C only needs to be valid, stable enough, and debuggable when necessary. Pretty emitted C can wait.

## Progress

- [x] Compiler `--check` mode exists and is covered by tests.
- [x] Compiler `--diagnostics=json` exists for the parser, I/O errors, and the semantic errors currently covered by snapshot tests.
- [x] Compiler JSON diagnostics are covered for CLI option failures and failed input reads.
- [x] Compiler JSON diagnostics mode is order-independent for CLI parsing, so `--diagnostics=json` still controls earlier bad-option errors.
- [x] Compiler JSON diagnostics cover the common incompatible-type path used by assignment/initializer/return checks.
- [x] Compiler JSON diagnostics cover high-value proc-call, proc-pointer-call, non-proc-call, and return-presence type errors, including declaration notes where available.
- [x] Compiler JSON diagnostics cover common cast, operator, field-access, initializer, const-assignment, and condition type errors.
- [x] Compiler JSON diagnostics cover lexer errors, printf `{}` format errors, generic requirement errors, import-cycle errors, duplicate/control-flow/generic-arity semantic errors, index type errors, and generated-output write errors.
- [x] Compiler JSON diagnostics are covered for duplicate proc parameters, locals, fields, enum items, procs, and globals, including previous-declaration notes.
- [x] Compiler JSON diagnostics are covered for duplicate type aliases, structs, enums, and generated reflection/enum value global collisions.
- [x] Compiler JSON diagnostics cover late native monomorph header write failures.
- [x] Compiler JSON diagnostics are covered for missing imports, including import-chain notes.
- [x] Compiler JSON import-cycle diagnostics point at the import token that closes the cycle instead of `0:0`.
- [x] Compiler JSON import-cycle diagnostics include an import-chain note for the cycle-closing import.
- [x] Compiler JSON diagnostics are covered for cross-import duplicate type/value declarations, including previous import-chain notes.
- [x] Compiler JSON diagnostics are covered for main generated C/header output write failures.
- [x] Compiler JSON diagnostics are covered for assignment/address target errors and initializer duplicate/count/index failures.
- [x] Compiler JSON diagnostics are covered for printf placeholder-count failures beyond the unsupported-type format path.
- [x] Compiler JSON semantic diagnostics are covered for `sizeof`/`alignof` arity and undeclared type/generic-type failures.
- [x] Compiler JSON diagnostics include primary source ranges, and the LSP publishes compiler-provided ranges instead of guessing one-character squiggles.
- [x] Compiler JSON type diagnostics include mismatch notes for pointer/value suggestions, fixed-array pointer decay element mismatches, generic instantiation sites, and proc signature mismatches.
- [x] Compiler JSON proc/proc-pointer arg-count diagnostics include expected-params notes, matching the human terminal diagnostics.
- [x] Human parser and named semantic diagnostics underline known token/name ranges with caret-plus-tilde spans.
- [x] The LSP publishes compiler JSON diagnostics through `textDocument/publishDiagnostics` while keeping the existing fast Python diagnostics.
- [x] The LSP treats compiler JSON diagnostics as the source of truth when `I.exe --check` is available, using Python diagnostics only as a fallback.
- [x] Compiler-backed LSP diagnostics publish with source `I`, while fallback Python diagnostics remain source `i-lsp`.
- [x] The LSP feeds dirty buffer text to `I.exe --check` through compiler stdin mode, preserving the real source path for imports and diagnostic ranges.
- [x] The LSP `didChange` hot path publishes compiler diagnostics only for the edited buffer and skips import reloads, workspace diagnostics, and compiler symbol extraction.
- [x] The LSP debounces `didChange` compiler diagnostics on a background timer, so typing does not synchronously wait for `I.exe --check`.
- [x] Debounced LSP diagnostics discard stale compiler results when a newer dirty buffer has replaced the scheduled text.
- [x] The LSP `didOpen` attach path skips compiler symbol extraction, workspace-wide diagnostic publishing, and Python semantic diagnostics, cutting Gini attach from ~11.5s to ~0.14s in the local synthetic open test.
- [x] The LSP `didChange` path handles a synthetic Gini edit in ~2.4ms locally, sending no diagnostics inline and queueing one debounced compiler check.
- [x] LSP semantic requests now refresh compiler-backed workspace symbols from one `I.exe --symbols=json` import-graph call instead of per-import symbol subprocesses.
- [x] LSP compiler symbol ingestion caches JSON file path/URI resolution, cutting Gini one-shot symbol ingestion from ~0.88s to ~0.10s locally.
- [x] First Gini `workspace/symbol` is ~98ms locally after one-shot compiler ingestion; repeated requests are ~2ms from cache.
- [x] The LSP debounces compiler workspace-symbol prefetch after open/edit, so dirty-buffer completions can use warmed compiler symbols without synchronously spawning `I.exe --symbols=json`.
- [x] Bulk completion items are lean and defer rich docs to `completionItem/resolve`, keeping Gini completion after symbol prefetch around ~7.7ms locally.
- [x] Completion local-scope matching caches the current proc scope/range once per request instead of rescanning the proc for every local candidate.
- [x] `I.exe --lsp=json` emits checked LSP payloads with diagnostics plus import-graph symbols, and emits the existing JSON diagnostic list on checked failure.
- [x] LSP scheduled diagnostics use the faster `I.exe --check --diagnostics=json` path, while workspace symbols are refreshed by a separate idle prefetch or on-demand semantic request.
- [x] Gini dirty-buffer diagnostics are measured separately from symbol JSON extraction, keeping live red-squiggle publication on the ~85ms compiler-check path instead of waiting for the 1.2 MB symbol payload.
- [x] Semantic-token generation uses cached proc ranges/scopes and one-pass identifier resolution, cutting Gini semantic tokens from ~1.4s to ~14ms locally.
- [x] LSP semantic requests no longer reapply the same compiler workspace symbol graph on every request, and reference/rename/highlight paths use a warmed workspace identifier index.
- [x] Gini reference requests are sub-millisecond locally after background warmup (`gin_update` ~0.3ms, `gops_update` ~0.5ms).
- [x] Workspace-symbol prefetch defaults to a longer idle debounce, applies compiler workspace symbols, and warms the reference index without blocking the live diagnostic path.
- [x] The LSP reference index stores precise generic spans without duplicate simple-identifier locations, keeping `Array` and `Payload` references inside `Array<Payload>` distinct.
- [x] The LSP reference-index builder skips expensive sanitizing/normalization on ordinary lines and simple identifiers, cutting Gini index construction from ~120ms to ~35-42ms and idle symbol prefetch from ~206ms to ~146ms locally.
- [x] After compiler symbols are warmed, symbol-stable edits preserve the existing symbol graph and skip the idle `--symbols=json` prefetch entirely; the measured warmed Gini body-only edit has no pending symbol timer and a ~0ms symbol flush.
- [x] Optional `I_LSP_TRACE` logging records live diagnostic publish counts and compiler diagnostic latency without writing to LSP stdout.
- [x] The Neovim installer copies the actual `after/ftplugin/i.lua` LSP starter without failing on a missing legacy `ftplugin/i.lua`.
- [x] The Neovim I ftplugin only marks a buffer attached after `vim.lsp.start()` succeeds, runs the server from the project root, and enables underline/sign diagnostics with insert-mode updates.
- [x] The LSP compiler-diagnostic cache keys on the `I.exe` binary timestamp, so it recovers when the compiler is built or rebuilt.
- [x] Compiler-backed LSP workspaces parse/index imports without retaining Python fallback diagnostics; standalone `Workspace()` still keeps fallback diagnostics for tests and compiler-missing use.
- [x] `I.exe --symbols=json` emits compiler-backed top-level symbols from live stdin source, and LSP `documentSymbol` prefers that data when available.
- [x] `I.exe --symbols=json` emits compiler-backed struct/union fields, including owner, detail, attributes, and source ranges.
- [x] `I.exe --symbols=json` emits compiler-backed proc parameters and local declarations, including nested block and for-init locals.
- [x] `I.exe --symbols=json` emits structured proc metadata (`params`, `return_type`, `variadic`) for LSP signature help and completions.
- [x] `I.exe --symbols=json` emits structured type metadata for fields, globals, parameters, locals, and aliases.
- [x] `I.exe --symbols=json` emits structured enum-member owner/item metadata.
- [x] `I.exe --symbols=json` emits proc scope metadata for parameters and local variables.
- [x] `I.exe --symbols=json` emits structured proc metadata for direct proc/proc-pointer aliases.
- [x] `I.exe --symbols=json` emits generic type-param metadata for structs and fields.
- [x] The LSP workspace index uses compiler-backed top-level symbols and globals when available, while preserving Python imports and path completions.
- [x] The LSP field index uses compiler-backed struct/union fields when available, while preserving Python field parsing as fallback.
- [x] The LSP variable index uses compiler-backed globals, proc parameters, and locals when available, while preserving Python variable parsing as fallback.
- [x] LSP completion, definition, and hover paths are covered against compiler-backed top-level symbols/globals, not only Python-parsed symbols.
- [x] The LSP advertises and serves `workspace/symbol` from the compiler-backed workspace index.
- [x] The LSP advertises and serves `textDocument/documentHighlight` using the same resolved symbol, field, enum member, local, and global reference paths.
- [x] The LSP has proc argument completions using signature-help context, including `name.&` suggestions for pointer parameters.
- [x] LSP signature help and proc-argument completions are covered against compiler-backed proc symbols/variables, not only Python-parsed signatures.
- [x] LSP proc signature help prefers compiler-backed structured proc params/return metadata over parsing human detail strings.
- [x] LSP proc hover prefers compiler-backed structured proc params/return metadata over display strings.
- [x] LSP proc completion detail prefers compiler-backed structured proc params/return metadata over display strings.
- [x] LSP symbol ingestion prefers compiler-backed `type`/`target_type` metadata over parsing display strings for fields, variables, and aliases.
- [x] LSP enum member resolution prefers compiler-backed owner/item metadata over parsing generated names or display strings.
- [x] LSP enum document-symbol children and enum hover member lists prefer compiler-backed owner/item metadata over generated-name prefixes.
- [x] LSP enum usage lookup, references, rename, and semantic tokens prefer compiler-backed owner/item metadata over emitted symbol names.
- [x] LSP local/parameter matching prefers compiler-backed proc scope metadata when available.
- [x] LSP proc-pointer alias signature help prefers compiler-backed alias params/return metadata over parsing target strings.
- [x] LSP generic field substitution prefers compiler-backed owner type-param metadata instead of assuming `T`.
- [x] LSP generic proc signature substitution prefers compiler-backed proc type-param metadata over parsing display names.
- [x] LSP alias hover prefers compiler-backed target/proc metadata over parsing display strings.
- [x] LSP callable argument diagnostics and call-expression type inference prefer compiler-backed proc metadata over parsing display strings.
- [x] LSP semantic token classification is covered against compiler-backed symbols/variables, not only Python-parsed symbols.
- [x] The LSP has expected-type completions for typed assignments, including matching locals/globals and pointer address-sugar suggestions.
- [x] The LSP has context-aware enum member completions and struct literal field completions.
- [x] Added an `i-torture/compile` smoke corpus that transpiles I fixtures to C and compiles them, covering control flow, pointer/array/proc-pointer code, unions, alias-backed callbacks, nested generics, and reflection.
- [x] Expanded the local `gcc.c-torture/compile` smoke corpus and fixed nested generic struct dependency emission exposed by the translated-I corpus.
- [ ] Broaden compiler JSON output to every remaining ad hoc semantic/type/codegen diagnostic.
- [ ] Move more LSP semantic ownership from Python helpers to compiler-backed checks.

## 1. Compiler Diagnostics First

Make every compiler error more clang-like:

- exact file, line, column, and range
- underline the bad token or expression
- show expected vs actual type/token
- show declaration-site notes
- show import-chain notes
- show generic instantiation notes
- show generated C line-map notes when relevant

The internal move should be a real diagnostic system instead of ad hoc `printf` plus `exit`.

Target shape:

```text
error: proc 'foo' argument 2 expected '*Payload', got 'Payload'
  --> src/game.i:42:19
   |
42 |     foo(arena, payload);
   |                ^^^^^^^
   |
note: parameter declared here
  --> src/foo.i:3:24
note: imported through: main.i -> game.i -> foo.i
```

## 2. Compiler Check Mode And JSON Diagnostics

Add:

```text
I.exe --check file.i
I.exe --check file.i --diagnostics=json
```

`--check` should parse, import, validate, and type-check without requiring generated C as the primary output.

`--diagnostics=json` should emit structured diagnostics suitable for the LSP:

```json
{
  "severity": "error",
  "file": "src/sops.i",
  "line": 439,
  "column": 65,
  "message": "proc argument 3 expected '*cgltf_float', got 'vec2'",
  "notes": []
}
```

## 3. LSP Diagnostics From The Compiler

The Python LSP should stop pretending to be the real compiler for errors.

Fast path:

- LSP runs `I.exe --check file.i --diagnostics=json` on save/debounce
- LSP publishes compiler diagnostics directly to nvim
- red squiggles come from the real compiler
- compiler diagnostics become the source of truth

This gives clangd-style feedback without rewriting the LSP immediately.

## 4. Keep Python LSP As A Thin Shell

Python is fine as a temporary LSP frontend. It is slow if it owns all semantics, but acceptable if it mostly handles:

- JSON-RPC
- file watching / debounce
- path completion
- calling `I.exe --check`
- publishing compiler diagnostics
- caching simple symbol indexes

Long-term, the LSP should be native or backed by compiler/library analysis. Short-term, do not rewrite it. Make it useful.

## 5. Ergonomic LSP Features

High-value editor features:

- proc argument completion
- expected-type completion
- enum member completion
- struct literal field completion
- better hover showing resolved type
- goto definition through imports
- rename/references good enough for project files
- live diagnostics from compiler JSON

Proc arg completion should reuse signature-help machinery.

## 6. Keep Modules Stupid

No `pub`, no `private`, no fancy visibility for now.

Current rule:

- imports are visible
- duplicate symbols are errors
- import order is deterministic
- generated output is one merged unit unless there is a practical reason not to
- diagnostics explain where duplicate/imported symbols came from

This is basically C-style global soup, but with better errors.

## 7. Generated C Only Needs To Compile

Do not spend energy making generated C beautiful right now.

Keep only:

- valid C
- stable enough names
- useful `#line`
- no broken include/header ordering
- debuggable enough for hard runtime crashes

If something serious crashes, debug generated C/assembly normally.

## 8. Tests For Ergonomics

Add tests that lock in diagnostics and LSP behavior:

- parser diagnostic snapshots
- type diagnostic snapshots
- import-chain diagnostic snapshots
- generic instantiation diagnostic snapshots
- LSP `publishDiagnostics` tests
- completion tests for proc args
- completion tests for enum members
- completion tests for struct fields and struct literals
- larger `gcc.c-torture/compile` corpus
- later: translated C torture cases into I

## Immediate Next Work

Do this in order:

1. Add compiler `--check`.
2. Add compiler `--diagnostics=json`.
3. Convert the LSP to publish compiler diagnostics.
4. Add proc argument completion.
5. Add enum/struct-field completions.
6. Improve compiler diagnostic formatting.
7. Expand c-torture and start translating small torture cases to I.
