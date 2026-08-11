# Neovim support for I

Filetype detection, syntax highlighting, and LSP attachment for `.i` files.

## Install

```
python nvim/install.py
```

Options:

| flag | effect |
| --- | --- |
| `--link` | symlink instead of copying, so edits in this repo apply without reinstalling |
| `--dest DIR` | install into a specific config directory |
| `--dry-run` | print what would happen, write nothing |
| `--uninstall` | remove the installed files |

The target defaults to Neovim's own config location: `%LOCALAPPDATA%\nvim` on
Windows, `$XDG_CONFIG_HOME/nvim` or `~/.config/nvim` elsewhere. `NVIM_APPNAME` is
honoured, so it installs alongside a named config rather than over your main one.

## Verify

Open any `.i` file and check:

```vim
:set filetype?     " filetype=i
:checkhealth lsp   " an i-lsp client, rooted at the project
```

## What gets installed

| file | purpose |
| --- | --- |
| `ftdetect/i.lua` | maps `*.i` to filetype `i` |
| `syntax/i.vim` | syntax highlighting |
| `ftplugin/i.lua` | loads the LSP attachment |
| `after/ftplugin/i.lua` | starts and attaches `i-lsp` |

**Why `ftdetect` is Lua rather than Vim script:** Neovim's built-in filetype table
already claims `*.i` for Progress, so a plain autocmd races against it. This uses
`vim.filetype.add()`, which is consulted first and makes the mapping deterministic.

## LSP entry point

`after/ftplugin/i.lua` resolves the language server in this order:

1. `vim.g.i_lsp_command`, if you set it
2. `$I_HOME/scripts/i_lsp.py`, the packaged toolchain layout
3. the repo path baked in by `install.py` at install time

To override it, set the command before the filetype loads:

```lua
vim.g.i_lsp_command = { "python", "-u", "C:/path/to/i/scripts/i_lsp.py" }
```

## Highlighting notes

`//` and `/* */` are comments. `#` is C preprocessor passthrough, so `#` lines are
highlighted as directives — and a `#` line that is not a recognized directive is
shown as an **error**, matching the compiler, which rejects it.
