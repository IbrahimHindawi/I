#!/usr/bin/env python3
"""Install the I language Neovim support into your Neovim config.

Copies (or symlinks) filetype detection, syntax highlighting, and the LSP
attachment into the Neovim config directory:

    python nvim/install.py                 # copy into the detected config dir
    python nvim/install.py --link          # symlink instead, so repo edits apply live
    python nvim/install.py --dest DIR      # install somewhere specific
    python nvim/install.py --dry-run       # show what would happen
    python nvim/install.py --uninstall     # remove what was installed

The LSP entry point is resolved at runtime from $I_HOME when that is set;
otherwise the repo path this script was run from is baked into the installed
after/ftplugin file.
"""
from __future__ import annotations

import argparse
import os
import shutil
import sys
from pathlib import Path


HERE = Path(__file__).resolve().parent
REPO = HERE.parent

# Relative paths under both this directory and the Neovim config directory.
FILES = (
    Path("ftdetect/i.lua"),
    Path("ftplugin/i.lua"),
    Path("after/ftplugin/i.lua"),
    Path("syntax/i.vim"),
)

# Written into after/ftplugin/i.lua so the installed copy can find i_lsp.py.
REPO_PLACEHOLDER = "@I_REPO@"


def default_config_dir() -> Path:
    """Neovim's config directory, following the same rules Neovim itself uses."""
    explicit = os.environ.get("NVIM_APPNAME")
    appname = explicit if explicit else "nvim"

    if sys.platform == "win32":
        base = os.environ.get("LOCALAPPDATA")
        if base:
            return Path(base) / appname
        return Path.home() / "AppData" / "Local" / appname

    xdg = os.environ.get("XDG_CONFIG_HOME")
    if xdg:
        return Path(xdg) / appname
    return Path.home() / ".config" / appname


def read_rendered(source: Path) -> str:
    """File contents with the repo path substituted where the template asks for it."""
    text = source.read_text(encoding="utf-8")
    return text.replace(REPO_PLACEHOLDER, REPO.as_posix())


def install(dest: Path, use_links: bool, dry_run: bool) -> int:
    print(f"repo:   {REPO}")
    print(f"target: {dest}")
    print(f"mode:   {'symlink' if use_links else 'copy'}{' (dry run)' if dry_run else ''}")
    print()

    failed = 0
    for rel in FILES:
        source = HERE / rel
        target = dest / rel
        if not source.exists():
            print(f"  MISSING {source}")
            failed += 1
            continue

        needs_substitution = REPO_PLACEHOLDER in source.read_text(encoding="utf-8")
        action = "link" if use_links and not needs_substitution else "copy"
        if use_links and needs_substitution:
            # A symlink cannot carry a substituted repo path, so this one is copied
            # even in link mode.
            action = "copy (templated)"

        print(f"  {action:16} {rel}")
        if dry_run:
            continue

        try:
            target.parent.mkdir(parents=True, exist_ok=True)
            if target.exists() or target.is_symlink():
                target.unlink()
            if action == "link":
                target.symlink_to(source)
            else:
                target.write_text(read_rendered(source), encoding="utf-8", newline="\n")
        except OSError as exc:
            print(f"    failed: {exc}")
            if action == "link" and sys.platform == "win32":
                print("    (Windows needs Developer Mode or an elevated shell for symlinks;"
                      " re-run without --link)")
            failed += 1

    print()
    if failed:
        print(f"{failed} file(s) failed")
        return 1
    if dry_run:
        print("dry run only, nothing was written")
        return 0

    print("installed. open a .i file and check with:")
    print("  :set filetype?      -> filetype=i")
    print("  :checkhealth lsp    -> an i-lsp client attached")
    return 0


def uninstall(dest: Path, dry_run: bool) -> int:
    print(f"target: {dest}")
    removed = 0
    for rel in FILES:
        target = dest / rel
        if not (target.exists() or target.is_symlink()):
            continue
        print(f"  remove {rel}")
        removed += 1
        if not dry_run:
            target.unlink()
    print()
    print(f"{removed} file(s) {'would be ' if dry_run else ''}removed")
    return 0


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Install I language support into a Neovim config.",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=__doc__,
    )
    parser.add_argument("--dest", type=Path, default=None,
                        help="Neovim config directory (default: auto-detected).")
    parser.add_argument("--link", action="store_true",
                        help="Symlink instead of copying, so repo edits take effect immediately.")
    parser.add_argument("--dry-run", action="store_true", help="Show what would happen.")
    parser.add_argument("--uninstall", action="store_true", help="Remove the installed files.")
    args = parser.parse_args()

    dest = args.dest if args.dest is not None else default_config_dir()
    if args.uninstall:
        return uninstall(dest, args.dry_run)
    return install(dest, args.link, args.dry_run)


if __name__ == "__main__":
    raise SystemExit(main())
