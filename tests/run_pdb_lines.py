"""Checks the PDB line table, which is what a Windows debugger consumes.

`run_i_debuginfo.py` covers DWARF through clang. This is the other half: MSVC
and clang-cl emit CodeView into a PDB, and that is what Visual Studio, WinDbg
and any disassembly-with-source view actually read. Nothing verified it, and a
misaligned line table looks exactly like a compiler bug from a debugger.

What it asserts, per compiler:

  * the line table names the `.i` source, never the generated `.c` -- if the
    generated file leaks in, a debugger shows machine-generated code
  * `<generated>` contributes no lines, since it is not a file anyone can open
  * known statements in the fixture map to their real `.i` line numbers, so
    stepping lands where the author would expect
  * the mapping is monotonic across a straight-line region, which is what
    catches an off-by-N drift rather than a wholesale mismatch

Known and not asserted: the `.i` files carry **no checksum**. A C compiler
hashes the files it opens, and it never opens the `.i` -- it only sees the name
in a `#line`. Debuggers use that checksum to confirm the source on disk matches
the build, so ilang sources are unverifiable by construction. Harmless when the
tree is clean; it means a stale `.i` will be shown without complaint.

Skips cleanly when a compiler or llvm-pdbutil is missing.
"""
from __future__ import annotations

import argparse
import os
import pathlib
import re
import shutil
import subprocess
import sys

ROOT = pathlib.Path(__file__).resolve().parent.parent
I_EXE = ROOT / "build" / "I.exe"
BUILD_DIR = ROOT / "build" / "pdb_lines"

DEFAULT_CL = pathlib.Path(
    r"C:\Program Files (x86)\Microsoft Visual Studio\2022\BuildTools"
    r"\VC\Tools\MSVC\14.44.35207\bin\Hostx64\x64\cl.exe")
DEFAULT_VCVARS = pathlib.Path(
    r"C:\Program Files (x86)\Microsoft Visual Studio\2022\BuildTools"
    r"\VC\Auxiliary\Build\vcvars64.bat")

# Statement -> the .i line it must map to. Deliberately spread across the body,
# including inside an `if` and a `for`, because a drift usually starts partway
# down rather than at the first statement.
FIXTURE = """cinclude "stdio.h"
printf: proc[external](f: *const char, ...) -> i32 = {}

main: proc() -> i32 = {
    total: i32 = 0;
    total = total + 1;
    if (total > 0) {
        total = total + 2;
    }
    for (i: i32 = 0; i < 3; i += 1) {
        total = total + i;
    }
    printf("%d", total);
    return 0;
}
"""
# line 4  main
# line 5  total: i32 = 0;
# line 6  total = total + 1;
# line 8  total = total + 2;      (inside the if)
# line 11 total = total + i;      (inside the for)
# line 13 printf
EXPECTED_LINES = [4, 5, 6, 8, 11, 13]


def msvc_environment(vcvars: pathlib.Path) -> dict[str, str] | None:
    if not vcvars.exists():
        return None
    result = subprocess.run(f'"{vcvars}" >nul 2>&1 && set',
                            capture_output=True, text=True, shell=True)
    if result.returncode != 0:
        return None
    env = dict(os.environ)
    for line in result.stdout.splitlines():
        if "=" in line:
            key, value = line.split("=", 1)
            env[key] = value
    return env if "INCLUDE" in env else None


def line_table(pdbutil: str, pdb: pathlib.Path) -> tuple[list[str], list[int]]:
    """Returns (source files named, line numbers) for the fixture's source."""
    dumped = subprocess.run([pdbutil, "pretty", "-lines", str(pdb)],
                            capture_output=True, text=True)
    files: list[str] = []
    lines: list[int] = []
    current_is_fixture = False
    for row in dumped.stdout.splitlines():
        stripped = row.strip()
        match = re.match(r"^Line (\d+), Address:", stripped)
        if match:
            if current_is_fixture:
                lines.append(int(match.group(1)))
            continue
        if stripped.endswith((".i", ".c", ".h")) or "(MD5" in stripped or "(no checksum)" in stripped:
            name = re.sub(r"\s*\((MD5|SHA-?\d*)[^)]*\)$", "", stripped)
            name = re.sub(r"\s*\(no checksum\)$", "", name).strip()
            files.append(name)
            current_is_fixture = name.endswith("pdb_lines.i")
    return files, lines


def check(label: str, compile_argv: list[str], env: dict[str, str] | None,
          pdb: pathlib.Path, pdbutil: str, source: pathlib.Path) -> list[str]:
    problems: list[str] = []
    built = subprocess.run(compile_argv, capture_output=True, text=True,
                           env=env, cwd=str(BUILD_DIR))
    if built.returncode != 0:
        return [f"{label}: did not build: "
                + ((built.stdout or "") + (built.stderr or "")).strip()[:300]]

    files, lines = line_table(pdbutil, pdb)
    if not files:
        return [f"{label}: llvm-pdbutil produced no line table"]

    if not any(f.endswith("pdb_lines.i") for f in files):
        problems.append(f"{label}: the .i source is not in the line table at all")
    for name in files:
        if name.endswith("pdb_lines.c"):
            problems.append(f"{label}: the generated .c leaked into the line table")
        if "<generated>" in name:
            problems.append(f"{label}: <generated> is not a file a debugger can open")

    missing = [n for n in EXPECTED_LINES if n not in lines]
    if missing:
        sample = sorted(set(lines))[:20]
        problems.append(f"{label}: statements missing from the line table: {missing} "
                        f"(table starts {sample}...)")

    # The body is straight-line apart from the loop, so the first appearance of
    # each expected line must be in source order. A constant offset or a
    # wholesale mismatch both show up here.
    first_seen = {}
    for position, value in enumerate(lines):
        first_seen.setdefault(value, position)
    ordered = [first_seen[n] for n in EXPECTED_LINES if n in first_seen]
    if ordered != sorted(ordered):
        problems.append(f"{label}: line table is out of source order: {ordered}")
    return problems


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--cl", type=pathlib.Path,
                        default=pathlib.Path(os.environ.get("CL_EXE", DEFAULT_CL)))
    parser.add_argument("--vcvars", type=pathlib.Path,
                        default=pathlib.Path(os.environ.get("VCVARS", DEFAULT_VCVARS)))
    args = parser.parse_args()

    pdbutil = shutil.which("llvm-pdbutil") or shutil.which("llvm-pdbutil.exe")
    if pdbutil is None:
        candidate = pathlib.Path(r"C:\Program Files\LLVM\bin\llvm-pdbutil.exe")
        pdbutil = str(candidate) if candidate.exists() else None
    if pdbutil is None:
        print("pdb-lines: skipped, llvm-pdbutil not found")
        return 0
    if not I_EXE.exists():
        print(f"pdb-lines: missing compiler {I_EXE}")
        return 1

    BUILD_DIR.mkdir(parents=True, exist_ok=True)
    source = BUILD_DIR / "pdb_lines.i"
    source.write_text(FIXTURE, encoding="utf-8", newline="\n")
    generated = BUILD_DIR / "pdb_lines.c"
    made = subprocess.run([str(I_EXE), "compile", str(source), "-o", str(generated),
                           "--no-header"], capture_output=True, text=True)
    if made.returncode != 0:
        print("pdb-lines: fixture failed to compile")
        print(made.stdout)
        return 1

    problems: list[str] = []
    checked: list[str] = []

    clang_cl = shutil.which("clang-cl") or shutil.which("clang-cl.exe")
    if clang_cl:
        pdb = BUILD_DIR / "clangcl.pdb"
        problems += check(
            "clang-cl",
            [clang_cl, "/nologo", "/Zi", "/Od",
             "/I", str(ROOT / "src" / "std"), "/I", str(ROOT / "src"),
             str(generated), "/Fe:" + str(BUILD_DIR / "clangcl.exe"),
             "/Fo:" + str(BUILD_DIR / "clangcl.obj"), "/Fd:" + str(pdb)],
            None, pdb, pdbutil, source)
        checked.append("clang-cl")

    env = msvc_environment(args.vcvars)
    if args.cl.exists() and env is not None:
        pdb = BUILD_DIR / "msvc.pdb"
        problems += check(
            "cl",
            [str(args.cl), "-nologo", "-Zi", "-Od",
             "-I", str(ROOT / "src" / "std"), "-I", str(ROOT / "src"),
             str(generated), "-Fe:" + str(BUILD_DIR / "msvc.exe"),
             "-Fo:" + str(BUILD_DIR / "msvc.obj"), "-Fd:" + str(pdb)],
            env, pdb, pdbutil, source)
        checked.append("cl")

    if not checked:
        print("pdb-lines: skipped, no Windows compiler available")
        return 0
    if problems:
        print("pdb-lines: line table problems")
        for problem in problems:
            print("  " + problem)
        return 1
    print("pdb-lines: %s map every statement to the .i source, in order"
          % " and ".join(checked))
    return 0


if __name__ == "__main__":
    sys.exit(main())
