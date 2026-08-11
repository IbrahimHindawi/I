"""Checks the debug line table a debugger actually consumes.

Generated C carries `#line` directives so debuggers show `.i` source. Those
directives are only emitted where the implied position would otherwise drift,
which is a correctness claim about debugging, not about text. This builds the
fixture with DWARF and reads the resulting line table to assert:

  * every mapped line points at the `.i` source, never the generated `.c`
  * lines holding real statements are present, so breakpoints can be set on them
  * the table is identical with and without `--emit-all-line-directives`, so the
    reduced output is invisible to a debugger

Needs clang and llvm-objdump; skips cleanly when they are missing.
"""
from __future__ import annotations

import argparse
import os
import re
import shutil
import subprocess
import sys
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
I_EXE = ROOT / "build" / "I.exe"
FIXTURE = ROOT / "tests" / "i-debuginfo" / "line-table.i"

# Lines that hold a statement or a proc signature, so a debugger must be able to
# stop on them. Kept explicit: a silent drop here would mean unbreakpointable code.
EXPECTED_LINES = {5, 6, 7, 8, 9, 10, 13, 14, 15, 16, 17, 19, 21, 22, 24, 27, 28, 29, 30}

LINE_NOTE = re.compile(r";\s*(.*?):(\d+)\s*$")


def run(cmd: list[str]) -> subprocess.CompletedProcess[str]:
    return subprocess.run(cmd, cwd=ROOT, text=True, stdout=subprocess.PIPE,
                          stderr=subprocess.STDOUT, timeout=180)


def line_table(objdump: str, obj: Path) -> list[tuple[str, int]]:
    dumped = run([objdump, "--line-numbers", "-d", str(obj)])
    if dumped.returncode != 0:
        raise RuntimeError(f"llvm-objdump failed:\n{dumped.stdout}")
    rows: list[tuple[str, int]] = []
    for raw in dumped.stdout.split("\n"):
        stripped = raw.strip()
        if not stripped.startswith(";"):
            continue
        m = LINE_NOTE.match(stripped)
        if m:
            rows.append((Path(m.group(1)).name, int(m.group(2))))
    return rows


def build(cc: str, out_dir: Path, tag: str, all_directives: bool) -> Path:
    c_path = out_dir / f"{tag}.c"
    obj_path = out_dir / f"{tag}.o"
    args = [str(I_EXE), "compile", str(FIXTURE), "-o", str(c_path), "--no-header"]
    if all_directives:
        args.append("--emit-all-line-directives")
    translated = run(args)
    if translated.returncode != 0:
        raise RuntimeError(f"translate failed:\n{translated.stdout}")
    compiled = run([cc, "-gdwarf-4", "-O0", "-c", str(c_path), "-I", "src", "-I", "src/std",
                    "-o", str(obj_path), "-Wno-everything"])
    if compiled.returncode != 0:
        raise RuntimeError(f"compile failed:\n{compiled.stdout}")
    return obj_path


def main() -> int:
    parser = argparse.ArgumentParser(description="I debug line table checks.")
    parser.add_argument("--cc", default=os.environ.get("CC", "clang.exe"))
    parser.add_argument("--objdump", default=os.environ.get("LLVM_OBJDUMP", "llvm-objdump.exe"))
    args = parser.parse_args()

    if not I_EXE.exists():
        print(f"i-debuginfo: missing compiler {I_EXE}")
        return 1
    cc = shutil.which(args.cc)
    objdump = shutil.which(args.objdump)
    if not cc or not objdump:
        missing = args.cc if not cc else args.objdump
        print(f"i-debuginfo: skipped, {missing} not on PATH")
        return 0

    out_dir = ROOT / "build" / "i_debuginfo"
    out_dir.mkdir(parents=True, exist_ok=True)

    try:
        reduced = line_table(objdump, build(cc, out_dir, "reduced", False))
        full = line_table(objdump, build(cc, out_dir, "full", True))
    except RuntimeError as exc:
        print(f"i-debuginfo: {exc}")
        return 1

    if not reduced:
        print("i-debuginfo: no line table rows found; debug info is missing entirely")
        return 1

    # System headers legitimately appear via inlined stdio, but no row may reference a
    # .c file: that is generated output, and hiding it is the whole point of #line.
    # Verified non-vacuous: stripping #line makes rows point at the generated .c.
    stray = sorted({name for name, _ in reduced if name.endswith(".c")})
    if stray:
        print(f"i-debuginfo: line table points at generated C {stray}; "
              f"stepping would land in emitted code instead of .i source")
        return 1

    mapped = {line for name, line in reduced if name == FIXTURE.name}
    if not mapped:
        print(f"i-debuginfo: no line-table rows reference {FIXTURE.name}")
        return 1
    missing = sorted(EXPECTED_LINES - mapped)
    if missing:
        print(f"i-debuginfo: no breakpoint location for {FIXTURE.name} lines {missing}")
        return 1

    out_of_range = sorted(line for line in mapped if line < 1 or line > 31)
    if out_of_range:
        print(f"i-debuginfo: line table references lines outside the fixture: {out_of_range}")
        return 1

    if reduced != full:
        print("i-debuginfo: reduced #line output produces a different line table than full output")
        for a, b in zip(full, reduced):
            if a != b:
                print(f"  full   ={a}")
                print(f"  reduced={b}")
                break
        print(f"  row counts: full={len(full)} reduced={len(reduced)}")
        return 1

    print(f"i-debuginfo: {len(reduced)} line-table rows, {len(mapped)} distinct .i lines, "
          f"identical with and without full #line output")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
