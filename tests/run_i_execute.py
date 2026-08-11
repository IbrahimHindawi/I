"""Differential runner for the I execute torture suite.

The compile suite only proves the generated C is accepted by a C compiler. This
one links and runs it, then compares stdout against a recorded .expected file, so
codegen that compiles but computes the wrong thing is caught too.

Regenerate expectations after an intentional behaviour change with --bless.
"""
from __future__ import annotations

import argparse
import os
import subprocess
import sys
from concurrent.futures import ThreadPoolExecutor, as_completed
from dataclasses import dataclass
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
I_EXE = ROOT / "build" / "I.exe"
DEFAULT_SUITE = ROOT / "tests" / "i-torture" / "execute"


@dataclass(frozen=True)
class Result:
    path: Path
    phase: str
    ok: bool
    detail: str


def run(cmd: list[str], cwd: Path = ROOT) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        cmd, cwd=cwd, text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, timeout=120
    )


def check_one(cc: str, suite: Path, source: Path, build_dir: Path, bless: bool) -> Result:
    case_dir = build_dir / source.stem
    case_dir.mkdir(parents=True, exist_ok=True)
    c_path = case_dir / f"{source.stem}.c"
    h_path = case_dir / f"{source.stem}.h"
    exe_path = case_dir / f"{source.stem}.exe"

    translate = run([str(I_EXE), "compile", str(source), "-o", str(c_path), "--header", str(h_path)])
    if translate.returncode != 0:
        return Result(source, "translate", False, translate.stdout)

    link = run([cc, str(c_path), "-I", "src", "-I", "src/std", "-o", str(exe_path), "-Wno-everything"])
    if link.returncode != 0:
        return Result(source, "link", False, link.stdout)

    try:
        got = run([str(exe_path)])
    except subprocess.TimeoutExpired:
        return Result(source, "run", False, "timed out")
    if got.returncode != 0:
        return Result(source, "run", False, f"exit {got.returncode}\n{got.stdout}")

    actual = got.stdout.replace("\r\n", "\n")
    expected_path = source.with_suffix(".expected")
    if bless:
        expected_path.write_text(actual, encoding="utf-8", newline="\n")
        return Result(source, "bless", True, "")
    if not expected_path.exists():
        return Result(source, "expect", False, f"missing {expected_path.name}; run with --bless\ngot:\n{actual}")

    expected = expected_path.read_text(encoding="utf-8").replace("\r\n", "\n")
    if actual != expected:
        return Result(source, "expect", False, f"--- expected ---\n{expected}--- actual ---\n{actual}")
    return Result(source, "expect", True, "")


def main() -> int:
    parser = argparse.ArgumentParser(description="I execute torture differential runner.")
    parser.add_argument("--suite", type=Path, default=DEFAULT_SUITE)
    parser.add_argument("--cc", default=os.environ.get("CC", "clang.exe"))
    parser.add_argument("--jobs", type=int, default=max(1, (os.cpu_count() or 2) - 1))
    parser.add_argument("--bless", action="store_true", help="Record current stdout as expected.")
    args = parser.parse_args()

    if not I_EXE.exists():
        print(f"i-torture/execute: missing compiler {I_EXE}")
        return 1
    if not args.suite.exists():
        print(f"i-torture/execute: suite not found: {args.suite}")
        return 1

    sources = sorted(args.suite.rglob("*.i"))
    if not sources:
        print(f"i-torture/execute: no .i files under {args.suite}")
        return 1

    build_dir = ROOT / "build" / "i_execute"
    build_dir.mkdir(parents=True, exist_ok=True)

    failures: list[Result] = []
    with ThreadPoolExecutor(max_workers=max(1, args.jobs)) as pool:
        futures = [pool.submit(check_one, args.cc, args.suite, s, build_dir, args.bless) for s in sources]
        for future in as_completed(futures):
            result = future.result()
            if not result.ok:
                failures.append(result)

    passed = len(sources) - len(failures)
    print(f"i-torture/execute: {passed}/{len(sources)} passed with {args.cc}")
    for failure in failures[:20]:
        print(f"\nFAIL {failure.phase} {failure.path.name}")
        print(failure.detail.rstrip())
    return 1 if failures else 0


if __name__ == "__main__":
    raise SystemExit(main())
