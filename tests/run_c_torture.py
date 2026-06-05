from __future__ import annotations

import argparse
import os
import subprocess
import sys
from concurrent.futures import ThreadPoolExecutor, as_completed
from dataclasses import dataclass
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
DEFAULT_SUITE_CANDIDATES = (
    ROOT / "extern" / "gcc" / "gcc" / "testsuite" / "gcc.c-torture" / "compile",
    ROOT / "tests" / "gcc.c-torture" / "compile",
)


@dataclass(frozen=True)
class Result:
    path: Path
    returncode: int
    output: str


def object_path_for_source(suite: Path, source: Path, build_dir: Path) -> Path:
    rel = source.relative_to(suite)
    parts = list(rel.parts)
    parts[-1] = f"{Path(parts[-1]).stem}.o"
    obj = build_dir.joinpath(*parts)
    obj.parent.mkdir(parents=True, exist_ok=True)
    return obj


def find_suite(explicit: Path | None) -> Path | None:
    candidates: list[Path] = []
    if explicit:
        candidates.append(explicit)
    env_suite = os.environ.get("I_GCC_TORTURE")
    if env_suite:
        candidates.append(Path(env_suite))
    candidates.extend(DEFAULT_SUITE_CANDIDATES)

    for candidate in candidates:
        if candidate.exists():
            if candidate.name == "compile":
                return candidate
            nested = candidate / "gcc.c-torture" / "compile"
            if nested.exists():
                return nested
            nested = candidate / "gcc" / "testsuite" / "gcc.c-torture" / "compile"
            if nested.exists():
                return nested
    return None


def compile_one(cc: str, suite: Path, source: Path, build_dir: Path) -> Result:
    obj = object_path_for_source(suite, source, build_dir)
    cmd = [cc, "-c", str(source), "-o", str(obj), "-Wno-everything"]
    proc = subprocess.run(cmd, text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    return Result(source, proc.returncode, proc.stdout)


def main() -> int:
    parser = argparse.ArgumentParser(description="Optional gcc.c-torture/compile smoke runner.")
    parser.add_argument("--suite", type=Path, help="Path to gcc.c-torture/compile or a GCC checkout.")
    parser.add_argument("--cc", default=os.environ.get("CC", "clang.exe"), help="C compiler to use.")
    parser.add_argument("--jobs", type=int, default=max(1, (os.cpu_count() or 2) - 1))
    parser.add_argument("--limit", type=int, default=0, help="Limit number of files, useful for quick checks.")
    parser.add_argument("--required", action="store_true", help="Fail instead of skipping when the suite is missing.")
    args = parser.parse_args()

    suite = find_suite(args.suite)
    if not suite:
        msg = "gcc.c-torture/compile: skipped, suite not found"
        if args.required:
            print(msg)
            return 1
        print(msg)
        print("set I_GCC_TORTURE or pass --suite to enable it")
        return 0

    sources = sorted(suite.rglob("*.c"))
    if args.limit > 0:
        sources = sources[: args.limit]
    if not sources:
        print(f"gcc.c-torture/compile: no .c files under {suite}")
        return 1

    build_dir = ROOT / "build" / "gcc_c_torture"
    build_dir.mkdir(parents=True, exist_ok=True)

    failures: list[Result] = []
    with ThreadPoolExecutor(max_workers=max(1, args.jobs)) as pool:
        futures = [pool.submit(compile_one, args.cc, suite, source, build_dir) for source in sources]
        for future in as_completed(futures):
            result = future.result()
            if result.returncode != 0:
                failures.append(result)

    passed = len(sources) - len(failures)
    print(f"gcc.c-torture/compile: {passed}/{len(sources)} passed with {args.cc}")
    for failure in failures[:20]:
        print(f"\nFAIL {failure.path}")
        print(failure.output.rstrip())
    if len(failures) > 20:
        print(f"\n... {len(failures) - 20} more failures omitted")
    return 1 if failures else 0


if __name__ == "__main__":
    raise SystemExit(main())
