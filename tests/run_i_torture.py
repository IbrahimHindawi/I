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
DEFAULT_SUITE = ROOT / "tests" / "i-torture" / "compile"


@dataclass(frozen=True)
class Result:
    path: Path
    phase: str
    returncode: int
    output: str


def output_paths_for_source(suite: Path, source: Path, build_dir: Path) -> tuple[Path, Path, Path]:
    rel = source.relative_to(suite)
    stem = rel.with_suffix("")
    case_dir = build_dir / stem
    case_dir.mkdir(parents=True, exist_ok=True)
    return case_dir / f"{source.stem}.c", case_dir / f"{source.stem}.h", case_dir / f"{source.stem}.o"


def run(cmd: list[str], cwd: Path = ROOT) -> subprocess.CompletedProcess[str]:
    return subprocess.run(cmd, cwd=cwd, text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)


def compile_one(i_exe: Path, cc: str, suite: Path, source: Path, build_dir: Path) -> Result:
    c_path, h_path, obj_path = output_paths_for_source(suite, source, build_dir)

    translate = run([str(i_exe), str(source), str(c_path), str(h_path)])
    if translate.returncode != 0:
        return Result(source, "translate", translate.returncode, translate.stdout)

    compile_result = run([
        cc,
        "-c",
        str(c_path),
        "-I",
        "src",
        "-I",
        "src/std",
        "-o",
        str(obj_path),
        "-Wno-everything",
    ])
    return Result(source, "compile", compile_result.returncode, compile_result.stdout)


def main() -> int:
    parser = argparse.ArgumentParser(description="Translated I compile torture smoke runner.")
    parser.add_argument("--suite", type=Path, default=DEFAULT_SUITE)
    parser.add_argument("--cc", default=os.environ.get("CC", "clang.exe"), help="C compiler to use.")
    parser.add_argument("--jobs", type=int, default=max(1, (os.cpu_count() or 2) - 1))
    parser.add_argument("--limit", type=int, default=0, help="Limit number of files, useful for quick checks.")
    args = parser.parse_args()

    if not I_EXE.exists():
        print(f"i-torture/compile: missing compiler {I_EXE}")
        return 1
    if not args.suite.exists():
        print(f"i-torture/compile: suite not found: {args.suite}")
        return 1

    sources = sorted(args.suite.rglob("*.i"))
    if args.limit > 0:
        sources = sources[: args.limit]
    if not sources:
        print(f"i-torture/compile: no .i files under {args.suite}")
        return 1

    build_dir = ROOT / "build" / "i_torture"
    build_dir.mkdir(parents=True, exist_ok=True)

    failures: list[Result] = []
    with ThreadPoolExecutor(max_workers=max(1, args.jobs)) as pool:
        futures = [pool.submit(compile_one, I_EXE, args.cc, args.suite, source, build_dir) for source in sources]
        for future in as_completed(futures):
            result = future.result()
            if result.returncode != 0:
                failures.append(result)

    passed = len(sources) - len(failures)
    print(f"i-torture/compile: {passed}/{len(sources)} passed with {args.cc}")
    for failure in failures[:20]:
        print(f"\nFAIL {failure.phase} {failure.path}")
        print(failure.output.rstrip())
    if len(failures) > 20:
        print(f"\n... {len(failures) - 20} more failures omitted")
    return 1 if failures else 0


if __name__ == "__main__":
    raise SystemExit(main())
