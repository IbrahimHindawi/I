"""Build and run the i-torture execute suite with MSVC `cl`, not clang.

Everything ilang emits had only ever met clang, so `shape.md` section 7 -- what
the generated C actually promises -- was a claim with nothing behind it. The
first run of this found three constructs that were clang-only:

  * `__alignof__`, a GCC extension. Now `_Alignof`, which is C11 and takes a
    *type*, so member alignment asks about the member's declared type rather
    than about `((P *)0)->x`, which no standard spelling accepts.
  * `enum E : T`, which is C23. MSVC's C mode rejects it at every /std level,
    `: int` included. The underlying type is asserted now instead of dictated.
  * `_Static_assert`, the keyword, which MSVC only has under /std:c11. The
    `static_assert` macro from <assert.h> works everywhere by default.

Compiling is not the bar -- generated C can compile and still mean the wrong
thing -- so this compares program output against the same `.expected` files
clang is held to.

Skipped, not failed, when cl is absent: a clang-only machine should still be
able to run the suite.
"""
from __future__ import annotations

import argparse
import os
import pathlib
import subprocess
import sys

ROOT = pathlib.Path(__file__).resolve().parent.parent
I_EXE = ROOT / "build" / "I.exe"
EXECUTE_DIR = ROOT / "tests" / "i-torture" / "execute"
BUILD_DIR = ROOT / "build" / "msvc_tests"

DEFAULT_CL = pathlib.Path(
    r"C:\Program Files (x86)\Microsoft Visual Studio\2022\BuildTools"
    r"\VC\Tools\MSVC\14.44.35207\bin\Hostx64\x64\cl.exe")
DEFAULT_VCVARS = pathlib.Path(
    r"C:\Program Files (x86)\Microsoft Visual Studio\2022\BuildTools"
    r"\VC\Auxiliary\Build\vcvars64.bat")


def msvc_environment(vcvars: pathlib.Path) -> dict[str, str] | None:
    """cl needs INCLUDE and LIB, which only vcvars knows how to set."""
    if not vcvars.exists():
        return None
    # shell=True, because the batch path has spaces and cmd wants the quoting
    # done its own way; passing an argv list here yields an empty environment.
    result = subprocess.run(
        f'"{vcvars}" >nul 2>&1 && set',
        capture_output=True, text=True, shell=True)
    if result.returncode != 0:
        return None
    env = dict(os.environ)
    for line in result.stdout.splitlines():
        if "=" in line:
            key, value = line.split("=", 1)
            env[key] = value
    return env if "INCLUDE" in env else None


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--cl", type=pathlib.Path,
                        default=pathlib.Path(os.environ.get("CL_EXE", DEFAULT_CL)))
    parser.add_argument("--vcvars", type=pathlib.Path,
                        default=pathlib.Path(os.environ.get("VCVARS", DEFAULT_VCVARS)))
    args = parser.parse_args()

    if not args.cl.exists():
        print("msvc: skipped, cl.exe not found")
        return 0
    env = msvc_environment(args.vcvars)
    if env is None:
        print("msvc: skipped, could not read the build environment from vcvars")
        return 0
    if not I_EXE.exists():
        print(f"msvc: missing compiler {I_EXE}")
        return 1

    BUILD_DIR.mkdir(parents=True, exist_ok=True)
    passed = 0
    failures: list[tuple[str, str]] = []

    for case in sorted(EXECUTE_DIR.glob("*.i")):
        expected_path = case.with_suffix(".expected")
        if not expected_path.exists():
            continue
        c_path = BUILD_DIR / (case.stem + ".c")
        exe_path = BUILD_DIR / (case.stem + ".exe")

        generated = subprocess.run(
            [str(I_EXE), "compile", str(case), "-o", str(c_path), "--no-header"],
            capture_output=True, text=True)
        if generated.returncode != 0:
            failures.append((case.name, "ilang: " + generated.stdout.strip()[:300]))
            continue

        built = subprocess.run(
            [str(args.cl), "-nologo",
             "-I", str(ROOT / "src" / "std"), "-I", str(ROOT / "src"),
             str(c_path),
             "-Fe:" + str(exe_path),
             "-Fo:" + str(BUILD_DIR / (case.stem + ".obj"))],
            capture_output=True, text=True, env=env, cwd=str(BUILD_DIR))
        if built.returncode != 0:
            diagnostics = [line.strip() for line in
                           ((built.stdout or "") + (built.stderr or "")).splitlines()
                           if " error " in line or "error C" in line]
            failures.append((case.name, "cl: " + "; ".join(diagnostics[:3])))
            continue

        ran = subprocess.run([str(exe_path)], capture_output=True, text=True)
        want = expected_path.read_text(encoding="utf-8").replace("\r\n", "\n")
        got = ran.stdout.replace("\r\n", "\n")
        if got != want:
            failures.append((case.name, f"output differs: want {want[:120]!r} got {got[:120]!r}"))
            continue
        passed += 1

    total = passed + len(failures)
    if failures:
        print(f"msvc: {passed}/{total} matched .expected with cl")
        for name, why in failures:
            print(f"  {name}: {why}")
        return 1
    print(f"msvc: {passed}/{total} passed with cl.exe (same .expected as clang)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
