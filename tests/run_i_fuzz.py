"""Mutation fuzzer for the I front end.

Diagnostics are recoverable rather than fatal, which means the compiler keeps
running after an error and can reach states the fixtures never produce. This
mutates valid sources into broken ones and asserts the front end always degrades
into diagnostics rather than a crash, a hang, or malformed JSON.

The invariants checked for every input:
  * the process exits 0 or 1, never a signal and never a timeout
  * `--diagnostics=json` emits either nothing or one well-formed JSON array
  * a failing exit is accompanied by at least one diagnostic

Runs are deterministic for a given --seed so a failure is reproducible; failing
inputs are written to build/i_fuzz for triage.
"""
from __future__ import annotations

import argparse
import json
import random
import subprocess
import sys
from dataclasses import dataclass
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
I_EXE = ROOT / "build" / "I.exe"
CORPUS_DIRS = (
    ROOT / "tests" / "i-fuzz" / "seeds",
    ROOT / "tests" / "i-torture" / "compile",
    ROOT / "tests" / "i-torture" / "execute",
)

# Fragments chosen to stress the paths that recover rather than exit: unbalanced
# brackets, declaration heads, and the tokens whose lexing can fail.
INJECT = (
    "{", "}", "(", ")", "[", "]", ";", ":", ",", "<", ">", "=", "*", "&", "~", "@",
    "proc", "struct", "union", "enum", "alias", "static", "label", "goto", "return",
    "case", "default", "switch", "for", "while", "import", "cinclude", "define",
    "'", '"', "'a'", "'\\q'", "#", "# if 0", "//", "/*", "shl=", "shr=", "...",
    "<T>", "T", "external;", "printfmt", "sizeof", "cast", "0x", "1e", "999999999999999999999",
)


@dataclass
class Failure:
    kind: str
    source: str
    detail: str


def mutate(text: str, rng: random.Random) -> str:
    chars = list(text)
    for _ in range(rng.randint(1, 6)):
        if not chars:
            break
        op = rng.choice(("delete", "inject", "duplicate", "truncate", "swap"))
        i = rng.randrange(len(chars))
        if op == "delete":
            del chars[i]
        elif op == "inject":
            chars.insert(i, rng.choice(INJECT))
        elif op == "duplicate":
            chars.insert(i, chars[i])
        elif op == "swap" and i + 1 < len(chars):
            chars[i], chars[i + 1] = chars[i + 1], chars[i]
        else:
            chars = chars[:i]
    return "".join(chars)


def check(path: Path, timeout: float) -> Failure | None:
    for mode in ([], ["--diagnostics=json"]):
        try:
            proc = subprocess.run(
                [str(I_EXE), "check", str(path), *mode],
                capture_output=True, text=True, timeout=timeout,
            )
        except subprocess.TimeoutExpired:
            return Failure("hang", path.read_text(encoding="utf-8", errors="replace"),
                           f"no exit within {timeout}s with {mode}")
        if proc.returncode not in (0, 1):
            return Failure("crash", path.read_text(encoding="utf-8", errors="replace"),
                           f"exit {proc.returncode} with {mode}\n{proc.stdout}\n{proc.stderr}")
        if not mode:
            continue
        out = proc.stdout.strip()
        if not out:
            if proc.returncode != 0:
                return Failure("silent-failure", path.read_text(encoding="utf-8", errors="replace"),
                               "non-zero exit with no diagnostics")
            continue
        try:
            payload = json.loads(out)
        except json.JSONDecodeError as exc:
            return Failure("bad-json", path.read_text(encoding="utf-8", errors="replace"),
                           f"{exc}\n{out[:600]}")
        if not isinstance(payload, list):
            return Failure("bad-json", path.read_text(encoding="utf-8", errors="replace"),
                           f"expected a JSON array, got {type(payload).__name__}")
        if proc.returncode != 0 and not payload:
            return Failure("silent-failure", path.read_text(encoding="utf-8", errors="replace"),
                           "non-zero exit with an empty diagnostic array")
    return None


def main() -> int:
    parser = argparse.ArgumentParser(description="Mutation fuzzer for the I front end.")
    parser.add_argument("--iterations", type=int, default=400)
    parser.add_argument("--seed", type=int, default=20240611, help="Fixed so runs reproduce.")
    parser.add_argument("--timeout", type=float, default=10.0)
    parser.add_argument("--keep-going", action="store_true", help="Report every failure, not just the first.")
    args = parser.parse_args()

    if not I_EXE.exists():
        print(f"i-fuzz: missing compiler {I_EXE}")
        return 1

    corpus = [p for d in CORPUS_DIRS if d.exists() for p in sorted(d.rglob("*.i"))]
    if not corpus:
        print("i-fuzz: empty corpus")
        return 1

    work_dir = ROOT / "build" / "i_fuzz"
    work_dir.mkdir(parents=True, exist_ok=True)
    scratch = work_dir / "current.i"

    rng = random.Random(args.seed)
    failures: list[Failure] = []
    for n in range(args.iterations):
        base = corpus[n % len(corpus)]
        text = base.read_text(encoding="utf-8", errors="replace")
        scratch.write_text(mutate(text, rng), encoding="utf-8", newline="\n")
        failure = check(scratch, args.timeout)
        if failure:
            repro = work_dir / f"fail_{len(failures):03}_{failure.kind}.i"
            repro.write_text(failure.source, encoding="utf-8", newline="\n")
            failure.detail += f"\nrepro: {repro}"
            failures.append(failure)
            if not args.keep_going:
                break

    checked = len(failures) if failures and not args.keep_going else args.iterations
    print(f"i-fuzz: {args.iterations - len(failures)}/{args.iterations} inputs "
          f"handled cleanly (seed={args.seed}, corpus={len(corpus)})")
    for failure in failures[:5]:
        print(f"\nFAIL {failure.kind}")
        print(failure.detail.rstrip())
        print("--- input ---")
        print(failure.source[:800])
    return 1 if failures else 0


if __name__ == "__main__":
    raise SystemExit(main())
