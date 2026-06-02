from __future__ import annotations

import subprocess
import sys
from dataclasses import dataclass
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
BUILD = ROOT / "build"
TEST_DIR = BUILD / "i_tests"
I_EXE = BUILD / "I.exe"


@dataclass(frozen=True)
class Case:
    name: str
    source: str
    expected_stdout: str
    generated_contains: tuple[str, ...] = ()
    header_contains: tuple[str, ...] = ()


CASES = (
    Case(
        name="basic_generics",
        source=r'''
import "stdio.h"
define("SAHA_IMPLEMENTATION")
import "saha.i.h"

memops_arena_initialize:proc(arena:*memops_arena)->void={external_emit;}
memops_arena_push:proc(arena:*memops_arena, alloc_size:u64, align:u64)->*void={external;}

memops_arena_push_array:proc<T>(arena:*memops_arena, count:u64)->*void={
    alloc_size:u64 = sizeof(T) * count;
    alignment:u64 = alignof(T);
    return memops_arena_push(arena, alloc_size, alignment);
}

array:struct<T> = {
    length:u64;
    border:u64;
    data:*T;
}

array<T>reserve:proc<T>(arena:*memops_arena, length:u64)->array<T>={
    arr:array<T> = {};
    if (length == 0) {
        return arr;
    }
    arr.data = memops_arena_push_array<T>(arena, length);
    arr.border = length;
    return arr;
}

main:proc()->i32={
    arena:memops_arena = {};
    memops_arena_initialize(&arena);
    a:array<i32> = array<i32>reserve(&arena, 4);
    for (i:i32=0; i<4; i+=1) {
        a.data[i] = i + 10;
    }
    printf("%d %d %d %d\n", a.data[0], a.data[1], a.data[2], a.data[3]);
    return 0;
}
''',
        expected_stdout="10 11 12 13\n",
        generated_contains=("array_i32_reflect", "memops_arena_push_array_i32"),
        header_contains=("void memops_arena_initialize(memops_arena * arena);",),
    ),
    Case(
        name="enum_reflect_preprocessor",
        source=r'''
import "stdio.h"
#define I_TEST_HP 77

Color:enum = {
    Red = 1,
    Green,
    Blue,
}

Player:struct = {
    kind:Color;
    hp:i32;
}

main:proc()->i32={
    p:Player = {};
    p.kind = Color_Green;
    p.hp = I_TEST_HP;
    printf("%s %llu %s %llu %s %s %d %d\n",
        Color_reflect.name,
        Color_reflect.value_count,
        Player_reflect.name,
        Player_reflect.field_count,
        Player_reflect.fields[0].name,
        Player_reflect.fields[1].type,
        p.kind,
        p.hp);
    return 0;
}
''',
        expected_stdout="Color 3 Player 2 kind i32 2 77\n",
        generated_contains=("#define I_TEST_HP 77", "typedef enum Color", "Player_reflect"),
        header_contains=("extern const i_reflect_type Player_reflect;", "typedef enum Color"),
    ),
    Case(
        name="boring_c_surface",
        source=r'''
import "stdio.h"
#define WINCALL
#define TWICE(x) ((x) * 2)

Packet:struct = {
    values:[4]i32;
    flags:u32;
}

Node:struct = {
    value:i32;
    parent:*Node;
}

platform_add:proc[WINCALL](a:i32, b:i32)->i32 = {
    return a + b;
}

main:proc()->i32={
    p:Packet = {};
    i:i32 = 0;
    while (i < 4) {
        if (i == 2) {
            i += 1;
            continue;
        }
        p.values[i] = i shl 1;
        i += 1;
    }
    p.flags = 16 shr 1;
    p.flags |= 1;
    p.flags &= 9;
    p.flags ^= 1;
    total:i32 = 0;
    mod:i32 = p.flags % 4;
    switch (p.values[1]) {
        case 2:
            total = platform_add(p.values[1], p.flags + mod);
            break;
        default:
            total = 99;
            break;
    }
    if (!(total >= 10 and total <= 10 and p.values[2] == 0) or p.values[3] != 6) {
        total = -1;
    }
    nodes:[3]Node = {};
    nodes[0].value = 11;
    nodes[1].value = 22;
    nodes[2].value = 33;
    nodes[2].parent = &nodes[0];
    node_index:long = &nodes[2] - nodes;
    parent_index:long = nodes[2].parent - nodes;
    printf("%d %llu %llu %ld %ld %d\n", total + TWICE(4), Packet_reflect.field_count, Packet_reflect.fields[0].size, node_index, parent_index, nodes[2].parent[0].value);
    return 0;
}
''',
        expected_stdout="18 2 16 2 0 11\n",
        generated_contains=("i32 values[4];", "while (", "switch (", "WINCALL platform_add", "TWICE(4)", "#line 1 ", "&(nodes[2]) - nodes"),
        header_contains=("extern const i_reflect_type Packet_reflect;", "i32 values[4];", "WINCALL platform_add"),
    ),
    Case(
        name="gin_c_surface",
        source=r'''
import "stdio.h"
#define WINCALL

I32:alias = i32;
Binary:alias = proc[WINCALL](a:i32, b:i32)->i32;

Value:union = {
    i:I32;
    f:f32;
}

add:proc[WINCALL](a:i32, b:i32)->i32 = {
    return a + b;
}

choose:proc(a:i32, ...)->i32 = {
    return a;
}

main:proc()->i32 = {
    v:Value = {};
    label:*const char = "he" "llo";
    v.i = 3;
    cb:Binary = add;
    total:i32 = 0;
    i:i32 = 0;
    do {
        total += i == 1 ? cb(v.i, 2) : choose(1, 2, 3);
        i += 1;
    } while (i < 3);
    printf("%d %llu %s %s %s\n", total, Value_reflect.field_count, Value_reflect.fields[0].name, Value_reflect.fields[1].name, label);
    return 0;
}
''',
        expected_stdout="7 2 i f hello\n",
        generated_contains=(
            "typedef i32 I32;",
            "typedef i32 (WINCALL *Binary)(i32, i32);",
            "uniondef(Value)",
            "do {",
            " ? ",
            "choose(i32 a, ...)",
            'const char * label = "hello";',
        ),
        header_contains=("typedef i32 I32;", "typedef i32 (WINCALL *Binary)(i32, i32);", "uniondef(Value)"),
    ),
    Case(
        name="initializer_lists",
        source=r'''
import "stdio.h"

Pair:struct = {
    a:i32;
    b:i32;
}

g_pairs:[3]Pair = {
    {.a = 1, .b = 2},
    [2] = {.a = 5, .b = 8},
};

g_map:[2][3]const u32 = {
    [1] = {[2] = 9},
};

main:proc()->i32 = {
    printf("%d %d %u\n", g_pairs[0].a + g_pairs[2].b, g_pairs[1].a, g_map[1][2]);
    return 0;
}
''',
        expected_stdout="9 0 9\n",
        generated_contains=(
            "Pair g_pairs[3] = {{.a = 1, .b = 2}, [2] = {.a = 5, .b = 8}};",
            "const u32 g_map[2][3] = {[1] = {[2] = 9}};",
        ),
        header_contains=("extern Pair g_pairs[3];", "extern const u32 g_map[2][3];"),
    ),
    Case(
        name="postfix_address_deref",
        source=r'''
import "stdio.h"

Node:struct = {
    value:i32;
    next:*Node;
}

main:proc()->i32 = {
    nodes:[2]Node = {};
    nodes[0].value = 10;
    nodes[1].value = 20;
    nodes[0].next = nodes[1].&;
    nodes[0].next.*.value += 5;
    roundtrip:*Node = nodes[0].next.*.&;
    roundtrip.*.value += 2;
    printf("%d %d %d\n", nodes[0].value, nodes[1].value, roundtrip.*.value);
    return 0;
}
''',
        expected_stdout="10 27 27\n",
        generated_contains=(
            "nodes[0].next = &(nodes[1]);",
            "nodes[0].next[0].value += 5;",
            "Node * roundtrip = &(nodes[0].next[0]);",
            "roundtrip[0].value += 2;",
        ),
        header_contains=("structdecl(Node);", "structdef(Node)"),
    ),
    Case(
        name="external_globals",
        source=r'''
import "stdio.h"

State:struct = {
    value:i32;
}

g_state:State = external;

main:proc()->i32 = {
    return 0;
}
''',
        expected_stdout="",
        generated_contains=(
            "extern State g_state;",
            "i32 main(void)",
        ),
        header_contains=("extern State g_state;",),
    ),
)


MODULE_SOURCE = r'''
SharedKind:enum = {
    None,
    Add,
}

SharedPayload:struct = {
    values:[3]i32;
}

shared_sum:proc(p:*SharedPayload)->i32 = {
    return p[0].values[0] + p[0].values[1] + p[0].values[2];
}
'''


MODULE_APP_SOURCE = r'''
import "stdio.h"
import "module.i"

main:proc()->i32 = {
    payload:SharedPayload = {};
    payload.values[0] = 3;
    payload.values[1] = 4;
    payload.values[2] = 5;
    result:i32 = shared_sum(&payload);
    printf("%d %s %llu %d\n", result, SharedPayload_reflect.fields[0].name, SharedKind_reflect.value_count, SharedKind_Add);
    return 0;
}
'''


def run(cmd: list[str], cwd: Path = ROOT) -> subprocess.CompletedProcess[str]:
    return subprocess.run(cmd, cwd=cwd, text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)


def main() -> int:
    TEST_DIR.mkdir(parents=True, exist_ok=True)

    build = run([sys.executable, "bunyan.py", "build", "debug"])
    if build.returncode != 0:
        print(build.stdout)
        return build.returncode

    for case in CASES:
        src = TEST_DIR / f"{case.name}.i"
        c_path = TEST_DIR / f"{case.name}.c"
        h_path = TEST_DIR / f"{case.name}.h"
        exe = TEST_DIR / f"{case.name}.exe"
        src.write_text(case.source.strip() + "\n", encoding="utf-8", newline="\n")

        translate = run([str(I_EXE), str(src), str(c_path)])
        if translate.returncode != 0:
            print(translate.stdout)
            return translate.returncode

        generated = c_path.read_text(encoding="utf-8")
        for needle in case.generated_contains:
            if needle not in generated:
                print(f"{case.name}: generated C missing {needle!r}")
                return 1
        if not h_path.exists():
            print(f"{case.name}: generated header missing")
            return 1
        header = h_path.read_text(encoding="utf-8")
        for needle in case.header_contains:
            if needle not in header:
                print(f"{case.name}: generated header missing {needle!r}")
                return 1

        compile_result = run([
            "clang.exe",
            str(c_path),
            "-I",
            "src",
            "-I",
            "extern/haikal/src/runtime",
            "-o",
            str(exe),
        ])
        if compile_result.returncode != 0:
            print(compile_result.stdout)
            return compile_result.returncode

        program = run([str(exe)])
        if program.returncode != 0:
            print(program.stdout)
            return program.returncode
        if program.stdout != case.expected_stdout:
            print(f"{case.name}: stdout mismatch")
            print("expected:")
            print(case.expected_stdout)
            print("actual:")
            print(program.stdout)
            return 1

        print(f"ok {case.name}")

    module_i = TEST_DIR / "module.i"
    module_c = TEST_DIR / "module.c"
    module_h = TEST_DIR / "module.h"
    app_i = TEST_DIR / "module_app.i"
    app_c = TEST_DIR / "module_app.c"
    app_h = TEST_DIR / "module_app.h"
    app_exe = TEST_DIR / "module_app.exe"
    module_i.write_text(MODULE_SOURCE.strip() + "\n", encoding="utf-8", newline="\n")
    app_i.write_text(MODULE_APP_SOURCE.strip() + "\n", encoding="utf-8", newline="\n")

    for src, c_path in ((module_i, module_c), (app_i, app_c)):
        translate = run([str(I_EXE), str(src), str(c_path)])
        if translate.returncode != 0:
            print(translate.stdout)
            return translate.returncode

    if not module_h.exists() or not app_h.exists():
        print("module_import: generated headers missing")
        return 1
    app_generated = app_c.read_text(encoding="utf-8")
    if '#include "module.h"' not in app_generated:
        print("module_import: app C did not include generated module header")
        return 1

    compile_result = run([
        "clang.exe",
        str(app_c),
        str(module_c),
        "-I",
        str(TEST_DIR),
        "-I",
        "src",
        "-I",
        "extern/haikal/src/runtime",
        "-o",
        str(app_exe),
    ])
    if compile_result.returncode != 0:
        print(compile_result.stdout)
        return compile_result.returncode

    program = run([str(app_exe)])
    if program.returncode != 0:
        print(program.stdout)
        return program.returncode
    expected = "12 values 2 1\n"
    if program.stdout != expected:
        print("module_import: stdout mismatch")
        print("expected:")
        print(expected)
        print("actual:")
        print(program.stdout)
        return 1

    print("ok module_import")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
