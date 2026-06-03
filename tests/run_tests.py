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
cinclude "stdio.h"
import "C:/devel/i/src/runtime/memops.i"

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
    arr.data = cast(memops_arena_push_array<T>(arena, length), *T);
    arr.border = length;
    return arr;
}

Bag:struct = {
    items:array<i32>;
    data:*i32;
    values:[2]i32;
}

main:proc()->i32={
    arena:memops_arena = {};
    memops_arena_initialize(&arena);
    a:array<i32> = array<i32>reserve(&arena, 4);
    for (i:i32=0; i<4; i+=1) {
        a.data[i] = i + 10;
    }
    printf("%d %d %d %d %s %llu %llu %s %s %llu %d %s %s %llu %d %d %s %s %s %s\n",
        a.data[0],
        a.data[1],
        a.data[2],
        a.data[3],
        array_i32_reflect.name,
        array_i32_reflect.align,
        array_i32_reflect.field_count,
        array_i32_reflect.fields[0].name,
        array_i32_reflect.fields[0].type,
        array_i32_reflect.fields[0].pointer_depth,
        array_i32_reflect.fields[0].kind,
        array_i32_reflect.fields[2].name,
        array_i32_reflect.fields[2].type,
        array_i32_reflect.fields[2].pointer_depth,
        array_i32_reflect.fields[2].kind,
        Bag_reflect.fields[0].kind,
        Bag_reflect.fields[0].base_type,
        Bag_reflect.fields[0].generic_arg_type,
        Bag_reflect.fields[1].elem_type,
        Bag_reflect.fields[2].elem_type);
    return 0;
}
''',
        expected_stdout="10 11 12 13 array_i32 8 3 length u64 0 0 data ptr_i32 1 1 2 array i32 i32 i32\n",
        generated_contains=("array_i32_reflect", "memops_arena_push_array_i32", "generic_arg_type"),
        header_contains=("void memops_arena_initialize(memops_arena * arena);",),
    ),
    Case(
        name="enum_reflect_preprocessor",
        source=r'''
cinclude "stdio.h"
#define I_TEST_HP 77

i_reflect_field:struct = {
    name:*const char;
    type:*const char;
    attrs:*const char;
    offset:u64;
    size:u64;
    align:u64;
    kind:i32;
    array_count:u64;
    pointer_depth:u64;
    base_type:*const char;
    elem_type:*const char;
    generic_arg_type:*const char;
    external;
}

i_reflect_type:struct = {
    name:*const char;
    size:u64;
    align:u64;
    field_count:u64;
    fields:*const i_reflect_field;
    external;
}

i_reflect_enum_value:struct = {
    name:*const char;
    value:i32;
    external;
}

i_reflect_enum:struct = {
    name:*const char;
    size:u64;
    align:u64;
    value_count:u64;
    values:*const i_reflect_enum_value;
    external;
}

i_reflect_find_field:proc(type:*const i_reflect_type, name:*const char)->*const i_reflect_field = { external; }
i_reflect_find_enum_value_by_name:proc(type:*const i_reflect_enum, name:*const char)->*const i_reflect_enum_value = { external; }
i_reflect_find_enum_value_by_value:proc(type:*const i_reflect_enum, value:i32)->*const i_reflect_enum_value = { external; }

Color:enum = {
    Red = 1,
    Green,
    Blue,
}

Player:struct = {
    kind:Color;
    hp:i32 @ "editor,serialize";
}

main:proc()->i32={
    p:Player = {};
    p.kind = Color_Green;
    p.hp = I_TEST_HP;
    hp_field:*const i_reflect_field = i_reflect_find_field(&Player_reflect, "hp");
    green_value:*const i_reflect_enum_value = i_reflect_find_enum_value_by_name(&Color_reflect, "Green");
    blue_value:*const i_reflect_enum_value = i_reflect_find_enum_value_by_value(&Color_reflect, Color_Blue);
    printf("%s %llu %llu %llu %s %d %s %d %s %d %s %llu %s %s %s %d %d %s %s %d %s\n",
        Color_reflect.name,
        Color_reflect.size,
        Color_reflect.align,
        Color_reflect.value_count,
        Color_reflect.values[0].name,
        Color_reflect.values[0].value,
        Color_reflect.values[1].name,
        Color_reflect.values[1].value,
        Color_reflect.values[2].name,
        Color_reflect.values[2].value,
        Player_reflect.name,
        Player_reflect.field_count,
        Player_reflect.fields[0].name,
        Player_reflect.fields[1].type,
        Player_reflect.fields[1].attrs,
        p.kind,
        p.hp,
        hp_field[0].name,
        green_value[0].name,
        green_value[0].value,
        blue_value[0].name);
    return 0;
}
''',
        expected_stdout="Color 4 4 3 Red 1 Green 2 Blue 3 Player 2 kind i32 editor,serialize 2 77 hp Green 2 Blue\n",
        generated_contains=("#define I_TEST_HP 77", "typedef enum Color", "Player_reflect", "i_reflect_find_field", "editor,serialize"),
        header_contains=("extern const i_reflect_type Player_reflect;", "typedef enum Color"),
    ),
    Case(
        name="boring_c_surface",
        source=r'''
cinclude "stdio.h"
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
    printf("%d %llu %llu %llu %llu %d %llu %llu %d %ld %ld %d\n",
        total + TWICE(4),
        Packet_reflect.field_count,
        Packet_reflect.align,
        Packet_reflect.fields[0].size,
        Packet_reflect.fields[0].array_count,
        Packet_reflect.fields[0].kind,
        Packet_reflect.fields[0].align,
        Node_reflect.fields[1].pointer_depth,
        Node_reflect.fields[1].kind,
        node_index,
        parent_index,
        nodes[2].parent[0].value);
    return 0;
}
''',
        expected_stdout="18 2 4 16 4 3 4 1 1 2 0 11\n",
        generated_contains=("i32 values[4];", "while (", "switch (", "WINCALL platform_add", "TWICE(4)", "#line 1 ", "&(nodes[2]) - nodes", "pointer_depth", "array_count"),
        header_contains=("extern const i_reflect_type Packet_reflect;", "i32 values[4];", "WINCALL platform_add"),
    ),
    Case(
        name="gin_c_surface",
        source=r'''
cinclude "stdio.h"
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
cinclude "stdio.h"

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
cinclude "stdio.h"

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
        name="function_pointer_types",
        source=r'''
cinclude "stdio.h"

Callback:alias = *proc(x:i32, label:*const char)->i32;

Holder:struct = {
    cb:Callback;
}

call_twice:proc(cb:Callback)->i32 = {
    return cb(5, "hi") + cb(7, "ok");
}

add_label:proc(x:i32, label:*const char)->i32 = {
    return x + cast(label[0], i32);
}

main:proc()->i32 = {
    h:Holder = {};
    h.cb = add_label;
    cb:Callback = h.cb;
    printf("%d %d\n", call_twice(cb), cb(1, "A"));
    return 0;
}
''',
        expected_stdout="227 66\n",
        generated_contains=("typedef i32 (*Callback)(i32, const char *);", "Callback cb;", "i32 call_twice(Callback cb)"),
        header_contains=("typedef i32 (*Callback)(i32, const char *);", "Callback cb;"),
    ),
    Case(
        name="external_globals",
        source=r'''
cinclude "stdio.h"

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
cinclude "stdio.h"
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
            "src/runtime",
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

    line_map_i = TEST_DIR / "generated_line_map.i"
    line_map_c = TEST_DIR / "generated_line_map.c"
    line_map_h = TEST_DIR / "generated_line_map.h"
    line_map_source = r'''
Box:struct<T> = {
    value:T;
}

Box<T>get:proc<T>(box:Box<T>)->T = {
    return box.value;
}

main:proc()->i32 = {
    box:Box<i32> = {.value = 1};
    value:i32 = Box<i32>get(box);
    value += 2;
    return value;
}
'''.strip() + "\n"
    line_map_i.write_text(line_map_source, encoding="utf-8", newline="\n")
    line_map = run([str(I_EXE), str(line_map_i), str(line_map_c)])
    if line_map.returncode != 0:
        print(line_map.stdout)
        return line_map.returncode
    line_map_generated = line_map_c.read_text(encoding="utf-8")
    return_line = line_map_source.splitlines().index("    return value;") + 1
    line_map_path = str(line_map_i).replace("\\", "\\\\")
    expected_line = f'#line {return_line} "{line_map_path}"'
    if expected_line not in line_map_generated:
        print("generated_line_map: expected statement #line directive")
        print(f"missing: {expected_line}")
        return 1
    generated_reflect_marker = '#line 1 "<generated>"\n#ifndef I_REFLECT_TYPES_DEFINED'
    if generated_reflect_marker not in line_map_generated:
        print("generated_line_map: expected reflection runtime to be marked as generated code")
        return 1
    if not line_map_h.exists():
        print("generated_line_map: expected generated header")
        return 1
    line_map_header = line_map_h.read_text(encoding="utf-8")
    if generated_reflect_marker not in line_map_header:
        print("generated_line_map: expected header reflection runtime to be marked as generated code")
        return 1
    proc_line = line_map_source.splitlines().index("Box<T>get:proc<T>(box:Box<T>)->T = {") + 1
    expected_proc_line = f'#line {proc_line} "{line_map_path}"'
    if expected_proc_line not in line_map_header:
        print("generated_line_map: expected proc prototype #line directive in header")
        print(f"missing: {expected_proc_line}")
        return 1
    print("ok generated_line_map")

    line_map_error_i = TEST_DIR / "generated_line_map_error.i"
    line_map_error_c = TEST_DIR / "generated_line_map_error.c"
    line_map_error_i.write_text(r'''
bad_c_proc:proc()->MissingCType = { external_emit; }

main:proc()->i32 = {
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    line_map_error = run([str(I_EXE), str(line_map_error_i), str(line_map_error_c)])
    if line_map_error.returncode != 0:
        print(line_map_error.stdout)
        return line_map_error.returncode
    line_map_error_compile = run([
        "clang.exe",
        str(line_map_error_c),
        "-I",
        "src",
        "-I",
        "src/runtime",
        "-o",
        str(TEST_DIR / "generated_line_map_error.exe"),
    ])
    if (
        line_map_error_compile.returncode == 0
        or str(line_map_error_i) not in line_map_error_compile.stdout
        or ":1:" not in line_map_error_compile.stdout
        or "MissingCType" not in line_map_error_compile.stdout
    ):
        print("generated_line_map_error: expected clang diagnostic to map generated C error back to .i line")
        print(line_map_error_compile.stdout)
        return 1
    print("ok generated_line_map_error")

    line_map_param_i = TEST_DIR / "generated_line_map_param_error.i"
    line_map_param_c = TEST_DIR / "generated_line_map_param_error.c"
    line_map_param_source = r'''
bad_param_proc:proc(
    ok:i32,
    bad:MISSING_C_PARAM_TYPE
)->i32 = { external_emit; }

main:proc()->i32 = {
    return 0;
}
'''.strip() + "\n"
    line_map_param_i.write_text(line_map_param_source, encoding="utf-8", newline="\n")
    line_map_param = run([str(I_EXE), str(line_map_param_i), str(line_map_param_c)])
    if line_map_param.returncode != 0:
        print(line_map_param.stdout)
        return line_map_param.returncode
    line_map_param_compile = run([
        "clang.exe",
        str(line_map_param_c),
        "-I",
        "src",
        "-I",
        "src/runtime",
        "-o",
        str(TEST_DIR / "generated_line_map_param_error.exe"),
    ])
    bad_param_line = line_map_param_source.splitlines().index("    bad:MISSING_C_PARAM_TYPE") + 1
    if (
        line_map_param_compile.returncode == 0
        or str(line_map_param_i) not in line_map_param_compile.stdout
        or f":{bad_param_line}:" not in line_map_param_compile.stdout
        or "MISSING_C_PARAM_TYPE" not in line_map_param_compile.stdout
    ):
        print("generated_line_map_param_error: expected clang diagnostic to map proc param error back to exact .i parameter line")
        print(line_map_param_compile.stdout)
        return 1
    print("ok generated_line_map_param_error")

    line_map_field_i = TEST_DIR / "generated_line_map_field_error.i"
    line_map_field_c = TEST_DIR / "generated_line_map_field_error.c"
    line_map_field_source = r'''
ExternalPayload:struct = {
    ok:i32;
    bad:MISSING_C_FIELD_TYPE;
}

main:proc()->i32 = {
    return 0;
}
'''.strip() + "\n"
    line_map_field_i.write_text(line_map_field_source, encoding="utf-8", newline="\n")
    line_map_field = run([str(I_EXE), str(line_map_field_i), str(line_map_field_c)])
    if line_map_field.returncode != 0:
        print(line_map_field.stdout)
        return line_map_field.returncode
    line_map_field_compile = run([
        "clang.exe",
        str(line_map_field_c),
        "-I",
        "src",
        "-I",
        "src/runtime",
        "-o",
        str(TEST_DIR / "generated_line_map_field_error.exe"),
    ])
    bad_field_line = line_map_field_source.splitlines().index("    bad:MISSING_C_FIELD_TYPE;") + 1
    if (
        line_map_field_compile.returncode == 0
        or str(line_map_field_i) not in line_map_field_compile.stdout
        or f":{bad_field_line}:" not in line_map_field_compile.stdout
        or "MISSING_C_FIELD_TYPE" not in line_map_field_compile.stdout
    ):
        print("generated_line_map_field_error: expected clang diagnostic to map struct field error back to exact .i field line")
        print(line_map_field_compile.stdout)
        return 1
    print("ok generated_line_map_field_error")

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
    if "shared_sum(" not in app_generated or "SharedPayload_reflect" not in app_generated:
        print("module_import: app C did not aggregate imported module definitions")
        return 1

    compile_result = run([
        "clang.exe",
        str(app_c),
        "-I",
        str(TEST_DIR),
        "-I",
        "src",
        "-I",
        "src/runtime",
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

    diamond_shared_i = TEST_DIR / "diamond_shared.i"
    diamond_left_i = TEST_DIR / "diamond_left.i"
    diamond_right_i = TEST_DIR / "diamond_right.i"
    diamond_app_i = TEST_DIR / "diamond_app.i"
    diamond_app_c = TEST_DIR / "diamond_app.c"
    diamond_app_exe = TEST_DIR / "diamond_app.exe"
    diamond_shared_i.write_text(r'''
DiamondPayload:struct = {
    value:i32;
}

diamond_value:proc(p:DiamondPayload)->i32 = {
    return p.value;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    diamond_left_i.write_text(f'''
import "{diamond_shared_i.as_posix()}"

diamond_left:proc(p:DiamondPayload)->i32 = {{
    return diamond_value(p) + 1;
}}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    diamond_right_i.write_text(f'''
import "{diamond_shared_i.as_posix()}"

diamond_right:proc(p:DiamondPayload)->i32 = {{
    return diamond_value(p) + 2;
}}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    diamond_app_i.write_text(f'''
cinclude "stdio.h"
import "{diamond_left_i.as_posix()}"
import "{diamond_right_i.as_posix()}"
import "{diamond_shared_i.as_posix()}"

main:proc()->i32 = {{
    p:DiamondPayload = {{.value = 5}};
    printf("%d %d %d\\n", diamond_value(p), diamond_left(p), diamond_right(p));
    return 0;
}}
'''.strip() + "\n", encoding="utf-8", newline="\n")

    diamond = run([str(I_EXE), str(diamond_app_i), str(diamond_app_c)])
    if diamond.returncode != 0:
        print(diamond.stdout)
        return diamond.returncode
    diamond_generated = diamond_app_c.read_text(encoding="utf-8")
    if diamond_generated.count("structdef(DiamondPayload)") != 1 or diamond_generated.count("i32 diamond_value(") != 2:
        print("module_diamond_import: expected shared module declarations to be emitted once")
        return 1
    diamond_compile = run([
        "clang.exe",
        str(diamond_app_c),
        "-I",
        str(TEST_DIR),
        "-I",
        "src",
        "-I",
        "src/runtime",
        "-o",
        str(diamond_app_exe),
    ])
    if diamond_compile.returncode != 0:
        print(diamond_compile.stdout)
        return diamond_compile.returncode
    diamond_program = run([str(diamond_app_exe)])
    if diamond_program.returncode != 0 or diamond_program.stdout != "5 6 7\n":
        print("module_diamond_import: stdout mismatch")
        print(diamond_program.stdout)
        return 1
    print("ok module_diamond_import")

    native_i = TEST_DIR / "native_monomorph.i"
    native_c = TEST_DIR / "native_monomorph.c"
    native_h = TEST_DIR / "native_monomorph.h"
    native_exe = TEST_DIR / "native_monomorph.exe"
    native_i.write_text(r'''
cinclude "stdio.h"
import "C:/devel/i/src/runtime/containers.i"

main:proc()->i32 = {
    arena:memops_arena = {};
    memops_arena_initialize(arena.&);
    values:Array<i32> = Array<i32>reserve(arena.&, 3);
    values.data[0] = 4;
    values.data[1] = 5;
    values.data[2] = 6;
    printf("%llu %d\n", values.length, values.data[0] + values.data[1] + values.data[2]);
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")

    for stale in (TEST_DIR / "Array.h", TEST_DIR / "Array_i32.h"):
        if stale.exists():
            stale.unlink()

    translate = run([str(I_EXE), str(native_i), str(native_c)])
    if translate.returncode != 0:
        print(translate.stdout)
        return translate.returncode

    array_header = TEST_DIR / "Array.h"
    array_i32_header = TEST_DIR / "Array_i32.h"
    if not native_h.exists() or not array_header.exists() or not array_i32_header.exists():
        print("native_monomorph: generated native headers missing")
        return 1
    if '#include "Array_i32.h"' not in array_header.read_text(encoding="utf-8"):
        print("native_monomorph: umbrella header missing Array_i32 include")
        return 1
    array_i32_text = array_i32_header.read_text(encoding="utf-8")
    if "structdef(Array_i32)" not in array_i32_text or "Array_i32_reserve" not in array_i32_text:
        print("native_monomorph: concrete header missing struct or proc prototype")
        return 1

    compile_result = run([
        "clang.exe",
        str(native_c),
        "-I",
        str(TEST_DIR),
        "-I",
        "src",
        "-I",
        "src/runtime",
        "-o",
        str(native_exe),
    ])
    if compile_result.returncode != 0:
        print(compile_result.stdout)
        return compile_result.returncode

    program = run([str(native_exe)])
    if program.returncode != 0:
        print(program.stdout)
        return program.returncode
    if program.stdout != "3 15\n":
        print("native_monomorph: stdout mismatch")
        print(program.stdout)
        return 1

    print("ok native_monomorph")

    missing_i = TEST_DIR / "missing_decl.i"
    missing_c = TEST_DIR / "missing_decl.c"
    missing_i.write_text(r'''
main:proc()->i32 = {
    return missing_symbol;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    missing = run([str(I_EXE), str(missing_i), str(missing_c)])
    if missing.returncode == 0 or "use of undeclared identifier 'missing_symbol'" not in missing.stdout:
        print("missing_decl: expected undeclared identifier diagnostic")
        print(missing.stdout)
        return 1
    print("ok missing_decl")

    cycle_a = TEST_DIR / "cycle_a.i"
    cycle_b = TEST_DIR / "cycle_b.i"
    cycle_c = TEST_DIR / "cycle_a.c"
    cycle_a.write_text('import "cycle_b.i"\n', encoding="utf-8", newline="\n")
    cycle_b.write_text('import "cycle_a.i"\n', encoding="utf-8", newline="\n")
    cycle = run([str(I_EXE), str(cycle_a), str(cycle_c)])
    if cycle.returncode == 0 or "import cycle:" not in cycle.stdout or "cycle_a.i" not in cycle.stdout or "cycle_b.i" not in cycle.stdout:
        print("import_cycle: expected import cycle diagnostic")
        print(cycle.stdout)
        return 1
    print("ok import_cycle")

    parse_error_i = TEST_DIR / "parse_expected_actual.i"
    parse_error_c = TEST_DIR / "parse_expected_actual.c"
    parse_error_i.write_text(r'''
Bad:struct = {
    value i32;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    parse_error = run([str(I_EXE), str(parse_error_i), str(parse_error_c)])
    if (
        parse_error.returncode == 0
        or "expected ':' after field name" not in parse_error.stdout
        or "expected ':'" not in parse_error.stdout
        or "got identifier `i32`" not in parse_error.stdout
    ):
        print("parse_expected_actual: expected rich parser diagnostic")
        print(parse_error.stdout)
        return 1
    print("ok parse_expected_actual")

    parse_expected_expr_i = TEST_DIR / "parse_expected_expression.i"
    parse_expected_expr_c = TEST_DIR / "parse_expected_expression.c"
    parse_expected_expr_i.write_text(r'''
main:proc()->i32 = {
    value:i32 = ;
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    parse_expected_expr = run([str(I_EXE), str(parse_expected_expr_i), str(parse_expected_expr_c)])
    if (
        parse_expected_expr.returncode == 0
        or "parse error: expected expression" not in parse_expected_expr.stdout
        or "got ';' `;`" not in parse_expected_expr.stdout
    ):
        print("parse_expected_expression: expected expression diagnostic with actual token")
        print(parse_expected_expr.stdout)
        return 1
    print("ok parse_expected_expression")

    parse_unexpected_stmt_i = TEST_DIR / "parse_unexpected_statement.i"
    parse_unexpected_stmt_c = TEST_DIR / "parse_unexpected_statement.c"
    parse_unexpected_stmt_i.write_text(r'''
main:proc()->i32 = {
    case 1:
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    parse_unexpected_stmt = run([str(I_EXE), str(parse_unexpected_stmt_i), str(parse_unexpected_stmt_c)])
    if (
        parse_unexpected_stmt.returncode == 0
        or "parse error: unexpected token" not in parse_unexpected_stmt.stdout
        or "got 'case' `case`" not in parse_unexpected_stmt.stdout
    ):
        print("parse_unexpected_statement: expected unexpected statement token diagnostic")
        print(parse_unexpected_stmt.stdout)
        return 1
    print("ok parse_unexpected_statement")

    parse_enum_value_i = TEST_DIR / "parse_enum_value.i"
    parse_enum_value_c = TEST_DIR / "parse_enum_value.c"
    parse_enum_value_i.write_text(r'''
Kind:enum = {
    A = };
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    parse_enum_value = run([str(I_EXE), str(parse_enum_value_i), str(parse_enum_value_c)])
    if (
        parse_enum_value.returncode == 0
        or "parse error: expected enum value" not in parse_enum_value.stdout
        or "got '}' `}`" not in parse_enum_value.stdout
    ):
        print("parse_enum_value: expected enum value diagnostic with actual token")
        print(parse_enum_value.stdout)
        return 1
    print("ok parse_enum_value")

    parse_switch_body_i = TEST_DIR / "parse_switch_body.i"
    parse_switch_body_c = TEST_DIR / "parse_switch_body.c"
    parse_switch_body_i.write_text(r'''
main:proc()->i32 = {
    switch (1) {
        value;
    }
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    parse_switch_body = run([str(I_EXE), str(parse_switch_body_i), str(parse_switch_body_c)])
    if (
        parse_switch_body.returncode == 0
        or "parse error: expected case/default in switch" not in parse_switch_body.stdout
        or "got identifier `value`" not in parse_switch_body.stdout
    ):
        print("parse_switch_body: expected switch token diagnostic")
        print(parse_switch_body.stdout)
        return 1
    print("ok parse_switch_body")

    control_break_i = TEST_DIR / "control_break_outside.i"
    control_break_c = TEST_DIR / "control_break_outside.c"
    control_break_i.write_text(r'''
main:proc()->i32 = {
    break;
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    control_break = run([str(I_EXE), str(control_break_i), str(control_break_c)])
    if (
        control_break.returncode == 0
        or "semantic error: break outside loop or switch" not in control_break.stdout
    ):
        print("control_break_outside: expected break outside loop/switch diagnostic")
        print(control_break.stdout)
        return 1
    print("ok control_break_outside")

    control_continue_i = TEST_DIR / "control_continue_outside.i"
    control_continue_c = TEST_DIR / "control_continue_outside.c"
    control_continue_i.write_text(r'''
main:proc()->i32 = {
    continue;
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    control_continue = run([str(I_EXE), str(control_continue_i), str(control_continue_c)])
    if (
        control_continue.returncode == 0
        or "semantic error: continue outside loop" not in control_continue.stdout
    ):
        print("control_continue_outside: expected continue outside loop diagnostic")
        print(control_continue.stdout)
        return 1
    print("ok control_continue_outside")

    control_switch_break_i = TEST_DIR / "control_switch_break.i"
    control_switch_break_c = TEST_DIR / "control_switch_break.c"
    control_switch_break_i.write_text(r'''
main:proc()->i32 = {
    switch (1) {
        case 1:
            break;
    }
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    control_switch_break = run([str(I_EXE), str(control_switch_break_i), str(control_switch_break_c)])
    if control_switch_break.returncode != 0:
        print("control_switch_break: expected break in switch to type-check")
        print(control_switch_break.stdout)
        return 1
    print("ok control_switch_break")

    control_switch_continue_i = TEST_DIR / "control_switch_continue.i"
    control_switch_continue_c = TEST_DIR / "control_switch_continue.c"
    control_switch_continue_i.write_text(r'''
main:proc()->i32 = {
    switch (1) {
        case 1:
            continue;
    }
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    control_switch_continue = run([str(I_EXE), str(control_switch_continue_i), str(control_switch_continue_c)])
    if (
        control_switch_continue.returncode == 0
        or "semantic error: continue outside loop" not in control_switch_continue.stdout
    ):
        print("control_switch_continue: expected continue in switch-only context diagnostic")
        print(control_switch_continue.stdout)
        return 1
    print("ok control_switch_continue")

    duplicate_local_i = TEST_DIR / "duplicate_local.i"
    duplicate_local_c = TEST_DIR / "duplicate_local.c"
    duplicate_local_i.write_text(r'''
main:proc()->i32 = {
    value:i32 = 1;
    value:i32 = 2;
    return value;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    duplicate_local = run([str(I_EXE), str(duplicate_local_i), str(duplicate_local_c)])
    if (
        duplicate_local.returncode == 0
        or "semantic error: duplicate local declaration 'value'" not in duplicate_local.stdout
        or "previous at 2:5" not in duplicate_local.stdout
    ):
        print("duplicate_local: expected previous local declaration diagnostic")
        print(duplicate_local.stdout)
        return 1
    print("ok duplicate_local")

    duplicate_param_local_i = TEST_DIR / "duplicate_param_local.i"
    duplicate_param_local_c = TEST_DIR / "duplicate_param_local.c"
    duplicate_param_local_i.write_text(r'''
main:proc(value:i32)->i32 = {
    value:i32 = 2;
    return value;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    duplicate_param_local = run([str(I_EXE), str(duplicate_param_local_i), str(duplicate_param_local_c)])
    if (
        duplicate_param_local.returncode == 0
        or "semantic error: duplicate local declaration 'value'" not in duplicate_param_local.stdout
        or "previous at 1:11" not in duplicate_param_local.stdout
    ):
        print("duplicate_param_local: expected previous parameter declaration diagnostic")
        print(duplicate_param_local.stdout)
        return 1
    print("ok duplicate_param_local")

    duplicate_field_i = TEST_DIR / "duplicate_field.i"
    duplicate_field_c = TEST_DIR / "duplicate_field.c"
    duplicate_field_i.write_text(r'''
Payload:struct = {
    value:i32;
    value:f32;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    duplicate_field = run([str(I_EXE), str(duplicate_field_i), str(duplicate_field_c)])
    if (
        duplicate_field.returncode == 0
        or str(duplicate_field_i) not in duplicate_field.stdout
        or "semantic error: duplicate field 'value'" not in duplicate_field.stdout
        or f"previous at {duplicate_field_i}:2:5" not in duplicate_field.stdout
    ):
        print("duplicate_field: expected duplicate struct field diagnostic")
        print(duplicate_field.stdout)
        return 1
    print("ok duplicate_field")

    import_duplicate_field_mod = TEST_DIR / "import_duplicate_field_mod.i"
    import_duplicate_field_app = TEST_DIR / "import_duplicate_field_app.i"
    import_duplicate_field_c = TEST_DIR / "import_duplicate_field_app.c"
    import_duplicate_field_mod.write_text(r'''
Payload:struct = {
    value:i32;
    value:f32;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    import_duplicate_field_app.write_text(r'''
import "import_duplicate_field_mod.i"

main:proc()->i32 = {
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    import_duplicate_field = run([str(I_EXE), str(import_duplicate_field_app), str(import_duplicate_field_c)])
    if (
        import_duplicate_field.returncode == 0
        or str(import_duplicate_field_mod) not in import_duplicate_field.stdout
        or "semantic error: duplicate field 'value'" not in import_duplicate_field.stdout
        or f"previous at {import_duplicate_field_mod}:2:5" not in import_duplicate_field.stdout
    ):
        print("import_duplicate_field: expected imported duplicate field diagnostic path")
        print(import_duplicate_field.stdout)
        return 1
    print("ok import_duplicate_field")

    duplicate_enum_item_i = TEST_DIR / "duplicate_enum_item.i"
    duplicate_enum_item_c = TEST_DIR / "duplicate_enum_item.c"
    duplicate_enum_item_i.write_text(r'''
Mode:enum = {
    A,
    A,
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    duplicate_enum_item = run([str(I_EXE), str(duplicate_enum_item_i), str(duplicate_enum_item_c)])
    if (
        duplicate_enum_item.returncode == 0
        or str(duplicate_enum_item_i) not in duplicate_enum_item.stdout
        or "semantic error: duplicate enum item 'A'" not in duplicate_enum_item.stdout
        or f"previous at {duplicate_enum_item_i}:2:5" not in duplicate_enum_item.stdout
    ):
        print("duplicate_enum_item: expected duplicate enum item diagnostic path")
        print(duplicate_enum_item.stdout)
        return 1
    print("ok duplicate_enum_item")

    undeclared_field_type_i = TEST_DIR / "undeclared_field_type.i"
    undeclared_field_type_c = TEST_DIR / "undeclared_field_type.c"
    undeclared_field_type_i.write_text(r'''
Payload:struct = {
    value:MissingType;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    undeclared_field_type = run([str(I_EXE), str(undeclared_field_type_i), str(undeclared_field_type_c)])
    if (
        undeclared_field_type.returncode == 0
        or str(undeclared_field_type_i) not in undeclared_field_type.stdout
        or "semantic error: use of undeclared type 'MissingType'" not in undeclared_field_type.stdout
    ):
        print("undeclared_field_type: expected unknown field type diagnostic")
        print(undeclared_field_type.stdout)
        return 1
    print("ok undeclared_field_type")

    undeclared_local_type_i = TEST_DIR / "undeclared_local_type.i"
    undeclared_local_type_c = TEST_DIR / "undeclared_local_type.c"
    undeclared_local_type_i.write_text(r'''
main:proc()->i32 = {
    value:MissingType = {};
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    undeclared_local_type = run([str(I_EXE), str(undeclared_local_type_i), str(undeclared_local_type_c)])
    if (
        undeclared_local_type.returncode == 0
        or str(undeclared_local_type_i) not in undeclared_local_type.stdout
        or "semantic error: use of undeclared type 'MissingType'" not in undeclared_local_type.stdout
    ):
        print("undeclared_local_type: expected unknown local type diagnostic")
        print(undeclared_local_type.stdout)
        return 1
    print("ok undeclared_local_type")

    undeclared_generic_arg_i = TEST_DIR / "undeclared_generic_arg.i"
    undeclared_generic_arg_c = TEST_DIR / "undeclared_generic_arg.c"
    undeclared_generic_arg_i.write_text(r'''
Array:struct<T> = {
    data:*T;
}

main:proc()->i32 = {
    arr:Array<MissingType> = {};
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    undeclared_generic_arg = run([str(I_EXE), str(undeclared_generic_arg_i), str(undeclared_generic_arg_c)])
    if (
        undeclared_generic_arg.returncode == 0
        or str(undeclared_generic_arg_i) not in undeclared_generic_arg.stdout
        or "semantic error: use of undeclared type 'MissingType'" not in undeclared_generic_arg.stdout
    ):
        print("undeclared_generic_arg: expected unknown generic argument diagnostic")
        print(undeclared_generic_arg.stdout)
        return 1
    print("ok undeclared_generic_arg")

    generic_type_extra_arg_i = TEST_DIR / "generic_type_extra_arg.i"
    generic_type_extra_arg_c = TEST_DIR / "generic_type_extra_arg.c"
    generic_type_extra_arg_i.write_text(r'''
Array:struct<T> = {
    data:*T;
}

main:proc()->i32 = {
    arr:Array<i32, f32> = {};
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    generic_type_extra_arg = run([str(I_EXE), str(generic_type_extra_arg_i), str(generic_type_extra_arg_c)])
    if (
        generic_type_extra_arg.returncode == 0
        or "semantic error: generic type 'Array' expects 1 type arg, got 2" not in generic_type_extra_arg.stdout
        or f"{generic_type_extra_arg_i}:1:1: note: struct 'Array' declared here" not in generic_type_extra_arg.stdout
    ):
        print("generic_type_extra_arg: expected generic type arity diagnostic")
        print(generic_type_extra_arg.stdout)
        return 1
    print("ok generic_type_extra_arg")

    nongeneric_type_arg_i = TEST_DIR / "nongeneric_type_arg.i"
    nongeneric_type_arg_c = TEST_DIR / "nongeneric_type_arg.c"
    nongeneric_type_arg_i.write_text(r'''
Payload:struct = {
    value:i32;
}

main:proc()->i32 = {
    payload:Payload<i32> = {};
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    nongeneric_type_arg = run([str(I_EXE), str(nongeneric_type_arg_i), str(nongeneric_type_arg_c)])
    if (
        nongeneric_type_arg.returncode == 0
        or "semantic error: type 'Payload' is not generic; got 1 type arg" not in nongeneric_type_arg.stdout
        or f"{nongeneric_type_arg_i}:1:1: note: struct 'Payload' declared here" not in nongeneric_type_arg.stdout
    ):
        print("nongeneric_type_arg: expected non-generic type arg diagnostic")
        print(nongeneric_type_arg.stdout)
        return 1
    print("ok nongeneric_type_arg")

    import_undeclared_type_mod = TEST_DIR / "import_undeclared_type_mod.i"
    import_undeclared_type_app = TEST_DIR / "import_undeclared_type_app.i"
    import_undeclared_type_c = TEST_DIR / "import_undeclared_type_app.c"
    import_undeclared_type_mod.write_text(r'''
Payload:struct = {
    value:MissingType;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    import_undeclared_type_app.write_text(r'''
import "import_undeclared_type_mod.i"

main:proc()->i32 = {
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    import_undeclared_type = run([str(I_EXE), str(import_undeclared_type_app), str(import_undeclared_type_c)])
    if (
        import_undeclared_type.returncode == 0
        or str(import_undeclared_type_mod) not in import_undeclared_type.stdout
        or "semantic error: use of undeclared type 'MissingType'" not in import_undeclared_type.stdout
    ):
        print("import_undeclared_type: expected imported unknown type diagnostic path")
        print(import_undeclared_type.stdout)
        return 1
    print("ok import_undeclared_type")

    generic_proc_missing_type_arg_i = TEST_DIR / "generic_proc_missing_type_arg.i"
    generic_proc_missing_type_arg_c = TEST_DIR / "generic_proc_missing_type_arg.c"
    generic_proc_missing_type_arg_i.write_text(r'''
identity:proc<T>(value:T)->T = {
    return value;
}

main:proc()->i32 = {
    return identity(1);
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    generic_proc_missing_type_arg = run([str(I_EXE), str(generic_proc_missing_type_arg_i), str(generic_proc_missing_type_arg_c)])
    if (
        generic_proc_missing_type_arg.returncode == 0
        or str(generic_proc_missing_type_arg_i) not in generic_proc_missing_type_arg.stdout
        or "type error: generic proc 'identity' expects 1 type arg, got 0" not in generic_proc_missing_type_arg.stdout
        or f"{generic_proc_missing_type_arg_i}:1:1: note: proc 'identity' declared here" not in generic_proc_missing_type_arg.stdout
    ):
        print("generic_proc_missing_type_arg: expected missing generic proc type arg diagnostic")
        print(generic_proc_missing_type_arg.stdout)
        return 1
    print("ok generic_proc_missing_type_arg")

    generic_proc_extra_type_arg_i = TEST_DIR / "generic_proc_extra_type_arg.i"
    generic_proc_extra_type_arg_c = TEST_DIR / "generic_proc_extra_type_arg.c"
    generic_proc_extra_type_arg_i.write_text(r'''
identity:proc<T>(value:T)->T = {
    return value;
}

main:proc()->i32 = {
    return identity<i32, f32>(1);
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    generic_proc_extra_type_arg = run([str(I_EXE), str(generic_proc_extra_type_arg_i), str(generic_proc_extra_type_arg_c)])
    if (
        generic_proc_extra_type_arg.returncode == 0
        or str(generic_proc_extra_type_arg_i) not in generic_proc_extra_type_arg.stdout
        or "type error: generic proc 'identity' expects 1 type arg, got 2" not in generic_proc_extra_type_arg.stdout
        or f"{generic_proc_extra_type_arg_i}:1:1: note: proc 'identity' declared here" not in generic_proc_extra_type_arg.stdout
    ):
        print("generic_proc_extra_type_arg: expected generic proc type arg arity diagnostic")
        print(generic_proc_extra_type_arg.stdout)
        return 1
    print("ok generic_proc_extra_type_arg")

    nongeneric_proc_type_arg_i = TEST_DIR / "nongeneric_proc_type_arg.i"
    nongeneric_proc_type_arg_c = TEST_DIR / "nongeneric_proc_type_arg.c"
    nongeneric_proc_type_arg_i.write_text(r'''
add:proc(a:i32, b:i32)->i32 = {
    return a + b;
}

main:proc()->i32 = {
    return add<i32>(1, 2);
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    nongeneric_proc_type_arg = run([str(I_EXE), str(nongeneric_proc_type_arg_i), str(nongeneric_proc_type_arg_c)])
    if (
        nongeneric_proc_type_arg.returncode == 0
        or str(nongeneric_proc_type_arg_i) not in nongeneric_proc_type_arg.stdout
        or "type error: proc 'add' is not generic; got 1 type arg" not in nongeneric_proc_type_arg.stdout
        or f"{nongeneric_proc_type_arg_i}:1:1: note: proc 'add' declared here" not in nongeneric_proc_type_arg.stdout
    ):
        print("nongeneric_proc_type_arg: expected non-generic proc type arg diagnostic")
        print(nongeneric_proc_type_arg.stdout)
        return 1
    print("ok nongeneric_proc_type_arg")

    import_generic_proc_type_arg_mod = TEST_DIR / "import_generic_proc_type_arg_mod.i"
    import_generic_proc_type_arg_app = TEST_DIR / "import_generic_proc_type_arg_app.i"
    import_generic_proc_type_arg_c = TEST_DIR / "import_generic_proc_type_arg_app.c"
    import_generic_proc_type_arg_mod.write_text(r'''
identity:proc<T>(value:T)->T = {
    return value;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    import_generic_proc_type_arg_app.write_text(r'''
import "import_generic_proc_type_arg_mod.i"

main:proc()->i32 = {
    return identity(1);
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    import_generic_proc_type_arg = run([str(I_EXE), str(import_generic_proc_type_arg_app), str(import_generic_proc_type_arg_c)])
    if (
        import_generic_proc_type_arg.returncode == 0
        or str(import_generic_proc_type_arg_app) not in import_generic_proc_type_arg.stdout
        or str(import_generic_proc_type_arg_mod) not in import_generic_proc_type_arg.stdout
        or "type error: generic proc 'identity' expects 1 type arg, got 0" not in import_generic_proc_type_arg.stdout
        or f"{import_generic_proc_type_arg_mod}:1:1: note: proc 'identity' declared here" not in import_generic_proc_type_arg.stdout
    ):
        print("import_generic_proc_type_arg: expected imported generic proc declaration-site diagnostic")
        print(import_generic_proc_type_arg.stdout)
        return 1
    print("ok import_generic_proc_type_arg")

    type_pointer_i = TEST_DIR / "type_pointer_value.i"
    type_pointer_c = TEST_DIR / "type_pointer_value.c"
    type_pointer_i.write_text(r'''
main:proc()->i32 = {
    x:i32 = 1;
    p:*i32 = x;
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_pointer = run([str(I_EXE), str(type_pointer_i), str(type_pointer_c)])
    if type_pointer.returncode == 0 or "type error: initializer expected 'ptr_i32', got 'i32'" not in type_pointer.stdout:
        print("type_pointer_value: expected pointer/value type diagnostic")
        print(type_pointer.stdout)
        return 1
    print("ok type_pointer_value")

    type_array_elem_i = TEST_DIR / "type_array_element_inference.i"
    type_array_elem_c = TEST_DIR / "type_array_element_inference.c"
    type_array_elem_i.write_text(r'''
main:proc()->i32 = {
    values:[2]i32 = {};
    p:*i32 = values[0];
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_array_elem = run([str(I_EXE), str(type_array_elem_i), str(type_array_elem_c)])
    if type_array_elem.returncode == 0 or "type error: initializer expected 'ptr_i32', got 'i32'" not in type_array_elem.stdout:
        print("type_array_element_inference: expected fixed-array element type diagnostic")
        print(type_array_elem.stdout)
        return 1
    print("ok type_array_element_inference")

    type_index_base_i = TEST_DIR / "type_index_base.i"
    type_index_base_c = TEST_DIR / "type_index_base.c"
    type_index_base_i.write_text(r'''
main:proc()->i32 = {
    value:i32 = 1;
    return value[0];
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_index_base = run([str(I_EXE), str(type_index_base_i), str(type_index_base_c)])
    if type_index_base.returncode == 0 or "type error: cannot index non-array/non-pointer type 'i32'" not in type_index_base.stdout:
        print("type_index_base: expected non-indexable base diagnostic")
        print(type_index_base.stdout)
        return 1
    print("ok type_index_base")

    type_index_value_i = TEST_DIR / "type_index_value.i"
    type_index_value_c = TEST_DIR / "type_index_value.c"
    type_index_value_i.write_text(r'''
main:proc()->i32 = {
    values:[2]i32 = {};
    index:*i32 = values[0].&;
    return values[index];
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_index_value = run([str(I_EXE), str(type_index_value_i), str(type_index_value_c)])
    if type_index_value.returncode == 0 or "type error: index expression must be numeric, got 'ptr_i32'" not in type_index_value.stdout:
        print("type_index_value: expected non-numeric index diagnostic")
        print(type_index_value.stdout)
        return 1
    print("ok type_index_value")

    type_addr_literal_i = TEST_DIR / "type_address_literal.i"
    type_addr_literal_c = TEST_DIR / "type_address_literal.c"
    type_addr_literal_i.write_text(r'''
main:proc()->i32 = {
    p:*i32 = 1.&;
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_addr_literal = run([str(I_EXE), str(type_addr_literal_i), str(type_addr_literal_c)])
    if (
        type_addr_literal.returncode == 0
        or "type error: address target must be a name, field, or indexed element; got number" not in type_addr_literal.stdout
    ):
        print("type_address_literal: expected invalid address target diagnostic")
        print(type_addr_literal.stdout)
        return 1
    print("ok type_address_literal")

    type_addr_binary_i = TEST_DIR / "type_address_binary.i"
    type_addr_binary_c = TEST_DIR / "type_address_binary.c"
    type_addr_binary_i.write_text(r'''
main:proc()->i32 = {
    value:i32 = 1;
    p:*i32 = (value + 1).&;
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_addr_binary = run([str(I_EXE), str(type_addr_binary_i), str(type_addr_binary_c)])
    if (
        type_addr_binary.returncode == 0
        or "type error: address target must be a name, field, or indexed element; got binary expression" not in type_addr_binary.stdout
    ):
        print("type_address_binary: expected invalid address target diagnostic")
        print(type_addr_binary.stdout)
        return 1
    print("ok type_address_binary")

    type_addr_call_i = TEST_DIR / "type_address_call.i"
    type_addr_call_c = TEST_DIR / "type_address_call.c"
    type_addr_call_i.write_text(r'''
get_value:proc()->i32 = {
    return 1;
}

main:proc()->i32 = {
    p:*i32 = get_value().&;
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_addr_call = run([str(I_EXE), str(type_addr_call_i), str(type_addr_call_c)])
    if (
        type_addr_call.returncode == 0
        or "type error: address target must be a name, field, or indexed element; got call" not in type_addr_call.stdout
    ):
        print("type_address_call: expected invalid address target diagnostic")
        print(type_addr_call.stdout)
        return 1
    print("ok type_address_call")

    type_addr_field_i = TEST_DIR / "type_address_field.i"
    type_addr_field_c = TEST_DIR / "type_address_field.c"
    type_addr_field_i.write_text(r'''
Payload:struct = {
    value:i32;
}

main:proc()->i32 = {
    payload:Payload = {};
    p:*i32 = payload.value.&;
    p[0] = 7;
    return payload.value;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_addr_field = run([str(I_EXE), str(type_addr_field_i), str(type_addr_field_c)])
    if type_addr_field.returncode != 0:
        print("type_address_field: expected field address target to type-check")
        print(type_addr_field.stdout)
        return 1
    print("ok type_address_field")

    type_index_enum_i = TEST_DIR / "type_index_enum.i"
    type_index_enum_c = TEST_DIR / "type_index_enum.c"
    type_index_enum_i.write_text(r'''
Slot:enum = {
    Zero,
    One,
}

main:proc()->i32 = {
    values:[2]i32 = {1, 2};
    return values[Slot_One];
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_index_enum = run([str(I_EXE), str(type_index_enum_i), str(type_index_enum_c)])
    if type_index_enum.returncode != 0:
        print("type_index_enum: expected enum index expression to type-check")
        print(type_index_enum.stdout)
        return 1
    print("ok type_index_enum")

    type_enum_int_i = TEST_DIR / "type_enum_int_cast.i"
    type_enum_int_c = TEST_DIR / "type_enum_int_cast.c"
    type_enum_int_i.write_text(r'''
Kind:enum = {
    None,
    Ready,
}

main:proc()->i32 = {
    kind:Kind = cast(1, Kind);
    value:i32 = kind;
    return value;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_enum_int = run([str(I_EXE), str(type_enum_int_i), str(type_enum_int_c)])
    if type_enum_int.returncode != 0:
        print("type_enum_int_cast: expected explicit int-to-enum cast and enum-to-int flow to type-check")
        print(type_enum_int.stdout)
        return 1
    print("ok type_enum_int_cast")

    type_enum_assign_i = TEST_DIR / "type_enum_int_assignment.i"
    type_enum_assign_c = TEST_DIR / "type_enum_int_assignment.c"
    type_enum_assign_i.write_text(r'''
Kind:enum = {
    None,
    Ready,
}

main:proc()->i32 = {
    kind:Kind = 1;
    return kind;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_enum_assign = run([str(I_EXE), str(type_enum_assign_i), str(type_enum_assign_c)])
    if type_enum_assign.returncode == 0 or "type error: initializer expected 'Kind', got 'i32'" not in type_enum_assign.stdout:
        print("type_enum_int_assignment: expected integer-to-enum assignment diagnostic")
        print(type_enum_assign.stdout)
        return 1
    print("ok type_enum_int_assignment")

    type_binary_pointer_i = TEST_DIR / "type_binary_pointer_arithmetic.i"
    type_binary_pointer_c = TEST_DIR / "type_binary_pointer_arithmetic.c"
    type_binary_pointer_i.write_text(r'''
main:proc()->i32 = {
    values:[4]i32 = {};
    p:*i32 = values;
    q:*i32 = p + 2;
    r:*i32 = values + 1;
    delta:long = q - p;
    delta2:long = r - values;
    return cast(delta + delta2, i32);
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_binary_pointer = run([str(I_EXE), str(type_binary_pointer_i), str(type_binary_pointer_c)])
    if type_binary_pointer.returncode != 0:
        print("type_binary_pointer_arithmetic: expected pointer arithmetic to type-check")
        print(type_binary_pointer.stdout)
        return 1
    print("ok type_binary_pointer_arithmetic")

    type_binary_bad_i = TEST_DIR / "type_binary_bad_operands.i"
    type_binary_bad_c = TEST_DIR / "type_binary_bad_operands.c"
    type_binary_bad_i.write_text(r'''
Payload:struct = {
    value:i32;
}

main:proc()->i32 = {
    payload:Payload = {};
    return payload + 1;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_binary_bad = run([str(I_EXE), str(type_binary_bad_i), str(type_binary_bad_c)])
    if (
        type_binary_bad.returncode == 0
        or "type error: operator '+' cannot be applied to 'Payload' and 'i32'" not in type_binary_bad.stdout
    ):
        print("type_binary_bad_operands: expected invalid binary operand diagnostic")
        print(type_binary_bad.stdout)
        return 1
    print("ok type_binary_bad_operands")

    type_modulo_float_i = TEST_DIR / "type_modulo_float.i"
    type_modulo_float_c = TEST_DIR / "type_modulo_float.c"
    type_modulo_float_i.write_text(r'''
main:proc()->i32 = {
    value:f32 = 3.0 % 2.0;
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_modulo_float = run([str(I_EXE), str(type_modulo_float_i), str(type_modulo_float_c)])
    if (
        type_modulo_float.returncode == 0
        or "type error: operator '%' cannot be applied to 'f32' and 'f32'" not in type_modulo_float.stdout
    ):
        print("type_modulo_float: expected float modulo diagnostic")
        print(type_modulo_float.stdout)
        return 1
    print("ok type_modulo_float")

    type_modulo_int_i = TEST_DIR / "type_modulo_int.i"
    type_modulo_int_c = TEST_DIR / "type_modulo_int.c"
    type_modulo_int_i.write_text(r'''
main:proc()->i32 = {
    value:i32 = 7 % 3;
    return value;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_modulo_int = run([str(I_EXE), str(type_modulo_int_i), str(type_modulo_int_c)])
    if type_modulo_int.returncode != 0:
        print("type_modulo_int: expected integer modulo to type-check")
        print(type_modulo_int.stdout)
        return 1
    print("ok type_modulo_int")

    type_compound_bitwise_float_i = TEST_DIR / "type_compound_bitwise_float.i"
    type_compound_bitwise_float_c = TEST_DIR / "type_compound_bitwise_float.c"
    type_compound_bitwise_float_i.write_text(r'''
main:proc()->i32 = {
    value:f32 = 1.0;
    value &= 1.0;
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_compound_bitwise_float = run([str(I_EXE), str(type_compound_bitwise_float_i), str(type_compound_bitwise_float_c)])
    if (
        type_compound_bitwise_float.returncode == 0
        or "type error: assignment expected 'f32', got 'f32'" not in type_compound_bitwise_float.stdout
    ):
        print("type_compound_bitwise_float: expected float bitwise compound assignment diagnostic")
        print(type_compound_bitwise_float.stdout)
        return 1
    print("ok type_compound_bitwise_float")

    type_compound_modulo_int_i = TEST_DIR / "type_compound_modulo_int.i"
    type_compound_modulo_int_c = TEST_DIR / "type_compound_modulo_int.c"
    type_compound_modulo_int_i.write_text(r'''
main:proc()->i32 = {
    value:i32 = 7;
    value %= 3;
    value &= 1;
    return value;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_compound_modulo_int = run([str(I_EXE), str(type_compound_modulo_int_i), str(type_compound_modulo_int_c)])
    if type_compound_modulo_int.returncode != 0:
        print("type_compound_modulo_int: expected integer modulo/bitwise compound assignment to type-check")
        print(type_compound_modulo_int.stdout)
        return 1
    print("ok type_compound_modulo_int")

    type_assign_binary_lhs_i = TEST_DIR / "type_assignment_binary_lhs.i"
    type_assign_binary_lhs_c = TEST_DIR / "type_assignment_binary_lhs.c"
    type_assign_binary_lhs_i.write_text(r'''
main:proc()->i32 = {
    value:i32 = 1;
    value + 1 = 2;
    return value;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_assign_binary_lhs = run([str(I_EXE), str(type_assign_binary_lhs_i), str(type_assign_binary_lhs_c)])
    if (
        type_assign_binary_lhs.returncode == 0
        or "type error: assignment target must be a name, field, or indexed element; got binary expression" not in type_assign_binary_lhs.stdout
    ):
        print("type_assignment_binary_lhs: expected invalid assignment target diagnostic")
        print(type_assign_binary_lhs.stdout)
        return 1
    print("ok type_assignment_binary_lhs")

    type_assign_call_lhs_i = TEST_DIR / "type_assignment_call_lhs.i"
    type_assign_call_lhs_c = TEST_DIR / "type_assignment_call_lhs.c"
    type_assign_call_lhs_i.write_text(r'''
get_value:proc()->i32 = {
    return 1;
}

main:proc()->i32 = {
    get_value() = 2;
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_assign_call_lhs = run([str(I_EXE), str(type_assign_call_lhs_i), str(type_assign_call_lhs_c)])
    if (
        type_assign_call_lhs.returncode == 0
        or "type error: assignment target must be a name, field, or indexed element; got call" not in type_assign_call_lhs.stdout
    ):
        print("type_assignment_call_lhs: expected invalid assignment target diagnostic")
        print(type_assign_call_lhs.stdout)
        return 1
    print("ok type_assignment_call_lhs")

    type_assign_index_lhs_i = TEST_DIR / "type_assignment_index_lhs.i"
    type_assign_index_lhs_c = TEST_DIR / "type_assignment_index_lhs.c"
    type_assign_index_lhs_i.write_text(r'''
main:proc()->i32 = {
    values:[2]i32 = {};
    values[0] = 7;
    return values[0];
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_assign_index_lhs = run([str(I_EXE), str(type_assign_index_lhs_i), str(type_assign_index_lhs_c)])
    if type_assign_index_lhs.returncode != 0:
        print("type_assignment_index_lhs: expected indexed assignment target to type-check")
        print(type_assign_index_lhs.stdout)
        return 1
    print("ok type_assignment_index_lhs")

    type_const_local_i = TEST_DIR / "type_const_local_assignment.i"
    type_const_local_c = TEST_DIR / "type_const_local_assignment.c"
    type_const_local_i.write_text(r'''
main:proc()->i32 = {
    value:const i32 = 1;
    value = 2;
    return value;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_const_local = run([str(I_EXE), str(type_const_local_i), str(type_const_local_c)])
    if (
        type_const_local.returncode == 0
        or "type error: cannot assign to const target of type 'const_i32'" not in type_const_local.stdout
    ):
        print("type_const_local_assignment: expected const local assignment diagnostic")
        print(type_const_local.stdout)
        return 1
    print("ok type_const_local_assignment")

    type_const_pointee_i = TEST_DIR / "type_const_pointee_assignment.i"
    type_const_pointee_c = TEST_DIR / "type_const_pointee_assignment.c"
    type_const_pointee_i.write_text(r'''
main:proc(p:*const i32)->i32 = {
    p[0] = 2;
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_const_pointee = run([str(I_EXE), str(type_const_pointee_i), str(type_const_pointee_c)])
    if (
        type_const_pointee.returncode == 0
        or "type error: cannot assign to const target of type 'const_i32'" not in type_const_pointee.stdout
    ):
        print("type_const_pointee_assignment: expected const pointee assignment diagnostic")
        print(type_const_pointee.stdout)
        return 1
    print("ok type_const_pointee_assignment")

    type_const_field_i = TEST_DIR / "type_const_field_assignment.i"
    type_const_field_c = TEST_DIR / "type_const_field_assignment.c"
    type_const_field_i.write_text(r'''
Payload:struct = {
    value:i32;
}

main:proc()->i32 = {
    payload:const Payload = {};
    payload.value = 2;
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_const_field = run([str(I_EXE), str(type_const_field_i), str(type_const_field_c)])
    if (
        type_const_field.returncode == 0
        or "type error: cannot assign to const target of type 'i32'" not in type_const_field.stdout
    ):
        print("type_const_field_assignment: expected const aggregate field assignment diagnostic")
        print(type_const_field.stdout)
        return 1
    print("ok type_const_field_assignment")

    type_const_pointer_reassign_i = TEST_DIR / "type_const_pointer_reassign.i"
    type_const_pointer_reassign_c = TEST_DIR / "type_const_pointer_reassign.c"
    type_const_pointer_reassign_i.write_text(r'''
main:proc()->i32 = {
    p:*const i32 = null;
    q:*const i32 = null;
    p = q;
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_const_pointer_reassign = run([str(I_EXE), str(type_const_pointer_reassign_i), str(type_const_pointer_reassign_c)])
    if type_const_pointer_reassign.returncode != 0:
        print("type_const_pointer_reassign: expected pointer-to-const variable reassignment to type-check")
        print(type_const_pointer_reassign.stdout)
        return 1
    print("ok type_const_pointer_reassign")

    type_const_pointer_drop_i = TEST_DIR / "type_const_pointer_drop.i"
    type_const_pointer_drop_c = TEST_DIR / "type_const_pointer_drop.c"
    type_const_pointer_drop_i.write_text(r'''
main:proc(p:*const i32)->i32 = {
    q:*i32 = p;
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_const_pointer_drop = run([str(I_EXE), str(type_const_pointer_drop_i), str(type_const_pointer_drop_c)])
    if (
        type_const_pointer_drop.returncode == 0
        or "type error: initializer expected 'ptr_i32', got 'ptr_const_i32'" not in type_const_pointer_drop.stdout
    ):
        print("type_const_pointer_drop: expected pointer-to-const to mutable pointer diagnostic")
        print(type_const_pointer_drop.stdout)
        return 1
    print("ok type_const_pointer_drop")

    type_const_pointer_add_i = TEST_DIR / "type_const_pointer_add.i"
    type_const_pointer_add_c = TEST_DIR / "type_const_pointer_add.c"
    type_const_pointer_add_i.write_text(r'''
main:proc(p:*i32)->i32 = {
    q:*const i32 = p;
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_const_pointer_add = run([str(I_EXE), str(type_const_pointer_add_i), str(type_const_pointer_add_c)])
    if type_const_pointer_add.returncode != 0:
        print("type_const_pointer_add: expected mutable pointer to pointer-to-const to type-check")
        print(type_const_pointer_add.stdout)
        return 1
    print("ok type_const_pointer_add")

    type_const_call_drop_i = TEST_DIR / "type_const_call_drop.i"
    type_const_call_drop_c = TEST_DIR / "type_const_call_drop.c"
    type_const_call_drop_i.write_text(r'''
take_mut:proc(p:*i32)->void = {
}

main:proc(p:*const i32)->i32 = {
    take_mut(p);
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_const_call_drop = run([str(I_EXE), str(type_const_call_drop_i), str(type_const_call_drop_c)])
    if (
        type_const_call_drop.returncode == 0
        or "type error: proc 'take_mut' argument 1 'p' expected 'ptr_i32', got 'ptr_const_i32'" not in type_const_call_drop.stdout
    ):
        print("type_const_call_drop: expected pointer-to-const call argument diagnostic")
        print(type_const_call_drop.stdout)
        return 1
    print("ok type_const_call_drop")

    type_const_array_decay_i = TEST_DIR / "type_const_array_decay.i"
    type_const_array_decay_c = TEST_DIR / "type_const_array_decay.c"
    type_const_array_decay_i.write_text(r'''
take_const:proc(p:*const i32)->void = {
}

main:proc()->i32 = {
    values:[4]i32 = {};
    take_const(values);
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_const_array_decay = run([str(I_EXE), str(type_const_array_decay_i), str(type_const_array_decay_c)])
    if type_const_array_decay.returncode != 0:
        print("type_const_array_decay: expected mutable fixed array to decay to pointer-to-const")
        print(type_const_array_decay.stdout)
        return 1
    print("ok type_const_array_decay")

    type_binary_logic_i = TEST_DIR / "type_binary_logic_inference.i"
    type_binary_logic_c = TEST_DIR / "type_binary_logic_inference.c"
    type_binary_logic_i.write_text(r'''
main:proc(p:*i32, q:*i32)->i32 = {
    ok:b32 = p == null or q != null;
    return ok;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_binary_logic = run([str(I_EXE), str(type_binary_logic_i), str(type_binary_logic_c)])
    if type_binary_logic.returncode != 0:
        print("type_binary_logic_inference: expected comparison/logical expressions to infer b32")
        print(type_binary_logic.stdout)
        return 1
    print("ok type_binary_logic_inference")

    type_ternary_ok_i = TEST_DIR / "type_ternary_ok.i"
    type_ternary_ok_c = TEST_DIR / "type_ternary_ok.c"
    type_ternary_ok_i.write_text(r'''
main:proc(flag:b32)->i32 = {
    value:i32 = flag ? 10 : 20;
    return value;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_ternary_ok = run([str(I_EXE), str(type_ternary_ok_i), str(type_ternary_ok_c)])
    if type_ternary_ok.returncode != 0:
        print("type_ternary_ok: expected compatible ternary to type-check")
        print(type_ternary_ok.stdout)
        return 1
    print("ok type_ternary_ok")

    type_ternary_cond_i = TEST_DIR / "type_ternary_condition.i"
    type_ternary_cond_c = TEST_DIR / "type_ternary_condition.c"
    type_ternary_cond_i.write_text(r'''
Payload:struct = {
    value:i32;
}

main:proc()->i32 = {
    payload:Payload = {};
    return payload ? 1 : 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_ternary_cond = run([str(I_EXE), str(type_ternary_cond_i), str(type_ternary_cond_c)])
    if type_ternary_cond.returncode == 0 or "type error: ternary condition must be scalar/pointer, got 'Payload'" not in type_ternary_cond.stdout:
        print("type_ternary_condition: expected invalid condition diagnostic")
        print(type_ternary_cond.stdout)
        return 1
    print("ok type_ternary_condition")

    type_ternary_arms_i = TEST_DIR / "type_ternary_arms.i"
    type_ternary_arms_c = TEST_DIR / "type_ternary_arms.c"
    type_ternary_arms_i.write_text(r'''
Payload:struct = {
    value:i32;
}

main:proc(flag:b32)->i32 = {
    payload:Payload = {};
    return flag ? payload : 1;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_ternary_arms = run([str(I_EXE), str(type_ternary_arms_i), str(type_ternary_arms_c)])
    if type_ternary_arms.returncode == 0 or "type error: ternary arms cannot mix 'Payload' and 'i32'" not in type_ternary_arms.stdout:
        print("type_ternary_arms: expected incompatible ternary arm diagnostic")
        print(type_ternary_arms.stdout)
        return 1
    print("ok type_ternary_arms")

    type_if_condition_i = TEST_DIR / "type_if_condition.i"
    type_if_condition_c = TEST_DIR / "type_if_condition.c"
    type_if_condition_i.write_text(r'''
Payload:struct = {
    value:i32;
}

main:proc()->i32 = {
    payload:Payload = {};
    if (payload) {
        return 1;
    }
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_if_condition = run([str(I_EXE), str(type_if_condition_i), str(type_if_condition_c)])
    if type_if_condition.returncode == 0 or "type error: if condition must be scalar/pointer, got 'Payload'" not in type_if_condition.stdout:
        print("type_if_condition: expected invalid if condition diagnostic")
        print(type_if_condition.stdout)
        return 1
    print("ok type_if_condition")

    type_while_condition_i = TEST_DIR / "type_while_condition.i"
    type_while_condition_c = TEST_DIR / "type_while_condition.c"
    type_while_condition_i.write_text(r'''
Payload:struct = {
    value:i32;
}

main:proc()->i32 = {
    payload:Payload = {};
    while (payload) {
        return 1;
    }
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_while_condition = run([str(I_EXE), str(type_while_condition_i), str(type_while_condition_c)])
    if type_while_condition.returncode == 0 or "type error: while condition must be scalar/pointer, got 'Payload'" not in type_while_condition.stdout:
        print("type_while_condition: expected invalid while condition diagnostic")
        print(type_while_condition.stdout)
        return 1
    print("ok type_while_condition")

    type_do_condition_i = TEST_DIR / "type_do_condition.i"
    type_do_condition_c = TEST_DIR / "type_do_condition.c"
    type_do_condition_i.write_text(r'''
Payload:struct = {
    value:i32;
}

main:proc()->i32 = {
    payload:Payload = {};
    do {
        payload.value = 1;
    } while (payload);
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_do_condition = run([str(I_EXE), str(type_do_condition_i), str(type_do_condition_c)])
    if type_do_condition.returncode == 0 or "type error: do while condition must be scalar/pointer, got 'Payload'" not in type_do_condition.stdout:
        print("type_do_condition: expected invalid do-while condition diagnostic")
        print(type_do_condition.stdout)
        return 1
    print("ok type_do_condition")

    type_for_condition_i = TEST_DIR / "type_for_condition.i"
    type_for_condition_c = TEST_DIR / "type_for_condition.c"
    type_for_condition_i.write_text(r'''
Payload:struct = {
    value:i32;
}

main:proc()->i32 = {
    payload:Payload = {};
    for (; payload; ) {
        return 1;
    }
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_for_condition = run([str(I_EXE), str(type_for_condition_i), str(type_for_condition_c)])
    if type_for_condition.returncode == 0 or "type error: for condition must be scalar/pointer, got 'Payload'" not in type_for_condition.stdout:
        print("type_for_condition: expected invalid for condition diagnostic")
        print(type_for_condition.stdout)
        return 1
    print("ok type_for_condition")

    type_pointer_condition_i = TEST_DIR / "type_pointer_condition.i"
    type_pointer_condition_c = TEST_DIR / "type_pointer_condition.c"
    type_pointer_condition_i.write_text(r'''
main:proc(p:*i32)->i32 = {
    if (p) {
        return p[0];
    }
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_pointer_condition = run([str(I_EXE), str(type_pointer_condition_i), str(type_pointer_condition_c)])
    if type_pointer_condition.returncode != 0:
        print("type_pointer_condition: expected pointer condition to type-check")
        print(type_pointer_condition.stdout)
        return 1
    print("ok type_pointer_condition")

    type_switch_enum_i = TEST_DIR / "type_switch_enum.i"
    type_switch_enum_c = TEST_DIR / "type_switch_enum.c"
    type_switch_enum_i.write_text(r'''
Kind:enum = {
    None,
    Ready,
}

main:proc(kind:Kind)->i32 = {
    switch (kind) {
        case Kind_None:
            return 0;
        case Kind_Ready:
            return 1;
    }
    return -1;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_switch_enum = run([str(I_EXE), str(type_switch_enum_i), str(type_switch_enum_c)])
    if type_switch_enum.returncode != 0:
        print("type_switch_enum: expected enum switch cases to type-check")
        print(type_switch_enum.stdout)
        return 1
    print("ok type_switch_enum")

    type_switch_case_i = TEST_DIR / "type_switch_case.i"
    type_switch_case_c = TEST_DIR / "type_switch_case.c"
    type_switch_case_i.write_text(r'''
Payload:struct = {
    value:i32;
}

main:proc(value:i32)->i32 = {
    payload:Payload = {};
    switch (value) {
        case payload:
            return 1;
    }
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_switch_case = run([str(I_EXE), str(type_switch_case_i), str(type_switch_case_c)])
    if type_switch_case.returncode == 0 or "type error: switch case expected 'i32', got 'Payload'" not in type_switch_case.stdout:
        print("type_switch_case: expected incompatible switch case diagnostic")
        print(type_switch_case.stdout)
        return 1
    print("ok type_switch_case")

    type_pointer_alias_i = TEST_DIR / "type_pointer_alias_compat.i"
    type_pointer_alias_c = TEST_DIR / "type_pointer_alias_compat.c"
    type_pointer_alias_i.write_text(r'''
MyU32:alias = u32;

take_u32s:proc(values:*u32)->void = { external; }

main:proc()->i32 = {
    values:[4]MyU32 = {};
    take_u32s(values);
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_pointer_alias = run([str(I_EXE), str(type_pointer_alias_i), str(type_pointer_alias_c)])
    if type_pointer_alias.returncode != 0:
        print("type_pointer_alias_compat: expected pointer/array alias compatibility")
        print(type_pointer_alias.stdout)
        return 1
    print("ok type_pointer_alias_compat")

    type_array_ptr_mismatch_i = TEST_DIR / "type_array_pointer_element_mismatch.i"
    type_array_ptr_mismatch_c = TEST_DIR / "type_array_pointer_element_mismatch.c"
    type_array_ptr_mismatch_i.write_text(r'''
take_i32s:proc(values:*i32)->void = {
    return;
}

main:proc()->i32 = {
    values:[4]f32 = {};
    take_i32s(values);
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_array_ptr_mismatch = run([str(I_EXE), str(type_array_ptr_mismatch_i), str(type_array_ptr_mismatch_c)])
    if (
        type_array_ptr_mismatch.returncode == 0
        or "type error: proc 'take_i32s' argument 1 'values' expected 'ptr_i32', got 'array_4_f32'" not in type_array_ptr_mismatch.stdout
        or "note: fixed array can decay to pointer only when element types match; expected element 'i32', got 'f32'" not in type_array_ptr_mismatch.stdout
    ):
        print("type_array_pointer_element_mismatch: expected array-to-pointer element mismatch note")
        print(type_array_ptr_mismatch.stdout)
        return 1
    print("ok type_array_pointer_element_mismatch")

    type_array_ptr_init_mismatch_i = TEST_DIR / "type_array_pointer_initializer_mismatch.i"
    type_array_ptr_init_mismatch_c = TEST_DIR / "type_array_pointer_initializer_mismatch.c"
    type_array_ptr_init_mismatch_i.write_text(r'''
main:proc()->i32 = {
    values:[4]f32 = {};
    ptr:*i32 = values;
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_array_ptr_init_mismatch = run([str(I_EXE), str(type_array_ptr_init_mismatch_i), str(type_array_ptr_init_mismatch_c)])
    if (
        type_array_ptr_init_mismatch.returncode == 0
        or "type error: initializer expected 'ptr_i32', got 'array_4_f32'" not in type_array_ptr_init_mismatch.stdout
        or "note: fixed array can decay to pointer only when element types match; expected element 'i32', got 'f32'" not in type_array_ptr_init_mismatch.stdout
    ):
        print("type_array_pointer_initializer_mismatch: expected initializer array-to-pointer mismatch note")
        print(type_array_ptr_init_mismatch.stdout)
        return 1
    print("ok type_array_pointer_initializer_mismatch")

    type_call_i = TEST_DIR / "type_proc_call.i"
    type_call_c = TEST_DIR / "type_proc_call.c"
    type_call_i.write_text(r'''
take_ptr:proc(p:*i32)->void = {
    return;
}

main:proc()->i32 = {
    x:i32 = 1;
    take_ptr(x);
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_call = run([str(I_EXE), str(type_call_i), str(type_call_c)])
    if (
        type_call.returncode == 0
        or "type error: proc 'take_ptr' argument 1 'p' expected 'ptr_i32', got 'i32'" not in type_call.stdout
        or f"{type_call_i}:1:15: note: parameter 'p' declared here" not in type_call.stdout
        or f"{type_call_i}:1:1: note: proc 'take_ptr' declared here" not in type_call.stdout
    ):
        print("type_proc_call: expected proc argument type diagnostic")
        print(type_call.stdout)
        return 1
    print("ok type_proc_call")

    type_call_count_i = TEST_DIR / "type_proc_call_count.i"
    type_call_count_c = TEST_DIR / "type_proc_call_count.c"
    type_call_count_i.write_text(r'''
add:proc(a:i32, b:i32)->i32 = {
    return a + b;
}

main:proc()->i32 = {
    return add(1);
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_call_count = run([str(I_EXE), str(type_call_count_i), str(type_call_count_c)])
    if (
        type_call_count.returncode == 0
        or "type error: proc 'add' expects 2 args, got 1" not in type_call_count.stdout
        or f"{type_call_count_i}:1:1: note: proc 'add' declared here" not in type_call_count.stdout
    ):
        print("type_proc_call_count: expected proc argument count diagnostic")
        print(type_call_count.stdout)
        return 1
    print("ok type_proc_call_count")

    import_type_call_mod = TEST_DIR / "import_type_proc_call_mod.i"
    import_type_call_app = TEST_DIR / "import_type_proc_call_app.i"
    import_type_call_c = TEST_DIR / "import_type_proc_call_app.c"
    import_type_call_mod.write_text(r'''
take_ptr:proc(p:*i32)->void = {
    return;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    import_type_call_app.write_text(r'''
import "import_type_proc_call_mod.i"

main:proc()->i32 = {
    x:i32 = 1;
    take_ptr(x);
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    import_type_call = run([str(I_EXE), str(import_type_call_app), str(import_type_call_c)])
    if (
        import_type_call.returncode == 0
        or str(import_type_call_app) not in import_type_call.stdout
        or "type error: proc 'take_ptr' argument 1 'p' expected 'ptr_i32', got 'i32'" not in import_type_call.stdout
        or f"{import_type_call_mod}:1:15: note: parameter 'p' declared here" not in import_type_call.stdout
        or f"{import_type_call_mod}:1:1: note: proc 'take_ptr' declared here" not in import_type_call.stdout
    ):
        print("import_type_proc_call: expected imported proc argument diagnostic")
        print(import_type_call.stdout)
        return 1
    print("ok import_type_proc_call")

    type_proc_ptr_i = TEST_DIR / "type_proc_pointer_compat.i"
    type_proc_ptr_c = TEST_DIR / "type_proc_pointer_compat.c"
    type_proc_ptr_i.write_text(r'''
CallbackBase:alias = *proc(x:i32)->i32;
Callback:alias = CallbackBase;

ok_cb:proc(x:i32)->i32 = {
    return x;
}

main:proc()->i32 = {
    cb:Callback = ok_cb;
    return cb(1);
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_proc_ptr = run([str(I_EXE), str(type_proc_ptr_i), str(type_proc_ptr_c)])
    if type_proc_ptr.returncode != 0:
        print("type_proc_pointer_compat: expected layered proc pointer alias to type-check")
        print(type_proc_ptr.stdout)
        return 1
    print("ok type_proc_pointer_compat")

    type_proc_ptr_call_arg_i = TEST_DIR / "type_proc_pointer_call_arg.i"
    type_proc_ptr_call_arg_c = TEST_DIR / "type_proc_pointer_call_arg.c"
    type_proc_ptr_call_arg_i.write_text(r'''
Callback:alias = *proc(x:i32)->i32;

ok_cb:proc(x:i32)->i32 = {
    return x;
}

main:proc()->i32 = {
    value:i32 = 1;
    cb:Callback = ok_cb;
    return cb(value.&);
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_proc_ptr_call_arg = run([str(I_EXE), str(type_proc_ptr_call_arg_i), str(type_proc_ptr_call_arg_c)])
    if (
        type_proc_ptr_call_arg.returncode == 0
        or "type error: proc pointer 'cb' argument 1 expected 'i32', got 'ptr_i32'" not in type_proc_ptr_call_arg.stdout
    ):
        print("type_proc_pointer_call_arg: expected proc pointer call argument diagnostic")
        print(type_proc_ptr_call_arg.stdout)
        return 1
    print("ok type_proc_pointer_call_arg")

    type_proc_ptr_call_count_i = TEST_DIR / "type_proc_pointer_call_count.i"
    type_proc_ptr_call_count_c = TEST_DIR / "type_proc_pointer_call_count.c"
    type_proc_ptr_call_count_i.write_text(r'''
Callback:alias = *proc(a:i32, b:i32)->i32;

add:proc(a:i32, b:i32)->i32 = {
    return a + b;
}

main:proc()->i32 = {
    cb:Callback = add;
    return cb(1);
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_proc_ptr_call_count = run([str(I_EXE), str(type_proc_ptr_call_count_i), str(type_proc_ptr_call_count_c)])
    if (
        type_proc_ptr_call_count.returncode == 0
        or "type error: proc pointer 'cb' expects 2 args, got 1" not in type_proc_ptr_call_count.stdout
    ):
        print("type_proc_pointer_call_count: expected proc pointer call count diagnostic")
        print(type_proc_ptr_call_count.stdout)
        return 1
    print("ok type_proc_pointer_call_count")

    type_proc_ptr_call_return_i = TEST_DIR / "type_proc_pointer_call_return.i"
    type_proc_ptr_call_return_c = TEST_DIR / "type_proc_pointer_call_return.c"
    type_proc_ptr_call_return_i.write_text(r'''
Callback:alias = *proc(x:i32)->i32;

ok_cb:proc(x:i32)->i32 = {
    return x;
}

main:proc()->i32 = {
    cb:Callback = ok_cb;
    ptr:*i32 = cb(1);
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_proc_ptr_call_return = run([str(I_EXE), str(type_proc_ptr_call_return_i), str(type_proc_ptr_call_return_c)])
    if (
        type_proc_ptr_call_return.returncode == 0
        or "type error: initializer expected 'ptr_i32', got 'i32'" not in type_proc_ptr_call_return.stdout
    ):
        print("type_proc_pointer_call_return: expected proc pointer call return inference diagnostic")
        print(type_proc_ptr_call_return.stdout)
        return 1
    print("ok type_proc_pointer_call_return")

    type_call_non_proc_i = TEST_DIR / "type_call_non_proc.i"
    type_call_non_proc_c = TEST_DIR / "type_call_non_proc.c"
    type_call_non_proc_i.write_text(r'''
main:proc()->i32 = {
    value:i32 = 1;
    return value(1);
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_call_non_proc = run([str(I_EXE), str(type_call_non_proc_i), str(type_call_non_proc_c)])
    if (
        type_call_non_proc.returncode == 0
        or "type error: cannot call non-proc symbol 'value' of type 'i32'" not in type_call_non_proc.stdout
    ):
        print("type_call_non_proc: expected non-proc call diagnostic")
        print(type_call_non_proc.stdout)
        return 1
    print("ok type_call_non_proc")

    type_proc_ptr_ret_i = TEST_DIR / "type_proc_pointer_return_mismatch.i"
    type_proc_ptr_ret_c = TEST_DIR / "type_proc_pointer_return_mismatch.c"
    type_proc_ptr_ret_i.write_text(r'''
Callback:alias = *proc(x:i32)->i32;

bad_cb:proc(x:i32)->*i32 = {
    return null;
}

main:proc()->i32 = {
    cb:Callback = bad_cb;
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_proc_ptr_ret = run([str(I_EXE), str(type_proc_ptr_ret_i), str(type_proc_ptr_ret_c)])
    if type_proc_ptr_ret.returncode == 0 or "type error: initializer expected 'Callback', got 'ptr_proc_ptr_i32_i32'" not in type_proc_ptr_ret.stdout:
        print("type_proc_pointer_return_mismatch: expected proc pointer return type diagnostic")
        print(type_proc_ptr_ret.stdout)
        return 1
    print("ok type_proc_pointer_return_mismatch")

    type_proc_ptr_arg_i = TEST_DIR / "type_proc_pointer_arg_mismatch.i"
    type_proc_ptr_arg_c = TEST_DIR / "type_proc_pointer_arg_mismatch.c"
    type_proc_ptr_arg_i.write_text(r'''
Callback:alias = *proc(x:i32)->i32;

bad_cb:proc(x:*i32)->i32 = {
    return 0;
}

main:proc()->i32 = {
    cb:Callback = bad_cb;
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_proc_ptr_arg = run([str(I_EXE), str(type_proc_ptr_arg_i), str(type_proc_ptr_arg_c)])
    if type_proc_ptr_arg.returncode == 0 or "type error: initializer expected 'Callback', got 'ptr_proc_i32_ptr_i32'" not in type_proc_ptr_arg.stdout:
        print("type_proc_pointer_arg_mismatch: expected proc pointer argument type diagnostic")
        print(type_proc_ptr_arg.stdout)
        return 1
    print("ok type_proc_pointer_arg_mismatch")

    type_return_i = TEST_DIR / "type_return.i"
    type_return_c = TEST_DIR / "type_return.c"
    type_return_i.write_text(r'''
main:proc()->*i32 = {
    return 1;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_return = run([str(I_EXE), str(type_return_i), str(type_return_c)])
    if type_return.returncode == 0 or "type error: return expected 'ptr_i32', got 'i32'" not in type_return.stdout:
        print("type_return: expected return type diagnostic")
        print(type_return.stdout)
        return 1
    print("ok type_return")

    type_return_missing_i = TEST_DIR / "type_return_missing_value.i"
    type_return_missing_c = TEST_DIR / "type_return_missing_value.c"
    type_return_missing_i.write_text(r'''
main:proc()->i32 = {
    return;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_return_missing = run([str(I_EXE), str(type_return_missing_i), str(type_return_missing_c)])
    if (
        type_return_missing.returncode == 0
        or "type error: non-void proc must return a value of type 'i32'" not in type_return_missing.stdout
    ):
        print("type_return_missing_value: expected non-void bare return diagnostic")
        print(type_return_missing.stdout)
        return 1
    print("ok type_return_missing_value")

    type_return_void_value_i = TEST_DIR / "type_return_void_value.i"
    type_return_void_value_c = TEST_DIR / "type_return_void_value.c"
    type_return_void_value_i.write_text(r'''
main:proc()->void = {
    return 1;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_return_void_value = run([str(I_EXE), str(type_return_void_value_i), str(type_return_void_value_c)])
    if (
        type_return_void_value.returncode == 0
        or "type error: void proc should not return a value" not in type_return_void_value.stdout
    ):
        print("type_return_void_value: expected void return value diagnostic")
        print(type_return_void_value.stdout)
        return 1
    print("ok type_return_void_value")

    type_return_void_bare_i = TEST_DIR / "type_return_void_bare.i"
    type_return_void_bare_c = TEST_DIR / "type_return_void_bare.c"
    type_return_void_bare_i.write_text(r'''
main:proc()->void = {
    return;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_return_void_bare = run([str(I_EXE), str(type_return_void_bare_i), str(type_return_void_bare_c)])
    if type_return_void_bare.returncode != 0:
        print("type_return_void_bare: expected bare return in void proc to type-check")
        print(type_return_void_bare.stdout)
        return 1
    print("ok type_return_void_bare")

    type_field_i = TEST_DIR / "type_field_access.i"
    type_field_c = TEST_DIR / "type_field_access.c"
    type_field_i.write_text(r'''
Payload:struct = {
    value:i32;
}

main:proc()->i32 = {
    payload:Payload = {};
    return payload.missing;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_field = run([str(I_EXE), str(type_field_i), str(type_field_c)])
    if type_field.returncode == 0 or "type error: type 'Payload' has no field 'missing'" not in type_field.stdout:
        print("type_field_access: expected missing field type diagnostic")
        print(type_field.stdout)
        return 1
    print("ok type_field_access")

    type_field_ptr_i = TEST_DIR / "type_field_pointer_access.i"
    type_field_ptr_c = TEST_DIR / "type_field_pointer_access.c"
    type_field_ptr_i.write_text(r'''
Payload:struct = {
    value:i32;
}

main:proc(p:*Payload)->i32 = {
    return p.value;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_field_ptr = run([str(I_EXE), str(type_field_ptr_i), str(type_field_ptr_c)])
    if (
        type_field_ptr.returncode == 0
        or "type error: field 'value' cannot be accessed on pointer type 'ptr_Payload'; use value[0].value" not in type_field_ptr.stdout
    ):
        print("type_field_pointer_access: expected pointer field access type diagnostic")
        print(type_field_ptr.stdout)
        return 1
    print("ok type_field_pointer_access")

    type_external_field_i = TEST_DIR / "type_external_field_access.i"
    type_external_field_c = TEST_DIR / "type_external_field_access.c"
    type_external_field_i.write_text(r'''
CMeta:struct = { external; }

main:proc(meta:*const CMeta)->i32 = {
    return meta[0].field_count;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_external_field = run([str(I_EXE), str(type_external_field_i), str(type_external_field_c)])
    if type_external_field.returncode != 0:
        print("type_external_field_access: expected empty external struct field access to remain C-tolerant")
        print(type_external_field.stdout)
        return 1
    print("ok type_external_field_access")

    type_init_field_i = TEST_DIR / "type_initializer_field.i"
    type_init_field_c = TEST_DIR / "type_initializer_field.c"
    type_init_field_i.write_text(r'''
Payload:struct = {
    value:i32;
}

main:proc()->i32 = {
    payload:Payload = {.missing = 1};
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_init_field = run([str(I_EXE), str(type_init_field_i), str(type_init_field_c)])
    if type_init_field.returncode == 0 or "type error: initializer for type 'Payload' has no field 'missing'" not in type_init_field.stdout:
        print("type_initializer_field: expected unknown initializer field diagnostic")
        print(type_init_field.stdout)
        return 1
    print("ok type_initializer_field")

    type_init_value_i = TEST_DIR / "type_initializer_value.i"
    type_init_value_c = TEST_DIR / "type_initializer_value.c"
    type_init_value_i.write_text(r'''
Payload:struct = {
    ptr:*i32;
}

main:proc()->i32 = {
    payload:Payload = {.ptr = 1};
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_init_value = run([str(I_EXE), str(type_init_value_i), str(type_init_value_c)])
    if type_init_value.returncode == 0 or "type error: field initializer expected 'ptr_i32', got 'i32'" not in type_init_value.stdout:
        print("type_initializer_value: expected initializer value type diagnostic")
        print(type_init_value.stdout)
        return 1
    print("ok type_initializer_value")

    type_init_duplicate_i = TEST_DIR / "type_initializer_duplicate_field.i"
    type_init_duplicate_c = TEST_DIR / "type_initializer_duplicate_field.c"
    type_init_duplicate_i.write_text(r'''
Payload:struct = {
    value:i32;
}

main:proc()->i32 = {
    payload:Payload = {.value = 1, .value = 2};
    return payload.value;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_init_duplicate = run([str(I_EXE), str(type_init_duplicate_i), str(type_init_duplicate_c)])
    if (
        type_init_duplicate.returncode == 0
        or "type error: duplicate initializer for field 'value'" not in type_init_duplicate.stdout
    ):
        print("type_initializer_duplicate_field: expected duplicate initializer field diagnostic")
        print(type_init_duplicate.stdout)
        return 1
    print("ok type_initializer_duplicate_field")

    type_init_duplicate_pos_i = TEST_DIR / "type_initializer_duplicate_positional_field.i"
    type_init_duplicate_pos_c = TEST_DIR / "type_initializer_duplicate_positional_field.c"
    type_init_duplicate_pos_i.write_text(r'''
Payload:struct = {
    value:i32;
    other:i32;
}

main:proc()->i32 = {
    payload:Payload = {1, .value = 2};
    return payload.value;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_init_duplicate_pos = run([str(I_EXE), str(type_init_duplicate_pos_i), str(type_init_duplicate_pos_c)])
    if (
        type_init_duplicate_pos.returncode == 0
        or "type error: duplicate initializer for field 'value'" not in type_init_duplicate_pos.stdout
    ):
        print("type_initializer_duplicate_positional_field: expected duplicate positional/designated initializer diagnostic")
        print(type_init_duplicate_pos.stdout)
        return 1
    print("ok type_initializer_duplicate_positional_field")

    type_init_count_i = TEST_DIR / "type_initializer_count.i"
    type_init_count_c = TEST_DIR / "type_initializer_count.c"
    type_init_count_i.write_text(r'''
Payload:struct = {
    a:i32;
}

main:proc()->i32 = {
    payload:Payload = {1, 2};
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_init_count = run([str(I_EXE), str(type_init_count_i), str(type_init_count_c)])
    if type_init_count.returncode == 0 or "type error: too many positional initializer values for type 'Payload'" not in type_init_count.stdout:
        print("type_initializer_count: expected positional initializer count diagnostic")
        print(type_init_count.stdout)
        return 1
    print("ok type_initializer_count")

    type_array_init_count_i = TEST_DIR / "type_array_initializer_count.i"
    type_array_init_count_c = TEST_DIR / "type_array_initializer_count.c"
    type_array_init_count_i.write_text(r'''
main:proc()->i32 = {
    values:[2]i32 = {1, 2, 3};
    return values[0];
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_array_init_count = run([str(I_EXE), str(type_array_init_count_i), str(type_array_init_count_c)])
    if (
        type_array_init_count.returncode == 0
        or "type error: too many positional initializer values for type 'array_2_i32'" not in type_array_init_count.stdout
    ):
        print("type_array_initializer_count: expected fixed-array initializer count diagnostic")
        print(type_array_init_count.stdout)
        return 1
    print("ok type_array_initializer_count")

    type_array_init_dup_i = TEST_DIR / "type_array_initializer_duplicate_index.i"
    type_array_init_dup_c = TEST_DIR / "type_array_initializer_duplicate_index.c"
    type_array_init_dup_i.write_text(r'''
main:proc()->i32 = {
    values:[2]i32 = {[0] = 1, [0] = 2};
    return values[0];
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_array_init_dup = run([str(I_EXE), str(type_array_init_dup_i), str(type_array_init_dup_c)])
    if (
        type_array_init_dup.returncode == 0
        or "type error: duplicate initializer for array index '0'" not in type_array_init_dup.stdout
    ):
        print("type_array_initializer_duplicate_index: expected duplicate array index diagnostic")
        print(type_array_init_dup.stdout)
        return 1
    print("ok type_array_initializer_duplicate_index")

    type_array_init_dup_pos_i = TEST_DIR / "type_array_initializer_duplicate_positional_index.i"
    type_array_init_dup_pos_c = TEST_DIR / "type_array_initializer_duplicate_positional_index.c"
    type_array_init_dup_pos_i.write_text(r'''
main:proc()->i32 = {
    values:[2]i32 = {1, [0] = 2};
    return values[0];
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_array_init_dup_pos = run([str(I_EXE), str(type_array_init_dup_pos_i), str(type_array_init_dup_pos_c)])
    if (
        type_array_init_dup_pos.returncode == 0
        or "type error: duplicate initializer for array index '0'" not in type_array_init_dup_pos.stdout
    ):
        print("type_array_initializer_duplicate_positional_index: expected duplicate positional/designated array index diagnostic")
        print(type_array_init_dup_pos.stdout)
        return 1
    print("ok type_array_initializer_duplicate_positional_index")

    type_array_init_bounds_i = TEST_DIR / "type_array_initializer_index_bounds.i"
    type_array_init_bounds_c = TEST_DIR / "type_array_initializer_index_bounds.c"
    type_array_init_bounds_i.write_text(r'''
main:proc()->i32 = {
    values:[2]i32 = {[2] = 1};
    return values[0];
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_array_init_bounds = run([str(I_EXE), str(type_array_init_bounds_i), str(type_array_init_bounds_c)])
    if (
        type_array_init_bounds.returncode == 0
        or "type error: initializer index '2' is out of bounds for type 'array_2_i32'" not in type_array_init_bounds.stdout
    ):
        print("type_array_initializer_index_bounds: expected fixed-array designator bounds diagnostic")
        print(type_array_init_bounds.stdout)
        return 1
    print("ok type_array_initializer_index_bounds")

    type_array_init_float_index_i = TEST_DIR / "type_array_initializer_float_index.i"
    type_array_init_float_index_c = TEST_DIR / "type_array_initializer_float_index.c"
    type_array_init_float_index_i.write_text(r'''
main:proc()->i32 = {
    values:[2]i32 = {[1.5] = 1};
    return values[0];
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_array_init_float_index = run([str(I_EXE), str(type_array_init_float_index_i), str(type_array_init_float_index_c)])
    if (
        type_array_init_float_index.returncode == 0
        or "type error: initializer index '1.5' must be a non-negative integer literal" not in type_array_init_float_index.stdout
    ):
        print("type_array_initializer_float_index: expected non-integer array initializer index diagnostic")
        print(type_array_init_float_index.stdout)
        return 1
    print("ok type_array_initializer_float_index")

    interop_i = TEST_DIR / "interop_type_compat.i"
    interop_c = TEST_DIR / "interop_type_compat.c"
    interop_i.write_text(r'''
take_module:proc(m:HMODULE)->void = { external; }
take_levels:proc(levels:*const D3D_FEATURE_LEVEL)->void = { external; }
take_float:proc(v:FLOAT)->void = { external; }
take_u8:proc(v:UINT8)->void = { external; }
take_vec:proc(v:vec2)->void = { external; }
take_vec_ptr:proc(v:*vec2)->void = { external; }

Kind:enum = {
    None,
    Ready,
}

is_ready:proc()->b32 = {
    return Kind_Ready;
}

main:proc()->i32 = {
    module:HMODULE = null;
    hr:HRESULT = 0;
    result:ma_result = 0;
    levels:[4]D3D_FEATURE_LEVEL = {};
    v:[2]f32 = {};
    take_module(module);
    take_module(null);
    take_levels(levels);
    take_float(0);
    take_u8(1);
    take_vec(v);
    take_vec_ptr(v.&);
    return hr + result + is_ready();
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    interop = run([str(I_EXE), str(interop_i), str(interop_c)])
    if interop.returncode != 0:
        print("interop_type_compat: expected translation to accept C interop scalar/array compatibility")
        print(interop.stdout)
        return 1
    print("ok interop_type_compat")

    import_diag_mod = TEST_DIR / "import_type_bad.i"
    import_diag_app = TEST_DIR / "import_type_app.i"
    import_diag_c = TEST_DIR / "import_type_app.c"
    import_diag_mod.write_text(r'''
bad_import_proc:proc()->*i32 = {
    return 1;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    import_diag_app.write_text(r'''
import "import_type_bad.i"

main:proc()->i32 = {
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    import_diag = run([str(I_EXE), str(import_diag_app), str(import_diag_c)])
    if (
        import_diag.returncode == 0
        or str(import_diag_mod) not in import_diag.stdout
        or "type error: return expected 'ptr_i32', got 'i32'" not in import_diag.stdout
    ):
        print("import_type_diagnostic: expected imported module path in type diagnostic")
        print(import_diag.stdout)
        return 1
    print("ok import_type_diagnostic")

    import_chain_bad = TEST_DIR / "import_chain_bad.i"
    import_chain_mid = TEST_DIR / "import_chain_mid.i"
    import_chain_app = TEST_DIR / "import_chain_app.i"
    import_chain_c = TEST_DIR / "import_chain_app.c"
    import_chain_bad.write_text(r'''
bad_import_chain_proc:proc()->*i32 = {
    return 1;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    import_chain_mid.write_text(r'''
import "import_chain_bad.i"
'''.strip() + "\n", encoding="utf-8", newline="\n")
    import_chain_app.write_text(r'''
import "import_chain_mid.i"

main:proc()->i32 = {
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    import_chain = run([str(I_EXE), str(import_chain_app), str(import_chain_c)])
    if (
        import_chain.returncode == 0
        or str(import_chain_bad) not in import_chain.stdout
        or "type error: return expected 'ptr_i32', got 'i32'" not in import_chain.stdout
        or "note: imported through:" not in import_chain.stdout
        or str(import_chain_app) not in import_chain.stdout
        or "import_chain_mid.i" not in import_chain.stdout
        or "import_chain_bad.i" not in import_chain.stdout
    ):
        print("import_chain_diagnostic: expected nested import chain note")
        print(import_chain.stdout)
        return 1
    print("ok import_chain_diagnostic")

    import_semantic_bad = TEST_DIR / "import_semantic_bad.i"
    import_semantic_mid = TEST_DIR / "import_semantic_mid.i"
    import_semantic_app = TEST_DIR / "import_semantic_app.i"
    import_semantic_c = TEST_DIR / "import_semantic_app.c"
    import_semantic_bad.write_text(r'''
bad_semantic_proc:proc()->i32 = {
    return missing_value;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    import_semantic_mid.write_text(r'''
import "import_semantic_bad.i"
'''.strip() + "\n", encoding="utf-8", newline="\n")
    import_semantic_app.write_text(r'''
import "import_semantic_mid.i"

main:proc()->i32 = {
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    import_semantic = run([str(I_EXE), str(import_semantic_app), str(import_semantic_c)])
    if (
        import_semantic.returncode == 0
        or str(import_semantic_bad) not in import_semantic.stdout
        or "semantic error: use of undeclared identifier 'missing_value'" not in import_semantic.stdout
        or "note: imported through:" not in import_semantic.stdout
        or str(import_semantic_app) not in import_semantic.stdout
        or "import_semantic_mid.i" not in import_semantic.stdout
        or "import_semantic_bad.i" not in import_semantic.stdout
    ):
        print("import_semantic_chain_diagnostic: expected nested import chain note")
        print(import_semantic.stdout)
        return 1
    print("ok import_semantic_chain_diagnostic")

    import_semantic_global_bad = TEST_DIR / "import_semantic_global_bad.i"
    import_semantic_global_app = TEST_DIR / "import_semantic_global_app.i"
    import_semantic_global_c = TEST_DIR / "import_semantic_global_app.c"
    import_semantic_global_bad.write_text(r'''
bad_global:i32 = missing_global;
'''.strip() + "\n", encoding="utf-8", newline="\n")
    import_semantic_global_app.write_text(r'''
import "import_semantic_global_bad.i"

main:proc()->i32 = {
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    import_semantic_global = run([str(I_EXE), str(import_semantic_global_app), str(import_semantic_global_c)])
    if (
        import_semantic_global.returncode == 0
        or str(import_semantic_global_bad) not in import_semantic_global.stdout
        or "semantic error: use of undeclared identifier 'missing_global'" not in import_semantic_global.stdout
        or "note: imported through:" not in import_semantic_global.stdout
        or str(import_semantic_global_app) not in import_semantic_global.stdout
        or "import_semantic_global_bad.i" not in import_semantic_global.stdout
    ):
        print("import_semantic_global_diagnostic: expected imported global semantic diagnostic")
        print(import_semantic_global.stdout)
        return 1
    print("ok import_semantic_global_diagnostic")

    import_semantic_generic_bad = TEST_DIR / "import_semantic_generic_bad.i"
    import_semantic_generic_mid = TEST_DIR / "import_semantic_generic_mid.i"
    import_semantic_generic_app = TEST_DIR / "import_semantic_generic_app.i"
    import_semantic_generic_c = TEST_DIR / "import_semantic_generic_app.c"
    import_semantic_generic_bad.write_text(r'''
Array:struct<T> = {
    data:*T;
}

Bad:struct = {
    arr:Array<i32, f32>;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    import_semantic_generic_mid.write_text(r'''
import "import_semantic_generic_bad.i"
'''.strip() + "\n", encoding="utf-8", newline="\n")
    import_semantic_generic_app.write_text(r'''
import "import_semantic_generic_mid.i"

main:proc()->i32 = {
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    import_semantic_generic = run([str(I_EXE), str(import_semantic_generic_app), str(import_semantic_generic_c)])
    if (
        import_semantic_generic.returncode == 0
        or str(import_semantic_generic_bad) not in import_semantic_generic.stdout
        or "semantic error: generic type 'Array' expects 1 type arg, got 2" not in import_semantic_generic.stdout
        or "note: imported through:" not in import_semantic_generic.stdout
        or str(import_semantic_generic_app) not in import_semantic_generic.stdout
        or "import_semantic_generic_mid.i" not in import_semantic_generic.stdout
        or "import_semantic_generic_bad.i" not in import_semantic_generic.stdout
        or "note: struct 'Array' declared here" not in import_semantic_generic.stdout
    ):
        print("import_semantic_generic_diagnostic: expected imported generic type diagnostic")
        print(import_semantic_generic.stdout)
        return 1
    print("ok import_semantic_generic_diagnostic")

    import_semantic_nongeneric_bad = TEST_DIR / "import_semantic_nongeneric_bad.i"
    import_semantic_nongeneric_app = TEST_DIR / "import_semantic_nongeneric_app.i"
    import_semantic_nongeneric_c = TEST_DIR / "import_semantic_nongeneric_app.c"
    import_semantic_nongeneric_bad.write_text(r'''
Payload:struct = {
    value:i32;
}

Bad:struct = {
    payload:Payload<i32>;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    import_semantic_nongeneric_app.write_text(r'''
import "import_semantic_nongeneric_bad.i"

main:proc()->i32 = {
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    import_semantic_nongeneric = run([str(I_EXE), str(import_semantic_nongeneric_app), str(import_semantic_nongeneric_c)])
    if (
        import_semantic_nongeneric.returncode == 0
        or str(import_semantic_nongeneric_bad) not in import_semantic_nongeneric.stdout
        or "semantic error: type 'Payload' is not generic; got 1 type arg" not in import_semantic_nongeneric.stdout
        or "note: imported through:" not in import_semantic_nongeneric.stdout
        or str(import_semantic_nongeneric_app) not in import_semantic_nongeneric.stdout
        or "import_semantic_nongeneric_bad.i" not in import_semantic_nongeneric.stdout
        or "note: struct 'Payload' declared here" not in import_semantic_nongeneric.stdout
    ):
        print("import_semantic_nongeneric_diagnostic: expected imported non-generic type diagnostic")
        print(import_semantic_nongeneric.stdout)
        return 1
    print("ok import_semantic_nongeneric_diagnostic")

    import_dup_mod = TEST_DIR / "import_duplicate_mod.i"
    import_dup_app = TEST_DIR / "import_duplicate_app.i"
    import_dup_c = TEST_DIR / "import_duplicate_app.c"
    import_dup_mod.write_text(r'''
Payload:struct = {
    value:i32;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    import_dup_app.write_text(r'''
import "import_duplicate_mod.i"

Payload:struct = {
    other:i32;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    import_dup = run([str(I_EXE), str(import_dup_app), str(import_dup_c)])
    if (
        import_dup.returncode == 0
        or str(import_dup_app) not in import_dup.stdout
        or str(import_dup_mod) not in import_dup.stdout
        or "duplicate struct declaration 'Payload'" not in import_dup.stdout
        or "previous at" not in import_dup.stdout
    ):
        print("import_duplicate_diagnostic: expected duplicate import source paths")
        print(import_dup.stdout)
        return 1
    print("ok import_duplicate_diagnostic")

    import_value_dup_mod = TEST_DIR / "import_value_duplicate_mod.i"
    import_value_dup_app = TEST_DIR / "import_value_duplicate_app.i"
    import_value_dup_c = TEST_DIR / "import_value_duplicate_app.c"
    import_value_dup_mod.write_text(r'''
shared_value:proc()->i32 = {
    return 1;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    import_value_dup_app.write_text(r'''
import "import_value_duplicate_mod.i"

shared_value:i32 = 2;
'''.strip() + "\n", encoding="utf-8", newline="\n")
    import_value_dup = run([str(I_EXE), str(import_value_dup_app), str(import_value_dup_c)])
    if (
        import_value_dup.returncode == 0
        or str(import_value_dup_app) not in import_value_dup.stdout
        or str(import_value_dup_mod) not in import_value_dup.stdout
        or "duplicate global declaration 'shared_value'" not in import_value_dup.stdout
        or "previous at" not in import_value_dup.stdout
    ):
        print("import_value_duplicate_diagnostic: expected proc/global C namespace collision diagnostic")
        print(import_value_dup.stdout)
        return 1
    print("ok import_value_duplicate_diagnostic")

    generic_constraint_i = TEST_DIR / "generic_constraint_site.i"
    generic_constraint_c = TEST_DIR / "generic_constraint_site.c"
    generic_constraint_i.write_text(r'''
Payload:struct = {
    value:i32;
}

need_hash:proc<T:hashable>(value:T)->u64 = {
    return hash<T>(value);
}

main:proc()->i32 = {
    payload:Payload = {};
    return cast(need_hash<Payload>(payload), i32);
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    generic_constraint = run([str(I_EXE), str(generic_constraint_i), str(generic_constraint_c)])
    if (
        generic_constraint.returncode == 0
        or str(generic_constraint_i) not in generic_constraint.stdout
        or "requirement error: proc 'need_hash' requires 'hashable' for type 'Payload'" not in generic_constraint.stdout
        or "missing function 'hash_Payload'" not in generic_constraint.stdout
        or "generic declared at" not in generic_constraint.stdout
    ):
        print("generic_constraint_site: expected instantiation-site requirement diagnostic")
        print(generic_constraint.stdout)
        return 1
    print("ok generic_constraint_site")

    ibind_exe = BUILD / "ibind.exe"
    if not ibind_exe.exists():
        print("ok ibind_bindgen: skipped, ibind not built")
    else:
        ibind_header = TEST_DIR / "ibind_bindgen.h"
        ibind_out = TEST_DIR / "ibind_bindgen.i"
        ibind_header.write_text(r'''
#define IB_CONST 42
#define IB_NAME "hello"
#define IB_ADD(x, y) ((x) + (y))

static const int IB_STATIC_CONST = 99;
static const unsigned IB_STATIC_HEX = 0x10u;
static const double IB_STATIC_DOUBLE = 3.5;
static const int NOT_IB_STATIC_SKIPPED = 101;

typedef int (*IB_Callback)(int x, const char *label);
typedef int (__stdcall *IB_StdCallback)(int value);

enum {
    IB_ANON_READY = 7,
    NOT_IB_ANON_SKIPPED = 9,
};

typedef struct IB_Payload {
    int value;
    IB_Callback cb;
} IB_Payload;

typedef struct IB_Anon {
    union {
        int x;
        float y;
    };
    struct {
        int a;
        int b;
    } named;
} IB_Anon;

typedef struct __attribute__((packed)) IB_Packed {
    char tag;
    int value;
} IB_Packed;

int IB_do(IB_Callback cb, IB_Payload *payload);
int __stdcall IB_call(IB_StdCallback cb, int value);
'''.strip() + "\n", encoding="utf-8", newline="\n")

        ibind = run([str(ibind_exe), str(ibind_header), str(ibind_out), "--prefix", "IB_", "--", "-target", "i686-pc-windows-msvc"])
        if ibind.returncode != 0:
            print(ibind.stdout)
            return ibind.returncode
        ibind_text = ibind_out.read_text(encoding="utf-8")
        for needle in (
            '#define IB_CONST 42',
            '#define IB_NAME "hello"',
            '#define IB_STATIC_CONST 99',
            '#define IB_STATIC_HEX 16',
            '#define IB_STATIC_DOUBLE 3.5',
            '#define IB_ANON_READY 7',
            "IB_Callback: alias = *proc(x:i32, label:*const char)->i32;",
            "IB_StdCallback: alias = *proc[__stdcall](value:i32)->i32;",
            "IB_Payload: struct = {",
            "    value:i32;",
            "    cb:IB_Callback;",
            "IB_Anon_anon0: union = {",
            "    x:i32;",
            "    y:f32;",
            "IB_Anon_anon1: struct = {",
            "    a:i32;",
            "    b:i32;",
            "IB_Anon: struct = {",
            "    _anon0:IB_Anon_anon0;",
            "    named:IB_Anon_anon1;",
            "// ibind: packed",
            "IB_Packed: struct = {",
            "    tag:char;",
            "    value:i32;",
            "IB_do: proc(cb: IB_Callback, payload: *IB_Payload)->i32 = { external_emit; }",
            "IB_call: proc[__stdcall](cb: IB_StdCallback, value: i32)->i32 = { external_emit; }",
        ):
            if needle not in ibind_text:
                print(f"ibind_bindgen: generated binding missing {needle!r}")
                print(ibind_text)
                return 1
        if "IB_ADD" in ibind_text:
            print("ibind_bindgen: function-like macro should not be emitted")
            print(ibind_text)
            return 1
        if "NOT_IB_ANON_SKIPPED" in ibind_text:
            print("ibind_bindgen: anonymous enum constants should honor --prefix")
            print(ibind_text)
            return 1
        if "NOT_IB_STATIC_SKIPPED" in ibind_text:
            print("ibind_bindgen: typed constants should honor --prefix")
            print(ibind_text)
            return 1
        print("ok ibind_bindgen")

    lsp = run([sys.executable, "tests/run_lsp_tests.py"])
    if lsp.returncode != 0:
        print(lsp.stdout)
        return lsp.returncode
    print(lsp.stdout.rstrip())

    torture = run([sys.executable, "tests/run_c_torture.py"])
    if torture.returncode != 0:
        print(torture.stdout)
        return torture.returncode
    print(torture.stdout.rstrip())

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
