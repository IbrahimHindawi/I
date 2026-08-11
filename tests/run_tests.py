from __future__ import annotations

import json
import re
import shutil
import subprocess
import sys
from dataclasses import dataclass
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
BUILD = ROOT / "build"
TEST_DIR = BUILD / "i_tests"
I_EXE = BUILD / "I.exe"

_LINE_DIRECTIVE = re.compile(r'^\s*#line\s+(\d+)\s+"((?:[^"\\]|\\.)*)"\s*$')


def c_line_mapping(c_text: str) -> list[tuple[str, str, int]]:
    """(content, source file, source line) for each generated line, per C #line rules.

    #line renumbers everything after it, so an elided directive is only correct
    when the implied position already matches. This reconstructs what the C
    compiler and debugger will actually believe about every emitted line.
    """
    cur_file, cur_line = "<none>", 0
    mapped: list[tuple[str, str, int]] = []
    for raw in c_text.split("\n"):
        m = _LINE_DIRECTIVE.match(raw)
        if m:
            cur_line, cur_file = int(m.group(1)), m.group(2).replace("\\\\", "\\")
            continue
        if raw.strip():
            mapped.append((raw.strip(), cur_file, cur_line))
        cur_line += 1
    return mapped


@dataclass(frozen=True)
class Case:
    name: str
    source: str
    expected_stdout: str
    extra_files: tuple[tuple[str, str], ...] = ()
    generated_contains: tuple[str, ...] = ()
    header_contains: tuple[str, ...] = ()


CASES = (
    Case(
        name="basic_generics",
        source=r'''
cinclude "stdio.h"
import "C:/devel/i/src/std/memops.i"

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
        generated_contains=("array_i32_reflect", "memops_arena_push_array_i32", "generic_arg_type", "I monomorph: struct array<T> -> array_i32;", "I monomorph: proc array<T>reserve -> array_i32_reserve;", "instantiated at"),
        header_contains=("void memops_arena_initialize(memops_arena * arena);", "I monomorph: struct array<T> -> array_i32;", "I monomorph: proc array<T>reserve -> array_i32_reserve;"),
    ),
    Case(
        name="comments",
        source=r'''
cinclude "stdio.h"

// top-level line comment
/* top-level block comment */
Payload:struct = {
    // field comment
    value:i32;
    /* another field comment */
    other:i32;
}

main:proc()->i32 = {
    payload:Payload = {}; // local trailing comment
    payload.value = 1;
    /* expression-adjacent block comment */
    payload.other = payload.value + 2;
    printf("%d %d\n", payload.value, payload.other);
    return 0;
}
''',
        expected_stdout="1 3\n",
    ),
    Case(
        name="enum_dot_members",
        source=r'''
cinclude "stdio.h"

Kind:enum = {
    None,
    Ready,
}

main:proc()->i32 = {
    kind:Kind = Kind.Ready;
    switch (kind) {
        case Kind.None: {
            printf("none\n");
            return 0;
        }
        case Kind.Ready: {
            printf("%d\n", kind);
            return 0;
        }
    }
    return 1;
}
''',
        expected_stdout="1\n",
        generated_contains=("Kind kind = Kind_Ready;", "case Kind_None:", "case Kind_Ready:"),
    ),
    Case(
        name="printfmt",
        source=r'''
cinclude "stdio.h"
import "C:/devel/i/src/std/Print.i"

Payload: struct = {
    x: i32;
    y: f32;
}

print: proc<Payload>(value: Payload)->void = {
    printfmt("Payload{x: {}, y: {}}", value.x, value.y);
}

main: proc()->i32 = {
    count: u64 = 4;
    label: *const char = "hi";
    p: Payload = {.x = 7, .y = 2.5};
    i: i32 = 1;
    printfmt("a {} {} {} {}\n", 3, count, 1.5, label);
    printfmt("{}\n", p);
    printfmt("field[{}] = {}\n", i, Payload<>.fields[i].name);
    print<i32>(9);
    print_cstr("\n");
    printf("{} stays raw\n");
    return 0;
}
''',
        expected_stdout="a 3 4 1.500000 hi\nPayload{x: 7, y: 2.500000}\nfield[1] = y\n9\n{} stays raw\n",
        generated_contains=("print_i32(3);", "print_u64(count);", "print_f32(1.5);", "print_ptr_const_char(label);", "print_Payload(p);", "print_ptr_const_char(Payload_reflect.fields[i].name);", "printf(\"{} stays raw\\n\");"),
    ),
    Case(
        name="reflection_print_runtime",
        source=r'''
cinclude "stdio.h"
import "C:/devel/i/src/std/containers.i"

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
    is_const:u64;
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

Kind:enum = {
    Idle = 1,
    Run,
}

Payload:struct = {
    x:i32;
    kind:Kind;
}

reflect_type_name:proc(type:*const i_reflect_type)->*const char = {
    return type[0].name;
}

reflect_enum_name:proc(type:*const i_reflect_enum, value:i32)->*const char = {
    for (i:u64 = 0; i < type[0].value_count; i += 1) {
        if (type[0].values[i].value == value) {
            return type[0].values[i].name;
        }
    }
    return "unknown";
}

print:proc<Kind>(value:Kind)->void = {
    print_cstr(reflect_enum_name(Kind<>.&, value));
}

print:proc<Payload>(value:Payload)->void = {
    printfmt("{}({}, {})", reflect_type_name(Payload<>.&), value.x, value.kind);
}

main:proc()->i32 = {
    arena:memops_arena = {};
    memops_arena_initialize(&arena);

    payload:Payload = {.x = 9, .kind = Kind_Run};
    opt:Option<Kind> = Option<Kind>some(Kind_Run);
    missing:Option<Kind> = Option<Kind>none();
    ok_payload:Result<Payload> = Result<Payload>ok(payload);
    bad_payload:Result<Payload> = Result<Payload>err(7);
    vec:Vec<Payload> = {};
    Vec<Payload>append(&arena, &vec, payload);

    printfmt("{} {} {} {} {} {} {}\n",
        Payload<>.fields[1].name,
        Option<Kind>unwrap(opt),
        Option<Kind>is_none(missing),
        Result<Payload>unwrap(ok_payload),
        Result<Payload>is_err(bad_payload),
        bad_payload.error,
        Vec<Payload>get(&vec, 0).value);
    return 0;
}
''',
        expected_stdout="kind Run true Payload(9, Run) true 7 Payload(9, Run)\n",
        generated_contains=("&(Kind_reflect)", "&(Payload_reflect)", "print_Kind", "print_Payload", "Payload_reflect.fields[1].name", "Option_Kind_reflect", "Result_Payload_reflect", "Vec_Payload_reflect", "Option_Kind_some", "Result_Payload_ok", "Vec_Payload_append"),
    ),
    Case(
        name="generic_dependency_closure",
        source=r'''
cinclude "stdio.h"
import "C:/devel/i/src/std/containers.i"

Payload:struct = {
    x:i32;
}

Holder:struct<T> = {
    item:T;
    opt:Option<T>;
    res:Result<T>;
    vec:Vec<T>;
}

print:proc<Payload>(value:Payload)->void = {
    printfmt("Payload({})", value.x);
}

make_local:proc<T>(arena:*memops_arena, value:T)->T = {
    vec:Vec<T> = {};
    Vec<T>append(arena, &vec, value);
    opt:Option<T> = Vec<T>get(&vec, 0);
    res:Result<T> = Result<T>ok(Option<T>unwrap(opt));
    return Result<T>unwrap(res);
}

make_option:proc<T>(value:T)->Option<T> = {
    return Option<T>some(value);
}

id_option:proc<T>(value:Option<T>)->T = {
    return Option<T>unwrap(value);
}

Holder<T>make:proc<T>(arena:*memops_arena, value:T)->Holder<T> = {
    holder:Holder<T> = {};
    holder.item = value;
    holder.opt = Option<T>some(value);
    holder.res = Result<T>ok(value);
    Vec<T>append(arena, &holder.vec, value);
    return holder;
}

main:proc()->i32 = {
    arena:memops_arena = {};
    memops_arena_initialize(&arena);

    a:Payload = make_local<Payload>(&arena, {.x = 3});
    b:Payload = id_option<Payload>(make_option<Payload>({.x = 5}));
    holder:Holder<Payload> = Holder<Payload>make(&arena, {.x = 7});

    printfmt("{} {} {}\n", a, b, Result<Payload>unwrap(holder.res));
    printf("%s %llu %s %s %s %s\n",
        Holder_Payload_reflect.name,
        Holder_Payload_reflect.field_count,
        Holder_Payload_reflect.fields[0].name,
        Holder_Payload_reflect.fields[1].name,
        Holder_Payload_reflect.fields[2].name,
        Holder_Payload_reflect.fields[3].name);
    return 0;
}
''',
        expected_stdout="Payload(3) Payload(5) Payload(7)\nHolder_Payload 4 item opt res vec\n",
        generated_contains=("make_local_Payload", "make_option_Payload", "id_option_Payload", "Holder_Payload_make", "Holder_Payload_reflect", "Option_Payload_reflect", "Result_Payload_reflect", "Vec_Payload_reflect", "Option_Payload_some", "Result_Payload_ok", "Vec_Payload_get"),
    ),
    Case(
        name="generic_delayed_numeric_algorithms",
        source=r'''
cinclude "stdio.h"

add:proc<T>(x:T, y:T)->T = {
    return x + y;
}

min_value:proc<T>(x:T, y:T)->T = {
    if (x < y) {
        return x;
    }
    return y;
}

main:proc()->i32 = {
    printf("%d %d %.2f\n", add<i32>(5, 6), min_value<i32>(9, 3), add<f32>(1.5, 2.25));
    return 0;
}
''',
        expected_stdout="11 3 3.75\n",
        generated_contains=("add_i32", "add_f32", "min_value_i32"),
    ),
    Case(
        name="type_operations_playground",
        source=r'''
import "C:/devel/i/src/std/memops.i"
import "C:/devel/i/src/std/Array.i"
import "C:/devel/i/src/std/Node.i"
import "C:/devel/i/src/std/List.i"
import "C:/devel/i/src/std/Print.i"

payload: struct = {
    x: f32;
    y: *u8;
}

add: proc<payload>(x: payload, y: payload) -> payload = {
    return { .x = x.x + y.x };
}

add: proc<i32>(x: i32, y: i32) -> i32 = {
    return x + y;
}

sum: proc<T>(items: *T, count: u64) -> T = {
    result: T = {};
    for (i: u64 = 0; i < count; i += 1) {
        result = add<T>(result, items[i]);
    }
    return result;
}

main: proc(argc: i32, argv: **char)-> i32 = {
    arena: memops_arena = {};
    memops_arena_initialize(arena.&);
    // todo: this line is intentionally an I comment, not C preprocessor output.
    printfmt("{}\n", add<i32>(1, 1));
    x: i32 = add<i32>(1, 1);
    printfmt("{}\n", x);
    y: payload = add<payload>({.x = 2}, {.x = 2});
    printfmt("{}\n", y.x);
    payloads: Array<payload> = Array<payload>reserve(arena.&, 128);
    for (i: i32 = 0; i < payloads.length; i += 1) {
        payloads.data[i] = {.x = i};
    }
    result: payload = sum<payload>(payloads.data, payloads.length);
    printfmt("{}\n", result.x);
    return 0;
}
''',
        expected_stdout="2\n2\n4.000000\n8128.000000\n",
        generated_contains=(
            "#include <reflect.h>",
            "print_i32(add_i32(1, 1));",
            "payload y = add_payload(((payload){.x = 2}), ((payload){.x = 2}));",
            "payload result = sum_payload(payloads.data, payloads.length);",
        ),
    ),
    Case(
        name="nested_generic_reflection",
        source=r'''
cinclude "stdio.h"
import "C:/devel/i/src/std/containers.i"

Payload:struct = {
    x:i32;
}

Pair:struct<T> = {
    value:T;
}

Wrap:struct<T> = {
    pair:Pair<T>;
    maybe:Option<Pair<T>>;
}

Pair<T>make:proc<T>(value:T)->Pair<T> = {
    pair:Pair<T> = {.value = value};
    return pair;
}

main:proc()->i32 = {
    pair:Pair<Payload> = Pair<Payload>make({.x = 11});
    wrap:Wrap<Payload> = {};
    wrap.pair = pair;
    wrap.maybe = Option<Pair<Payload>>some(pair);
    unboxed:Pair<Payload> = Option<Pair<Payload>>unwrap(wrap.maybe);

    printf("%d %s %s %s %s %s %s\n",
        unboxed.value.x,
        Pair_Payload_reflect.name,
        Wrap_Payload_reflect.name,
        Option_Pair_Payload_reflect.name,
        Wrap_Payload_reflect.fields[0].type,
        Wrap_Payload_reflect.fields[1].type,
        Wrap_Payload_reflect.fields[1].generic_arg_type);
    return 0;
}
''',
        expected_stdout="11 Pair_Payload Wrap_Payload Option_Pair_Payload Pair_Payload Option_Pair_Payload Pair_Payload\n",
        generated_contains=("Pair_Payload_reflect", "Wrap_Payload_reflect", "Option_Pair_Payload_reflect", "Pair_Payload_make", "Option_Pair_Payload_some", "Option_Pair_Payload_unwrap"),
    ),
    Case(
        name="runtime_containers",
        source=r'''
cinclude "stdio.h"
import "C:/devel/i/src/std/containers.i"

main:proc()->i32 = {
    arena:memops_arena = {};
    memops_arena_initialize(&arena);

    opt:Option<i32> = Option<i32>some(7);
    none:Option<i32> = Option<i32>none();
    ok:Result<i32> = Result<i32>ok(11);
    err:Result<i32> = Result<i32>err(404);

    arr:Array<i32> = Array<i32>reserve(&arena, 3);
    arr.data[0] = 3;
    arr.data[1] = 5;
    arr.data[2] = 7;
    arr_get:Option<i32> = Array<i32>get(&arr, 1);
    arr_at:*i32 = Array<i32>at(&arr, 2);

    vec:Vec<i32> = {};
    Vec<i32>append(&arena, &vec, 10);
    Vec<i32>append(&arena, &vec, 20);
    Vec<i32>append(&arena, &vec, 30);
    vec_get:Option<i32> = Vec<i32>get(&vec, 2);

    list:*List<i32> = List<i32>create(&arena);
    List<i32>append(&arena, list, 1);
    List<i32>append(&arena, list, 2);
    List<i32>prepend(&arena, list, 0);
    list_removed:*Node<i32> = List<i32>remove_at(&arena, list, 1);
    list_removed_value:*Node<i32> = List<i32>remove(&arena, list, 2);

    dlist:*DList<i32> = DList<i32>create(&arena);
    DList<i32>append(&arena, dlist, 4);
    DList<i32>append(&arena, dlist, 5);
    DList<i32>prepend(&arena, dlist, 3);
    dlist_removed:*BiNode<i32> = DList<i32>remove_at(&arena, dlist, 2);
    dlist_removed_value:*BiNode<i32> = DList<i32>remove(&arena, dlist, 3);

    queue:*Queue<i32> = Queue<i32>create(&arena);
    Queue<i32>enqueue(&arena, queue, 8);
    Queue<i32>enqueue(&arena, queue, 9);
    queue_removed:*Node<i32> = Queue<i32>dequeue(&arena, queue);
    queue_peek:*Node<i32> = Queue<i32>peek(&arena, queue);

    stack:*Stack<i32> = Stack<i32>create(&arena);
    Stack<i32>push(&arena, stack, 12);
    Stack<i32>push(&arena, stack, 13);
    stack_removed:*Node<i32> = Stack<i32>pop(&arena, stack);
    stack_peek:*Node<i32> = Stack<i32>peek(&arena, stack);

    map:*Map<i32> = Map<i32>create(&arena);
    Map<i32>set(&arena, map, "dog", 3);
    Map<i32>set(&arena, map, "frog", 4);
    Map<i32>set(&arena, map, "dog", 5);
    map_dog:*i32 = Map<i32>try_emplace(&arena, map, "dog", 99);
    map_bird:*i32 = Map<i32>try_emplace(&arena, map, "bird", 6);
    it:MapIterator<i32> = MapIterator<i32>create(&arena, map);
    map_sum:i32 = 0;
    map_count:i32 = 0;
    while (MapIterator<i32>next(&arena, &it)) {
        map_sum += it.val;
        map_count += 1;
    }

    printf("%d %d %d %d %llu %d %d %llu %d %llu %d %d %llu %d %d %llu %d %d %llu %d %d %llu %d %d %d %d\n",
        Option<i32>unwrap(opt),
        Option<i32>is_none(none),
        Result<i32>unwrap(ok),
        Result<i32>is_err(err),
        arr.length,
        Option<i32>unwrap(arr_get),
        arr_at[0],
        vec.length,
        Option<i32>unwrap(vec_get),
        list[0].length,
        list_removed[0].data,
        list_removed_value[0].data,
        dlist[0].length,
        dlist_removed[0].data,
        dlist_removed_value[0].data,
        queue[0].length,
        queue_removed[0].data,
        queue_peek[0].data,
        stack[0].length,
        stack_removed[0].data,
        stack_peek[0].data,
        Map<i32>length(&arena, map),
        map_dog[0],
        map_bird[0],
        map_count,
        map_sum);
    return 0;
}
''',
        expected_stdout="7 1 11 1 3 5 7 3 30 1 1 2 1 5 3 1 8 9 1 13 12 3 5 6 3 15\n",
        generated_contains=("Option_i32_reflect", "Result_i32_reflect", "Array_i32_reflect", "Vec_i32_reflect", "List_i32_reflect", "DList_i32_reflect", "Queue_i32_reflect", "Stack_i32_reflect", "Map_i32_reflect"),
    ),
    Case(
        name="runtime_string8",
        source=r'''
cinclude "stdio.h"
import "C:/devel/i/src/std/Print.i"
import "C:/devel/i/src/std/string8.i"

main:proc()->i32 = {
    arena:memops_arena = {};
    memops_arena_initialize(&arena);

    empty:string8 = string8_from_cstr(&arena, "");
    null_s:string8 = string8_from_cstr(&arena, null);
    zero:string8 = {};
    string8_append_cstr(&arena, &zero, "zero");

    text:string8 = string8_from_cstr(&arena, "hello");
    string8_append_byte(&arena, &text, cast(44, u8));
    string8_append_cstr(&arena, &text, "world");

    parts:Vec<string8slice> = string8slice_split_from_string8(&arena, text, cast(44, u8));
    owned:Vec<string8> = string8_split_char(&arena, text, cast(44, u8));
    copy:string8 = string8_copy_from_slice(&arena, parts.data[1].data, parts.data[1].length);
    trim_src:string8 = string8_from_cstr(&arena, "  Hello/World.TXT  ");
    trimmed:string8slice = string8_trim(trim_src);
    lower:string8 = string8slice_lower_copy(&arena, trimmed);
    norm:string8 = path_normalize_slashes(&arena, string8slice_from_cstr("root\\dir\\file.txt"));
    joined:string8 = path_join(&arena, string8slice_from_cstr("root/"), string8slice_from_cstr("/child\\file.i"));
    dir:string8slice = path_dirname(string8slice_from_string8(norm));
    base:string8slice = path_basename(string8slice_from_string8(norm));
    ext:string8slice = path_extension(string8slice_from_string8(norm));
    stripped:string8slice = path_strip_extension(string8slice_from_string8(norm));

    printf("%llu %d %d %d ",
        text.length,
        string8_equals_cstr(&text, "hello,world"),
        string8slice_equals_cstr(parts.data[0], "hello"),
        string8_equals_cstr(&owned.data[1], "world"));
    string8_print(&text);
    printf(" ");
    string8slice_print(parts.data[0]);
    printf(" %s %llu %llu ", string8_to_cstr_temp(&arena, copy), parts.length, owned.length);
    printf("%llu %llu %llu %d %d %d ",
        empty.length,
        zero.length,
        null_s.length,
        string8_equals_cstr(&empty, ""),
        string8_equals_cstr(&zero, "zero"),
        string8_equals_cstr(&null_s, ""));
    printf("%d %d %lld %d %d %d ",
        string8slice_starts_with(trimmed, string8slice_from_cstr("Hello")),
        string8slice_ends_with(trimmed, string8slice_from_cstr(".TXT")),
        string8slice_find(trimmed, string8slice_from_cstr("World")),
        string8slice_contains(trimmed, string8slice_from_cstr("World")),
        string8slice_eq_ignore_case(trimmed, string8slice_from_cstr("hello/world.txt")),
        string8_hash(lower) == string8slice_hash(string8slice_from_cstr("hello/world.txt")));
    printfmt("{} {} {} {} {} {} {} {}\n", trimmed, lower, norm, joined, dir, base, ext, stripped);
    return 0;
}
''',
        expected_stdout="11 1 1 1 hello,world hello world 2 2 0 4 0 1 1 1 1 1 6 1 1 1 Hello/World.TXT hello/world.txt root/dir/file.txt root/child/file.i root/dir file.txt .txt root/dir/file\n",
        generated_contains=("string8_reflect", "string8slice_reflect", "Vec_string8_reflect", "Vec_string8slice_reflect", "print_string8", "print_string8slice"),
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
    is_const:u64;
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

i_reflect_type_kind_name:proc(kind:i32)->*const char = { external; }
i_reflect_field_is_pointer:proc(field:*const i_reflect_field)->i32 = { external; }
i_reflect_field_is_array:proc(field:*const i_reflect_field)->i32 = { external; }
i_reflect_field_is_generic:proc(field:*const i_reflect_field)->i32 = { external; }
i_reflect_count_fields_with_kind:proc(type:*const i_reflect_type, kind:i32)->u64 = { external; }
i_reflect_find_field_with_kind:proc(type:*const i_reflect_type, kind:i32)->*const i_reflect_field = { external; }
i_reflect_next_field_with_kind:proc(type:*const i_reflect_type, kind:i32, after:*const i_reflect_field)->*const i_reflect_field = { external; }
i_reflect_find_field:proc(type:*const i_reflect_type, name:*const char)->*const i_reflect_field = { external; }
i_reflect_field_index:proc(type:*const i_reflect_type, field:*const i_reflect_field, fallback:u64)->u64 = { external; }
i_reflect_find_field_index:proc(type:*const i_reflect_type, name:*const char, fallback:u64)->u64 = { external; }
i_reflect_field_at:proc(type:*const i_reflect_type, index:u64)->*const i_reflect_field = { external; }
i_reflect_find_field_by_offset:proc(type:*const i_reflect_type, offset:u64)->*const i_reflect_field = { external; }
i_reflect_field_end_offset:proc(field:*const i_reflect_field)->u64 = { external; }
i_reflect_find_field_containing_offset:proc(type:*const i_reflect_type, offset:u64)->*const i_reflect_field = { external; }
i_reflect_field_ptr:proc(base:*void, field:*const i_reflect_field)->*void = { external; }
i_reflect_field_const_ptr:proc(base:*const void, field:*const i_reflect_field)->*const void = { external; }
i_reflect_field_copy:proc(dst_base:*void, src_base:*const void, field:*const i_reflect_field)->i32 = { external; }
i_reflect_field_zero:proc(base:*void, field:*const i_reflect_field)->i32 = { external; }
i_reflect_field_copy_by_name:proc(dst_base:*void, src_base:*const void, type:*const i_reflect_type, name:*const char)->i32 = { external; }
i_reflect_field_zero_by_name:proc(base:*void, type:*const i_reflect_type, name:*const char)->i32 = { external; }
i_reflect_field_has_attr:proc(field:*const i_reflect_field, attr:*const char)->i32 = { external; }
i_reflect_count_fields_with_attr:proc(type:*const i_reflect_type, attr:*const char)->u64 = { external; }
i_reflect_find_field_with_attr:proc(type:*const i_reflect_type, attr:*const char)->*const i_reflect_field = { external; }
i_reflect_next_field_with_attr:proc(type:*const i_reflect_type, attr:*const char, after:*const i_reflect_field)->*const i_reflect_field = { external; }
i_reflect_find_enum_value_by_name:proc(type:*const i_reflect_enum, name:*const char)->*const i_reflect_enum_value = { external; }
i_reflect_find_enum_value_by_value:proc(type:*const i_reflect_enum, value:i32)->*const i_reflect_enum_value = { external; }
i_reflect_enum_value_at:proc(type:*const i_reflect_enum, index:u64)->*const i_reflect_enum_value = { external; }
i_reflect_enum_name_from_value:proc(type:*const i_reflect_enum, value:i32)->*const char = { external; }
i_reflect_enum_value_from_name:proc(type:*const i_reflect_enum, name:*const char, fallback:i32)->i32 = { external; }

Color:enum = {
    Red = 1,
    Green,
    Blue,
}

Bag:struct<T> = {
    item:T;
}

Player:struct = {
    kind:Color;
    hp:i32 @ "editor,serialize";
    label:*const char;
    score:i32 @ "editor,path\\tag";
    inventory:[3]i32;
    bag:Bag<i32>;
}

main:proc()->i32={
    p:Player = {};
    p.kind = Color_Green;
    p.hp = I_TEST_HP;
    p.label = "hero";
    p.score = 123;
    hp_field:*const i_reflect_field = i_reflect_find_field(&Player_reflect, "hp");
    hp_value_ptr:*i32 = cast(i_reflect_field_ptr(&p, hp_field), *i32);
    hp_value_ptr[0] += 1;
    hp_offset_field:*const i_reflect_field = i_reflect_find_field_by_offset(&Player_reflect, Player_reflect.fields[1].offset);
    hp_containing_field:*const i_reflect_field = i_reflect_find_field_containing_offset(&Player_reflect, Player_reflect.fields[1].offset + 1);
    hp_index_field:*const i_reflect_field = i_reflect_field_at(&Player_reflect, 1);
    missing_index_field:*const i_reflect_field = i_reflect_field_at(&Player_reflect, Player_reflect.field_count);
    editor_field:*const i_reflect_field = i_reflect_find_field_with_attr(&Player_reflect, "editor");
    missing_attr_field:*const i_reflect_field = i_reflect_find_field_with_attr(&Player_reflect, "missing");
    first_editor_field:*const i_reflect_field = i_reflect_next_field_with_attr(&Player_reflect, "editor", null);
    second_editor_field:*const i_reflect_field = i_reflect_next_field_with_attr(&Player_reflect, "editor", first_editor_field);
    no_more_editor_field:*const i_reflect_field = i_reflect_next_field_with_attr(&Player_reflect, "editor", second_editor_field);
    green_value:*const i_reflect_enum_value = i_reflect_find_enum_value_by_name(&Color_reflect, "Green");
    blue_value:*const i_reflect_enum_value = i_reflect_find_enum_value_by_value(&Color_reflect, Color_Blue);
    blue_index_value:*const i_reflect_enum_value = i_reflect_enum_value_at(&Color_reflect, 2);
    missing_index_value:*const i_reflect_enum_value = i_reflect_enum_value_at(&Color_reflect, Color_reflect.value_count);
    green_name:*const char = i_reflect_enum_name_from_value(&Color_reflect, Color_Green);
    missing_name:*const char = i_reflect_enum_name_from_value(&Color_reflect, 99);
    generic_kind_field:*const i_reflect_field = i_reflect_find_field_with_kind(&Player_reflect, 2);
    missing_kind_field:*const i_reflect_field = i_reflect_find_field_with_kind(&Player_reflect, 4);
    first_name_kind_field:*const i_reflect_field = i_reflect_next_field_with_kind(&Player_reflect, 0, null);
    second_name_kind_field:*const i_reflect_field = i_reflect_next_field_with_kind(&Player_reflect, 0, first_name_kind_field);
    third_name_kind_field:*const i_reflect_field = i_reflect_next_field_with_kind(&Player_reflect, 0, second_name_kind_field);
    no_more_name_kind_field:*const i_reflect_field = i_reflect_next_field_with_kind(&Player_reflect, 0, third_name_kind_field);
    q:Player = {};
    copy_ok:i32 = i_reflect_field_copy(&q, &p, hp_field);
    copied_hp:i32 = q.hp;
    zero_ok:i32 = i_reflect_field_zero(&q, hp_field);
    zeroed_hp:i32 = q.hp;
    copy_missing:i32 = i_reflect_field_copy(null, &p, hp_field);
    copy_score_ok:i32 = i_reflect_field_copy_by_name(&q, &p, &Player_reflect, "score");
    copied_score:i32 = q.score;
    zero_score_ok:i32 = i_reflect_field_zero_by_name(&q, &Player_reflect, "score");
    zeroed_score:i32 = q.score;
    copy_missing_name:i32 = i_reflect_field_copy_by_name(&q, &p, &Player_reflect, "missing");
    printf("%s %llu %llu %llu %s %d %s %d %s %d %s %llu %s %s %s %llu %d %d %s %s %s %d %s %d %d %d %llu %llu %llu %s %d %s %s %d %s %s %s %s %d %d %d %d %d %d %llu %llu %llu %llu %llu %s %d %s %s %s %d %llu %s %d %d %d %d %llu %llu %s %d %s %d %d %d %d %d %d %d %d %d %d %d\n",
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
        Player_reflect.fields[2].is_const,
        p.kind,
        p.hp,
        hp_field[0].name,
        hp_offset_field[0].name,
        green_value[0].name,
        green_value[0].value,
        blue_value[0].name,
        i_reflect_field_has_attr(hp_field, "editor"),
        i_reflect_field_has_attr(hp_field, "serialize"),
        i_reflect_field_has_attr(hp_field, "serial"),
        i_reflect_count_fields_with_attr(&Player_reflect, "editor"),
        i_reflect_count_fields_with_attr(&Player_reflect, "serialize"),
        i_reflect_count_fields_with_attr(&Player_reflect, "missing"),
        editor_field[0].name,
        missing_attr_field == null,
        first_editor_field[0].name,
        second_editor_field[0].name,
        no_more_editor_field == null,
        i_reflect_type_kind_name(Player_reflect.fields[1].kind),
        i_reflect_type_kind_name(Player_reflect.fields[2].kind),
        i_reflect_type_kind_name(999),
        green_name,
        i_reflect_enum_value_from_name(&Color_reflect, "Blue", -1),
        i_reflect_enum_value_from_name(&Color_reflect, "Missing", -1) + (missing_name == null),
        i_reflect_field_is_pointer(&Player_reflect.fields[2]),
        i_reflect_field_is_array(&Player_reflect.fields[4]),
        i_reflect_field_is_generic(&Player_reflect.fields[5]),
        i_reflect_field_is_pointer(&Player_reflect.fields[1]),
        i_reflect_count_fields_with_kind(&Player_reflect, 0),
        i_reflect_count_fields_with_kind(&Player_reflect, 1),
        i_reflect_count_fields_with_kind(&Player_reflect, 2),
        i_reflect_count_fields_with_kind(&Player_reflect, 3),
        i_reflect_count_fields_with_kind(&Player_reflect, 4),
        generic_kind_field[0].name,
        missing_kind_field == null,
        first_name_kind_field[0].name,
        second_name_kind_field[0].name,
        third_name_kind_field[0].name,
        no_more_name_kind_field == null,
        i_reflect_field_end_offset(hp_field),
        hp_containing_field[0].name,
        i_reflect_find_field_containing_offset(&Player_reflect, Player_reflect.size) == null,
        hp_value_ptr[0],
        i_reflect_field_const_ptr(&p, hp_field) != null,
        i_reflect_field_ptr(null, hp_field) == null,
        i_reflect_field_index(&Player_reflect, hp_field, 999),
        i_reflect_find_field_index(&Player_reflect, "score", 999),
        hp_index_field[0].name,
        missing_index_field == null,
        blue_index_value[0].name,
        missing_index_value == null,
        copy_ok,
        copied_hp,
        zero_ok,
        zeroed_hp,
        copy_missing,
        copy_score_ok,
        copied_score,
        zero_score_ok,
        zeroed_score,
        copy_missing_name);
    return 0;
}
''',
        expected_stdout="Color 4 4 3 Red 1 Green 2 Blue 3 Player 6 kind i32 editor,serialize 1 2 78 hp hp Green 2 Blue 1 1 0 2 1 0 hp 1 hp score 1 name ptr unknown Green 3 0 1 1 1 0 3 1 1 1 0 bag 1 kind hp score 1 8 hp 1 78 1 1 1 3 hp 1 Blue 1 1 78 1 0 0 1 123 1 0 0\n",
        generated_contains=("#define I_TEST_HP 77", "typedef enum Color", "Player_reflect", "i_reflect_type_kind_name", "i_reflect_field_is_pointer", "i_reflect_field_is_array", "i_reflect_field_is_generic", "i_reflect_count_fields_with_kind", "i_reflect_find_field_with_kind", "i_reflect_next_field_with_kind", "i_reflect_find_field", "i_reflect_field_index", "i_reflect_find_field_index", "i_reflect_field_at", "i_reflect_find_field_by_offset", "i_reflect_field_end_offset", "i_reflect_find_field_containing_offset", "i_reflect_field_ptr", "i_reflect_field_const_ptr", "i_reflect_field_copy", "i_reflect_field_zero", "i_reflect_field_copy_by_name", "i_reflect_field_zero_by_name", "i_reflect_field_has_attr", "i_reflect_count_fields_with_attr", "i_reflect_find_field_with_attr", "i_reflect_next_field_with_attr", "i_reflect_enum_value_at", "i_reflect_enum_name_from_value", "i_reflect_enum_value_from_name", "editor,serialize", "editor,path\\\\\\\\tag", "is_const"),
        header_contains=("extern const i_reflect_type Player_reflect;", "typedef enum Color"),
    ),
    Case(
        name="reflect_angle_syntax",
        source=r'''
cinclude "stdio.h"

Payload:struct = {
    x:i32;
    y:*u8;
}

main:proc()->i32 = {
    printf("%s %llu %s\n", Payload<>.name, Payload<>.field_count, Payload<>.fields[1].name);
    return 0;
}
''',
        expected_stdout="Payload 2 y\n",
        generated_contains=("Payload_reflect.name", "Payload_reflect.field_count", "Payload_reflect.fields[1].name"),
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
        case 2: {
            total = platform_add(p.values[1], p.flags + mod);
            break;
        }
        default: {
            total = 99;
            break;
        }
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

Mode:enum = {
    None,
    Ready,
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
    total += choose(0, Mode.Ready);
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
            "typedef i32 (WINCALL *Binary)(i32 a, i32 b);",
            "uniondef(Value)",
            "do {",
            " ? ",
            "choose(i32 a, ...)",
            'const char * label = "hello";',
            "Mode_Ready",
        ),
        header_contains=("typedef i32 I32;", "typedef i32 (WINCALL *Binary)(i32 a, i32 b);", "uniondef(Value)"),
    ),
    Case(
        name="external_c_array_alias_generic_specialization",
        source=r'''
cinclude "stdio.h"
cinclude "external_c_array_alias_generic_specialization_types.h"

vec2:alias = [2]f32;
vec3:alias = [3]f32;

touch_vec2:proc(v:vec2)->void = { external; }
touch_vec3:proc(v:vec3)->void = { external; }

json_read:proc<vec2>(out:vec2)->i32 = {
    out[0] = 2.0f;
    out[1] = 3.0f;
    return 2;
}

json_read:proc<vec3>(out:vec3)->i32 = {
    out[0] = 5.0f;
    out[1] = 7.0f;
    out[2] = 11.0f;
    return 3;
}

main:proc()->i32 = {
    a:vec2 = {};
    b:vec3 = {};
    count:i32 = json_read<vec2>(a) + json_read<vec3>(b);
    printf("%d %.0f %.0f\n", count, a[1], b[2]);
    return 0;
}
''',
        expected_stdout="5 3 11\n",
        extra_files=(
            (
                "external_c_array_alias_generic_specialization_types.h",
                "typedef float vec2[2];\ntypedef float vec3[3];\n",
            ),
        ),
        generated_contains=("json_read_vec2", "json_read_vec3", "vec2 a", "vec3 b", "a[1]", "b[2]"),
        header_contains=("i32 json_read_vec2(vec2 out);", "i32 json_read_vec3(vec3 out);"),
    ),
    Case(
        name="generic_type_arg_pattern_overloads",
        source=r'''
cinclude "stdio.h"
import "C:/devel/i/src/std/memops.i"
import "C:/devel/i/src/std/Array.i"
import "C:/devel/i/src/std/Vec.i"

g_counter:i32 = 1;

json_read:proc<i32>(out:*i32)->b32 = {
    out[0] = g_counter;
    g_counter += 1;
    return 1;
}

json_read:proc<Array<T>>(arena:*memops_arena, out:*Array<T>, count:u64)->b32 = {
    out[0] = Array<T>reserve(arena, count);
    for (i:u64 = 0; i < count; i += 1) {
        if (json_read<T>(out[0].data[i].&) == 0) {
            return 0;
        }
    }
    return 1;
}

json_read:proc<Vec<T>>(arena:*memops_arena, out:*Vec<T>, count:u64)->b32 = {
    out[0] = Vec<T>reserve(arena, count);
    for (i:u64 = 0; i < count; i += 1) {
        value:T = {};
        if (json_read<T>(value.&) == 0) {
            return 0;
        }
        Vec<T>append(arena, out, value);
    }
    return 1;
}

main:proc()->i32 = {
    arena:memops_arena = {};
    arr:Array<i32> = {};
    vec:Vec<i32> = {};
    memops_arena_initialize(arena.&);
    json_read<Array<i32>>(arena.&, arr.&, 3);
    json_read<Vec<i32>>(arena.&, vec.&, 2);
    printf("%llu %d %d %llu %llu %d %d\n", arr.length, arr.data[0], arr.data[2], vec.length, vec.border, vec.data[0], vec.data[1]);
    return 0;
}
''',
        expected_stdout="3 1 3 2 2 4 5\n",
        generated_contains=("json_read_Array_i32", "json_read_Vec_i32", "Array_i32_reserve", "Vec_i32_append", "json_read_i32"),
        header_contains=("b32 json_read_Array_i32(memops_arena * arena, Array_i32 * out, u64 count);", "b32 json_read_Vec_i32(memops_arena * arena, Vec_i32 * out, u64 count);"),
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
        name="typed_compound_initializers",
        source=r'''
cinclude "stdio.h"

Payload:struct = {
    x:i32;
    y:i32;
}

Box:struct<T> = {
    value:T;
    pair:[2]T;
}

take_payload:proc(p:Payload)->i32 = {
    return p.x + p.y;
}

take_box:proc(box:Box<i32>)->i32 = {
    return box.value + box.pair[0] + box.pair[1];
}

main:proc()->i32 = {
    p:Payload = Payload{.x = 2, .y = 3};
    sum:i32 = take_payload(Payload{.x = 4, .y = 5});
    b:Box<i32> = Box<i32>{.value = 6, .pair = {7, 8}};
    total:i32 = take_box(Box<i32>{.value = 1, .pair = {2, 3}}) + take_box(b);
    printf("%d %d %d\n", p.x + p.y, sum, total);
    return 0;
}
''',
        expected_stdout="5 9 27\n",
        generated_contains=(
            "((Payload){.x = 4, .y = 5})",
            "((Box_i32){.value = 1, .pair = {2, 3}})",
            "Box_i32_reflect",
        ),
        header_contains=("structdecl(Payload);", "structdecl(Box_i32);"),
    ),
    Case(
        name="generic_value_struct_order_and_bare_init_arg",
        source=r'''
cinclude "stdio.h"

Box:struct<T> = {
    value:T;
}

Payload:struct = {
    x:i32;
}

take_box:proc(box:Box<Payload>)->i32 = {
    return box.value.x;
}

main:proc()->i32 = {
    value:i32 = take_box({.value = {.x = 42}});
    printf("%d\n", value);
    return 0;
}
''',
        expected_stdout="42\n",
        generated_contains=(
            "take_box(((Box_Payload){.value = {.x = 42}}))",
            "structdef(Payload)",
            "structdef(Box_Payload)",
        ),
        header_contains=("structdecl(Payload);", "structdecl(Box_Payload);"),
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
        generated_contains=("typedef i32 (*Callback)(i32 x, const char * label);", "Callback cb;", "i32 call_twice(Callback cb)"),
        header_contains=("typedef i32 (*Callback)(i32 x, const char * label);", "Callback cb;"),
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


def run(cmd: list[str], cwd: Path = ROOT, input: str | None = None) -> subprocess.CompletedProcess[str]:
    return subprocess.run(cmd, cwd=cwd, input=input, text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)


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
        for rel_path, contents in case.extra_files:
            extra_path = TEST_DIR / rel_path
            extra_path.parent.mkdir(parents=True, exist_ok=True)
            extra_path.write_text(contents, encoding="utf-8", newline="\n")
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
            "src/std",
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

    check_i = TEST_DIR / "check_mode.i"
    check_c = TEST_DIR / "check_mode_should_not_exist.c"
    if check_c.exists():
        check_c.unlink()
    check_i.write_text(r'''
main:proc()->i32 = {
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    check = run([str(I_EXE), "--check", str(check_i), str(check_c)])
    if check.returncode != 0 or f"i: checked {check_i}" not in check.stdout or check_c.exists():
        print("check_mode: expected --check to validate without generating C")
        print(check.stdout)
        return 1
    print("ok check_mode")

    cli_help = run([str(I_EXE), "--help"])
    if (
        cli_help.returncode != 0
        or "usage:" not in cli_help.stdout
        or "I compile [input.i]" not in cli_help.stdout
        or "I check   [input.i]" not in cli_help.stdout
        or "--importdir <dir>" not in cli_help.stdout
    ):
        print("cli_help: expected readable command-line help")
        print(cli_help.stdout)
        return 1
    print("ok cli_help")

    cli_version = run([str(I_EXE), "--version"])
    if cli_version.returncode != 0 or "I compiler" not in cli_version.stdout:
        print("cli_version: expected compiler version output")
        print(cli_version.stdout)
        return 1
    print("ok cli_version")

    cli_check = run([str(I_EXE), "check", str(check_i)])
    if cli_check.returncode != 0 or f"i: checked {check_i}" not in cli_check.stdout or check_c.exists():
        print("cli_check_command: expected check command to validate without generating C")
        print(cli_check.stdout)
        return 1
    print("ok cli_check_command")

    cli_compile_c = TEST_DIR / "cli_compile.c"
    cli_compile_h = TEST_DIR / "cli_compile.h"
    for path in (cli_compile_c, cli_compile_h):
        if path.exists():
            path.unlink()
    cli_compile = run(
        [
            str(I_EXE),
            "compile",
            str(check_i),
            "-o",
            str(cli_compile_c),
            "--header",
            str(cli_compile_h),
        ]
    )
    if (
        cli_compile.returncode != 0
        or not cli_compile_c.exists()
        or not cli_compile_h.exists()
        or f"i: generated {cli_compile_c} and {cli_compile_h}" not in cli_compile.stdout
    ):
        print("cli_compile_command: expected compile command to generate C and header outputs")
        print(cli_compile.stdout)
        return 1
    print("ok cli_compile_command")

    cli_no_header_c = TEST_DIR / "cli_no_header.c"
    cli_no_header_h = TEST_DIR / "cli_no_header.h"
    for path in (cli_no_header_c, cli_no_header_h):
        if path.exists():
            path.unlink()
    cli_no_header = run(
        [
            str(I_EXE),
            "compile",
            str(check_i),
            "-o",
            str(cli_no_header_c),
            "--no-header",
        ]
    )
    if (
        cli_no_header.returncode != 0
        or not cli_no_header_c.exists()
        or cli_no_header_h.exists()
        or f"i: generated {cli_no_header_c}" not in cli_no_header.stdout
    ):
        print("cli_no_header_command: expected compile command to generate only C output")
        print(cli_no_header.stdout)
        return 1
    print("ok cli_no_header_command")

    cli_importdir_root = TEST_DIR / "cli_importdir_root"
    cli_importdir_std = cli_importdir_root / "std"
    cli_importdir_std.mkdir(parents=True, exist_ok=True)
    (cli_importdir_std / "importdir_smoke.i").write_text(r'''
ImportDirPayload:struct = {
    value:i32;
}

importdir_value:proc()->i32 = {
    payload:ImportDirPayload = {.value = 42};
    return payload.value;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    cli_importdir_i = TEST_DIR / "cli_importdir.i"
    cli_importdir_i.write_text(r'''
import "std/importdir_smoke.i"

main:proc()->i32 = {
    return importdir_value();
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    cli_importdir = run([
        str(I_EXE),
        "check",
        str(cli_importdir_i),
        "--importdir",
        str(cli_importdir_root),
        "--diagnostics=json",
    ])
    try:
        cli_importdir_data = json.loads(cli_importdir.stdout)
    except json.JSONDecodeError:
        print("cli_importdir: expected JSON diagnostics")
        print(cli_importdir.stdout)
        return 1
    if cli_importdir.returncode != 0 or cli_importdir_data != []:
        print("cli_importdir: expected --importdir to resolve imported module")
        print(cli_importdir.stdout)
        return 1
    print("ok cli_importdir")

    cli_symbols = run([str(I_EXE), "symbols", str(check_i)])
    try:
        cli_symbols_data = json.loads(cli_symbols.stdout)
    except json.JSONDecodeError:
        print("cli_symbols_command: expected JSON symbol output")
        print(cli_symbols.stdout)
        return 1
    if (
        cli_symbols.returncode != 0
        or not isinstance(cli_symbols_data, list)
        or not any(item.get("kind") == "proc" and item.get("name") == "main" for item in cli_symbols_data)
    ):
        print("cli_symbols_command: expected symbols command to emit compiler JSON symbols")
        print(cli_symbols.stdout)
        return 1
    print("ok cli_symbols_command")

    cli_lsp = run([str(I_EXE), "lsp", str(check_i)])
    try:
        cli_lsp_data = json.loads(cli_lsp.stdout)
    except json.JSONDecodeError:
        print("cli_lsp_command: expected JSON LSP output")
        print(cli_lsp.stdout)
        return 1
    if (
        cli_lsp.returncode != 0
        or not isinstance(cli_lsp_data, dict)
        or cli_lsp_data.get("diagnostics") != []
        or not any(item.get("kind") == "proc" and item.get("name") == "main" for item in cli_lsp_data.get("symbols", []))
    ):
        print("cli_lsp_command: expected lsp command to emit diagnostics plus symbols")
        print(cli_lsp.stdout)
        return 1
    print("ok cli_lsp_command")

    check_json = run([str(I_EXE), "--check", str(check_i), "--diagnostics=json"])
    try:
        check_json_data = json.loads(check_json.stdout)
    except json.JSONDecodeError:
        print("check_json_success: expected JSON diagnostics array")
        print(check_json.stdout)
        return 1
    if check_json.returncode != 0 or check_json_data != []:
        print("check_json_success: expected empty diagnostics array")
        print(check_json.stdout)
        return 1
    print("ok check_json_success")

    check_json_cli = run([str(I_EXE), "--diagnostics=json", "--definitely-not-an-i-option"])
    try:
        check_json_cli_data = json.loads(check_json_cli.stdout)
    except json.JSONDecodeError:
        print("check_json_cli: expected JSON CLI diagnostic")
        print(check_json_cli.stdout)
        return 1
    if (
        check_json_cli.returncode == 0
        or not isinstance(check_json_cli_data, list)
        or not check_json_cli_data
        or check_json_cli_data[0].get("category") != "cli"
        or check_json_cli_data[0].get("file") != "<cli>"
        or "unknown option --definitely-not-an-i-option" not in check_json_cli_data[0].get("message", "")
    ):
        print("check_json_cli: expected structured CLI diagnostic")
        print(check_json_cli.stdout)
        return 1
    print("ok check_json_cli")

    check_json_cli_order = run([str(I_EXE), "--definitely-not-an-i-option", "--diagnostics=json"])
    try:
        check_json_cli_order_data = json.loads(check_json_cli_order.stdout)
    except json.JSONDecodeError:
        print("check_json_cli_order: expected JSON CLI diagnostic even when --diagnostics=json appears later")
        print(check_json_cli_order.stdout)
        return 1
    if (
        check_json_cli_order.returncode == 0
        or not isinstance(check_json_cli_order_data, list)
        or not check_json_cli_order_data
        or check_json_cli_order_data[0].get("category") != "cli"
        or "unknown option --definitely-not-an-i-option" not in check_json_cli_order_data[0].get("message", "")
    ):
        print("check_json_cli_order: expected order-independent structured CLI diagnostic")
        print(check_json_cli_order.stdout)
        return 1
    print("ok check_json_cli_order")

    check_json_io_missing = TEST_DIR / "does_not_exist.i"
    check_json_io = run([str(I_EXE), "--diagnostics=json", "--check", str(check_json_io_missing)])
    try:
        check_json_io_data = json.loads(check_json_io.stdout)
    except json.JSONDecodeError:
        print("check_json_io: expected JSON I/O diagnostic")
        print(check_json_io.stdout)
        return 1
    if (
        check_json_io.returncode == 0
        or not isinstance(check_json_io_data, list)
        or not check_json_io_data
        or check_json_io_data[0].get("category") != "io"
        or check_json_io_data[0].get("file") != str(check_json_io_missing)
        or f"failed to read {check_json_io_missing}" not in check_json_io_data[0].get("message", "")
    ):
        print("check_json_io: expected structured failed-read diagnostic")
        print(check_json_io.stdout)
        return 1
    print("ok check_json_io")

    check_json_write_i = TEST_DIR / "check_json_write.i"
    check_json_write_i.write_text(r'''
main:proc()->i32 = {
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    check_json_output_write_dir = TEST_DIR / "check_json_output_write_dir"
    if check_json_output_write_dir.exists() and not check_json_output_write_dir.is_dir():
        check_json_output_write_dir.unlink()
    check_json_output_write_dir.mkdir(parents=True, exist_ok=True)
    check_json_output_write = run(
        [
            str(I_EXE),
            str(check_json_write_i),
            str(check_json_output_write_dir),
            str(TEST_DIR / "check_json_output_write_unused.h"),
            "--diagnostics=json",
        ]
    )
    try:
        check_json_output_write_data = json.loads(check_json_output_write.stdout)
    except json.JSONDecodeError:
        print("check_json_output_write: expected JSON I/O diagnostic")
        print(check_json_output_write.stdout)
        return 1
    if (
        check_json_output_write.returncode == 0
        or not isinstance(check_json_output_write_data, list)
        or not check_json_output_write_data
        or check_json_output_write_data[0].get("category") != "io"
        or check_json_output_write_data[0].get("file") != str(check_json_output_write_dir)
        or f"failed to write {check_json_output_write_dir}" not in check_json_output_write_data[0].get("message", "")
    ):
        print("check_json_output_write: expected structured output write diagnostic")
        print(check_json_output_write.stdout)
        return 1
    print("ok check_json_output_write")

    check_json_header_write_c = TEST_DIR / "check_json_header_write.c"
    check_json_header_write_dir = TEST_DIR / "check_json_header_write_dir"
    if check_json_header_write_c.exists():
        check_json_header_write_c.unlink()
    if check_json_header_write_dir.exists() and not check_json_header_write_dir.is_dir():
        check_json_header_write_dir.unlink()
    check_json_header_write_dir.mkdir(parents=True, exist_ok=True)
    check_json_header_write = run(
        [
            str(I_EXE),
            str(check_json_write_i),
            str(check_json_header_write_c),
            str(check_json_header_write_dir),
            "--diagnostics=json",
        ]
    )
    try:
        check_json_header_write_data = json.loads(check_json_header_write.stdout)
    except json.JSONDecodeError:
        print("check_json_header_write: expected JSON I/O diagnostic")
        print(check_json_header_write.stdout)
        return 1
    if (
        check_json_header_write.returncode == 0
        or not isinstance(check_json_header_write_data, list)
        or not check_json_header_write_data
        or check_json_header_write_data[0].get("category") != "io"
        or check_json_header_write_data[0].get("file") != str(check_json_header_write_dir)
        or f"failed to write {check_json_header_write_dir}" not in check_json_header_write_data[0].get("message", "")
    ):
        print("check_json_header_write: expected structured header write diagnostic")
        print(check_json_header_write.stdout)
        return 1
    print("ok check_json_header_write")

    check_json_semantic_i = TEST_DIR / "check_json_semantic.i"
    check_json_semantic_i.write_text(r'''
main:proc()->i32 = {
    return missing_symbol;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    check_json_semantic = run([str(I_EXE), "--check", str(check_json_semantic_i), "--diagnostics=json"])
    try:
        check_json_semantic_data = json.loads(check_json_semantic.stdout)
    except json.JSONDecodeError:
        print("check_json_semantic: expected JSON semantic diagnostic")
        print(check_json_semantic.stdout)
        return 1
    if (
        check_json_semantic.returncode == 0
        or not isinstance(check_json_semantic_data, list)
        or not check_json_semantic_data
        or check_json_semantic_data[0].get("category") != "semantic"
        or check_json_semantic_data[0].get("file") != str(check_json_semantic_i)
        or "use of undeclared identifier 'missing_symbol'" not in check_json_semantic_data[0].get("message", "")
        or check_json_semantic_data[0].get("end_column") != check_json_semantic_data[0].get("column", 0) + len("missing_symbol")
    ):
        print("check_json_semantic: expected structured undeclared identifier diagnostic")
        print(check_json_semantic.stdout)
        return 1
    print("ok check_json_semantic")

    check_json_stdin_i = TEST_DIR / "check_json_stdin.i"
    check_json_stdin_i.write_text(r'''
main:proc()->i32 = {
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    check_json_stdin_source = r'''
main:proc()->i32 = {
    return dirty_missing_symbol;
}
'''.strip() + "\n"
    check_json_stdin = run(
        [str(I_EXE), "--check", str(check_json_stdin_i), "--diagnostics=json", "--stdin"],
        input=check_json_stdin_source,
    )
    try:
        check_json_stdin_data = json.loads(check_json_stdin.stdout)
    except json.JSONDecodeError:
        print("check_json_stdin: expected JSON diagnostic from stdin source")
        print(check_json_stdin.stdout)
        return 1
    if (
        check_json_stdin.returncode == 0
        or not isinstance(check_json_stdin_data, list)
        or not check_json_stdin_data
        or check_json_stdin_data[0].get("category") != "semantic"
        or check_json_stdin_data[0].get("file") != str(check_json_stdin_i)
        or "dirty_missing_symbol" not in check_json_stdin_data[0].get("message", "")
        or check_json_stdin_data[0].get("end_column") != check_json_stdin_data[0].get("column", 0) + len("dirty_missing_symbol")
    ):
        print("check_json_stdin: expected structured dirty-buffer diagnostic using logical source path")
        print(check_json_stdin.stdout)
        return 1
    print("ok check_json_stdin")

    check_json_stdin_root_i = TEST_DIR / "check_json_stdin_root.i"
    check_json_stdin_mod_i = TEST_DIR / "check_json_stdin_mod.i"
    check_json_stdin_mod_i.write_text(r'''
mod_value:proc()->i32 = {
    return root_value;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    check_json_stdin_root_i.write_text(f'''
import "{check_json_stdin_mod_i.as_posix()}"

root_value:i32 = 7;

main:proc()->i32 = {{
    return mod_value();
}}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    check_json_stdin_mod_source = r'''
mod_value:proc()->i32 = {
    return dirty_module_missing;
}
'''.strip() + "\n"
    check_json_stdin_import = run(
        [
            str(I_EXE),
            "--check",
            str(check_json_stdin_root_i),
            "--diagnostics=json",
            "--stdin-path",
            str(check_json_stdin_mod_i),
        ],
        input=check_json_stdin_mod_source,
    )
    try:
        check_json_stdin_import_data = json.loads(check_json_stdin_import.stdout)
    except json.JSONDecodeError:
        print("check_json_stdin_import: expected JSON diagnostic from stdin import override")
        print(check_json_stdin_import.stdout)
        return 1
    if (
        check_json_stdin_import.returncode == 0
        or not isinstance(check_json_stdin_import_data, list)
        or not check_json_stdin_import_data
        or check_json_stdin_import_data[0].get("file") != str(check_json_stdin_mod_i)
        or "dirty_module_missing" not in check_json_stdin_import_data[0].get("message", "")
    ):
        print("check_json_stdin_import: expected dirty imported module diagnostic using project entry")
        print(check_json_stdin_import.stdout)
        return 1
    print("ok check_json_stdin_import")

    check_symbols_mod_i = TEST_DIR / "check_symbols_mod.i"
    check_symbols_root_i = TEST_DIR / "check_symbols_root.i"
    check_symbols_mod_i.write_text(r'''
Shared:struct = {
    value:i32;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    check_symbols_root_i.write_text(r'''
main:proc()->i32 = {
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    check_symbols_source = f'''
import "{check_symbols_mod_i.as_posix()}"

Dirty:struct = {{
    value:i32 @ "editor";
}}

Box:struct<T> = {{
    value:T;
}}

Crate:struct<Item> = {{
    item:Item;
}}

Mode:enum = {{
    Run,
}}

Callback:alias = *proc(x:i32)->i32;

dirty_proc:proc(dirty_arg:i32)->i32 = {{
    dirty_local:i32 = dirty_arg;
    for (dirty_i:i32 = 0; dirty_i < 1; dirty_i += 1) {{
        dirty_local += dirty_i;
    }}
    return dirty_local;
}}

Box<T>get:proc<T>(box:Box<T>)->T = {{
    return box.value;
}}

global_value:i32 = 1;
'''.strip() + "\n"
    check_symbols = run(
        [str(I_EXE), str(check_symbols_root_i), "--symbols=json", "--stdin"],
        input=check_symbols_source,
    )
    try:
        check_symbols_data = json.loads(check_symbols.stdout)
    except json.JSONDecodeError:
        print("check_symbols_json: expected JSON symbol table")
        print(check_symbols.stdout)
        return 1
    if check_symbols.returncode != 0 or not isinstance(check_symbols_data, list):
        print("check_symbols_json: expected successful symbol table")
        print(check_symbols.stdout)
        return 1
    symbols_by_name = {
        item.get("name"): item
        for item in check_symbols_data
        if isinstance(item, dict)
    }
    fields_by_owner_name = {
        (item.get("owner"), item.get("name")): item
        for item in check_symbols_data
        if isinstance(item, dict) and item.get("kind") == "field"
    }
    variables_by_kind_name = {
        (item.get("kind"), item.get("name")): item
        for item in check_symbols_data
        if isinstance(item, dict) and item.get("kind") in {"global", "parameter", "variable"}
    }
    if (
        symbols_by_name.get("Shared", {}).get("file") != str(check_symbols_mod_i)
        or symbols_by_name.get("Dirty", {}).get("kind") != "struct"
        or fields_by_owner_name.get(("Shared", "value"), {}).get("file") != str(check_symbols_mod_i)
        or fields_by_owner_name.get(("Shared", "value"), {}).get("detail") != "Shared.value: i32"
        or fields_by_owner_name.get(("Shared", "value"), {}).get("type") != "i32"
        or fields_by_owner_name.get(("Dirty", "value"), {}).get("attrs") != "editor"
        or fields_by_owner_name.get(("Box", "value"), {}).get("detail") != "Box.value: T"
        or fields_by_owner_name.get(("Box", "value"), {}).get("type") != "T"
        or fields_by_owner_name.get(("Box", "value"), {}).get("type_param") != "T"
        or symbols_by_name.get("Box", {}).get("type_param") != "T"
        or fields_by_owner_name.get(("Crate", "item"), {}).get("type") != "Item"
        or fields_by_owner_name.get(("Crate", "item"), {}).get("type_param") != "Item"
        or symbols_by_name.get("Crate", {}).get("type_param") != "Item"
        or symbols_by_name.get("Mode", {}).get("kind") != "enum"
        or symbols_by_name.get("Mode_Run", {}).get("detail") != "Mode.Run: enum member"
        or symbols_by_name.get("Mode_Run", {}).get("owner") != "Mode"
        or symbols_by_name.get("Mode_Run", {}).get("item") != "Run"
        or symbols_by_name.get("Callback", {}).get("detail") != "Callback:alias = *proc(x:i32)->i32;"
        or symbols_by_name.get("Callback", {}).get("target_type") != "*proc(x:i32)->i32"
        or symbols_by_name.get("Callback", {}).get("params") != [{"name": "x", "type": "i32"}]
        or symbols_by_name.get("Callback", {}).get("return_type") != "i32"
        or symbols_by_name.get("Callback", {}).get("variadic") is not False
        or symbols_by_name.get("dirty_proc", {}).get("detail") != "dirty_proc:proc(dirty_arg:i32)->i32"
        or symbols_by_name.get("dirty_proc", {}).get("params") != [{"name": "dirty_arg", "type": "i32"}]
        or symbols_by_name.get("dirty_proc", {}).get("return_type") != "i32"
        or symbols_by_name.get("dirty_proc", {}).get("variadic") is not False
        or variables_by_kind_name.get(("parameter", "dirty_arg"), {}).get("detail") != "dirty_arg: i32"
        or variables_by_kind_name.get(("parameter", "dirty_arg"), {}).get("type") != "i32"
        or variables_by_kind_name.get(("parameter", "dirty_arg"), {}).get("scope") != "dirty_proc"
        or variables_by_kind_name.get(("variable", "dirty_local"), {}).get("detail") != "dirty_local: i32"
        or variables_by_kind_name.get(("variable", "dirty_local"), {}).get("type") != "i32"
        or variables_by_kind_name.get(("variable", "dirty_local"), {}).get("scope") != "dirty_proc"
        or variables_by_kind_name.get(("variable", "dirty_i"), {}).get("detail") != "dirty_i: i32"
        or variables_by_kind_name.get(("variable", "dirty_i"), {}).get("scope") != "dirty_proc"
        or symbols_by_name.get("Box<T>get", {}).get("detail") != "Box<T>get:proc<T>(box:Box<T>)->T"
        or symbols_by_name.get("Box<T>get", {}).get("params") != [{"name": "box", "type": "Box<T>"}]
        or symbols_by_name.get("Box<T>get", {}).get("return_type") != "T"
        or symbols_by_name.get("Box<T>get", {}).get("type_param") != "T"
        or variables_by_kind_name.get(("parameter", "box"), {}).get("scope") != "Box<T>get"
        or variables_by_kind_name.get(("global", "global_value"), {}).get("detail") != "global_value: i32"
        or variables_by_kind_name.get(("global", "global_value"), {}).get("type") != "i32"
    ):
        print("check_symbols_json: expected compiler-backed top-level symbols and variables")
        print(check_symbols.stdout)
        return 1
    print("ok check_symbols_json")

    check_lsp = run(
        [str(I_EXE), str(check_symbols_root_i), "--lsp=json", "--stdin"],
        input=check_symbols_source,
    )
    try:
        check_lsp_data = json.loads(check_lsp.stdout)
    except json.JSONDecodeError:
        print("check_lsp_json: expected combined LSP JSON payload")
        print(check_lsp.stdout)
        return 1
    if (
        check_lsp.returncode != 0
        or not isinstance(check_lsp_data, dict)
        or check_lsp_data.get("diagnostics") != []
        or not isinstance(check_lsp_data.get("symbols"), list)
        or not any(
            isinstance(item, dict) and item.get("name") == "Dirty" and item.get("kind") == "struct"
            for item in check_lsp_data.get("symbols", [])
        )
        or not any(
            isinstance(item, dict) and item.get("name") == "Shared" and item.get("file") == str(check_symbols_mod_i)
            for item in check_lsp_data.get("symbols", [])
        )
    ):
        print("check_lsp_json: expected checked diagnostics plus import-graph symbols")
        print(check_lsp.stdout)
        return 1

    check_lsp_dirty = run(
        [str(I_EXE), str(check_symbols_root_i), "--lsp=json", "--stdin"],
        input="main:proc()->i32 = {\n    return lsp_dirty_missing;\n}\n",
    )
    try:
        check_lsp_dirty_data = json.loads(check_lsp_dirty.stdout)
    except json.JSONDecodeError:
        print("check_lsp_json_dirty: expected JSON diagnostics on checked LSP failure")
        print(check_lsp_dirty.stdout)
        return 1
    if (
        check_lsp_dirty.returncode == 0
        or not isinstance(check_lsp_dirty_data, list)
        or not check_lsp_dirty_data
        or "lsp_dirty_missing" not in check_lsp_dirty_data[0].get("message", "")
    ):
        print("check_lsp_json_dirty: expected compiler diagnostic list when LSP check fails")
        print(check_lsp_dirty.stdout)
        return 1
    print("ok check_lsp_json")

    check_json_parse_i = TEST_DIR / "check_json_parse.i"
    check_json_parse_i.write_text(r'''
Payload:struct = {
    value i32;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    check_json_parse = run([str(I_EXE), "--check", str(check_json_parse_i), "--diagnostics=json"])
    try:
        check_json_parse_data = json.loads(check_json_parse.stdout)
    except json.JSONDecodeError:
        print("check_json_parse: expected JSON parse diagnostic")
        print(check_json_parse.stdout)
        return 1
    if (
        check_json_parse.returncode == 0
        or not isinstance(check_json_parse_data, list)
        or not check_json_parse_data
        or check_json_parse_data[0].get("category") != "parse"
        or check_json_parse_data[0].get("file") != str(check_json_parse_i)
        or "expected ':' after field name" not in check_json_parse_data[0].get("message", "")
        or check_json_parse_data[0].get("end_column") != check_json_parse_data[0].get("column", 0) + len("i32")
    ):
        print("check_json_parse: expected structured parse diagnostic")
        print(check_json_parse.stdout)
        return 1
    print("ok check_json_parse")

    check_json_type_i = TEST_DIR / "check_json_type.i"
    check_json_type_i.write_text(r'''
Payload:struct = {
    value:i32;
}

main:proc()->i32 = {
    payload:Payload = {};
    value:i32 = payload;
    return value;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    check_json_type = run([str(I_EXE), "--check", str(check_json_type_i), "--diagnostics=json"])
    try:
        check_json_type_data = json.loads(check_json_type.stdout)
    except json.JSONDecodeError:
        print("check_json_type: expected JSON type diagnostic")
        print(check_json_type.stdout)
        return 1
    if (
        check_json_type.returncode == 0
        or not isinstance(check_json_type_data, list)
        or not check_json_type_data
        or check_json_type_data[0].get("category") != "type"
        or check_json_type_data[0].get("file") != str(check_json_type_i)
        or "initializer expected 'i32', got 'Payload'" not in check_json_type_data[0].get("message", "")
    ):
        print("check_json_type: expected structured incompatible-type diagnostic")
        print(check_json_type.stdout)
        return 1
    print("ok check_json_type")

    check_json_type_cases = (
        (
            "check_json_proc_arg",
            r'''
take_ptr:proc(p:*i32)->void = {
    return;
}

main:proc()->i32 = {
    value:i32 = 1;
    take_ptr(value);
    return 0;
}
''',
            "proc 'take_ptr' argument 1 'p' expected 'ptr_i32', got 'i32'",
            "parameter declared here",
        ),
        (
            "check_json_proc_count",
            r'''
add:proc(a:i32, b:i32)->i32 = {
    return a + b;
}

main:proc()->i32 = {
    return add(1);
}
''',
            "proc 'add' expects 2 args, got 1",
            ("expected params: a:i32, b:i32", "proc declared here"),
        ),
        (
            "check_json_return_presence",
            r'''
main:proc()->i32 = {
    return;
}
''',
            "non-void proc must return a value of type 'i32'",
            "proc declared here",
        ),
        (
            "check_json_call_non_proc",
            r'''
main:proc()->i32 = {
    value:i32 = 1;
    return value(1);
}
''',
            "cannot call non-proc symbol 'value' of type 'i32'",
            "",
        ),
        (
            "check_json_proc_pointer_arg",
            r'''
Callback:alias = *proc(x:i32)->i32;

ok_cb:proc(x:i32)->i32 = {
    return x;
}

main:proc()->i32 = {
    value:i32 = 1;
    cb:Callback = ok_cb;
    return cb(value.&);
}
''',
            "proc pointer 'cb' argument 1 'x' expected 'i32', got 'ptr_i32'",
            "expected params: x:i32",
        ),
        (
            "check_json_proc_pointer_count",
            r'''
Callback:alias = *proc(a:i32, b:i32)->i32;

add:proc(a:i32, b:i32)->i32 = {
    return a + b;
}

main:proc()->i32 = {
    cb:Callback = add;
    return cb(1);
}
''',
            "proc pointer 'cb' expects 2 args, got 1",
            "expected params: a:i32, b:i32",
        ),
        (
            "check_json_cast",
            r'''
Payload:struct = {
    value:i32;
}

main:proc()->i32 = {
    payload:Payload = {};
    return cast(payload, i32);
}
''',
            "cannot cast 'Payload' to 'i32'",
            "",
        ),
        (
            "check_json_binary",
            r'''
Payload:struct = {
    value:i32;
}

main:proc()->i32 = {
    payload:Payload = {};
    return payload + 1;
}
''',
            "operator '+' cannot be applied to 'Payload' and 'i32'",
            "",
        ),
        (
            "check_json_field",
            r'''
Payload:struct = {
    value:i32;
}

main:proc()->i32 = {
    payload:Payload = {};
    return payload.missing;
}
''',
            "type 'Payload' has no field 'missing'",
            "",
        ),
        (
            "check_json_initializer_field",
            r'''
Payload:struct = {
    value:i32;
}

main:proc()->i32 = {
    payload:Payload = { .missing = 1 };
    return payload.value;
}
''',
            "initializer for type 'Payload' has no field 'missing'",
            "",
        ),
        (
            "check_json_const_assignment",
            r'''
main:proc()->i32 = {
    value:const i32 = 1;
    value = 2;
    return value;
}
''',
            "cannot assign to const target of type 'const_i32'",
            "",
        ),
        (
            "check_json_condition",
            r'''
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
''',
            "if condition must be scalar/pointer, got 'Payload'",
            "",
        ),
        (
            "check_json_assignment_target",
            r'''
make_value:proc()->i32 = {
    return 1;
}

main:proc()->i32 = {
    make_value() = 3;
    return 0;
}
''',
            "assignment target must be a name, field, or indexed element; got call",
            "",
        ),
        (
            "check_json_address_target",
            r'''
main:proc()->i32 = {
    value:*i32 = (1 + 2).&;
    return 0;
}
''',
            "address target must be a name, field, or indexed element; got binary expression",
            "",
        ),
        (
            "check_json_index_base",
            r'''
main:proc()->i32 = {
    value:i32 = 1;
    return value[0];
}
''',
            "cannot index non-array/non-pointer type 'i32'",
            "",
        ),
        (
            "check_json_index_value",
            r'''
main:proc()->i32 = {
    values:[2]i32 = {};
    index:*i32 = values[0].&;
    return values[index];
}
''',
            "index expression must be numeric, got 'ptr_i32'",
            "",
        ),
        (
            "check_json_initializer_duplicate_field",
            r'''
Payload:struct = {
    value:i32;
}

main:proc()->i32 = {
    payload:Payload = {.value = 1, .value = 2};
    return payload.value;
}
''',
            "duplicate initializer for field 'value'",
            "previous initializer here",
        ),
        (
            "check_json_initializer_count",
            r'''
Payload:struct = {
    value:i32;
}

main:proc()->i32 = {
    payload:Payload = {1, 2};
    return payload.value;
}
''',
            "too many positional initializer values for type 'Payload'",
            "",
        ),
        (
            "check_json_array_initializer_duplicate_index",
            r'''
main:proc()->i32 = {
    values:[2]i32 = {[1] = 1, [1] = 2};
    return values[0];
}
''',
            "duplicate initializer for array index '1'",
            "previous initializer here",
        ),
        (
            "check_json_array_initializer_index_bounds",
            r'''
main:proc()->i32 = {
    values:[2]i32 = {[2] = 1};
    return values[0];
}
''',
            "initializer index '2' is out of bounds for type 'array_2_i32'",
            "",
        ),
        (
            "check_json_array_initializer_float_index",
            r'''
main:proc()->i32 = {
    values:[2]i32 = {[1.0] = 1};
    return values[0];
}
''',
            "initializer index '1.0' must be a non-negative integer literal",
            "",
        ),
        (
            "check_json_pointer_value_note",
            r'''
main:proc()->i32 = {
    x:i32 = 0;
    p:*i32 = x.&;
    x = p;
    return x;
}
''',
            "assignment expected 'i32', got 'ptr_i32'",
            "got a pointer; use '[0]' to access the pointed value",
        ),
        (
            "check_json_array_pointer_note",
            r'''
take_i32s:proc(values:*i32)->void = {
    return;
}

main:proc()->i32 = {
    values:[4]f32 = {};
    take_i32s(values);
    return 0;
}
''',
            "proc 'take_i32s' argument 1 'values' expected 'ptr_i32', got 'array_4_f32'",
            "fixed array can decay to pointer only when element types match; expected element 'i32', got 'f32'",
        ),
        (
            "check_json_proc_signature_note",
            r'''
Callback:alias = *proc(x:i32)->i32;

bad_cb:proc(x:i32)->*i32 = {
    return null;
}

main:proc()->i32 = {
    cb:Callback = bad_cb;
    return 0;
}
''',
            "initializer expected 'Callback', got 'ptr_proc_ptr_i32_i32'",
            "expected proc signature: (arg0:i32)->i32",
        ),
    )
    for case_name, source, message, note_messages in check_json_type_cases:
        case_i = TEST_DIR / f"{case_name}.i"
        case_i.write_text(source.strip() + "\n", encoding="utf-8", newline="\n")
        result = run([str(I_EXE), "--check", str(case_i), "--diagnostics=json"])
        try:
            data = json.loads(result.stdout)
        except json.JSONDecodeError:
            print(f"{case_name}: expected JSON diagnostic")
            print(result.stdout)
            return 1
        notes = data[0].get("notes", []) if isinstance(data, list) and data else []
        if isinstance(note_messages, str):
            expected_notes = (note_messages,) if note_messages else ()
        else:
            expected_notes = note_messages
        if (
            result.returncode == 0
            or not isinstance(data, list)
            or not data
            or data[0].get("category") != "type"
            or data[0].get("file") != str(case_i)
            or message not in data[0].get("message", "")
            or any(
                not any(note_message in note.get("message", "") for note in notes if isinstance(note, dict))
                for note_message in expected_notes
            )
        ):
            print(f"{case_name}: expected structured type diagnostic")
            print(result.stdout)
            return 1
        print(f"ok {case_name}")

    check_json_lexer_i = TEST_DIR / "check_json_lexer.i"
    check_json_lexer_i.write_text(r'''
main:proc()->i32 = {
    return 0;
}
$
'''.strip() + "\n", encoding="utf-8", newline="\n")
    check_json_lexer = run([str(I_EXE), "--check", str(check_json_lexer_i), "--diagnostics=json"])
    try:
        check_json_lexer_data = json.loads(check_json_lexer.stdout)
    except json.JSONDecodeError:
        print("check_json_lexer: expected JSON lexer diagnostic")
        print(check_json_lexer.stdout)
        return 1
    if (
        check_json_lexer.returncode == 0
        or not isinstance(check_json_lexer_data, list)
        or not check_json_lexer_data
        or check_json_lexer_data[0].get("category") != "lexer"
        or check_json_lexer_data[0].get("file") != str(check_json_lexer_i)
        or "unexpected char '$'" not in check_json_lexer_data[0].get("message", "")
        or check_json_lexer_data[0].get("end_column") != check_json_lexer_data[0].get("column", 0) + 1
    ):
        print("check_json_lexer: expected structured lexer diagnostic")
        print(check_json_lexer.stdout)
        return 1
    print("ok check_json_lexer")

    check_json_format_i = TEST_DIR / "check_json_format.i"
    check_json_format_i.write_text(r'''
main:proc()->i32 = {
    fmt:*const char = "{}\n";
    printfmt(fmt, 1);
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    check_json_format = run([str(I_EXE), "--check", str(check_json_format_i), "--diagnostics=json"])
    try:
        check_json_format_data = json.loads(check_json_format.stdout)
    except json.JSONDecodeError:
        print("check_json_format: expected JSON format diagnostic")
        print(check_json_format.stdout)
        return 1
    if (
        check_json_format.returncode == 0
        or not isinstance(check_json_format_data, list)
        or not check_json_format_data
        or check_json_format_data[0].get("category") != "format"
        or check_json_format_data[0].get("file") != str(check_json_format_i)
        or "printfmt expects a string literal format" not in check_json_format_data[0].get("message", "")
    ):
        print("check_json_format: expected structured printfmt format diagnostic")
        print(check_json_format.stdout)
        return 1
    print("ok check_json_format")

    check_json_format_cases = (
        (
            "check_json_format_too_many_placeholders",
            r'''
main:proc()->i32 = {
    printfmt("{} {}\n", 1);
    return 0;
}
''',
            "printfmt placeholder count (2) does not match arg count (1)",
        ),
        (
            "check_json_format_count_mismatch",
            r'''
main:proc()->i32 = {
    printfmt("{}\n", 1, 2);
    return 0;
}
''',
            "printfmt placeholder count (1) does not match arg count (2)",
        ),
    )
    for case_name, source, message in check_json_format_cases:
        case_i = TEST_DIR / f"{case_name}.i"
        case_i.write_text(source.strip() + "\n", encoding="utf-8", newline="\n")
        result = run([str(I_EXE), "--check", str(case_i), "--diagnostics=json"])
        try:
            data = json.loads(result.stdout)
        except json.JSONDecodeError:
            print(f"{case_name}: expected JSON format diagnostic")
            print(result.stdout)
            return 1
        if (
            result.returncode == 0
            or not isinstance(data, list)
            or not data
            or data[0].get("category") != "format"
            or data[0].get("file") != str(case_i)
            or message not in data[0].get("message", "")
        ):
            print(f"{case_name}: expected structured format diagnostic")
            print(result.stdout)
            return 1
        print(f"ok {case_name}")

    check_json_requirement_i = TEST_DIR / "check_json_requirement.i"
    check_json_requirement_i.write_text(r'''
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
    check_json_requirement = run([str(I_EXE), "--check", str(check_json_requirement_i), "--diagnostics=json"])
    try:
        check_json_requirement_data = json.loads(check_json_requirement.stdout)
    except json.JSONDecodeError:
        print("check_json_requirement: expected JSON requirement diagnostic")
        print(check_json_requirement.stdout)
        return 1
    requirement_notes = check_json_requirement_data[0].get("notes", []) if isinstance(check_json_requirement_data, list) and check_json_requirement_data else []
    if (
        check_json_requirement.returncode == 0
        or not isinstance(check_json_requirement_data, list)
        or not check_json_requirement_data
        or check_json_requirement_data[0].get("category") != "requirement"
        or check_json_requirement_data[0].get("file") != str(check_json_requirement_i)
        or "requires 'hashable' for type 'Payload'" not in check_json_requirement_data[0].get("message", "")
        or "missing function 'hash_Payload'" not in check_json_requirement_data[0].get("message", "")
        or not any("generic 'need_hash' instantiated here with type 'Payload'" in note.get("message", "") for note in requirement_notes if isinstance(note, dict))
        or not any("generic declared here with requirement 'hashable'" in note.get("message", "") for note in requirement_notes if isinstance(note, dict))
    ):
        print("check_json_requirement: expected structured requirement diagnostic")
        print(check_json_requirement.stdout)
        return 1
    print("ok check_json_requirement")

    check_json_import_root_i = TEST_DIR / "check_json_import_cycle_root.i"
    check_json_import_a_i = TEST_DIR / "check_json_import_cycle_a.i"
    check_json_import_b_i = TEST_DIR / "check_json_import_cycle_b.i"
    check_json_import_root_i.write_text(f'import "{check_json_import_a_i.as_posix()}"\n', encoding="utf-8", newline="\n")
    check_json_import_a_i.write_text(f'import "{check_json_import_b_i.as_posix()}"\n', encoding="utf-8", newline="\n")
    check_json_import_b_i.write_text(f'import "{check_json_import_a_i.as_posix()}"\n', encoding="utf-8", newline="\n")
    check_json_import = run([str(I_EXE), "--check", str(check_json_import_root_i), "--diagnostics=json"])
    try:
        check_json_import_data = json.loads(check_json_import.stdout)
    except json.JSONDecodeError:
        print("check_json_import_cycle: expected JSON import diagnostic")
        print(check_json_import.stdout)
        return 1
    import_cycle_notes = check_json_import_data[0].get("notes", []) if isinstance(check_json_import_data, list) and check_json_import_data else []
    if (
        check_json_import.returncode == 0
        or not isinstance(check_json_import_data, list)
        or not check_json_import_data
        or check_json_import_data[0].get("category") != "semantic"
        or check_json_import_data[0].get("file") != str(check_json_import_b_i)
        or check_json_import_data[0].get("line") != 1
        or check_json_import_data[0].get("column") != 8
        or "import cycle:" not in check_json_import_data[0].get("message", "")
        or check_json_import_a_i.name not in check_json_import_data[0].get("message", "")
        or check_json_import_b_i.name not in check_json_import_data[0].get("message", "")
        or not any(
            "imported through:" in note.get("message", "")
            and check_json_import_root_i.name in note.get("message", "")
            and check_json_import_b_i.name in note.get("message", "")
            for note in import_cycle_notes
            if isinstance(note, dict)
        )
    ):
        print("check_json_import_cycle: expected structured import cycle diagnostic at the closing import with import-chain note")
        print(check_json_import.stdout)
        return 1
    print("ok check_json_import_cycle")

    check_json_missing_import_root_i = TEST_DIR / "check_json_missing_import_root.i"
    check_json_missing_import_dep_i = TEST_DIR / "check_json_missing_import_dep.i"
    if check_json_missing_import_dep_i.exists():
        check_json_missing_import_dep_i.unlink()
    check_json_missing_import_root_i.write_text(
        f'''
import "{check_json_missing_import_dep_i.as_posix()}"

main:proc()->i32 = {{
    return 0;
}}
'''.strip() + "\n",
        encoding="utf-8",
        newline="\n",
    )
    check_json_missing_import = run([str(I_EXE), "--check", str(check_json_missing_import_root_i), "--diagnostics=json"])
    try:
        check_json_missing_import_data = json.loads(check_json_missing_import.stdout)
    except json.JSONDecodeError:
        print("check_json_missing_import: expected JSON missing-import diagnostic")
        print(check_json_missing_import.stdout)
        return 1
    missing_import_notes = check_json_missing_import_data[0].get("notes", []) if isinstance(check_json_missing_import_data, list) and check_json_missing_import_data else []
    if (
        check_json_missing_import.returncode == 0
        or not isinstance(check_json_missing_import_data, list)
        or not check_json_missing_import_data
        or check_json_missing_import_data[0].get("category") != "semantic"
        or check_json_missing_import_data[0].get("file") != str(check_json_missing_import_root_i)
        or "failed to read import" not in check_json_missing_import_data[0].get("message", "")
        or check_json_missing_import_dep_i.name not in check_json_missing_import_data[0].get("message", "")
        or not any("imported through:" in note.get("message", "") for note in missing_import_notes if isinstance(note, dict))
    ):
        print("check_json_missing_import: expected structured missing-import diagnostic with import-chain note")
        print(check_json_missing_import.stdout)
        return 1
    print("ok check_json_missing_import")

    check_json_import_dup_mod = TEST_DIR / "check_json_import_duplicate_mod.i"
    check_json_import_dup_mid = TEST_DIR / "check_json_import_duplicate_mid.i"
    check_json_import_dup_app = TEST_DIR / "check_json_import_duplicate_app.i"
    check_json_import_dup_mod.write_text(r'''
Payload:struct = {
    value:i32;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    check_json_import_dup_mid.write_text(r'''
import "check_json_import_duplicate_mod.i"
'''.strip() + "\n", encoding="utf-8", newline="\n")
    check_json_import_dup_app.write_text(r'''
import "check_json_import_duplicate_mid.i"

Payload:struct = {
    other:i32;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    check_json_import_dup = run([str(I_EXE), "--check", str(check_json_import_dup_app), "--diagnostics=json"])
    try:
        check_json_import_dup_data = json.loads(check_json_import_dup.stdout)
    except json.JSONDecodeError:
        print("check_json_import_duplicate: expected JSON import duplicate diagnostic")
        print(check_json_import_dup.stdout)
        return 1
    import_dup_notes = check_json_import_dup_data[0].get("notes", []) if isinstance(check_json_import_dup_data, list) and check_json_import_dup_data else []
    if (
        check_json_import_dup.returncode == 0
        or not isinstance(check_json_import_dup_data, list)
        or not check_json_import_dup_data
        or check_json_import_dup_data[0].get("category") != "semantic"
        or check_json_import_dup_data[0].get("file") != str(check_json_import_dup_app)
        or "duplicate struct declaration 'Payload'" not in check_json_import_dup_data[0].get("message", "")
        or str(check_json_import_dup_mod) not in check_json_import_dup_data[0].get("message", "")
        or not any("previous declaration imported through:" in note.get("message", "") for note in import_dup_notes if isinstance(note, dict))
        or not any(check_json_import_dup_mid.name in note.get("message", "") for note in import_dup_notes if isinstance(note, dict))
    ):
        print("check_json_import_duplicate: expected structured import duplicate diagnostic with previous import-chain note")
        print(check_json_import_dup.stdout)
        return 1
    print("ok check_json_import_duplicate")

    check_json_import_value_dup_mod = TEST_DIR / "check_json_import_value_duplicate_mod.i"
    check_json_import_value_dup_mid = TEST_DIR / "check_json_import_value_duplicate_mid.i"
    check_json_import_value_dup_app = TEST_DIR / "check_json_import_value_duplicate_app.i"
    check_json_import_value_dup_mod.write_text(r'''
shared_value:proc()->i32 = {
    return 1;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    check_json_import_value_dup_mid.write_text(r'''
import "check_json_import_value_duplicate_mod.i"
'''.strip() + "\n", encoding="utf-8", newline="\n")
    check_json_import_value_dup_app.write_text(r'''
import "check_json_import_value_duplicate_mid.i"

shared_value:i32 = 2;
'''.strip() + "\n", encoding="utf-8", newline="\n")
    check_json_import_value_dup = run([str(I_EXE), "--check", str(check_json_import_value_dup_app), "--diagnostics=json"])
    try:
        check_json_import_value_dup_data = json.loads(check_json_import_value_dup.stdout)
    except json.JSONDecodeError:
        print("check_json_import_value_duplicate: expected JSON import value duplicate diagnostic")
        print(check_json_import_value_dup.stdout)
        return 1
    import_value_dup_notes = check_json_import_value_dup_data[0].get("notes", []) if isinstance(check_json_import_value_dup_data, list) and check_json_import_value_dup_data else []
    if (
        check_json_import_value_dup.returncode == 0
        or not isinstance(check_json_import_value_dup_data, list)
        or not check_json_import_value_dup_data
        or check_json_import_value_dup_data[0].get("category") != "semantic"
        or check_json_import_value_dup_data[0].get("file") != str(check_json_import_value_dup_app)
        or "duplicate global declaration 'shared_value'" not in check_json_import_value_dup_data[0].get("message", "")
        or str(check_json_import_value_dup_mod) not in check_json_import_value_dup_data[0].get("message", "")
        or not any("previous declaration imported through:" in note.get("message", "") for note in import_value_dup_notes if isinstance(note, dict))
        or not any(check_json_import_value_dup_mid.name in note.get("message", "") for note in import_value_dup_notes if isinstance(note, dict))
    ):
        print("check_json_import_value_duplicate: expected structured import value duplicate diagnostic with previous import-chain note")
        print(check_json_import_value_dup.stdout)
        return 1
    print("ok check_json_import_value_duplicate")

    check_json_semantic_cases = (
        (
            "check_json_sizeof_arg_count",
            r'''
main:proc()->i32 = {
    value:i32 = sizeof(1, 2);
    return value;
}
''',
            "sizeof expects exactly 1 argument",
            "",
        ),
        (
            "check_json_alignof_arg_count",
            r'''
main:proc()->i32 = {
    value:i32 = alignof(1, 2);
    return value;
}
''',
            "alignof expects exactly 1 argument",
            "",
        ),
        (
            "check_json_undeclared_type",
            r'''
main:proc()->i32 = {
    value:Missing = {};
    return 0;
}
''',
            "use of undeclared type 'Missing'",
            "",
        ),
        (
            "check_json_undeclared_generic_type",
            r'''
main:proc()->i32 = {
    box:Box<i32> = {};
    return 0;
}
''',
            "use of undeclared generic type 'Box'",
            "",
        ),
        (
            "check_json_duplicate_global",
            r'''
value:i32 = 1;
value:i32 = 2;

main:proc()->i32 = {
    return value;
}
''',
            "duplicate global declaration 'value'",
            "previous declaration here",
        ),
        (
            "check_json_duplicate_type_alias",
            r'''
Payload:alias = i32;
Payload:alias = i32;
''',
            "duplicate type alias 'Payload'",
            "previous declaration here",
        ),
        (
            "check_json_duplicate_struct",
            r'''
Payload:struct = {
    value:i32;
}

Payload:struct = {
    other:i32;
}
''',
            "duplicate struct declaration 'Payload'",
            "previous declaration here",
        ),
        (
            "check_json_duplicate_enum",
            r'''
Kind:enum = {
    Ready,
}

Kind:enum = {
    Done,
}
''',
            "duplicate enum declaration 'Kind'",
            "previous declaration here",
        ),
        (
            "check_json_generated_struct_reflect_collision",
            r'''
define("Payload_reflect")

Payload:struct = {
    value:i32;
}
''',
            "duplicate generated global declaration 'Payload_reflect'",
            "previous declaration here",
        ),
        (
            "check_json_generated_enum_value_collision",
            r'''
define("Kind_Ready")

Kind:enum = {
    Ready,
}
''',
            "duplicate generated global declaration 'Kind_Ready'",
            "previous declaration here",
        ),
        (
            "check_json_duplicate_proc_param",
            r'''
main:proc(value:i32, value:i32)->i32 = {
    return value;
}
''',
            "duplicate proc parameter 'value'",
            "previous declaration here",
        ),
        (
            "check_json_duplicate_local",
            r'''
main:proc()->i32 = {
    value:i32 = 1;
    value:i32 = 2;
    return value;
}
''',
            "duplicate local declaration 'value'",
            "previous declaration here",
        ),
        (
            "check_json_duplicate_field",
            r'''
Payload:struct = {
    value:i32;
    value:i32;
}

main:proc()->i32 = {
    return 0;
}
''',
            "duplicate field 'value'",
            "previous declaration here",
        ),
        (
            "check_json_duplicate_enum_item",
            r'''
Kind:enum = {
    Ready,
    Ready,
}

main:proc()->i32 = {
    return 0;
}
''',
            "duplicate enum item 'Ready'",
            "previous declaration here",
        ),
        (
            "check_json_duplicate_proc",
            r'''
value:proc()->i32 = {
    return 1;
}

value:proc()->i32 = {
    return 2;
}
''',
            "duplicate proc declaration 'value'",
            "previous declaration here",
        ),
        (
            "check_json_control_flow",
            r'''
main:proc()->i32 = {
    break;
    return 0;
}
''',
            "break outside loop or switch",
            "",
        ),
        (
            "check_json_generic_type_arity",
            r'''
Array:struct<T> = {
    data:*T;
}

main:proc()->i32 = {
    a:Array<i32, f32> = {};
    return 0;
}
''',
            "generic type 'Array' expects 1 type arg, got 2",
            "struct 'Array' declared here",
        ),
        (
            "check_json_nongeneric_type_arg",
            r'''
Payload:struct = {
    value:i32;
}

main:proc()->i32 = {
    payload:Payload<i32> = {};
    return payload.value;
}
''',
            "type 'Payload' is not generic; got 1 type arg",
            "struct 'Payload' declared here",
        ),
    )
    for case_name, source, message, note_message in check_json_semantic_cases:
        case_i = TEST_DIR / f"{case_name}.i"
        case_i.write_text(source.strip() + "\n", encoding="utf-8", newline="\n")
        result = run([str(I_EXE), "--check", str(case_i), "--diagnostics=json"])
        try:
            data = json.loads(result.stdout)
        except json.JSONDecodeError:
            print(f"{case_name}: expected JSON semantic diagnostic")
            print(result.stdout)
            return 1
        notes = data[0].get("notes", []) if isinstance(data, list) and data else []
        if (
            result.returncode == 0
            or not isinstance(data, list)
            or not data
            or data[0].get("category") != "semantic"
            or data[0].get("file") != str(case_i)
            or message not in data[0].get("message", "")
            or (note_message and not any(note_message in note.get("message", "") for note in notes if isinstance(note, dict)))
        ):
            print(f"{case_name}: expected structured semantic diagnostic")
            print(result.stdout)
            return 1
        print(f"ok {case_name}")

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
    line_map_comment_path = str(line_map_i)
    expected_source_banner = f"/* Generated by I from {line_map_comment_path} (source). Do not edit. */\n"
    if not line_map_generated.startswith(expected_source_banner):
        print("generated_line_map: expected source banner with originating .i path")
        print(f"missing: {expected_source_banner.strip()}")
        return 1
    # #line directives are only emitted where the implied position would drift, so
    # assert the statement maps back to the right .i line instead of assuming a
    # directive sits immediately above it.
    mapped = c_line_mapping(line_map_generated)
    return_sites = [(f, l) for text, f, l in mapped if text == "return value;"]
    if (str(line_map_i), return_line) not in return_sites:
        print("generated_line_map: 'return value;' does not map back to its .i line")
        print(f"expected: ({line_map_i}, {return_line}), got: {return_sites}")
        return 1
    generated_reflect_include = "#include <reflect.h>"
    if generated_reflect_include not in line_map_generated:
        print("generated_line_map: expected generated source to include reflect runtime header")
        return 1
    if "I_REFLECT_TYPES_DEFINED" in line_map_generated:
        print("generated_line_map: reflection runtime helpers should live in std/reflect.h, not generated source")
        return 1
    generated_struct_reflect_marker = '#line 1 "<generated>"\nstatic const i_reflect_field i_reflect_fields_Box_i32'
    if generated_struct_reflect_marker not in line_map_generated:
        print("generated_line_map: expected struct reflection metadata to be marked as generated code")
        return 1
    if "I monomorph: struct Box<T> -> Box_i32;" not in line_map_generated:
        print("generated_line_map: expected monomorphized struct comment")
        return 1
    if not line_map_h.exists():
        print("generated_line_map: expected generated header")
        return 1
    line_map_header = line_map_h.read_text(encoding="utf-8")
    expected_header_banner = f"/* Generated by I from {line_map_comment_path} (header). Do not edit. */\n"
    if not line_map_header.startswith(expected_header_banner):
        print("generated_line_map: expected header banner with originating .i path")
        print(f"missing: {expected_header_banner.strip()}")
        return 1
    if generated_reflect_include not in line_map_header:
        print("generated_line_map: expected generated header to include reflect runtime header")
        return 1
    if "I_REFLECT_TYPES_DEFINED" in line_map_header:
        print("generated_line_map: reflection runtime helpers should live in std/reflect.h, not generated header")
        return 1
    generated_reflect_extern_marker = '#line 1 "<generated>"\nextern const i_reflect_type Box_i32_reflect;'
    if generated_reflect_extern_marker not in line_map_header:
        print("generated_line_map: expected reflection externs to be marked as generated code")
        return 1
    if "I monomorph: struct Box<T> -> Box_i32;" not in line_map_header:
        print("generated_line_map: expected header monomorphized struct comment")
        return 1
    proc_line = line_map_source.splitlines().index("Box<T>get:proc<T>(box:Box<T>)->T = {") + 1
    expected_proc_line = f'#line {proc_line} "{line_map_path}"'
    if expected_proc_line not in line_map_header:
        print("generated_line_map: expected proc prototype #line directive in header")
        print(f"missing: {expected_proc_line}")
        return 1
    mono_return_line = line_map_source.splitlines().index("    return box.value;") + 1
    mono_sites = [(f, l) for text, f, l in mapped if text == "return box.value;"]
    if (str(line_map_i), mono_return_line) not in mono_sites:
        print("generated_line_map: monomorphized body does not map back to its .i line")
        print(f"expected: ({line_map_i}, {mono_return_line}), got: {mono_sites}")
        return 1
    if (
        "I monomorph: proc Box<T>get -> Box_i32_get;" not in line_map_generated
        or "instantiated at" not in line_map_generated
    ):
        print("generated_line_map: expected monomorphized proc instantiation comment")
        return 1
    print("ok generated_line_map")

    line_map_mono_param_i = TEST_DIR / "generated_line_map_mono_param_error.i"
    line_map_mono_param_c = TEST_DIR / "generated_line_map_mono_param_error.c"
    line_map_mono_param_source = r'''
bad_generic:proc<T>(
    ok:T,
    bad:MISSING_C_MONO_PARAM_TYPE
)->i32 = { external_emit; }

main:proc()->i32 = {
    return bad_generic<i32>(1, cast(null, MISSING_C_MONO_PARAM_TYPE));
}
'''.strip() + "\n"
    line_map_mono_param_i.write_text(line_map_mono_param_source, encoding="utf-8", newline="\n")
    line_map_mono_param = run([str(I_EXE), str(line_map_mono_param_i), str(line_map_mono_param_c)])
    if line_map_mono_param.returncode != 0:
        print(line_map_mono_param.stdout)
        return line_map_mono_param.returncode
    line_map_mono_param_compile = run([
        "clang.exe",
        str(line_map_mono_param_c),
        "-I",
        "src",
        "-I",
        "src/std",
        "-o",
        str(TEST_DIR / "generated_line_map_mono_param_error.exe"),
    ])
    bad_mono_param_line = line_map_mono_param_source.splitlines().index("    bad:MISSING_C_MONO_PARAM_TYPE") + 1
    if (
        line_map_mono_param_compile.returncode == 0
        or str(line_map_mono_param_i) not in line_map_mono_param_compile.stdout
        or f":{bad_mono_param_line}:" not in line_map_mono_param_compile.stdout
        or "MISSING_C_MONO_PARAM_TYPE" not in line_map_mono_param_compile.stdout
    ):
        print("generated_line_map_mono_param_error: expected clang diagnostic to map monomorphized generic param error back to exact .i parameter line")
        print(line_map_mono_param_compile.stdout)
        return 1
    print("ok generated_line_map_mono_param_error")

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
        "src/std",
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
        "src/std",
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
        "src/std",
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

    line_map_import_mod_i = TEST_DIR / "generated_line_map_import_mod.i"
    line_map_import_app_i = TEST_DIR / "generated_line_map_import_app.i"
    line_map_import_app_c = TEST_DIR / "generated_line_map_import_app.c"
    line_map_import_mod_source = r'''
ImportedPayload:struct = {
    ok:i32;
    bad:MISSING_IMPORTED_FIELD_TYPE;
}
'''.strip() + "\n"
    line_map_import_app_source = f'''
import "{line_map_import_mod_i.name}"

main:proc()->i32 = {{
    return 0;
}}
'''.strip() + "\n"
    line_map_import_mod_i.write_text(line_map_import_mod_source, encoding="utf-8", newline="\n")
    line_map_import_app_i.write_text(line_map_import_app_source, encoding="utf-8", newline="\n")
    line_map_import = run([str(I_EXE), str(line_map_import_app_i), str(line_map_import_app_c)])
    if line_map_import.returncode != 0:
        print(line_map_import.stdout)
        return line_map_import.returncode
    line_map_import_compile = run([
        "clang.exe",
        str(line_map_import_app_c),
        "-I",
        "src",
        "-I",
        "src/std",
        "-o",
        str(TEST_DIR / "generated_line_map_import_app.exe"),
    ])
    imported_bad_field_line = line_map_import_mod_source.splitlines().index("    bad:MISSING_IMPORTED_FIELD_TYPE;") + 1
    if (
        line_map_import_compile.returncode == 0
        or str(line_map_import_mod_i) not in line_map_import_compile.stdout
        or f":{imported_bad_field_line}:" not in line_map_import_compile.stdout
        or "MISSING_IMPORTED_FIELD_TYPE" not in line_map_import_compile.stdout
    ):
        print("generated_line_map_import_error: expected clang diagnostic to map imported generated C error back to imported .i line")
        print(line_map_import_compile.stdout)
        return 1
    print("ok generated_line_map_import_error")

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
        "src/std",
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
    diamond_shared_dot_path = f"{TEST_DIR.as_posix()}/./diamond_shared.i"
    diamond_shared_i.write_text(r'''
cinclude "stdio.h"
#define DIAMOND_SHARED_FLAG 1

DiamondPayload:struct = {
    value:i32;
}

diamond_value:proc(p:DiamondPayload)->i32 = {
    return p.value;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    diamond_left_i.write_text(f'''
cinclude "stdio.h"
#define DIAMOND_SHARED_FLAG 1
import "{diamond_shared_i.as_posix()}"

diamond_left:proc(p:DiamondPayload)->i32 = {{
    return diamond_value(p) + 1;
}}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    diamond_right_i.write_text(f'''
cinclude "stdio.h"
#define DIAMOND_SHARED_FLAG 1
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
import "{diamond_shared_dot_path}"

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
    if diamond_generated.count('#include "stdio.h"') != 1 or diamond_generated.count("#define DIAMOND_SHARED_FLAG 1") != 1:
        print("module_diamond_import: expected duplicate imported cincludes and macros to be emitted once")
        return 1
    diamond_compile = run([
        "clang.exe",
        str(diamond_app_c),
        "-I",
        str(TEST_DIR),
        "-I",
        "src",
        "-I",
        "src/std",
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

    diamond_rev_app_i = TEST_DIR / "diamond_rev_app.i"
    diamond_rev_app_c = TEST_DIR / "diamond_rev_app.c"
    diamond_rev_app_exe = TEST_DIR / "diamond_rev_app.exe"
    diamond_rev_app_i.write_text(f'''
cinclude "stdio.h"
import "{diamond_right_i.as_posix()}"
import "{diamond_left_i.as_posix()}"
import "{diamond_shared_i.as_posix()}"

main:proc()->i32 = {{
    p:DiamondPayload = {{.value = 5}};
    printf("%d %d %d\\n", diamond_value(p), diamond_right(p), diamond_left(p));
    return 0;
}}
'''.strip() + "\n", encoding="utf-8", newline="\n")

    diamond_rev = run([str(I_EXE), str(diamond_rev_app_i), str(diamond_rev_app_c)])
    if diamond_rev.returncode != 0:
        print(diamond_rev.stdout)
        return diamond_rev.returncode
    diamond_rev_generated = diamond_rev_app_c.read_text(encoding="utf-8")
    if diamond_rev_generated.count("structdef(DiamondPayload)") != 1 or diamond_rev_generated.count("i32 diamond_value(") != 2:
        print("module_diamond_import: reversed import order should still dedupe shared module")
        return 1
    if diamond_rev_generated.count('#include "stdio.h"') != 1 or diamond_rev_generated.count("#define DIAMOND_SHARED_FLAG 1") != 1:
        print("module_diamond_import: reversed import order should still dedupe imported cincludes and macros")
        return 1
    payload_pos = diamond_rev_generated.find("structdef(DiamondPayload)")
    right_pos = diamond_rev_generated.find("i32 diamond_right(")
    left_pos = diamond_rev_generated.find("i32 diamond_left(")
    main_pos = diamond_rev_generated.find("i32 main(")
    if payload_pos < 0 or right_pos < 0 or left_pos < 0 or main_pos < 0 or not (payload_pos < right_pos < left_pos < main_pos):
        print("module_diamond_import: expected deterministic dependency-first import order")
        return 1
    diamond_rev_compile = run([
        "clang.exe",
        str(diamond_rev_app_c),
        "-I",
        str(TEST_DIR),
        "-I",
        "src",
        "-I",
        "src/std",
        "-o",
        str(diamond_rev_app_exe),
    ])
    if diamond_rev_compile.returncode != 0:
        print(diamond_rev_compile.stdout)
        return diamond_rev_compile.returncode
    diamond_rev_program = run([str(diamond_rev_app_exe)])
    if diamond_rev_program.returncode != 0 or diamond_rev_program.stdout != "5 7 6\n":
        print("module_diamond_import: reversed import stdout mismatch")
        print(diamond_rev_program.stdout)
        return 1
    print("ok module_diamond_import")

    native_i = TEST_DIR / "native_monomorph.i"
    native_c = TEST_DIR / "native_monomorph.c"
    native_h = TEST_DIR / "native_monomorph.h"
    native_exe = TEST_DIR / "native_monomorph.exe"
    native_i.write_text(r'''
cinclude "stdio.h"
import "C:/devel/i/src/std/containers.i"

NativeBox:struct<T> = {
    value:T;
    external;
}

main:proc()->i32 = {
    arena:memops_arena = {};
    memops_arena_initialize(arena.&);
    values:Array<i32> = Array<i32>reserve(arena.&, 3);
    values.data[0] = 4;
    values.data[1] = 5;
    values.data[2] = 6;
    box:NativeBox<i32> = {};
    box.value = values.data[0] + values.data[1] + values.data[2];
    printf("%llu %d\n", values.length, box.value);
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")

    for stale in (TEST_DIR / "NativeBox.h", TEST_DIR / "NativeBox_i32.h"):
        if stale.exists():
            stale.unlink()

    translate = run([str(I_EXE), str(native_i), str(native_c)])
    if translate.returncode != 0:
        print(translate.stdout)
        return translate.returncode

    native_box_header = TEST_DIR / "NativeBox.h"
    native_box_i32_header = TEST_DIR / "NativeBox_i32.h"
    if not native_h.exists() or not native_box_header.exists() or not native_box_i32_header.exists():
        print("native_monomorph: generated native headers missing")
        return 1
    if '#include "NativeBox_i32.h"' not in native_box_header.read_text(encoding="utf-8"):
        print("native_monomorph: umbrella header missing NativeBox_i32 include")
        return 1
    native_box_i32_text = native_box_i32_header.read_text(encoding="utf-8")
    if "structdef(NativeBox_i32)" not in native_box_i32_text or "i32 value;" not in native_box_i32_text:
        print("native_monomorph: concrete header missing external struct")
        return 1

    compile_result = run([
        "clang.exe",
        str(native_c),
        "-I",
        str(TEST_DIR),
        "-I",
        "src",
        "-I",
        "src/std",
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

    native_json_dir = TEST_DIR / "native_monomorph_json_out"
    if native_json_dir.exists():
        shutil.rmtree(native_json_dir)
    native_json_dir.mkdir(parents=True)
    native_json_i = TEST_DIR / "native_monomorph_json.i"
    native_json_c = native_json_dir / "native_monomorph_json.c"
    native_json_i.write_text(r'''
import "C:/devel/i/src/std/containers.i"

NativeBox:struct<T> = {
    value:T;
    external;
}

main:proc()->i32 = {
    arena:memops_arena = {};
    value:NativeBox<i32> = {};
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    native_json_blocker = native_json_dir / "NativeBox_i32.h"
    native_json_blocker.mkdir()
    native_json = run([str(I_EXE), str(native_json_i), str(native_json_c), "--diagnostics=json"])
    try:
        native_json_data = json.loads(native_json.stdout)
    except json.JSONDecodeError:
        print("check_json_native_monomorph_write: expected JSON I/O diagnostic")
        print(native_json.stdout)
        return 1
    if (
        native_json.returncode == 0
        or not isinstance(native_json_data, list)
        or not native_json_data
        or native_json_data[0].get("category") != "io"
        or native_json_data[0].get("file") != str(native_json_blocker)
        or "failed to write" not in native_json_data[0].get("message", "")
    ):
        print("check_json_native_monomorph_write: expected structured native monomorph write diagnostic")
        print(native_json.stdout)
        return 1
    print("ok check_json_native_monomorph_write")

    missing_i = TEST_DIR / "missing_decl.i"
    missing_c = TEST_DIR / "missing_decl.c"
    missing_i.write_text(r'''
main:proc()->i32 = {
    return missing_symbol;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    missing = run([str(I_EXE), str(missing_i), str(missing_c)])
    if (
        missing.returncode == 0
        or "use of undeclared identifier 'missing_symbol'" not in missing.stdout
        or "    return missing_symbol;" not in missing.stdout
        or "           ^" not in missing.stdout
        or "^~~~~~~~~~~~~~" not in missing.stdout
    ):
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

    missing_import_i = TEST_DIR / "missing_import.i"
    missing_import_c = TEST_DIR / "missing_import.c"
    missing_import_i.write_text(r'''
import "missing_import_dep.i"

main:proc()->i32 = {
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    missing_import = run([str(I_EXE), str(missing_import_i), str(missing_import_c)])
    if (
        missing_import.returncode == 0
        or "semantic error: failed to read import" not in missing_import.stdout
        or "missing_import_dep.i" not in missing_import.stdout
        or "note: imported through:" not in missing_import.stdout
        or str(missing_import_i) not in missing_import.stdout
        or 'import "missing_import_dep.i"' not in missing_import.stdout
        or "^" not in missing_import.stdout
    ):
        print("missing_import: expected failed import diagnostic to include import chain")
        print(missing_import.stdout)
        return 1
    print("ok missing_import")

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
        or "    value i32;" not in parse_error.stdout
        or "          ^" not in parse_error.stdout
        or "^~~" not in parse_error.stdout
    ):
        print("parse_expected_actual: expected rich parser diagnostic")
        print(parse_error.stdout)
        return 1
    print("ok parse_expected_actual")

    import_parse_bad_i = TEST_DIR / "import_parse_bad.i"
    import_parse_app_i = TEST_DIR / "import_parse_app.i"
    import_parse_app_c = TEST_DIR / "import_parse_app.c"
    import_parse_bad_i.write_text(r'''
Bad:struct = {
    value i32;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    import_parse_app_i.write_text(r'''
import "import_parse_bad.i"

main:proc()->i32 = {
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    import_parse = run([str(I_EXE), str(import_parse_app_i), str(import_parse_app_c)])
    if (
        import_parse.returncode == 0
        or str(import_parse_bad_i) not in import_parse.stdout
        or "expected ':' after field name" not in import_parse.stdout
        or "note: imported through:" not in import_parse.stdout
        or str(import_parse_app_i) not in import_parse.stdout
        or "import_parse_bad.i" not in import_parse.stdout
    ):
        print("import_parse_diagnostic: expected imported parse error to include import chain")
        print(import_parse.stdout)
        return 1
    print("ok import_parse_diagnostic")

    import_c_header_bad_i = TEST_DIR / "import_c_header_bad.i"
    import_c_header_app_i = TEST_DIR / "import_c_header_app.i"
    import_c_header_app_c = TEST_DIR / "import_c_header_app.c"
    import_c_header_bad_i.write_text(r'''
import "stdio.h"
'''.strip() + "\n", encoding="utf-8", newline="\n")
    import_c_header_app_i.write_text(r'''
import "import_c_header_bad.i"

main:proc()->i32 = {
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    import_c_header = run([str(I_EXE), str(import_c_header_app_i), str(import_c_header_app_c)])
    if (
        import_c_header.returncode == 0
        or str(import_c_header_bad_i) not in import_c_header.stdout
        or "parse error: import expects an .i module; use cinclude for C headers" not in import_c_header.stdout
        or 'got string `"stdio.h"`' not in import_c_header.stdout
        or '    import "stdio.h"' not in import_c_header.stdout
        or "           ^" not in import_c_header.stdout
        or "note: imported through:" not in import_c_header.stdout
        or str(import_c_header_app_i) not in import_c_header.stdout
    ):
        print("import_c_header_diagnostic: expected imported C-header import error to include token context and import chain")
        print(import_c_header.stdout)
        return 1
    print("ok import_c_header_diagnostic")

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
        or "    value:i32 = ;" not in parse_expected_expr.stdout
        or "                ^" not in parse_expected_expr.stdout
    ):
        print("parse_expected_expression: expected expression diagnostic with actual token")
        print(parse_expected_expr.stdout)
        return 1
    print("ok parse_expected_expression")

    parse_eof_context_i = TEST_DIR / "parse_eof_context.i"
    parse_eof_context_c = TEST_DIR / "parse_eof_context.c"
    parse_eof_context_i.write_text(r'''
main:proc()->i32 = {
    return 0;
'''.strip() + "\n", encoding="utf-8", newline="\n")
    parse_eof_context = run([str(I_EXE), str(parse_eof_context_i), str(parse_eof_context_c)])
    if (
        parse_eof_context.returncode == 0
        or "parse error:" not in parse_eof_context.stdout
        or "got end of file" not in parse_eof_context.stdout
        or "    return 0;" not in parse_eof_context.stdout
        or "^" not in parse_eof_context.stdout
    ):
        print("parse_eof_context: expected EOF diagnostic to point at last source line")
        print(parse_eof_context.stdout)
        return 1
    print("ok parse_eof_context")

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
        or "parse error: expected statement: local declaration, assignment, expression, if, for, while, do, switch, break, continue, or return" not in parse_unexpected_stmt.stdout
        or "got 'case' `case`" not in parse_unexpected_stmt.stdout
        or "    case 1:" not in parse_unexpected_stmt.stdout
        or "    ^" not in parse_unexpected_stmt.stdout
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
        case 1: {
            break;
        }
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
        case 1: {
            continue;
        }
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
        or generic_proc_extra_type_arg.stdout.count("    return identity<i32, f32>(1);") != 1
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

    generic_proc_arg_mismatch_i = TEST_DIR / "generic_proc_arg_mismatch.i"
    generic_proc_arg_mismatch_c = TEST_DIR / "generic_proc_arg_mismatch.c"
    generic_proc_arg_mismatch_i.write_text(r'''
Payload:struct = {
    value:i32;
}

identity:proc<T>(value:T, other:T)->T = {
    return value;
}

main:proc()->i32 = {
    payload:Payload = {};
    return identity<i32>(1, payload);
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    generic_proc_arg_mismatch = run([str(I_EXE), str(generic_proc_arg_mismatch_i), str(generic_proc_arg_mismatch_c)])
    if (
        generic_proc_arg_mismatch.returncode == 0
        or str(generic_proc_arg_mismatch_i) not in generic_proc_arg_mismatch.stdout
        or "type error: proc 'identity' argument 2 'other' expected 'i32', got 'Payload'" not in generic_proc_arg_mismatch.stdout
        or "note: generic 'identity' instantiated here with type 'i32'" not in generic_proc_arg_mismatch.stdout
        or f"{generic_proc_arg_mismatch_i}:5:1: note: proc 'identity' declared here" not in generic_proc_arg_mismatch.stdout
        or "    return identity<i32>(1, payload);" not in generic_proc_arg_mismatch.stdout
    ):
        print("generic_proc_arg_mismatch: expected concrete generic argument mismatch diagnostic")
        print(generic_proc_arg_mismatch.stdout)
        return 1
    print("ok generic_proc_arg_mismatch")

    generic_delayed_invalid_instance_i = TEST_DIR / "generic_delayed_invalid_instance.i"
    generic_delayed_invalid_instance_c = TEST_DIR / "generic_delayed_invalid_instance.c"
    generic_delayed_invalid_instance_i.write_text(r'''
Payload:struct = {
    value:i32;
}

add:proc<T>(x:T, y:T)->T = {
    return x + y;
}

main:proc()->i32 = {
    payload:Payload = {};
    result:Payload = add<Payload>(payload, payload);
    return result.value;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    generic_delayed_invalid_instance = run([str(I_EXE), str(generic_delayed_invalid_instance_i), str(generic_delayed_invalid_instance_c)])
    if (
        generic_delayed_invalid_instance.returncode == 0
        or str(generic_delayed_invalid_instance_i) not in generic_delayed_invalid_instance.stdout
        or "type error: operator '+' cannot be applied to 'Payload' and 'Payload'" not in generic_delayed_invalid_instance.stdout
        or "return x + y;" not in generic_delayed_invalid_instance.stdout
    ):
        print("generic_delayed_invalid_instance: expected concrete generic body diagnostic")
        print(generic_delayed_invalid_instance.stdout)
        return 1
    print("ok generic_delayed_invalid_instance")

    missing_type_operation_i = TEST_DIR / "missing_type_operation.i"
    missing_type_operation_c = TEST_DIR / "missing_type_operation.c"
    missing_type_operation_i.write_text(r'''
Payload:struct = {
    value:i32;
}

sum:proc<T>(items:*T, count:u64)->T = {
    result:T = {};
    for (i:u64 = 0; i < count; i += 1) {
        result = add<T>(result, items[i]);
    }
    return result;
}

main:proc()->i32 = {
    items:[1]Payload = {{.value = 1}};
    result:Payload = sum<Payload>(items, 1);
    return result.value;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    missing_type_operation = run([str(I_EXE), str(missing_type_operation_i), str(missing_type_operation_c)])
    if (
        missing_type_operation.returncode == 0
        or str(missing_type_operation_i) not in missing_type_operation.stdout
        or "type error: missing type operation proc 'add_Payload' for call 'add<Payload>'" not in missing_type_operation.stdout
        or "result = add<T>(result, items[i]);" not in missing_type_operation.stdout
    ):
        print("missing_type_operation: expected missing type-operation diagnostic")
        print(missing_type_operation.stdout)
        return 1
    print("ok missing_type_operation")

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
    if (
        type_pointer.returncode == 0
        or "type error: initializer expected 'ptr_i32', got 'i32'" not in type_pointer.stdout
        or "note: expected a pointer; use '.&' to take the value address" not in type_pointer.stdout
        or "    p:*i32 = x;" not in type_pointer.stdout
        or "    ^" not in type_pointer.stdout
    ):
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
    if (
        type_array_elem.returncode == 0
        or "type error: initializer expected 'ptr_i32', got 'i32'" not in type_array_elem.stdout
        or "note: expected a pointer; use '.&' to take the value address" not in type_array_elem.stdout
    ):
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

    type_enum_dot_bad_i = TEST_DIR / "type_enum_dot_bad_member.i"
    type_enum_dot_bad_c = TEST_DIR / "type_enum_dot_bad_member.c"
    type_enum_dot_bad_i.write_text(r'''
Kind:enum = {
    None,
    Ready,
}

main:proc()->i32 = {
    kind:Kind = Kind.Bad;
    return kind;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_enum_dot_bad = run([str(I_EXE), str(type_enum_dot_bad_i), str(type_enum_dot_bad_c)])
    if type_enum_dot_bad.returncode == 0 or "type error: enum 'Kind' has no member 'Bad'" not in type_enum_dot_bad.stdout:
        print("type_enum_dot_bad_member: expected enum dot member diagnostic")
        print(type_enum_dot_bad.stdout)
        return 1
    print("ok type_enum_dot_bad_member")

    type_enum_float_assign_i = TEST_DIR / "type_enum_float_assignment.i"
    type_enum_float_assign_c = TEST_DIR / "type_enum_float_assignment.c"
    type_enum_float_assign_i.write_text(r'''
Kind:enum = {
    None,
    Ready,
}

main:proc()->i32 = {
    kind:Kind = Kind_Ready;
    value:f32 = kind;
    return cast(value, i32);
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_enum_float_assign = run([str(I_EXE), str(type_enum_float_assign_i), str(type_enum_float_assign_c)])
    if (
        type_enum_float_assign.returncode == 0
        or "type error: initializer expected 'f32', got 'Kind'" not in type_enum_float_assign.stdout
        or "    value:f32 = kind;" not in type_enum_float_assign.stdout
        or "^" not in type_enum_float_assign.stdout
    ):
        print("type_enum_float_assignment: expected implicit enum-to-float assignment diagnostic")
        print(type_enum_float_assign.stdout)
        return 1
    print("ok type_enum_float_assignment")

    type_enum_float_cast_i = TEST_DIR / "type_enum_float_cast.i"
    type_enum_float_cast_c = TEST_DIR / "type_enum_float_cast.c"
    type_enum_float_cast_i.write_text(r'''
Kind:enum = {
    None,
    Ready,
}

main:proc()->i32 = {
    kind:Kind = Kind_Ready;
    value:f32 = cast(kind, f32);
    return cast(value, i32);
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_enum_float_cast = run([str(I_EXE), str(type_enum_float_cast_i), str(type_enum_float_cast_c)])
    if type_enum_float_cast.returncode != 0:
        print("type_enum_float_cast: expected explicit enum-to-float cast to type-check")
        print(type_enum_float_cast.stdout)
        return 1
    print("ok type_enum_float_cast")

    type_cast_bad_i = TEST_DIR / "type_invalid_aggregate_cast.i"
    type_cast_bad_c = TEST_DIR / "type_invalid_aggregate_cast.c"
    type_cast_bad_i.write_text(r'''
Payload:struct = {
    value:i32;
}

main:proc()->i32 = {
    payload:Payload = {};
    return cast(payload, i32);
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_cast_bad = run([str(I_EXE), str(type_cast_bad_i), str(type_cast_bad_c)])
    if (
        type_cast_bad.returncode == 0
        or "type error: cannot cast 'Payload' to 'i32'" not in type_cast_bad.stdout
        or "    return cast(payload, i32);" not in type_cast_bad.stdout
        or "^" not in type_cast_bad.stdout
    ):
        print("type_invalid_aggregate_cast: expected invalid aggregate cast diagnostic")
        print(type_cast_bad.stdout)
        return 1
    print("ok type_invalid_aggregate_cast")

    type_cast_pointer_int_i = TEST_DIR / "type_pointer_integer_cast.i"
    type_cast_pointer_int_c = TEST_DIR / "type_pointer_integer_cast.c"
    type_cast_pointer_int_i.write_text(r'''
main:proc(p:*i32)->i32 = {
    bits:usize = cast(p, usize);
    q:*i32 = cast(bits, *i32);
    return q[0];
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_cast_pointer_int = run([str(I_EXE), str(type_cast_pointer_int_i), str(type_cast_pointer_int_c)])
    if type_cast_pointer_int.returncode != 0:
        print("type_pointer_integer_cast: expected pointer/integer casts to type-check")
        print(type_cast_pointer_int.stdout)
        return 1
    print("ok type_pointer_integer_cast")

    type_cast_pointer_float_i = TEST_DIR / "type_pointer_float_cast.i"
    type_cast_pointer_float_c = TEST_DIR / "type_pointer_float_cast.c"
    type_cast_pointer_float_i.write_text(r'''
main:proc(p:*i32)->i32 = {
    value:f32 = cast(p, f32);
    return cast(value, i32);
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_cast_pointer_float = run([str(I_EXE), str(type_cast_pointer_float_i), str(type_cast_pointer_float_c)])
    if (
        type_cast_pointer_float.returncode == 0
        or "type error: cannot cast 'ptr_i32' to 'f32'" not in type_cast_pointer_float.stdout
        or "    value:f32 = cast(p, f32);" not in type_cast_pointer_float.stdout
        or "^" not in type_cast_pointer_float.stdout
    ):
        print("type_pointer_float_cast: expected pointer-to-float cast diagnostic")
        print(type_cast_pointer_float.stdout)
        return 1
    print("ok type_pointer_float_cast")

    type_cast_float_pointer_i = TEST_DIR / "type_float_pointer_cast.i"
    type_cast_float_pointer_c = TEST_DIR / "type_float_pointer_cast.c"
    type_cast_float_pointer_i.write_text(r'''
main:proc()->i32 = {
    p:*i32 = cast(1.0f, *i32);
    return p[0];
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_cast_float_pointer = run([str(I_EXE), str(type_cast_float_pointer_i), str(type_cast_float_pointer_c)])
    if (
        type_cast_float_pointer.returncode == 0
        or "type error: cannot cast 'f32' to 'ptr_i32'" not in type_cast_float_pointer.stdout
        or "    p:*i32 = cast(1.0f, *i32);" not in type_cast_float_pointer.stdout
        or "^" not in type_cast_float_pointer.stdout
    ):
        print("type_float_pointer_cast: expected float-to-pointer cast diagnostic")
        print(type_cast_float_pointer.stdout)
        return 1
    print("ok type_float_pointer_cast")

    type_cast_array_pointer_i = TEST_DIR / "type_array_pointer_cast.i"
    type_cast_array_pointer_c = TEST_DIR / "type_array_pointer_cast.c"
    type_cast_array_pointer_i.write_text(r'''
main:proc()->i32 = {
    values:[4]i32 = {};
    p:*i32 = cast(values, *i32);
    return p[0];
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_cast_array_pointer = run([str(I_EXE), str(type_cast_array_pointer_i), str(type_cast_array_pointer_c)])
    if type_cast_array_pointer.returncode != 0:
        print("type_array_pointer_cast: expected fixed-array to pointer cast to type-check")
        print(type_cast_array_pointer.stdout)
        return 1
    print("ok type_array_pointer_cast")

    type_cast_array_int_i = TEST_DIR / "type_array_integer_cast.i"
    type_cast_array_int_c = TEST_DIR / "type_array_integer_cast.c"
    type_cast_array_int_i.write_text(r'''
main:proc()->i32 = {
    values:[4]i32 = {};
    return cast(values, i32);
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_cast_array_int = run([str(I_EXE), str(type_cast_array_int_i), str(type_cast_array_int_c)])
    if (
        type_cast_array_int.returncode == 0
        or "type error: cannot cast 'array_4_i32' to 'i32'" not in type_cast_array_int.stdout
        or "    return cast(values, i32);" not in type_cast_array_int.stdout
        or "^" not in type_cast_array_int.stdout
    ):
        print("type_array_integer_cast: expected fixed-array to integer cast diagnostic")
        print(type_cast_array_int.stdout)
        return 1
    print("ok type_array_integer_cast")

    type_cast_pointer_array_i = TEST_DIR / "type_pointer_array_cast.i"
    type_cast_pointer_array_c = TEST_DIR / "type_pointer_array_cast.c"
    type_cast_pointer_array_i.write_text(r'''
main:proc(p:*i32)->i32 = {
    values:[4]i32 = cast(p, [4]i32);
    return values[0];
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_cast_pointer_array = run([str(I_EXE), str(type_cast_pointer_array_i), str(type_cast_pointer_array_c)])
    if (
        type_cast_pointer_array.returncode == 0
        or "type error: cannot cast 'ptr_i32' to 'array_4_i32'" not in type_cast_pointer_array.stdout
        or "    values:[4]i32 = cast(p, [4]i32);" not in type_cast_pointer_array.stdout
        or "^" not in type_cast_pointer_array.stdout
    ):
        print("type_pointer_array_cast: expected pointer to fixed-array cast diagnostic")
        print(type_cast_pointer_array.stdout)
        return 1
    print("ok type_pointer_array_cast")

    type_proc_ptr_cast_i = TEST_DIR / "type_proc_pointer_cast_mismatch.i"
    type_proc_ptr_cast_c = TEST_DIR / "type_proc_pointer_cast_mismatch.c"
    type_proc_ptr_cast_i.write_text(r'''
Callback:alias = *proc(x:*i32)->i32;

good_cb:proc(x:i32)->i32 = {
    return x;
}

main:proc()->i32 = {
    cb:Callback = cast(good_cb, Callback);
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_proc_ptr_cast = run([str(I_EXE), str(type_proc_ptr_cast_i), str(type_proc_ptr_cast_c)])
    if (
        type_proc_ptr_cast.returncode == 0
        or "type error: cannot cast 'ptr_proc_i32_i32' to 'ptr_proc_i32_ptr_i32'" not in type_proc_ptr_cast.stdout
        or "note: expected proc signature: (arg0:ptr_i32)->i32" not in type_proc_ptr_cast.stdout
        or "note: actual proc signature: (arg0:i32)->i32" not in type_proc_ptr_cast.stdout
        or "    cb:Callback = cast(good_cb, Callback);" not in type_proc_ptr_cast.stdout
    ):
        print("type_proc_pointer_cast_mismatch: expected invalid proc pointer cast diagnostic")
        print(type_proc_ptr_cast.stdout)
        return 1
    print("ok type_proc_pointer_cast_mismatch")

    type_proc_ptr_opaque_cast_i = TEST_DIR / "type_proc_pointer_opaque_cast.i"
    type_proc_ptr_opaque_cast_c = TEST_DIR / "type_proc_pointer_opaque_cast.c"
    type_proc_ptr_opaque_cast_i.write_text(r'''
FARPROC:alias = proc()->void;
Callback:alias = proc(x:i32)->i32;

get_proc:proc()->FARPROC = { external; }

main:proc()->i32 = {
    cb:Callback = cast(get_proc(), Callback);
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_proc_ptr_opaque_cast = run([str(I_EXE), str(type_proc_ptr_opaque_cast_i), str(type_proc_ptr_opaque_cast_c)])
    if type_proc_ptr_opaque_cast.returncode != 0:
        print("type_proc_pointer_opaque_cast: expected FARPROC-style opaque callback cast to type-check")
        print(type_proc_ptr_opaque_cast.stdout)
        return 1
    print("ok type_proc_pointer_opaque_cast")

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

    type_enum_binary_mismatch_i = TEST_DIR / "type_enum_binary_mismatch.i"
    type_enum_binary_mismatch_c = TEST_DIR / "type_enum_binary_mismatch.c"
    type_enum_binary_mismatch_i.write_text(r'''
Kind:enum = {
    None,
    Ready,
}

Other:enum = {
    Bad,
}

main:proc()->i32 = {
    return Kind_Ready < Other_Bad;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_enum_binary_mismatch = run([str(I_EXE), str(type_enum_binary_mismatch_i), str(type_enum_binary_mismatch_c)])
    if (
        type_enum_binary_mismatch.returncode == 0
        or "type error: operator '<' cannot be applied to 'Kind' and 'Other'" not in type_enum_binary_mismatch.stdout
        or "    return Kind_Ready < Other_Bad;" not in type_enum_binary_mismatch.stdout
        or "^" not in type_enum_binary_mismatch.stdout
    ):
        print("type_enum_binary_mismatch: expected enum relational mismatch diagnostic")
        print(type_enum_binary_mismatch.stdout)
        return 1
    print("ok type_enum_binary_mismatch")

    type_enum_arithmetic_mismatch_i = TEST_DIR / "type_enum_arithmetic_mismatch.i"
    type_enum_arithmetic_mismatch_c = TEST_DIR / "type_enum_arithmetic_mismatch.c"
    type_enum_arithmetic_mismatch_i.write_text(r'''
Kind:enum = {
    None,
    Ready,
}

Other:enum = {
    Bad,
}

main:proc()->i32 = {
    return Kind_Ready + Other_Bad;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_enum_arithmetic_mismatch = run([str(I_EXE), str(type_enum_arithmetic_mismatch_i), str(type_enum_arithmetic_mismatch_c)])
    if (
        type_enum_arithmetic_mismatch.returncode == 0
        or "type error: operator '+' cannot be applied to 'Kind' and 'Other'" not in type_enum_arithmetic_mismatch.stdout
        or "    return Kind_Ready + Other_Bad;" not in type_enum_arithmetic_mismatch.stdout
        or "^" not in type_enum_arithmetic_mismatch.stdout
    ):
        print("type_enum_arithmetic_mismatch: expected enum arithmetic mismatch diagnostic")
        print(type_enum_arithmetic_mismatch.stdout)
        return 1
    print("ok type_enum_arithmetic_mismatch")

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

    type_binary_pointer_mismatch_i = TEST_DIR / "type_binary_pointer_subtraction_mismatch.i"
    type_binary_pointer_mismatch_c = TEST_DIR / "type_binary_pointer_subtraction_mismatch.c"
    type_binary_pointer_mismatch_i.write_text(r'''
main:proc()->i32 = {
    ints:[2]i32 = {};
    floats:[2]f32 = {};
    p:*i32 = ints;
    q:*f32 = floats;
    delta:long = p - q;
    return cast(delta, i32);
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_binary_pointer_mismatch = run([str(I_EXE), str(type_binary_pointer_mismatch_i), str(type_binary_pointer_mismatch_c)])
    if (
        type_binary_pointer_mismatch.returncode == 0
        or "type error: operator '-' cannot be applied to 'ptr_i32' and 'ptr_f32'" not in type_binary_pointer_mismatch.stdout
        or "    delta:long = p - q;" not in type_binary_pointer_mismatch.stdout
        or "^" not in type_binary_pointer_mismatch.stdout
    ):
        print("type_binary_pointer_subtraction_mismatch: expected pointer element mismatch diagnostic")
        print(type_binary_pointer_mismatch.stdout)
        return 1
    print("ok type_binary_pointer_subtraction_mismatch")

    type_binary_pointer_const_i = TEST_DIR / "type_binary_pointer_subtraction_const.i"
    type_binary_pointer_const_c = TEST_DIR / "type_binary_pointer_subtraction_const.c"
    type_binary_pointer_const_i.write_text(r'''
main:proc()->i32 = {
    values:[2]i32 = {};
    p:*const i32 = values;
    q:*i32 = values;
    delta:long = p - q;
    return cast(delta, i32);
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_binary_pointer_const = run([str(I_EXE), str(type_binary_pointer_const_i), str(type_binary_pointer_const_c)])
    if type_binary_pointer_const.returncode != 0:
        print("type_binary_pointer_subtraction_const: expected const-compatible pointer subtraction to type-check")
        print(type_binary_pointer_const.stdout)
        return 1
    print("ok type_binary_pointer_subtraction_const")

    type_binary_pointer_compare_const_i = TEST_DIR / "type_binary_pointer_comparison_const.i"
    type_binary_pointer_compare_const_c = TEST_DIR / "type_binary_pointer_comparison_const.c"
    type_binary_pointer_compare_const_i.write_text(r'''
main:proc()->i32 = {
    values:[2]i32 = {};
    p:*const i32 = values;
    q:*i32 = values;
    return p <= q;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_binary_pointer_compare_const = run([str(I_EXE), str(type_binary_pointer_compare_const_i), str(type_binary_pointer_compare_const_c)])
    if type_binary_pointer_compare_const.returncode != 0:
        print("type_binary_pointer_comparison_const: expected const-compatible pointer comparison to type-check")
        print(type_binary_pointer_compare_const.stdout)
        return 1
    print("ok type_binary_pointer_comparison_const")

    type_binary_pointer_compare_mismatch_i = TEST_DIR / "type_binary_pointer_comparison_mismatch.i"
    type_binary_pointer_compare_mismatch_c = TEST_DIR / "type_binary_pointer_comparison_mismatch.c"
    type_binary_pointer_compare_mismatch_i.write_text(r'''
main:proc()->i32 = {
    ints:[2]i32 = {};
    floats:[2]f32 = {};
    p:*i32 = ints;
    q:*f32 = floats;
    return p < q;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_binary_pointer_compare_mismatch = run([str(I_EXE), str(type_binary_pointer_compare_mismatch_i), str(type_binary_pointer_compare_mismatch_c)])
    if (
        type_binary_pointer_compare_mismatch.returncode == 0
        or "type error: operator '<' cannot be applied to 'ptr_i32' and 'ptr_f32'" not in type_binary_pointer_compare_mismatch.stdout
        or "    return p < q;" not in type_binary_pointer_compare_mismatch.stdout
        or "^" not in type_binary_pointer_compare_mismatch.stdout
    ):
        print("type_binary_pointer_comparison_mismatch: expected pointer element mismatch diagnostic")
        print(type_binary_pointer_compare_mismatch.stdout)
        return 1
    print("ok type_binary_pointer_comparison_mismatch")

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
        or "    return payload + 1;" not in type_binary_bad.stdout
        or "^" not in type_binary_bad.stdout
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
        or "type error: operator '&=' cannot be applied to 'f32' and 'f32'" not in type_compound_bitwise_float.stdout
        or "    value &= 1.0;" not in type_compound_bitwise_float.stdout
        or "^" not in type_compound_bitwise_float.stdout
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

    type_compound_enum_i = TEST_DIR / "type_compound_enum.i"
    type_compound_enum_c = TEST_DIR / "type_compound_enum.c"
    type_compound_enum_i.write_text(r'''
Flags:enum = {
    A = 1,
    B = 2,
}

main:proc()->i32 = {
    flags:Flags = Flags_A;
    flags |= Flags_B;
    flags &= Flags_A;
    return flags;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_compound_enum = run([str(I_EXE), str(type_compound_enum_i), str(type_compound_enum_c)])
    if type_compound_enum.returncode != 0:
        print("type_compound_enum: expected same-enum compound bitwise assignment to type-check")
        print(type_compound_enum.stdout)
        return 1
    print("ok type_compound_enum")

    type_compound_enum_mismatch_i = TEST_DIR / "type_compound_enum_mismatch.i"
    type_compound_enum_mismatch_c = TEST_DIR / "type_compound_enum_mismatch.c"
    type_compound_enum_mismatch_i.write_text(r'''
Flags:enum = {
    A = 1,
}

Other:enum = {
    B = 2,
}

main:proc()->i32 = {
    flags:Flags = Flags_A;
    flags |= Other_B;
    return flags;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_compound_enum_mismatch = run([str(I_EXE), str(type_compound_enum_mismatch_i), str(type_compound_enum_mismatch_c)])
    if (
        type_compound_enum_mismatch.returncode == 0
        or "type error: operator '|=' cannot be applied to 'Flags' and 'Other'" not in type_compound_enum_mismatch.stdout
        or "    flags |= Other_B;" not in type_compound_enum_mismatch.stdout
        or "^" not in type_compound_enum_mismatch.stdout
    ):
        print("type_compound_enum_mismatch: expected enum compound mismatch diagnostic")
        print(type_compound_enum_mismatch.stdout)
        return 1
    print("ok type_compound_enum_mismatch")

    type_compound_pointer_int_i = TEST_DIR / "type_compound_pointer_int.i"
    type_compound_pointer_int_c = TEST_DIR / "type_compound_pointer_int.c"
    type_compound_pointer_int_i.write_text(r'''
main:proc()->i32 = {
    values:[4]i32 = {};
    p:*i32 = values;
    p += 1;
    p -= 1;
    return p[0];
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_compound_pointer_int = run([str(I_EXE), str(type_compound_pointer_int_i), str(type_compound_pointer_int_c)])
    if type_compound_pointer_int.returncode != 0:
        print("type_compound_pointer_int: expected pointer integer compound assignment to type-check")
        print(type_compound_pointer_int.stdout)
        return 1
    print("ok type_compound_pointer_int")

    type_compound_pointer_float_i = TEST_DIR / "type_compound_pointer_float.i"
    type_compound_pointer_float_c = TEST_DIR / "type_compound_pointer_float.c"
    type_compound_pointer_float_i.write_text(r'''
main:proc()->i32 = {
    values:[4]i32 = {};
    p:*i32 = values;
    p += 1.0f;
    return p[0];
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_compound_pointer_float = run([str(I_EXE), str(type_compound_pointer_float_i), str(type_compound_pointer_float_c)])
    if (
        type_compound_pointer_float.returncode == 0
        or "type error: operator '+=' cannot be applied to 'ptr_i32' and 'f32'" not in type_compound_pointer_float.stdout
        or "    p += 1.0f;" not in type_compound_pointer_float.stdout
        or "^" not in type_compound_pointer_float.stdout
    ):
        print("type_compound_pointer_float: expected pointer float compound assignment diagnostic")
        print(type_compound_pointer_float.stdout)
        return 1
    print("ok type_compound_pointer_float")

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

    type_pointer_to_value_assign_i = TEST_DIR / "type_pointer_to_value_assignment.i"
    type_pointer_to_value_assign_c = TEST_DIR / "type_pointer_to_value_assignment.c"
    type_pointer_to_value_assign_i.write_text(r'''
main:proc()->i32 = {
    x:i32 = 0;
    p:*i32 = x.&;
    x = p;
    return x;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_pointer_to_value_assign = run([str(I_EXE), str(type_pointer_to_value_assign_i), str(type_pointer_to_value_assign_c)])
    if (
        type_pointer_to_value_assign.returncode == 0
        or "type error: assignment expected 'i32', got 'ptr_i32'" not in type_pointer_to_value_assign.stdout
        or "note: got a pointer; use '[0]' to access the pointed value" not in type_pointer_to_value_assign.stdout
    ):
        print("type_pointer_to_value_assignment: expected pointer dereference suggestion")
        print(type_pointer_to_value_assign.stdout)
        return 1
    print("ok type_pointer_to_value_assignment")

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
        or "note: constness comes from lvalue base type 'const_Payload'" not in type_const_field.stdout
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

    type_const_void_drop_i = TEST_DIR / "type_const_void_pointer_drop.i"
    type_const_void_drop_c = TEST_DIR / "type_const_void_pointer_drop.c"
    type_const_void_drop_i.write_text(r'''
main:proc(p:*const i32)->i32 = {
    raw:*void = p;
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_const_void_drop = run([str(I_EXE), str(type_const_void_drop_i), str(type_const_void_drop_c)])
    if (
        type_const_void_drop.returncode == 0
        or "type error: initializer expected 'ptr_void', got 'ptr_const_i32'" not in type_const_void_drop.stdout
        or "    raw:*void = p;" not in type_const_void_drop.stdout
        or "^" not in type_const_void_drop.stdout
    ):
        print("type_const_void_pointer_drop: expected pointer-to-const to mutable void pointer diagnostic")
        print(type_const_void_drop.stdout)
        return 1
    print("ok type_const_void_pointer_drop")

    type_const_void_add_i = TEST_DIR / "type_const_void_pointer_add.i"
    type_const_void_add_c = TEST_DIR / "type_const_void_pointer_add.c"
    type_const_void_add_i.write_text(r'''
main:proc(p:*const i32)->i32 = {
    raw:*const void = p;
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_const_void_add = run([str(I_EXE), str(type_const_void_add_i), str(type_const_void_add_c)])
    if type_const_void_add.returncode != 0:
        print("type_const_void_pointer_add: expected pointer-to-const to pointer-to-const void to type-check")
        print(type_const_void_add.stdout)
        return 1
    print("ok type_const_void_pointer_add")

    type_const_void_typed_drop_i = TEST_DIR / "type_const_void_typed_pointer_drop.i"
    type_const_void_typed_drop_c = TEST_DIR / "type_const_void_typed_pointer_drop.c"
    type_const_void_typed_drop_i.write_text(r'''
main:proc(raw:*const void)->i32 = {
    p:*i32 = raw;
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_const_void_typed_drop = run([str(I_EXE), str(type_const_void_typed_drop_i), str(type_const_void_typed_drop_c)])
    if (
        type_const_void_typed_drop.returncode == 0
        or "type error: initializer expected 'ptr_i32', got 'ptr_const_void'" not in type_const_void_typed_drop.stdout
        or "    p:*i32 = raw;" not in type_const_void_typed_drop.stdout
        or "^" not in type_const_void_typed_drop.stdout
    ):
        print("type_const_void_typed_pointer_drop: expected const void pointer to mutable typed pointer diagnostic")
        print(type_const_void_typed_drop.stdout)
        return 1
    print("ok type_const_void_typed_pointer_drop")

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

    type_const_array_element_i = TEST_DIR / "type_const_array_element_assignment.i"
    type_const_array_element_c = TEST_DIR / "type_const_array_element_assignment.c"
    type_const_array_element_i.write_text(r'''
main:proc()->i32 = {
    values:const [2]i32 = {};
    values[0] = 1;
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_const_array_element = run([str(I_EXE), str(type_const_array_element_i), str(type_const_array_element_c)])
    if (
        type_const_array_element.returncode == 0
        or "type error: cannot assign to const target of type 'i32'" not in type_const_array_element.stdout
        or "note: constness comes from lvalue base type 'const_array_2_i32'" not in type_const_array_element.stdout
    ):
        print("type_const_array_element_assignment: expected const array element assignment diagnostic")
        print(type_const_array_element.stdout)
        return 1
    print("ok type_const_array_element_assignment")

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
        case Kind_None: {
            return 0;
        }
        case Kind_Ready: {
            return 1;
        }
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

    type_switch_enum_mismatch_i = TEST_DIR / "type_switch_enum_mismatch.i"
    type_switch_enum_mismatch_c = TEST_DIR / "type_switch_enum_mismatch.c"
    type_switch_enum_mismatch_i.write_text(r'''
Kind:enum = {
    None,
    Ready,
}

Other:enum = {
    Bad,
}

main:proc(kind:Kind)->i32 = {
    switch (kind) {
        case Other_Bad: {
            return 1;
        }
    }
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_switch_enum_mismatch = run([str(I_EXE), str(type_switch_enum_mismatch_i), str(type_switch_enum_mismatch_c)])
    if (
        type_switch_enum_mismatch.returncode == 0
        or "type error: switch case expected 'Kind', got 'Other'" not in type_switch_enum_mismatch.stdout
        or "        case Other_Bad:" not in type_switch_enum_mismatch.stdout
        or "^" not in type_switch_enum_mismatch.stdout
    ):
        print("type_switch_enum_mismatch: expected enum switch case mismatch diagnostic")
        print(type_switch_enum_mismatch.stdout)
        return 1
    print("ok type_switch_enum_mismatch")

    type_switch_case_i = TEST_DIR / "type_switch_case.i"
    type_switch_case_c = TEST_DIR / "type_switch_case.c"
    type_switch_case_i.write_text(r'''
Payload:struct = {
    value:i32;
}

main:proc(value:i32)->i32 = {
    payload:Payload = {};
    switch (value) {
        case payload: {
            return 1;
        }
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

    type_float_pointer_alias_i = TEST_DIR / "type_float_pointer_alias_compat.i"
    type_float_pointer_alias_c = TEST_DIR / "type_float_pointer_alias_compat.c"
    type_float_pointer_alias_i.write_text(r'''
MyF32:alias = f32;
vec2:alias = [2]f32;

take_f32s:proc(values:*MyF32)->void = { external; }

main:proc()->i32 = {
    uv:vec2 = {};
    take_f32s(uv);
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_float_pointer_alias = run([str(I_EXE), str(type_float_pointer_alias_i), str(type_float_pointer_alias_c)])
    if type_float_pointer_alias.returncode != 0:
        print("type_float_pointer_alias_compat: expected c-float vector to decay to pointer-to-f32 alias")
        print(type_float_pointer_alias.stdout)
        return 1
    print("ok type_float_pointer_alias_compat")

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

    type_array_ptr_assign_mismatch_i = TEST_DIR / "type_array_pointer_assignment_mismatch.i"
    type_array_ptr_assign_mismatch_c = TEST_DIR / "type_array_pointer_assignment_mismatch.c"
    type_array_ptr_assign_mismatch_i.write_text(r'''
main:proc()->i32 = {
    values:[4]f32 = {};
    ptr:*i32 = {};
    ptr = values;
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_array_ptr_assign_mismatch = run([str(I_EXE), str(type_array_ptr_assign_mismatch_i), str(type_array_ptr_assign_mismatch_c)])
    if (
        type_array_ptr_assign_mismatch.returncode == 0
        or "type error: assignment expected 'ptr_i32', got 'array_4_f32'" not in type_array_ptr_assign_mismatch.stdout
        or "note: fixed array can decay to pointer only when element types match; expected element 'i32', got 'f32'" not in type_array_ptr_assign_mismatch.stdout
    ):
        print("type_array_pointer_assignment_mismatch: expected assignment array-to-pointer mismatch note")
        print(type_array_ptr_assign_mismatch.stdout)
        return 1
    print("ok type_array_pointer_assignment_mismatch")

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
        or "note: expected a pointer; use '.&' to take the value address" not in type_call.stdout
        or "    take_ptr(x);" not in type_call.stdout
        or "^" not in type_call.stdout
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
        or "note: expected params: a:i32, b:i32" not in type_call_count.stdout
        or "    return add(1);" not in type_call_count.stdout
        or "^" not in type_call_count.stdout
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
        or "note: expected a pointer; use '.&' to take the value address" not in import_type_call.stdout
        or "    take_ptr(x);" not in import_type_call.stdout
        or "^" not in import_type_call.stdout
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
        or "type error: proc pointer 'cb' argument 1 'x' expected 'i32', got 'ptr_i32'" not in type_proc_ptr_call_arg.stdout
        or "note: got a pointer; use '[0]' to access the pointed value" not in type_proc_ptr_call_arg.stdout
        or "note: expected params: x:i32" not in type_proc_ptr_call_arg.stdout
        or "    return cb(value.&);" not in type_proc_ptr_call_arg.stdout
        or "^" not in type_proc_ptr_call_arg.stdout
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
        or "note: expected params: a:i32, b:i32" not in type_proc_ptr_call_count.stdout
        or "    return cb(1);" not in type_proc_ptr_call_count.stdout
        or "^" not in type_proc_ptr_call_count.stdout
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
    if (
        type_proc_ptr_ret.returncode == 0
        or "type error: initializer expected 'Callback', got 'ptr_proc_ptr_i32_i32'" not in type_proc_ptr_ret.stdout
        or "note: expected proc signature: (arg0:i32)->i32" not in type_proc_ptr_ret.stdout
        or "note: actual proc signature: (arg0:i32)->ptr_i32" not in type_proc_ptr_ret.stdout
    ):
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
    if (
        type_proc_ptr_arg.returncode == 0
        or "type error: initializer expected 'Callback', got 'ptr_proc_i32_ptr_i32'" not in type_proc_ptr_arg.stdout
        or "note: expected proc signature: (arg0:i32)->i32" not in type_proc_ptr_arg.stdout
        or "note: actual proc signature: (arg0:ptr_i32)->i32" not in type_proc_ptr_arg.stdout
    ):
        print("type_proc_pointer_arg_mismatch: expected proc pointer argument type diagnostic")
        print(type_proc_ptr_arg.stdout)
        return 1
    print("ok type_proc_pointer_arg_mismatch")

    type_proc_ptr_const_arg_i = TEST_DIR / "type_proc_pointer_const_arg_mismatch.i"
    type_proc_ptr_const_arg_c = TEST_DIR / "type_proc_pointer_const_arg_mismatch.c"
    type_proc_ptr_const_arg_i.write_text(r'''
Callback:alias = *proc(x:*i32)->void;

bad_cb:proc(x:*const i32)->void = {
    return;
}

main:proc()->i32 = {
    cb:Callback = bad_cb;
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    type_proc_ptr_const_arg = run([str(I_EXE), str(type_proc_ptr_const_arg_i), str(type_proc_ptr_const_arg_c)])
    if (
        type_proc_ptr_const_arg.returncode == 0
        or "type error: initializer expected 'Callback', got 'ptr_proc_void_ptr_const_i32'" not in type_proc_ptr_const_arg.stdout
        or "note: expected proc signature: (arg0:ptr_i32)->void" not in type_proc_ptr_const_arg.stdout
        or "note: actual proc signature: (arg0:ptr_const_i32)->void" not in type_proc_ptr_const_arg.stdout
    ):
        print("type_proc_pointer_const_arg_mismatch: expected proc pointer const argument mismatch diagnostic")
        print(type_proc_ptr_const_arg.stdout)
        return 1
    print("ok type_proc_pointer_const_arg_mismatch")

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
        or "    return;" not in type_return_missing.stdout
        or f"{type_return_missing_i}:1:1: note: proc 'main' declared here" not in type_return_missing.stdout
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
        or "    return 1;" not in type_return_void_value.stdout
        or f"{type_return_void_value_i}:1:1: note: proc 'main' declared here" not in type_return_void_value.stdout
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
    if (
        type_field.returncode == 0
        or "type error: type 'Payload' has no field 'missing'" not in type_field.stdout
        or "    return payload.missing;" not in type_field.stdout
        or "^" not in type_field.stdout
    ):
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
        or "type error: field 'value' cannot be accessed on pointer type 'ptr_Payload'; use p[0].value" not in type_field_ptr.stdout
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
    if (
        type_init_field.returncode == 0
        or "type error: initializer for type 'Payload' has no field 'missing'" not in type_init_field.stdout
        or "    payload:Payload = {.missing = 1};" not in type_init_field.stdout
        or "^" not in type_init_field.stdout
    ):
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
        or "(previous at 6:" not in type_init_duplicate.stdout
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
        or "(previous at 7:" not in type_init_duplicate_pos.stdout
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
        or "(previous at 2:" not in type_array_init_dup.stdout
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
        or "(previous at 2:" not in type_array_init_dup_pos.stdout
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
vec2:alias = [2]f32;
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
    import_dup_mid = TEST_DIR / "import_duplicate_mid.i"
    import_dup_app = TEST_DIR / "import_duplicate_app.i"
    import_dup_c = TEST_DIR / "import_duplicate_app.c"
    import_dup_mod.write_text(r'''
Payload:struct = {
    value:i32;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    import_dup_mid.write_text(r'''
import "import_duplicate_mod.i"
'''.strip() + "\n", encoding="utf-8", newline="\n")
    import_dup_app.write_text(r'''
import "import_duplicate_mid.i"

Payload:struct = {
    other:i32;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    import_dup = run([str(I_EXE), str(import_dup_app), str(import_dup_c)])
    if (
        import_dup.returncode == 0
        or str(import_dup_app) not in import_dup.stdout
        or str(import_dup_mod) not in import_dup.stdout
        or str(import_dup_mid) not in import_dup.stdout
        or "duplicate struct declaration 'Payload'" not in import_dup.stdout
        or "previous at" not in import_dup.stdout
        or "note: previous declaration imported through:" not in import_dup.stdout
    ):
        print("import_duplicate_diagnostic: expected duplicate import source paths")
        print(import_dup.stdout)
        return 1
    print("ok import_duplicate_diagnostic")

    import_value_dup_mod = TEST_DIR / "import_value_duplicate_mod.i"
    import_value_dup_mid = TEST_DIR / "import_value_duplicate_mid.i"
    import_value_dup_app = TEST_DIR / "import_value_duplicate_app.i"
    import_value_dup_c = TEST_DIR / "import_value_duplicate_app.c"
    import_value_dup_mod.write_text(r'''
shared_value:proc()->i32 = {
    return 1;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    import_value_dup_mid.write_text(r'''
import "import_value_duplicate_mod.i"
'''.strip() + "\n", encoding="utf-8", newline="\n")
    import_value_dup_app.write_text(r'''
import "import_value_duplicate_mid.i"

shared_value:i32 = 2;
'''.strip() + "\n", encoding="utf-8", newline="\n")
    import_value_dup = run([str(I_EXE), str(import_value_dup_app), str(import_value_dup_c)])
    if (
        import_value_dup.returncode == 0
        or str(import_value_dup_app) not in import_value_dup.stdout
        or str(import_value_dup_mod) not in import_value_dup.stdout
        or str(import_value_dup_mid) not in import_value_dup.stdout
        or "duplicate global declaration 'shared_value'" not in import_value_dup.stdout
        or "previous at" not in import_value_dup.stdout
        or "note: previous declaration imported through:" not in import_value_dup.stdout
    ):
        print("import_value_duplicate_diagnostic: expected proc/global C namespace collision diagnostic")
        print(import_value_dup.stdout)
        return 1
    print("ok import_value_duplicate_diagnostic")

    macro_proc_dup_i = TEST_DIR / "macro_proc_duplicate.i"
    macro_proc_dup_c = TEST_DIR / "macro_proc_duplicate.c"
    macro_proc_dup_i.write_text(r'''
#define macro_proc 1

macro_proc:proc()->i32 = {
    return 0;
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    macro_proc_dup = run([str(I_EXE), str(macro_proc_dup_i), str(macro_proc_dup_c)])
    if (
        macro_proc_dup.returncode == 0
        or str(macro_proc_dup_i) not in macro_proc_dup.stdout
        or "semantic error: duplicate proc declaration 'macro_proc'" not in macro_proc_dup.stdout
        or "previous at" not in macro_proc_dup.stdout
    ):
        print("macro_proc_duplicate: expected macro/proc namespace collision diagnostic")
        print(macro_proc_dup.stdout)
        return 1
    print("ok macro_proc_duplicate")

    define_global_dup_i = TEST_DIR / "define_global_duplicate.i"
    define_global_dup_c = TEST_DIR / "define_global_duplicate.c"
    define_global_dup_i.write_text(r'''
define("macro_global")

macro_global:i32 = 1;
'''.strip() + "\n", encoding="utf-8", newline="\n")
    define_global_dup = run([str(I_EXE), str(define_global_dup_i), str(define_global_dup_c)])
    if (
        define_global_dup.returncode == 0
        or str(define_global_dup_i) not in define_global_dup.stdout
        or "semantic error: duplicate global declaration 'macro_global'" not in define_global_dup.stdout
        or "previous at" not in define_global_dup.stdout
    ):
        print("define_global_duplicate: expected define/global namespace collision diagnostic")
        print(define_global_dup.stdout)
        return 1
    print("ok define_global_duplicate")

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
        or "note: generic 'need_hash' instantiated here with type 'Payload'" not in generic_constraint.stdout
        or "note: generic declared here with requirement 'hashable'" not in generic_constraint.stdout
        or "    return cast(need_hash<Payload>(payload), i32);" not in generic_constraint.stdout
        or "need_hash:proc<T:hashable>(value:T)->u64" not in generic_constraint.stdout
    ):
        print("generic_constraint_site: expected instantiation-site requirement diagnostic")
        print(generic_constraint.stdout)
        return 1
    print("ok generic_constraint_site")

    generic_constraint_signature_i = TEST_DIR / "generic_constraint_signature.i"
    generic_constraint_signature_c = TEST_DIR / "generic_constraint_signature.c"
    generic_constraint_signature_i.write_text(r'''
Payload:struct = {
    value:i32;
}

hash_Payload:proc(value:*Payload)->i32 = {
    return 0;
}

need_hash:proc<T:hashable>(value:T)->u64 = {
    return hash<T>(value);
}

main:proc()->i32 = {
    payload:Payload = {};
    return cast(need_hash<Payload>(payload), i32);
}
'''.strip() + "\n", encoding="utf-8", newline="\n")
    generic_constraint_signature = run([str(I_EXE), str(generic_constraint_signature_i), str(generic_constraint_signature_c)])
    if (
        generic_constraint_signature.returncode == 0
        or str(generic_constraint_signature_i) not in generic_constraint_signature.stdout
        or "requirement error: proc 'need_hash' requires 'hashable' for type 'Payload'" not in generic_constraint_signature.stdout
        or "function 'hash_Payload' has incompatible signature" not in generic_constraint_signature.stdout
        or "note: expected signature: hash_Payload(value:Payload)->u64" not in generic_constraint_signature.stdout
        or "note: function 'hash_Payload' declared here" not in generic_constraint_signature.stdout
        or "    return cast(need_hash<Payload>(payload), i32);" not in generic_constraint_signature.stdout
        or "hash_Payload:proc(value:*Payload)->i32" not in generic_constraint_signature.stdout
    ):
        print("generic_constraint_signature: expected incompatible requirement helper diagnostic")
        print(generic_constraint_signature.stdout)
        return 1
    print("ok generic_constraint_signature")

    ibind_exe = BUILD / "ibind.exe"
    if not ibind_exe.exists():
        print("ok ibind_bindgen: skipped, ibind not built")
    else:
        ibind_header = TEST_DIR / "ibind_bindgen.h"
        ibind_out = TEST_DIR / "ibind_bindgen.i"
        ibind_header.write_text(r'''
#define IB_CONST 42
#define IB_NAME "hello"
#define IB_PAREN_CONST (42)
#define IB_CAST_CONST ((int)7)
#define IB_GROUP_CONST (IB_CONST | IB_CAST_CONST)
#define IB_UNSIGNED_CONST 42u
#define IB_ULL_CONST 18446744073709551615ULL
#define IB_HEX_SUFFIX_CONST 0xffUL
#define IB_ADD(x, y) ((x) + (y))

static const int IB_STATIC_CONST = 99;
static const unsigned IB_STATIC_HEX = 0x10u;
static const double IB_STATIC_DOUBLE = 3.5;
static const char *IB_STATIC_TEXT = "typed text";
static const char IB_STATIC_CHAR = 'A';
static const int NOT_IB_STATIC_SKIPPED = 101;

typedef int (*IB_Callback)(int x, const char *label);
typedef void (*IB_DataCallback)(void *ctx, const void *data);
typedef int (*IB_VarCallback)(int code, ...);
typedef int (__stdcall *IB_StdCallback)(int value);
typedef unsigned short IB_WChar;
typedef const IB_WChar *IB_LPCWSTR;
typedef float IB_Vec3[3];
typedef IB_Vec3 IB_Mat3[3];
typedef float IB_Mat4[4][4];
typedef struct IB_Opaque IB_Opaque;
typedef struct IB_Private *IB_Handle;
typedef const struct IB_Private *IB_ConstHandle;
typedef struct IB_Defined IB_Defined;

enum {
    IB_ANON_READY = 7,
    NOT_IB_ANON_SKIPPED = 9,
};

enum IB_Mode {
    IB_MODE_READY = 1,
    NOT_IB_MODE_SKIPPED = 2,
};

enum NOT_IB_Mode {
    NOT_IB_MODE_DECL_SKIPPED = 3,
};

typedef struct IB_Payload {
    int value;
    float weights[4];
    IB_Callback cb;
    IB_DataCallback data_cb;
    IB_VarCallback var_cb;
    int (*raw_cb)(int count, const char *label);
    IB_LPCWSTR title;
    IB_Handle handle;
    struct IB_FieldOpaque *field_opaque;
} IB_Payload;

struct IB_Defined {
    int value;
};

typedef struct IB_Bits {
    unsigned flags:3;
    unsigned mode:5;
} IB_Bits;

typedef struct IB_Anon {
    union {
        int x;
        float y;
    };
    struct {
        int a;
        int b;
    } named;
    struct {
        int z;
    };
} IB_Anon;

typedef struct __attribute__((packed)) IB_Packed {
    char tag;
    int value;
} IB_Packed;

typedef struct IB_Flex {
    unsigned count;
    char bytes[];
} IB_Flex;

int IB_do(IB_Callback cb, IB_Payload *payload);
void *IB_copy(void *dst, const void *src, unsigned count);
int IB_use_handle(IB_Handle handle, const IB_Opaque *opaque, IB_Defined *defined);
int __stdcall IB_call(IB_StdCallback cb, int value);
int IB_wide(IB_LPCWSTR title, IB_WChar *out_title);
int IB_use_vec(IB_Vec3 v, IB_Mat3 m, IB_Mat4 mm);
int IB_log(const char *fmt, ...);
'''.strip() + "\n", encoding="utf-8", newline="\n")

        ibind = run([str(ibind_exe), str(ibind_header), str(ibind_out), "--prefix", "IB_", "--", "-target", "i686-pc-windows-msvc"])
        if ibind.returncode != 0:
            print(ibind.stdout)
            return ibind.returncode
        ibind_text = ibind_out.read_text(encoding="utf-8")
        for needle in (
            '#define IB_CONST 42',
            '#define IB_NAME "hello"',
            '#define IB_PAREN_CONST 42',
            '#define IB_CAST_CONST 7',
            '#define IB_GROUP_CONST IB_CONST | IB_CAST_CONST',
            '#define IB_UNSIGNED_CONST 42',
            '#define IB_ULL_CONST 18446744073709551615',
            '#define IB_HEX_SUFFIX_CONST 0xff',
            '#define IB_STATIC_CONST 99',
            '#define IB_STATIC_HEX 16',
            '#define IB_STATIC_DOUBLE 3.5',
            '#define IB_STATIC_TEXT "typed text"',
            '#define IB_STATIC_CHAR 65',
            '#define IB_ANON_READY 7',
            "IB_Mode: enum = {",
            "    IB_MODE_READY = 1,",
            "IB_Callback: alias = *proc(x:i32, label:*const char)->i32;",
            "IB_DataCallback: alias = *proc(ctx:*void, data:*const void)->void;",
            "IB_VarCallback: alias = *proc(code:i32, ...)->i32;",
            "IB_StdCallback: alias = *proc[__stdcall](value:i32)->i32;",
            "IB_WChar: alias = u16;",
            "IB_LPCWSTR: alias = *const IB_WChar;",
            "IB_Vec3: alias = [3]f32;",
            "IB_Mat3: alias = [3]IB_Vec3;",
            "IB_Mat4: alias = [4][4]f32;",
            "IB_Opaque: struct = { external; }",
            "IB_Private: struct = { external; }",
            "IB_FieldOpaque: struct = { external; }",
            "IB_Handle: alias = *IB_Private;",
            "IB_ConstHandle: alias = *const IB_Private;",
            "IB_Payload: struct = {",
            "    value:i32;",
            "    weights:[4]f32;",
            "    cb:IB_Callback;",
            "    data_cb:IB_DataCallback;",
            "    var_cb:IB_VarCallback;",
            "    raw_cb:*proc(count:i32, label:*const char)->i32;",
            "    title:IB_LPCWSTR;",
            "    handle:IB_Handle;",
            "    field_opaque:*IB_FieldOpaque;",
            "IB_Defined: struct = {",
            "    value:i32;",
            "IB_Bits: struct = {",
            "    // ibind: bitfield flags:3",
            "    // ibind: field_offset flags:0",
            "    flags:u32;",
            "    // ibind: bitfield mode:5",
            "    // ibind: field_offset mode:3",
            "    mode:u32;",
            "IB_Anon_anon0: union = {",
            "    x:i32;",
            "    y:f32;",
            "IB_Anon_anon1: struct = {",
            "    a:i32;",
            "    b:i32;",
            "IB_Anon_anon2: struct = {",
            "    z:i32;",
            "IB_Anon: struct = {",
            "    _anon0:IB_Anon_anon0;",
            "    named:IB_Anon_anon1;",
            "    _anon2:IB_Anon_anon2;",
            "// ibind: packed",
            "// ibind: layout size=5 align=1",
            "IB_Packed: struct = {",
            "    // ibind: field_offset tag:0",
            "    tag:char;",
            "    // ibind: field_offset value:8",
            "    value:i32;",
            "IB_Flex: struct = {",
            "    count:u32;",
            "    // ibind: incomplete_array bytes",
            "    bytes:*char;",
            "IB_do: proc(cb: IB_Callback, payload: *IB_Payload)->i32 = { external_emit; }",
            "IB_copy: proc(dst: *void, src: *const void, count: u32)->*void = { external_emit; }",
            "IB_use_handle: proc(handle: IB_Handle, opaque: *const IB_Opaque, defined: *IB_Defined)->i32 = { external_emit; }",
            "IB_call: proc[__stdcall](cb: IB_StdCallback, value: i32)->i32 = { external_emit; }",
            "IB_wide: proc(title: IB_LPCWSTR, out_title: *IB_WChar)->i32 = { external_emit; }",
            "IB_use_vec: proc(v: IB_Vec3, m: IB_Mat3, mm: IB_Mat4)->i32 = { external_emit; }",
            "IB_log: proc(fmt: *const char, ...)->i32 = { external_emit; }",
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
        if "NOT_IB_MODE_SKIPPED" in ibind_text or "NOT_IB_MODE_DECL_SKIPPED" in ibind_text or "NOT_IB_Mode" in ibind_text:
            print("ibind_bindgen: named enum declarations and constants should honor --prefix")
            print(ibind_text)
            return 1
        if "NOT_IB_STATIC_SKIPPED" in ibind_text:
            print("ibind_bindgen: typed constants should honor --prefix")
            print(ibind_text)
            return 1
        if "IB_Defined: struct = { external; }" in ibind_text:
            print("ibind_bindgen: defined forward typedef should not emit opaque external record")
            print(ibind_text)
            return 1

        ibind_array_alias_header = TEST_DIR / "ibind_array_alias.h"
        ibind_array_alias_out = TEST_DIR / "ibind_array_alias.i"
        ibind_array_alias_header.write_text(r'''
typedef float IB_ArrayVec3[3];
typedef float IB_ArrayVec4[4];
typedef IB_ArrayVec3 IB_ArrayMat3[3];
typedef float IB_ArrayMat4[4][4];
typedef union IB_ArrayVec3s {
    IB_ArrayVec3 raw;
} IB_ArrayVec3s;
typedef union IB_ArrayVec4s {
    IB_ArrayVec4 raw;
} IB_ArrayVec4s;
typedef union IB_ArrayMat4s {
    IB_ArrayVec4 raw[4];
    IB_ArrayVec4s col[4];
} IB_ArrayMat4s;
int IB_array_use(IB_ArrayVec3 v, IB_ArrayMat3 m, IB_ArrayMat4 mm, IB_ArrayVec3s vs, IB_ArrayMat4s ms);
'''.strip() + "\n", encoding="utf-8", newline="\n")
        ibind_array_alias = run([str(ibind_exe), str(ibind_array_alias_header), str(ibind_array_alias_out), "--prefix", "IB_", "--", "-I", str(TEST_DIR)])
        if ibind_array_alias.returncode != 0:
            print(ibind_array_alias.stdout)
            return ibind_array_alias.returncode
        ibind_array_alias_text = ibind_array_alias_out.read_text(encoding="utf-8")
        for needle in (
            "IB_ArrayVec3: alias = [3]f32;",
            "IB_ArrayVec4: alias = [4]f32;",
            "IB_ArrayMat3: alias = [3]IB_ArrayVec3;",
            "IB_ArrayMat4: alias = [4][4]f32;",
            "IB_ArrayVec3s: union = {",
            "    raw:IB_ArrayVec3;",
            "IB_ArrayVec4s: union = {",
            "    raw:IB_ArrayVec4;",
            "IB_ArrayMat4s: union = {",
            "    raw:[4]IB_ArrayVec4;",
            "    col:[4]IB_ArrayVec4s;",
            "IB_array_use: proc(v: IB_ArrayVec3, m: IB_ArrayMat3, mm: IB_ArrayMat4, vs: IB_ArrayVec3s, ms: IB_ArrayMat4s)->i32 = { external_emit; }",
        ):
            if needle not in ibind_array_alias_text:
                print(f"ibind_array_alias: generated binding missing {needle!r}")
                print(ibind_array_alias_text)
                return 1

        ibind_array_alias_use_i = TEST_DIR / "ibind_array_alias_use.i"
        ibind_array_alias_use_c = TEST_DIR / "ibind_array_alias_use.c"
        ibind_array_alias_use_exe = TEST_DIR / "ibind_array_alias_use.exe"
        ibind_array_alias_source = r'''
cinclude "stdio.h"
cinclude "ibind_array_alias.h"
import "{IBIND_OUT}"

json_read:proc<IB_ArrayVec3>(out:IB_ArrayVec3)->i32 = {
    out[0] = 1.0f;
    out[1] = 2.0f;
    out[2] = 3.0f;
    return 3;
}

main:proc()->i32 = {
    v:IB_ArrayVec3 = {};
    m:IB_ArrayMat3 = {};
    vs:IB_ArrayVec3s = {};
    ms:IB_ArrayMat4s = {};
    count:i32 = json_read<IB_ArrayVec3>(v);
    m[0][0] = v[2];
    vs.raw[1] = v[1];
    ms.col[0].raw[3] = 4.0f;
    printf("%d %.0f %.0f %.0f %.0f\n", count, v[1], m[0][0], vs.raw[1], ms.col[0].raw[3]);
    return 0;
}
'''.replace("{IBIND_OUT}", ibind_array_alias_out.as_posix())
        ibind_array_alias_use_i.write_text(ibind_array_alias_source.strip() + "\n", encoding="utf-8", newline="\n")
        ibind_array_alias_translate = run([str(I_EXE), str(ibind_array_alias_use_i), str(ibind_array_alias_use_c)])
        if ibind_array_alias_translate.returncode != 0:
            print(ibind_array_alias_translate.stdout)
            return ibind_array_alias_translate.returncode
        ibind_array_alias_generated = ibind_array_alias_use_c.read_text(encoding="utf-8")
        for needle in ("json_read_IB_ArrayVec3", "IB_ArrayVec3 v", "IB_ArrayMat3 m", "IB_ArrayVec3s vs", "IB_ArrayMat4s ms", "m[0][0] = v[2]", "ms.col[0].raw[3] = 4.0f"):
            if needle not in ibind_array_alias_generated:
                print(f"ibind_array_alias_use: generated C missing {needle!r}")
                print(ibind_array_alias_generated)
                return 1
        ibind_array_alias_compile = run([
            "clang.exe",
            str(ibind_array_alias_use_c),
            "-I",
            "src",
            "-I",
            "src/std",
            "-I",
            str(TEST_DIR),
            "-o",
            str(ibind_array_alias_use_exe),
        ])
        if ibind_array_alias_compile.returncode != 0:
            print(ibind_array_alias_compile.stdout)
            return ibind_array_alias_compile.returncode
        ibind_array_alias_program = run([str(ibind_array_alias_use_exe)])
        if ibind_array_alias_program.returncode != 0 or ibind_array_alias_program.stdout != "3 2 3 2 4\n":
            print("ibind_array_alias_use: stdout mismatch")
            print(ibind_array_alias_program.stdout)
            return 1

        ibind_filter_noise = TEST_DIR / "not_ibind_selected_main.h"
        ibind_filter_main = TEST_DIR / "ibind_selected_main.h"
        ibind_filter_out = TEST_DIR / "ibind_selected_main.i"
        ibind_filter_noise.write_text(r'''
#define IB_FILTER_NOISE 1
typedef struct IB_FilterNoise {
    int should_skip;
} IB_FilterNoise;
'''.strip() + "\n", encoding="utf-8", newline="\n")
        ibind_filter_main.write_text(r'''
#include "not_ibind_selected_main.h"

typedef struct IB_FilterPayload {
    IB_FilterNoise *noise;
    int value;
} IB_FilterPayload;
'''.strip() + "\n", encoding="utf-8", newline="\n")
        ibind_filter = run([str(ibind_exe), str(ibind_filter_main), str(ibind_filter_out), "--filter", ibind_filter_main.name, "--prefix", "IB_", "--", "-I", str(TEST_DIR)])
        if ibind_filter.returncode != 0:
            print(ibind_filter.stdout)
            return ibind_filter.returncode
        ibind_filter_text = ibind_filter_out.read_text(encoding="utf-8")
        if "IB_FilterPayload: struct = {" not in ibind_filter_text or "noise:*IB_FilterNoise;" not in ibind_filter_text:
            print("ibind_bindgen_filter: expected selected header declaration and dependency type reference")
            print(ibind_filter_text)
            return 1
        if "IB_FilterNoise: struct" in ibind_filter_text or "IB_FILTER_NOISE" in ibind_filter_text:
            print("ibind_bindgen_filter: selected header filter should not leak similarly named included header declarations")
            print(ibind_filter_text)
            return 1
        print("ok ibind_bindgen_filter")
        print("ok ibind_bindgen")

    # Error recovery: one bad construct must not hide the rest of the file, and must
    # not invent follow-on errors in code that is actually fine.
    recovery_cases = (
        (
            "recovery_multi_semantic",
            r'''
a:proc()->i32 = { return undefined_one; }
b:proc()->i32 = { return undefined_two; }
c:proc()->i32 = { return undefined_three; }
''',
            ("undefined_one", "undefined_two", "undefined_three"),
            (),
        ),
        (
            "recovery_parse_resync",
            r'''
a:proc()->i32 = { x:i32 = ; return 0; }
b:proc()->i32 = { return 1 }
c:proc()->i32 = { return 2; }
''',
            ("expected expression", "expected ';' after return"),
            # 'c' is valid, so nothing may be reported against it
            ("perr.i:3", "line 3"),
        ),
        (
            "recovery_unclosed_brace",
            "a:proc()->i32 = {\n    return 0;\n",
            ("unclosed '{'",),
            (),
        ),
        # Uniform block rule: switch cases, switch defaults, and labels all take a
        # block, and variables all say what they start as.
        (
            "blockless_case_rejected",
            "main:proc()->i32 = {\n    switch (1) {\n        case 1: return 5;\n    }\n    return 0;\n}\n",
            ("a switch case takes a block",),
            (),
        ),
        (
            "blockless_default_rejected",
            "main:proc()->i32 = {\n    switch (1) {\n        default: return 5;\n    }\n    return 0;\n}\n",
            ("a switch default takes a block",),
            (),
        ),
        (
            "blockless_label_rejected",
            "main:proc()->i32 = {\n    done: label;\n    return 0;\n}\n",
            ("expected '=' after label",),
            (),
        ),
        (
            "uninitialized_local_rejected",
            "main:proc()->i32 = {\n    x: i32;\n    return x;\n}\n",
            ("needs an initializer", "'= ?'"),
            (),
        ),
        (
            "uninitialized_global_rejected",
            "g: i32;\nmain:proc()->i32 = { return g; }\n",
            ("needs an initializer",),
            (),
        ),
        # Passthrough directives reach the generated C untouched, so an unbalanced
        # conditional must be caught here rather than surfacing as a C error that
        # points at emitted code.
        (
            "preproc_unterminated_if",
            "#if 0\nmain:proc()->i32 = { return 0; }\n",
            ("unterminated '#if'", ":1:1"),
            (),
        ),
        (
            "preproc_stray_endif",
            "main:proc()->i32 = { return 0; }\n#endif\n",
            ("'#endif' without a matching '#if'", ":2:1"),
            (),
        ),
        (
            "preproc_stray_else",
            "#else\nmain:proc()->i32 = { return 0; }\n",
            ("'#else' without a matching '#if'",),
            (),
        ),
        (
            "preproc_stray_elif",
            "#elif 1\nmain:proc()->i32 = { return 0; }\n",
            ("'#elif' without a matching '#if'",),
            (),
        ),
        # Independent errors inside one statement must all be reported, while an
        # expression whose type could not be resolved stays quiet instead of
        # cascading. Unresolved types act as the poison value that makes both hold.
        (
            "recovery_within_statement",
            r'''
P:struct = { v:i32; }
takes_two:proc(a:i32, b:i32)->i32 = { return a + b; }
gen:proc<T>(x:T)->T = { return x; }

main:proc()->i32 = {
    p:P = {};
    a:i32 = takes_two(undeclared_thing, p);
    b:i32 = takes_two(p.nofield, p);
    c:i32 = takes_two(gen<i32, f32>(1), p);
    d:i32 = p.missing_x + p.missing_y;
    return a + b + c + d;
}
''',
            (
                "use of undeclared identifier 'undeclared_thing'",
                "type 'P' has no field 'nofield'",
                "generic proc 'gen' expects 1 type arg, got 2",
                # the independent sibling argument is still checked in each case
                "argument 2 'b' expected 'i32', got 'P'",
                # both sides of one binary expression report
                "has no field 'missing_x'",
                "has no field 'missing_y'",
            ),
            (
                # the unresolved arguments must not also produce argument-1 type errors
                "argument 1 'a' expected",
            ),
        ),
    )
    for name, source, expected, forbidden in recovery_cases:
        rec_i = TEST_DIR / f"{name}.i"
        rec_i.write_text(source.strip() + "\n", encoding="utf-8", newline="\n")
        rec = run([str(I_EXE), "check", str(rec_i)])
        if rec.returncode == 0:
            print(f"{name}: expected a non-zero exit")
            print(rec.stdout)
            return 1
        for needle in expected:
            if needle not in rec.stdout:
                print(f"{name}: missing diagnostic {needle!r}")
                print(rec.stdout)
                return 1
        for needle in forbidden:
            if needle in rec.stdout:
                print(f"{name}: unexpected cascade {needle!r}")
                print(rec.stdout)
                return 1
        # the JSON form must stay a single well-formed array no matter how many
        # diagnostics it carries
        rec_json = run([str(I_EXE), "check", str(rec_i), "--diagnostics=json"])
        try:
            payload = json.loads(rec_json.stdout)
        except json.JSONDecodeError as exc:
            print(f"{name}: malformed JSON diagnostics: {exc}")
            print(rec_json.stdout)
            return 1
        if not isinstance(payload, list) or not payload:
            print(f"{name}: expected a non-empty JSON diagnostic array")
            print(rec_json.stdout)
            return 1
        print(f"ok {name}")

    # Fields and parameters have nothing to initialize, so the rule must not reach
    # them; '= ?' must lower to a plain C declaration with no zeroing.
    exempt_i = TEST_DIR / "init_exemptions.i"
    exempt_c = TEST_DIR / "init_exemptions.c"
    exempt_i.write_text(
        "S:struct = {\n    a:i32;\n    b:*S;\n}\n"
        "g_scratch:[8]u8 = ?;\n"
        "f:proc(x:i32, y:*S)->i32 = {\n    buf:[16]u8 = ?;\n    buf[0] = 1;\n    return x + cast(buf[0], i32);\n}\n"
        "main:proc()->i32 = { s:S = {}; return f(s.a, s.b); }\n",
        encoding="utf-8", newline="\n",
    )
    exempt = run([str(I_EXE), "compile", str(exempt_i), "-o", str(exempt_c), "--no-header"])
    if exempt.returncode != 0:
        print("init_exemptions: struct fields, params, and '= ?' should all be accepted")
        print(exempt.stdout)
        return 1
    exempt_generated = exempt_c.read_text(encoding="utf-8")
    if "u8 g_scratch[8];" not in exempt_generated or "u8 buf[16];" not in exempt_generated:
        print("init_exemptions: '= ?' should emit a bare declaration with no initializer")
        print(exempt_generated)
        return 1
    if "g_scratch[8] = " in exempt_generated or "buf[16] = " in exempt_generated:
        print("init_exemptions: '= ?' must not emit an initializer")
        return 1
    print("ok init_exemptions")

    # Same-named locals in different switch cases used to emit a C redefinition.
    case_scope_i = TEST_DIR / "case_scope.i"
    case_scope_c = TEST_DIR / "case_scope.c"
    case_scope_i.write_text(
        "main:proc()->i32 = {\n    x:i32 = 1;\n    switch (x) {\n"
        "        case 1: {\n            a:i32 = 5;\n            return a;\n        }\n"
        "        case 2: {\n            a:i32 = 6;\n            return a;\n        }\n"
        "    }\n    return 0;\n}\n",
        encoding="utf-8", newline="\n",
    )
    case_scope = run([str(I_EXE), "compile", str(case_scope_i), "-o", str(case_scope_c), "--no-header"])
    if case_scope.returncode != 0:
        print("case_scope: per-case locals should compile")
        print(case_scope.stdout)
        return 1
    case_generated = case_scope_c.read_text(encoding="utf-8")
    if "case 1: {" not in case_generated or "case 2: {" not in case_generated:
        print("case_scope: expected each case to emit its own C block")
        print(case_generated)
        return 1
    print("ok case_scope")

    # Balanced conditionals, including the ifdef/ifndef spellings, must stay silent.
    balanced_i = TEST_DIR / "preproc_balanced.i"
    balanced_i.write_text(
        "#if 0\n#define PREPROC_A 1\n#else\n#define PREPROC_B 2\n#endif\n"
        "#ifdef PREPROC_Y\n#endif\n#ifndef PREPROC_Z\n#endif\n"
        "main:proc()->i32 = { return 0; }\n",
        encoding="utf-8", newline="\n",
    )
    balanced = run([str(I_EXE), "check", str(balanced_i)])
    if balanced.returncode != 0:
        print("preproc_balanced: balanced conditionals should check cleanly")
        print(balanced.stdout)
        return 1
    print("ok preproc_balanced")

    # 'c' is valid code in the resync case, so it must produce no diagnostics at all
    resync = run([str(I_EXE), "check", str(TEST_DIR / "recovery_parse_resync.i"), "--diagnostics=json"])
    resync_lines = {d["line"] for d in json.loads(resync.stdout)}
    if 3 in resync_lines:
        print(f"recovery_parse_resync: valid line 3 produced a diagnostic: {sorted(resync_lines)}")
        print(resync.stdout)
        return 1
    if resync_lines != {1, 2}:
        print(f"recovery_parse_resync: expected exactly one diagnostic per bad line, got {sorted(resync_lines)}")
        print(resync.stdout)
        return 1
    print("ok recovery_parse_resync_exact")

    # Reduced #line output must map every generated line to the same source position
    # that fully-directive output would.
    map_sources = [ROOT / "src" / "main.i"] + sorted((ROOT / "tests" / "i-torture" / "execute").glob("*.i"))
    for src_path in map_sources:
        full_c = TEST_DIR / f"map_full_{src_path.stem}.c"
        red_c = TEST_DIR / f"map_red_{src_path.stem}.c"
        full = run([str(I_EXE), "compile", str(src_path), "-o", str(full_c), "--no-header",
                    "--emit-all-line-directives"])
        red = run([str(I_EXE), "compile", str(src_path), "-o", str(red_c), "--no-header"])
        if full.returncode != 0 or red.returncode != 0:
            print(f"line_map_equivalence: failed to compile {src_path.name}")
            print(full.stdout, red.stdout)
            return 1
        full_map = c_line_mapping(full_c.read_text(encoding="utf-8"))
        red_map = c_line_mapping(red_c.read_text(encoding="utf-8"))
        if full_map != red_map:
            print(f"line_map_equivalence: {src_path.name} maps differently without every #line")
            for a, b in zip(full_map, red_map):
                if a != b:
                    print(f"  full   ={a}")
                    print(f"  reduced={b}")
                    break
            return 1
    print(f"ok line_map_equivalence ({len(map_sources)} sources)")

    lsp = run([sys.executable, "tests/run_lsp_tests.py"])
    if lsp.returncode != 0:
        print(lsp.stdout)
        return lsp.returncode
    print(lsp.stdout.rstrip())

    i_torture = run([sys.executable, "tests/run_i_torture.py"])
    if i_torture.returncode != 0:
        print(i_torture.stdout)
        return i_torture.returncode
    print(i_torture.stdout.rstrip())

    i_execute = run([sys.executable, "tests/run_i_execute.py"])
    if i_execute.returncode != 0:
        print(i_execute.stdout)
        return i_execute.returncode
    print(i_execute.stdout.rstrip())

    i_debuginfo = run([sys.executable, "tests/run_i_debuginfo.py"])
    if i_debuginfo.returncode != 0:
        print(i_debuginfo.stdout)
        return i_debuginfo.returncode
    print(i_debuginfo.stdout.rstrip())

    # Bounded here so the suite stays quick; soak with
    # `python tests/run_i_fuzz.py --iterations 6000 --seed <n> --keep-going`.
    i_fuzz = run([sys.executable, "tests/run_i_fuzz.py", "--iterations", "400"])
    if i_fuzz.returncode != 0:
        print(i_fuzz.stdout)
        return i_fuzz.returncode
    print(i_fuzz.stdout.rstrip())

    torture = run([sys.executable, "tests/run_c_torture.py"])
    if torture.returncode != 0:
        print(torture.stdout)
        return torture.returncode
    print(torture.stdout.rstrip())

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
