import "std/memops.i"
import "std/Array.i"
import "std/Node.i"
import "std/List.i"

payload: struct = {
    x: f32;
    y: *u8;
}

main: proc(argc: i32, argv: **char)-> i32 = {
    printf("Hello, World!\n");

    num: i32 = 0;
    printf("num = {}\n", num);

    # array: *[]i32 = {};

    arena: memops_arena = {};
    memops_arena_initialize(&arena);

    p: payload = {};
    name: *char = payload<>.name;
    printf("name %s\n", name);
    for (i: i32 = 0; i < payload<>.field_count; i += 1) {
        printf("field[%d] = %s\n", i, payload<>.fields[i].name);
    }

    len: i32 = 16;
    temp: memops_arena_temp = memops_arena_temp_begin(&arena);
    payloads: Array<payload> = Array<payload>reserve(temp.arena, 128);
    for (i: i32 = 0; i < 128; i += 1) {
        payloads.data[i] = { .x = i };
    }
    memops_arena_temp_end(temp);

    numbers: Array<i32> = Array<i32>reserve(&arena, len);
    for (i: i32 = 0; i < len; i += 1) { 
        numbers.data[i] = i; 
    }
    for (i: i32 = 0; i < len; i += 1 ) {
        printf("i = {}, ", numbers.data[i]); 
    }

    numberchain: *List<payload>;
    numberchain = List<payload>create(&arena);
    List<payload>append(arena.&, numberchain, { .x = 3.16 });
    List<payload>append(arena.&, numberchain, { .x = 8.6 });

    node: *Node<payload> = numberchain.*.head;
    for (i: i32 = 0; i < numberchain.*.length; i += 1) {
        printf("%f", node.*.data.x);
        node = node.*.next;
    }

    return 0;
}
