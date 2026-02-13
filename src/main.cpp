//---------------------------------------------------------------------------------------------------
// monomorphization codegen limitations:
//---------------------------------------------------------------------------------------------------
// for containers that have value types eg `T`,
// the type must be included before the generated header.
// this is because the container expects to know the type in it's struct.
// Warning: cannot be recursive type
//
// for containers that have pointer types eg `T *`,
// the type can be included before or after the generated header.
// this is because the container has `T` forward declared.
// Warning: can be recursive type
//
// for types that include a container of themselves eg `struct T { Array_T arr; };`
// the type must be included after the generated header.
// this is because the type needs to know the container definition.
// Warning: can be recursive type with `T *` but not `T`
//---------------------------------------------------------------------------------------------------
// primitives
//---------------------------------------------------------------------------------------------------
// haikal@Array:voidptr:p
// haikal@Array:i8:p
// haikal@Array:i32:p
// haikal@Array:f32:p
// haikal@Map:i32:p
// haikal@Map:u64:p
// haikal@Node:i32:p
// haikal@List:i32:p
// haikal@BiNode:i32:p
// haikal@DList:i32:p
// haikal@Queue:i32:p
// haikal@Stack:i32:p
//---------------------------------------------------------------------------------------------------
// structs
//---------------------------------------------------------------------------------------------------
// haikal@Array:List_i32:s
// haikal@Array:vec3:s
// haikal@Array:vec4:s
// haikal@Map:vec3:s
// haikal@Map:Array_i8:s
// haikal@Map:Array_i32:s
//---------------------------------------------------------------------------------------------------
// unions
//---------------------------------------------------------------------------------------------------

#include <stdio.h>
#include <string.h>
#define CORE_IMPL
#include <core.h>

// #include "Arena.h"
#include "vec3.h"

#include <Array.h>
#include <Node.h>
#include <List.h>
#include <BiNode.h>
#include <DList.h>
#include <Queue.h>
#include <Stack.h>
#include <Map.h>

// #include "archetype.h"

#include "Component.h"

void Array_test(Arena *arena) {
    printf("Array_test:\n");
    printf("----------------------------\n");
    Array_i8 string = Array_i8_reserve(arena, 27);
    for (i32 i = 0; i < string.length; ++i) {
        string.data[i] = 0b01100000 | i + 1;
    }
    string.data[string.length - 1] = '\0';
    printf("string: %s\n", string.data);
    Array_i8_destroy(arena, &string);

    Array_vec3 vectors = Array_vec3_reserve(arena, 10);
    for (i32 i = 0; i < vectors.length; ++i) {
        vectors.data[i].x = 1.0f;
        vectors.data[i].y = (f32)i;
        vectors.data[i].z = 3.141592f;
    }
    for (i32 i = 0; i < vectors.length; ++i) { 
        printf("vectors[%d] = {%f, %f, %f}\n", i, vectors.data[i].x, vectors.data[i].y, vectors.data[i].z); 
    }
    Array_vec3_destroy(arena, &vectors);

    Array_i8 arr = {0};
    Array_i8_reserve(arena, 32);
    Array_i8_append(arena, &arr, 127);
    Array_i8_append(arena, &arr, 23);
    Array_i8_append(arena, &arr, 11);
    Array_i8_append(arena, &arr, 8);
    Array_i8_append(arena, &arr, 127);
    Array_i8_append(arena, &arr, 23);
    Array_i8_append(arena, &arr, 11);
    Array_i8_append(arena, &arr, 8);
    for (i32 i = 0; i < arr.length; ++i) { printf("arr[%d] = %d\n", i, arr.data[i]); }
    arr.length = 0;
    for (i32 i = 0; i < arr.length; ++i) { printf("arr[%d] = %d\n", i, arr.data[i]); }
    Array_i8_append(arena, &arr, 0x13);
    Array_i8_append(arena, &arr, 0x13);
    Array_i8_append(arena, &arr, 0x13);
    Array_i8_append(arena, &arr, 0x13);
    Array_i8_append(arena, &arr, 0x13);
    Array_i8_append(arena, &arr, 0x13);
    Array_i8_append(arena, &arr, 0x13);
    Array_i8_append(arena, &arr, 0x13);
    for (i32 i = 0; i < arr.length; ++i) { printf("arr[%d] = %d\n", i, arr.data[i]); }
    Array_i8_destroy(arena, &arr);
    printf("\n");
}

void List_test(Arena *arena) {
    printf("List_test:\n");
    List_i32 loi = {0};
    Node_i32 *node = NULL;
    List_i32_append(arena, &loi, 11);
    List_i32_append(arena, &loi, 22);
    List_i32_append(arena, &loi, 33);
    List_i32_append(arena, &loi, 44);
    List_i32_print(arena, &loi);

    node = List_i32_remove_at(arena, &loi, 0);
    if (node) {
        Node_i32_destroy(arena, &node);
    }
    List_i32_print(arena, &loi);
    node = List_i32_remove_at(arena, &loi, 1);
    if (node) {
        Node_i32_destroy(arena, &node);
    }
    List_i32_print(arena, &loi);
    node = List_i32_remove_at(arena, &loi, 1);
    if (node) {
        Node_i32_destroy(arena, &node);
    }
    List_i32_print(arena, &loi);
    node = List_i32_remove_at(arena, &loi, 0);
    if (node) {
        Node_i32_destroy(arena, &node);
    }
    List_i32_print(arena, &loi);
    if (loi.length == 0) {
        printf("list is empty\n");
    }
    List_i32_destroy(arena, &loi);

    printf("Array_List_i32:\n");
    Array_List_i32 arrayoflists = {0};
    List_i32 *list = Array_List_i32_append(arena, &arrayoflists, List_i32{});
    if (!list) { printf("list invalid!\n"); }
    List_i32_append(arena, list, 32);
    List_i32_append(arena, list, 22);
    List_i32_append(arena, list, 12);
    List_i32_print(arena, list);
    list = Array_List_i32_append(arena, &arrayoflists, List_i32{});
    if (!list) { printf("list invalid!\n"); }
    List_i32_append(arena, list, 16);
    List_i32_append(arena, list, 26);
    List_i32_append(arena, list, 36);
    List_i32_print(arena, list);
    printf("array.length = %llu\n", arrayoflists.length);
    for (i32 i = 0; i < arrayoflists.length; ++i) {
        printf("list[%d] = \n", i);
        List_i32 list = arrayoflists.data[i];
        List_i32_print(arena, &list);
    }
    printf("\n");
}

void DList_test(Arena *arena) {
    printf("DList_test:\n");
    printf("----------------------------\n");
    DList_i32 *loi = DList_i32_create(arena);
    BiNode_i32 *node = NULL;
    DList_i32_append(arena, loi, 11);
    DList_i32_append(arena, loi, 22);
    DList_i32_append(arena, loi, 33);
    DList_i32_append(arena, loi, 44);
    DList_i32_print(arena, loi);
    node = DList_i32_remove_at(arena, loi, 0); if (node) { BiNode_i32_destroy(arena, &node); }
    DList_i32_print(arena, loi);
    node = DList_i32_remove_at(arena, loi, 1); if (node) { BiNode_i32_destroy(arena, &node); }
    DList_i32_print(arena, loi);
    node = DList_i32_remove_at(arena, loi, 1); if (node) { BiNode_i32_destroy(arena, &node); }
    DList_i32_print(arena, loi);
    DList_i32_destroy(arena, &loi);
    printf("\n");
}

void Queue_test(Arena *arena) {
    printf("Queue_test:\n");
    printf("----------------------------\n");

    Queue_i32 *q = Queue_i32_create(arena);
    Queue_i32_print(arena, q);
    Queue_i32_enqueue(arena, q, 0);
    Queue_i32_print(arena, q);
    Queue_i32_enqueue(arena, q, 1);
    Queue_i32_print(arena, q);
    Queue_i32_enqueue(arena, q, 2);
    Queue_i32_print(arena, q);

    Node_i32 *node = NULL;
    i32 value = 0;

    node = Queue_i32_dequeue(arena, q);
    Node_i32_get(node, value);
    printf("node value: %d\n", value);
    Node_i32_destroy(arena, &node);
    Queue_i32_print(arena, q);

    node = Queue_i32_dequeue(arena, q);
    Node_i32_get(node, value);
    printf("node value: %d\n", value);
    Node_i32_destroy(arena, &node);
    Queue_i32_print(arena, q);

    node = Queue_i32_dequeue(arena, q);
    Node_i32_get(node, value);
    printf("node value: %d\n", value);
    Node_i32_destroy(arena, &node);
    Queue_i32_print(arena, q);

    node = Queue_i32_dequeue(arena, q);
    Node_i32_get(node, value);
    printf("node value: %d\n", value);
    Node_i32_destroy(arena, &node);
    Queue_i32_print(arena, q);

    Queue_i32_destroy(arena, &q);
    printf("\n");
}

void Stack_test(Arena *arena) {
    printf("Stack_test:\n");
    printf("----------------------------\n");
    Stack_i32 *stack = Stack_i32_create(arena);
    Node_i32 *node = NULL;
    Stack_i32_push(arena, stack, 32);
    Stack_i32_push(arena, stack, 12);
    Stack_i32_push(arena, stack, 22);
    Stack_i32_push(arena, stack, 42);
    Stack_i32_print(arena, stack);

    node = Stack_i32_pop(arena, stack);
    i32 value = 0;
    Stack_i32_print(arena, stack);

    node = Stack_i32_pop(arena, stack);
    Node_i32_get(node, value);
    Stack_i32_print(arena, stack);

    node = Stack_i32_pop(arena, stack);
    Node_i32_get(node, value);
    Stack_i32_print(arena, stack);

    node = Stack_i32_pop(arena, stack);
    Node_i32_get(node, value);
    Stack_i32_print(arena, stack);

    node = Stack_i32_pop(arena, stack);
    Node_i32_get(node, value);
    Stack_i32_print(arena, stack);

    Stack_i32_destroy(arena, &stack);
    printf("\n");
}

void Map_test(Arena *arena) {
    printf("Map_test:\n");
    puts("");
    printf("Map_i32:\n");
    Map_i32 *hashmap = Map_i32_create(arena);
    printf("hashmap length = %llu\n", Map_i32_length(arena, hashmap));
    if (!hashmap) {
        printf("nomem\n");
        exit(-1);
    }
    if (!Map_i32_set(arena, hashmap, "dog", 3)) {
        printf("nomem\n");
        exit(-1);
    }
    i32 *result = Map_i32_get(arena, hashmap, "dog");
    if (result) {
        printf("key = %s, val = %d\n", "dog", *result);
    }
    printf("hashmap length = %llu\n", Map_i32_length(arena, hashmap));
    Map_i32_destroy(arena, hashmap);

    puts("");
    printf("Map_vec:\n");
    Map_vec3 *hashmapvec = Map_vec3_create(arena);
    printf("hashmapvec length = %llu\n", Map_vec3_length(arena, hashmapvec));
    if (!hashmapvec) {
        printf("nomem\n");
        exit(-1);
    }
    if (!Map_vec3_set(arena, hashmapvec, "dog", vec3{1.f, 0.f, 0.f})) {
        printf("nomem\n");
        exit(-1);
    }
    printf("hashmapvec length = %llu\n", Map_vec3_length(arena, hashmapvec));
    if (!Map_vec3_set(arena, hashmapvec, "frog", vec3{0.f, 1.f, 0.f})) {
        printf("nomem\n");
        exit(-1);
    }
    printf("hashmapvec length = %llu\n", Map_vec3_length(arena, hashmapvec));
    vec3 *resultvec = Map_vec3_get(arena, hashmapvec, "dog");
    if (resultvec) {
        printf("key = %s, val = {%f, %f, %f}\n", "dog", resultvec->x, resultvec->y, resultvec->z);
    }
    printf("hashmapvec length = %llu\n", Map_vec3_length(arena, hashmapvec));
    printf("hash iterator...\n");
    MapIterator_vec3 itvec = MapIterator_vec3_create(arena, hashmapvec);
    while (MapIterator_vec3_next(arena, &itvec)) {
        printf("key = %s, val = {%f, %f, %f}\n", itvec.key, itvec.val.x, itvec.val.y, itvec.val.z);
    }
    Map_vec3_destroy(arena, hashmapvec);

    puts("");
    printf("Map_Array_i32:\n");
    Map_Array_i32 *hashmaparray = Map_Array_i32_create(arena);
    Array_i32 *resultarray = Map_Array_i32_get(arena, hashmaparray, "dog");
    if (!resultarray) {
        Map_Array_i32_set(arena, hashmaparray, "dog", Array_i32{0});
        resultarray = Map_Array_i32_get(arena, hashmaparray, "dog");
    }
    printf("key = %s, val = %p", "dog", resultarray);
    *resultarray = Array_i32_reserve(arena, 12);
    for (i32 i = 0; i < 12; i++) {
        resultarray->data[i] = i * i;
    }
    for (i32 i = 0; i < 12; i++) {
        printf("Array.data[%d] = %d\n", i, resultarray->data[i]);
    }
    printf("hashmapvec length = %llu\n", Map_Array_i32_length(arena, hashmaparray));

    printf("hash iterator...\n");
    MapIterator_Array_i32 itarr = MapIterator_Array_i32_create(arena, hashmaparray);
    while (MapIterator_Array_i32_next(arena, &itarr)) {
        printf("key = %s, val = {%llu, %llu, %p}\n", itarr.key, itarr.val.length, itarr.val.border, itarr.val.data);
        Array_i32_destroy(arena, &itarr.val);
    }
    Map_Array_i32_destroy(arena, hashmaparray);
    printf("\n");
}

structdef(Payload) {
    i32 id;
    i32 mx;
    char *str;
};

structdef(vec4i8) { i8 x; i8 y; i8 z; i8 w; };

void Arena_test(Arena *arena) {
    printf("Arena_test:\n");
    printf("----------------------------\n");
    // Arena arena = {0};
    // arenaInit(arena);

    const i32 len = 4;
    f32 *nums = (f32 *)arenaPushArray(arena, i32, len);
    for (i32 i = 0; i < len; ++i) {
        nums[i] = (f32)(i + 1);
    }
    for (i32 i = 0; i < len; ++i) {
        printf("%f ", nums[i]);
    }
    printf("\n");

    u8 *ptr = (u8 *)nums;
    for (i32 i = 0; i < sizeof(f32) * len; ++i) {
        printf("%02x ", ptr[i]);
    }
    printf("\n");

    void *pos = arena->cursor;

    char *str0 = strAlloc(arena, "this is a te");
    char *str1 = strAlloc(arena, "st string to");
    char *str2 = strAlloc(arena, "alloc bytes.");
    printf("%s\n", str0);
    printf("%s\n", str1);
    printf("%s\n", str2);

    strDealloc(arena, str2);
    str2 = strAlloc(arena, "fortitude");
    printf("%s\n", str0);
    printf("%s\n", str1);
    printf("%s\n", str2);

    Payload *pld = (Payload *)arenaPushStruct(arena, Payload);
    pld->id = 0xDEADBEEF;
    pld->mx = 0xCAFEBABE;
    pld->str = "Name0";
    arenaPop(arena, sizeof(Payload));
    pld = (Payload *)arenaPushStruct(arena, Payload);
    pld->id = 0xFFFFFFFF;
    pld->mx = 0xFFFFFFFF;
    pld->str = "Name0";
    arenaPop(arena, sizeof(Payload));

    arenaSetPos(arena, pos);
    i8 *x = (i8 *)arenaPush(arena, sizeof(i8), haikal_alignof(i8));
    *x = 0x13;

    arenaPop(arena, sizeof(Payload));


    arenaSetPos(arena, pos);
    nums = (f32 *)arenaPushArray(arena, i32, len);
    for (i32 i = 0; i < len; ++i) {
        nums[i] = (f32)(i + 1);
    }

    arenaClear(arena);

    // vec4i8 *vs = arenaPushArrayZero(&arena, vec4i8, 32);
    const i32 npts = 32;
    vec4i8 *vs = (vec4i8 *)arenaPushArray(arena, vec4i8, npts);
    for (i32 i = 0; i < npts; ++i) {
        vs[i].x = 0x10;
        vs[i].y = 0x11;
        vs[i].z = 0x12;
        vs[i].w = 0x13;
    }
    arenaPopArray(arena, vec4i8, npts);

    // arenaPrint(arena);
    arenaClear(arena);
    printf("\n");
}

void itos(int value, char* buffer) {
    char temp[12]; // Temporary buffer (enough for INT_MIN or INT_MAX with sign)
    int i = 0;
    bool is_negative = false;

    // Handle negative numbers
    if (value < 0) {
        is_negative = true;
        value = -value;
    }

    // Convert digits to characters in reverse order
    do {
        temp[i++] = (value % 10) + '0'; // Convert digit to character
        value /= 10;
    } while (value > 0);

    // Add negative sign if necessary
    if (is_negative) {
        temp[i++] = '-';
    }

    // Reverse the string into the buffer
    int j = 0;
    while (i > 0) {
        buffer[j++] = temp[--i];
    }
    buffer[j] = '\0'; // Null-terminate the string
}

i32 main(i32 argc, char *argv[]) {
    printf("haikal test begin...\n");
    Arena arena = {};
    arenaInit(&arena);
    Array_test(&arena);
    Map_test(&arena);
    List_test(&arena);
    DList_test(&arena);
    Queue_test(&arena);
    Stack_test(&arena);
    Arena_test(&arena);
    // Archetype_test();
    printf("haikal test end...\n");
    printf("----------------------------\n");
    // TODO: fix code gen for external files
    // for this to work, we need to read all the included files
    // compile_commands.json should be enough...
    // or, use a unity build and just include everything in main.c 
    // but LSP will die in Component.h...
    //---------------------------------------------------------------------------------------------------
    // Component comp = Component_create(10);
    // printf("comp.id = %d\n", comp.id);
    // Component_destroy(&comp);

    return 0;
}

#include <Array.c>
#include <Node.c>
#include <List.c>
#include <BiNode.c>
#include <DList.c>
#include <Queue.c>
#include <Stack.c>
#include <Map.c>
