import "memops.i"
import "BiNode.i"
import "Equal.i"

DList: struct<T> = {
    head: *BiNode<T>;
    tail: *BiNode<T>;
    length: usize;
}

DList<T>create: proc<T>(arena: *memops_arena)->*DList<T> = {
    list: *DList<T> = cast(memops_arena_push_zero(arena, sizeof(DList<T>), alignof(DList<T>)), *DList<T>);
    if (list == null) {
        printf("I runtime: DList allocation failure\n");
        exit(1);
    }
    return list;
}

DList<T>append: proc<T>(arena: *memops_arena, list: *DList<T>, item: T)->void = {
    node: *BiNode<T> = BiNode<T>create(arena, item);
    if (list[0].head == null) {
        list[0].head = node;
        list[0].tail = node;
        list[0].length = 1;
        return;
    }
    node[0].prev = list[0].tail;
    list[0].tail[0].next = node;
    list[0].tail = node;
    list[0].length += 1;
}

DList<T>prepend: proc<T>(arena: *memops_arena, list: *DList<T>, item: T)->void = {
    node: *BiNode<T> = BiNode<T>create(arena, item);
    if (list[0].head == null) {
        list[0].head = node;
        list[0].tail = node;
        list[0].length = 1;
        return;
    }
    node[0].next = list[0].head;
    list[0].head[0].prev = node;
    list[0].head = node;
    list[0].length += 1;
}

DList<T>insert_at: proc<T>(arena: *memops_arena, list: *DList<T>, item: T, index: i32)->void = {
    if (list == null or index < 0 or cast(index, usize) > list[0].length) {
        printf("I runtime: DList invalid index\n");
        return;
    }
    if (index == 0) {
        DList<T>prepend(arena, list, item);
        return;
    }
    if (cast(index, usize) == list[0].length) {
        DList<T>append(arena, list, item);
        return;
    }
    iter: *BiNode<T> = DList<T>get_at(arena, list, index);
    if (iter == null) {
        return;
    }
    node: *BiNode<T> = BiNode<T>create(arena, item);
    node[0].next = iter;
    node[0].prev = iter[0].prev;
    if (iter[0].prev != null) {
        iter[0].prev[0].next = node;
    }
    iter[0].prev = node;
    list[0].length += 1;
}

DList<T>get_length: proc<T>(arena: *memops_arena, list: *DList<T>)->usize = {
    if (list == null) {
        return 0;
    }
    return list[0].length;
}

DList<T>get_at: proc<T>(arena: *memops_arena, list: *DList<T>, index: i32)->*BiNode<T> = {
    if (list == null or index < 0 or cast(index, usize) >= list[0].length) {
        return null;
    }
    iter: *BiNode<T> = list[0].head;
    for (i: i32 = 0; iter != null and i < index; i += 1) {
        iter = iter[0].next;
    }
    return iter;
}

DList<T>remove_node: proc<T>(arena: *memops_arena, list: *DList<T>, node: *BiNode<T>)->*BiNode<T> = {
    if (list == null or node == null or list[0].length == 0) {
        return null;
    }
    if (node[0].prev != null) {
        node[0].prev[0].next = node[0].next;
    }
    if (node[0].next != null) {
        node[0].next[0].prev = node[0].prev;
    }
    if (node == list[0].head) {
        list[0].head = node[0].next;
    }
    if (node == list[0].tail) {
        list[0].tail = node[0].prev;
    }
    node[0].prev = null;
    node[0].next = null;
    list[0].length -= 1;
    return node;
}

DList<T>remove_at: proc<T>(arena: *memops_arena, list: *DList<T>, index: i32)->*BiNode<T> = {
    node: *BiNode<T> = DList<T>get_at(arena, list, index);
    return DList<T>remove_node(arena, list, node);
}

DList<T>remove: proc<T>(arena: *memops_arena, list: *DList<T>, item: T)->*BiNode<T> = {
    if (list == null or list[0].length == 0) {
        return null;
    }
    iter: *BiNode<T> = list[0].head;
    while (iter != null) {
        if (runtime_equal<T>(iter[0].data, item)) {
            return DList<T>remove_node(arena, list, iter);
        }
        iter = iter[0].next;
    }
    return null;
}

DList<T>destroy: proc<T>(arena: *memops_arena, list: *DList<T>)->void = {
    if (list != null) {
        list[0].head = null;
        list[0].tail = null;
        list[0].length = 0;
    }
}

DList<T>is_empty: proc<T>(list: *DList<T>)->bool = {
    return list == null or list[0].length == 0;
}
