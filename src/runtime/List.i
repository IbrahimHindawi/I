import "memops.i"
import "Node.i"
import "Equal.i"

List: struct<T> = {
    head: *Node<T>;
    length: usize;
}

List<T>create: proc<T>(arena: *memops_arena)->*List<T> = {
    list: *List<T> = cast(memops_arena_push_zero(arena, sizeof(List<T>), alignof(List<T>)), *List<T>);
    if (list == null) {
        printf("I runtime: List allocation failure\n");
        exit(1);
    }
    return list;
}

List<T>append: proc<T>(arena: *memops_arena, list: *List<T>, item: T)->void = {
    node: *Node<T> = Node<T>create(arena, item);
    if (list[0].head == null) {
        list[0].head = node;
        list[0].length += 1;
        return;
    }
    iter: *Node<T> = list[0].head;
    while (iter[0].next != null) {
        iter = iter[0].next;
    }
    iter[0].next = node;
    list[0].length += 1;
}

List<T>prepend: proc<T>(arena: *memops_arena, list: *List<T>, item: T)->void = {
    node: *Node<T> = Node<T>create(arena, item);
    node[0].next = list[0].head;
    list[0].head = node;
    list[0].length += 1;
}

List<T>insert_at: proc<T>(arena: *memops_arena, list: *List<T>, item: T, index: i32)->void = {
    if (list == null or index < 0 or cast(index, usize) > list[0].length) {
        printf("I runtime: List invalid index\n");
        return;
    }
    if (index == 0) {
        List<T>prepend(arena, list, item);
        return;
    }
    if (cast(index, usize) == list[0].length) {
        List<T>append(arena, list, item);
        return;
    }
    prev: *Node<T> = List<T>get_at(arena, list, index - 1);
    if (prev == null) {
        return;
    }
    node: *Node<T> = Node<T>create(arena, item);
    node[0].next = prev[0].next;
    prev[0].next = node;
    list[0].length += 1;
}

List<T>get_length: proc<T>(arena: *memops_arena, list: *List<T>)->usize = {
    if (list == null) {
        return 0;
    }
    return list[0].length;
}

List<T>get_at: proc<T>(arena: *memops_arena, list: *List<T>, index: i32)->*Node<T> = {
    if (list == null or index < 0 or cast(index, usize) >= list[0].length) {
        return null;
    }
    iter: *Node<T> = list[0].head;
    count: i32 = 0;
    while (iter != null) {
        if (count == index) {
            return iter;
        }
        iter = iter[0].next;
        count += 1;
    }
    return null;
}

List<T>remove_at: proc<T>(arena: *memops_arena, list: *List<T>, index: i32)->*Node<T> = {
    if (list == null or index < 0 or cast(index, usize) >= list[0].length) {
        return null;
    }
    iter: *Node<T> = list[0].head;
    prev: *Node<T> = null;
    count: i32 = 0;
    while (iter != null) {
        if (count == index) {
            if (prev == null) {
                list[0].head = iter[0].next;
            } else {
                prev[0].next = iter[0].next;
            }
            iter[0].next = null;
            list[0].length -= 1;
            return iter;
        }
        prev = iter;
        iter = iter[0].next;
        count += 1;
    }
    return null;
}

List<T>remove: proc<T>(arena: *memops_arena, list: *List<T>, item: T)->*Node<T> = {
    if (list == null or list[0].length == 0) {
        return null;
    }
    iter: *Node<T> = list[0].head;
    prev: *Node<T> = null;
    while (iter != null) {
        if (runtime_equal<T>(iter[0].data, item)) {
            if (prev == null) {
                list[0].head = iter[0].next;
            } else {
                prev[0].next = iter[0].next;
            }
            iter[0].next = null;
            list[0].length -= 1;
            return iter;
        }
        prev = iter;
        iter = iter[0].next;
    }
    return null;
}

List<T>destroy: proc<T>(arena: *memops_arena, list: *List<T>)->void = {
    if (list != null) {
        list[0].head = null;
        list[0].length = 0;
    }
}

List<T>is_empty: proc<T>(list: *List<T>)->bool = {
    return list == null or list[0].length == 0;
}
