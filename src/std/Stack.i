import "memops.i"
import "Node.i"

Stack: struct<T> = {
    head: *Node<T>;
    length: usize;
}

Stack<T>create: proc<T>(arena: *memops_arena) -> *Stack<T> = {
    stack: *Stack<T> = cast(memops_arena_push_zero(arena, sizeof(Stack<T>), alignof(Stack<T>)), *Stack<T>);
    if (stack == null) {
        printf("I runtime: Stack allocation failure\n");
        exit(1);
    }
    return stack;
}

Stack<T>push: proc<T>(arena: *memops_arena, stack: *Stack<T>, data: T) -> void = {
    node: *Node<T> = Node<T>create(arena, data);
    node[0].next = stack[0].head;
    stack[0].head = node;
    stack[0].length += 1;
}

Stack<T>pop: proc<T>(arena: *memops_arena, stack: *Stack<T>) -> *Node<T> = {
    if (stack == null or stack[0].head == null) {
        return null;
    }
    node: *Node<T> = stack[0].head;
    stack[0].head = node[0].next;
    node[0].next = null;
    stack[0].length -= 1;
    return node;
}

Stack<T>peek: proc<T>(arena: *memops_arena, stack: *Stack<T>) -> *Node<T> = {
    if (stack == null) {
        return null;
    }
    return stack[0].head;
}

Stack<T>destroy: proc<T>(arena: *memops_arena, stack: *Stack<T>) -> void = {
    if (stack != null) {
        stack[0].head = null;
        stack[0].length = 0;
    }
}

Stack<T>is_empty: proc<T>(stack: *Stack<T>) -> b32 = {
    return stack == null or stack[0].length == 0;
}
