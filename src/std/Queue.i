import "memops.i"
import "Node.i"

Queue: struct<T> = {
    head: *Node<T>;
    tail: *Node<T>;
    length: usize;
}

Queue<T>create: proc<T>(arena: *memops_arena)->*Queue<T> = {
    queue: *Queue<T> = cast(memops_arena_push_zero(arena, sizeof(Queue<T>), alignof(Queue<T>)), *Queue<T>);
    if (queue == null) {
        printf("I runtime: Queue allocation failure\n");
        exit(1);
    }
    return queue;
}

Queue<T>enqueue: proc<T>(arena: *memops_arena, queue: *Queue<T>, data: T)->void = {
    node: *Node<T> = Node<T>create(arena, data);
    if (queue[0].head == null) {
        queue[0].head = node;
        queue[0].tail = node;
        queue[0].length = 1;
        return;
    }
    queue[0].tail[0].next = node;
    queue[0].tail = node;
    queue[0].length += 1;
}

Queue<T>dequeue: proc<T>(arena: *memops_arena, queue: *Queue<T>)->*Node<T> = {
    if (queue == null or queue[0].head == null) {
        return null;
    }
    node: *Node<T> = queue[0].head;
    queue[0].head = node[0].next;
    node[0].next = null;
    queue[0].length -= 1;
    if (queue[0].length == 0) {
        queue[0].tail = null;
    }
    return node;
}

Queue<T>peek: proc<T>(arena: *memops_arena, queue: *Queue<T>)->*Node<T> = {
    if (queue == null) {
        return null;
    }
    return queue[0].head;
}

Queue<T>destroy: proc<T>(arena: *memops_arena, queue: *Queue<T>)->void = {
    if (queue != null) {
        queue[0].head = null;
        queue[0].tail = null;
        queue[0].length = 0;
    }
}

Queue<T>is_empty: proc<T>(queue: *Queue<T>)->b32 = {
    return queue == null or queue[0].length == 0;
}
