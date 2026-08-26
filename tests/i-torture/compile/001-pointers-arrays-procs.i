TortureNode: struct = {
    value: i32;
    next: *TortureNode;
}

TortureCallback: alias = *proc(node: *TortureNode, scale: i32) -> i32;

torture_visit: proc(node: *TortureNode, scale: i32) -> i32 = {
    if (node == cast(0, *TortureNode)) {
        return 0;
    }
    return node[0].value * scale;
}

torture_apply: proc(nodes: *TortureNode, count: i32, cb: TortureCallback) -> i32 = {
    total: i32 = 0;
    for (i: i32 = 0; i < count; i += 1) {
        total += cb(nodes[i].&, i + 1);
    }
    return total;
}

main: proc() -> i32 = {
    nodes: [3]TortureNode = {
        [0] = {.value = 2},
        [1] = {.value = 3},
        [2] = {.value = 5},
    };
    nodes[0].next = nodes[1].&;
    nodes[1].next = nodes[2].&;
    return torture_apply(nodes, 3, torture_visit);
}
