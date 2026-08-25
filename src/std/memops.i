#define max_alloc_size 0x10000000000

cinclude "std/memops_os.h"
cinclude "stdlib.h"
cinclude "stdio.h"
cinclude "string.h"
import "cstd.i"

memops_os_page_size: proc[external]()->u64 = {}
memops_os_reserve: proc[external](size: u64)->*u8 = {}
memops_os_commit: proc[external](ptr: *void, size: u64)->*void = {}
memops_os_debug_break: proc[external]()->void = {}

memops_arena: struct = {
    base: *u8;
    cursor: *u8;
    previous: *u8;
    used: u64;
    pagesize: u64;
    npages: u64;
}

memops_arena_temp: struct = {
    arena: *memops_arena;
    used: u64;
}

memops_is_power_of_two: proc(x: usize)->b32 = {
    return (x & (x - 1)) == 0;
}

memops_align_forward: proc(ptr: usize, align: usize)->usize = {
    p: usize = ptr;
    a: usize = align;
    modulo: usize = p & (a - 1);
    if (modulo != 0) {
        p += a - modulo;
    }
    return p;
}

memops_arena_initialize: proc(arena: *memops_arena)->void = {
    arena[0].pagesize = memops_os_page_size();
    arena[0].base = memops_os_reserve(max_alloc_size);
    if (arena[0].base == null) {
        exit(1);
    }
    arena[0].cursor = arena[0].base;
    arena[0].previous = arena[0].base;
    arena[0].used = 0;
    arena[0].npages = 0;
}

memops_arena_push: proc(arena: *memops_arena, alloc_size: u64, align: u64)->*void = {
    base_addr: usize = cast(arena[0].base, usize);
    curr_addr: usize = base_addr + cast(arena[0].used, usize);
    aligned_addr: usize = memops_align_forward(curr_addr, cast(align, usize));
    diff: u64 = cast(aligned_addr - curr_addr, u64);
    needed: u64 = arena[0].used + alloc_size + diff;
    committed: u64 = arena[0].pagesize * arena[0].npages;

    if (needed > committed) {
        missing_bytes: u64 = needed - committed;
        pages_to_commit: u64 = (missing_bytes + arena[0].pagesize - 1) / arena[0].pagesize;
        commit_addr: usize = base_addr + cast(committed, usize);
        r: *void = memops_os_commit(cast(commit_addr, *void), pages_to_commit * arena[0].pagesize);
        if (r == null) {
            printf("memops_arena commit failed at %p!\n", cast(commit_addr, *void));
            memops_os_debug_break();
            exit(1);
        }
        arena[0].npages += pages_to_commit;
    }

    if (needed > max_alloc_size) {
        printf("memops_arena maximum size exceeded!\n");
        exit(1);
    }

    arena[0].used += alloc_size + diff;
    arena[0].previous = arena[0].cursor;
    arena[0].cursor = cast(aligned_addr, *u8);
    oldpos: *void = arena[0].cursor;
    arena[0].cursor += alloc_size;
    return oldpos;
}

memops_arena_push_zero: proc(arena: *memops_arena, alloc_size: u64, align: u64)->*void = {
    p: *void = memops_arena_push(arena, alloc_size, align);
    memset(p, 0, alloc_size);
    return p;
}

memops_arena_get_pos: proc(arena: *memops_arena)->*void = {
    return arena[0].base + arena[0].used;
}

memops_arena_set_pos: proc(arena: *memops_arena, pos: *void)->*void = {
    arena[0].cursor = cast(pos, *u8);
    arena[0].used = cast(arena[0].cursor - arena[0].base, u64);
    return arena[0].cursor;
}

memops_arena_pop: proc(arena: *memops_arena, alloc_size: u64)->*void = {
    arena[0].cursor -= alloc_size;
    arena[0].used -= alloc_size;
    return arena[0].cursor;
}

memops_arena_realloc_: proc(arena: *memops_arena, new_alloc_size: u64, old_ptr: *void, old_alloc_size: u64, align: u64)->*void = {
    new_ptr: *void = memops_arena_push(arena, new_alloc_size, align);
    memcpy(new_ptr, old_ptr, old_alloc_size);
    return new_ptr;
}

memops_arena_clear: proc(arena: *memops_arena)->void = {
    arena[0].cursor = arena[0].base;
    arena[0].used = 0;
}

memops_arena_temp_begin: proc(arena: *memops_arena)->memops_arena_temp = {
    t: memops_arena_temp = {};
    t.arena = arena;
    t.used = arena[0].used;
    return t;
}

memops_arena_temp_end: proc(temp: memops_arena_temp)->void = {
    temp.arena[0].used = temp.used;
    temp.arena[0].cursor = temp.arena[0].base + temp.used;
}
