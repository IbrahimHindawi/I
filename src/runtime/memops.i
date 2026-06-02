memops_arena_initialize:proc(arena:*memops_arena)->void = { external; }
memops_arena_push:proc(arena:*memops_arena, alloc_size:u64, align:u64)->*void = { external; }
memops_arena_realloc_:proc(arena:*memops_arena, new_alloc_size:u64, old_ptr:*void, old_alloc_size:u64, align:u64)->*void = { external; }
memops_arena_temp_begin:proc(arena:*memops_arena)->memops_arena_temp = { external; }
memops_arena_temp_end:proc(temp:memops_arena_temp)->void = { external; }
memops_arena_clear:proc(arena:*memops_arena)->void = { external; }
