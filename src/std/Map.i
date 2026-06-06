import "memops.i"

#define I_RUNTIME_FNV_OFFSET 14695981039346656037ULL
#define I_RUNTIME_FNV_PRIME 1099511628211ULL
#define I_RUNTIME_MAP_INITIAL_CAPACITY 512

MapEntry: struct<T> = {
    key: *const char;
    val: T;
}

Map: struct<T> = {
    entries: *MapEntry<T>;
    length: usize;
    border: usize;
}

MapIterator: struct<T> = {
    key: *const char;
    val: T;
    _hashmap: *Map<T>;
    _index: usize;
}

runtime_cstr_copy: proc(arena: *memops_arena, key: *const char)->*const char = {
    if (key == null) {
        return null;
    }
    byte_count: usize = strlen(key) + 1;
    copy: *char = cast(memops_arena_push(arena, byte_count, alignof(char)), *char);
    if (copy == null) {
        printf("I runtime: cstring allocation failure\n");
        exit(1);
    }
    memcpy(copy, key, byte_count);
    return copy;
}

map_hash_key: proc(key: *const char)->u64 = {
    hash: u64 = I_RUNTIME_FNV_OFFSET;
    p: *const char = key;
    if (p == null) {
        return 0;
    }
    while (p[0] != 0) {
        hash = hash ^ cast(cast(p[0], u8), u64);
        hash *= I_RUNTIME_FNV_PRIME;
        p += 1;
    }
    return hash;
}

Map<T>set_entry: proc<T>(arena: *memops_arena, entries: *MapEntry<T>, border: usize, key: *const char, val: T, plength: *usize)->*const char = {
    hash: u64 = map_hash_key(key);
    index: usize = cast(hash & cast(border - 1, u64), usize);
    while (entries[index].key != null) {
        if (strcmp(key, entries[index].key) == 0) {
            entries[index].val = val;
            return entries[index].key;
        }
        index += 1;
        if (index >= border) {
            index = 0;
        }
    }
    stored_key: *const char = key;
    if (plength != null) {
        stored_key = runtime_cstr_copy(arena, key);
        if (stored_key == null) {
            return null;
        }
        plength[0] += 1;
    }
    entries[index].key = stored_key;
    entries[index].val = val;
    return stored_key;
}

Map<T>expand: proc<T>(arena: *memops_arena, hashmap: *Map<T>)->bool = {
    new_capacity: usize = hashmap[0].border * 2;
    if (new_capacity < hashmap[0].border) {
        return 0;
    }
    new_entries: *MapEntry<T> = cast(memops_arena_push_zero(arena, sizeof(MapEntry<T>) * new_capacity, alignof(MapEntry<T>)), *MapEntry<T>);
    if (new_entries == null) {
        return 0;
    }
    for (i: usize = 0; i < hashmap[0].border; i += 1) {
        entry: MapEntry<T> = hashmap[0].entries[i];
        if (entry.key != null) {
            Map<T>set_entry(arena, new_entries, new_capacity, entry.key, entry.val, null);
        }
    }
    hashmap[0].entries = new_entries;
    hashmap[0].border = new_capacity;
    return 1;
}

Map<T>create: proc<T>(arena: *memops_arena)->*Map<T> = {
    hashmap: *Map<T> = cast(memops_arena_push_zero(arena, sizeof(Map<T>), alignof(Map<T>)), *Map<T>);
    if (hashmap == null) {
        printf("I runtime: Map allocation failure\n");
        exit(1);
    }
    hashmap[0].length = 0;
    hashmap[0].border = I_RUNTIME_MAP_INITIAL_CAPACITY;
    hashmap[0].entries = cast(memops_arena_push_zero(arena, sizeof(MapEntry<T>) * hashmap[0].border, alignof(MapEntry<T>)), *MapEntry<T>);
    if (hashmap[0].entries == null) {
        printf("I runtime: Map entries allocation failure\n");
        exit(1);
    }
    return hashmap;
}

Map<T>destroy: proc<T>(arena: *memops_arena, hashmap: *Map<T>)->void = {
    if (hashmap != null) {
        hashmap[0].entries = null;
        hashmap[0].length = 0;
        hashmap[0].border = 0;
    }
}

Map<T>get: proc<T>(arena: *memops_arena, hashmap: *Map<T>, key: *const char)->*T = {
    if (hashmap == null or hashmap[0].entries == null or hashmap[0].border == 0) {
        return null;
    }
    hash: u64 = map_hash_key(key);
    index: usize = cast(hash & cast(hashmap[0].border - 1, u64), usize);
    while (hashmap[0].entries[index].key != null) {
        if (strcmp(key, hashmap[0].entries[index].key) == 0) {
            return hashmap[0].entries[index].val.&;
        }
        index += 1;
        if (index >= hashmap[0].border) {
            index = 0;
        }
    }
    return null;
}

Map<T>set: proc<T>(arena: *memops_arena, hashmap: *Map<T>, key: *const char, val: T)->*const char = {
    if (hashmap[0].length >= hashmap[0].border / 2) {
        if (!Map<T>expand(arena, hashmap)) {
            return null;
        }
    }
    return Map<T>set_entry(arena, hashmap[0].entries, hashmap[0].border, key, val, hashmap[0].length.&);
}

Map<T>try_emplace: proc<T>(arena: *memops_arena, hashmap: *Map<T>, key: *const char, val: T)->*T = {
    result: *T = Map<T>get(arena, hashmap, key);
    if (result == null) {
        Map<T>set(arena, hashmap, key, val);
        result = Map<T>get(arena, hashmap, key);
    }
    return result;
}

Map<T>length: proc<T>(arena: *memops_arena, hashmap: *Map<T>)->usize = {
    if (hashmap == null) {
        return 0;
    }
    return hashmap[0].length;
}

MapIterator<T>create: proc<T>(arena: *memops_arena, hashmap: *Map<T>)->MapIterator<T> = {
    it: MapIterator<T> = {};
    it._hashmap = hashmap;
    it._index = 0;
    return it;
}

MapIterator<T>next: proc<T>(arena: *memops_arena, it: *MapIterator<T>)->bool = {
    hashmap: *Map<T> = it[0]._hashmap;
    if (hashmap == null) {
        return 0;
    }
    while (it[0]._index < hashmap[0].border) {
        i: usize = it[0]._index;
        it[0]._index += 1;
        if (hashmap[0].entries[i].key != null) {
            entry: MapEntry<T> = hashmap[0].entries[i];
            it[0].key = entry.key;
            it[0].val = entry.val;
            return 1;
        }
    }
    return 0;
}
