#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#endif

#include <core.h>
#include <stdalign.h>
#define maxallocsize 0x10000000000

typedef struct Arena Arena;
struct Arena {
    u8 *base;
    u8 *cursor;
    u8 *previous;
    u64 used;
    u64 pagesize;
    u64 npages;
};

typedef struct hkArray_Node hkArray_Node;
structdef(Node) {
    i32 length;
    // Node *nodes;
    hkArray_Node *nodes;
};

void arenaInit(Arena *arena) {
    SYSTEM_INFO systeminfo = {0};
    GetSystemInfo(&systeminfo);
    // printf("allocationgranularity = %lu\n", systeminfo.dwAllocationGranularity);
    // printf("page size = %lu\n", systeminfo.dwPageSize);
    arena->pagesize = systeminfo.dwPageSize; // 4096 or 0x1000
    arena->base = VirtualAlloc(NULL, maxallocsize, MEM_RESERVE, PAGE_NOACCESS); // reserve 1,099,511,627,776 bytes
    if (!arena->base) { exit(EXIT_FAILURE); }
    arena->cursor = arena->base;
    arena->previous = arena->base;
}

bool isPowerOfTwo(uintptr_t x) {
	return (x & (x-1)) == 0;
}

uintptr_t memoryAlignForward(uintptr_t ptr, size_t align) {
	uintptr_t p;
    uintptr_t a;
    uintptr_t modulo;

	assert(isPowerOfTwo(align));

	p = ptr;
	a = (uintptr_t)align;
	// Same as (p % a) but faster as 'a' is a power of two
	modulo = p & (a-1);

	if (modulo != 0) {
		// If 'p' address is not aligned, push the address to the
		// next value which is aligned
		p += a - modulo;
	}
	return p;
}

void *arenaPush(Arena *arena, u64 allocsize, u64 align) {
	// Align 'curr_offset' forward to the specified alignment
	uintptr_t curr_ptr = (uintptr_t)arena->base + (uintptr_t)arena->used;
	uintptr_t offset = memoryAlignForward(curr_ptr, align);
    // Change to relative offset
	// offset -= (uintptr_t)arena->base;
    // uintptr_t diff = curr_ptr - offset;
    uintptr_t diff = offset - curr_ptr;
    // printf("align = %llu\n", align);
    // printf("offset = %llu\n", offset);
    if (arena->used + allocsize + diff > arena->pagesize * arena->npages) {
        i32 npages = ceil((f32)(arena->used + allocsize + diff) / arena->pagesize);
        arena->npages = npages;
        arena->base = VirtualAlloc(arena->base, arena->pagesize * arena->npages, MEM_COMMIT, PAGE_READWRITE);
        if (!arena->base) { exit(EXIT_FAILURE); }
    }
    if (arena->used > maxallocsize) {
        printf("Memory allocation failure! Maximum memory reached!\n");
        exit(EXIT_FAILURE);
    }
    arena->used += allocsize + diff;
    // save cursor before push
    arena->previous = arena->cursor;
    // align cursor
    arena->cursor += diff;
    // save aligned cursor
    void *oldpos = arena->cursor;
    // allocate
    arena->cursor += allocsize;
    // return aligned cursor
    return oldpos;
}

void *arenaPushZero(Arena *arena, u64 allocsize, u64 align) {
    arenaPush(arena, allocsize, align);
    memcpy(arena->cursor, 0, allocsize);
    return arena->cursor;
}

void *arenaSetPos(Arena *arena, void *pos) {
    u64 diff = (u64)arena->cursor - (u64)pos;
    arena->used -= diff;
    arena->cursor = pos;
    return arena->cursor;
}

void arenaClear(Arena *arena) {
    arena->cursor = arena->base;
    arena->used = 0;
}

void *arenaPop(Arena *arena, u64 allocsize) {
    // leads to fragmentation, needs solution
    // uintptr_t diff = arena->cursor - arena->previous;
    // uintptr_t offset = diff - allocsize;
    // uintptr_t oldprevious = arena->previous;
    // arena->cursor = arena->previous;
    // arena->cursor -= allocsize + offset;
    arena->cursor -= allocsize;
    arena->used -= allocsize;
    return arena->cursor;
}

void *arenaGetPos(Arena *arena) {
    return arena->cursor;
}

char *strAlloc(Arena *arena, char *input_str) {
    u64 input_str_len = strlen(input_str) + 1;
    char *output_str = arenaPush(arena, sizeof(char) * input_str_len, _Alignof(char));
    memcpy(output_str, input_str, input_str_len);
    // output_str[0] = 'a';
    // for (i32 i = 0; i < input_str_len; ++i) {
    //     output_str[i] = input_str[i];
    // }
    return output_str;
}

void *strDealloc(Arena *arena, const char *input_str) {
    u64 input_str_len = strlen(input_str) + 1;
    return arenaPop(arena, input_str_len);
}

void arenaPrint(Arena *arena) {
    printf("Memory Dump: %llu bytes allocated.\n", arena->used);
    printf("%p: ", arena->base);
    for (i32 i = 0; i < arena->used; ++i) {
        if(i % 8 == 0 && i != 0) {
            printf("\n");
            printf("%p: ", &arena->base[i]);
        }
        printf("%02x ", arena->base[i]);
    }
    printf("Memory Dump: End.\n");
}

#define arenaPushStruct(arena, type) arenaPush(arena, sizeof(type), _Alignof(type))
#define arenaPushArray(arena, type, count) arenaPush(arena, sizeof(type) * count, _Alignof(type))
#define arenaPopArray(arena, type, count) arenaPop(arena, sizeof(type) * count)
#define arenaPushArrayZero(arena, type, count) arenaPushZero(arena, sizeof(type) * count, _Alignof(type))
