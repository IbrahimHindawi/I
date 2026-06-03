#pragma once
#include <stdint.h>
#include <stdlib.h>

#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>

static inline uint64_t memops_os_page_size(void) {
    SYSTEM_INFO systeminfo = {0};
    GetSystemInfo(&systeminfo);
    return (uint64_t)systeminfo.dwPageSize;
}

static inline uint8_t *memops_os_reserve(uint64_t size) {
    return (uint8_t *)VirtualAlloc(NULL, (SIZE_T)size, MEM_RESERVE, PAGE_NOACCESS);
}

static inline void *memops_os_commit(void *ptr, uint64_t size) {
    return VirtualAlloc(ptr, (SIZE_T)size, MEM_COMMIT, PAGE_READWRITE);
}

static inline void memops_os_debug_break(void) {
    DebugBreak();
}
#else
static inline uint64_t memops_os_page_size(void) {
    return 4096;
}

static inline uint8_t *memops_os_reserve(uint64_t size) {
    return (uint8_t *)malloc((size_t)size);
}

static inline void *memops_os_commit(void *ptr, uint64_t size) {
    (void)size;
    return ptr;
}

static inline void memops_os_debug_break(void) {
}
#endif
