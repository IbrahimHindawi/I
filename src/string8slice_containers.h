#pragma  once
#include "core.h"
#include "saha.h"
#include "Array.h"
#include "string8slice.h"
#include "string8slice_containers.h"

Array_string8slice string8slice_split(memops_arena *arena, string8slice src, char sep);
Array_string8slice string8slice_split_from_string8(memops_arena *arena, string8 s, char sep);

