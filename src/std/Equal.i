cinclude "string.h"

runtime_equal: proc<T>(a: T, b: T)->bool = {
    return memcmp(a.&, b.&, sizeof(T)) == 0;
}
