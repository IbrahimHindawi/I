cinclude "string.h"
import "cstd.i"

runtime_equal: proc<T>(a: T, b: T)->bool = {
    return memcmp(a.&, b.&, sizeof(T)) == 0;
}
