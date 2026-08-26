cinclude "string.h"
import "cstd.i"

runtime_equal: proc<T>(a: T, b: T) -> b32 = {
    return memcmp(a.&, b.&, sizeof(T)) == 0;
}
