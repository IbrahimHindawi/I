cinclude "stdio.h"
cinclude "stdlib.h"
import "cstd.i"

Result: struct<T> = {
    ok: bool;
    value: T;
    error: i32;
}

Result<T>ok: proc<T>(value: T)->Result<T> = {
    result: Result<T> = {};
    result.ok = 1;
    result.value = value;
    result.error = 0;
    return result;
}

Result<T>err: proc<T>(error: i32)->Result<T> = {
    result: Result<T> = {};
    result.ok = 0;
    result.error = error;
    return result;
}

Result<T>is_ok: proc<T>(result: Result<T>)->bool = {
    return result.ok;
}

Result<T>is_err: proc<T>(result: Result<T>)->bool = {
    return !result.ok;
}

Result<T>unwrap: proc<T>(result: Result<T>)->T = {
    if (!result.ok) {
        printf("I runtime: Result unwrap on error %d\n", result.error);
        exit(1);
    }
    return result.value;
}

Result<T>unwrap_or: proc<T>(result: Result<T>, fallback: T)->T = {
    if (result.ok) {
        return result.value;
    }
    return fallback;
}
