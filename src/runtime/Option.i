cinclude "stdio.h"
cinclude "stdlib.h"

Option: struct<T> = {
    has_value: bool;
    value: T;
}

Option<T>some: proc<T>(value: T)->Option<T> = {
    option: Option<T> = {};
    option.has_value = 1;
    option.value = value;
    return option;
}

Option<T>none: proc<T>()->Option<T> = {
    option: Option<T> = {};
    option.has_value = 0;
    return option;
}

Option<T>is_some: proc<T>(option: Option<T>)->bool = {
    return option.has_value;
}

Option<T>is_none: proc<T>(option: Option<T>)->bool = {
    return !option.has_value;
}

Option<T>unwrap: proc<T>(option: Option<T>)->T = {
    if (!option.has_value) {
        printf("I runtime: Option unwrap on none\n");
        exit(1);
    }
    return option.value;
}

Option<T>unwrap_or: proc<T>(option: Option<T>, fallback: T)->T = {
    if (option.has_value) {
        return option.value;
    }
    return fallback;
}
