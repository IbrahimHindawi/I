typedef int (*i_torture_callback)(int value, void *user);

static int i_torture_apply(int *values, int count, i_torture_callback cb, void *user) {
    int total = 0;
    for (int i = 0; i < count; ++i) {
        total += cb(values[i], user);
    }
    return total;
}

static int i_torture_scale(int value, void *user) {
    int *scale = (int *)user;
    return value * *scale;
}

int i_torture_callbacks(void) {
    int values[3] = {1, 2, 3};
    int scale = 4;
    return i_torture_apply(values, 3, i_torture_scale, &scale);
}
