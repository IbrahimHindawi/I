typedef struct i_smoke_pair {
    int x;
    int y;
} i_smoke_pair;

enum i_smoke_kind {
    I_SMOKE_A = 1,
    I_SMOKE_B = 2,
};

static int i_smoke_sum(i_smoke_pair pair, enum i_smoke_kind kind) {
    int total = pair.x + pair.y;
    switch (kind) {
        case I_SMOKE_A: total += 3; break;
        case I_SMOKE_B: total += 5; break;
    }
    return total;
}

int i_smoke_entry(void) {
    i_smoke_pair pair = {1, 2};
    return i_smoke_sum(pair, I_SMOKE_B);
}
