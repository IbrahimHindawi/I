enum i_torture_mode {
    I_TORTURE_ZERO,
    I_TORTURE_ONE,
    I_TORTURE_TWO,
};

int i_torture_control(int x, enum i_torture_mode mode) {
    int total = 0;
    for (int i = 0; i < x; ++i) {
        if ((i & 1) == 0) continue;
        total += i;
    }
    switch (mode) {
        case I_TORTURE_ZERO: total += 1; break;
        case I_TORTURE_ONE: total += 2; break;
        default: total += 3; break;
    }
    return total > 10 ? total : -total;
}
