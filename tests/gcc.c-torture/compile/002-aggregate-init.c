struct i_torture_vec {
    int x;
    int y;
};

union i_torture_word {
    unsigned u;
    int i;
};

struct i_torture_packet {
    struct i_torture_vec points[2];
    union i_torture_word word;
};

int i_torture_aggregate(void) {
    struct i_torture_packet packet = {
        .points = {
            [0] = {.x = 1, .y = 2},
            [1] = {.x = 3, .y = 4},
        },
        .word = {.i = 5},
    };
    return packet.points[0].x + packet.points[1].y + packet.word.i;
}
