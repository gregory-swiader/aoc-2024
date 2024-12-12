#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_D 150
#define DELIM '.'

struct state {
        char    board[MAX_D][MAX_D];
        bool    vis[MAX_D][MAX_D];
        int32_t rows, cols;
};

struct view {
        char w[2][2];
};

struct res {
        int64_t area, perimeter, doublecorners;
};

void add_delim(struct state *s, int32_t cols) {
        for (int32_t i = 0; i < s->cols; i++)
                s->board[s->rows][i] = DELIM;
        s->rows++;
}

void add_line(struct state *s, char *line) {
        memcpy(&s->board[s->rows][1], line, s->cols - 2);
        s->board[s->rows][0]           = DELIM;
        s->board[s->rows][s->cols - 1] = DELIM;
        s->rows++;
}

void readInput(struct state *s, FILE *f) {
        char  *line = NULL;
        size_t n = 0, linelen = 0;

        if (-1 == (linelen = getline(&line, &n, f)))
                return;

        s->cols = linelen + 1;
        add_delim(s, linelen);
        add_line(s, line);

        while (-1 != getline(&line, &linelen, f))
                add_line(s, line);

        add_delim(s, linelen);

        free(line);
}

int32_t perimeter(struct state *s, int32_t i, int32_t j) {
        char    c    = s->board[i][j];
        int32_t ret  = 0;
        ret         += s->board[i - 1][j] != c;
        ret         += s->board[i + 1][j] != c;
        ret         += s->board[i][j - 1] != c;
        ret         += s->board[i][j + 1] != c;
        return ret;
}

int32_t count_corners(struct view v) {
        bool vertical   = v.w[0][0] == v.w[1][0];
        bool horizontal = v.w[0][0] == v.w[0][1];
        bool diagonal   = v.w[0][0] == v.w[1][1];
        // clang-format off
        bool inside     =  vertical &&  horizontal &&  diagonal;
        bool vstraight  =  vertical && !horizontal && !diagonal;
        bool hstraight  = !vertical &&  horizontal && !diagonal;
        bool incurve    =  vertical &&  horizontal && !diagonal;
        bool L          =  vertical && !horizontal &&  diagonal;
        bool flipL      = !vertical &&  horizontal &&  diagonal;
        bool corner1    = !vertical && !horizontal &&  diagonal;
        bool corner2    = !vertical && !horizontal && !diagonal;
        // clang-format on

        if (inside || vstraight || hstraight || incurve)
                return 0;
        if (L || flipL)
                return 1;
        if (corner1 || corner2)
                return 2;
        return 0; /*unreachable*/
}

int32_t doublecorners(struct state *s, int32_t i, int32_t j) {
        char window[3][3] = {
                // clang-format off
                {s->board[i - 1][j - 1], s->board[i - 1][j], s->board[i - 1][j + 1]},
                {s->board[i    ][j - 1], s->board[i    ][j], s->board[i    ][j + 1]},
                {s->board[i + 1][j - 1], s->board[i + 1][j], s->board[i + 1][j + 1]}
                // clang-format on
        };
        int32_t botright = count_corners((struct view){
                .w = {{window[1][1], window[1][2]},
                      {window[2][1], window[2][2]}}
        });
        int32_t botleftt = count_corners((struct view){
                .w = {{window[1][1], window[2][1]},
                      {window[1][0], window[2][0]}}
        });
        int32_t topleftt = count_corners((struct view){
                .w = {{window[1][1], window[1][0]},
                      {window[0][1], window[0][0]}}
        });
        int32_t topright = count_corners((struct view){
                .w = {{window[1][1], window[0][1]},
                      {window[1][2], window[0][2]}}
        });
        return botright + botleftt + topleftt + topright;
}

void add(struct res *dest, struct res source) {
        dest->area          += source.area;
        dest->perimeter     += source.perimeter;
        dest->doublecorners += source.doublecorners;
}

struct res dfs(struct state *s, int32_t i, int32_t j) {
        char       c   = s->board[i][j];
        struct res res = {};
        if (s->vis[i][j])
                return res;

        s->vis[i][j]      = true;
        res.area          = 1;
        res.perimeter     = perimeter(s, i, j);
        res.doublecorners = doublecorners(s, i, j);

        if (c == s->board[i - 1][j])
                add(&res, dfs(s, i - 1, j));

        if (c == s->board[i + 1][j])
                add(&res, dfs(s, i + 1, j));

        if (c == s->board[i][j - 1])
                add(&res, dfs(s, i, j - 1));

        if (c == s->board[i][j + 1])
                add(&res, dfs(s, i, j + 1));

        return res;
}

void run(struct state *s) {
        struct res tmp  = {};
        int64_t    sumA = 0, sumB = 0;
        for (int32_t i = 1; i < s->rows - 1; i++) {
                for (int32_t j = 1; j < s->cols - 1; j++) {
                        if (!s->vis[i][j]) {
                                tmp   = dfs(s, i, j);
                                sumA += tmp.area * tmp.perimeter;
                                sumB += tmp.area * tmp.doublecorners / 2;
                        }
                }
        }
        printf("sumA: %ld\n", sumA);
        printf("sumB: %ld\n", sumB);
}

int32_t main(int32_t argc, char *argv[]) {
        struct state state = {};
        FILE        *f     = 2 == argc ? fopen(argv[1], "r") : stdin;
        readInput(&state, f);
        run(&state);
        return 0;
}
