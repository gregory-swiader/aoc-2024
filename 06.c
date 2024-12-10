#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_D    150
#define DELIM    'B'
#define EMPTY    '.'
#define OBSTACLE '#'
#define VISITED  'X'

enum direction { UP, RIGHT, DOWN, LEFT };

struct state {
        char    board[MAX_D][MAX_D];
        bool    vis[MAX_D][MAX_D][4];
        int32_t rows, cols;
        struct heading {
                int32_t        x, y;
                enum direction dir;
        } start;
};

bool possible_loops[MAX_D][MAX_D] = {};

void add_delim(struct state *s, int32_t cols) {
        for (int32_t i = 0; i < s->cols; i++)
                s->board[s->rows][i] = DELIM;
        s->rows++;
}

void add_line(struct state *s, char *line) {
        for (int32_t i = 0; i < s->cols - 2; i++) {
                s->board[s->rows][i + 1] = line[i];
                // clang-format off
                switch (line[i]) {
                case '^': s->start = (struct heading){ .x = s->rows, .y = i + 1, .dir = UP   }; break;
                case '>': s->start = (struct heading){ .x = s->rows, .y = i + 1, .dir = RIGHT}; break;
                case 'V': s->start = (struct heading){ .x = s->rows, .y = i + 1, .dir = DOWN }; break;
                case '<': s->start = (struct heading){ .x = s->rows, .y = i + 1, .dir = LEFT }; break;
                default: break;
                }
                // clang-format on
        }
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

char *enum_to_str(enum direction e) {
        // clang-format off
        switch (e) {
        case UP:                 return "^";
        case RIGHT:              return ">";
        case DOWN:               return "V";
        case LEFT:               return "<";
        default: /*unreachable*/ return NULL;
        }
        // clang-format on
}

void print(struct state *s) {
        int32_t cnt_vis, cnt_loops, i, j, k, vis;
        for (i = 0; i < s->rows; i++)
                printf("%s\n", s->board[i]);

        cnt_vis   = 0;
        cnt_loops = -possible_loops[s->start.x][s->start.y];
        for (i = 0; i < MAX_D; i++) {
                for (j = 0; j < MAX_D; j++) {
                        vis = false;
                        for (k = 0; k < 4; k++) {
                                vis |= s->vis[i][j][k];
                        }
                        cnt_vis += vis;

                        cnt_loops += possible_loops[i][j];
                }
        }

        printf(" rows: %d\n"
               " cols: %d\n"
               "start: (%d, %d), %s\n"
               "  vis: %d\n"
               "loops: %d\n",
               s->rows, s->cols, s->start.x, s->start.y,
               enum_to_str(s->start.dir), cnt_vis, cnt_loops);
}

enum direction clockwise(enum direction d) {
        // clang-format off
        switch (d) {
        case UP:                 return RIGHT;
        case RIGHT:              return DOWN;
        case DOWN:               return LEFT;
        case LEFT:               return UP;
        default: /*unreachable*/ return -1;
        }
        // clang-format on
}

struct heading rotate(struct heading p) {
        return (struct heading){.x = p.x, .y = p.y, .dir = clockwise(p.dir)};
}

struct heading forward(struct heading p) {
        // clang-format off
        switch (p.dir) {
        case UP:    return (struct heading){.x = p.x - 1, .y = p.y,     .dir = p.dir};
        case RIGHT: return (struct heading){.x = p.x,     .y = p.y + 1, .dir = p.dir};
        case DOWN:  return (struct heading){.x = p.x + 1, .y = p.y,     .dir = p.dir};
        case LEFT:  return (struct heading){.x = p.x,     .y = p.y - 1, .dir = p.dir};
        default: /*unreachable*/ return (struct heading){};
        }
        // clang-format on
}

struct heading move(struct state *s, struct heading pos) {
        struct heading f = forward(pos);
        if (OBSTACLE == s->board[f.x][f.y])
                return rotate(pos);
        return f;
}

void check_loop(struct state *sptr, struct heading p) {
        struct heading new_obstacle = forward(p);
        struct state   s;

        if (EMPTY != sptr->board[new_obstacle.x][new_obstacle.y])
                return;
        if (possible_loops[new_obstacle.x][new_obstacle.y])
                return;

        memcpy(&s, sptr, sizeof(s));
        s.board[new_obstacle.x][new_obstacle.y] = OBSTACLE;

        while (DELIM != s.board[p.x][p.y]) {
                s.board[p.x][p.y]      = VISITED;
                s.vis[p.x][p.y][p.dir] = true;
                p                      = move(&s, p);
                if (s.vis[p.x][p.y][p.dir]) {
                        possible_loops[new_obstacle.x][new_obstacle.y] = true;
                        break;
                }
        }
}

void walk(struct state *s) {
        struct heading p = {
                .x = s->start.x, .y = s->start.y, .dir = s->start.dir};

        while (DELIM != s->board[p.x][p.y]) {
                s->vis[p.x][p.y][p.dir] = true;
                s->board[p.x][p.y]      = VISITED;
                check_loop(s, p);
                p = move(s, p);
        }
}

int32_t main(int32_t argc, char *argv[]) {
        struct state state = {};
        FILE        *f     = fopen(argv[1], "r");
        readInput(&state, f);
        walk(&state);
        print(&state);
        return 0;
}
