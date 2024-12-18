#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* the following shows that there are 20000 instructions:
 *   cat inputs/15.in | tail -n+52 | fold -b1 | wc -l
 */
#define NR_INST       20000
#define MAX_D         150
#define EMPTY         '.'
#define BOX           'O'
#define BOX_L         '['
#define BOX_R         ']'
#define BOX_L_SHIFTED '{'
#define BOX_R_SHIFTED '}'
#define WALL          '#'
#define ROBOT         '@'

enum direction { UP, RIGHT, DOWN, LEFT };

enum direction char_to_dir(char c) {
        // clang-format off
        switch (c) {
        case '^':                return UP;
        case '>':                return RIGHT;
        case 'v':                return DOWN;
        case '<':                return LEFT;
        default: /*unreachable*/ return -1;
        }
        // clang-format on
}

char *dir_to_str(enum direction d) {
        // clang-format off
        switch (d) {
        case UP:                 return "^";
        case RIGHT:              return ">";
        case DOWN:               return "V";
        case LEFT:               return "<";
        default: /*unreachable*/ return NULL;
        }
        // clang-format on
}

struct point {
        int32_t x, y;
};

typedef struct point (*step_f)(struct point);

// clang-format off
struct point up   (struct point p) { return (struct point){.x = p.x - 1, .y = p.y}; }
struct point right(struct point p) { return (struct point){.x = p.x, .y = p.y + 1}; }
struct point down (struct point p) { return (struct point){.x = p.x + 1, .y = p.y}; }
struct point left (struct point p) { return (struct point){.x = p.x, .y = p.y - 1}; }
// clang-format on

struct warehouse {
        char         floor[MAX_D][MAX_D];
        int32_t      nr_rows, nr_cols, cur_move;
        struct point robot;
};

struct movements {
        enum direction moves[NR_INST];
        int32_t        nr_moves;
        step_f         steps[4];
};

struct state {
        struct warehouse a, b;
        struct movements instr;
};

void add_warehouse_line(struct warehouse *w, char *line, size_t linelen) {
        int32_t i;
        for (i = 0; i < linelen; i++) {
                w->floor[w->nr_rows][i] = line[i];
                if (ROBOT == line[i])
                        w->robot = (struct point){.x = w->nr_rows, .y = i};
        }
        w->nr_rows++;
}

void add_instruction_line(struct movements *m, char *line, size_t linelen) {
        int32_t i;
        for (i = 0; i < linelen; i++)
                m->moves[m->nr_moves++] = char_to_dir(line[i]);
}

void make_board_b(struct warehouse *b, struct warehouse *a) {
        int32_t i, j;
        for (i = 0; i < a->nr_rows; i++) {
                for (j = 0; j < a->nr_cols; j++) {
                        switch (a->floor[i][j]) {
                        case EMPTY:
                                b->floor[i][2 * j]     = EMPTY;
                                b->floor[i][2 * j + 1] = EMPTY;
                                break;
                        case WALL:
                                b->floor[i][2 * j]     = WALL;
                                b->floor[i][2 * j + 1] = WALL;
                                break;
                        case BOX:
                                b->floor[i][2 * j]     = BOX_L;
                                b->floor[i][2 * j + 1] = BOX_R;
                                break;
                        case ROBOT:
                                b->floor[i][2 * j]     = ROBOT;
                                b->floor[i][2 * j + 1] = EMPTY;
                                break;
                        default: break;
                        }
                }
        }
        b->robot     = a->robot;
        b->robot.y  *= 2;
        b->nr_cols   = a->nr_cols * 2;
        b->nr_rows   = a->nr_rows;
        b->cur_move  = 0;
}

void readInput(struct state *s, FILE *f) {
        char  *line = NULL;
        size_t n = 0, linelen = 0;

        if (-1 == (linelen = getline(&line, &n, f)))
                return;

        s->a.nr_cols = linelen - 1;
        add_warehouse_line(&s->a, line, linelen - 1);

        while (1 < (linelen = getline(&line, &n, f)))
                add_warehouse_line(&s->a, line, linelen - 1);

        while (-1 != (linelen = getline(&line, &n, f)))
                add_instruction_line(&s->instr, line, linelen - 1);

        s->instr.steps[UP]    = up;
        s->instr.steps[RIGHT] = right;
        s->instr.steps[DOWN]  = down;
        s->instr.steps[LEFT]  = left;

        make_board_b(&s->b, &s->a);

        free(line);
}

int32_t score(int32_t i, int32_t j) { return 100 * i + j; }

void print_board(struct warehouse *b, FILE *f) {
        int32_t i, j, sum;

        for (i = 0; i < b->nr_rows; i++)
                fprintf(f, "%s\n", b->floor[i]);

        sum = 0;
        for (i = 0; i < MAX_D; i++) {
                for (j = 0; j < MAX_D; j++) {
                        if (BOX == b->floor[i][j] || BOX_L == b->floor[i][j]) {
                                sum += score(i, j);
                        }
                }
        }

        fprintf(f,
                " rows: %d\n"
                " cols: %d\n"
                "robot: (%d, %d)\n"
                " inst: %d\n"
                "score: %d\n",
                b->nr_rows, b->nr_cols, b->robot.x, b->robot.y, b->cur_move,
                sum);
}

void move_a(struct warehouse *a, struct movements *i) {
        enum direction inst = i->moves[a->cur_move];
        step_f         step = i->steps[inst];
        struct point   r    = step(a->robot);

        while (BOX == a->floor[r.x][r.y])
                r = step(r);

        if (WALL != a->floor[r.x][r.y] && EMPTY != a->floor[r.x][r.y]) {
                fprintf(stderr, "\nREACHED AN INCONSISTENT BOARD STATE!\n");
                print_board(a, stderr);
                exit(EXIT_FAILURE);
        }

        if (WALL == a->floor[r.x][r.y])
                return;

        a->floor[r.x][r.y] = BOX;

        a->floor[a->robot.x][a->robot.y] = EMPTY;
        a->robot                         = step(a->robot);
        a->floor[a->robot.x][a->robot.y] = ROBOT;
}

void run_a(struct state *s) {
        int32_t i;
        for (i = 0; i < s->instr.nr_moves; i++) {
                move_a(&s->a, &s->instr);
                s->a.cur_move++;
        }
}

char opposite(char c) {
        switch (c) {
        case BOX_L: return BOX_R;
        case BOX_R: return BOX_L;
        default: return WALL;
        }
}

void move_horizontally(struct warehouse *b, struct movements *m) {
        enum direction inst = m->moves[b->cur_move];
        step_f         step = m->steps[inst];
        struct point   r    = step(b->robot);
        char           closing;

        while (BOX_L == b->floor[r.x][r.y] || BOX_R == b->floor[r.x][r.y])
                r = step(r);

        if (WALL != b->floor[r.x][r.y] && EMPTY != b->floor[r.x][r.y]) {
                fprintf(stderr, "\nREACHED AN INCONSISTENT BOARD STATE!\n");
                print_board(b, stderr);
                exit(EXIT_FAILURE);
        }

        if (WALL == b->floor[r.x][r.y])
                return;

        r       = step(b->robot);
        closing = opposite(b->floor[r.x][r.y]);
        while (BOX_L == b->floor[r.x][r.y] || BOX_R == b->floor[r.x][r.y]) {
                b->floor[r.x][r.y] = opposite(b->floor[r.x][r.y]);
                r                  = step(r);
        }
        b->floor[r.x][r.y] = closing;

        b->floor[b->robot.x][b->robot.y] = EMPTY;
        b->robot                         = step(b->robot);
        b->floor[b->robot.x][b->robot.y] = ROBOT;
}

struct point other(struct point r, char c) {
        switch (c) {
        case BOX_L: return right(r);
        case BOX_R: return left(r);
        default: return r;
        }
}

bool shift(struct warehouse *b, step_f f, struct point p, char subst) {
        struct point q;
        switch (b->floor[p.x][p.y]) {
        case EMPTY: b->floor[p.x][p.y] = subst; return true;
        case BOX_L:
                b->floor[p.x][p.y] = subst;
                q                  = right(p);
                b->floor[q.x][q.y] = EMPTY;
                return shift(b, f, f(p), BOX_L_SHIFTED) &&
                       shift(b, f, f(q), BOX_R_SHIFTED);
        case BOX_R:
                b->floor[p.x][p.y] = subst;
                q                  = left(p);
                b->floor[q.x][q.y] = EMPTY;
                return shift(b, f, f(p), BOX_R_SHIFTED) &&
                       shift(b, f, f(q), BOX_L_SHIFTED);
        default: return false;
        }
}

// just be lazy: write a new board, then copy it over if the move is possible
void move_vertically(struct warehouse *b, struct movements *m) {
        enum direction   inst = m->moves[b->cur_move];
        step_f           step = m->steps[inst];
        struct point     r    = step(b->robot);
        struct warehouse nb;
        int32_t          i, j;

        memcpy(&nb, b, sizeof(nb));

        if (!shift(&nb, step, step(nb.robot), EMPTY))
                return;

        for (i = 0; i < b->nr_rows; i++) {
                for (j = 0; j < b->nr_cols; j++) {
                        if (nb.floor[i][j] == BOX_L_SHIFTED)
                                nb.floor[i][j] = BOX_L;
                        if (nb.floor[i][j] == BOX_R_SHIFTED)
                                nb.floor[i][j] = BOX_R;
                }
        }

        nb.floor[nb.robot.x][nb.robot.y] = EMPTY;
        nb.robot                         = step(nb.robot);
        nb.floor[nb.robot.x][nb.robot.y] = ROBOT;
        memcpy(b, &nb, sizeof(*b));
}

void run_b(struct state *s) {
        int32_t i;
        for (i = 0; i < s->instr.nr_moves; i++) {
                switch (s->instr.moves[s->b.cur_move]) {
                case LEFT:
                case RIGHT: move_horizontally(&s->b, &s->instr); break;
                case UP:
                case DOWN: move_vertically(&s->b, &s->instr); break;
                }
                s->b.cur_move++;
        }
}

int32_t main(int32_t argc, char *argv[]) {
        struct state state = {};
        FILE        *f     = 2 <= argc ? fopen(argv[1], "r") : stdin;
        readInput(&state, f);
        fclose(f);
        /*print_board(&state.a, stdout);*/
        run_a(&state);
        print_board(&state.a, stdout);
        /*print_board(&state.b, stdout);*/
        run_b(&state);
        print_board(&state.b, stdout);
        return 0;
}
