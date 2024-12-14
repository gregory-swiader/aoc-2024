#include <assert.h>
#include <regex.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define COST_A    3
#define COST_B    1
#define MAX_REC   100
#define MAX_GAMES 350
#define OFFSET_P  10000000000000

const char button_pattern[] =
        "^Button [AB]: X\\+([[:digit:]]+), Y\\+([[:digit:]]+)";
const char prize_pattern[] = "^Prize: X=([[:digit:]]+), Y=([[:digit:]]+)";

struct point {
        int64_t x, y;
};

struct game {
        struct point a, b, p;
        int64_t      result;
};

struct state {
        struct game games[MAX_GAMES];
        size_t      nr_games;
};

struct point increase_p(struct point c) {
        return (struct point){.x = c.x + OFFSET_P, .y = c.y + OFFSET_P};
}

struct point parse_line(char *line, regex_t reg) {
        struct point out       = {};
        regmatch_t   pmatch[3] = {};
        char         buf[20]   = {};

        if (regexec(&reg, line, 3, pmatch, 0))
                exit(EXIT_FAILURE);

        sprintf(buf, "%.*s", pmatch[1].rm_eo - pmatch[1].rm_so,
                line + pmatch[1].rm_so);
        out.x = atoi(buf);

        sprintf(buf, "%.*s", pmatch[2].rm_eo - pmatch[2].rm_so,
                line + pmatch[2].rm_so);
        out.y = atoi(buf);

        return out;
}

void read_input(FILE *f, struct state *s) {
        char       *line = NULL;
        size_t      n    = 0;
        regex_t     button_regex, prize_regex;
        struct game g;

        if (regcomp(&button_regex, button_pattern, REG_EXTENDED))
                exit(EXIT_FAILURE);

        if (regcomp(&prize_regex, prize_pattern, REG_EXTENDED))
                exit(EXIT_FAILURE);

        while (!feof(f)) {
                getline(&line, &n, f);
                g.a = parse_line(line, button_regex);
                getline(&line, &n, f);
                g.b = parse_line(line, button_regex);
                getline(&line, &n, f);
                g.p = parse_line(line, prize_regex);
                getline(&line, &n, f);

                s->games[s->nr_games++] = g;
        }

        regfree(&button_regex);
        regfree(&prize_regex);
        free(line);
}

int64_t cost(int64_t a, int64_t b) { return a * COST_A + b * COST_B; }

// this problem is a vector equation with a constraint:
//   a * cnt_a + b * cnt_b = p
//   minimize: COST_A * cnt_a + COST_B * cnt_b
//
// actually, fuck it, why bother minimizing?
//
//
// WHY ARE ALL OF THE MATRICES INVERTIBLE!?!?!?!!?!?
// WHY IS AOC ALWAYS SO POPULATED WITH HIDDEN ASSUMPTIONS? I ALWAYS LOOK FOR
// EDGE CASES AND THEN FIND OUT THAT ONLY THE SIMPLEST CASE IS ACTUALLY NEEDED.
int64_t play(struct game g) {
        int64_t mat[2][3] = {
                {g.a.x, g.b.x, g.p.x},
                {g.a.y, g.b.y, g.p.y}
        };
        // reduce
        int64_t tmp = mat[0][0];

        mat[0][0] *= mat[1][0];
        mat[0][1] *= mat[1][0];
        mat[0][2] *= mat[1][0];
        mat[1][0] *= tmp;
        mat[1][1] *= tmp;
        mat[1][2] *= tmp;

        mat[1][0] -= mat[0][0];
        mat[1][1] -= mat[0][1];
        mat[1][2] -= mat[0][2];

        if (0 == mat[1][1] && 0 == mat[1][2]) {
                printf("trouble ahead\n"
                       "system of equations with infinitely many solutions\n");
                return -1;
        }
        if (0 == mat[1][1] && 0 != mat[1][2])
                return 0; /*inconsistent*/


        if (0 != mat[1][2] % mat[1][1])
                return 0; /*no integer multiple*/

        mat[1][2] /= mat[1][1];
        mat[1][1]  = 1;

        mat[0][2] -= mat[0][1] * mat[1][2];
        mat[0][1] -= mat[0][1] * mat[1][1];

        if (0 != mat[0][2] % mat[0][0])
                return 0; /*no integer multiple*/

        mat[0][2] /= mat[0][0];
        mat[0][0]  = 1;

        return cost(mat[0][2], mat[1][2]);
}

void computeA(struct state *s) {
        int64_t sum = 0;
        for (int32_t i = 0; i < s->nr_games; i++)
                sum += play(s->games[i]);
        printf("%ld\n", sum);
}

// unfortunately, a dp linear in p.x is too slow. There is no way to filter out
// the possible matches before reaching the end.
void computeB(struct state *s) {
        for (int32_t i = 0; i < s->nr_games; i++)
                s->games[i].p = increase_p(s->games[i].p);
        int64_t sum = 0;
        for (int32_t i = 0; i < s->nr_games; i++)
                sum += play(s->games[i]);
        printf("%ld\n", sum);
}

void print_state(struct state *s) {
        for (int32_t i = 0; i < s->nr_games; i++) {
                printf("A: x y -> x+%ld y+%ld\n", s->games[i].a.x,
                       s->games[i].a.y);
                printf("B: x y -> x+%ld y+%ld\n", s->games[i].b.x,
                       s->games[i].b.y);
                printf("prize at: %ld %ld\n", s->games[i].p.x, s->games[i].p.y);
        }
        printf("%zu\n", s->nr_games);
}

int32_t main(int32_t argc, char *argv[]) {
        FILE        *f = 2 <= argc ? fopen(argv[1], "r") : stdin;
        struct state s = {};
        read_input(f, &s);
        /*print_state(&s);*/
        computeA(&s);
        computeB(&s);
}
