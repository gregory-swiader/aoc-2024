#include <assert.h>
#include <regex.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define COST_A        3
#define COST_B        1
#define MAX_REC       100
#define MAX_GAMES     350
#define CIRCULAR_SIZE 20000
#define OFFSET_P      10000000000000

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

struct cnt {
        int64_t a, b;
};

struct arr {
        struct cnt *data;
        size_t      __size, size;
};

struct cnt increase_a(struct cnt c) {
        return (struct cnt){.a = c.a + 1, .b = c.b};
}

struct cnt increase_b(struct cnt c) {
        return (struct cnt){.a = c.a, .b = c.b + 1};
}

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

bool contains(struct arr *a, struct cnt e) {
        for (int32_t i = 0; i < a->size; i++)
                if (e.a == a->data[i].a && e.b == a->data[i].b)
                        return true;
        return false;
}

void append(struct arr *a, struct cnt c) {
        if (a->__size <= a->size) {
                a->__size = 0 == a->__size ? 16 : a->__size * 2;
                a->data   = realloc(a->data, sizeof(*a->data) * a->__size);
        }
        a->data[a->size++] = c;
}

void try_append(struct arr *dest, struct arr src, struct cnt (*f)(struct cnt)) {
        for (int32_t i = 0; i < src.size; i++) {
                struct cnt e = f(src.data[i]);
                if (contains(dest, e))
                        continue;
                append(dest, e);
        }
}

void clear(struct arr *a) { a->size = 0; }
void delete(struct arr *a) {
        if (a->data)
                free(a->data);
        memset(a, 0, sizeof(*a));
}

struct arr move(struct arr *a) {
        struct arr res = *a;
        memset(a, 0, sizeof(*a));
        return res;
}

// idea: do the coin dp thing for one dimension (x), return a list of possible
// matches, then filter to retain those that match in the other dimension (y)
// and find the lowest cost
struct arr play_x(struct game g) {
        struct arr dp[CIRCULAR_SIZE] = {};

        append(&dp[g.a.x], (struct cnt){.a = 1, .b = 0});
        append(&dp[g.b.x], (struct cnt){.a = 0, .b = 1});

        for (int32_t i = 0; i <= g.p.x; i++) {
                if (i != g.a.x && i != g.b.x)
                        clear(&dp[i % CIRCULAR_SIZE]);
                if (0 <= i - g.a.x)
                        try_append(&dp[i % CIRCULAR_SIZE],
                                   dp[(i - g.a.x) % CIRCULAR_SIZE], increase_a);
                if (0 <= i - g.b.x)
                        try_append(&dp[i % CIRCULAR_SIZE],
                                   dp[(i - g.b.x) % CIRCULAR_SIZE], increase_b);
        }


        struct arr res = move(&dp[g.p.x % CIRCULAR_SIZE]);
        for (int32_t i = 0; i < CIRCULAR_SIZE; i++)
                delete (&dp[i]);
        return res;
}

bool check(struct game g, struct cnt c) {
        int32_t target = g.p.y;
        int32_t result = c.a * g.a.y + c.b * g.b.y;
        return target == result;
}

struct arr filter(struct arr a, struct game g) {
        struct arr res = {};
        for (int32_t i = 0; i < a.size; i++)
                if (check(g, a.data[i]))
                        append(&res, a.data[i]);
        delete (&a);
        return res;
}

int64_t cost(struct cnt c) { return c.a * COST_A + c.b * COST_B; }

int64_t play(struct game g) {
        struct arr x = filter(play_x(g), g);

        if (0 == x.size)
                return 0;

        int64_t res = cost(x.data[0]);
        for (int32_t i = 0; i < x.size; i++) {
                int32_t c = cost(x.data[i]);
                res       = res <= c ? res : c;
        }

        return res;
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
        for (int32_t i = 0; i < s->nr_games; i++) {
                sum += play(s->games[i]);
                printf("finished game %d\n", i);
        }
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
