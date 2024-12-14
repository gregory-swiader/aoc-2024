#include <regex.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#define MAX_ITER   10000
#define MAX_ROBOTS 1000
/*#define MOD_X      11*/
/*#define MOD_Y      7*/
#define MOD_X 101
#define MOD_Y 103

struct robot {
        int32_t x, y, vx, vy;
};

struct state {
        int32_t      nr_robots;
        struct robot robots[MAX_ROBOTS];
        int32_t      floor[MOD_X][MOD_Y];
};

const char robot_pattern[] =
        "p=([-[:digit:]]+),([-[:digit:]]+) v=([-[:digit:]]+),([-[:digit:]]+)";

void read_input(FILE *f, struct state *s) {
        char        *line        = NULL;
        size_t       n           = 0;
        regex_t      robot_regex = {};
        regmatch_t   pmatch[5]   = {};
        struct robot r           = {};

        if (regcomp(&robot_regex, robot_pattern, REG_EXTENDED))
                exit(EXIT_FAILURE);

        getline(&line, &n, f);
        while (!feof(f)) {
                if (regexec(&robot_regex, line, 5, pmatch, 0)) {
                        printf("regexec\n");
                        exit(EXIT_FAILURE);
                }

                line[pmatch[1].rm_eo] = '\0';
                line[pmatch[2].rm_eo] = '\0';
                line[pmatch[3].rm_eo] = '\0';
                line[pmatch[4].rm_eo] = '\0';

                r.x  = atoi(line + pmatch[1].rm_so);
                r.y  = atoi(line + pmatch[2].rm_so);
                r.vx = atoi(line + pmatch[3].rm_so);
                r.vy = atoi(line + pmatch[4].rm_so);

                s->robots[s->nr_robots] = r;
                s->nr_robots++;
                s->floor[r.x][r.y]++;

                getline(&line, &n, f);
        }

        regfree(&robot_regex);
        free(line);
}

int32_t mod_add(int32_t a, int32_t b, int32_t mod) {
        return (((a + b) % mod) + mod) % mod;
}

struct robot move(struct robot r) {
        return (struct robot){.x  = mod_add(r.x, r.vx, MOD_X),
                              .y  = mod_add(r.y, r.vy, MOD_Y),
                              .vx = r.vx,
                              .vy = r.vy};
}

void update(struct state *s) {
        for (int32_t i = 0; i < s->nr_robots; i++) {
                struct robot r = s->robots[i];
                s->floor[r.x][r.y]--;
                r = move(r);
                s->floor[r.x][r.y]++;
                s->robots[i] = r;
        }
}

void print(struct state *s, int32_t iter, FILE *f) {
        for (int32_t j = 0; j < MOD_Y; j++) {
                fprintf(f, "%d: ", iter);
                for (int32_t i = 0; i < MOD_X; i++) {
                        if (0 < s->floor[i][j]) {
                                fputc('#', f);
                        } else {
                                fputc('.', f);
                        }
                }
                fputc('\n', f);
        }
        fputc('\n', f);
}

void simulate(struct state *s, FILE *out) {
        for (int32_t i = 0; i < MAX_ITER; i++) {
                print(s, i, out);
                if (6398 == i || 16801 == i || 27204 == i)
                        print(s, i, stdout);
                update(s);
        }
}

int32_t main(int32_t argc, char *argv[]) {
        struct state s   = {};
        FILE        *f   = 2 <= argc ? fopen(argv[1], "r") : stdin;
        FILE        *out = 2 <= argc ? fopen("out", "r") : stdin;
        read_input(f, &s);
        simulate(&s, out);
}
