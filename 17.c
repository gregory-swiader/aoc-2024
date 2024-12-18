#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_PROG 64
#define DELIMS   "Program: ,"

/*#define DDEBUG*/
#ifdef DDEBUG
#define dprintf(...)                                                           \
        do {                                                                   \
                fprintf(stderr, __VA_ARGS__);                                  \
        } while (0)
#else
#define dprintf(...)
#endif

enum instructions { ADV, BXL, BST, JNZ, BXC, OUT, BDV, CDV };
enum operand { ZERO, ONE, TWO, THREE, REG_A, REG_B, REG_C, RESERVED };

struct machine {
        int64_t reg_a, reg_b, reg_c;
        uint8_t program[MAX_PROG];
        int32_t program_length, pc;
};

void print_machine(struct machine *m) {
        printf("  reg_a: %ld\n"
               "  reg_b: %ld\n"
               "  reg_c: %ld\n",
               m->reg_a, m->reg_b, m->reg_c);
        printf("program: %d", m->program[0]);
        for (int32_t i = 1; i < m->program_length; i++) {
                printf(",%d", m->program[i]);
        }
        printf("\n");
        printf("     pc: ");
        for (int32_t i = 0; i < m->pc; i++) {
                printf("  ");
        }
        printf("^\n\n\n");
}

char *combo(enum operand op) {
        // clang-format off
        switch (op) {
        case ZERO:               return "0";
        case ONE:                return "1";
        case TWO:                return "2";
        case THREE:              return "3";
        case REG_A:              return "RA";
        case REG_B:              return "RB";
        case REG_C:              return "RC";
        case RESERVED:           return "RESERVED";
        default: /*unreachable*/ return NULL;
        }
        // clang-format on
}

void print_program(struct machine *m) {
        for (int32_t i = 0; i < m->program_length; i += 2) {
                // clang-format off
                switch ((enum instructions)m->program[i]) {
                case ADV: printf("%02d: adv %s\n", i, combo(m->program[i + 1])); break;
                case BXL: printf("%02d: bxl %d\n", i,      (m->program[i + 1])); break;
                case BST: printf("%02d: bst %s\n", i, combo(m->program[i + 1])); break;
                case JNZ: printf("%02d: jnz %d\n", i,      (m->program[i + 1])); break;
                case BXC: printf("%02d: bxc\n"   , i                          ); break;
                case OUT: printf("%02d: out %s\n", i, combo(m->program[i + 1])); break;
                case BDV: printf("%02d: bdv %s\n", i, combo(m->program[i + 1])); break;
                case CDV: printf("%02d: cdv %s\n", i, combo(m->program[i + 1])); break;
                }
                // clang-format on
        }
}

void read_input(struct machine *m, FILE *f) {
        char   *line = NULL, *cur;
        size_t  n    = 0;
        ssize_t linelen;

        linelen         = getline(&line, &n, f);
        line[--linelen] = '\0';
        cur             = strchr(line, ':') + 2;
        m->reg_a        = strtoll(cur, NULL, 10);

        linelen         = getline(&line, &n, f);
        line[--linelen] = '\0';
        cur             = strchr(line, ':') + 2;
        m->reg_b        = strtoll(cur, NULL, 10);

        linelen         = getline(&line, &n, f);
        line[--linelen] = '\0';
        cur             = strchr(line, ':') + 2;
        m->reg_c        = strtoll(cur, NULL, 10);

        (void)getline(&line, &n, f);
        linelen         = getline(&line, &n, f);
        line[--linelen] = '\0';
        cur             = strtok(line, DELIMS);
        while (cur) {
                m->program[m->program_length++] = atoi(cur);
                cur                             = strtok(NULL, DELIMS);
        }
}

void adv(struct machine *m, enum operand op) {
        dprintf("adv\n");
        switch (op) {
        case ZERO:
        case ONE:
        case TWO:
        case THREE: m->reg_a = m->reg_a >> op; return;
        case REG_A: m->reg_a = m->reg_a >> m->reg_a; return;
        case REG_B: m->reg_a = m->reg_a >> m->reg_b; return;
        case REG_C: m->reg_a = m->reg_a >> m->reg_c; return;
        case RESERVED: fprintf(stderr, "reserved\n"); exit(1);
        }
}

void bxl(struct machine *m, enum operand op) {
        dprintf("bxl\n");
        m->reg_b = m->reg_b ^ (int32_t)op;
}

void bst(struct machine *m, enum operand op) {
        dprintf("bst\n");
        switch (op) {
        case ZERO:
        case ONE:
        case TWO:
        case THREE: m->reg_b = op % 8; return;
        case REG_A: m->reg_b = m->reg_a % 8; return;
        case REG_B: m->reg_b = m->reg_b % 8; return;
        case REG_C: m->reg_b = m->reg_c % 8; return;
        case RESERVED: fprintf(stderr, "reserved\n"); exit(1);
        }
}

void jnz(struct machine *m, enum operand op) {
        dprintf("jnz\n");
        if (0 == m->reg_a)
                return;
        m->pc = (int32_t)op - 2;
}

void bxc(struct machine *m, enum operand op) {
        dprintf("bxc\n");
        m->reg_b = m->reg_b ^ m->reg_c;
}

void out(struct machine *m, enum operand op) {
        dprintf("out\n");
        switch (op) {
        case ZERO:
        case ONE:
        case TWO:
        case THREE: printf("%d,", op % 8); return;
        case REG_A: printf("%ld,", m->reg_a % 8); return;
        case REG_B: printf("%ld,", m->reg_b % 8); return;
        case REG_C: printf("%ld,", m->reg_c % 8); return;
        case RESERVED: fprintf(stderr, "reserved\n"); exit(1);
        }
}

void bdv(struct machine *m, enum operand op) {
        dprintf("bdv\n");
        switch (op) {
        case ZERO:
        case ONE:
        case TWO:
        case THREE: m->reg_b = m->reg_a >> op; return;
        case REG_A: m->reg_b = m->reg_a >> m->reg_a; return;
        case REG_B: m->reg_b = m->reg_a >> m->reg_b; return;
        case REG_C: m->reg_b = m->reg_a >> m->reg_c; return;
        case RESERVED: fprintf(stderr, "reserved\n"); exit(1);
        }
}

void cdv(struct machine *m, enum operand op) {
        dprintf("cdv\n");
        switch (op) {
        case ZERO:
        case ONE:
        case TWO:
        case THREE: m->reg_c = m->reg_a >> op; return;
        case REG_A: m->reg_c = m->reg_a >> m->reg_a; return;
        case REG_B: m->reg_c = m->reg_a >> m->reg_b; return;
        case REG_C: m->reg_c = m->reg_a >> m->reg_c; return;
        case RESERVED: fprintf(stderr, "reserved\n"); exit(1);
        }
}

void runA(struct machine *m) {
        while (m->pc < m->program_length) {
                /*print_machine(m);*/
                switch (m->program[m->pc]) {
                case ADV: adv(m, m->program[m->pc + 1]); break;
                case BXL: bxl(m, m->program[m->pc + 1]); break;
                case BST: bst(m, m->program[m->pc + 1]); break;
                case JNZ: jnz(m, m->program[m->pc + 1]); break;
                case BXC: bxc(m, m->program[m->pc + 1]); break;
                case OUT: out(m, m->program[m->pc + 1]); break;
                case BDV: bdv(m, m->program[m->pc + 1]); break;
                case CDV: cdv(m, m->program[m->pc + 1]); break;
                }
                m->pc += 2;
        }
        printf("\n\n");
}

int32_t main(int32_t argc, char *argv[]) {
        FILE          *f = 2 <= argc ? fopen(argv[1], "r") : stdin;
        struct machine m = {};
        read_input(&m, f);
        print_machine(&m);
        print_program(&m);
        runA(&m);
        print_machine(&m);
}
