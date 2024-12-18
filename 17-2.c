#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

/**
 * The program seems to be the same for all inputs, up to a reordering of
 * instructions and a change in literal operands. The reordering of instructions
 * doesn't modify any data dependencies:
 *   00: bst RA
 *   02: bxl LIT_1
 *   04: cdv RB
 *   06: bxc
 *   08: bxl LIT_2
 *   10: out RB
 *   12: adv 3
 *   14: jnz 0
 *
 * The body of this loop translates to exactly the function `iterate` below:
 */
uint8_t program[] = {2, 4, 1, 5, 7, 5, 0, 3, 4, 1, 1, 6, 5, 5, 3, 0};

#define LIT_1 5
#define LIT_2 6

struct vec {
        int64_t *arr;
        int64_t  size, __size;
};

void push_back(struct vec *a, int64_t e) {
        if (a->__size <= a->size) {
                a->__size = (0 == a->__size) ? 8 : a->__size * 2;
                a->arr    = realloc(a->arr, sizeof(a->arr[0]) * a->__size);
        }
        a->arr[a->size++] = e;
}

int64_t iterate(int64_t a) {
        int64_t b = LIT_1 ^ (a % 8);
        int64_t c = (a >> b) % 8;
        return (b ^ c ^ LIT_2) % 8;
}

struct vec next_triples(int64_t a, uint8_t p) {
        struct vec res = {};
        for (int64_t i = 0; i < 8; i++) {
                if (iterate((a << 3) | i) == p) {
                        push_back(&res, i);
                }
        }
        return res;
}

int32_t main() {
        struct vec avec = {}, next = {}, triples = {};
        int32_t    pc, i, j;
        push_back(&avec, 0);
        for (pc = sizeof(program) - 1; pc >= 0; pc--) {
                next = (struct vec){};
                for (i = 0; i < avec.size; i++) {
                        triples = next_triples(avec.arr[i], program[pc]);
                        for (j = 0; j < triples.size; j++) {
                                push_back(&next,
                                          (avec.arr[i] << 3) | triples.arr[j]);
                        }
                        free(triples.arr);
                }
                free(avec.arr);
                avec = next;
        }
        printf("%ld\n", avec.arr[0]);
        free(avec.arr);
}
