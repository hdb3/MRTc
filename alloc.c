#include <assert.h>
#include <stddef.h>
#include <stdio.h>
#include <sys/mman.h>

int small_index = 0;
void *small_buf;
#define SMALL 256LL
// SMALL_MAX is 10M
#define SMALL_MAX 10000000LL

void reinit_alloc_small() {
  small_index = 0;
};

void init_alloc_small() {
  small_buf = mmap(NULL, SMALL * SMALL_MAX, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  assert(MAP_FAILED != small_buf);
  // printf("small_buf %p\n", small_buf);
};

static inline void *alloc_small() {
  // assert (small_index < SMALL_MAX);
  return small_buf + SMALL * (small_index++);
};

int large_index = 0;
void *large_buf;
#define LARGE 4096
// LARGE_MAX is 10M
#define LARGE_MAX 1000000LL

void reinit_alloc_large() {
  large_index = 0;
};

void init_alloc_large() {
  large_buf = mmap(NULL, LARGE * LARGE_MAX, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  assert(MAP_FAILED != large_buf);
  // printf("large_buf %p\n", large_buf);
};

static inline void *alloc_large() {
  // assert (large_index < LARGE_MAX);
  return large_buf + LARGE * (large_index++);
};

void init_alloc() {
  init_alloc_large();
  init_alloc_small();
};

void reinit_alloc() {
  reinit_alloc_large();
  reinit_alloc_small();
};
