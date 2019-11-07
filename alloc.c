#include <assert.h>
#include <stddef.h>
#include <stdio.h>
#include <stdint.h>
#include <sys/mman.h>

#define LARGE 4096 + 128
// LARGE_MAX is 1M
#define LARGE_MAX 1000000LL

#define SMALL 256LL
// SMALL_MAX is 10M
#define SMALL_MAX 20000000LL

/*
void init_alloc_small() {
  small_buf = mmap(NULL, SMALL * SMALL_MAX, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  assert(MAP_FAILED != small_buf);
};
*/
// ### start small code to mirror for large

uint32_t small_index = 0;
void * small_buf;
void ** small_free = NULL;

void reinit_alloc_small() {
  small_index = 0;
  small_free = NULL;
};

static inline void *alloc_small() {
  if (small_free) {
    void * p = small_free;
    small_free = * small_free;
    return p; 
  } else if (small_index < SMALL_MAX) {
    return small_buf + SMALL * (small_index++);
  } else
    assert (small_index < SMALL_MAX);
};

static inline void dalloc_small(void *p) {
  * (void **) p = *small_free;
  small_free = p;
};

// ### end small code to mirror for large

// ############# large mirror of small

uint32_t large_index = 0;
void * large_buf;
void ** large_free = NULL;

void reinit_alloc_large() {
  large_index = 0;
  large_free = NULL;
};

static inline void *alloc_large() {
  if (large_free) {
    void * p = large_free;
    large_free = *large_free;
    return p; 
  } else if (large_index < LARGE_MAX) {
    return large_buf + LARGE * (large_index++);
  } else
    assert (large_index < LARGE_MAX);
};

static inline void dalloc_large(void *p) {
  * (void **) p = *large_free;
  large_free = p;
};

// ############# end of large mirror of small

void * both_buf;

void init_alloc_both() {
  both_buf = mmap(NULL, SMALL * SMALL_MAX + LARGE * LARGE_MAX, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  assert(MAP_FAILED != both_buf);
  small_buf = both_buf;
  large_buf = both_buf + SMALL * SMALL_MAX;
};

void init_alloc() {
  // init_alloc_large();
  // init_alloc_small();
  init_alloc_both();
};

void reinit_alloc() {
  reinit_alloc_large();
  reinit_alloc_small();
};

static inline void dalloc(void *p) {
  assert (p >= both_buf && p < both_buf + SMALL * SMALL_MAX + LARGE * LARGE_MAX);
  int is_large = p >= both_buf + SMALL * SMALL_MAX; 
  if (is_large)
    dalloc_large(p);
  else
    dalloc_small(p);
};

static inline void *alloc(size_t sz) {
  assert(LARGE >= sz);
  if (SMALL < sz)
    return alloc_large();
  else
    return alloc_small();
};
