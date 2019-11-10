#ifndef __ALLOC_C
#define __ALLOC_C

#include <assert.h>
#include <stddef.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <sys/mman.h>

#define LARGE (4096 + 128)
// LARGE_MAX is 1M
#define LARGE_MAX 1000000LL

#define SMALL (256)
// SMALL_MAX is 10M
#define SMALL_MAX 40000000LL

void * small_buf = NULL;
void * large_buf = NULL;
void * large_buf_limit = NULL;

void ** small_free = NULL;
void ** large_free = NULL;

uint32_t small_index = 0;
uint32_t large_index = 0;

uint32_t small_free_count = 0;
uint32_t large_free_count = 0;


#define ISLARGE(ptr) ((void*)ptr>=(void*)large_buf)
#define LIMITCHECK(ptr) (assert(ptr >= small_buf && ptr < large_buf_limit ))

// pointer checks
// 0 - check if null
// 1 - check if in entire range
// 2 - workout if small or large, and set parameters accordingly (buf and size)
// 3 - check alignment ( 0 == (buf-base) % size )
// errors are (1) range check (2) alignment check
// report (1) value, low high
// report (2) value
//
//
static inline int _checkptr(void* p, void* low, void* high, size_t size, int null_allowed, int non_stop) {

  if ( p ) {
    if (p<low)
      printf("\nlimit check low %p %p\n",p,low);
    else if (p>high)
      printf("\nlimit check high %p %p\n",p,high);
    else if ( (p-low) % size)
      printf("\nalignment check %p %p %ld\n",p,low,size);
    else
      return 0;
  } else if (null_allowed)
    return 0;
  else
    printf("\nnull check %p\n",p);

  if (non_stop)
    return 1;

  fflush(stdout);
  assert(0);
};
#ifdef DEBUG
#define CHECKL(ptr) (_checkptr(ptr,large_buf,large_buf + (LARGE-1) * LARGE_MAX,LARGE,0,0))
#define CHECKLN(ptr) (_checkptr(ptr,large_buf,large_buf + (LARGE-1) * LARGE_MAX,LARGE,1,0))
#define CHECKLC(ptr) (_checkptr(ptr,large_buf,large_buf + (LARGE-1) * LARGE_MAX,LARGE,1,1))
#define CHECKS(ptr) (_checkptr(ptr, small_buf, small_buf + (SMALL-1) * SMALL_MAX,SMALL,0,0))
#define CHECKSN(ptr) (_checkptr(ptr,small_buf,small_buf + (SMALL-1) * SMALL_MAX,SMALL,1,0))
#define CHECKSC(ptr) (_checkptr(ptr,small_buf,small_buf + (SMALL-1) * SMALL_MAX,SMALL,1,1))
#define CHECK(ptr) if (ISLARGE(ptr)) CHECKL(ptr); else CHECKS(ptr)
#else
#define CHECKL(ptr) ;
#define CHECKLN(ptr) ;
#define CHECKLC(ptr) 0
#define CHECKS(ptr) ;
#define CHECKSN(ptr) ;
#define CHECKSC(ptr) 0
#define CHECK(ptr) ;
#endif

void * both_buf;

void free_check_large() {

  void *p = large_free;
  void * from = NULL;
  int depth = 0;
  while (NULL != p) {
    depth++;
    assert(depth<=large_free_count);
    if (CHECKLC(p)) {
      printf("free_check_large depth %d/%d from %p\n",depth,large_free_count,from);
      fflush(stdout);
      assert(0);
    };
    from = p;
    p = * (void**) p;
  };
  assert(depth==large_free_count);
};

void free_check_small () {
  void *p = small_free;
  void * from = NULL;
  int depth = 0;
  while (NULL != p) {
    depth++;
    if (CHECKSC(p)) {
      printf("free_check_small depth %d/%d from %p\n",depth,small_free_count,from);
      fflush(stdout);
      assert(0);
    } else
    ;
    from = p;
    p = * (void**) p;
    assert(depth<=small_free_count);
  };
  assert(depth==small_free_count);
};

// ### start small code to mirror for large

void reinit_alloc_small() {
  small_index = 0;
  small_free = NULL;
  small_free_count = 0;
};

static inline void *alloc_small() {
  void * p;
  CHECKSN(small_free);
  if (small_free) {
    CHECKS(small_free);

    p = small_free;
    small_free = * small_free;
    small_free_count--;

    CHECKSN(small_free);
  } else if (small_index < SMALL_MAX) {
    p = small_buf + SMALL * (small_index++);
  } else
    assert (small_index < SMALL_MAX);
  CHECKS(p);
  return p;
};

static inline void dalloc_small(void *p) {
  CHECKSN(small_free);

  if (small_free) {
    *(void**) p = small_free;
    small_free = p;
  } else {
    small_free = p;
    *(void**) p = NULL;
  };
  small_free_count++;

  CHECKS(small_free);
};

// ### end small code to mirror for large

// ############# large mirror of small

void reinit_alloc_large() {
  large_index = 0;
  large_free = NULL;
  large_free_count = 0;
};

static inline void *alloc_large() {
  void *p;

  if (large_free) {
    CHECKL(large_free);
    CHECKLN(*large_free);
    p = large_free;
    large_free = *large_free;
    large_free_count--;
  } else if (large_index < LARGE_MAX) {
    p = large_buf + LARGE * (large_index++);
  } else
    assert (large_index < LARGE_MAX);

  CHECKL(p);
  return p;
};

static inline void dalloc_large(void *p) {
  CHECKLN(large_free);
  if (large_free) {
    *(void**)p = large_free;
    large_free = p;
  } else {
    large_free = p;
    *(void**)p = NULL;
  };
  large_free_count++;
  CHECKL(large_free);
};

// ############# end of large mirror of large

void init_alloc() {
  both_buf = mmap(NULL, SMALL * SMALL_MAX + LARGE * LARGE_MAX, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  assert(MAP_FAILED != both_buf);
  small_buf = both_buf;
  large_buf = both_buf + SMALL * SMALL_MAX;
  large_buf_limit = large_buf + LARGE * LARGE_MAX;
};

void reinit_alloc() {
  reinit_alloc_large();
  reinit_alloc_small();
};

static inline void dalloc(void *p) {
  LIMITCHECK(p);
  if (ISLARGE(p))
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

void report_route_table () {
  printf("route table size = %d (%d/%d)\n", small_index + large_index, small_index, large_index);
};

#endif
