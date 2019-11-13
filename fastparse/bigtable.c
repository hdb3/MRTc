#ifndef __BIGTABLE_C
#define __BIGTABLE_C
#include "include.h"

#include "getw.h"

/*
 *  explanation:
 *  map every prefix of /24 and smaller in a table of size ~/25
 *  the convention is to encode the length in the upper byte of 4,
 *  remainder is the upper 3 bytes of the address
 *  
 *  the generated /23 address works like this
 *  first, /24 is /24 (top bit is 1) 1xxxxxxxxxx(24)
 *  next, /23 has top bit 0, next is 1 01xxxxxxx(23)
 *  next, /22 has top bits 001xxxxxxxxxxx22)
 * 
 * encoding is simple: shift the address right as a 64 bit value with ..00001 (0000 0001 xxxx xxxx)
 * in the upper word 
 *
 * 0000 25(1) >> n
 *
 * ...00000001(24x) -> 1(24x)
 * ...00000001(23x) -> 01(23x)
*/


void print_prefix(uint8_t l, uint32_t address) {
  printf("%s/%d", inet_ntoa((struct in_addr){__bswap_32(address)}),l);
};

void print_prefix64(uint64_t la) {
  print_prefix(la >> 32, la & 0xffffffff);
};

#define BIG 1000000
#define TOO_BIG (BIG + 1)
#define RIBSIZE (2 << 25)
static inline uint32_t encode(uint8_t l, uint32_t a) {
  assert(l < 25);
  return (uint32_t)((uint64_t)(0x100000000LL | (uint64_t)a) >> (32 - l));
};

static inline uint64_t decode(uint32_t ref) {
  uint8_t l = 31 - __builtin_clz(ref);
  uint32_t a = ref << (32 - l);
  return (((uint64_t)l) << 32) | ((uint64_t)a);
};

static inline uint32_t encode64(uint64_t la) {
  return encode(la >> 32, la & 0xffffffff);
};

static uint32_t *RIB;
static uint32_t *bigtable;
static uint32_t bigtable_index = 0;

void reinit_bigtable() {
  bigtable_index = 0;
  memset(RIB, 0, 4 * RIBSIZE);
  memset(bigtable, 0, 4 * BIG);
};

void init_bigtable() {
  bigtable_index = 0;
  RIB = calloc(4, RIBSIZE);
  bigtable = calloc(4, BIG);
};

void dump_bigtable() {

  uint32_t btindex;
  uint64_t la;
  for (btindex = 0; btindex < bigtable_index; btindex++) {
    la = decode(bigtable[btindex]);
    printf("%6d : %s/%d\n", btindex, inet_ntoa((struct in_addr){la & 0xffffffff}), (uint32_t)(la >> 32));
  };
};

static inline uint64_t lookup_bigtable(uint32_t index) {
  if (index < bigtable_index || TOO_BIG == index)
    ;
  else {
    printf("** index=%d limit=%d _msg_count=%d\n", index, bigtable_index, _msg_count);
    assert(index < bigtable_index || TOO_BIG == index);
  };
  return decode(bigtable[index]);
};

static inline uint32_t lookup_RIB(uint8_t l, uint32_t address) {

  uint32_t index = encode(l, address);
  assert(index < RIBSIZE);
  uint32_t btindex = RIB[index];
  if (0 == btindex) {
    btindex = bigtable_index++;
    RIB[index] = btindex;
    bigtable[btindex] = index;
  };
  return btindex;
};

static inline uint32_t lookup_RIB64(uint64_t la) {
  if (25 > (la >> 32))
    return lookup_RIB(la >> 32, la & 0xffffffff);
  else {
    return TOO_BIG;
  };
};
#endif
