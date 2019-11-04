#include <assert.h>
#include <stddef.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <sys/mman.h>

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

static inline uint32_t encode (uint8_t l, uint32_t a) {
  return (0x100000000 | (uint64_t)a) >> (32-l);
};

static inline uint32_t encode64 (uint64_t la) {
  return encode ( la >> 32 , la & 0xffffffff);
};

static uint32_t bigtable [ 2 << 23 ];
#define BIG 1000000
static uint32_t bigtable_index = 0;

void init_bigtable() {
  bigtable_index = 0;
  memset(&bigtable,0,BIG*4);
};

uint32_t lookup_bigtable (uint8_t l, uint32_t address) {
  uint32_t index = encode(l, address);
  uint32_t *entry = bigtable+index;
  if (0 == *entry) {
    uint32_t rval = bigtable_index++;
    *entry = rval;
    return rval;
  } else
    return *entry;
};
