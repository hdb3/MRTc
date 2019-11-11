#include <stdio.h>
#include <stdint.h>
#include "bigtable.c"


void try(uint8_t l, uint32_t a) {
  uint64_t a64 = ((uint64_t) l) << 32 | ((uint64_t) a);

  uint32_t ref = lookup_RIB64(a64);
  uint64_t addr = lookup_bigtable(ref);

  // uint32_t ref = encode64(a64);
  // uint64_t addr = decode(ref);
  printf("%010lx %08x %010lx\n\n",a64,ref,addr);
  assert(addr == a64);
};

int main(int argc, char **argv) {

/*
  encode(16,0x80000000);
  encode(17,0xffffffff);
  encode(18,0xffffffff);
  encode(19,0xffffffff);
  encode(20,0xffffffff);
  encode(21,0xffffffff);
  encode(22,0xffffffff);
  encode(23,0xffffffff);
  encode(24,0x00000000);
*/
  init_bigtable();
  // try(16,0x80000000);
  // try(17,0xffffffff);
  // try(18,0xffffffff);
  // try(19,0xffffffff);
  // try(20,0xffffffff);
  // try(21,0xffffffff);
  // try(22,0xffffffff);
  try(24,0x80000000);
  try(24,0xffffff00);
  try(23,0xfffff000);
  try(22,0xfffff000);
};
