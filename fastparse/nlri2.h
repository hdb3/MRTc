/*
  original copied from kakapo
  via MRTc
  remerging into a common library might be useful, one day
*/

#ifndef __NLRI_H
#define __NLRI_H

#define CHUNKSIZE(L) (1 + (L - 1) / 8)

static inline uint64_t nlri_iter(void **p) {
  void *nlri;
  uint8_t length, i;
  uint32_t acc = 0;

  nlri = *p;
  length = *(uint8_t *)(nlri++);
  // accumulate with shift the most significant bytes
  for (i = 0; i < CHUNKSIZE(length); i++)
    acc = (acc << 8) + *(uint8_t *)(nlri++);
  // apply the remaining shift and byteswap for canonical form
  acc = acc << (8 * (4 - CHUNKSIZE(length)));
  *p = nlri;
  return acc | ((uint64_t)length) << 32;
};

static inline uint64_t nlri_get(void *nlri) {
  uint8_t length = *(uint8_t *)nlri;
  uint32_t acc = 0;
  uint8_t i;
  // accumulate with shift the most significant bytes
  for (i = 0; i < CHUNKSIZE(length); i++)
    acc = (acc << 8) + *(uint8_t *)(nlri + 1 + i);
  // apply the remaining shift and byteswap for canonical form
  acc = acc << (8 * (4 - CHUNKSIZE(length)));
  return acc | ((uint64_t)length) << 32;
};

static inline int nlri_count(void *p, int limit) {
  int pfxc = 0;
  int off = 0;
  while (off < limit) {
    uint8_t length = *(uint8_t *)(p + off++);
    off += CHUNKSIZE(length);
    pfxc++;
  };
  return pfxc;
};

static inline int nlri_list(void *nlris, uint32_t *pfxs, int limit) {
  int pfxc = 0;
  uint8_t i;
  uint32_t acc = 0;
  void *p = nlris;
  void *l = nlris + limit;
  while (p < l) {
    uint8_t length = *(uint8_t *)p++;
    // accumulate with shift the most significant bytes
    for (i = 0; i < CHUNKSIZE(length); i++)
      acc = (acc << 8) + *(uint8_t *)(p++);
    // apply the remaining shift and byteswap for canonical form
    acc = acc << (8 * (4 - CHUNKSIZE(length)));
    pfxs[pfxc++] = acc | ((uint64_t)length) << 32;
  };
  return pfxc;
};

static inline void build_nlri(uint8_t **nlri, uint64_t prefix){
  uint8_t *p  = *nlri;

  uint32_t address = __bswap_32((uint32_t) (0xffffffff & (uint32_t)prefix));
  uint8_t length = (uint8_t) (0xff & (prefix >> 32));
  uint8_t chunk;
  *(p++) = length;
  for (chunk=0;chunk<CHUNKSIZE(length) ;chunk++) {
    *(p++) = (uint8_t) (0xff & address);
    address >> 8;
  };
  *nlri = p;
};

#endif
