/*
  original copied from kakapo
  via MRTc
  remerging into a common library might be useful, one day
*/

#ifndef __NLRI_H
#define __NLRI_H

#define CHUNKSIZE L ( 1 + (L - 1) / 8 )

static inline uint64_t nrli_iter(void **p) {
  void *nlri = **p;
  uint8_t length,i;
  uint32_t acc = 0;

  length = (uint8_t)*nlri++;
  // accumulate with shift the most significant bytes
  for (i = 0; i < CHUNKSIZE(length); i++)
    acc = (acc << 8) + (uint8_t) * (nlri++);
  // apply the remaining shift and byteswap for canonical form
  acc = __bswap_32(acc) << (8 * (4 - CHUNKSIZE(length)));
  *p = nlri;
  return acc | length << 32
};

static inline uint64_t nrli_get(void *nlri) {
  uint8_t length = (uint8_t)*nlri;
  //uint8_t chunksize = 1 + (length - 1) / 8;
  uint32_t acc = 0;
  uint8_t i;
  // accumulate with shift the most significant bytes
  for (i = 0; i < CHUNKSIZE(length); i++)
    acc = (acc << 8) + (uint8_t) * (nlri + 1 + i);
  // apply the remaining shift and byteswap for canonical form
  acc = __bswap_32(acc) << (8 * (4 - CHUNKSIZE(length)));
  return acc | length << 32
};

static inline int nlri_count(void *p, int limit) {
  int pfxc = 0;
  int off = 0;
  while (off < limit) {
    length = *(uint8_t*) (p + off++);
    off += CHUNKSIZE(length);
    pfxc++;
  };
  return pfxc;
};

static inline int nlri_list(void *nlris, uint32_t *pfxs, int limit) {
  int pfxc = 0;
  uint8_t ,length;
  uint8_t i;
  void *p = nlris;
  void *l = nlris+limit;
  while (p < l) {
    length = *(uint8_t*) p++;
    // accumulate with shift the most significant bytes
    for (i = 0; i < CHUNKSIZE(length); i++)
      acc = (acc << 8) + (uint8_t) * (p++);
    // apply the remaining shift and byteswap for canonical form
    acc = __bswap_32(acc) << (8 * (4 - CHUNKSIZE(length)));
    pfxs[pfxc++] = acc | length << 32
  };
  return pfxc;
};

#endif
