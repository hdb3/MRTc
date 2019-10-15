/*
  original copied from kakapo
  remerging into a common library might be useful, one day
*/

#ifndef __NLRI_H
#define __NLRI_H

struct prefix {
  uint32_t ip;
  uint8_t length;
};
// extern inline void nlri_list(struct chunk nlris, struct prefix *pfxs, int limit);
// extern inline int nlri_count(struct chunk nlris);

static inline void get_prefix_nlri(char *nlri, struct prefix *pfx) {
  uint8_t length = (uint8_t)*nlri;
  uint8_t chunksize = 1 + (length - 1) / 8;
  uint32_t acc = 0;
  uint8_t i, b;
  // accumulate with shift the most significant bytes
  for (i = 0; i < chunksize; i++)
    acc = (acc << 8) + (uint8_t) * (nlri + 1 + i);
  // apply the remaining shift and byteswap for canonical form
  acc = __bswap_32(acc) << (8 * (4 - chunksize));
  *pfx = (struct prefix){acc, length};
};

static inline void nlri_list(struct chunk nlris, struct prefix *pfxs, int limit) {
  uint8_t length, chunksize;
  int offset = 0;
  int pfx_count = 0;
  while (offset < nlris.length) {
    length = *(uint8_t*) (nlris.data + offset);
    get_prefix_nlri(nlris.data + offset, pfxs + pfx_count++ );
    // get_prefix_nlri(nlris.data + offset, pfxs[pfx_count++] );
    chunksize = 1 + (length + 7) / 8;
    offset += chunksize;
    assert(pfx_count<limit);
  };
};

static inline int nlri_count(struct chunk nlris) {
  uint8_t length, chunksize;
  int offset = 0;
  int pfx_count = 0;
  while (offset < nlris.length) {
    length = *(uint8_t*) (nlris.data + offset);
    pfx_count++;
    chunksize = 1 + (length + 7) / 8;
    offset += chunksize;
  };
  return pfx_count;
};

#endif
