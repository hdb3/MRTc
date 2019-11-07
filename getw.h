
#ifndef __GETW_H
#define __GETW_H
int _msg_count = 0;
static inline uint16_t getw16(void *p) { return __bswap_16(*(uint16_t *)p); };
static inline uint32_t getw32(void *p) { return __bswap_32(*(uint32_t *)p); }
static inline uint8_t getw8(void *p) { return *(uint8_t *)p; }

#endif
