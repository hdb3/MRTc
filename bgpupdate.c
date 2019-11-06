#include <arpa/inet.h>
#include <assert.h>
#include <fcntl.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "aspath.c"
#include "getw.h"
#include "libupdates2.h"

static inline void parse_attribute(uint8_t type_code, void *p, uint16_t length, struct route *route) {

  if ((type_code > 64) && (ATTR_SET != type_code))
    printf("unexpected type code %d\n", type_code);
  else
    switch (type_code) {

    case ORIGIN:
      route->tiebreak.origin = *(uint8_t *)p;
      break;

    case AS_PATH:
      route->tiebreak.path_length = as_path_count(p, length);
      break;

    case NEXT_HOP:
      assert(4 == length);
      // route->next_hop = *(uint32_t *)p;
      break;

    case MULTI_EXIT_DISC:
      assert(4 == length);
      route->tiebreak.med = *(uint32_t *)p;
      break;

    case LOCAL_PREF:
      assert(4 == length);
      route->tiebreak.local_pref = *(uint32_t *)p;
      break;

    case COMMUNITY:
      assert(0 == length % 4);
      break;

    case EXTENDED_COMMUNITIES:
      assert(0 == length % 8);
      break;

    case LARGE_COMMUNITY:
      assert(0 == length % 12);
      break;

    case ATOMIC_AGGREGATE:
      assert(0 == length);
      break;

    case AGGREGATOR:
      assert(8 == length); // this the AS4 case - otherwise would be 6 not 8
      break;

    case MP_REACH_NLRI:
      break;

    case MP_UNREACH_NLRI:
      break;

    case AS_PATHLIMIT:
      assert(5 == length);
      break;

    case CONNECTOR:
      break;

    case ATTR_SET:
      break;

    default:
      printf("unexpected attribute, type code =%d\n", type_code);
    }
};

static inline void parse_attributes(void *p, uint16_t length, struct route *r) {
  void *limit = p + length;
  uint8_t flags, type_code;
  uint16_t attr_length;

  do {
    flags = *(uint8_t *)p++;
    type_code = *(uint8_t *)p++;
    attr_length = *(uint8_t *)p++;
    if (0x10 & flags)
      attr_length = attr_length << 8 | (*(uint8_t *)p++);
    parse_attribute(type_code, p, attr_length, r);
    p += attr_length;
  } while (p < limit);
  assert(p == limit);
};
