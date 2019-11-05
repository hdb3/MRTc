#define _GNU_SOURCE
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
#include "getw.h"
#include "libupdates2.h"


static inline void process_path_attribute_route(uint8_t type_code, void *p, uint16_t length, struct route *route ) {

  if ((type_code > 64) && (ATTR_SET != type_code))
    printf("unexpected type code %d\n", type_code);
  else
    switch (type_code) {

  case ORIGIN:
    route->tiebreak.origin = * (uint8_t *) p;
    break;
  case AS_PATH:
/*
    // assume that the as_PATH is a singleton AS_SEQUENCE
    route->path_length = *(uint8_t *)(p + 1);
    if (length > 2 + 4 * route->path_length) {
      // printf("complex AS_PATH: (%ld,%d) ", length, route->path_length);
      // print_chunk(msg);
      route->complex_path = 1;
    };
    assert(MAX_PATH_LENGTH >= route->path_length);
    assert(2 == *(uint8_t *)p);                // AS_SEQUENCE segment type == 1
    assert(length >= 2 + 4 * route->path_length); // the test that the segment exactly fills the attribute
    memcpy(route->as_path, p + 2, length - 2);
*/
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

static inline void process_path_attributes(void *p, uint16_t length, struct route *r) {
  void *limit = p + length;
  uint8_t flags, type_code;
  uint16_t attr_length;
  
  do {
    flags = *(uint8_t *)p++;
    type_code = *(uint8_t *)p++;
    attr_length = *(uint8_t *)p++;
    if (0x10 & flags)
      attr_length = attr_length << 8 | (*(uint8_t *)p++);
    process_path_attribute_route(type_code, p, attr_length,r );
    p += attr_length;
  } while (p < limit);
  assert(p == limit);
};


/*
 * design for basic update processing - just locate the sections
*/
static inline void parse_update(void *p, uint16_t length, struct route *r) {

    uint16_t withdraw_length = getw16(p);
    uint16_t pathattributes_length = getw16(p + 2 + withdraw_length);
    uint16_t nlri_length = length - withdraw_length - pathattributes_length - 4;
    assert(length >= 4 + withdraw_length + pathattributes_length); // sanity check
    void *withdrawn = p + 2;
    void * path_attributes = p + 4 + withdraw_length;
    void * nlri = p + 4 + withdraw_length + pathattributes_length;

    process_path_attributes(path_attributes,pathattributes_length,r);
};
