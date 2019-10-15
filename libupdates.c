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

#include "libmrt.h"
#include "libupdates.h"
#define MAX_PATH_LENGTH 50
#define MAX_COMMUNITY_LENGTH 10
#define MAX_EXTENDED_COMMUNITY_LENGTH 10
#define MAX_LARGE_COMMUNITY_LENGTH 10

  static inline void process_path_attribute_route(uint8_t type_code, struct chunk msg, struct route * route) {

    assert(type_code < 64);
    route->attributes |= (1ULL << (type_code));
    switch (type_code) {

    case ORIGIN:
      route->origin = *(uint8_t*) msg.data;
      break;

    case AS_PATH:
      // assume that the as_PATH is a singleton AS_SEQUENCE
      route->path_length = * (uint8_t *) (msg.data + 1);
      assert(MAX_PATH_LENGTH >= route->path_length);
      assert(1 == *(uint8_t*)msg.data);              // AS_SEQUENCE segment type == 1
      assert(msg.length == 2 + 4 * route->path_length); // the test that the segment exactly fills the attribute
      memcpy(route->as_path, msg.data + 2, msg.length - 2);
      break;

    case NEXT_HOP:
      assert(4 == msg.length);
      route->next_hop = *(uint32_t*)msg.data;
      break;
    case MULTI_EXIT_DISC:
      assert(4 == msg.length);
      route->med = *(uint32_t*)msg.data;
      break;
    case LOCAL_PREF:
      assert(4 == msg.length);
      route->local_pref = *(uint32_t*)msg.data;
      break;
    case COMMUNITY:
      route->communities_length = msg.length >> 2; // standard community is 4 bytes
      assert(0 == route->communities_length % 4);
      assert(MAX_COMMUNITY_LENGTH >= route->communities_length);
      memcpy(route->communities, msg.data, msg.length);
      break;
    case EXTENDED_COMMUNITIES:
      route->extended_communities_length = msg.length >> 3; // extended community is 8 bytes
      assert(0 == route->extended_communities_length % 8);
      assert(MAX_EXTENDED_COMMUNITY_LENGTH >= route->extended_communities_length);
      memcpy(route->extended_communities, msg.data, msg.length);
      break;
    case LARGE_COMMUNITY:
      route->large_communities_length = msg.length / 12; // standard community is 4 bytes
      assert(0 == route->large_communities_length % 12);
      assert(MAX_LARGE_COMMUNITY_LENGTH >= route->large_communities_length);
      memcpy(route->large_communities, msg.data, msg.length);
      break;
    default:
      printf("unexpected attribute, type code =%d\n", type_code);
    }
  };
/*
  static inline void process_path_attributes(struct chunk msg, struct route * route) {
    void *p = msg.data;
    void *limit = msg.data + msg.length;
    uint8_t flags, type_code;
    uint16_t length;
    while (p < limit) {
      flags = *(uint8_t *)p++;
      type_code = *(uint8_t *)p++;
      length = *(uint8_t *)p++;
      if (0x10 & flags)
        length = length << 8 | (*(uint8_t *)p++);
      process_path_attribute_route(type_code, (struct chunk){p, length}, route);
      p += length;
    };
    if (p != limit) {
      printf("process_path_attributes exception %p %p %p %ld %ld\n", msg.data, limit, p, limit - p, msg.length);
    };
    assert(p == limit);
  };
  */
