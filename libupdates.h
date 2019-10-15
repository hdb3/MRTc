#define _GNU_SOURCE
// #include <arpa/inet.h>
// #include <assert.h>
// #include <fcntl.h>
// #include <stdint.h>
// #include <stdio.h>
// #include <stdlib.h>
// #include <string.h>
// #include <sys/mman.h>
// #include <sys/stat.h>
// #include <sys/types.h>
// #include <unistd.h>

// #include "libmrt.h"
//
// arbitrary constants for fixed size route structs
#define MAX_PATH_LENGTH 50
#define MAX_COMMUNITY_LENGTH 10
#define MAX_EXTENDED_COMMUNITY_LENGTH 10
#define MAX_LARGE_COMMUNITY_LENGTH 10

// typedef struct large_community { uint32_t word[3] } large_community;
struct large_community { uint32_t word[3]; };
struct route {
  uint64_t attributes;
  uint8_t origin;
  uint8_t path_length;
  uint32_t as_path[MAX_PATH_LENGTH];
  uint32_t next_hop;
  uint32_t med;
  uint32_t local_pref;
  uint8_t communities_length;
  uint32_t communities[MAX_COMMUNITY_LENGTH];
  uint8_t extended_communities_length;
  uint64_t extended_communities[MAX_EXTENDED_COMMUNITY_LENGTH];
  uint8_t large_communities_length;
  // large_community large_communities[MAX_LARGE_COMMUNITY_LENGTH];
  // struct large_community { uint32_t word[3] } large_communities[MAX_LARGE_COMMUNITY_LENGTH];
  struct large_community large_communities[MAX_LARGE_COMMUNITY_LENGTH];
};

// BGP protocol constants
//
#define ORIGIN 1
#define AS_PATH 2
#define NEXT_HOP 3
#define MULTI_EXIT_DISC 4
#define LOCAL_PREF 5
#define AGGREGATOR 7
#define COMMUNITY 8
#define ORIGINATOR_ID 9
#define CLUSTER_LIST 10
#define MP_REACH_NLRI 14
#define MP_UNREACH_NLRI 15
#define EXTENDED_COMMUNITIES 16
#define AS4_PATH 17
#define AS4_AGGREGATOR 18
#define LARGE_COMMUNITY 32
#define BGPsec_Path 33
