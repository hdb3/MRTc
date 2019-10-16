#define _GNU_SOURCE

// arbitrary constants for fixed size route structs
#define MAX_PATH_LENGTH 255
#define MAX_COMMUNITY_LENGTH 255
#define MAX_EXTENDED_COMMUNITY_LENGTH 255
#define MAX_LARGE_COMMUNITY_LENGTH 255

// typedef struct large_community { uint32_t word[3] } large_community;
struct large_community { uint32_t word[3]; };
struct route {
  uint64_t attributes;
  uint8_t complex_path;
  uint8_t origin;
  uint8_t exception;
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
#define ATOMIC_AGGREGATE 6
#define AGGREGATOR 7
#define COMMUNITY 8
#define ORIGINATOR_ID 9
#define CLUSTER_LIST 10
#define MP_REACH_NLRI 14
#define MP_UNREACH_NLRI 15
#define EXTENDED_COMMUNITIES 16
#define AS4_PATH 17
#define AS4_AGGREGATOR 18
#define	CONNECTOR 20
#define	AS_PATHLIMIT 21
#define LARGE_COMMUNITY 32
#define BGPsec_Path 33
#define ATTR_SET 128
#define _ATTR_SET 63 // kludge to allow use of 64-bit word for a set representation