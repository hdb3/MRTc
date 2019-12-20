#include <arpa/inet.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#ifndef BUILD_UPDATE_LIST
#define BUILD_UPDATE_LIST 0
#endif
#ifndef CDFFILE
#define CDFFILE 1
#endif
#define MAX(x, y) ((y) < (x) ? (x) : (y))
#define MIN_MRT_LENGTH 12
#define MIN_MRT_LENGTH_ET 16
#define BGP4MP_IPV4_PEER_HEADER_LENGTH 20
#define BGP4MP_IPV6_PEER_HEADER_LENGTH 44
#define BGP4MP_PEER_HEADER_LENGTH 44

// MRT Update stream constants
#define BGP4MP 16
#define BGP4MP_ET 17
#define BGP4MP_STATE_CHANGE 0
#define BGP4MP_MESSAGE 1
#define BGP4MP_MESSAGE_AS4 4
#define BGP4MP_STATE_CHANGE_AS4 5
#define BGP4MP_MESSAGE_LOCAL 6
#define BGP4MP_MESSAGE_AS4_LOCAL 7

// MRT TABLE dump stream constants
#define TABLE_DUMP_V2 13
#define PEER_INDEX_TABLE 1
#define RIB_IPV4_UNICAST 2
#define RIB_IPV4_MULTICAST 3
#define RIB_IPV6_UNICAST 4
#define RIB_IPV6_MULTICAST 5
#define RIB_GENERIC 6

#define MULTI_EXIT_DISC 4
#define LOCAL_PREF 5

#define OFFSET_View_Name_Length (MIN_MRT_LENGTH + 4)
#define OFFSET_View_Name (MIN_MRT_LENGTH + 6)

#define ADD_LOCAL_PREF 1

struct chunk {
  void *data;
  uint64_t length;
};

struct update_list_item {
  struct update_list_item *next;
  struct chunk msg;
};

struct mrt_ribentry {
  struct chunk prefix;
  struct chunk path_attributes;
};

struct mrt_tabledump_peer_record {
  int count;
  struct mrt_ribentry *table;
};

struct bgp4mp_bgp_stats {
  int msg_count;
  int open_count;
  int all_update_count;
  int notification_count;
  int keepalive_count;
  // update sub-types
  int update_count;
  int eor_count;
  int withdraw_count;
  int complex_path_count;
  int mpbgp_count;
  int attr_set_count;
  // path attribute level
  int med_count;
  int ibgp_count;
  int max_update_length;
  int max_path_length;
  int max_raw_attributes_size;
  int max_raw_community_size;
  int max_raw_nlri_size;
  int max_nlri_length;
  int max_community_length;
};

struct bgp4mp_peer {
  int rec_count;
  int bgp_msg_count;
  int update_count;
  struct update_list_item *update_list_head, *update_list_tail;
  uint8_t peer_header[BGP4MP_PEER_HEADER_LENGTH];
  struct bgp4mp_bgp_stats bgp_stats;
};

struct mrt_peer_record {
  int mrt_file_index;
  struct mrt_peer_record *link;
  uint32_t local_as;
  uint32_t peer_as;
  uint32_t peer_bgpid;
  union {
    struct in_addr peer_ip;
    struct in6_addr peer_ip6;
  };
  union {
    struct in_addr local_ip;
    struct in6_addr local_ip6;
  };
  struct chunk tabledump_updates;
  struct chunk bgp4mp_updates;
  union {
    struct mrt_tabledump_peer_record rib;
    struct bgp4mp_peer bgp4mp;
  };
  int8_t is_ipv6;
};

static int peer_addr_compare(struct mrt_peer_record *peer_a, struct mrt_peer_record *peer_b) {
  return (peer_a->is_ipv6 == peer_a->is_ipv6) && ((peer_a->is_ipv6) ? (0 == memcmp(&peer_a->peer_ip6, &peer_b->peer_ip6, sizeof(peer_a->peer_ip6))) : (0 == memcmp(&peer_a->peer_ip, &peer_b->peer_ip, sizeof(peer_a->peer_ip))));
};

struct mrt_tabledump {
  int count_PEER_INDEX_TABLE;
  int count_RIB_IPV4_UNICAST;
  int count_RIB_IPV4_MULTICAST;
  int count_RIB_IPV6_UNICAST;
  int count_RIB_IPV6_MULTICAST;
  int count_RIB_GENERIC;
  int max_peer_index;
};

struct mrt_bgp4mp {
  int mrt_bgp_msg_count;
  int state_changes;
  int as2_discards;
  int ipv6_discards;
  // struct bgp4mp_bgp_stats bgp_stats;
  struct update_list_item *update_list_head, *update_list_tail;
  int update_count;
};

#define TYPE_TABLEDUMP 1
#define TYPE_BGP4MP 2
struct mrt {
  int type;
  int peer_count;
  int mrt_rec_count;
  struct mrt_peer_record *peer_table;
  union {
    struct mrt_tabledump tabledump;
    struct mrt_bgp4mp bgp4mp;
  };
};

static inline uint16_t getw16(void *p) { return __bswap_16(*(uint16_t *)p); };
static inline uint32_t getw32(void *p) { return __bswap_32(*(uint32_t *)p); };

extern int verbose;
extern int unquiet;
#include "libmrtcommon.h"
#include "libmrtextra.h"
#include "libmrttabledump.h"
#include "libmrttabledumpextra.h"
#include "libmrtupdates.h"
#include "libupdates.h"
#include "nlri.h"
