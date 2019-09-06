#include <stdint.h>
#include <stdlib.h>
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

// MRT RIB dump stream constants
#define TABLE_DUMP_V2 13
#define PEER_INDEX_TABLE 1
#define RIB_IPV4_UNICAST 2
#define RIB_IPV4_MULTICAST 3
#define RIB_IPV6_UNICAST 4
#define RIB_IPV6_MULTICAST 5
#define RIB_GENERIC 6

struct chunk {
  void *data;
  int length;
};

struct msg_list_item {
  struct msg_list_item *next;
  struct chunk msg;
};

struct stats_bgp4mp_mrt {
  int mrt_count;
  int bgp_messages;
  int state_changes;
  int as2_discards;
  int ipv6_discards;
  struct msg_list_item *msg_list;
  int peer_count;
  struct bgp4mp_peer *peers;
};

struct stats_bgp4mp_bgp {
  int open_count;
  int update_count;
  int eor_count;
  int keepalive_count;
  int notification_count;
  int withdraw_count;
  int mixed_update_count;
};

struct bgp4mp_peer {
  int mrt_file_index;
  struct msg_list_item *msg_list;
  int msg_list_length;
  uint8_t peer_header[BGP4MP_PEER_HEADER_LENGTH];
  struct stats_bgp4mp_bgp bgp_stats;
};

void report_stats_bgp4mp_bgp(struct stats_bgp4mp_bgp *sp);
void report_stats_bgp4mp_mrt(struct stats_bgp4mp_mrt *sp);

static inline uint16_t getw16(void *p) { return __bswap_16(*(uint16_t *)p); };

static inline uint32_t getw32(void *p) { return __bswap_32(*(uint32_t *)p); };

void print_chunk(struct chunk ch);
void unmap_mrt_file(struct chunk ch);
struct chunk map_mrt_file(char *fname);

void mrt_parse(struct chunk buf, struct stats_bgp4mp_mrt *sp);

int count_msg_list(struct msg_list_item *list);
struct msg_list_item *filter_msgs(struct msg_list_item *list, struct stats_bgp4mp_bgp *spb);
