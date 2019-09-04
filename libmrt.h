#include <stdint.h>
#include <stdlib.h>
#define LENGTH_BGP4MP_COMMON_AS4 20
#define MIN_MRT_LENGTH 12

// MRT Update stream constants
#define BGP4MP 16
#define BGP4MP_STATE_CHANGE 0
#define BGP4MP_MESSAGE 1
#define BGP4MP_MESSAGE_AS4 4
#define BGP4MP_STATE_CHANGE_AS4 5

// MRT RIB dump stream constants
#define TABLE_DUMP_V2 13
#define PEER_INDEX_TABLE 1
#define RIB_IPV4_UNICAST 2
#define RIB_IPV4_MULTICAST 3
#define RIB_IPV6_UNICAST 4
#define RIB_IPV6_MULTICAST 5
#define RIB_GENERIC 6

struct stats_bgp4mp {
  // MRT parse level
  int mrt_count;
  int bgp_messages;
  int state_changes;
  int as2_discards;
  int ipv6_discards;

  // bgp parse level
  int open_count;
  int update_count;
  int eor_count;
  int keepalive_count;
  int notification_count;
  int withdraw_count;
  int mixed_update_count;
};

void report_stats_bgp4mp(struct stats_bgp4mp *sp);

struct chunk {
  void *data;
  int length;
};

struct msg_list_item {
  struct msg_list_item *next;
  struct chunk msg;
};

static inline uint16_t getw16(void *p) { return __bswap_16(*(uint16_t *)p); };

static inline uint32_t getw32(void *p) { return __bswap_32(*(uint32_t *)p); };

void print_chunk(struct chunk ch);
void unmap_mrt_file(struct chunk ch);
struct chunk map_mrt_file(char *fname);

struct msg_list_item *mrt_parse(struct chunk buf, struct stats_bgp4mp *sp);

int count_msg_list(struct msg_list_item *list);
struct msg_list_item *filter_msgs(struct msg_list_item *list,
                                  struct stats_bgp4mp *sp);

struct peer {
  uint32_t peer_as;
  uint32_t local_as;
  uint32_t peer_ip;
  uint32_t local_ip;
};
