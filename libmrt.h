#include <arpa/inet.h>
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

// MRT TABLE dump stream constants
#define TABLE_DUMP_V2 13
#define PEER_INDEX_TABLE 1
#define RIB_IPV4_UNICAST 2
#define RIB_IPV4_MULTICAST 3
#define RIB_IPV6_UNICAST 4
#define RIB_IPV6_MULTICAST 5
#define RIB_GENERIC 6

#define ORIGIN 1
#define AS_PATH 2
#define NEXT_HOP 3
#define MULTI_EXIT_DISC 4
#define LOCAL_PREF 5

#define OFFSET_View_Name_Length (MIN_MRT_LENGTH + 4)
#define OFFSET_View_Name (MIN_MRT_LENGTH + 6)

#define ADD_LOCAL_PREF 1

struct chunk {
  void *data;
  int length;
};

struct msg_list_item {
  struct msg_list_item *next;
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
  int mixed_update_count;
  int mpbgp_count;
  // path attribute level
  int med_count;
  int ibgp_count;
};

struct bgp4mp_peer {
  int rec_count;
  int bgp_msg_count;
  int update_count;
  struct msg_list_item *msg_list_head, *msg_list_tail;
  uint8_t peer_header[BGP4MP_PEER_HEADER_LENGTH];
  struct bgp4mp_bgp_stats bgp_stats;
};

struct mrt_peer_record {
  int mrt_file_index;
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

struct mrt_tabledump {
  int mrt_count;
  int ipv4_unicast_count;
  int non_ipv4_unicast_count;
  int peer_count;
  struct mrt_peer_record *peer_table;
  int count_PEER_INDEX_TABLE;
  int count_RIB_IPV4_UNICAST;
  int count_RIB_IPV4_MULTICAST;
  int count_RIB_IPV6_UNICAST;
  int count_RIB_IPV6_MULTICAST;
  int count_RIB_GENERIC;
  int max_peer_index;
};

struct mrt_bgp4mp {
  int mrt_msg_count;
  int mrt_bgp_msg_count;
  int state_changes;
  int as2_discards;
  int ipv6_discards;
  struct msg_list_item *msg_list_head, *msg_list_tail;
  int msg_list_length;
  int peer_count;
  struct mrt_peer_record *peer_table;
};

void build_tabledump_updates(struct mrt_peer_record *pr);
void build_mrt_tabledump_tabledump_updates(struct mrt_tabledump *tabledump, int requested_table_size);

void build_mrt_tabledump_bgp4mp_updates(struct mrt_tabledump *tabledump, struct mrt_bgp4mp *sp);
void write_mrt_tabledump_all_updates(struct mrt_tabledump *tabledump);

struct chunk get_updates(struct mrt_tabledump *rib, int index);

struct mrt_tabledump *get_mrt_tabledump(struct chunk buf);
void report_mrt_tabledump(struct mrt_tabledump *pt);
void analyse_mrt_tabledump(struct mrt_tabledump *pt);
void build_updates_mrt_tabledump(struct mrt_tabledump *tabledump, int requested_table_size);
void sort_peer_table(struct mrt_tabledump *pt);

static inline int compare_bgp4mp_peer(const void *a, const void *b) {
  struct mrt_peer_record *_a = (struct mrt_peer_record *)a;
  struct mrt_peer_record *_b = (struct mrt_peer_record *)b;
  return (_b->bgp4mp.update_count - _a->bgp4mp.update_count);
};

static inline void sort_bgp4mp_peers(struct mrt_bgp4mp *sp) {
  qsort(sp->peer_table, sp->peer_count, sizeof(struct mrt_peer_record), compare_bgp4mp_peer);
};

void report_bgp4mp_bgp_stats(struct bgp4mp_bgp_stats *sp);
void report_mrt_bgp4mp(struct mrt_bgp4mp *sp);

static inline uint16_t getw16(void *p) { return __bswap_16(*(uint16_t *)p); };

static inline uint32_t getw32(void *p) { return __bswap_32(*(uint32_t *)p); };

void print_chunk(struct chunk ch);
void unmap_mrt_file(struct chunk ch);
void write_chunk(const char *fname, struct chunk buf);
struct chunk map_mrt_file(char *fname);

void mrt_parse(struct chunk buf, struct mrt_bgp4mp *sp);
struct chunk *get_blocks_bgp4mp(struct mrt_bgp4mp *sp, int nblocks);
struct chunk get_one_bgp4mp(struct mrt_bgp4mp *sp, int peer, int msg_number);

int count_msg_list(struct msg_list_item *list);
struct msg_list_item *filter_msgs(struct msg_list_item *list, struct bgp4mp_bgp_stats *spb);
void show_mrt_peer_record(struct mrt_peer_record *peer);
struct chunk get_blocks_bgp4mp_peer(struct mrt_bgp4mp *sp, uint32_t as, struct in_addr ip);
struct chunk update_fixup_localpreference(uint32_t local_preference, struct chunk update);
