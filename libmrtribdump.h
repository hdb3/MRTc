#include <arpa/inet.h>

#define OFFSET_View_Name_Length (MIN_MRT_LENGTH + 4)
#define OFFSET_View_Name (MIN_MRT_LENGTH + 6)
#define N_LARGE_TABLE 500000

struct mrt_ribentry {
  struct chunk prefix;
  struct chunk path_attributes;
};

struct mrt_ribdump_peerrecord {
  int count;
  struct mrt_ribentry *table;
};

struct mrt_peerrecord {
  int file_index;
  uint32_t peer_as;
  uint32_t peer_bgpid;
  int is_ipv6;
  union {
    struct in_addr peer_ip;
    struct in6_addr peer_ip6;
  };
  struct chunk updates;
  struct mrt_ribdump_peerrecord rib;
};

struct mrt_ribdump {
  int mrt_count;
  int ipv4_unicast_count;
  int non_ipv4_unicast_count;
  int peer_count;
  struct mrt_peerrecord *peer_table;
  int count_PEER_INDEX_TABLE;
  int count_RIB_IPV4_UNICAST;
  int count_RIB_IPV4_MULTICAST;
  int count_RIB_IPV6_UNICAST;
  int count_RIB_IPV6_MULTICAST;
  int count_RIB_GENERIC;
  int max_peer_index;
};

void build_updates(struct mrt_peerrecord *pr);
struct chunk get_updates(struct mrt_ribdump *rib, int index);

struct mrt_ribdump *get_mrt_ribdump(struct chunk buf);
void report_mrt_ribdump(struct mrt_ribdump *pt);
void analyse_mrt_ribdump(struct mrt_ribdump *pt);
void sort_peertable(struct mrt_ribdump *pt);
