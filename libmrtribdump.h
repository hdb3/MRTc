#include <arpa/inet.h>

#define OFFSET_View_Name_Length (MIN_MRT_LENGTH + 4)
#define OFFSET_View_Name (MIN_MRT_LENGTH + 6)
#define N_LARGE_TABLE 500000

struct mrtrib_ribentry {
  struct chunk prefix;
  struct chunk path_attributes;
};

struct mrtrib_peerrecord {
  int index;
  uint32_t peer_as;
  uint32_t peer_bgpid;
  int entry_count;
  struct mrtrib_ribentry *rib_entry_table;
  int is_ipv6;
  union {
    struct in_addr peer_ip;
    struct in6_addr peer_ip6;
  };
  struct chunk updates;
};

struct mrtrib {
  int mrt_count;
  int ipv4_unicast_count;
  int non_ipv4_unicast_count;
  int peer_count;
  struct mrtrib_peerrecord *peer_table;
};

void build_updates(struct mrtrib_peerrecord *pr);
struct chunk get_updates(struct mrtrib *rib, int index);

struct mrtrib *get_mrtrib(struct chunk buf);
void report_mrtrib(struct mrtrib *pt);
void analyse_mrtrib(struct mrtrib *pt);
void sort_peertable(struct mrtrib *pt);
