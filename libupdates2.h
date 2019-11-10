#ifndef __LIBUPDATES2_H
#define __LIBUPDATES2_H

#define _GNU_SOURCE

struct tiebreak {
  uint32_t local_pref;
  uint8_t  path_length;
  uint8_t  origin;
  uint32_t med;
  uint8_t  ebgp;
  uint32_t igp_metric;
  uint32_t bgpid;
  uint32_t peer_address;
};

struct route {
  uint64_t unique;
  uint16_t use_count;
  uint16_t update_length;
  struct tiebreak tiebreak;
  /*
  struct offsets offsets {
    uint8_t origin;
    uint8_t as_path;
    uint8_t next_hop;
    uint8_t med;
    uint8_t loc_pref;
    uint8_t at_agg;
    uint8_t agg;
    uint8_t community;
    uint8_t originator_id;
    uint8_t cluster_list;
    uint8_t mp_reach;
    uint8_t mp_unreach;
    uint8_t extended_community;
    uint8_t as4_path;
    uint8_t as4_agg;
    uint8_t large_community;
    uint8_t bgpsec_path;
  };
*/
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
#endif
