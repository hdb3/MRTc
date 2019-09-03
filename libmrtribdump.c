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

#define OFFSET_Peer_Count (MIN_MRT_LENGTH + 8)
#define OFFSET_Peer_Entries (MIN_MRT_LENGTH + 10)

struct mrtrib_ribentry {
  struct mrtrib_ribentry *next;
  struct chunk prefix;
  struct chunk path_attributes;
};

struct mrtrib_peerrecord {
  uint32_t peer_as;
  uint32_t peer_ip;
  uint32_t peer_bgpid;
  int entry_count;
  struct mrtrib_ribentry *rib_entry_list;
};

struct mrtrib_peertable {
  int mrt_count;
  int ipv4_unicast_count;
  int non_ipv4_unicast_count;
  int peer_count;
  struct mrtrib_peerrecord *peer_table;
};

void report_mrtrib_peertable(struct mrtrib_peertable *pt) {
  printf("\nMRT RIB dump Peer Table\n\n");
  printf("got %d MRT items\n", pt->mrt_count);
  if (0 == pt->non_ipv4_unicast_count)
    printf("got %d IPv4 unicast rib entries\n", pt->ipv4_unicast_count);
  else {
    printf("got %d rib entries\n", pt->non_ipv4_unicast_count + pt->ipv4_unicast_count);
    printf("got %d IPv4 unicast rib entries\n", pt->ipv4_unicast_count);
    printf("got %d non IPv4 unicast rib entries\n", pt->non_ipv4_unicast_count);
  };
  printf("peer table contains %d records\n", pt->peer_count);
};

static int count_PEER_INDEX_TABLE = 0;
static int count_RIB_IPV4_UNICAST = 0;
static int count_RIB_IPV4_MULTICAST = 0;
static int count_RIB_IPV6_UNICAST = 0;
static int count_RIB_IPV6_MULTICAST = 0;
static int count_RIB_GENERIC = 0;

int mrt_item_process(void *p, uint32_t l) {
  uint16_t msg_type, msg_subtype;
  int rval = 1;

  msg_type = getw16(p + 4);
  msg_subtype = getw16(p + 6);
  assert(TABLE_DUMP_V2 == msg_type);
  assert(PEER_INDEX_TABLE <= msg_subtype || RIB_GENERIC >= msg_subtype);
  if (TABLE_DUMP_V2 == msg_type)
    switch (msg_subtype) {
    case PEER_INDEX_TABLE:
      count_PEER_INDEX_TABLE++;
      break;
    case RIB_IPV4_UNICAST:
      count_RIB_IPV4_UNICAST++;
      break;
    case RIB_IPV4_MULTICAST:
      count_RIB_IPV4_MULTICAST++;
      break;
    case RIB_IPV6_UNICAST:
      count_RIB_IPV6_UNICAST++;
      break;
    case RIB_IPV6_MULTICAST:
      count_RIB_IPV6_MULTICAST++;
      break;
    case RIB_GENERIC:
      count_RIB_GENERIC++;
      break;
    default:
      printf("subtype exception in mrt_item_process %d/%d\n", msg_type, msg_subtype);
      rval = 0;
    }
  else
    printf("type exception in mrt_item_process %d/%d\n", msg_type, msg_subtype);
  return rval;
};

int mrt_list_walker(struct chunk buf) {
  void *mrt_buffer_base = buf.data;
  void *mrt_buffer_ptr = mrt_buffer_base;
  void *mrt_buffer_next;
  void *mrt_buffer_limit = mrt_buffer_base + buf.length;
  int mrt_item_count = 0;
  uint32_t mrt_rec_length;

  while (mrt_buffer_ptr < mrt_buffer_limit) {
    // printf("mrt_list_walker cycle %d %p %ld\n", mrt_item_count, mrt_buffer_ptr, mrt_buffer_ptr - mrt_buffer_base);
    assert(MIN_MRT_LENGTH <= mrt_buffer_limit - mrt_buffer_ptr);
    mrt_rec_length = getw32(mrt_buffer_ptr + 8);
    mrt_buffer_next = mrt_buffer_ptr + mrt_rec_length + MIN_MRT_LENGTH;
    assert(mrt_buffer_next <= mrt_buffer_limit);

    if (mrt_item_process(mrt_buffer_ptr, mrt_rec_length)) {
      mrt_item_count++;
      mrt_buffer_ptr = mrt_buffer_next;
    } else {
      printf("exception processing MRT message %d\n", mrt_item_count);
      exit(1);
    };
  };
  assert(mrt_buffer_ptr == mrt_buffer_limit);
  return mrt_item_count;
};

uint16_t parse_mrt_TABLE_DUMP_V2(struct chunk buf) {
  uint32_t mrt_rec_length;
  uint16_t msg_type, msg_subtype, peer_count;

  assert(MIN_MRT_LENGTH <= buf.length);
  mrt_rec_length = getw32(buf.data + 8);
  assert(mrt_rec_length >= MIN_MRT_LENGTH);
  // should really check it is at least the length of a TABLE_DUMP_V2 PEER_INDEX_TABLE
  msg_type = getw16(buf.data + 4);
  assert(TABLE_DUMP_V2 == msg_type);
  msg_subtype = getw16(buf.data + 6);
  assert(PEER_INDEX_TABLE == msg_subtype);
  peer_count = getw16(buf.data + OFFSET_Peer_Count);

  return peer_count;
};

struct mrtrib_peertable *get_mrtrib_peertable(struct chunk buf) {

  struct mrtrib_peertable *rval = calloc(1, sizeof(struct mrtrib_peertable));

  // parse the initial MRT record which must be type TABLE_DUMP_V2 subtype PEER_INDEX_TABLE
  uint16_t peer_count = parse_mrt_TABLE_DUMP_V2(buf);
  rval->peer_count = peer_count;
  rval->peer_table = calloc(peer_count, sizeof(struct mrtrib_peerrecord));
  rval->mrt_count = mrt_list_walker(buf);
  assert(1 == count_PEER_INDEX_TABLE);
  rval->ipv4_unicast_count = count_RIB_IPV4_UNICAST;
  rval->non_ipv4_unicast_count = count_RIB_IPV4_MULTICAST + count_RIB_IPV6_UNICAST + count_RIB_IPV6_MULTICAST + count_RIB_GENERIC;
  return rval;
};
