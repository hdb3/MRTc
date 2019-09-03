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

int mrt_item_process(void *p, uint32_t l) {
  uint16_t msg_type, msg_subtype;

  msg_type = getw16(p + 4);
  assert(TABLE_DUMP_V2 == msg_type);
  msg_subtype = getw16(p + 6);
  assert(PEER_INDEX_TABLE == msg_subtype || RIB_IPV4_UNICAST == msg_subtype);
  return 1;
};

int mrt_list_walker(struct chunk buf) {
  void *mrt_buffer_base = buf.data;
  void *mrt_buffer_ptr = mrt_buffer_base;
  void *mrt_buffer_next;
  void *mrt_buffer_limit = mrt_buffer_base + buf.length;
  int mrt_item_count = 0;
  uint32_t mrt_rec_length;

  while (mrt_buffer_ptr < mrt_buffer_limit) {
    assert(MIN_MRT_LENGTH <= mrt_buffer_limit - mrt_buffer_ptr);
    mrt_rec_length = getw32(mrt_buffer_ptr + 8);
    assert(mrt_rec_length >= MIN_MRT_LENGTH);
    mrt_buffer_next = mrt_buffer_ptr + mrt_rec_length;
    assert(mrt_buffer_next <= mrt_buffer_limit);

    if (mrt_item_process(mrt_buffer_ptr, mrt_rec_length)) {
      mrt_item_count++;
      mrt_buffer_ptr = mrt_buffer_next;
    } else {
      printf("exception processing MRT message %d\n", mrt_item_count);
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
  return rval;
};
