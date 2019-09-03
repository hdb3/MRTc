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
#define N_LARGE_TABLE 500000

struct mrtrib_ribentry {
  // struct mrtrib_ribentry *next;
  struct chunk prefix;
  struct chunk path_attributes;
};

struct mrtrib_peerrecord {
  int index;
  uint32_t peer_as;
  uint32_t peer_ip;
  uint32_t peer_bgpid;
  int entry_count;
  struct mrtrib_ribentry *rib_entry_table;
};

struct mrtrib_peertable {
  int mrt_count;
  int ipv4_unicast_count;
  int non_ipv4_unicast_count;
  int peer_count;
  struct mrtrib_peerrecord *peer_table;
};

static uint16_t peer_count = 0;
static struct mrtrib_peerrecord *peer_table;

// there should be a simple cast over the function signature which
// would alias the natural function, but I could not make it compile ;-(
// int compare_mrtrib_peerrecord(struct mrtrib_peerrecord *a, struct mrtrib_peerrecord *b) {
int compare_mrtrib_peerrecord(const void *a, const void *b) {
  struct mrtrib_peerrecord *_a = (struct mrtrib_peerrecord *)a;
  struct mrtrib_peerrecord *_b = (struct mrtrib_peerrecord *)b;
  return (_b->entry_count - _a->entry_count);
};

void sort_peertable(struct mrtrib_peertable *pt) {
  int i = 0;
  for (i = 0; i < pt->peer_count; i++)
    pt->peer_table[i].index = i;

  qsort(pt->peer_table, pt->peer_count, sizeof(struct mrtrib_peerrecord), compare_mrtrib_peerrecord);
  // for (i = 0; i < pt->peer_count; i++) {
  // if (0 == pt->peer_table[i].entry_count)
  // break;
  // printf("%d %d %d\n", i, pt->peer_table[i].index, pt->peer_table[i].entry_count);
  // };
};

void analyse_mrtrib_peertable(struct mrtrib_peertable *pt) {
  int i, max_entry_count, min_entry_count, non_zero_entry_counts, aggregate_counts;
  int large_table_count = 0;
  printf("\nMRT RIB dump Peer Table\n\n");
  min_entry_count = peer_table[0].entry_count;
  max_entry_count = 0;
  aggregate_counts = 0;
  non_zero_entry_counts = 0;
  for (i = 0; i < peer_count; i++) {
    if (0 < peer_table[i].entry_count) {
      // printf("%d - %d\n", i, peer_table[i].entry_count);
      non_zero_entry_counts++;
      aggregate_counts += peer_table[i].entry_count;
    };
    min_entry_count = min_entry_count > peer_table[i].entry_count ? peer_table[i].entry_count : min_entry_count;
    max_entry_count = max_entry_count < peer_table[i].entry_count ? peer_table[i].entry_count : max_entry_count;
  };
  printf("max_entry_count: %d\n", max_entry_count);
  printf("min_entry_count: %d\n", min_entry_count);
  printf("non_zero_entry_counts: %d\n", non_zero_entry_counts);
  printf("aggregate_counts: %d\n", aggregate_counts);
  sort_peertable(pt);
  for (i = 0; i < peer_count; i++)
    if (N_LARGE_TABLE <= peer_table[i].entry_count)
      large_table_count++;
  printf("large_table_count: %d (>%d)\n", large_table_count, N_LARGE_TABLE);
  for (i = 0; i < large_table_count; i++)
    printf("%d %d %d\n", i, pt->peer_table[i].index, pt->peer_table[i].entry_count);
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

static int count_RIB_IPV4_UNICAST_entries = 0;
int process_RIB_IPV4_UNICAST_entry(uint16_t peer_index, struct chunk bgp_attributes, struct chunk nlri) {
  count_RIB_IPV4_UNICAST_entries++;
  assert(peer_table[peer_index].entry_count < 999999);
  if (0 == peer_table[peer_index].entry_count) {
    peer_table[peer_index].rib_entry_table = calloc(1000, sizeof(struct mrtrib_ribentry));
  } else if (999 == peer_table[peer_index].entry_count) {
    peer_table[peer_index].rib_entry_table = realloc(peer_table[peer_index].rib_entry_table, 1000000 * sizeof(struct mrtrib_ribentry));
  };
  peer_table[peer_index].rib_entry_table[peer_table[peer_index].entry_count++] = (struct mrtrib_ribentry){nlri, bgp_attributes};
};

int process_RIB_IPV4_UNICAST(void *p, uint32_t l) {
  // only get here if the subtype is RIB_IPV4_UNICAST
  // the pointer is to the payload, not the MRT container
  int i;
  struct chunk nlri, bgp_attributes;
  uint16_t peer_index, attribute_length;
  void *nlri_ptr = p + 4;
  uint8_t prefix_length = *(uint8_t *)nlri_ptr;
  uint8_t prefix_size = (prefix_length + 15) / 8;
  uint16_t entry_count = getw16(p + 4 + prefix_size);
  void *rib_entries = p + 6 + prefix_size;
  void *rib_entries_limit = p + l;
  nlri.length = prefix_size;
  nlri.data = nlri_ptr;
  for (i = 0; i < entry_count; i++) {
    peer_index = getw16(rib_entries);
    assert(peer_index <= peer_count);
    attribute_length = getw16(rib_entries + 6);
    bgp_attributes.length = attribute_length;
    bgp_attributes.data = rib_entries + 8;
    process_RIB_IPV4_UNICAST_entry(peer_index, bgp_attributes, nlri);
    rib_entries += attribute_length + 8;
  };
  if (rib_entries != rib_entries_limit)
    printf("excepttion in process_RIB_IPV4_UNICAST %d %p %p %p\n", i, p, rib_entries, rib_entries_limit);
  assert(rib_entries == rib_entries_limit);
};

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
      process_RIB_IPV4_UNICAST(p + MIN_MRT_LENGTH, l);
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
  peer_count = parse_mrt_TABLE_DUMP_V2(buf);
  printf("parse_mrt_TABLE_DUMP_V2: %d peers\n", peer_count);
  rval->peer_count = peer_count;
  rval->peer_table = calloc(peer_count, sizeof(struct mrtrib_peerrecord));
  peer_table = rval->peer_table;
  rval->mrt_count = mrt_list_walker(buf);
  assert(1 == count_PEER_INDEX_TABLE);
  rval->ipv4_unicast_count = count_RIB_IPV4_UNICAST;
  rval->non_ipv4_unicast_count = count_RIB_IPV4_MULTICAST + count_RIB_IPV6_UNICAST + count_RIB_IPV6_MULTICAST + count_RIB_GENERIC;
  printf("get_mrtrib_peertable: %d count_RIB_IPV4_UNICAST_entries\n", count_RIB_IPV4_UNICAST_entries);
  return rval;
};
