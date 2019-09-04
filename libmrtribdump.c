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
#include "libmrtribdump.h"

static struct mrtrib *rib;

void build_updates(struct message_stream *ms, struct mrtrib_peerrecord *pr) {
  long int i, length;
  void *p;
  unsigned char marker[16] = {0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff};
  uint16_t attributes_length, update_length;

  length = 0;
  for (i = 0; i < pr->entry_count; i++)
    length += 23 + pr->rib_entry_table[i].prefix.length + pr->rib_entry_table[i].path_attributes.length;
  printf("build_updates: allocating %ld\n", length);
  p = malloc(length);
  assert(NULL != p);
  ms->base = p;
  ms->length = length;
  for (i = 0; i < pr->entry_count; i++) {
    memcpy(p, marker, 16);
    update_length = 23 + pr->rib_entry_table[i].prefix.length + pr->rib_entry_table[i].path_attributes.length;
    *(uint16_t *)(p + 16) = __bswap_16(update_length);
    *(uint8_t *)(p + 18) = 2;  // type == update
    *(uint16_t *)(p + 19) = 0; //   Withdrawn Routes Length
    attributes_length = (uint16_t)pr->rib_entry_table[i].path_attributes.length;
    *(uint16_t *)(p + 21) = __bswap_16(attributes_length);
    memcpy(p + 23, pr->rib_entry_table[i].path_attributes.data, pr->rib_entry_table[i].path_attributes.length);
    memcpy(p + 23 + update_length, pr->rib_entry_table[i].prefix.data, pr->rib_entry_table[i].prefix.length);
    p += update_length;
  };
  assert(p == ms->base + ms->length);
};

// there should be a simple cast over the function signature which
// would alias the natural function, but I could not make it compile ;-(
// int compare_mrtrib_peerrecord(struct mrtrib_peerrecord *a, struct mrtrib_peerrecord *b) {
int compare_mrtrib_peerrecord(const void *a, const void *b) {
  struct mrtrib_peerrecord *_a = (struct mrtrib_peerrecord *)a;
  struct mrtrib_peerrecord *_b = (struct mrtrib_peerrecord *)b;
  return (_b->entry_count - _a->entry_count);
};

void show_mrtrib_peerrecord(struct mrtrib_peerrecord *peer) {
  char peer_ip_str[INET6_ADDRSTRLEN];
  if (peer->is_ipv6)
    inet_ntop(AF_INET6, &peer->peer_ip6, peer_ip_str, INET6_ADDRSTRLEN);
  else
    inet_ntop(AF_INET, &peer->peer_ip, peer_ip_str, sizeof(struct sockaddr));
  printf("[%d %s AS%6d ", peer->index, peer_ip_str, peer->peer_as);
  printf("%s]", inet_ntoa((struct in_addr){peer->peer_bgpid}));
};

void sort_peertable(struct mrtrib *rib) {
  int i = 0;
  for (i = 0; i < rib->peer_count; i++)
    rib->peer_table[i].index = i;

  qsort(rib->peer_table, rib->peer_count, sizeof(struct mrtrib_peerrecord), compare_mrtrib_peerrecord);
  struct message_stream ms;
  build_updates(&ms, &rib->peer_table[0]);
};

void analyse_mrtrib(struct mrtrib *rib) {
  int i, max_entry_count, min_entry_count, non_zero_entry_counts, aggregate_counts;
  int large_table_count = 0;
  printf("\nMRT RIB dump Peer Table\n\n");
  min_entry_count = rib->peer_table[0].entry_count;
  max_entry_count = 0;
  aggregate_counts = 0;
  non_zero_entry_counts = 0;
  for (i = 0; i < rib->peer_count; i++) {
    if (0 < rib->peer_table[i].entry_count) {
      non_zero_entry_counts++;
      aggregate_counts += rib->peer_table[i].entry_count;
    };
    min_entry_count = min_entry_count > rib->peer_table[i].entry_count ? rib->peer_table[i].entry_count : min_entry_count;
    max_entry_count = max_entry_count < rib->peer_table[i].entry_count ? rib->peer_table[i].entry_count : max_entry_count;
  };
  printf("max_entry_count: %d\n", max_entry_count);
  printf("min_entry_count: %d\n", min_entry_count);
  printf("non_zero_entry_counts: %d\n", non_zero_entry_counts);
  printf("aggregate_counts: %d\n", aggregate_counts);
  sort_peertable(rib);
  for (i = 0; i < rib->peer_count; i++)
    if (N_LARGE_TABLE <= rib->peer_table[i].entry_count)
      large_table_count++;
  printf("large_table_count: %d (>%d)\n", large_table_count, N_LARGE_TABLE);
  for (i = 0; i < large_table_count; i++) {
    printf("%d: ", i);
    show_mrtrib_peerrecord(&rib->peer_table[i]);
    printf(" %d\n", rib->peer_table[i].entry_count);
  };
};

void report_mrtrib(struct mrtrib *rib) {
  printf("\nMRT RIB dump Peer Table\n\n");
  printf("got %d MRT items\n", rib->mrt_count);
  if (0 == rib->non_ipv4_unicast_count)
    printf("got %d IPv4 unicast rib entries\n", rib->ipv4_unicast_count);
  else {
    printf("got %d rib entries\n", rib->non_ipv4_unicast_count + rib->ipv4_unicast_count);
    printf("got %d IPv4 unicast rib entries\n", rib->ipv4_unicast_count);
    printf("got %d non IPv4 unicast rib entries\n", rib->non_ipv4_unicast_count);
  };
  printf("peer table contains %d records\n", rib->peer_count);
};

static int count_PEER_INDEX_TABLE = 0;
static int count_RIB_IPV4_UNICAST = 0;
static int count_RIB_IPV4_MULTICAST = 0;
static int count_RIB_IPV6_UNICAST = 0;
static int count_RIB_IPV6_MULTICAST = 0;
static int count_RIB_GENERIC = 0;
static int max_peer_index = 0;

static int count_RIB_IPV4_UNICAST_entries = 0;
int process_RIB_IPV4_UNICAST_entry(uint16_t peer_index, struct chunk bgp_attributes, struct chunk nlri) {
  max_peer_index = max_peer_index < peer_index ? peer_index : max_peer_index;
  count_RIB_IPV4_UNICAST_entries++;
  assert(rib->peer_table[peer_index].entry_count < 999999);
  if (0 == rib->peer_table[peer_index].entry_count) {
    rib->peer_table[peer_index].rib_entry_table = calloc(1000, sizeof(struct mrtrib_ribentry));
  } else if (999 == rib->peer_table[peer_index].entry_count) {
    rib->peer_table[peer_index].rib_entry_table = realloc(rib->peer_table[peer_index].rib_entry_table, 1000000 * sizeof(struct mrtrib_ribentry));
  };
  rib->peer_table[peer_index].rib_entry_table[rib->peer_table[peer_index].entry_count++] = (struct mrtrib_ribentry){nlri, bgp_attributes};
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
    assert(peer_index <= rib->peer_count);
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

uint16_t parse_mrt_TABLE_DUMP_V2(struct mrtrib *rib, struct chunk buf) {
  uint32_t mrt_rec_length, mrt_as;
  uint16_t msg_type, msg_subtype, mrt_peer_count, mrt_view_name_length;
  uint8_t peer_type;
  void *peer_entries, *peer_entries_limit;
  int i;
  int ip_addr_length, as_addr_length;
  int offset = 0;

  assert(MIN_MRT_LENGTH <= buf.length);
  mrt_rec_length = getw32(buf.data + 8);
  msg_type = getw16(buf.data + 4);
  assert(TABLE_DUMP_V2 == msg_type);
  msg_subtype = getw16(buf.data + 6);
  assert(PEER_INDEX_TABLE == msg_subtype);
  mrt_view_name_length = getw16(buf.data + OFFSET_View_Name_Length);
  mrt_peer_count = getw16(buf.data + OFFSET_View_Name_Length + 2 + mrt_view_name_length);
  rib->peer_table = calloc(mrt_peer_count, sizeof(struct mrtrib_peerrecord));
  peer_entries = buf.data + OFFSET_View_Name_Length + 4 + mrt_view_name_length;
  peer_entries_limit = peer_entries + buf.length;
  for (i = 0; i < mrt_peer_count; i++) {
    assert(peer_entries_limit > peer_entries);
    peer_type = *(uint8_t *)peer_entries;
    ip_addr_length = (0x01 & peer_type) ? 16 : 4;
    as_addr_length = (0x02 & peer_type) ? 4 : 2;
    printf("%3d (%2x:%1d/%1d) ", i, peer_type, ip_addr_length, as_addr_length);

    if (2 == as_addr_length) // 16 bit peer AS, may still be operating in AS4 sessions??
      rib->peer_table[i].peer_as = getw16(peer_entries + 5 + ip_addr_length);
    else
      rib->peer_table[i].peer_as = getw32(peer_entries + 5 + ip_addr_length);

    rib->peer_table[i].is_ipv6 = (16 == ip_addr_length);
    rib->peer_table[i].index = i;
    rib->peer_table[i].peer_bgpid = __bswap_32(getw32(peer_entries + 1));

    if (rib->peer_table[i].is_ipv6) // IPv6 peer may still have IPv4 data!!
      rib->peer_table[i].peer_ip6 = *(struct in6_addr *)(peer_entries + 5);
    else
      rib->peer_table[i].peer_ip = (struct in_addr){__bswap_32(getw32(peer_entries + 5))};

    show_mrtrib_peerrecord(&rib->peer_table[i]);
    // printf(" %d\n", i);
    printf(" @%d\n", offset);
    offset += 5 + ip_addr_length + as_addr_length;
    peer_entries += 5 + ip_addr_length + as_addr_length;
  };
  printf("%d peers processed, peer_entries %p peer_entries_limit %p [%ld]\n", i, peer_entries, peer_entries_limit, peer_entries_limit - peer_entries);
  // why does this assertion fail???
  // assert(peer_entries_limit == peer_entries);
  rib->peer_count = mrt_peer_count;
};

struct mrtrib *get_mrtrib(struct chunk buf) {

  rib = calloc(1, sizeof(struct mrtrib));

  // parse the initial MRT record which must be type TABLE_DUMP_V2 subtype PEER_INDEX_TABLE
  parse_mrt_TABLE_DUMP_V2(rib, buf);
  printf("parse_mrt_TABLE_DUMP_V2: %d peers\n", rib->peer_count);
  rib->mrt_count = mrt_list_walker(buf);
  assert(1 == count_PEER_INDEX_TABLE);
  rib->ipv4_unicast_count = count_RIB_IPV4_UNICAST;
  rib->non_ipv4_unicast_count = count_RIB_IPV4_MULTICAST + count_RIB_IPV6_UNICAST + count_RIB_IPV6_MULTICAST + count_RIB_GENERIC;
  printf("get_mrtrib: %d count_RIB_IPV4_UNICAST_entries\n", count_RIB_IPV4_UNICAST_entries);
  printf("get_mrtrib: highest active peer index: %d\n", max_peer_index);
  return rib;
};

/*
// a useful scrap for hex
    if (rib->peer_table[i].is_ipv6) {
      uint8_t *p = peer_entries + 5;
      int j;
      printf("[");
      for (j=0;j<10;j++)
        printf(" %02x",*p++);
      printf("]\n");
    };
*/
