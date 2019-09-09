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

struct chunk get_updates(struct mrt_tabledump *rib, int index) {
  assert(index < rib->peer_count);
  return rib->peer_table[index].updates;
};

void build_updates(struct mrt_peer_record *pr) {
  long int i, length;
  void *p;
  unsigned char marker[16] = {0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff};
  uint16_t attributes_length, update_length;

  length = 0;
  for (i = 0; i < pr->rib.count; i++)
    length += 23 + pr->rib.table[i].prefix.length + pr->rib.table[i].path_attributes.length;
  p = malloc(length);
  assert(NULL != p);
  pr->updates.length = length;
  pr->updates.data = p;
  for (i = 0; i < pr->rib.count; i++) {
    struct mrt_ribentry *re = &pr->rib.table[i];
    memcpy(p, marker, 16);
    update_length = 23 + re->prefix.length + re->path_attributes.length;
    *(uint16_t *)(p + 16) = __bswap_16(update_length);
    *(uint8_t *)(p + 18) = 2;  // type == update
    *(uint16_t *)(p + 19) = 0; //   Withdrawn Routes Length
    attributes_length = (uint16_t)re->path_attributes.length;
    *(uint16_t *)(p + 21) = __bswap_16(attributes_length);
    memcpy(p + 23, re->path_attributes.data, attributes_length);
    memcpy(p + 23 + attributes_length, re->prefix.data, re->prefix.length);
    p += update_length;
  };
  assert(p == pr->updates.length + pr->updates.data);
};

// there should be a simple cast over the function signature which
// would alias the natural function, but I could not make it compile ;-(
// int compare_mrt_peer_record(struct mrt_peer_record *a, struct mrt_peer_record *b) {
int compare_mrt_peer_record(const void *a, const void *b) {
  struct mrt_peer_record *_a = (struct mrt_peer_record *)a;
  struct mrt_peer_record *_b = (struct mrt_peer_record *)b;
  return (_b->rib.count - _a->rib.count);
};

void show_mrt_peer_record(struct mrt_peer_record *peer) {
  char peer_ip_str[INET6_ADDRSTRLEN];
  if (peer->is_ipv6)
    inet_ntop(AF_INET6, &peer->peer_ip6, peer_ip_str, INET6_ADDRSTRLEN);
  else
    inet_ntop(AF_INET, &peer->peer_ip, peer_ip_str, sizeof(struct sockaddr));
  printf("[%-3d %-15s AS%-6d ", peer->mrt_file_index, peer_ip_str, peer->peer_as);
  printf("%-15s]", inet_ntoa((struct in_addr){peer->peer_bgpid}));
};

void sort_peer_table(struct mrt_tabledump *tabledump) {
  int i = 0;
  for (i = 0; i < tabledump->peer_count; i++)
    tabledump->peer_table[i].mrt_file_index = i;

  qsort(tabledump->peer_table, tabledump->peer_count, sizeof(struct mrt_peer_record), compare_mrt_peer_record);
};

void analyse_mrt_tabledump(struct mrt_tabledump *tabledump) {
  int i, max_entry_count, min_entry_count, non_zero_entry_counts, aggregate_counts;
  int large_table_count = 0;
  printf("\nMRT TABLE dump Peer Table\n\n");
  min_entry_count = tabledump->peer_table[0].rib.count;
  max_entry_count = 0;
  aggregate_counts = 0;
  non_zero_entry_counts = 0;
  struct mrt_tabledump_peer_record *rib;
  for (i = 0; i < tabledump->peer_count; rib = &tabledump->peer_table[i++].rib) {
    if (0 < rib->count) {
      non_zero_entry_counts++;
      aggregate_counts += rib->count;
    };
    min_entry_count = min_entry_count > rib->count ? rib->count : min_entry_count;
    max_entry_count = max_entry_count < rib->count ? rib->count : max_entry_count;
  };
  printf("max_entry_count: %d\n", max_entry_count);
  printf("min_entry_count: %d\n", min_entry_count);
  printf("non_zero_entry_counts: %d\n", non_zero_entry_counts);
  printf("aggregate_counts: %d\n", aggregate_counts);
  sort_peer_table(tabledump);
  for (i = 0; i < tabledump->peer_count; i++)
    if (N_LARGE_TABLE <= tabledump->peer_table[i].rib.count)
      large_table_count++;
  printf("large_table_count: %d (>%d)\n", large_table_count, N_LARGE_TABLE);
  for (i = 0; i < large_table_count; i++) {
    printf("%2d: ", i);
    show_mrt_peer_record(&tabledump->peer_table[i]);
    build_updates(&tabledump->peer_table[i]);
    printf(" entries %d allocated update size %d\n", tabledump->peer_table[i].rib.count, tabledump->peer_table[i].updates.length);
  };
};

void report_mrt_tabledump(struct mrt_tabledump *tabledump) {
  printf("\nMRT TABLE dump Peer Table\n\n");
  printf("got %d MRT items\n", tabledump->mrt_count);
  if (0 == tabledump->non_ipv4_unicast_count)
    printf("got %d IPv4 unicast rib entries\n", tabledump->ipv4_unicast_count);
  else {
    printf("got %d rib entries\n", tabledump->non_ipv4_unicast_count + tabledump->ipv4_unicast_count);
    printf("got %d IPv4 unicast rib entries\n", tabledump->ipv4_unicast_count);
    printf("got %d non IPv4 unicast rib entries\n", tabledump->non_ipv4_unicast_count);
  };
  printf("peer table contains %d records\n", tabledump->peer_count);
};

static inline int process_RIB_IPV4_UNICAST_entry(struct mrt_tabledump *tabledump, uint16_t peer_index, struct chunk bgp_attributes, struct chunk nlri) {
  tabledump->max_peer_index = tabledump->max_peer_index < peer_index ? peer_index : tabledump->max_peer_index;
  struct mrt_tabledump_peer_record *rib = &tabledump->peer_table[peer_index].rib;
  assert(rib->count < 999999);
  if (0 == rib->count) {
    rib->table = calloc(1000, sizeof(struct mrt_ribentry));
  } else if (999 == rib->count) {
    rib->table = realloc(rib->table, 1000000 * sizeof(struct mrt_ribentry));
  };
  rib->table[rib->count++] = (struct mrt_ribentry){nlri, bgp_attributes};
};

static inline int process_RIB_IPV4_UNICAST(struct mrt_tabledump *tabledump, void *p, uint32_t l) {
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
    assert(peer_index <= tabledump->peer_count);
    attribute_length = getw16(rib_entries + 6);
    bgp_attributes.length = attribute_length;
    bgp_attributes.data = rib_entries + 8;
    process_RIB_IPV4_UNICAST_entry(tabledump, peer_index, bgp_attributes, nlri);
    rib_entries += attribute_length + 8;
  };
  if (rib_entries != rib_entries_limit)
    printf("exception in process_RIB_IPV4_UNICAST %d %p %p %p\n", i, p, rib_entries, rib_entries_limit);
  assert(rib_entries == rib_entries_limit);
};

static inline int mrt_item_process(struct mrt_tabledump *tabledump, void *p, uint32_t l) {
  uint16_t msg_type, msg_subtype;
  int rval = 1;

  msg_type = getw16(p + 4);
  msg_subtype = getw16(p + 6);
  assert(TABLE_DUMP_V2 == msg_type);
  assert(PEER_INDEX_TABLE <= msg_subtype || RIB_GENERIC >= msg_subtype);
  if (TABLE_DUMP_V2 == msg_type)
    switch (msg_subtype) {
    case PEER_INDEX_TABLE:
      tabledump->count_PEER_INDEX_TABLE++;
      break;
    case RIB_IPV4_UNICAST:
      process_RIB_IPV4_UNICAST(tabledump, p + MIN_MRT_LENGTH, l);
      tabledump->count_RIB_IPV4_UNICAST++;
      break;
    case RIB_IPV4_MULTICAST:
      tabledump->count_RIB_IPV4_MULTICAST++;
      break;
    case RIB_IPV6_UNICAST:
      tabledump->count_RIB_IPV6_UNICAST++;
      break;
    case RIB_IPV6_MULTICAST:
      tabledump->count_RIB_IPV6_MULTICAST++;
      break;
    case RIB_GENERIC:
      tabledump->count_RIB_GENERIC++;
      break;
    default:
      printf("subtype exception in mrt_item_process %d/%d\n", msg_type, msg_subtype);
      rval = 0;
    }
  else
    printf("type exception in mrt_item_process %d/%d\n", msg_type, msg_subtype);
  return rval;
};

void mrt_list_walker(struct mrt_tabledump *tabledump, struct chunk buf) {
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
    if (mrt_item_process(tabledump, mrt_buffer_ptr, mrt_rec_length)) {
      mrt_item_count++;
      mrt_buffer_ptr = mrt_buffer_next;
    } else {
      printf("exception processing MRT message %d\n", mrt_item_count);
      exit(1);
    };
  };
  assert(mrt_buffer_ptr == mrt_buffer_limit);
  tabledump->mrt_count = mrt_item_count;
};

static inline uint16_t parse_mrt_TABLE_DUMP_V2(struct mrt_tabledump *rib, struct chunk buf) {
  uint32_t mrt_rec_length, mrt_as;
  uint16_t msg_type, msg_subtype, mrt_peer_count, mrt_view_name_length;
  uint8_t peer_type;
  void *peer_entries, *peer_entries_limit;
  int i;
  int ip_addr_length, as_addr_length;

  assert(MIN_MRT_LENGTH <= buf.length);
  mrt_rec_length = getw32(buf.data + 8);
  msg_type = getw16(buf.data + 4);
  assert(TABLE_DUMP_V2 == msg_type);
  msg_subtype = getw16(buf.data + 6);
  assert(PEER_INDEX_TABLE == msg_subtype);
  mrt_view_name_length = getw16(buf.data + OFFSET_View_Name_Length);
  mrt_peer_count = getw16(buf.data + OFFSET_View_Name_Length + 2 + mrt_view_name_length);
  rib->peer_table = calloc(mrt_peer_count, sizeof(struct mrt_peer_record));
  peer_entries = buf.data + OFFSET_View_Name_Length + 4 + mrt_view_name_length;
  peer_entries_limit = peer_entries + buf.length;
  for (i = 0; i < mrt_peer_count; i++) {
    struct mrt_peer_record *peer = &rib->peer_table[i];
    assert(peer_entries_limit > peer_entries);
    peer_type = *(uint8_t *)peer_entries;
    peer->is_ipv6 = 0x01 & peer_type;
    ip_addr_length = peer->is_ipv6 ? 16 : 4;
    //ip_addr_length = (0x01 & peer_type) ? 16 : 4;
    as_addr_length = (0x02 & peer_type) ? 4 : 2;

    if (2 == as_addr_length) // 16 bit peer AS, may still be operating in AS4 sessions??
      peer->peer_as = getw16(peer_entries + 5 + ip_addr_length);
    else
      peer->peer_as = getw32(peer_entries + 5 + ip_addr_length);

    //peer->is_ipv6 = (16 == ip_addr_length);
    peer->mrt_file_index = i;
    peer->peer_bgpid = __bswap_32(getw32(peer_entries + 1));

    if (peer->is_ipv6) // IPv6 peer may still have IPv4 data!!
      peer->peer_ip6 = *(struct in6_addr *)(peer_entries + 5);
    else
      peer->peer_ip = (struct in_addr){__bswap_32(getw32(peer_entries + 5))};

    show_mrt_peer_record(peer);
    printf("\n");
    peer_entries += 5 + ip_addr_length + as_addr_length;
  };
  //printf("%d peers processed, peer_entries %p peer_entries_limit %p [%ld]\n", i, peer_entries, peer_entries_limit, peer_entries_limit - peer_entries);
  // why does this assertion fail???
  // assert(peer_entries_limit == peer_entries);
  rib->peer_count = mrt_peer_count;
};

struct mrt_tabledump *get_mrt_tabledump(struct chunk buf) {

  struct mrt_tabledump *tabledump;
  tabledump = calloc(1, sizeof(struct mrt_tabledump));

  // parse the initial MRT record which must be type TABLE_DUMP_V2 subtype PEER_INDEX_TABLE
  parse_mrt_TABLE_DUMP_V2(tabledump, buf);
  printf("parse_mrt_TABLE_DUMP_V2: %d peers\n", tabledump->peer_count);
  mrt_list_walker(tabledump, buf);
  assert(1 == tabledump->count_PEER_INDEX_TABLE);
  tabledump->ipv4_unicast_count = tabledump->count_RIB_IPV4_UNICAST;
  tabledump->non_ipv4_unicast_count = tabledump->count_RIB_IPV4_MULTICAST + tabledump->count_RIB_IPV6_UNICAST + tabledump->count_RIB_IPV6_MULTICAST + tabledump->count_RIB_GENERIC;
  printf("get_mrt_tabledump: %d count_RIB_IPV4_UNICAST_entries\n", tabledump->count_RIB_IPV4_UNICAST);
  printf("get_mrt_tabledump: highest active peer index: %d\n", tabledump->max_peer_index);
  return tabledump;
};
