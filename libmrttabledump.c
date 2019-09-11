#define _GNU_SOURCE
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

struct chunk get_updates(struct mrt *mrt, int index) {
  assert(index < mrt->peer_count);
  return mrt->peer_table[index].tabledump_updates;
};

static unsigned char marker[16] = {0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff};
struct chunk build_update(void *target, const struct chunk withdrawn, const struct chunk path_attributes, const struct chunk nlri) {
  void *base = target;

  uint16_t length = 23 + withdrawn.length + path_attributes.length + nlri.length;
  memcpy(target, marker, 16);
  target += 16;

  *(uint16_t *)target = __bswap_16(length);
  target += 2;

  *(uint8_t *)target = 2; // type == update
  target += 1;

  *(uint16_t *)target = __bswap_16(withdrawn.length);
  target += 2;
  memcpy(target, withdrawn.data, withdrawn.length);
  target += withdrawn.length;

  *(uint16_t *)target = __bswap_16(path_attributes.length);
  target += 2;
  memcpy(target, path_attributes.data, path_attributes.length);
  target += path_attributes.length;

  memcpy(target, nlri.data, nlri.length);
  target += nlri.length;

  return (struct chunk){base, target - base};
};

static uint8_t _buf_fixup_localpreference[4096];
static uint8_t local_preference_buf[7] = {0x40, 5, 4, 0, 0, 0, 0};
struct chunk fixup_localpreference(uint32_t local_preference, struct chunk path_attributes) {
  void *p = path_attributes.data;
  void *limit = path_attributes.data + path_attributes.length;
  uint8_t flags, type_code;
  uint16_t length;
  *(uint32_t *)(local_preference_buf + 3) = __bswap_32(local_preference);
  // find the break point to insert local preference
  // before any attribute with type code which exceeds 5
  while (p < limit) {
    flags = *(uint8_t *)p++;
    type_code = *(uint8_t *)p++;
    if (type_code > 4) {
      assert(type_code != 5);           // there should not be already a LOCAL_PREF attribute
      assert(p > path_attributes.data); // sanity - it should not be the first attribute
                                        // better check would look at the mandatory ones which should have pased by already
      p -= 2;
      break;
    };
    length = *(uint8_t *)p++;
    if (0x10 & flags)
      length = length << 8 | (*(uint8_t *)p++);
    p += length;
  };
  void *break_point = p;
  long int break_offset = break_point - path_attributes.data;
  memcpy(_buf_fixup_localpreference, path_attributes.data, break_offset);
  memcpy(_buf_fixup_localpreference + break_offset, local_preference_buf, 7);
  memcpy(_buf_fixup_localpreference + break_offset + 7, break_point, path_attributes.length - break_offset);
  return (struct chunk){_buf_fixup_localpreference, path_attributes.length + 7};
};

static uint8_t _buf_update_fixup_localpreference[4096];
struct chunk update_fixup_localpreference(uint32_t local_preference, struct chunk update) {

  assert(18 < update.length);
  uint16_t length = getw16(update.data + 16);
  uint8_t typecode = *(uint8_t *)(update.data + 18);
  if (2 != typecode)
    return update;

  // if there are any pathattributes then we have work to do, otherwise not
  // the work consists in copying the three sections NLRI, Attributes and Withdrawn NLRI
  // having modified the section attributes using the function fixup_localpreference
  // for ease of understanding we deconstruct the three sections as chunks
  // and then compose them.
  // NOTE - the input is unaffected, the eventual output lies in static buffer (non-reentrant).

  uint16_t withdraw_length = getw16(update.data + 19);
  uint16_t pathattributes_length = getw16(update.data + 21 + withdraw_length);
  if (0 == pathattributes_length)
    return update;

  uint16_t nlri_length = length - withdraw_length - pathattributes_length - 23;
  assert(length >= 23 + withdraw_length + pathattributes_length); // sanity check
  struct chunk withdrawn = (struct chunk){update.data + 21, withdraw_length};
  struct chunk pathattributes = (struct chunk){update.data + 23 + withdraw_length, pathattributes_length};
  struct chunk nlri = (struct chunk){update.data + 23 + withdraw_length + pathattributes_length, nlri_length};
  struct chunk updated_pathattributes = fixup_localpreference(local_preference, pathattributes);
  return build_update(_buf_update_fixup_localpreference, withdrawn, updated_pathattributes, nlri);
};

void build_tabledump_updates(struct mrt_peer_record *pr) {
  long int i, length;
  void *p;
  uint16_t attributes_length, update_length;

  length = 0;
  for (i = 0; i < pr->rib.count; i++)
    length += 23 + pr->rib.table[i].prefix.length + pr->rib.table[i].path_attributes.length + (ADD_LOCAL_PREF ? 7 : 0);
  p = malloc(length);
  assert(NULL != p);
  pr->tabledump_updates.length = length;
  pr->tabledump_updates.data = p;
  for (i = 0; i < pr->rib.count; i++) {
    struct mrt_ribentry *re = &pr->rib.table[i];
    if (ADD_LOCAL_PREF)
      re->path_attributes = fixup_localpreference(100, re->path_attributes);
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
  assert(p == pr->tabledump_updates.length + pr->tabledump_updates.data);
};

// there should be a simple cast over the function signature which
// would alias the natural function, but I could not make it compile ;-(
// int compare_mrt_peer_record(struct mrt_peer_record *a, struct mrt_peer_record *b) {
static inline int compare_mrt_peer_record(const void *a, const void *b) {
  struct mrt_peer_record *_a = (struct mrt_peer_record *)a;
  struct mrt_peer_record *_b = (struct mrt_peer_record *)b;
  return (_b->rib.count - _a->rib.count);
};

void sort_peer_table(struct mrt *tabledump) {
  assert(TYPE_TABLEDUMP == tabledump->type);
  int i = 0;
  for (i = 0; i < tabledump->peer_count; i++)
    tabledump->peer_table[i].mrt_file_index = i;

  qsort(tabledump->peer_table, tabledump->peer_count, sizeof(struct mrt_peer_record), compare_mrt_peer_record);
};

//void analyse_mrt_tabledump(struct mrt_tabledump *tabledump) {
void analyse_mrt_tabledump(struct mrt *tabledump) {
  assert(TYPE_TABLEDUMP == tabledump->type);
  int i, max_entry_count, min_entry_count, non_zero_entry_counts, aggregate_counts;
  struct mrt_tabledump_peer_record *rib = NULL;
  min_entry_count = tabledump->peer_table[0].rib.count;
  max_entry_count = 0;
  aggregate_counts = 0;
  non_zero_entry_counts = 0;
  for (i = 0; i < tabledump->peer_count; i++) {
    rib = &tabledump->peer_table[i].rib;
    if (0 < rib->count) {
      non_zero_entry_counts++;
      aggregate_counts += rib->count;
    };
    min_entry_count = min_entry_count > rib->count ? rib->count : min_entry_count;
    max_entry_count = max_entry_count < rib->count ? rib->count : max_entry_count;
  };
  printf("analyse_mrt_tabledump: max_entry_count: %d\n", max_entry_count);
  printf("analyse_mrt_tabledump: min_entry_count: %d\n", min_entry_count);
  printf("analyse_mrt_tabledump: non_zero_entry_counts: %d\n", non_zero_entry_counts);
  printf("analyse_mrt_tabledump: aggregate_counts: %d\n", aggregate_counts);
};

void write_mrt_tabledump_all_updates(struct mrt *tabledump) {
  int i;
  char fname[128];
  assert(TYPE_TABLEDUMP == tabledump->type);
  for (i = 0; i < tabledump->peer_count; i++) {
    struct mrt_peer_record *peer = &tabledump->peer_table[i];
    printf("writing stream files       %2d: ", i);
    print_mrt_peer_record(peer);
    printf(" update size %ld\r", peer->bgp4mp_updates.length);
    sprintf(fname, "updates.%02d.bin", i);
    write_chunk(fname, peer->bgp4mp_updates);
    sprintf(fname, "table.%02d.bin", i);
    write_chunk(fname, peer->tabledump_updates);
  };
  printf("write_mrt_tabledump_all_updates: wrote %d table dump update streams\e[K\n", i);
};

void build_mrt_tabledump_bgp4mp_updates(struct mrt *tabledump, struct mrt *updatedump) {
  int i;
  assert(TYPE_TABLEDUMP == tabledump->type);
  for (i = 0; i < tabledump->peer_count; i++) {
    struct mrt_peer_record *peer = &tabledump->peer_table[i];
    assert(!peer->is_ipv6);
    assert(peer->link);
    printf("building bgp4mp_updates    %2d: ", i);
    print_mrt_peer_record(peer);
    struct chunk bgp4mp_updates = get_blocks_bgp4mp_peer(peer->link);
    printf(" update size %ld\r", bgp4mp_updates.length);
    peer->bgp4mp_updates = bgp4mp_updates;
  };
  printf("build_mrt_tabledump_bgp4mp_updates: built %d bgp4mp update streams\e[K\n", i);
};

void build_mrt_tabledump_tabledump_updates(struct mrt *tabledump, int requested_table_size) {
  int i;
  int large_table_count = 0;
  assert(TYPE_TABLEDUMP == tabledump->type);
  sort_peer_table(tabledump);
  for (i = 0; i < tabledump->peer_count; i++)
    if (requested_table_size <= tabledump->peer_table[i].rib.count)
      large_table_count++;
  printf("large_table_count: %d (>%d)\n", large_table_count, requested_table_size);
  for (i = 0; i < large_table_count; i++) {
    struct mrt_peer_record *peer = &tabledump->peer_table[i];
    printf("building tabledump updates %2d: ", i);
    print_mrt_peer_record(peer);
    build_tabledump_updates(peer);
    printf(" entries %d update size %ld\r", peer->rib.count, peer->tabledump_updates.length);
  };
  printf("build_mrt_tabledump_tabledump_updates: built %d tabledump update streams\e[K\n", i);
  for (i = large_table_count; i < tabledump->peer_count; i++) {
    struct mrt_peer_record *peer = &tabledump->peer_table[i];

    // leave print staments for diagnostics...!
    // printf("freeing %2d: ", i);
    // show_mrt_peer_record(peer);
    assert(NULL == peer->tabledump_updates.data);
    assert(0 == peer->tabledump_updates.length);
    free(peer->rib.table);
    // printf("\n");
  };
  tabledump->peer_count = large_table_count;
};

void report_mrt_tabledump(struct mrt *mrt) {
  assert(TYPE_TABLEDUMP == mrt->type);
  printf("\nreport_mrt_tabledump\n");
  printf("report_mrt_tabledump: %d MRT items\n", mrt->mrt_rec_count);
  int non_ipv4_unicast_count = mrt->tabledump.count_RIB_IPV4_MULTICAST + mrt->tabledump.count_RIB_IPV6_UNICAST + mrt->tabledump.count_RIB_IPV6_MULTICAST + mrt->tabledump.count_RIB_GENERIC;
  if (0 == non_ipv4_unicast_count)
    printf("report_mrt_tabledump: %d IPv4 unicast rib entries\n", mrt->tabledump.count_RIB_IPV4_UNICAST);
  else {
    printf("report_mrt_tabledump: %d rib entries\n", non_ipv4_unicast_count + mrt->tabledump.count_RIB_IPV4_UNICAST);
    printf("report_mrt_tabledump: %d IPv4 unicast rib entries\n", mrt->tabledump.count_RIB_IPV4_UNICAST);
    printf("report_mrt_tabledump: %d non IPv4 unicast rib entries\n", non_ipv4_unicast_count);
  };
  printf("report_mrt_tabledump: %d peer records\n", mrt->peer_count);
};

static inline void process_RIB_IPV4_UNICAST_entry(struct mrt *mrt, uint16_t peer_index, struct chunk bgp_attributes, struct chunk nlri) {
  assert(TYPE_TABLEDUMP == mrt->type);
  mrt->tabledump.max_peer_index = mrt->tabledump.max_peer_index < peer_index ? peer_index : mrt->tabledump.max_peer_index;
  struct mrt_tabledump_peer_record *rib = &mrt->peer_table[peer_index].rib;
  assert(rib->count < 999999);
  if (0 == rib->count) {
    rib->table = calloc(1000, sizeof(struct mrt_ribentry));
  } else if (999 == rib->count) {
    rib->table = realloc(rib->table, 1000000 * sizeof(struct mrt_ribentry));
  };
  rib->table[rib->count++] = (struct mrt_ribentry){nlri, bgp_attributes};
};

static inline void process_RIB_IPV4_UNICAST(struct mrt *mrt, void *p, uint32_t l) {
  // only get here if the subtype is RIB_IPV4_UNICAST
  // the pointer is to the payload, not the MRT container
  assert(TYPE_TABLEDUMP == mrt->type);
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
    assert(peer_index <= mrt->peer_count);
    attribute_length = getw16(rib_entries + 6);
    bgp_attributes.length = attribute_length;
    bgp_attributes.data = rib_entries + 8;
    process_RIB_IPV4_UNICAST_entry(mrt, peer_index, bgp_attributes, nlri);
    rib_entries += attribute_length + 8;
  };
  if (rib_entries != rib_entries_limit)
    printf("exception in process_RIB_IPV4_UNICAST %d %p %p %p\n", i, p, rib_entries, rib_entries_limit);
  assert(rib_entries == rib_entries_limit);
};

static inline int mrt_item_process(struct mrt *mrt, void *p, uint32_t l) {
  assert(TYPE_TABLEDUMP == mrt->type);
  uint16_t msg_type, msg_subtype;
  int rval = 1;

  msg_type = getw16(p + 4);
  msg_subtype = getw16(p + 6);
  assert(TABLE_DUMP_V2 == msg_type);
  assert(PEER_INDEX_TABLE <= msg_subtype || RIB_GENERIC >= msg_subtype);
  if (TABLE_DUMP_V2 == msg_type)
    switch (msg_subtype) {
    case PEER_INDEX_TABLE:
      mrt->tabledump.count_PEER_INDEX_TABLE++;
      break;
    case RIB_IPV4_UNICAST:
      process_RIB_IPV4_UNICAST(mrt, p + MIN_MRT_LENGTH, l);
      mrt->tabledump.count_RIB_IPV4_UNICAST++;
      break;
    case RIB_IPV4_MULTICAST:
      mrt->tabledump.count_RIB_IPV4_MULTICAST++;
      break;
    case RIB_IPV6_UNICAST:
      mrt->tabledump.count_RIB_IPV6_UNICAST++;
      break;
    case RIB_IPV6_MULTICAST:
      mrt->tabledump.count_RIB_IPV6_MULTICAST++;
      break;
    case RIB_GENERIC:
      mrt->tabledump.count_RIB_GENERIC++;
      break;
    default:
      printf("subtype exception in mrt_item_process %d/%d\n", msg_type, msg_subtype);
      rval = 0;
    }
  else
    printf("type exception in mrt_item_process %d/%d\n", msg_type, msg_subtype);
  return rval;
};

void mrt_list_walker(struct mrt *mrt, struct chunk buf) {
  assert(TYPE_TABLEDUMP == mrt->type);
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
    if (mrt_item_process(mrt, mrt_buffer_ptr, mrt_rec_length)) {
      mrt_item_count++;
      mrt_buffer_ptr = mrt_buffer_next;
    } else {
      printf("exception processing MRT message %d\n", mrt_item_count);
      exit(1);
    };
  };
  assert(mrt_buffer_ptr == mrt_buffer_limit);
  mrt->mrt_rec_count = mrt_item_count;
  assert(1 == mrt->tabledump.count_PEER_INDEX_TABLE);
};

void parse_mrt_TABLE_DUMP_V2(struct mrt *mrt, struct chunk buf) {
  assert(TYPE_TABLEDUMP == mrt->type);
  uint16_t msg_type, msg_subtype, mrt_peer_count, mrt_view_name_length;
  uint8_t peer_type;
  void *peer_entries, *peer_entries_limit;
  int i;
  int ip_addr_length, as_addr_length;

  // we're not really doing much integrity checking here...
  // assert(MIN_MRT_LENGTH <= buf.length);
  // mrt_rec_length = getw32(buf.data + 8);

  // except to check the table type/subtype, which is pretty fundamental!
  msg_type = getw16(buf.data + 4);
  assert(TABLE_DUMP_V2 == msg_type);
  msg_subtype = getw16(buf.data + 6);
  assert(PEER_INDEX_TABLE == msg_subtype);

  mrt_view_name_length = getw16(buf.data + OFFSET_View_Name_Length);
  mrt_peer_count = getw16(buf.data + OFFSET_View_Name_Length + 2 + mrt_view_name_length);
  mrt->peer_table = calloc(mrt_peer_count, sizeof(struct mrt_peer_record));
  peer_entries = buf.data + OFFSET_View_Name_Length + 4 + mrt_view_name_length;
  peer_entries_limit = peer_entries + buf.length;
  for (i = 0; i < mrt_peer_count; i++) {
    struct mrt_peer_record *peer = &mrt->peer_table[i];
    assert(peer_entries_limit > peer_entries);
    peer_type = *(uint8_t *)peer_entries;
    peer->is_ipv6 = 0x01 & peer_type;
    ip_addr_length = peer->is_ipv6 ? 16 : 4;
    as_addr_length = (0x02 & peer_type) ? 4 : 2;

    if (2 == as_addr_length) // 16 bit peer AS, may still be operating in AS4 sessions??
      peer->peer_as = getw16(peer_entries + 5 + ip_addr_length);
    else
      peer->peer_as = getw32(peer_entries + 5 + ip_addr_length);

    peer->mrt_file_index = i;
    peer->peer_bgpid = __bswap_32(getw32(peer_entries + 1));

    if (peer->is_ipv6) // IPv6 peer may still have IPv4 data!!
      peer->peer_ip6 = *(struct in6_addr *)(peer_entries + 5);
    else
      peer->peer_ip = (struct in_addr){__bswap_32(getw32(peer_entries + 5))};

    // show_mrt_peer_record(peer);
    // printf("\n");
    peer_entries += 5 + ip_addr_length + as_addr_length;
  };
  //printf("%d peers processed, peer_entries %p peer_entries_limit %p [%ld]\n", i, peer_entries, peer_entries_limit, peer_entries_limit - peer_entries);
  // why does this assertion fail???
  // assert(peer_entries_limit == peer_entries);
  mrt->peer_count = mrt_peer_count;
};

struct mrt *get_mrt_tabledump(struct chunk buf) {

  struct mrt *mrt;
  mrt = calloc(1, sizeof(struct mrt));
  mrt->type = TYPE_TABLEDUMP;

  // parse the initial MRT record which must be type TABLE_DUMP_V2 subtype PEER_INDEX_TABLE
  parse_mrt_TABLE_DUMP_V2(mrt, buf);
  printf("get_mrt_tabledump: found %d peers\n", mrt->peer_count);
  mrt_list_walker(mrt, buf);
  printf("get_mrt_tabledump: %d IPV4_UNICAST entries\n", mrt->tabledump.count_RIB_IPV4_UNICAST);
  printf("get_mrt_tabledump: highest active peer index: %d\n", mrt->tabledump.max_peer_index);
  return mrt;
};
