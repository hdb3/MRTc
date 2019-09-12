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

/* functions over pure table dump structures
 *
 * Providing utility, sorting, filtering, reporting and output
 *
 */

// there should be a simple cast over the function signature which
// would alias the natural function, but I could not make it compile ;-(
// int compare_mrt_peer_record(struct mrt_peer_record *a, struct mrt_peer_record *b) {
//

/* Utility funcions
 *
 * sort
 *
 */

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

/*
 * reporting and analysis
 *
 */

void report_mrt_tabledump(struct mrt *mrt) {
  int i, max_entry_count, min_entry_count, non_zero_entry_counts;
  struct mrt_tabledump_peer_record *rib = NULL;
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

  min_entry_count = mrt->peer_table[0].rib.count;
  max_entry_count = 0;
  non_zero_entry_counts = 0;
  for (i = 0; i < mrt->peer_count; i++) {
    rib = &mrt->peer_table[i].rib;
    if (0 < rib->count) {
      non_zero_entry_counts++;
    };
    min_entry_count = min_entry_count > rib->count ? rib->count : min_entry_count;
    max_entry_count = max_entry_count < rib->count ? rib->count : max_entry_count;
  };
  printf("report_mrt_tabledump: largest table size: %d\n", max_entry_count);
  printf("report_mrt_tabledump: smallest table size: %d\n", min_entry_count);
  printf("report_mrt_tabledump: non empty tables: %d\n", non_zero_entry_counts);
};

void report_mrt_tabledump_peers(struct mrt *mrt) {
  assert(TYPE_TABLEDUMP == mrt->type);
  int i;
  printf("report_tabledump_peer: peer     Remote                   Remote   Remote           RIB\n");
  printf("report_tabledump_peer: index    IP                       ASN      BGPID            size\n");
  for (i = 0; i < mrt->peer_count; i++) {
    struct mrt_peer_record *peer = &mrt->peer_table[i];
    printf("report_tabledump_peer: %-3d %s %-8d\n", i, show_mrt_peer_record(peer), peer->rib.count);
  };
};

/*
 * output
 *
 */

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

void build_mrt_tabledump_updates(struct mrt *tabledump) {
  int i;
  assert(TYPE_TABLEDUMP == tabledump->type);
  for (i = 0; i < tabledump->peer_count; i++) {
    struct mrt_peer_record *peer = &tabledump->peer_table[i];
    build_tabledump_updates(peer);
  };
  printf("build_mrt_tabledump_tabledump_updates: built %d tabledump update streams\e[K\n", i);
};

/*
 * filtering
 *
 */

int trim_mrt_tabledump_size(struct mrt *tabledump, int requested_table_size) {
  int i;
  int retained = 0;
  assert(TYPE_TABLEDUMP == tabledump->type);
  sort_peer_table(tabledump);
  for (i = 0; i < tabledump->peer_count; i++) {
    struct mrt_peer_record *peer = &tabledump->peer_table[i];
    if (requested_table_size <= peer->rib.count) {
      if (i != retained)
        tabledump->peer_table[retained] = *peer;
      retained++;
    } else {
      assert(NULL == peer->tabledump_updates.data);
      assert(0 == peer->tabledump_updates.length);
      free(peer->rib.table);
    };
  };
  int removed = tabledump->peer_count - retained;
  tabledump->peer_count = retained;
  tabledump->peer_table = realloc(tabledump->peer_table, retained * sizeof(struct mrt_peer_record));
  return removed;
};

int trim_mrt_tabledump_unlinked(struct mrt *tabledump) {
  int i;
  int retained = 0;
  assert(TYPE_TABLEDUMP == tabledump->type);
  sort_peer_table(tabledump);
  for (i = 0; i < tabledump->peer_count; i++) {
    struct mrt_peer_record *peer = &tabledump->peer_table[i];
    if (peer->link) {
      if (i != retained)
        tabledump->peer_table[retained] = *peer;
      retained++;
    } else {
      assert(NULL == peer->tabledump_updates.data);
      assert(0 == peer->tabledump_updates.length);
      free(peer->rib.table);
    };
  };
  int removed = tabledump->peer_count - retained;
  tabledump->peer_count = retained;
  tabledump->peer_table = realloc(tabledump->peer_table, retained * sizeof(struct mrt_peer_record));
  return removed;
};
