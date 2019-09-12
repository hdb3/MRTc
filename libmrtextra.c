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

void add_bgp4mp_bgp_stats(struct bgp4mp_bgp_stats *to, struct bgp4mp_bgp_stats *from) {
  to->msg_count += from->msg_count;
  to->open_count += from->open_count;
  to->all_update_count += from->all_update_count;
  to->notification_count += from->notification_count;
  to->keepalive_count += from->keepalive_count;

  to->update_count += from->update_count;
  to->eor_count += from->eor_count;
  to->withdraw_count += from->withdraw_count;
  to->mixed_update_count += from->mixed_update_count;
  to->mpbgp_count += from->mpbgp_count;
  to->zero_nrli_count += from->zero_nrli_count;

  to->med_count += from->med_count;
  to->ibgp_count += from->ibgp_count;
};

static inline int compare_bgp4mp_peer(const void *a, const void *b) {
  struct mrt_peer_record *_a = (struct mrt_peer_record *)a;
  struct mrt_peer_record *_b = (struct mrt_peer_record *)b;
  return (_b->bgp4mp.update_count - _a->bgp4mp.update_count);
};

void sort_bgp4mp_peers(struct mrt *mrt) {
  assert(TYPE_BGP4MP == mrt->type);
  qsort(mrt->peer_table, mrt->peer_count, sizeof(struct mrt_peer_record), compare_bgp4mp_peer);
};

int filter_updates_unlinked(struct mrt *mrt) {
  int i, retained;
  struct mrt_peer_record *peer;
  assert(TYPE_BGP4MP == mrt->type);
  retained = 0;
  for (i = 0; i < mrt->peer_count; i++) {
    peer = &mrt->peer_table[i];
    if (peer->link) {
      if (i != retained) // neccessary because list is not sorted on this property
        mrt->peer_table[retained] = *peer;
      retained++;
    } else {
      free_update_list(peer->bgp4mp.update_list_head);
      peer->bgp4mp.update_list_head = NULL;
      peer->bgp4mp.update_list_tail = NULL;
    };
  };
  int removed = mrt->peer_count - retained;
  mrt->peer_count = retained;
  mrt->peer_table = realloc(mrt->peer_table, retained * sizeof(struct mrt_peer_record));
  return removed;
};

int filter_updates_on_size(struct mrt *mrt, int min_size) {
  int i, remaining;
  struct mrt_peer_record *peer;
  assert(TYPE_BGP4MP == mrt->type);
  sort_bgp4mp_peers(mrt);
  remaining = 0;
  for (i = 0; i < mrt->peer_count; i++) {
    peer = &mrt->peer_table[i];
    // printf("filter_updates_on_size: (%d) %d ", i, peer->bgp4mp.update_count);
    if (min_size > peer->bgp4mp.update_count) {
      // printf("NO\n");
      free_update_list(peer->bgp4mp.update_list_head);
      peer->bgp4mp.update_list_head = NULL;
      peer->bgp4mp.update_list_tail = NULL;
    } else {
      assert(remaining == i); // should be true if the list was sorted
      remaining++;
      // printf("YES\n");
    };
  };
  int removed = mrt->peer_count - remaining;
  mrt->peer_count = remaining;
  mrt->peer_table = realloc(mrt->peer_table, remaining * sizeof(struct mrt_peer_record));
  return removed;
};

struct mrt_peer_record *lookup_mrt_peer(struct mrt *mrt, struct mrt_peer_record *key) {
  assert(TYPE_BGP4MP == mrt->type || TYPE_TABLEDUMP == mrt->type);
  int i;
  struct mrt_peer_record *peer;
  for (i = 0; i < mrt->peer_count; i++) {
    peer = &mrt->peer_table[i];
    if (key->peer_as == peer->peer_as && peer_addr_compare(peer, key))
      return peer;
  };
  return NULL;
};

int match_tabledump_bgp4mp(struct mrt *tabledump, struct mrt *updatesdump) {
  assert(TYPE_BGP4MP == updatesdump->type);
  assert(TYPE_TABLEDUMP == tabledump->type);
  int i;
  int matched = 0;
  struct mrt_peer_record *tabledump_peer, *updatesdump_peer;
  for (i = 0; i < tabledump->peer_count; i++) {
    tabledump_peer = &tabledump->peer_table[i];
    if (updatesdump_peer = lookup_mrt_peer(updatesdump, tabledump_peer)) {
      matched++;
      assert(NULL == tabledump_peer->link || updatesdump_peer == tabledump_peer->link);
      tabledump_peer->link = updatesdump_peer;
      assert(NULL == updatesdump_peer->link || tabledump_peer == updatesdump_peer->link);
      updatesdump_peer->link = tabledump_peer;
    } else
      // no need to report - mrt_summary will show if detail wanted
      ; // printf("lookup in updates failed for peer %s\n", show_mrt_peer_record(tabledump_peer));
  };
  return matched;
};

void mrt_clear_links(struct mrt *updatesdump, struct mrt *tabledump) {
  assert(TYPE_BGP4MP == updatesdump->type);
  assert(TYPE_TABLEDUMP == tabledump->type);
  int i;

  for (i = 0; i < updatesdump->peer_count; i++)
    updatesdump->peer_table[i].link = NULL;

  for (i = 0; i < tabledump->peer_count; i++)
    tabledump->peer_table[i].link = NULL;
};

int match_bgp4mp_tabledump(struct mrt *updatesdump, struct mrt *tabledump) {
  assert(TYPE_BGP4MP == updatesdump->type);
  assert(TYPE_TABLEDUMP == tabledump->type);
  int i;
  int matched = 0;
  struct mrt_peer_record *tabledump_peer, *updatesdump_peer;
  for (i = 0; i < updatesdump->peer_count; i++) {
    updatesdump_peer = &updatesdump->peer_table[i];
    if (tabledump_peer = lookup_mrt_peer(tabledump, updatesdump_peer)) {
      matched++;
      // assertions only valid on first pass, probably should explicitly clear them
      // after table rearrangement
      assert(NULL == updatesdump_peer->link || tabledump_peer == updatesdump_peer->link);
      updatesdump_peer->link = tabledump_peer;
      assert(NULL == tabledump_peer->link || updatesdump_peer == tabledump_peer->link);
      tabledump_peer->link = updatesdump_peer;
    } else
      // no need to report - mrt_summary will show if detail wanted
      ; // printf("lookup in tabledump failed for peer %s\n", show_mrt_peer_record(updatesdump_peer));
  };
  return matched;
};

void mrt_summary(struct mrt *mrt) {
  int i;
  printf("mrt_summary: type=%s #peers %d #MRT records %d\n", mrt->type == TYPE_TABLEDUMP ? "TABLE DUMP" : "UPDATES   ", mrt->peer_count, mrt->mrt_rec_count);
  for (i = 0; i < mrt->peer_count; i++) {
    struct mrt_peer_record *peer = &mrt->peer_table[i];
    print_mrt_peer_record(peer);
    if (NULL == peer->link)
      printf(" unlinked\n");
    else {
      if (peer->link->link != peer) {
        printf("** assymetric link! **");
        if (NULL == peer->link->link)
          printf("(NULL)");
        else
          printf("(%d)", peer->link->link->mrt_file_index);
      };
      printf(" link->%s\n", show_mrt_peer_record(peer->link));
    };
  };
};

void mrt_link_summary(struct mrt *mrt) {
  int i;
  printf("mrt_link_summary:                                                                                        RIB size  updates\n");
  for (i = 0; i < mrt->peer_count; i++) {
    struct mrt_peer_record *peer = &mrt->peer_table[i];
    if (NULL != peer->link && peer == peer->link->link) {
      struct mrt_peer_record *updatepeer, *tabledumppeer;
      if (mrt->type == TYPE_TABLEDUMP) {
        tabledumppeer = peer;
        updatepeer = tabledumppeer->link;
      } else {
        updatepeer = peer;
        tabledumppeer = updatepeer->link;
      };
      printf("mrt_link_summary: %s %-7d   %-9d\n", show_bgp4mp_peer_address(updatepeer), tabledumppeer->rib.count, updatepeer->bgp4mp.update_count);
    } else {
      if (NULL == peer->link)
        printf("mrt_link_summary: %s unlinked\n", show_mrt_peer_record(peer));
      else if (NULL == peer->link->link)
        printf("mrt_link_summary: %s reverse unlinked (link: %s)\n", show_mrt_peer_record(peer), show_mrt_peer_record(peer->link));
      else if (peer->link->link != peer)
        printf("mrt_link_summary: %s ** assymetric link! ** (link: %s)\n", show_mrt_peer_record(peer), show_mrt_peer_record(peer->link));
      else
        printf("mrt_link_summary: True == False\n");
    };
  };
};
