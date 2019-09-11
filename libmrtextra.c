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
      printf("lookup in updates failed for peer %s\n", show_mrt_peer_record(tabledump_peer));
  };
  return matched;
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
      assert(NULL == updatesdump_peer->link || tabledump_peer == updatesdump_peer->link);
      updatesdump_peer->link = tabledump_peer;
      assert(NULL == tabledump_peer->link || updatesdump_peer == tabledump_peer->link);
      tabledump_peer->link = updatesdump_peer;
    } else
      printf("lookup in tabledump failed for peer %s\n", show_mrt_peer_record(updatesdump_peer));
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
