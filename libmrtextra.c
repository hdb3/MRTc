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

struct mrt_peer_record *lookup_update_peer(struct mrt *mrt, uint32_t as, struct in_addr ip) {
  assert(TYPE_TABLEDUMP == mrt->type);
  int i;
  struct mrt_peer_record *peer;
  for (i = 0; i < mrt->peer_count; i++) {
    peer = &mrt->peer_table[i];
    if (as == peer->peer_as && ip.s_addr == peer->peer_ip.s_addr)
      return peer;
  };
  return NULL;
};

struct mrt_peer_record *lookup_mrt_peer(struct mrt *mrt, uint32_t as, struct in_addr ip) {
  assert(TYPE_BGP4MP == mrt->type);
  int i;
  struct mrt_peer_record *peer;
  for (i = 0; i < mrt->peer_count; i++) {
    peer = &mrt->peer_table[i];
    if (as == peer->peer_as && ip.s_addr == peer->peer_ip.s_addr)
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
    if (updatesdump_peer = lookup_mrt_peer(updatesdump, tabledump_peer->peer_as, tabledump_peer->peer_ip)) {
      matched++;
      tabledump_peer->link = updatesdump_peer;
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
    if (tabledump_peer = lookup_update_peer(tabledump, updatesdump_peer->peer_as, updatesdump_peer->peer_ip)) {
      matched++;
      updatesdump_peer->link = tabledump_peer;
      tabledump_peer->link = updatesdump_peer;
    } else
      printf("lookup in tabledump failed for peer %s\n", show_mrt_peer_record(updatesdump_peer));
  };
  return matched;
};
