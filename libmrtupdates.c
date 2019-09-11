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

#define INFINITY 0x7ffff000

static inline int compare_bgp4mp_peer(const void *a, const void *b) {
  struct mrt_peer_record *_a = (struct mrt_peer_record *)a;
  struct mrt_peer_record *_b = (struct mrt_peer_record *)b;
  return (_b->bgp4mp.update_count - _a->bgp4mp.update_count);
};

void sort_bgp4mp_peers(struct mrt *sp) {
  qsort(sp->peer_table, sp->peer_count, sizeof(struct mrt_peer_record), compare_bgp4mp_peer);
};

static inline struct chunk update_block_builder(struct update_list_item *update_list) {
  long int length = 0;
  struct update_list_item *p;
  int offset;

  p = update_list;
  while (p != NULL) {
    length += p->msg.length + (ADD_LOCAL_PREF ? 7 : 0);
    p = p->next;
  };
  // NOTE - the assigned length assumes all messages get local pref added - but some may not be eligible e.g. withdraw
  // however, allocating too much memory is nota problem as long as we trim it or something when we finish....
  assert(length < INFINITY);
  void *buffer = malloc(length);
  assert(NULL != buffer);
  p = update_list;
  offset = 0;
  while (p != NULL) {
    struct chunk pre = p->msg;
    if (ADD_LOCAL_PREF)
      p->msg = update_fixup_localpreference(100, p->msg);
    assert(pre.length + 7 == p->msg.length);
    memcpy(buffer + offset, p->msg.data, p->msg.length);
    offset += p->msg.length;
    assert(offset <= length);
    p = p->next;
  };
  return (struct chunk){buffer, offset};
};

void show_bgp4mp_peer_address(struct mrt_peer_record *peer) {
  char peer_ip_str[INET6_ADDRSTRLEN];
  char local_ip_str[INET6_ADDRSTRLEN];

  if (peer->is_ipv6) {
    inet_ntop(AF_INET6, &peer->peer_ip6, peer_ip_str, INET6_ADDRSTRLEN);
    inet_ntop(AF_INET6, &peer->local_ip6, local_ip_str, INET6_ADDRSTRLEN);
  } else {
    inet_ntop(AF_INET, &peer->peer_ip, peer_ip_str, INET6_ADDRSTRLEN);
    inet_ntop(AF_INET, &peer->local_ip, local_ip_str, INET6_ADDRSTRLEN);
  };
  printf("[as %-6d ip %-24s (local: as %-6d ip %-24s)]", peer->peer_as, peer_ip_str, peer->local_as, local_ip_str);
};

void report_bgp4mp_bgp_stats(struct bgp4mp_bgp_stats *sp) {
  printf("report_bgp4mp_bgp_stats: %-7d Opens\n", sp->open_count);
  printf("report_bgp4mp_bgp_stats: %-7d Notifications\n", sp->notification_count);
  printf("report_bgp4mp_bgp_stats: %-7d Keepalives\n", sp->keepalive_count);
  printf("report_bgp4mp_bgp_stats: %-7d Updates\n", sp->all_update_count);

  printf("report_bgp4mp_bgp_stats:   %-7d Plain Updates\n", sp->update_count);
  printf("report_bgp4mp_bgp_stats:   %-7d Withdraws\n", sp->withdraw_count);
  printf("report_bgp4mp_bgp_stats:   %-7d MP-BGP Updates\n", sp->mpbgp_count);
  printf("report_bgp4mp_bgp_stats:   %-7d Mixed Updates\n", sp->mixed_update_count);
  printf("report_bgp4mp_bgp_stats:   %-7d Zero NLRI Update\n", sp->zero_nrli_count);
  printf("report_bgp4mp_bgp_stats:   %-7d End-of-RIB\n", sp->eor_count);
  printf("report_bgp4mp_bgp_stats:     %-7d IBGP count\n", sp->ibgp_count);
  printf("report_bgp4mp_bgp_stats:     %-7d MED count\n", sp->med_count);
};

struct chunk get_one_bgp4mp(struct mrt *mrt, int peer, int msg_number) {
  assert(TYPE_BGP4MP == mrt->type);
  int i;
  struct chunk rval = (struct chunk){NULL, 0};
  sort_bgp4mp_peers(mrt);
  if (peer >= mrt->peer_count) {
    printf("get_one_bgp4mp: *** WARNING *** insufficient peers, cannot continue (%d/%d)\n", peer, mrt->peer_count);
    exit(1);
  } else if (msg_number > mrt->peer_table[peer].bgp4mp.update_count) {
    printf("get_one_bgp4mp: *** WARNING *** insufficient msgs, cannot continue (%d/%d)\n", msg_number, mrt->peer_table[peer].bgp4mp.update_count);
    exit(1);
  } else {

    i = 0;
    struct update_list_item *list = mrt->peer_table[i].bgp4mp.update_list_head;
    while (list != NULL) {
      if (msg_number == i) {
        rval = list->msg;
        break;
      };
      i++;
      list = list->next;
    };
  };
  return rval;
};

// this function (get_blocks_bgp4mp) simple builds a set of transmittable update sequences
// with no direct control over the source peers
//
// a more specific function is required to match update sequences with corresponding peers and peer table dumps
// the mechanism requires the peer ID, i.e. peer_as and peer_ip
struct chunk *get_blocks_bgp4mp(struct mrt *mrt, int nblocks) {
  assert(TYPE_BGP4MP == mrt->type);
  int i;
  struct chunk *blocks = calloc(nblocks + 1, sizeof(struct chunk));
  sort_bgp4mp_peers(mrt);
  if (nblocks > mrt->peer_count)
    printf("get_blocks_bgp4mp: *** WARNING *** insufficient peers, returning some empty blocks\n");
  for (i = 0; i < nblocks && i < mrt->peer_count; i++)
    if (i >= mrt->peer_count) {
      blocks[i] = (struct chunk){NULL, 0}; // redundant due to calloc...
    } else {
      blocks[i] = update_block_builder(mrt->peer_table[i].bgp4mp.update_list_head);
    };
  return blocks;
};

// todo this is an unneeded level of indirection now
// because the match is done already....
struct chunk get_blocks_bgp4mp_peer(struct mrt_peer_record *peer) {
  return update_block_builder(peer->bgp4mp.update_list_head);
};

void report_mrt_bgp4mp(struct mrt *mrt) {
  assert(TYPE_BGP4MP == mrt->type);
  int i;
  printf("report_mrt_bgp4mp: %d MRT items\n", mrt->mrt_rec_count);
  printf("report_mrt_bgp4mp:    %d messages, %d state changes\n", mrt->bgp4mp.mrt_bgp_msg_count, mrt->bgp4mp.state_changes);
  if (0 < mrt->bgp4mp.ipv6_discards)
    printf("report_mrt_bgp4mp:    discarded %d IPv6 items\n", mrt->bgp4mp.ipv6_discards);
  if (0 < mrt->bgp4mp.as2_discards)
    printf("report_mrt_bgp4mp:    discarded %d AS2 items\n", mrt->bgp4mp.as2_discards);
  sort_bgp4mp_peers(mrt);
  printf("report_mrt_bgp4mp: displaying top 5 peers\n");
  printf("report_mrt_bgp4mp: peer index                                                                                records  msgs     filtered updates\n");
  for (i = 0; i < 5 && i < mrt->peer_count; i++) {
    struct mrt_peer_record *peer = &mrt->peer_table[i];
    printf("report_mrt_bgp4mp: %-3d", i);
    show_bgp4mp_peer_address(peer);
    printf(" %-7d  %-7d  %-7d\n", peer->bgp4mp.rec_count, peer->bgp4mp.bgp_msg_count, peer->bgp4mp.update_count);
  };
};

static inline void process_path_attribute(uint8_t type_code, struct chunk msg, struct bgp4mp_bgp_stats *sp) {
  if (MULTI_EXIT_DISC == type_code) {
    sp->med_count++;
  } else if (LOCAL_PREF == type_code) {
    sp->ibgp_count++;
  };
};

static inline void process_path_attributes(struct chunk msg, struct bgp4mp_bgp_stats *sp) {
  void *p = msg.data;
  void *limit = msg.data + msg.length;
  uint8_t flags, type_code;
  uint16_t length;
  while (p < limit) {
    flags = *(uint8_t *)p++;
    type_code = *(uint8_t *)p++;
    length = *(uint8_t *)p++;
    if (0x10 & flags)
      length = length << 8 | (*(uint8_t *)p++);
    process_path_attribute(type_code, (struct chunk){p, length}, sp);
    p += length;
  };
  if (p != limit) {
    printf("process_path_attributes exception %p %p %p %ld %ld\n", msg.data, limit, p, limit - p, msg.length);
  };
  assert(p == limit);
};

static int zero_nrli_flag = 1;
static inline int process_bgp_message(struct chunk msg, struct bgp4mp_bgp_stats *sp) {
  assert(18 < msg.length);
  uint16_t length = getw16(msg.data + 16);
  uint8_t typecode = *(uint8_t *)(msg.data + 18);
  int is_update = 0;
  sp->msg_count++;
  assert(length == msg.length);
  switch (typecode) {
  case 1:
    sp->open_count++;
    break;
  case 2: { // Update cases - EOR/Withdraw/Update
    sp->all_update_count++;
    uint16_t withdraw_length = getw16(msg.data + 19);
    uint16_t pathattributes_length = getw16(msg.data + 21 + withdraw_length);
    uint16_t nlri_length = length - withdraw_length - pathattributes_length - 23;
    assert(length >= 23 + withdraw_length + pathattributes_length); // sanity check

    if (23 == length) // 0 == pathattributes_length == withdraw_length ~ empty Update aka End-of-RIB
      sp->eor_count++;
    else if (0 == pathattributes_length) {
      assert(length == 23 + withdraw_length); // i.e. other than EOR the only legal Update with no path attributes is a plain withdraw
      sp->withdraw_count++;                   // simple withdraw
    } else {                                  // non-empty Path Attribute options now
      process_path_attributes((struct chunk){msg.data + 23 + withdraw_length, pathattributes_length}, sp);
      if (0 == withdraw_length) {
        if (0 == nlri_length) // MP-BGP  -  NLRI only and always present if this is a plain IPv4 messsage (not MP-BGP which has no NLRI but instead attribute MP_REACH_NLRI)
          sp->mpbgp_count++;
        else { // simple update
          sp->update_count++;
          is_update = 1;
        }
      } else { // withdraw_length non-zero
        if (0 == nlri_length && zero_nrli_flag) {
          printf("Update with path attributes and no NLRI at msg# %d\n", sp->msg_count);
          // write_chunk("debug.bin",msg);
          // print_chunk(msg);
          // print_chunk((struct chunk){msg.data + 23 + withdraw_length, pathattributes_length});
          zero_nrli_flag = 0;
          sp->zero_nrli_count++;
        };
        // assert(0 < nlri_length); // shouldn't have path attributes and withdraw with no NLRI!
        // but they are out there!!!
        // combined withdraw and update
        sp->mixed_update_count++;
      };
    };
    break;
  };
  case 3:
    sp->notification_count++;
    break;
  case 4:
    sp->keepalive_count++;
    break;
  default:
    printf("invalid typecode %d\n", typecode);
    exit(1);
  };
  return is_update;
};

void initialise_bgp4mp_peer(struct mrt_peer_record *peer) {
  void *header = peer->bgp4mp.peer_header;
  uint16_t address_family = getw16(header + 10);
  assert(0x0001 == address_family || 0x0002 == address_family);
  peer->is_ipv6 = 0x0002 == address_family;
  peer->peer_as = getw32(header + 0);
  peer->local_as = getw32(header + 4);
  if (peer->is_ipv6) {
    peer->peer_ip6 = *(struct in6_addr *)(header + 12);
    peer->local_ip6 = *(struct in6_addr *)(header + 20);
  } else {
    peer->peer_ip = (struct in_addr){__bswap_32(getw32(header + 12))};
    peer->local_ip = (struct in_addr){__bswap_32(getw32(header + 16))};
  };
};

struct mrt *mrt_updates_parse(struct chunk buf) {
  struct mrt *mrt;
  uint16_t msg_type, msg_subtype;
  uint32_t msg_length, msg_timestamp;
  int found, pn;
  int ET_extension, BGP4MP_header_length, min_mrt_length;
  int is_AS4 = 0;
  int64_t bytes_left = buf.length;
  void *ptr = buf.data;

  mrt = calloc(1, sizeof(*mrt));
  mrt->type = TYPE_BGP4MP;
  while (bytes_left >= MIN_MRT_LENGTH) {
    msg_timestamp = getw32(ptr + 0);
    msg_type = getw16(ptr + 4);
    msg_subtype = getw16(ptr + 6);
    msg_length = getw32(ptr + 8);
    assert(bytes_left >= MIN_MRT_LENGTH + msg_length);
    mrt->mrt_rec_count++;

    if (msg_type == BGP4MP) {
      min_mrt_length = MIN_MRT_LENGTH;
      ET_extension = 0;
    } else if (msg_type == BGP4MP_ET) {
      min_mrt_length = MIN_MRT_LENGTH_ET;
      ET_extension = 4;
    } else {
      printf("mrt_parse: wrong msg_type %d/%d msg# %d\n", msg_type, msg_subtype, mrt->mrt_rec_count);
      exit(1);
    };
    switch (msg_subtype) {
    case BGP4MP_STATE_CHANGE:
    case BGP4MP_MESSAGE:
    case BGP4MP_MESSAGE_LOCAL:
      is_AS4 = 0;
      if (0 == mrt->bgp4mp.as2_discards)
        printf("mrt_parse: unsupported AS2 msg_subtype %d at msg %d\n", msg_subtype, mrt->mrt_rec_count);
      mrt->bgp4mp.as2_discards++;
      break;
    case BGP4MP_MESSAGE_AS4:
    case BGP4MP_STATE_CHANGE_AS4:
    case BGP4MP_MESSAGE_AS4_LOCAL:
      is_AS4 = 1;
      break;
    default:
      printf("mrt_parse: bad msg_sub_type %d/%d\n", msg_type, msg_subtype);
      exit(1);
    };

    if (is_AS4) {
      // BGP4MP -> the AFI is at a fixed offset in all subtypes
      uint16_t afi = getw16(ptr + min_mrt_length + 10);
      BGP4MP_header_length = (1 == afi) ? BGP4MP_IPV4_PEER_HEADER_LENGTH : BGP4MP_IPV6_PEER_HEADER_LENGTH;
      if ((1 == afi) || (2 == afi)) {
      } else
        printf("mrt_parse: AFI wrong %d at mrt_count %d %d/%d\n", afi, mrt->mrt_rec_count, msg_type, msg_subtype);
      assert((1 == afi) || (2 == afi));
      // lookup the bgp4mp common header in the raw peer table
      found = 0;
      void *raw_bgp4mp_header = ptr + min_mrt_length;
      for (pn = 0; pn < mrt->peer_count; pn++)
        if (0 == memcmp(raw_bgp4mp_header, &mrt->peer_table[pn].bgp4mp.peer_header, BGP4MP_header_length)) {
          found = 1;
          break;
        };
      // on exit either found==0 or found==1 and pn is the index in the currrent
      // table if found==0 we should make a new entry in the raw table by
      // (re-)allocating space and copying the bgp4mp common header to it
      //
      // create a new raw peer entry if either
      //   - this is the first entry
      //   - the current peer is not found in the existing table

      if (0 == found) {
        mrt->peer_count++;
        assert(1 == mrt->peer_count - pn);
        mrt->peer_table = realloc(mrt->peer_table, mrt->peer_count * sizeof(struct mrt_peer_record));
        struct mrt_peer_record *peer = &mrt->peer_table[pn];
        memset(peer, 0, sizeof(struct mrt_peer_record));
        peer->mrt_file_index = pn;
        memcpy(&peer->bgp4mp.peer_header, raw_bgp4mp_header, BGP4MP_header_length);
        initialise_bgp4mp_peer(peer);
      };
      // pp is a convenience pointer for the current peer record
      struct mrt_peer_record *pp = &mrt->peer_table[pn];

      pp->bgp4mp.rec_count++;
      if (msg_subtype == BGP4MP_MESSAGE_AS4) {
        // capture the BGP4MP message payload)
        struct chunk msg_chunk = (struct chunk){ptr + min_mrt_length + BGP4MP_header_length, msg_length - BGP4MP_header_length - ET_extension};
        pp->bgp4mp.bgp_msg_count++;
        mrt->bgp4mp.mrt_bgp_msg_count++;

        struct update_list_item *itemg = calloc(1, sizeof(struct update_list_item));
        itemg->msg = msg_chunk;

        if (NULL == mrt->bgp4mp.update_list_head)
          mrt->bgp4mp.update_list_head = itemg;
        else
          mrt->bgp4mp.update_list_tail->next = itemg;
        mrt->bgp4mp.update_list_length++;
        mrt->bgp4mp.update_list_tail = itemg;

        // now create a separate shadow list for each peer
        // using same chunk but new update_list_item

        int bgp_msg_status = process_bgp_message(msg_chunk, &pp->bgp4mp.bgp_stats);
        if (bgp_msg_status) { // we only want update messages in the list, so skip if not Update
          struct update_list_item *itemp = calloc(1, sizeof(struct update_list_item));
          itemp->msg = msg_chunk;
          if (NULL == pp->bgp4mp.update_list_head)
            pp->bgp4mp.update_list_head = itemp;
          else
            pp->bgp4mp.update_list_tail->next = itemp;
          pp->bgp4mp.update_list_tail = itemp;
          pp->bgp4mp.update_count++;
        };
      } else if (msg_subtype == BGP4MP_STATE_CHANGE_AS4) {
        mrt->bgp4mp.state_changes++;
        // uint16_t old_state = getw16(ptr + min_mrt_length + BGP4MP_header_length);
        // uint16_t new_state = getw16(ptr + min_mrt_length + BGP4MP_header_length + 2);
        // show_bgp4mp_peer_header(ptr + min_mrt_length);
        // printf("mrt_parse: state change %d -> %d at %d\n", old_state, new_state, sp->mrt_count);
      } else {
        printf("mrt_parse: wrong msg_subtype %d at msg %d\n", msg_subtype, mrt->mrt_rec_count);
      };
    };
    // note - even for _ET extended type messages the message length calculation uses the old
    // method, i.e. the timestamp extension is effectively part of the payload for _ET types
    ptr += MIN_MRT_LENGTH + msg_length;
    bytes_left -= MIN_MRT_LENGTH + msg_length;
  };
  return mrt;
};

int count_update_list(struct update_list_item *list) {
  int i = 0;

  // simple list walk without list modification
  while (list != NULL) {
    i++;
    list = list->next;
  };
  return i;
};

struct update_list_item *filter_msgs(struct update_list_item *list, struct bgp4mp_bgp_stats *sp) {
  // filter list of non-compliant chunks based on process_bgp_message return
  // value builds a new list with a dummy head, which is discarded on exit
  struct update_list_item head = {NULL, (struct chunk){NULL, 0}};
  struct update_list_item *last = &head;
  struct update_list_item *next;
  memset(sp, 0, sizeof(*sp));
  while (list != NULL) {
    next = list->next;
    if (process_bgp_message(list->msg, sp)) {
      last->next = list;
      last = list;
    } else {
      // the underlying chunk is not freed
      // as it is part of the contiguous original file buffer!!!
      free(list);
    };
    list = next;
  };
  return head.next;
};
