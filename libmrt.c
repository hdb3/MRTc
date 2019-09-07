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

struct chunk block_builder(struct msg_list_item *msg_list) {
  long int length = 0;
  struct msg_list_item *p;
  int offset;

  p = msg_list;
  while (p != NULL) {
    length += p->msg.length;
    p = p->next;
  };
  assert(length < INFINITY);
  void *buffer = malloc(length);
  assert(NULL != buffer);
  p = msg_list;
  offset = 0;
  while (p != NULL) {
    memcpy(buffer + offset, p->msg.data, p->msg.length);
    offset += p->msg.length;
    p = p->next;
  };
  return (struct chunk){buffer, offset};
};

void write_chunk(const char *fname, struct chunk buf) {
  int fd = creat(fname, 00664);
  int tmp = write(fd, buf.data, buf.length);
};

void print_chunk(struct chunk ch) {
  int j;
  uint8_t *p = ch.data;
  printf("[");
  for (j = 0; j < ch.length; j++)
    printf(" %02x", *p++);
  printf("]\n");
};

void unmap_mrt_file(struct chunk ch) {
  munmap(ch.data, ch.length);
};

struct chunk map_mrt_file(char *fname) {
  void *buf = NULL;
  struct stat sb;
  int fd = open(fname, O_RDONLY);
  if (-1 == fd) {
    perror("file open error");
    exit(1);
  };

  fstat(fd, &sb);
  buf = mmap(NULL, sb.st_size, PROT_READ, MAP_PRIVATE, fd, 0);
  close(fd);
  printf("opened %s file size %ld\n", fname, sb.st_size);
  return (struct chunk){buf, sb.st_size};
};

void show_bgp4mp_peer_header(void *p) {
  char peer_ip_str[INET6_ADDRSTRLEN];
  char local_ip_str[INET6_ADDRSTRLEN];
  uint32_t peer_as = getw32(p + 0);
  uint32_t local_as = getw32(p + 4);
  uint16_t if_index = getw16(p + 8);
  uint16_t afi = getw16(p + 10);

  if (2 == afi) { // ipv6
    inet_ntop(AF_INET6, p + 12, peer_ip_str, INET6_ADDRSTRLEN);
    inet_ntop(AF_INET6, p + 28, local_ip_str, INET6_ADDRSTRLEN);
  } else { // ipv4
    inet_ntop(AF_INET, p + 12, peer_ip_str, INET6_ADDRSTRLEN);
    inet_ntop(AF_INET, p + 16, local_ip_str, INET6_ADDRSTRLEN);
  };
  printf("[ peer_as %-6d local_as %-6d peer_ip %-24s local_ip %-24s]", peer_as, local_as, peer_ip_str, local_ip_str);
};

void show_bgp4mp_peer(struct bgp4mp_peer *peer) {
  int msg_count = count_msg_list(peer->msg_list_head);
  assert(msg_count == peer->bgp_update_count);

  printf("peer #%-3d ", peer->mrt_file_index);

  show_bgp4mp_peer_header(peer->peer_header);
  printf(" MRT records: %-7d  BGP msgs: %-7d  filtered Updates: %-7d\n",
         peer->mrt_msg_count, peer->mrt_bgp_msg_count, peer->bgp_update_count);
};

void report_stats_bgp4mp_bgp(struct stats_bgp4mp_bgp *sp) {
  printf("\nBGP Message Statistics\n\n");
  printf("Opens %d\n", sp->open_count);
  printf("Notifications %d\n", sp->notification_count);
  printf("Keepalives %d\n", sp->keepalive_count);
  printf("Updates %d\n", sp->all_update_count);

  printf("  Plain Updates %d\n", sp->update_count);
  printf("  Withdraws %d\n", sp->withdraw_count);
  printf("  MP-BGP Updates %d\n", sp->mpbgp_count);
  printf("  Mixed Updates %d\n", sp->mixed_update_count);
  printf("  End-of-RIB %d\n", sp->eor_count);
};

struct chunk get_one_bgp4mp(struct stats_bgp4mp_mrt *sp, int peer, int msg_number) {
  int i;
  struct chunk rval = (struct chunk){NULL, 0};
  sort_bgp4mp_peers(sp);
  if (peer >= sp->peer_count) {
    printf("get_one_bgp4mp: *** WARNING *** insufficient peers, cannot continue (%d/%d)\n", peer, sp->peer_count);
    exit(1);
  } else if (msg_number > sp->peers[peer].bgp_update_count) {
    printf("get_one_bgp4mp: *** WARNING *** insufficient msgs, cannot continue (%d/%d)\n", msg_number, sp->peers[peer].bgp_update_count);
    exit(1);
  } else {

    struct msg_list_item *list = sp->peers[i].msg_list_head;
    i = 0;
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

struct chunk *get_blocks_bgp4mp(struct stats_bgp4mp_mrt *sp, int nblocks) {
  int i;
  struct chunk *blocks = calloc(nblocks + 1, sizeof(struct chunk));
  sort_bgp4mp_peers(sp);
  if (nblocks > sp->peer_count)
    printf("get_blocks_bgp4mp: *** WARNING *** insufficient peers, returning some empty blocks\n");
  for (i = 0; i < nblocks && i < sp->peer_count; i++)
    if (i >= sp->peer_count) {
      blocks[i] = (struct chunk){NULL, 0}; // redundant due to calloc...
    } else {
      blocks[i] = block_builder(sp->peers[i].msg_list_head);
    };
  return blocks;
};

void report_stats_bgp4mp_mrt(struct stats_bgp4mp_mrt *sp) {
  int i;
  printf("\nMRT Record Statistics\n\n");
  printf("got %d MRT items\n", sp->mrt_msg_count);
  printf("    %d messages, %d state changes\n", sp->mrt_bgp_msg_count, sp->state_changes);
  if (0 < sp->ipv6_discards)
    printf("    discarded %d IPv6 items\n", sp->ipv6_discards);
  if (0 < sp->as2_discards)
    printf("    discarded %d AS2 items\n", sp->as2_discards);
  printf("sorting peers based on update counts\n");
  sort_bgp4mp_peers(sp);
  printf("displaying top 10 peers\n");
  for (i = 0; i < 10 && i < sp->peer_count; i++)
    show_bgp4mp_peer(&sp->peers[i]);
};

static inline int process_bgp_message(struct chunk msg, struct stats_bgp4mp_bgp *sp) {
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
      if (0 == withdraw_length) {
        if (0 == nlri_length) // MP-BGP  -  NLRI only and always present if this is a plain IPv4 messsage (not MP-BGP which has no NLRI but instead attribute MP_REACH_NLRI)
          sp->mpbgp_count++;
        else { // simple update
          sp->update_count++;
          is_update = 1;
        }
      } else {                   // withdraw_length non-zero
        assert(0 < nlri_length); // can't have path attributes and withdraw with no NLRI!
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

void mrt_parse(struct chunk buf, struct stats_bgp4mp_mrt *sp) {
  struct chunk bgp_msg;
  uint16_t msg_type, msg_subtype;
  uint32_t msg_length, msg_timestamp;
  int peers = 0;
  int found, pn;
  int ET_extension, BGP4MP_header_length, min_mrt_length;
  int is_AS4 = 0;
  int bytes_left = buf.length;
  void *ptr = buf.data;

  memset(sp, 0, sizeof(*sp));
  while (bytes_left >= MIN_MRT_LENGTH) {
    msg_timestamp = getw32(ptr + 0);
    msg_type = getw16(ptr + 4);
    msg_subtype = getw16(ptr + 6);
    msg_length = getw32(ptr + 8);
    assert(bytes_left >= MIN_MRT_LENGTH + msg_length);
    sp->mrt_msg_count++;

    if (msg_type == BGP4MP) {
      min_mrt_length = MIN_MRT_LENGTH;
      ET_extension = 0;
    } else if (msg_type == BGP4MP_ET) {
      min_mrt_length = MIN_MRT_LENGTH_ET;
      ET_extension = 4;
    } else {
      printf("wrong msg_type %d/%d msg# %d\n", msg_type, msg_subtype, sp->mrt_msg_count);
      exit(1);
    };
    switch (msg_subtype) {
    case BGP4MP_STATE_CHANGE:
    case BGP4MP_MESSAGE:
    case BGP4MP_MESSAGE_LOCAL:
      is_AS4 = 0;
      if (0 == sp->as2_discards)
        printf("first unsupported AS2 msg_subtype %d at msg %d\n", msg_subtype, sp->mrt_msg_count);
      sp->as2_discards++;
      break;
    case BGP4MP_MESSAGE_AS4:
    case BGP4MP_STATE_CHANGE_AS4:
    case BGP4MP_MESSAGE_AS4_LOCAL:
      is_AS4 = 1;
      break;
    default:
      printf("bad msg_sub_type %d/%d\n", msg_type, msg_subtype);
      exit(1);
    };

    if (is_AS4) {
      // BGP4MP -> the AFI is at a fixed offset in all subtypes
      uint16_t afi = getw16(ptr + min_mrt_length + 10);
      BGP4MP_header_length = (1 == afi) ? BGP4MP_IPV4_PEER_HEADER_LENGTH : BGP4MP_IPV6_PEER_HEADER_LENGTH;
      if ((1 == afi) || (2 == afi)) {
      } else
        printf("AFI wrong %d at mrt_count %d %d/%d\n", afi, sp->mrt_msg_count, msg_type, msg_subtype);
      assert((1 == afi) || (2 == afi));
      // lookup the bgp4mp common header in the raw peer table
      found = 0;
      for (pn = 0; pn < peers; pn++)
        if (0 == memcmp(ptr + min_mrt_length, &sp->peers[pn].peer_header, BGP4MP_header_length)) {
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
        peers++;
        pn = peers - 1;
        sp->peers = realloc(sp->peers, peers * sizeof(struct bgp4mp_peer));
        memset(&sp->peers[pn], 0, sizeof(struct bgp4mp_peer));
        sp->peers[pn].mrt_file_index = pn;
        memcpy(&sp->peers[pn].peer_header, ptr + min_mrt_length, BGP4MP_header_length);
      };
      // pp is a convenience pointer for the current peer record
      struct bgp4mp_peer *pp = &sp->peers[pn];

      pp->mrt_msg_count++;
      if (msg_subtype == BGP4MP_MESSAGE_AS4) {
        // capture the BGP4MP message payload)
        struct chunk msg_chunk = (struct chunk){ptr + min_mrt_length + BGP4MP_header_length, msg_length - BGP4MP_header_length - ET_extension};
        pp->mrt_bgp_msg_count++;
        sp->mrt_bgp_msg_count++;

        struct msg_list_item *itemg = calloc(1, sizeof(struct msg_list_item));
        itemg->msg = msg_chunk;

        if (NULL == sp->msg_list_head)
          sp->msg_list_head = itemg;
        else
          sp->msg_list_tail->next = itemg;
        sp->msg_list_length++;
        sp->msg_list_tail = itemg;

        // now create a separate shadow list for each peer
        // using same chunk but new msg_list_item

        int bgp_msg_status = process_bgp_message(msg_chunk, &pp->bgp_stats);
        if (bgp_msg_status) { // we only want update messages in the list, so skip if not Update
          struct msg_list_item *itemp = calloc(1, sizeof(struct msg_list_item));
          itemp->msg = msg_chunk;
          if (NULL == pp->msg_list_head)
            pp->msg_list_head = itemp;
          else
            pp->msg_list_tail->next = itemp;
          pp->msg_list_tail = itemp;
          pp->bgp_update_count++;
        };
      } else if (msg_subtype == BGP4MP_STATE_CHANGE_AS4) {
        sp->state_changes++;
        uint16_t old_state = getw16(ptr + min_mrt_length + BGP4MP_header_length);
        uint16_t new_state = getw16(ptr + min_mrt_length + BGP4MP_header_length + 2);
        // show_bgp4mp_peer_header(ptr + min_mrt_length);
        // printf("state change %d -> %d at %d\n", old_state, new_state, sp->mrt_count);
      } else {
        printf("wrong msg_subtype %d at msg %d\n", msg_subtype, sp->mrt_msg_count);
      };
    };
    // note - even for _ET extended type messages the message length calculation uses the old
    // method, i.e. the timestamp extension is effectively part of the payload for _ET types
    ptr += MIN_MRT_LENGTH + msg_length;
    bytes_left -= MIN_MRT_LENGTH + msg_length;
  };
  sp->peer_count = peers;
};

int count_msg_list(struct msg_list_item *list) {
  int i = 0;

  // simple list walk without list modification
  while (list != NULL) {
    i++;
    list = list->next;
  };
  return i;
};

struct msg_list_item *filter_msgs(struct msg_list_item *list, struct stats_bgp4mp_bgp *sp) {
  // filter list of non-compliant chunks based on process_bgp_message return
  // value builds a new list with a dummy head, which is discarded on exit
  struct msg_list_item head = {NULL, (struct chunk){NULL, 0}};
  struct msg_list_item *last = &head;
  struct msg_list_item *next;
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
