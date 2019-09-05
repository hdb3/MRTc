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

void report_stats_bgp4mp(struct stats_bgp4mp *sp) {
  printf("\nMRT Record Statistics\n\n");
  printf("got %d MRT items\n", sp->mrt_count);
  printf("    %d messages, %d state changes\n", sp->bgp_messages,
         sp->state_changes);
  if (0 < sp->ipv6_discards)
    printf("    discarded %d IPv6 items\n", sp->ipv6_discards);
  if (0 < sp->as2_discards)
    printf("    discarded %d AS2 items\n", sp->as2_discards);

  printf("\nBGP Message Statistics\n\n");
  printf("Opens %d\n", sp->open_count);
  printf("Updates %d\n", sp->update_count);
  printf("Withdraws %d\n", sp->withdraw_count);
  printf("Mixed Updates %d\n", sp->mixed_update_count);
  printf("End-of-RIB %d\n", sp->eor_count);
  printf("Notifications %d\n", sp->notification_count);
  printf("Keepalives %d\n", sp->keepalive_count);
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

void show_bgp4mp_common(void *p) {
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
  printf("bgp4mp_common peer_as %-6d local_as %-6d peer_ip %-24s local_ip %s\n", peer_as, local_as, peer_ip_str, local_ip_str);
};

void mrt_parse(struct chunk buf, struct stats_bgp4mp *sp) {
  struct msg_list_item *next, *current, *head;
  head = NULL;
  struct chunk bgp_msg;
  uint16_t msg_type, msg_subtype;
  uint32_t msg_length, msg_timestamp;
  void *ppeers = NULL;
  int peers = 0;
  int found, pn;
  int BGP4MP_header_length, min_mrt_length;
  int is_AS4 = 0;
  int bytes_left = buf.length;
  void *ptr = buf.data;

  while (bytes_left >= MIN_MRT_LENGTH) {
    // printf("progress: %d left %ld consumed\n",bytes_left,ptr-buf.data);
    msg_timestamp = getw32(ptr + 0);
    msg_type = getw16(ptr + 4);
    msg_subtype = getw16(ptr + 6);
    msg_length = getw32(ptr + 8);
    assert(bytes_left >= MIN_MRT_LENGTH + msg_length);

    if (msg_type == BGP4MP)
      min_mrt_length = MIN_MRT_LENGTH;
    else if (msg_type == BGP4MP_ET)
      min_mrt_length = MIN_MRT_LENGTH_ET;
    else {
      printf("wrong msg_type %d/%d msg# %d\n", msg_type, msg_subtype, sp->mrt_count);
      exit(1);
    };
    switch (msg_subtype) {
    case BGP4MP_STATE_CHANGE:
    case BGP4MP_MESSAGE:
    case BGP4MP_MESSAGE_LOCAL:
      is_AS4 = 0;
      if (0 == sp->as2_discards)
        printf("first unsupported AS2 msg_subtype %d at msg %d\n", msg_subtype, sp->mrt_count);
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
      BGP4MP_header_length = (1 == afi) ? 20 : 44;
      if ((1 == afi) || (2 == afi)) {
      } else
        printf("AFI wrong %d at mrt_count %d %d/%d\n", afi, sp->mrt_count, msg_type, msg_subtype);
      assert((1 == afi) || (2 == afi));
      // lookup the bgp4mp common header in the raw peer table
      found = 0;
      for (pn = 0; pn < peers; pn++)
        if (0 == memcmp(ptr + min_mrt_length, ppeers + pn * 44, BGP4MP_header_length)) {
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
        ppeers = realloc(ppeers, peers * 44);
        memcpy(ppeers + (peers - 1) * 44, ptr + min_mrt_length, BGP4MP_header_length);
        printf("new peer added,number %3d - ", peers);
        show_bgp4mp_common(ptr + min_mrt_length);
        pn = peers - 1;
      };

      if (msg_subtype == BGP4MP_MESSAGE_AS4) {
        sp->bgp_messages++;
        next = calloc(1, sizeof(struct msg_list_item));
        // capture the BGP4MP message payload)
        next->msg.data = ptr + min_mrt_length + BGP4MP_header_length;
        next->msg.length = msg_length - BGP4MP_header_length;
        next->peer_index = pn;
        if (NULL == head) {
          head = next;
        } else {
          current->next = next;
        }
        current = next;
      } else if (msg_subtype == BGP4MP_STATE_CHANGE_AS4) {
        sp->state_changes++;
        uint16_t old_state = getw16(ptr + min_mrt_length + BGP4MP_header_length);
        uint16_t new_state = getw16(ptr + min_mrt_length + BGP4MP_header_length + 2);
        // show_bgp4mp_common(ptr + min_mrt_length);
        // printf("state change %d -> %d at %d\n", old_state, new_state,
        // sp->mrt_count);
      } else {
        printf("wrong msg_subtype %d at msg %d\n", msg_subtype, sp->mrt_count);
      };
    };
    // note - even for _ET extended type messages the message length calculation uses the old
    // method, i.e. the timestamp extension is effectively part of the payload for _ET types
    ptr += MIN_MRT_LENGTH + msg_length;
    bytes_left -= MIN_MRT_LENGTH + msg_length;
    sp->mrt_count++;
  };
  // printf("progress: %d left %ld consumed\n",bytes_left,ptr-buf.data);
  sp->msg_list = head;
  sp->peer_count = peers;
};

int process_update(struct chunk msg){};

static inline int process_bgp_message(struct chunk msg,
                                      struct stats_bgp4mp *sp) {
  assert(18 < msg.length);
  uint16_t length = getw16(msg.data + 16);
  uint8_t typecode = *(uint8_t *)(msg.data + 18);
  int is_update = 0;
  switch (typecode) {
  case 1:
    sp->open_count++;
    break;
  case 2: // Update cases - EOR/Withdraw/Update
    if (23 == length)
      sp->eor_count++;
    else {
      uint16_t withdraw_length = getw16(msg.data + 19);
      // simple update
      if (0 == withdraw_length) {
        process_update(msg);
        sp->update_count++;
        is_update = 1;
        // combined withdraw and update
      } else if (length != 23 + withdraw_length) {
        // printf("*** length=%d withdraw_length=%d\n", length,
        // withdraw_length);
        sp->mixed_update_count++;
        // simple withdraw
      } else
        sp->withdraw_count++;
    };
    break;
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

int count_msg_list(struct msg_list_item *list) {
  int i = 0;

  // simple list walk without list modification
  while (list != NULL) {
    i++;
    list = list->next;
  };
  return i;
};

struct msg_list_item *filter_msgs(struct msg_list_item *list, struct stats_bgp4mp *sp, int peer_index) {
  // filter list of non-compliant chunks based on process_bgp_message return
  // value builds a new list with a dummy head, which is discarded on exit
  struct msg_list_item head = {NULL, (struct chunk){NULL, 0}};
  struct msg_list_item *last = &head;
  struct msg_list_item *next;
  while (list != NULL) {
    next = list->next;
    if ((-1 == peer_index || list->peer_index == peer_index) && process_bgp_message(list->msg, sp)) {
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
