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
  uint32_t peer_as = getw32(p + 0);
  uint32_t local_as = getw32(p + 4);
  uint16_t if_index = getw16(p + 8);
  uint16_t afi = getw16(p + 10);
  uint32_t peer_ip = getw32(p + 12);
  uint32_t local_ip = getw32(p + 16);
  // printf("bgp4mp_common peer_as=%6d local_as=%6d if_index=%d afi=%d
  // peer_ip=%s ",
  printf("bgp4mp_common peer_as %-6d local_as %-6d peer_ip %-16s ", peer_as,
         local_as,
         // peer_as, local_as, if_index, afi,
         inet_ntoa((struct in_addr){peer_ip}));
  printf("local_ip %-16s\n", inet_ntoa((struct in_addr){local_ip}));
};

struct msg_list_item *mrt_parse(struct chunk buf, struct stats_bgp4mp *sp) {
  struct msg_list_item *next, *current, *head;
  head = NULL;
  struct chunk bgp_msg;
  uint16_t msg_type, msg_subtype;
  uint32_t msg_length, msg_timestamp;
  void *ppeers = NULL;
  int peers = 0;
  int found, pn;

  while (buf.length >= MIN_MRT_LENGTH) {
    msg_timestamp = getw32(buf.data + 0);
    msg_type = getw16(buf.data + 4);
    msg_subtype = getw16(buf.data + 6);
    msg_length = getw32(buf.data + 8);
    assert(buf.length >= MIN_MRT_LENGTH + msg_length);
    if (msg_type != BGP4MP) {
      printf("wrong msg_type %d/%d\n", msg_type, msg_subtype);
      exit(1);
    };
    // BGP4MP -> the AFI is at a fixed offset in all subtypes
    uint16_t afi = getw16(buf.data + MIN_MRT_LENGTH + 10);
    if (1 != afi) // IPv6 is 2....
      sp->ipv6_discards++;
    else { // lookup the bgp4mp common header in the raw peer table
      found = 0;
      for (pn = 0; pn < peers; pn++)
        if (0 == memcmp(buf.data + MIN_MRT_LENGTH,
                        ppeers + pn * LENGTH_BGP4MP_COMMON_AS4,
                        LENGTH_BGP4MP_COMMON_AS4)) {
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
        ppeers = realloc(ppeers, peers * LENGTH_BGP4MP_COMMON_AS4);
        memcpy(ppeers + (peers - 1) * LENGTH_BGP4MP_COMMON_AS4,
               buf.data + MIN_MRT_LENGTH, LENGTH_BGP4MP_COMMON_AS4);
        printf("new peer added,number %3d - ", peers);
        show_bgp4mp_common(buf.data + MIN_MRT_LENGTH);
        pn = peers - 1;
      };

      if (msg_subtype == BGP4MP_MESSAGE_AS4) {
        sp->bgp_messages++;
        next = calloc(1, sizeof(struct msg_list_item));
        (next->msg).data = buf.data + MIN_MRT_LENGTH + 20;
        (next->msg).length = msg_length - MIN_MRT_LENGTH;
        if (NULL == head) {
          head = next;
        } else {
          current->next = next;
        }
        current = next;
      } else if (msg_subtype == BGP4MP_STATE_CHANGE_AS4) {
        sp->state_changes++;
        uint16_t old_state = getw16(buf.data + MIN_MRT_LENGTH + 20);
        uint16_t new_state = getw16(buf.data + MIN_MRT_LENGTH + 22);
        // show_bgp4mp_common(buf.data + MIN_MRT_LENGTH);
        // printf("state change %d -> %d at %d\n", old_state, new_state,
        // sp->mrt_count);
      } else if (msg_subtype == BGP4MP_MESSAGE) {
        sp->as2_discards++;
      } else if (msg_subtype == BGP4MP_STATE_CHANGE) {
        sp->as2_discards++;
      } else {
        printf("wrong msg_subtype %d at msg %d\n", msg_subtype, sp->mrt_count);
        // exit(1);
      };
    };
    buf.data += MIN_MRT_LENGTH + msg_length;
    buf.length -= MIN_MRT_LENGTH + msg_length;
    sp->mrt_count++;
  };
  return head;
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

struct msg_list_item *filter_msgs(struct msg_list_item *list,
                                  struct stats_bgp4mp *sp) {
  // filter list of non-compliant chunks based on process_bgp_message return
  // value builds a new list with a dummy head, which is discarded on exit
  struct msg_list_item head = {NULL, (struct chunk){NULL, 0}};
  struct msg_list_item *last = &head;
  struct msg_list_item *next;
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
