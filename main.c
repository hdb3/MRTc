#include "stdint.h"
#include "stdio.h"
#include "stdlib.h"
#include <arpa/inet.h>
#include <assert.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#define MIN_MRT_LENGTH 12
#define BGP4MP 16
#define BGP4MP_STATE_CHANGE 0
#define BGP4MP_MESSAGE 1
#define BGP4MP_MESSAGE_AS4 4
#define BGP4MP_STATE_CHANGE_AS4 5

static inline uint16_t getw16(void *p) { return __bswap_16(*(uint16_t *)p); };

static inline uint32_t getw32(void *p) { return __bswap_32(*(uint32_t *)p); };

struct chunk {
  void *data;
  int length;
};

struct msg_list_item {
  struct msg_list_item *next;
  struct chunk msg;
};

struct chunk map_mrt_file(char *fname) {
  void *buf = NULL;
  struct stat sb;
  int fd = open(fname, O_RDONLY);
  if (-1 == fd) {
    perror("file open error");
    exit(1);
  } else {
    fstat(fd, &sb);
    buf = mmap(NULL, sb.st_size, PROT_READ, MAP_PRIVATE, fd, 0);
  }
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
  printf("bgp4mp_common peer_as=%d local_as=%d if_index=%d afi=%d peer_ip=%s ",
         peer_as, local_as, if_index, afi,
         inet_ntoa((struct in_addr){peer_ip}));
  printf("local_ip=%s\n", inet_ntoa((struct in_addr){local_ip}));
};

struct msg_list_item *mrt_parse(struct chunk buf) {
  struct msg_list_item *next, *current, *head;
  head = NULL;
  struct chunk bgp_msg;
  uint16_t msg_type, msg_subtype;
  uint32_t msg_length, msg_timestamp;
  int msg_index = 0;
  int as2_discards = 0;
  int ipv6_discards = 0;
  int messages = 0;
  int state_changes = 0;

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
    if (1 != afi) { // IPv6 is 2....
      ipv6_discards++;
      if (1 == ipv6_discards)
        printf("discard IPv6 BGP4MP ITEM at msg %d\n", msg_index);
    } else if (msg_subtype == BGP4MP_MESSAGE_AS4) {
      messages++;
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
      state_changes++;
      uint16_t old_state = getw16(buf.data + MIN_MRT_LENGTH + 20);
      uint16_t new_state = getw16(buf.data + MIN_MRT_LENGTH + 22);
      // show_bgp4mp_common(buf.data + MIN_MRT_LENGTH);
      // printf("state change %d -> %d at %d\n", old_state, new_state,
      // msg_index);
    } else if (msg_subtype == BGP4MP_MESSAGE) {
      as2_discards++;
      if (1 == as2_discards)
        printf("discard AS2 BGP4MP_MESSAGE at msg %d\n", msg_index);
    } else if (msg_subtype == BGP4MP_STATE_CHANGE) {
      as2_discards++;
      if (1 == as2_discards)
        printf("discard AS2 BGP4MP_STATE_CHANGE at msg %d\n", msg_index);
    } else {
      printf("wrong msg_subtype %d at msg %d\n", msg_subtype, msg_index);
      // exit(1);
    };
    buf.data += MIN_MRT_LENGTH + msg_length;
    buf.length -= MIN_MRT_LENGTH + msg_length;
    msg_index++;
  };
  printf("got %d MRT items\n", msg_index);
  printf("    %d messages, %d state changes\n", messages, state_changes);
  if (0 < ipv6_discards)
    printf("    discarded %d IPv6 items\n", ipv6_discards);
  if (0 < as2_discards)
    printf("    discarded %d AS2 items\n", as2_discards);
  return head;
};

int open_count = 0;
int update_count = 0;
int eor_count = 0;
int keepalive_count = 0;
int notification_count = 0;
int withdraw_count = 0;
int mixed_update_count = 0;

int process_update(struct chunk msg){};

int process_bgp_message(struct chunk msg) {
  assert(18 < msg.length);
  uint16_t length = getw16(msg.data + 16);
  uint8_t typecode = *(uint8_t *)(msg.data + 18);
  int is_update = 0;
  switch (typecode) {
  case 1:
    open_count++;
    break;
  case 2: // Update cases - EOR/Withdraw/Update
    if (23 == length)
      eor_count++;
    else {
      uint16_t withdraw_length = getw16(msg.data + 19);
      // simple update
      if (0 == withdraw_length) {
        process_update(msg);
        update_count++;
        is_update = 1;
        // combined withdraw and update
      } else if (length != 23 + withdraw_length) {
        // printf("*** length=%d withdraw_length=%d\n", length,
        // withdraw_length);
        mixed_update_count++;
        // simple withdraw
      } else
        withdraw_count++;
    };
    break;
  case 3:
    notification_count++;
    break;
  case 4:
    keepalive_count++;
    break;
  default:
    printf("invalid typecode %d\n", typecode);
    exit(1);
  };
  return is_update;
};

void use_msgs(char *fname, struct msg_list_item *list) {
  int i = 0;
  while (list != NULL) {
    if (process_bgp_message(list->msg))
      ;
    i++;
    list = list->next;
  };
  printf("got %d messages from %s\n", i, fname);
};

int main(int argc, char **argv) {
  int i;
  struct chunk buf;
  struct msg_list_item *msg_list;
  printf("hello cruel MRTc world\n");
  for (i = 1; i < argc; i++) {
    buf = map_mrt_file(argv[i]);
    msg_list = mrt_parse(buf);
    use_msgs(argv[i], msg_list);
  };
  printf("\nBGP Message Statistics\n\n");
  printf("Opens %d\n", open_count);
  printf("Updates %d\n", update_count);
  printf("Withdraws %d\n", withdraw_count);
  printf("Mixed Updates %d\n", mixed_update_count);
  printf("End-of-RIB %d\n", eor_count);
  printf("Notifications %d\n", notification_count);
  printf("Keepalives %d\n", keepalive_count);
};
