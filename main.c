#include "stdint.h"
#include "stdio.h"
#include "stdlib.h"
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

struct msg_list_item *mrt_parse(struct chunk buf) {
  struct msg_list_item *next, *current, *head;
  head = NULL;
  struct chunk bgp_msg;
  uint16_t msg_type, msg_subtype;
  uint32_t msg_length, msg_timestamp;
  int msg_index = 0;
  int as2_discards = 0;
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
    if (msg_subtype == BGP4MP_MESSAGE_AS4) {
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
      uint16_t old_state = getw16(buf.data + MIN_MRT_LENGTH + 16);
      uint16_t new_state = getw16(buf.data + MIN_MRT_LENGTH + 18);
      printf("state change %d -> %d\n", old_state, new_state);
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
  if (0 < as2_discards)
    printf("    discarded %d AS2 items\n", as2_discards);
  return head;
};

void use_msgs(char *fname, struct msg_list_item *list) {
  int i = 0;
  while (list != NULL) {
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
};
