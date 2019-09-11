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

static inline struct chunk block_builder(struct update_list_item *update_list) {
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
