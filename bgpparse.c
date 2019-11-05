#define _GNU_SOURCE
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
#include <time.h>
#include "timespec.h"
#include "getw.h"
#include "alloc.c"
#include "bigtable.c"
#include "bgpupdate.c"
#include "nlri2.h"

static unsigned char marker[16] = {0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff};

static int large_msg_count;

int msg_parse(void * base, int64_t length) {

  void *ptr, *limit, *msg;
  uint8_t msg_type;
  uint16_t msg_length;
  int msg_count=0;
  int _large_msg_count=0;

  ptr = base;
  limit = base + length;

  while (ptr < limit) {
    assert (0 == memcmp(marker, ptr, 16));
    msg_length = getw16(ptr + 16);
    msg_type = getw8(ptr + 18);
    uint16_t route_length = msg_length + sizeof(struct route) -16; // we store the route header and the body of the message
                                                                   // but not the 16 byte marker
    if (SMALL < route_length) {
      _large_msg_count++;
      msg = alloc_large();
    } else {
      msg = alloc_small();
    };
    // printf("msg %p ptr %p\n", msg, ptr);
    parse_update(ptr+19, msg_length-19, (struct route *) msg);
    // memcpy(msg+sizeof(struct route),ptr+16,msg_length-16);
    ptr += msg_length;
    msg_count++;

    // printf("msg_count %d msg_length %d msg_type %d\n", msg_count, msg_length, msg_type);
  };
  // printf("messages %d delta %ld\n", msg_count, limit - ptr);
  // printf("bytes_left %p messages %p\n", ptr, limit);
  assert(ptr == limit);
  large_msg_count = _large_msg_count;
  reinit_alloc();
  return msg_count;
};

int main(int argc, char **argv) {

  struct stat sb;
  int fd;
  char *fname;
  void *buf;
  int64_t length;
  int message_count, tmp, i;
  struct timespec tstart, tend;
  double duration;
  int repeat = 100;

  fname = argv[1];
  fd = open(fname, O_RDONLY);
  if (-1 == fd) {
    perror("file open error");
    exit(1);
  };
  fstat(fd, &sb);
  length = sb.st_size;
  printf("opened %s file size %ld\n", fname, length);
  buf = mmap(NULL, length, PROT_READ, MAP_PRIVATE | MAP_POPULATE, fd, 0);
  close(fd);

  init_alloc();

  tmp = clock_gettime(CLOCK_REALTIME, &tstart);
  for (i = 0 ; i < repeat ; i++)
    message_count =  msg_parse(buf, length);
  tmp = clock_gettime(CLOCK_REALTIME, &tend);
  duration = timespec_to_ms(timespec_sub(tend, tstart))/1000.0;
  printf("read %d messages\n", message_count);
  printf("complete in %f\n", duration);
  printf("M msgs/sec = %f\n", repeat * message_count / duration / 1000000);
  printf("msgs latency (nsec) = %f\n", duration / repeat / message_count  * 1000000000);
  printf("Gbytes/sec = %f\n", repeat * length / duration / 1000000000);
  printf("(average message size is %0.2f bytes)\n", (1.0 * length) / message_count);
  printf("large message count = %d\n",large_msg_count);
};
