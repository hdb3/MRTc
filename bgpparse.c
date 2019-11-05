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

static inline void parse_update(void *p, uint16_t length) {
  struct route *route; 
  uint16_t route_length = length + sizeof(struct route);

  if (SMALL < route_length)
    route = alloc_large();
  else
    route = alloc_small();

  memcpy(route+sizeof(struct route),p,length);
  route->update_length = length;

  uint16_t withdraw_length = getw16(p);
  uint16_t pathattributes_length = getw16(p + 2 + withdraw_length);
  uint16_t nlri_length = length - withdraw_length - pathattributes_length - 4;
  assert(length >= 4 + withdraw_length + pathattributes_length); // sanity check
  void *withdrawn = p + 2;
  void * path_attributes = p + 4 + withdraw_length;
  void * nlri = p + 4 + withdraw_length + pathattributes_length;

  parse_attributes(path_attributes,pathattributes_length,route);
};

static unsigned char marker[16] = {0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff};

int msg_parse(void * base, int64_t length) {

  void *ptr, *limit, *msg;
  uint8_t msg_type;
  uint16_t msg_length;
  int msg_count=0;

  ptr = base;
  limit = base + length;

  while (ptr < limit) {
    assert (0 == memcmp(marker, ptr, 16));
    msg_length = getw16(ptr + 16);
    msg_type = getw8(ptr + 18);
    assert(2==msg_type); // this is an update parser, not a BGP FSM!!!!
    if (msg_length > 23) // i.e., not an EOR
      parse_update(ptr+19, msg_length-19);
    ptr += msg_length;
    msg_count++;
  };

  assert(ptr == limit);
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
};
