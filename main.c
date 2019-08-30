#include "stdio.h"
#include "stdlib.h"
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

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
  return NULL;
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
