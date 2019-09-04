#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "libmrt.h"
#include "libmrtribdump.h"

int main(int argc, char **argv) {
  struct chunk buf;
  printf("MRTribdump\n");
  struct mrtrib *rib = NULL;

  assert(1 < argc);
  buf = map_mrt_file(argv[1]);
  rib = get_mrtrib(buf);
  report_mrtrib(rib);
  analyse_mrtrib(rib);
  unmap_mrt_file(buf);
  buf = get_updates(rib, 0);
  int fd = creat("updates.bin", 00664);
  int tmp = write(fd, buf.data, buf.length);
};
