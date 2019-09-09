#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "libmrt.h"

int main(int argc, char **argv) {
  struct chunk buf;
  printf("MRTc table dump test\n");
  struct mrt_tabledump *rib = NULL;

  assert(1 < argc);
  buf = map_mrt_file(argv[1]);
  rib = get_mrt_tabledump(buf);
  report_mrt_tabledump(rib);
  analyse_mrt_tabledump(rib);
  build_mrt_tabledump_tabledump_updates(rib, 500000);
  unmap_mrt_file(buf);
  if (3 == argc) {
    int peer_index;
    if (1 != sscanf(argv[2], "%d", &peer_index)) {
      printf("could not parse argv[2] for peer_index");
      exit(1);
    } else {
      struct chunk buf = get_updates(rib, peer_index);
      write_chunk("tabledump.bin", buf);
    };
  };
};
