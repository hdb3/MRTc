#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "libmrt.h"
#define MIN_TABLE_SIZE 100000
int main(int argc, char **argv) {
  struct chunk buf;
  printf("MRTc table dump test\n");
  struct mrt *mrt;

  assert(1 < argc);
  buf = map_mrt_file(argv[1]);
  mrt = get_mrt_tabledump(buf);
  report_mrt_tabledump(mrt);
  // analyse_mrt_tabledump(mrt);
  printf("removing short tables (<%d)", MIN_TABLE_SIZE);
  fflush(stdout);
  int removed = trim_mrt_tabledump_size(mrt, MIN_TABLE_SIZE);
  printf(" - removed %d\n", removed);
  report_mrt_tabledump(mrt);
  build_mrt_tabledump_updates(mrt);
  unmap_mrt_file(buf);
  if (3 == argc) {
    int peer_index;
    if (1 != sscanf(argv[2], "%d", &peer_index)) {
      printf("could not parse argv[2] for peer_index");
      exit(1);
    } else {
      struct chunk buf = get_updates(mrt, peer_index);
      write_chunk("tabledump.bin", buf);
    };
  };
};
