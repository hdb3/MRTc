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
  struct mrt_ribdump *rib = NULL;

  assert(1 < argc);
  buf = map_mrt_file(argv[1]);
  rib = get_mrt_ribdump(buf);
  report_mrt_ribdump(rib);
  analyse_mrt_ribdump(rib);
  unmap_mrt_file(buf);
  if (3 == argc) {
    int peer_index;
    if (1 != sscanf(argv[2], "%d", &peer_index)) {
      printf("could not parse argv[2] for peer_index");
      exit(1);
    } else {
      struct chunk buf = get_updates(rib, peer_index);
      write_chunk("ribdump.bin", buf);
    };
  };
};
