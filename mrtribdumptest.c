#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

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
  //sort_peertable(rib);
};
