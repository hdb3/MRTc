#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "libmrt.h"
#include "libmrtribdump.h"

int main(int argc, char **argv) {
  struct chunk buf;
  printf("MRTribdump\n");
  struct mrtrib_peertable *pt = NULL;

  assert(1 < argc);
  buf = map_mrt_file(argv[1]);
  pt = get_mrtrib_peertable(buf);
  report_mrtrib_peertable(pt);
};
