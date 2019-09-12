#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "libmrt.h"

int minimum_route_table_size = 500000;
int minimum_update_count = 100000;

void process(char *fn_tabledump, char *fn_update) {

  struct chunk buf_tabledump, buf_updates;
  struct mrt *tabledump, *updatedump;

  // table dump processing stage
  printf("table dump processing stage\n\n");

  buf_tabledump = map_mrt_file(fn_tabledump);

  tabledump = get_mrt_tabledump(buf_tabledump);
  report_mrt_tabledump(tabledump);
  int ignore = trim_mrt_tabledump_size(tabledump, 1);
  //report_mrt_tabledump_peers(tabledump);
  printf("removing short tables (<%d)", minimum_route_table_size);
  fflush(stdout);
  int removed = trim_mrt_tabledump_size(tabledump, minimum_route_table_size);
  printf(" - removed %d\n", removed);
  report_mrt_tabledump_peers(tabledump);

  // Update processing stage
  printf("update processing stage\n\n");

  buf_updates = map_mrt_file(fn_update);

  updatedump = mrt_updates_parse(buf_updates);
  printf("removing IPv4 inactive peers");
  fflush(stdout);
  removed = filter_updates_on_size(updatedump, 1);
  printf(" - removed %d\n", removed);
  printf("full data analysis before peer selection\n");
  report_mrt_bgp4mp(updatedump);
  removed = filter_updates_on_size(updatedump, minimum_update_count);
  printf("\nremoved %d update peer records while trimming to > %d\n", removed, minimum_update_count);
  report_mrt_bgp4mp(updatedump);
  report_mrt_bgp4mp_peers(updatedump);

  // exit(0);

  // Combined processing stage
  printf("\nCombined processing stage\n\n");

  mrt_summary(tabledump);
  mrt_summary(updatedump);
  int match_count = match_bgp4mp_tabledump(updatedump, tabledump);
  printf("matched %d update peer records in table dump\n", match_count);
  match_count = match_tabledump_bgp4mp(tabledump, updatedump);
  printf("matched %d tabledump peer records in updates\n", match_count);
  mrt_summary(tabledump);
  mrt_summary(updatedump);

  exit(0);

  // Output processing stage
  printf("\nOutput processing stage\n\n");

  build_mrt_tabledump_updates(tabledump);
  build_mrt_tabledump_bgp4mp_updates(tabledump, updatedump);
  unmap_mrt_file(buf_tabledump);
  unmap_mrt_file(buf_updates);
  write_mrt_tabledump_all_updates(tabledump);
};

int main(int argc, char **argv) {

  printf("MRTc match table dump and updates\n");
  assert(2 < argc);

  if (argc > 3) {
    if (1 != sscanf(argv[3], "%d", &minimum_route_table_size)) {
      printf("could not parse argv[3] for minimum_route_table_size");
      exit(1);
    };
  };

  if (argc > 4) {
    if (1 != sscanf(argv[4], "%d", &minimum_update_count)) {
      printf("could not parse argv[4] for minimum_update_count");
      exit(1);
    };
  };

  printf("processing table dump from %s\n", argv[1]);
  printf("processing updates from    %s\n", argv[2]);
  process(argv[1], argv[2]);
};
