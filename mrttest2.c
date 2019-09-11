#include <assert.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "libmrt.h"

int main(int argc, char **argv) {

  int minimum_route_table_size;
  struct update_list_item *update_list;
  struct chunk buf_tabledump, buf_updates;
  printf("MRTc match table dump and updates\n");
  struct mrt *tabledump = NULL;
  struct bgp4mp_bgp_stats bgp_stats;
  struct mrt *updatedump;

  assert(2 < argc);
  printf("processing table dump in %s\n",argv[1]); 
  buf_tabledump = map_mrt_file(argv[1]);
  tabledump = get_mrt_tabledump(buf_tabledump);
  report_mrt_tabledump(tabledump);
  analyse_mrt_tabledump(tabledump);
  printf("processing updates in %s\n",argv[2]); 
  buf_updates = map_mrt_file(argv[2]);


  updatedump = mrt_parse(buf_updates);
  update_list = updatedump->bgp4mp.update_list_head;
  printf("got %d messages from %s\n", count_update_list(update_list), argv[1]);
  update_list = filter_msgs(update_list, &bgp_stats);
  printf("got %d messages after filtering\n", count_update_list(update_list));
  report_mrt_bgp4mp(updatedump);
  report_bgp4mp_bgp_stats(&bgp_stats);
  int match_count = match_tabledump_bgp4mp(tabledump,updatedump);
  printf("matched %d tabledump peer records in updates\n",match_count);
  match_count = match_bgp4mp_tabledump(updatedump,tabledump);
  printf("matched %d update peer records in table dump\n",match_count);

/*
  if (4 > argc)
    minimum_route_table_size = 500000;
  else {
    if (1 != sscanf(argv[3], "%d", &minimum_route_table_size)) {
      printf("could not parse argv[3] for minimum_route_table_size");
      exit(1);
    };
  };
  printf("getting peers with large route tables > %d\n",minimum_route_table_size);
  build_mrt_tabledump_tabledump_updates(tabledump, minimum_route_table_size);
  build_mrt_tabledump_bgp4mp_updates(tabledump, &mrt_bgp4mp_data);
  unmap_mrt_file(buf_tabledump);
  unmap_mrt_file(buf_updates);
  write_mrt_tabledump_all_updates(tabledump);
  */
};
