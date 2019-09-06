#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "libmrt.h"
int main(int argc, char **argv) {

  struct chunk buf;
  struct msg_list_item *msg_list;
  printf("MRTc\n");
  struct stats_bgp4mp_bgp bgp_stats;
  struct stats_bgp4mp_mrt mrt_stats;

  assert(2 == argc);
  buf = map_mrt_file(argv[1]);
  mrt_parse(buf, &mrt_stats);
  msg_list = mrt_stats.msg_list_head;
  printf("got %d messages from %s\n", count_msg_list(msg_list), argv[1]);
  msg_list = filter_msgs(msg_list, &bgp_stats);
  printf("got %d messages after filtering\n", count_msg_list(msg_list));
  report_stats_bgp4mp_mrt(&mrt_stats);
  report_stats_bgp4mp_bgp(&bgp_stats);
};
