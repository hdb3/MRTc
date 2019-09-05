#include "libmrt.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char **argv) {
  struct chunk buf;
  struct msg_list_item *msg_list;
  printf("MRTc\n");
  struct stats_bgp4mp *sp = calloc(1, sizeof(*sp));

  assert(2 == argc);
  buf = map_mrt_file(argv[1]);
  mrt_parse(buf, sp);
  msg_list = sp->msg_list;
  printf("got %d messages from %s\n", count_msg_list(msg_list), argv[1]);
  msg_list = filter_msgs(msg_list, sp, -1);
  printf("got %d messages after filtering\n", count_msg_list(msg_list));
  msg_list = filter_msgs(msg_list, sp, 0);
  printf("got %d messages for peer 0\n", count_msg_list(msg_list));
  report_stats_bgp4mp(sp);
};
