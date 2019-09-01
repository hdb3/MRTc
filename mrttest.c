#include "libmrt.h"
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char **argv) {
  int i;
  struct chunk buf;
  struct msg_list_item *msg_list;
  printf("MRTc\n");
  struct stats_bgp4mp *sp = calloc(1, sizeof(*sp));

  for (i = 1; i < argc; i++) {
    buf = map_mrt_file(argv[i]);
    msg_list = mrt_parse(buf, sp);
    printf("got %d messages from %s\n", count_msg_list(msg_list), argv[i]);
    msg_list = filter_msgs(msg_list, sp);
    printf("got %d messages after filtering\n", count_msg_list(msg_list));
  };
  report_stats_bgp4mp(sp);
};
