#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "libmrt.h"
int main(int argc, char **argv) {

  struct chunk buf, *blocks;
  struct msg_list_item *msg_list;
  printf("MRTc update test\n");
  struct bgp4mp_bgp_stats bgp_stats;
  struct mrt_bgp4mp mrt_bgp4mp_data;

  assert(1 < argc);
  buf = map_mrt_file(argv[1]);
  mrt_parse(buf, &mrt_bgp4mp_data);
  msg_list = mrt_bgp4mp_data.msg_list_head;
  printf("got %d messages from %s\n", count_msg_list(msg_list), argv[1]);
  msg_list = filter_msgs(msg_list, &bgp_stats);
  printf("got %d messages after filtering\n", count_msg_list(msg_list));
  report_mrt_bgp4mp(&mrt_bgp4mp_data);
  report_bgp4mp_bgp_stats(&bgp_stats);
  if (2 == argc) {
    blocks = get_blocks_bgp4mp(&mrt_bgp4mp_data, 1);
    write_chunk("updates.bin", blocks[0]);
  } else if (4 == argc) {
    int peer_index, msg_index;
    if (1 != sscanf(argv[2], "%d", &peer_index)) {
      printf("could not parse argv[2] for peer_index");
      exit(1);
    };
    if (1 != sscanf(argv[3], "%d", &msg_index)) {
      printf("could not parse argv[3] for msg_index");
      exit(1);
    };
    struct chunk msg = get_one_bgp4mp(&mrt_bgp4mp_data, peer_index, msg_index);
    print_chunk(msg);
  }
};
