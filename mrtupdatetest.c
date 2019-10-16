#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "libmrt.h"
#include "timespec.h"
#define LIMIT 10
void gettime(struct timespec *ts) {
  int tmp = clock_gettime(CLOCK_REALTIME, ts);
};

int main(int argc, char **argv) {

  struct chunk buf, *blocks;
  struct update_list_item *update_list;
  printf("MRTc update test\n");
  struct bgp4mp_bgp_stats bgp_stats;
  struct mrt *updatedump;
  struct timespec tstart, tend, tdelta;
  int n, acctime = 0;

  assert(1 < argc);
  buf = map_mrt_file(argv[1]);
  gettime(&tstart);
  updatedump = mrt_updates_parse(buf);
  gettime(&tend);
  tdelta = timespec_sub(tend, tstart);
  mrt_summary(updatedump);
  printf("first parse complete in %ld\n", timespec_to_ms(tdelta));
  for (n = 0; n < LIMIT; n++) {
    gettime(&tstart);
    updatedump = mrt_updates_parse(buf);
    gettime(&tend);
    tdelta = timespec_sub(tend, tstart);
    acctime += timespec_to_ms(tdelta);
    printf("parse %5d complete in %ld\n", n, timespec_to_ms(tdelta));
  };
  // update_list = updatedump->bgp4mp.update_list_head;
  // printf("got %d messages from %s\n", count_update_list(update_list), argv[1]);
  // update_list = filter_msgs(update_list, &bgp_stats);
  //printf("got %d messages after filtering\n", count_update_list(update_list));
  report_mrt_bgp4mp(updatedump);
  printf("average parse time %d\n", acctime / LIMIT);
  // report_bgp4mp_bgp_stats(&bgp_stats);
  /*
  // code excesised here is superseeded for all useful purposes
  // will rewrite unit test only if needed
  if (2 == argc) {
    blocks = get_blocks_bgp4mp(updatedump, 1);
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
    struct chunk msg = get_one_bgp4mp(updatedump, peer_index, msg_index);
    print_chunk(msg);
  }
*/
};
