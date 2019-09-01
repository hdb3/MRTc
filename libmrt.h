
#define MIN_MRT_LENGTH 12
#define BGP4MP 16
#define BGP4MP_STATE_CHANGE 0
#define BGP4MP_MESSAGE 1
#define BGP4MP_MESSAGE_AS4 4
#define BGP4MP_STATE_CHANGE_AS4 5

struct stats_bgp4mp {
  // MRT parse level
  int mrt_count;
  int bgp_messages;
  int state_changes;
  int as2_discards;
  int ipv6_discards;

  // bgp parse level
  int open_count;
  int update_count;
  int eor_count;
  int keepalive_count;
  int notification_count;
  int withdraw_count;
  int mixed_update_count;
};

void report_stats_bgp4mp(struct stats_bgp4mp *sp);

struct chunk {
  void *data;
  int length;
};

struct msg_list_item {
  struct msg_list_item *next;
  struct chunk msg;
};

struct chunk map_mrt_file(char *fname);

struct msg_list_item *mrt_parse(struct chunk buf, struct stats_bgp4mp *sp);

int count_msg_list(struct msg_list_item *list);
struct msg_list_item *filter_msgs(struct msg_list_item *list, struct stats_bgp4mp *sp);
