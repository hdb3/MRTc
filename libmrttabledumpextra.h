void sort_peer_table(struct mrt *tabledump);
void report_mrt_tabledump(struct mrt *mrt);
void report_mrt_tabledump_peers(struct mrt *mrt);
void write_mrt_tabledump_all_updates(struct mrt *tabledump);
void build_mrt_tabledump_bgp4mp_updates(struct mrt *tabledump, struct mrt *updatedump);
void build_mrt_tabledump_updates(struct mrt *tabledump);
int trim_mrt_tabledump_size(struct mrt *tabledump, int requested_table_size);
int trim_mrt_tabledump_unlinked(struct mrt *tabledump);
