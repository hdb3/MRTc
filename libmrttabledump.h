struct chunk get_updates(struct mrt *mrt, int index);
struct chunk build_update(void *target, const struct chunk withdrawn, const struct chunk path_attributes, const struct chunk nlri);
struct chunk fixup_localpreference(uint32_t local_preference, struct chunk path_attributes);
struct chunk update_fixup_localpreference(uint32_t local_preference, struct chunk update);
void build_tabledump_updates(struct mrt_peer_record *pr);
void sort_peer_table(struct mrt *tabledump);
void analyse_mrt_tabledump(struct mrt *tabledump);
void write_mrt_tabledump_all_updates(struct mrt *tabledump);
void build_mrt_tabledump_bgp4mp_updates(struct mrt *tabledump, struct mrt *updatedump);
int trim_mrt_tabledump_size(struct mrt *tabledump, int requested_table_size);
void report_mrt_tabledump(struct mrt *mrt);
void mrt_list_walker(struct mrt *mrt, struct chunk buf);
struct mrt *get_mrt_tabledump(struct chunk buf);
void build_mrt_tabledump_updates(struct mrt *tabledump);
