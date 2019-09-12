struct chunk get_updates(struct mrt *mrt, int index);
struct chunk build_update(void *target, const struct chunk withdrawn, const struct chunk path_attributes, const struct chunk nlri);
struct chunk fixup_localpreference(uint32_t local_preference, struct chunk path_attributes);
struct chunk update_fixup_localpreference(uint32_t local_preference, struct chunk update);
void build_tabledump_updates(struct mrt_peer_record *pr);
void mrt_list_walker(struct mrt *mrt, struct chunk buf);
struct mrt *get_mrt_tabledump(struct chunk buf);
