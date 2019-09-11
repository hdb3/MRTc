void mrt_summary(struct mrt *mrt);
struct mrt_peer_record *lookup_update_peer(struct mrt *mrt, uint32_t as, struct in_addr ip);
struct mrt_peer_record *lookup_mrt_peer(struct mrt *mrt, struct mrt_peer_record *key);
int match_tabledump_bgp4mp(struct mrt *tabledump, struct mrt *updatesdump);
int match_bgp4mp_tabledump(struct mrt *updatesdump, struct mrt *tabledump);
int filter_updates_on_size(struct mrt *mrt, int min_size);
