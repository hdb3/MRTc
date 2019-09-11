void free_update_list(struct update_list_item *head);
struct chunk block_builder(struct update_list_item *update_list);
void write_chunk(const char *fname, struct chunk buf);
void print_chunk(struct chunk ch);
void unmap_mrt_file(struct chunk ch);
struct chunk map_mrt_file(char *fname);
char *show_mrt_peer_record(struct mrt_peer_record *peer);
void print_mrt_peer_record(struct mrt_peer_record *peer);
