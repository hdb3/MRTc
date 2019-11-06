
static inline uint8_t as_path_count(void *p, uint16_t length) {

  uint16_t offset = 0;
  uint16_t path_count = 0;
  uint8_t seg_type, seg_length;

  while (offset < length) {
    seg_type = *(uint8_t *)(p + offset);
    seg_length = *(uint8_t *)(p + offset + 1);
    if (1 == seg_type) // AS SET
      path_count++;
    else if (2 == seg_type) // AS SEQ
      path_count += seg_length;
    else
      assert(seg_type < 3);
    offset += 2 + 4 * seg_length;
  };
  assert(offset == length);
  assert(path_count < 256);
  return (uint8_t)path_count;
};
