static char *toHex(char *buf, int l) {

  char hex_str[] = "0123456789abcdef";
  int i;
  char *result;

  // ** DANGER - unreturned memory allocation!!!!
  if (!(result = (char *)malloc(l * 2 + 1)))
    return (NULL);

  (result)[l * 2] = 0;

  if (!l)
    return (NULL);

  for (i = 0; i < l; i++) {
    (result)[i * 2 + 0] = hex_str[(buf[i] >> 4) & 0x0F];
    (result)[i * 2 + 1] = hex_str[(buf[i]) & 0x0F];
  }
  return (result);
}

static void printHex(FILE *fd, char *buf, int l) {
  char *hex = toHex(buf, l);
  fprintf(fd, "[%s]\n", hex);
  free(hex);
}

