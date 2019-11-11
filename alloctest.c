#define _GNU_SOURCE

#include <arpa/inet.h>
#include <assert.h>
#include <fcntl.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>

#include "alloc.c"
#include "libupdates2.h"


int main(int argc, char **argv) {

  int i,j;
  struct route *route[10];

  init_alloc();
  for (j=0;j<10;j++) {

    reinit_alloc();

    for (i=0;i<10;i++) {
      route[i] = alloc(128 + sizeof(struct route));
      route[i]->tiebreak.local_pref = i * j;
    };

    for (i=0;i<5;i++)
      dalloc(route[i]);
 
    for (i=0;i<10;i++) {
      route[i] = alloc(128 + sizeof(struct route));
      route[i]->tiebreak.local_pref = i * j;
    };

  };
};
