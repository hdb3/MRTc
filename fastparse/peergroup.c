#include "include.h"
#include "nlri2.h"
#include "bigtable.c"

struct route * ibgpserialize(struct route *route);
struct peergroup {
  struct route * (*serialize)(struct route *route);
  int fd;
};
  
static inline void  build_nlris(uint8_t * nlri, uint32_t *addrreftable, uint32_t table_index){
  uint32_t index;
  uint8_t *nlrip = nlri;
  for (index=0; index < table_index; index++)
    build_nlri(&nlrip,lookup_bigtable(addrreftable[index]));
};

void phase3(struct peergroup *peergroup, struct route *route, uint32_t *addrreftable, uint32_t table_index) {
  uint8_t nlri[4096];
  build_nlris(nlri,addrreftable,table_index);
  struct route *send_route = peergroup->serialize(route);
};
int npeergroups = 1;
struct peergroup peergroups[1];
void init_peergroups () {
  int fd = open("/dev/null",O_WRONLY);
  peergroups[0] = (struct peergroup){ ibgpserialize , fd };
};
