#include "libupdates2.h"
#include "bigtable.c"

#define TOP64 0x8000000000000000

struct route **LOCRIB=NULL;

void locrib_init() {
  LOCRIB = calloc(BIG, sizeof(void*));
};

void locrib_push(uint32_t address){
  // add a journal entry for this address
};
/* push logic
 * if the tiebreak changes the winner then:
 *   if the pushed bit is set dont push again
 *   but do ensure that the pushed bit i sprpagated 
 *   if not set the set it, and push
 *
 *   i.e ALWAYS set the pushed bit
 *       ONLY push if it was not set already
*/

#define ISSET64(route) (TOP64 & (uint64_t) route)
#define ISNOTSET64(route) (!(ISSET64(route)))
#define CLEAR64(route) (~TOP64 & (uint64_t) route)
#define SET64(route) (TOP64 | (uint64_t) route)

void locrib(uint32_t address, struct route *new) {

  struct route * current = LOCRIB[address];

  if (tiebreaker(&new->tiebreak,&current->tiebreak)) {
    LOCRIB[address] = (struct route *) SET64(new);
    if (ISNOTSET64(current))
      locrib_push(address);
  };
};
