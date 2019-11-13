#ifndef __PHASE3_C
#define __PHASE3_C
#include "include.h"

//#include "locrib.c"
#include "locribjournal.c"
#include "peergroup.c"

static inline struct route * read_and_clear(uint32_t addrref) {
    // read and clear the push flag
    // if the route changes whilst we are working then a new action will be scheduled
    uint_fast64_t* p = (uint_fast64_t*) &LOCRIB[addrref];
    uint_fast64_t routeptr = atomic_fetch_xor(p,TOP64);
    return (struct route *) routeptr;
};

// void phase3(struct peergroup *peergroup, struct route *route, uint32_t *addrreftable, uint32_t table_index);
void schedule_phase3() {
  uint32_t addrref;
  uint32_t addrreftable [ 4096 ];
  uint16_t table_index;
  struct route * route, *route2;

  addrref = locribj_pull();
  while (JOURNAL_EMPTY != addrref) {
    table_index = 0;
    addrreftable[table_index++] = _LR_INDEX_MASK & addrref;
    route = read_and_clear(_LR_INDEX_MASK & addrref);
    while (!(_LR_EOB & addrref)) {
      addrref = locribj_pull();
      addrreftable[table_index++] = _LR_INDEX_MASK & addrref;
      route2 = read_and_clear(_LR_INDEX_MASK & addrref);
      assert(route2==route);  // this is a corner case which can only happen if routes were updated after the journal was written, which is
                              // allowed but unlikely in unstressed or single thread testing
                              // to be fixed up later.....
    };
    assert((JOURNAL_EMPTY != addrref)); // this is a hard assertion
                                       // because the journal should always contain blocks terminated with a marker,
                                       // so hitting the end of the journal on a non terminal addrref
                                       // is either a bug or proof that we need a spinlock (we do really)
                                       // the spin lock would allow journal blocked operations to be atomic
                                       // and also probably remove th eneed for the atomic operation over the active flag
  // now we have a contiguous block and a single route
  // we need to call per peergroup ARO functions.
 
  uint16_t pix;
  for (pix=0;pix<npeergroups;pix++)
    phase3(&peergroups[pix],route,addrreftable,table_index);   
  };
};
#endif
