#include "libupdates2.h"
#include "bigtable.c"
#include "locribjournal.c"
void schedule_phase3();

/*
 * this is the unoptimised version of locrib hich demonstrates performance for the
 * simple replacement cases
 *
 * for complete operation it needs to be enhanced to support search of peer ARIs when a current winner is replaced or withdrawn
 *
*/

#define TOP64 0x8000000000000000

#define TOP32 0x80000000
#define SND32 0x40000000

#define _LR_EOB TOP32
#define _LR_NULL_ROUTE SND32
#define _LR_INDEX_MASK 0x3fffffff

struct route **LOCRIB=NULL;

void locrib_init() {
  LOCRIB = calloc(BIG, sizeof(void*));
  locribj_init();
};


#define ISSET64(route) (TOP64 & (uint64_t) route)
#define ISNOTSET64(route) (!(ISSET64(route)))
#define CLEAR64(route) (~TOP64 & (uint64_t) route)
#define SET64(route) (TOP64 | (uint64_t) route)

/*
 * there are some critical scheduling issues in this function:
 * 1) pushing an action request in to the ARO queue - but,
 *    this is only done when there is not already an action request - 
 *    this prevents multiple updates for the same route being queued,
 *    and also provides a hard guard over the amount of queued work and corresponding storage
 * 2) the locrib is the main point of concurrency control if multiple threads are working on input updates
 *    concurrent;y.
 *    Simple logic would dictate a spin lock over the locrib entry.
 * 3) Pushing work onto the ARO queue is also the trigger to start 
 *    working the queue, if it is not already active.  THis can be done after/outside the spin lock...
 *
 *  The deatil of push logic follows:
 *    locrib is only updated if the tiebreak favours the new route
 *    updating locrib ALWAYS sets the pushed bit (it is only cleared in ARO processing)
 *    updating locrib ONLY pushes a ARO action item when it finds that the push bit has been claered
 *    This ensures that ARO processing will come back to this route promptly.
 *
 * The stored state (per address) is the pushed bit and another bit for the spin lock
*/

void locrib(uint32_t address, struct route *new) {

  struct route * current = LOCRIB[_LR_INDEX_MASK & address];

  if (( (void*) TOP64 == current ) || // the RIB was empty for this address
                                      // so the tiebreak is not needed....
      (tiebreaker(&new->tiebreak,&current->tiebreak))) {
    LOCRIB[_LR_INDEX_MASK & address] = (struct route *) SET64(new);
    if (ISNOTSET64(current))
      locribj_push(address);
  };
  if (_LR_EOB & address)
    schedule_phase3();  // this is the point at which input processing for this route can stop
                        //  and start work on other update messages

};
#include "phase3.c"
