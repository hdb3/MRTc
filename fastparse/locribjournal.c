#ifndef __LRJ_C
#define __LRJ_C
#include "include.h"
#include "bigtable.c"


/* locrib journal
 *
 * log locrib changes unti they are actioned by the output chain
 * a simple wrap-around buffer of address refernces
 * the producer writes an entry and increments the write cursor
 * the consumer, when the read cursor is behind the write cursor,
 * reads values from the current read cursor and increments it
 * the cursors wrap around when the reach the buffer limit
 * the buffer should be at most the allowed max size of the RIB
 * logic elsewhere guarantees that duplicate entries can not be pushed
 * the consumer re-enables updates for an address as it reads the current locrib.
 * The atomic operation is required for the locrib update only....
 *
 *
*/
#define JOURNAL_EMPTY 0xffffffff

uint32_t *_LRJOURNAL;

uint32_t jread,jwrite;

void locribj_init() {
  _LRJOURNAL=malloc(BIG*sizeof(uint32_t));
  jread = 0;
  jwrite = 0;
};

void locribj_push(uint32_t address){
  // add a journal entry for this address
  assert(JOURNAL_EMPTY != address);
  _LRJOURNAL[jwrite++] = address;
  jwrite = jwrite % BIG;
  assert(jwrite != jread);  // that would mean the buffer overran
};

uint32_t locribj_pull() {
  // get a journal entry
  if (jwrite != jread) {
    uint32_t rval =_LRJOURNAL[jread++];
    jread = jread % BIG;
    return rval;
  } else
    return JOURNAL_EMPTY;
};
#endif
