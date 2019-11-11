
#include "libupdates2.h"

void phase1(struct route **r){
  (*r) -> tiebreak.ebgp = 1;
  (*r) -> tiebreak.peer_address = 0x12312377;
  (*r) -> tiebreak.local_pref = 100;
  (*r) -> tiebreak.bgpid = 0x21223344;
};
