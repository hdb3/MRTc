
#undef __TRAILER__
#undef __BODY__
#define __HEADER__
#include "bgpserialize.tpl"
#undef __HEADER__

#define __BODY__ ibgpserialize
#include "bgpserialize.tpl"
#undef __BODY__

  get(ORIGIN);
  set(1,0x40,ORIGIN,attr_ptr);
  get(AS_PATH);
  copy();
  get(NEXT_HOP);
  copy();
  set4(0x40,LOCAL_PREF,route->tiebreak.local_pref);

#define __TRAILER__
#include "bgpserialize.tpl"
#undef __TRAILER__
