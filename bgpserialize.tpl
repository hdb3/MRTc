#ifdef __HEADER__

#include "include.h"

#include "alloc.c"
#include "libupdates2.h"
#include "hex.c"
#endif

#ifdef __BODY__
#define MARGIN 24
// allowing for at least an extra AS in the path and a locpref attribute
// as well as the 12 bytes header in the route.
//
static inline struct route * serialize_attributes___BODY__(struct route *route) {

  uint16_t q_max = route->update_length + MARGIN;
  struct route *export = alloc(q_max);
  void * r_base = (void*)route + sizeof(struct route);
  void * r_limit = r_base + route->update_length;

  void * q_base = (void*) &(export->tiebreak);
  void * q_limit = q_base + q_max;

  void * q = q_base;
  void * r = r_base;

  uint8_t flags, type_code;
  uint16_t attr_length;
  void *attr_ptr;

  void set (uint16_t attr_length, uint8_t flags, uint8_t type_code, void *attr_ptr) {
    if (attr_length<256) {
      *(uint8_t *)q++ = flags & ~0x10;
      *(uint8_t *)q++ = type_code;
      *(uint8_t *)q++ = attr_length & 0xff;
    } else {
      *(uint8_t *)q++ = flags | 0x10;
      *(uint8_t *)q++ = type_code;
      *(uint8_t *)q++ = attr_length >> 8;
      *(uint8_t *)q++ = attr_length & 0xff;
    };  
    memcpy(q,attr_ptr,attr_length);
    q += attr_length;
  };

  void set4 (uint8_t flags, uint8_t type_code, uint32_t v) {
    *(uint8_t *)q++ = flags & ~0x10;
    *(uint8_t *)q++ = type_code;
    *(uint8_t *)q++ = 4;
    *(uint32_t*)q = __bswap_32(v);
    q += 4;
  };

  void copy () {
    if (attr_ptr) {
      if (attr_length<256) {
        *(uint8_t *)q++ = flags & ~0x10;
        *(uint8_t *)q++ = type_code;
        *(uint8_t *)q++ = attr_length & 0xff;
      } else {
        *(uint8_t *)q++ = flags | 0x10;
        *(uint8_t *)q++ = type_code;
        *(uint8_t *)q++ = attr_length >> 8;
        *(uint8_t *)q++ = attr_length & 0xff;
      };  
      memcpy(q,attr_ptr,attr_length);
      q += attr_length;
    };
  };

  void get (uint8_t wanted_type_code) {
    while (r<r_limit-2) { // limit -2 because we need at least a following flags and length field too
      if (r<r_limit-2){
        flags = *(uint8_t *)r++;
        type_code = *(uint8_t *)r++;
        attr_length = *(uint8_t *)r++;
          if (0x10 & flags)
            attr_length = attr_length << 8 | (*(uint8_t *)r++);
        r += attr_length;
        if ( type_code < wanted_type_code)
	  continue;
        else if ( type_code > wanted_type_code) {
          attr_ptr = NULL; // the attribute was not found in the source route
	  break;
	} else { // the attribute was found in the source route
          attr_ptr = r;
	  break;
	};

      } else attr_ptr = NULL;  // the attribute was not found in the source route (and we reached the end of the route!)
    };  // on exit the attr_length and attr_ptr are correctly set for this attribute.
  };
#endif
#ifdef __TRAILER__
  export->update_length = (uint16_t) (q - q_base); 
  return export;
filtered:
  dalloc(export);
  return NULL;
};
#endif
