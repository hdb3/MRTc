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
#include <unistd.h>

#include "libmrt.h"
#include "nlri.h"

struct chunk get_blocks_bgp4mp_peer(struct mrt_peer_record *peer) {
  long int length = 0;
  struct update_list_item *update_list = peer->bgp4mp.update_list_head;
  struct update_list_item *p;
  int offset;

  p = update_list;
  while (p != NULL) {
    length += p->msg.length + (ADD_LOCAL_PREF ? 7 : 0);
    p = p->next;
  };
    // NOTE - the assigned length assumes all messages get local pref added - but some may not be eligible e.g. withdraw
    // however, allocating too much memory is not a problem as long as we trim it or something when we finish....
#define INFINITY 0x7ffff000
  // because linux syscalls don't work with buffers bigger than 2G....
  assert(length < INFINITY);
  void *buffer = malloc(length);
  assert(NULL != buffer);
  p = update_list;
  offset = 0;
  while (p != NULL) {
    struct chunk pre = p->msg;
    if (ADD_LOCAL_PREF)
      p->msg = update_fixup_localpreference(100, p->msg);
    assert(pre.length + 7 == p->msg.length);
    memcpy(buffer + offset, p->msg.data, p->msg.length);
    offset += p->msg.length;
    assert(offset <= length);
    p = p->next;
  };
  return (struct chunk){buffer, offset};
};

static char *_show_bgp4mp_peer_address = NULL;
char *show_bgp4mp_peer_address(struct mrt_peer_record *peer) {
  if (NULL != _show_bgp4mp_peer_address)
    free(_show_bgp4mp_peer_address);
  char peer_ip_str[INET6_ADDRSTRLEN];
  char local_ip_str[INET6_ADDRSTRLEN];

  if (peer->is_ipv6) {
    inet_ntop(AF_INET6, &peer->peer_ip6, peer_ip_str, INET6_ADDRSTRLEN);
    inet_ntop(AF_INET6, &peer->local_ip6, local_ip_str, INET6_ADDRSTRLEN);
  } else {
    inet_ntop(AF_INET, &peer->peer_ip, peer_ip_str, INET6_ADDRSTRLEN);
    inet_ntop(AF_INET, &peer->local_ip, local_ip_str, INET6_ADDRSTRLEN);
  };
  assert(asprintf(&_show_bgp4mp_peer_address, "[as %-6d ip %-24s (local: as %-6d ip %-24s)]", peer->peer_as, peer_ip_str, peer->local_as, local_ip_str));
  return _show_bgp4mp_peer_address;
};

void print_bgp4mp_peer_address(struct mrt_peer_record *peer) {
  fputs(show_bgp4mp_peer_address(peer), stdout);
};

void report_bgp4mp_bgp_stats(struct bgp4mp_bgp_stats *sp) {
  printf("report_bgp4mp_bgp_stats: %-7d Opens\n", sp->open_count);
  printf("report_bgp4mp_bgp_stats: %-7d Notifications\n", sp->notification_count);
  printf("report_bgp4mp_bgp_stats: %-7d Keepalives\n", sp->keepalive_count);
  printf("report_bgp4mp_bgp_stats: %-7d Updates\n", sp->all_update_count);

  printf("report_bgp4mp_bgp_stats:   %-7d Plain Updates\n", sp->update_count);
  printf("report_bgp4mp_bgp_stats:   %-7d Withdraws\n", sp->withdraw_count);
  printf("report_bgp4mp_bgp_stats:   %-7d MP-BGP Updates\n", sp->mpbgp_count);
  //printf("report_bgp4mp_bgp_stats:   %-7d Mixed Updates\n", sp->mixed_update_count);
  //printf("report_bgp4mp_bgp_stats:   %-7d Zero NLRI Update\n", sp->zero_nrli_count);
  printf("report_bgp4mp_bgp_stats:   %-7d End-of-RIB\n", sp->eor_count);
  printf("report_bgp4mp_bgp_stats:     %-7d IBGP count\n", sp->ibgp_count);
  printf("report_bgp4mp_bgp_stats:     %-7d MED count\n", sp->med_count);
  printf("report_bgp4mp_bgp_stats:     %-7d complex_path count\n", sp->complex_path_count);
  printf("report_bgp4mp_bgp_stats:     %-7d attr_set count\n", sp->attr_set_count);

  printf("report_bgp4mp_bgp_stats:     %-7d max_update length\n", sp->max_update_length);
  printf("report_bgp4mp_bgp_stats:     %-7d max_path length\n", sp->max_path_length);
  printf("report_bgp4mp_bgp_stats:     %-7d max_raw_attributes size\n", sp->max_raw_attributes_size);
  printf("report_bgp4mp_bgp_stats:     %-7d max_raw_community size\n", sp->max_raw_community_size);
  printf("report_bgp4mp_bgp_stats:     %-7d max_raw_nlri size\n", sp->max_raw_nlri_size);
  printf("report_bgp4mp_bgp_stats:     %-7d max_nlri length\n", sp->max_nlri_length);
};

void report_mrt_bgp4mp(struct mrt *mrt) {
  assert(TYPE_BGP4MP == mrt->type);
  int i;
  struct bgp4mp_bgp_stats bgp_stats;
  memset(&bgp_stats, 0, sizeof(bgp_stats));
  for (i = 0; i < mrt->peer_count; i++) {
    //struct mrt_peer_record *peer = &mrt->peer_table[i];
    //add_bgp4mp_bgp_stats(&bgp_stats,&peer->bgp4mp.bgp_stats);
    add_bgp4mp_bgp_stats(&bgp_stats, &mrt->peer_table[i].bgp4mp.bgp_stats);
  };
  printf("report_mrt_bgp4mp: %d MRT items\n", mrt->mrt_rec_count);
  printf("report_mrt_bgp4mp: %d peers\n", mrt->peer_count);
  printf("report_mrt_bgp4mp:    %d messages, %d state changes\n", mrt->bgp4mp.mrt_bgp_msg_count, mrt->bgp4mp.state_changes);
  if (0 < mrt->bgp4mp.ipv6_discards)
    printf("report_mrt_bgp4mp:    discarded %d IPv6 items\n", mrt->bgp4mp.ipv6_discards);
  if (0 < mrt->bgp4mp.as2_discards)
    printf("report_mrt_bgp4mp:    discarded %d AS2 items\n", mrt->bgp4mp.as2_discards);
  report_bgp4mp_bgp_stats(&bgp_stats);
};

void report_mrt_bgp4mp_peers(struct mrt *mrt) {
  assert(TYPE_BGP4MP == mrt->type);
  int i;
  printf("report_mrt_bgp4mp: peer                                                                                      MRT       BGP       filtered  MPBGP\n");
  printf("report_mrt_bgp4mp: index                                                                                     records   msgs      updates   updates\n");
  for (i = 0; i < mrt->peer_count; i++) {
    struct mrt_peer_record *peer = &mrt->peer_table[i];
    printf("report_mrt_bgp4mp: %-3d", i);
    print_bgp4mp_peer_address(peer);
    printf(" %-8d  %-8d  %-8d  %-8d\n", peer->bgp4mp.rec_count, peer->bgp4mp.bgp_msg_count, peer->bgp4mp.update_count, peer->bgp4mp.bgp_stats.mpbgp_count);
  };
};

static inline void process_path_attribute_route(uint8_t type_code, struct chunk msg, struct route *route) {

  if (type_code < 64)
    route->attributes |= (1ULL << (type_code));
  else if (ATTR_SET == type_code)
    route->attributes |= (1ULL << (_ATTR_SET));
  else {
    printf("unexpected type code %d\n", type_code);
    return;
  };
  switch (type_code) {

  case ORIGIN:
    route->origin = *(uint8_t *)msg.data;
    break;

  case AS_PATH:
    // assume that the as_PATH is a singleton AS_SEQUENCE
    route->path_length = *(uint8_t *)(msg.data + 1);
    if (msg.length > 2 + 4 * route->path_length) {
      // printf("complex AS_PATH: (%ld,%d) ", msg.length, route->path_length);
      // print_chunk(msg);
      route->complex_path = 1;
    };
    assert(MAX_PATH_LENGTH >= route->path_length);
    assert(2 == *(uint8_t *)msg.data);                // AS_SEQUENCE segment type == 1
    assert(msg.length >= 2 + 4 * route->path_length); // the test that the segment exactly fills the attribute
    memcpy(route->as_path, msg.data + 2, msg.length - 2);
    break;

  case NEXT_HOP:
    assert(4 == msg.length);
    route->next_hop = *(uint32_t *)msg.data;
    break;

  case MULTI_EXIT_DISC:
    assert(4 == msg.length);
    route->med = *(uint32_t *)msg.data;
    break;

  case LOCAL_PREF:
    assert(4 == msg.length);
    route->local_pref = *(uint32_t *)msg.data;
    break;

  case COMMUNITY:
    route->communities_length = msg.length >> 2; // standard community is 4 bytes
    assert(0 == msg.length % 4);
    // printf("COMMUNITY: ");
    // print_chunk(msg);
    if (MAX_COMMUNITY_LENGTH < route->communities_length) {
      printf("oversize COMMUNITY: (%d) ", route->communities_length);
      print_chunk(msg);
    } else {
      memcpy(route->communities, msg.data, msg.length);
    };
    // assert(MAX_COMMUNITY_LENGTH >= route->communities_length);
    // memcpy(route->communities, msg.data, msg.length);
    break;

  case EXTENDED_COMMUNITIES:
    route->extended_communities_length = msg.length >> 3; // extended community is 8 bytes
    assert(0 == msg.length % 8);
    assert(MAX_EXTENDED_COMMUNITY_LENGTH >= route->extended_communities_length);
    memcpy(route->extended_communities, msg.data, msg.length);
    break;

  case LARGE_COMMUNITY:
    route->large_communities_length = msg.length / 12; // standard community is 4 bytes
    assert(0 == msg.length % 12);
    assert(MAX_LARGE_COMMUNITY_LENGTH >= route->large_communities_length);
    memcpy(route->large_communities, msg.data, msg.length);
    break;

  case ATOMIC_AGGREGATE:
    assert(0 == msg.length);
    break;

  case AGGREGATOR:
    // printf("AGGREGATOR: ");
    // print_chunk(msg);
    assert(8 == msg.length); // this the AS4 case - otherwise would be 6 not 8
    break;

  case MP_REACH_NLRI:
    break;

  case MP_UNREACH_NLRI:
    break;

  case AS_PATHLIMIT:
    assert(5 == msg.length);
    break;

  case CONNECTOR:
    // printf("CONNECTOR %ld\n", msg.length);
    // assert(6 == msg.length);
    break;

  case ATTR_SET:
    break;

  default:
    printf("unexpected attribute, type code =%d\n", type_code);
  }
};

static inline void process_path_attributes(struct chunk msg, struct route *route) {
  void *p = msg.data;
  void *limit = msg.data + msg.length;
  uint8_t flags, type_code;
  uint16_t length;
  while (p < limit) {
    flags = *(uint8_t *)p++;
    type_code = *(uint8_t *)p++;
    length = *(uint8_t *)p++;
    if (0x10 & flags)
      length = length << 8 | (*(uint8_t *)p++);
    process_path_attribute_route(type_code, (struct chunk){p, length}, route);
    // process_path_attribute(type_code, (struct chunk){p, length}, sp);
    p += length;
  };
  if (p != limit) {
    printf("process_path_attributes exception %p %p %p %ld %ld\n", msg.data, limit, p, limit - p, msg.length);
  };
  assert(p == limit);
};
static inline void update_bgp4mp_bgp_stats(struct bgp4mp_bgp_stats *sp, struct route *route, struct chunk attributes, struct chunk nlri, struct chunk withdrawn) {
  sp->all_update_count++;

  if (0 > withdrawn.length)
    sp->withdraw_count++;
  else if ((0 == nlri.length) && (0 == route->attributes))
    sp->eor_count++;
  else if (0 < nlri.length) {
    // this is the IPv4 only Update analysis
    sp->update_count++;
    if (route->attributes & (1ULL << MULTI_EXIT_DISC))
      sp->med_count++;
    if (route->attributes & (1ULL << LOCAL_PREF))
      sp->ibgp_count++;
    if (route->attributes & (1ULL << _ATTR_SET))
      sp->attr_set_count++;
    if (route->complex_path)
      sp->complex_path_count++;
    int update_length = nlri.length + withdrawn.length + attributes.length;
    int nlri_length = nlri_count(nlri);
#define UPDATEMAX(x, y) x = ((y) < (x) ? (x) : (y))
    UPDATEMAX(sp->max_update_length, update_length);
    UPDATEMAX(sp->max_raw_attributes_size, attributes.length);
    UPDATEMAX(sp->max_raw_nlri_size, nlri.length);
    UPDATEMAX(sp->max_nlri_length, nlri_length);
    UPDATEMAX(sp->max_raw_community_size, route->communities_length * 4);
    UPDATEMAX(sp->max_raw_attributes_size, attributes.length);
    UPDATEMAX(sp->max_path_length, route->path_length);
  } else if ((route->attributes & (1ULL << MP_REACH_NLRI)) || (route->attributes & (1ULL << MP_UNREACH_NLRI)))
    sp->mpbgp_count++;
  else
    route->exception = 1;
};

static int zero_nrli_flag = 1;
static inline int process_bgp_message(struct chunk msg, struct bgp4mp_bgp_stats *sp) {
  struct route route;
  memset(&route, 0, sizeof(struct route));
  assert(18 < msg.length);
  uint16_t length = getw16(msg.data + 16);
  uint8_t typecode = *(uint8_t *)(msg.data + 18);
  int is_update = 0;
  sp->msg_count++;
  assert(length == msg.length);
  switch (typecode) {

  case 1:
    sp->open_count++;
    break;

  case 2: { // Update cases - EOR/Withdraw/Update

    uint16_t withdraw_length = getw16(msg.data + 19);
    uint16_t pathattributes_length = getw16(msg.data + 21 + withdraw_length);
    uint16_t nlri_length = length - withdraw_length - pathattributes_length - 23;
    assert(length >= 23 + withdraw_length + pathattributes_length); // sanity check
    struct chunk withdrawn = {msg.data + 21, withdraw_length};
    struct chunk path_attributes = {msg.data + 23 + withdraw_length, pathattributes_length};
    struct chunk nlri = {msg.data + 23 + withdraw_length + pathattributes_length, nlri_length};

    if (0 < pathattributes_length) {
      process_path_attributes(path_attributes, &route);
      update_bgp4mp_bgp_stats(sp, &route, path_attributes, nlri, withdrawn);
      if (route.exception) {
        printf("exception: ");
        print_chunk(msg);
      }
      is_update = 1;
    };
    break;
  };

  case 3:
    sp->notification_count++;
    break;

  case 4:
    sp->keepalive_count++;
    break;

  default:
    printf("invalid typecode %d\n", typecode);
    exit(1);
  };
  return is_update;
};

void initialise_bgp4mp_peer(struct mrt_peer_record *peer) {
  void *header = peer->bgp4mp.peer_header;
  uint16_t address_family = getw16(header + 10);
  assert(0x0001 == address_family || 0x0002 == address_family);
  peer->is_ipv6 = 0x0002 == address_family;
  peer->peer_as = getw32(header + 0);
  peer->local_as = getw32(header + 4);
  if (peer->is_ipv6) {
    peer->peer_ip6 = *(struct in6_addr *)(header + 12);
    peer->local_ip6 = *(struct in6_addr *)(header + 28);
  } else {
    peer->peer_ip = (struct in_addr){__bswap_32(getw32(header + 12))};
    peer->local_ip = (struct in_addr){__bswap_32(getw32(header + 16))};
  };
};

struct mrt *mrt_updates_parse(struct chunk buf) {
  struct mrt *mrt;
  uint16_t msg_type, msg_subtype;
  uint32_t msg_length, msg_timestamp;
  int found, pn;
  int ET_extension, BGP4MP_header_length, min_mrt_length;
  int is_AS4 = 0;
  int64_t bytes_left = buf.length;
  void *ptr = buf.data;

  mrt = calloc(1, sizeof(*mrt));
  mrt->type = TYPE_BGP4MP;
  while (bytes_left >= MIN_MRT_LENGTH) {
    msg_timestamp = getw32(ptr + 0);
    msg_type = getw16(ptr + 4);
    msg_subtype = getw16(ptr + 6);
    msg_length = getw32(ptr + 8);
    assert(bytes_left >= MIN_MRT_LENGTH + msg_length);
    mrt->mrt_rec_count++;

    if (msg_type == BGP4MP) {
      min_mrt_length = MIN_MRT_LENGTH;
      ET_extension = 0;
    } else if (msg_type == BGP4MP_ET) {
      min_mrt_length = MIN_MRT_LENGTH_ET;
      ET_extension = 4;
    } else {
      printf("mrt_parse: wrong msg_type %d/%d msg# %d\n", msg_type, msg_subtype, mrt->mrt_rec_count);
      exit(1);
    };
    switch (msg_subtype) {
    case BGP4MP_STATE_CHANGE:
    case BGP4MP_MESSAGE:
    case BGP4MP_MESSAGE_LOCAL:
      is_AS4 = 0;
      if (unquiet && 0 == mrt->bgp4mp.as2_discards)
        printf("mrt_parse: unsupported AS2 msg_subtype %d at msg %d\n", msg_subtype, mrt->mrt_rec_count);
      mrt->bgp4mp.as2_discards++;
      break;
    case BGP4MP_MESSAGE_AS4:
    case BGP4MP_STATE_CHANGE_AS4:
    case BGP4MP_MESSAGE_AS4_LOCAL:
      is_AS4 = 1;
      break;
    default:
      printf("mrt_parse: bad msg_sub_type %d/%d\n", msg_type, msg_subtype);
      exit(1);
    };

    if (is_AS4) {
      // BGP4MP -> the AFI is at a fixed offset in all subtypes
      uint16_t afi = getw16(ptr + min_mrt_length + 10);
      BGP4MP_header_length = (1 == afi) ? BGP4MP_IPV4_PEER_HEADER_LENGTH : BGP4MP_IPV6_PEER_HEADER_LENGTH;
      if ((1 == afi) || (2 == afi)) {
      } else
        printf("mrt_parse: AFI wrong %d at mrt_count %d %d/%d\n", afi, mrt->mrt_rec_count, msg_type, msg_subtype);
      assert((1 == afi) || (2 == afi));
      // lookup the bgp4mp common header in the raw peer table
      found = 0;
      void *raw_bgp4mp_header = ptr + min_mrt_length;
      for (pn = 0; pn < mrt->peer_count; pn++)
        if (0 == memcmp(raw_bgp4mp_header, &mrt->peer_table[pn].bgp4mp.peer_header, BGP4MP_header_length)) {
          found = 1;
          break;
        };
      // on exit either found==0 or found==1 and pn is the index in the currrent
      // table if found==0 we should make a new entry in the raw table by
      // (re-)allocating space and copying the bgp4mp common header to it
      //
      // create a new raw peer entry if either
      //   - this is the first entry
      //   - the current peer is not found in the existing table

      if (0 == found) {
        mrt->peer_count++;
        assert(1 == mrt->peer_count - pn);
        mrt->peer_table = realloc(mrt->peer_table, mrt->peer_count * sizeof(struct mrt_peer_record));
        struct mrt_peer_record *peer = &mrt->peer_table[pn];
        memset(peer, 0, sizeof(struct mrt_peer_record));
        peer->mrt_file_index = pn;
        memcpy(&peer->bgp4mp.peer_header, raw_bgp4mp_header, BGP4MP_header_length);
        initialise_bgp4mp_peer(peer);
      };
      // pp is a convenience pointer for the current peer record
      struct mrt_peer_record *pp = &mrt->peer_table[pn];

      pp->bgp4mp.rec_count++;
      if (msg_subtype == BGP4MP_MESSAGE_AS4) {
        // capture the BGP4MP message payload)
        struct chunk msg_chunk = (struct chunk){ptr + min_mrt_length + BGP4MP_header_length, msg_length - BGP4MP_header_length - ET_extension};
        pp->bgp4mp.bgp_msg_count++;
        mrt->bgp4mp.mrt_bgp_msg_count++;

        int bgp_msg_status = process_bgp_message(msg_chunk, &pp->bgp4mp.bgp_stats);
        if (bgp_msg_status) { // we only want update messages in the list, so skip if not Update
          struct update_list_item *itemp = calloc(1, sizeof(struct update_list_item));
          itemp->msg = msg_chunk;
          if (NULL == pp->bgp4mp.update_list_head)
            pp->bgp4mp.update_list_head = itemp;
          else
            pp->bgp4mp.update_list_tail->next = itemp;
          pp->bgp4mp.update_list_tail = itemp;
          pp->bgp4mp.update_count++;
        };
      } else if (msg_subtype == BGP4MP_STATE_CHANGE_AS4) {
        mrt->bgp4mp.state_changes++;
        // state change reporting, if ever wanted...
        // uint16_t old_state = getw16(ptr + min_mrt_length + BGP4MP_header_length);
        // uint16_t new_state = getw16(ptr + min_mrt_length + BGP4MP_header_length + 2);
        // show_bgp4mp_peer_header(ptr + min_mrt_length);
        // printf("mrt_parse: state change %d -> %d at %d\n", old_state, new_state, sp->mrt_count);
      } else {
        printf("mrt_parse: wrong msg_subtype %d at msg %d\n", msg_subtype, mrt->mrt_rec_count);
      };
    };
    // note - even for _ET extended type messages the message length calculation uses the old
    // method, i.e. the timestamp extension is effectively part of the payload for _ET types
    ptr += MIN_MRT_LENGTH + msg_length;
    bytes_left -= MIN_MRT_LENGTH + msg_length;
  };
  return mrt;
};
