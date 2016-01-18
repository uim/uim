/* 
  Copyright (c) 2003-2013 uim Project https://github.com/uim/uim

  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions
  are met:

  1. Redistributions of source code must retain the above copyright
     notice, this list of conditions and the following disclaimer.
  2. Redistributions in binary form must reproduce the above copyright
     notice, this list of conditions and the following disclaimer in the
     documentation and/or other materials provided with the distribution.
  3. Neither the name of authors nor the names of its contributors
     may be used to endorse or promote products derived from this software
     without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS IS'' AND
  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE
  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
  OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
  SUCH DAMAGE.

*/

/*
 * SKK is a simple Japanese input method
 *
 * Many many things are to be implemented!
 */
#include <config.h>

#include <sys/types.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <signal.h>
#include <errno.h>
#include <sys/socket.h>
#include <netdb.h>
#include <sys/param.h>
#ifdef HAVE_STRINGS_H
#include <strings.h>
#endif
#ifdef HAVE_POLL_H
#include <poll.h>
#elif defined(HAVE_SYS_POLL_H)
#include <sys/poll.h>
#else
#include "bsd-poll.h"
#endif

#include "uim.h"
#include "uim-scm.h"
#include "uim-scm-abbrev.h"
#include "uim-helper.h"
#include "dynlib.h"
#include "uim-notify.h"
#include "gettext.h"

#include "bsdlook.h"

#define skk_isalpha(ch)	(skk_islower(ch) || skk_isupper(ch))
#define skk_islower(ch)	((((unsigned char)ch) >= 'a') && (((unsigned char)ch) <= 'z'))
#define skk_isupper(ch)	((((unsigned char)ch) >= 'A') && (((unsigned char)ch) <= 'Z'))
#define skk_isascii(ch)	((((unsigned char)ch) & ~0x7f) == 0)

#define IGNORING_WORD_MAX	63
#define USE_SKK_JISYO_S_BUF	1	/* use SKK-JISYO.S as a cache for
					   word completion */
#define SKK_JISYO_S	DATADIR "/skk/SKK-JISYO.S"

/*
 * cand : candidate
 */


/* candidate array for each okurigana
 *
 * |C0|C1| .. |Cnr_real_cands| ..	       |Cnr_cands|
 * <-------should be saved --><-- cache of master dict -->
 */
struct skk_cand_array {
  /* okurigana string */
  char *okuri;

  int nr_cands; /* length of cands array allocated */
  int nr_real_cands; /* length of read from file part */
  /* candidate string */
  char **cands;

  /* this array was used and merged with okuri-nasi entry array */
  int is_used;
  /* link to its parent line */
  struct skk_line *line;
};

/* skk_line state */
#define SKK_LINE_NEED_SAVE	(1<<0)
#define SKK_LINE_USE_FOR_COMPLETION	(1<<1)

/* skk dictionary line */
struct skk_line {
  /* line index. head part */
  char *head;
  /* line index. okurigana part. value will be 0 if it is okuri-nasi
     entry */
  char okuri_head;
  /* array of candidate array for different okuri-gana */
  int nr_cand_array;
  struct skk_cand_array *cands;
  /* state of line */
  int state;
  /* link to next entry in the list */
  struct skk_line *next;
};

/* skk dictionary file */
typedef struct dic_info_ {
  /* address of mmap'ed dictionary file */
  void *addr;
  /* byte offset of first valid entry in mmap'ed region */
  int first;
  /* byte offset of first okuri-nasi entry */
  int border;
  /* size of dictionary file */
  int size;
  /* head of cached skk dictionary line list. LRU ordered */
  struct skk_line head;
  /* timestamp of personal dictionary */
  time_t personal_dic_timestamp;
  /* whether cached lines are modified or not */
  int cache_modified;
  /* length of cached lines */
  int cache_len;
  /* skkserv related state */
  int skkserv_state;
  /* skkserv hostname */
  char *skkserv_hostname;
  /* skkserv port number */
  int skkserv_portnum;
  /* skkserv address family. AF_UNSPEC or AF_INET or AF_INET6 */
  int skkserv_family;
  /* timeout (milisec) for skkserv completion */
  int skkserv_completion_timeout;
} dic_info;

/* completion */
struct skk_comp_array {
  /* index of completion */
  char *head;
  /* array of completion string */
  int nr_comps;
  char **comps;
  /**/
  int refcount;
  /**/
  struct skk_comp_array *next;
} *skk_comp;

/* XXX should create skk.h */
static uim_lisp skk_replace_numeric(uim_lisp head_);

static uim_lisp restore_numeric(const char *s, uim_lisp numlst_);
static char *replace_numeric(const char *str);
static char *sanitize_word(const char *str, const char *prefix);
static int is_purged_cand(const char *str);
static void merge_purged_cands(dic_info *skk_dic,
		struct skk_cand_array *src_ca,
		struct skk_cand_array *dst_ca, int src_nth, int dst_nth);
static void merge_purged_cand_to_dst_array(dic_info *skk_dic,
		struct skk_cand_array *src_ca,
		struct skk_cand_array *dst_ca, char *purged_cand);
static void update_personal_dictionary_cache_with_file(dic_info *skk_dic,
		const char *fn, int is_personal);
static void look_get_comp(struct skk_comp_array *ca, const char *str);
static uim_lisp look_get_top_word(const char *str);
static char *quote_word(const char *word, const char *prefix);

/* skkserv connection */
#define SKK_SERV_BUFSIZ	1024
#define SKK_SERV_USE	(1<<0)
#define SKK_SERV_CONNECTED	(1<<1)
#define SKK_SERV_TRY_COMPLETION	(1<<2)

static int skkservsock = -1;
static FILE *rserv, *wserv;
/* prototype */
static int open_skkserv(const char *hostname, int portnum, int family);
static void close_skkserv(void);
static void skkserv_disconnected(dic_info *di);

static int use_look = 0;
static uim_look_ctx *skk_look_ctx = NULL;

static uim_bool is_setugid;

static int
calc_line_len(const char *s)
{
  int i;
  for (i = 0; s[i] != '\n'; i++);
  return i;
}

static int
is_okuri(const char *line_str)
{
  const char *b;
  /* find first white space */
  b = strchr(line_str, ' ');
  if (!b || b == line_str)
    return 0;
  /* check previous character */
  b--;
  if (skk_isalpha(*b) && (!skk_isascii(line_str[0]) || line_str[0] == '>'))
    return 1;
  return 0;
}

static int
find_first_line(dic_info *di)
{
  char *s = di->addr;
  int off = 0;

  while (off < di->size && s[off] == ';') {
    int l = calc_line_len(&s[off]);
    off += l + 1;
  }
  return off;
}

static int
find_border(dic_info *di)
{
  char *s = di->addr;
  int off = 0;
  while (off < di->size) {
    int l = calc_line_len(&s[off]);
    if (s[off] == ';') {
      off += l + 1;
      continue;
    }
    if (!is_okuri(&s[off]))
      return off;
    off += l + 1;
  }
  /* every entry is okuri-ari, it may not happen. */
  return di->size - 1;
}

static dic_info *
open_dic(const char *fn, uim_bool use_skkserv, const char *skkserv_hostname,
	 int skkserv_portnum, int skkserv_family)
{
  dic_info *di;
  struct stat st;
  int fd;
  void *addr = NULL;
  int mmap_done = 0;

  di = (dic_info *)uim_malloc(sizeof(dic_info));

  di->skkserv_hostname = NULL;
  if (use_skkserv) {
    di->skkserv_hostname = uim_strdup(skkserv_hostname);
    di->skkserv_portnum = skkserv_portnum;
    di->skkserv_family = skkserv_family;
    di->skkserv_state = SKK_SERV_USE | open_skkserv(skkserv_hostname,
						    skkserv_portnum,
						    skkserv_family);
    di->skkserv_completion_timeout = uim_scm_symbol_value_int("skk-skkserv-completion-timeout");
  } else {
    di->skkserv_state = 0;
    fd = open(fn, O_RDONLY);
    if (fd != -1) {
      if (fstat(fd, &st) != -1) {
	addr = mmap(0, st.st_size, PROT_READ, MAP_SHARED, fd, 0);
	if (addr != MAP_FAILED) {
	  mmap_done = 1;
	}
      }
      close(fd);
    }
  }

  di->addr = mmap_done ? addr : NULL;
  di->size = mmap_done ? st.st_size : 0;
  di->first = mmap_done ? find_first_line(di) : 0;
  di->border = mmap_done ? find_border(di) : 0;

  di->head.next = NULL;
  di->personal_dic_timestamp = 0;
  di->cache_modified = 0;
  di->cache_len = 0;

  return di;
}

static const char *
find_line(dic_info *di, int off)
{
  char *ptr = di->addr;
  while (off > 0 && (ptr[off] != '\n' || ptr[off + 1] == ';'))
    off--;

  if (off)
    off++;

  return &ptr[off];
}

static char *
extract_line_index(dic_info *di, int off, char *buf, int len)
{
  const char *p = find_line(di, off);
  int i;
  if (p[0] == ';')
    return NULL;

  for (i = 0; i < len && p[i] != ' '; i++)
    buf[i] = p[i];
  buf[i] = '\0';

  return buf;
}

static int
do_search_line(dic_info *di, const char *s, int min,
	       int max, int d)
{
  char buf[256];
  char *r;
  int idx = ((unsigned int)min + (unsigned int)max) >> 1;
  int c = 0;

  if (abs(max - min) < 4)
    return -1;

  r = extract_line_index(di, idx, buf, 256);
  if (r)
    c = strcmp(s, r);
  else
    return -1;

  if (!c)
    return idx;

  if (c * d > 0)
    return do_search_line(di, s, idx, max, d);
  else
    return do_search_line(di, s, min, idx, d);

  return -1;
}

/* This function name is temporary. I want a better name. */
static char *
first_space(char *str)
{
  while (*str && (*str != ' '))
    str++;

  return str;
}

/* This function returns a pointer with '/' or '\0' */
static char *
next_cand_slash(char *str)
{
  int i = 0;
  int open_bracket = 0;

  while (*str && (*str != '/' || open_bracket == 1)) {
    if (*str == '[' && i == 0)
      open_bracket = 1;

    if (open_bracket == 1 && *str == ']' && *(str + 1) == '/')
      open_bracket = 0;
    str++;
    i++;
  }
  return str;
}

static char *
next_slash_in_bracket(char *str)
{
  while (*str && *str != '/')
    str++;

  return str;
}

static char *
okuri_in_bracket(char *str)
{
  char *p, *term;

  if (!str)
    return NULL;

  p = uim_strdup(str);
  term = next_slash_in_bracket(p);

  if (*term == '\0') {
    /* this is not the bracket used for skk-henkan-strict-okuri-precedence */
    free(p);
    return NULL;
  }
  
  *term = '\0';
  return p;
}

static char *
nth_candidate(char *str, int nth)
{
  char *p, *term;
  int i;

  str = first_space(str);
  for (i = 0; i <= nth; i++) {
    str = next_cand_slash(str);
    if (*str == '/')
      str++;
  }

  if (*str == '\0')
    return NULL;

  p = uim_strdup(str);
  term = next_cand_slash(p);
  *term = '\0';
  return p;
}

static void
free_skk_line(struct skk_line *sl)
{
  int i, j;

  if (!sl)
    return ;

  for (i = 0; i < sl->nr_cand_array; i++) {
    struct skk_cand_array *ca = &sl->cands[i];
    for (j = 0; j < ca->nr_cands; j++)
      free(ca->cands[j]);
    free(ca->okuri);
    free(ca->cands);
  }
  free(sl->head);
  free(sl->cands);
  free(sl);
}

static void
free_skk_dic(dic_info *skk_dic)
{
  if (skk_dic) {
    struct skk_line *sl, *tmp;

    if (skk_dic->addr)
      munmap(skk_dic->addr, skk_dic->size);

    sl = skk_dic->head.next;
    while (sl) {
      tmp = sl;
      sl = sl->next;
      free_skk_line(tmp);
    }

    if (skk_dic->skkserv_state & SKK_SERV_CONNECTED)
      close_skkserv();
    free(skk_dic->skkserv_hostname);

    free(skk_dic);
  }
}

/* init */
static uim_lisp
skk_dic_open(uim_lisp fn_, uim_lisp use_skkserv_, uim_lisp skkserv_hostname_,
	     uim_lisp skkserv_portnum_, uim_lisp skkserv_family_)
{
  const char *fn, *skkserv_hostname, *skkserv_family_str;
  uim_bool use_skkserv;
  int skkserv_portnum, skkserv_family;
  dic_info *skk_dic;

  fn = REFER_C_STR(fn_);
  use_skkserv = C_BOOL(use_skkserv_);
  skkserv_hostname = REFER_C_STR(skkserv_hostname_);
  skkserv_portnum = C_INT(skkserv_portnum_);
  skkserv_family_str = REFER_C_STR(skkserv_family_);

  is_setugid = uim_helper_is_setugid();
  signal(SIGPIPE, SIG_IGN);

  skkserv_family = AF_UNSPEC;
  if (skkserv_family_str) {
    if (!strcmp(skkserv_family_str, "inet"))
      skkserv_family = AF_INET;
    else if (!strcmp(skkserv_family_str, "inet6"))
      skkserv_family = AF_INET6;
  }

  skk_dic = open_dic(fn, use_skkserv, skkserv_hostname, skkserv_portnum,
		     skkserv_family);

  return MAKE_PTR(skk_dic);
}

static uim_lisp
skk_free_dic(uim_lisp skk_dic_)
{
  dic_info *skk_dic = NULL;

  if (PTRP(skk_dic_))
    skk_dic = C_PTR(skk_dic_);

  free_skk_dic(skk_dic);

  return uim_scm_f();
}

static struct skk_cand_array *
find_candidate_array_from_line(struct skk_line *sl, const char *okuri,
			       int create_if_notfound)
{
  int i;
  struct skk_cand_array *ca;

  if (!okuri || !strlen(okuri))
    return &sl->cands[0];

  for (i = 1; i < sl->nr_cand_array; i++) {
    if (okuri && !strcmp(okuri, sl->cands[i].okuri))
      return &sl->cands[i];
  }

  if (!create_if_notfound)
    return &sl->cands[0];

  /* allocate now */
  sl->nr_cand_array++;
  sl->cands = uim_realloc(sl->cands,
		      sizeof(struct skk_cand_array) * sl->nr_cand_array);
  ca = &sl->cands[sl->nr_cand_array - 1];
  ca->is_used = 0;
  ca->cands = NULL;
  ca->nr_cands = 0;
  ca->nr_real_cands = 0;
  ca->okuri = uim_strdup(okuri);
  ca->line = sl;
  return ca;
}

static void
push_back_candidate_to_array(struct skk_cand_array *ca, const char *cand)
{
  ca->nr_cands++;
  if (ca->cands)
    ca->cands = uim_realloc(ca->cands, sizeof(char *) * ca->nr_cands);
  else
    ca->cands = uim_malloc(sizeof(char *));
  ca->cands[ca->nr_cands - 1] = uim_strdup(cand);
}

static void
merge_base_candidates_to_array(dic_info *skk_dic,
		               struct skk_line *sl,
			       struct skk_cand_array *dst_ca)
{
  int i, j;
  struct skk_cand_array *src_ca;

  if (!sl)
    return ;

  src_ca = &sl->cands[0];
  if (src_ca == dst_ca)
    return ;

  for (i = 0; i < src_ca->nr_cands; i++) {
    int dup = 0;
    int src_purged_cand_index = -1;
    int dst_purged_cand_index = -1;

    if (i < src_ca->nr_real_cands && is_purged_cand(src_ca->cands[i]))
      src_purged_cand_index = i;

    for (j = 0; j < dst_ca->nr_cands; j++) {
      if (dst_purged_cand_index == -1 && is_purged_cand(dst_ca->cands[j]))
	dst_purged_cand_index = j;
      if (!strcmp(src_ca->cands[i], dst_ca->cands[j])) {
	dup = 1;
      }
    }
    if (!dup) {
      if (src_purged_cand_index != -1 && dst_purged_cand_index != -1)
	merge_purged_cands(skk_dic, src_ca, dst_ca, src_purged_cand_index,
			dst_purged_cand_index);
      else if (src_purged_cand_index != -1 && dst_purged_cand_index == -1)
	merge_purged_cand_to_dst_array(skk_dic, src_ca, dst_ca,
			src_ca->cands[src_purged_cand_index]);
#if 0
      /*
       * Just adding words subsequent to real_cands
       * (push_back_candidate_to_array) is enough.
       */
      else if (src_purged_cand_index == -1 && dst_purged_cand_index != -1)
	merge_word_to_dst_cand_array_with_purged_words(dst_ca,
			src_ca, src_ca->cands[i]);
#endif
      else
	push_back_candidate_to_array(dst_ca, src_ca->cands[i]);
    }
  }
}

static void
compose_line_parts(dic_info *di, struct skk_line *sl,
		   char *okuri, char *line)
{
  int nth;
  char *tmp;
  struct skk_cand_array *ca = find_candidate_array_from_line(sl, okuri, 1);

  nth = 0;
  do {
    tmp = nth_candidate(line, nth);
    if (tmp) {
      if (tmp[0] == '[') {
	char *str = okuri_in_bracket(&tmp[1]);
	if (!str) {
	  /*
	   * this is not the bracket used for
	   * skk-henkan-strict-okuri-precedence
	   */
	  char *quoted = quote_word(tmp, "(concat \"");
	  push_back_candidate_to_array(ca, quoted);
	  free(quoted);
	} else {
	  tmp[0] = ' '; /* create first_space */
	  compose_line_parts(di, sl, str, &tmp[0]);
	  free(str);
	}
      } else if (tmp[0] != ']') {
	push_back_candidate_to_array(ca, tmp);
      }
      nth++;
      free(tmp);
    } else {
      break;
    }
  } while (1);
}

static struct skk_line *
alloc_skk_line(const char *word, char okuri_head)
{
  struct skk_line *sl;
  sl = uim_malloc(sizeof(struct skk_line));
  sl->state = 0;
  sl->head = uim_strdup(word);
  sl->okuri_head = okuri_head;
  sl->nr_cand_array = 1;
  sl->cands = uim_malloc(sizeof(struct skk_cand_array));
  sl->cands[0].okuri = NULL;
  sl->cands[0].cands = NULL;
  sl->cands[0].nr_cands = 0;
  sl->cands[0].nr_real_cands = 0;
  sl->cands[0].is_used = 0;
  sl->cands[0].line = sl;
  return sl;
}

static struct skk_line *
copy_skk_line(struct skk_line *p)
{
  int i, j;
  struct skk_line *sl;

  if (!p)
    return NULL;

  sl = uim_malloc(sizeof(struct skk_line));
  sl->state = p->state;
  sl->head = uim_strdup(p->head);
  sl->okuri_head = p->okuri_head;
  sl->nr_cand_array = p->nr_cand_array;
  sl->cands = uim_malloc(sizeof(struct skk_cand_array) * sl->nr_cand_array);
  for (i = 0; i < sl->nr_cand_array; i++) {
    struct skk_cand_array *ca = &sl->cands[i];
    struct skk_cand_array *q = &p->cands[i];

    ca->okuri = q->okuri ? uim_strdup(q->okuri) : NULL;
    ca->nr_cands = q->nr_cands;
    ca->nr_real_cands = q->nr_real_cands;
    ca->cands = uim_malloc(sizeof(char *) * ca->nr_cands);
    for (j = 0; j < ca->nr_cands; j++)
      ca->cands[j] = uim_strdup(q->cands[j]);
    ca->is_used = q->is_used;
    ca->line = sl;
  }
  sl->next = NULL;
  return sl;
}

/*
 * Compose skk line
 */
static struct skk_line *
compose_line(dic_info *di, const char *word, char okuri_head, char *entry)
{
  struct skk_line *sl;

  sl = alloc_skk_line(word, okuri_head);
  /* parse */
  compose_line_parts(di, sl, NULL, entry);

  return sl;
}

static void
add_line_to_cache_head(dic_info *di, struct skk_line *sl)
{
  sl->next = di->head.next;
  di->head.next = sl;

  di->cache_len++;
  di->cache_modified = 1;
}

static void
move_line_to_cache_head(dic_info *di, struct skk_line *sl)
{
  struct skk_line *prev;

  if (di->head.next == sl)
    return;

  prev = di->head.next;
  while (prev->next != sl) {
    prev = prev->next;
  }
  prev->next = sl->next;
  sl->next = di->head.next;
  di->head.next = sl;

  di->cache_modified = 1;
}

#if 0
static void
add_line_to_cache_last(dic_info *di, struct skk_line *sl)
{
  struct skk_line *prev;

  if (di->head.next == NULL)
    di->head.next = sl;
  else {
    prev = di->head.next;
    while (prev->next) {
      prev = prev->next;
    }
    prev->next = sl;
  }
  sl->next = NULL;

  di->cache_len++;
  di->cache_modified = 1;
}
#endif

static struct skk_line *
search_line_from_server(dic_info *di, const char *s, char okuri_head)
{
  char r;
  struct skk_line *sl;
  int n = 0, ret, len;
  char buf[SKK_SERV_BUFSIZ];
  char *line, *idx;
  ssize_t nr;

  if (!(di->skkserv_state & SKK_SERV_CONNECTED)) {
    if (!((di->skkserv_state |= open_skkserv(di->skkserv_hostname,
					     di->skkserv_portnum,
					     di->skkserv_family)) &
	  SKK_SERV_CONNECTED))
      return NULL;
  }

  uim_asprintf(&idx, "%s%c", s, okuri_head);

  fprintf(wserv, "1%s \n", idx);
  ret = fflush(wserv);
  if (ret != 0 && errno == EPIPE) {
    free(idx);
    skkserv_disconnected(di);
    return NULL;
  }

  uim_asprintf(&line, "%s ", idx);
  free(idx);

  if ((nr = read(skkservsock, &r, 1)) == -1 || nr == 0) {
    skkserv_disconnected(di);
    free(line);
    return NULL;
  }

  if (r == '1') {  /* succeeded */
    while (1) {
      if ((nr = read(skkservsock, &r, 1)) == -1 || nr == 0) {
	skkserv_disconnected(di);
	free(line);
	return NULL;
      }

      if (r == '\n') {
	len = strlen(line) + n;
	line = uim_realloc(line, len + 1);
	strlcat(line, buf, len + 1);
	break;
      }

      buf[n] = r;
      buf[n + 1] = '\0';
      if (n == SKK_SERV_BUFSIZ - 2) {
	len = strlen(line) + n + 1;
	line = uim_realloc(line, len + 1);
	strlcat(line, buf, len + 1);
	n = 0;
      } else {
	n++;
      }
    }
    sl = compose_line(di, s, okuri_head, line);
    free(line);
    return sl;
  } else {
    while ((nr = read(skkservsock, &r, 1)) != -1 && nr != 0 && r != '\n')
      ;
    free(line);
    return NULL;
  }
}

static struct skk_line *
search_line_from_file(dic_info *di, const char *s, char okuri_head)
{
  int n;
  const char *p;
  int len;
  char *line, *idx;
  struct skk_line *sl;

  if (!di->addr)
    return NULL;

  uim_asprintf(&idx, "%s%c", s, okuri_head);

  if (okuri_head)
    n = do_search_line(di, idx, di->first, di->border - 1, -1);
  else
    n = do_search_line(di, idx, di->border, di->size - 1, 1);

  free(idx);

  if (n == -1)
    return NULL;

  p = find_line(di, n);
  len = calc_line_len(p);
  line = uim_malloc(len + 1);
  /* strncat is used intentionally because *p is too long string */
  line[0] = '\0';
  strncat(line, p, len);
  sl = compose_line(di, s, okuri_head, line);
  free(line);
  return sl;
}

static struct skk_line *
search_line_from_cache(dic_info *di, const char *s, char okuri_head)
{
  struct skk_line *sl;

  if (!di)
    return NULL;

  /* search from cache */
  for (sl = di->head.next; sl; sl = sl->next) {
    if (!strcmp(sl->head, s) && sl->okuri_head == okuri_head)
      return sl;
  }
  return NULL;
}


static struct skk_cand_array *
find_cand_array(dic_info *di, const char *s,
		char okuri_head, const char *okuri,
		int create_if_not_found)
{
  struct skk_line *sl, *sl_file;
  struct skk_cand_array *ca;
  int from_file = 0;

  if (!di)
    return NULL;

  sl = search_line_from_cache(di, s, okuri_head);
  if (!sl) {
    if (di->skkserv_state & SKK_SERV_USE)
      sl = search_line_from_server(di, s, okuri_head);
    else
      sl = search_line_from_file(di, s, okuri_head);
    if (!sl) {
      if (!create_if_not_found)
	return NULL;
      sl = alloc_skk_line(s, okuri_head);
    }
    from_file = 1;
    add_line_to_cache_head(di, sl);
  }

  ca = find_candidate_array_from_line(sl, okuri, create_if_not_found);

  if (!ca->is_used) {
    merge_base_candidates_to_array(di, sl, ca);
    ca->is_used = 1;
    if (!from_file) {
      if (di->skkserv_state & SKK_SERV_USE) {
	sl_file = search_line_from_server(di, s, okuri_head);
	if (!(di->skkserv_state & SKK_SERV_CONNECTED))
	  ca->is_used = 0;
      } else
	sl_file = search_line_from_file(di, s, okuri_head);
      merge_base_candidates_to_array(di, sl_file, ca);
      free_skk_line(sl_file);
    }
  }

  return ca;
}

static struct skk_cand_array *
find_cand_array_lisp(dic_info *skk_dic, uim_lisp head_, uim_lisp okuri_head_,
		     uim_lisp okuri_, int create_if_not_found,
		     uim_lisp numeric_conv_)
{
  char o;
  const char *hs;
  const char *okuri = NULL;
  struct skk_cand_array *ca;
  char *rs = NULL;

  hs = REFER_C_STR(head_);

  if (TRUEP(numeric_conv_))
    rs = replace_numeric(hs);

  if (okuri_ != uim_scm_null())
    okuri = REFER_C_STR(okuri_);

  if (okuri_head_ == uim_scm_null()) {
    o = '\0';
  } else {
    const char *os = REFER_C_STR(okuri_head_);
    o = os[0];
  }

  if (!rs)
    ca = find_cand_array(skk_dic, hs, o, okuri, create_if_not_found);
  else {
    ca = find_cand_array(skk_dic, rs, o, okuri, create_if_not_found);
    free(rs);
  }

  return ca;
}

/*
 * purged_cand: /(skk-ignore-dic-word "foo" "bar" ...)/
 * purged_words: {"foo", "bar", ..., NULL}
 */
static int
is_purged_cand(const char *str)
{
  char *p;

  p = strstr(str, "(skk-ignore-dic-word ");
  if (p == str)
    return 1;

  return 0;
}

static char *
expand_str(const char *p)
{
  char buf[BUFSIZ];
  int i = 0;
  int c, n, ndigits;

  while (*p != '\0') {
    c = *p;
    if (c == '\\') {
      p++;
      c = *p;
      if (c == '\0')
	break;
      switch (c) {
      case '\\':
	c = '\\';
	break;
      case 'n':
	c = '\n';
	break;
      case 'r':
	c = '\r';
	break;
      case '0':
      case '1':
      case '2':
      case '3':
      case '4':
      case '5':
      case '6':
      case '7':
    	n = c - '0';
    	ndigits = 1;
    	while (ndigits < 3) {
    	  p++;
    	  c = *p;
    	  if (*p == '\0') {
	    uim_notify_fatal(N_("uim-skk: error in expand_str"));
	    return NULL;
	  }
	  if (c >= '0' && c <= '7') {
	    n = n * 8 + c - '0';
	    ndigits++;
	  } else {
	    p--;
	    break;
	  }
	}
	c = n;
      }
    }
    if ((i + 1) >= BUFSIZ) {
      uim_notify_fatal(N_("uim-skk: too long word"));
      return NULL;
    }
    buf[i] = c;
    i++;
    p++;
  }
  buf[i] = '\0';
  return uim_strdup(buf);
}

static char **
get_purged_words(const char *str)
{
  char *p;
  char **words = NULL;
  char *word = NULL;
  int nr = 0;
  int open = 0;
  int len = 0;

  p = strstr(str, "(skk-ignore-dic-word");
  if (!p)
    return NULL;

  p = first_space(p);
  if (*p == '\0')
    return NULL;
  p++;

  while (*p != '\0') {
    if (*p == '"' && p[-1] != '\\') {
      open = open ? 0 : 1;
      if (open) {
	p++;
	word = p;
	len = 0;
      } else {
	char *orig = uim_malloc(len + 1);
	char *expanded_word;

	nr++;
	if (words)
	  words = uim_realloc(words, sizeof(char *) * nr);
	else
	  words = uim_malloc(sizeof(char *));
	strlcpy(orig, word, len + 1);

	expanded_word = expand_str(orig);
	if (expanded_word)
	  words[nr - 1] = expanded_word;
	else
	  words[nr - 1] = uim_strdup(orig);
	free(orig);
      }
    }
    p++;
    len++;
  }
  if (words) {
    words = uim_realloc(words, sizeof(char *) * (nr + 1));
    words[nr] = NULL;
  }
  return words;
}

static int
nr_purged_words(char **p)
{
  int i = 0;

  while (p && p[i])
    i++;
  return i;
}

static void
free_allocated_purged_words(char **p)
{
  int i = 0;

  if (!p)
    return;

  while (p[i]) {
    free(p[i]);
    i++;
  }
  free(p);
}

static int
is_purged_only(struct skk_cand_array *ca)
{
  int i, j;
  char **purged_words;

  if (ca->nr_real_cands > 1)
    return 0;

  if ((purged_words = get_purged_words(ca->cands[0])) != NULL) {
    int nr_purged = nr_purged_words(purged_words);
    /* going to compare words beyond nr_real_cands */
    for (i = ca->nr_real_cands; i < ca->nr_cands; i++) {
      for (j = 0; j < nr_purged; j++) {
	/* return false if there is any different candidate */
	if (strcmp(ca->cands[i], purged_words[j])) {
	  free_allocated_purged_words(purged_words);
	  return 0;
	}
      }
    }
    free_allocated_purged_words(purged_words);
    return 1;
  }
  return 0;
}

static int
match_to_discarding_index(int indices[], int n)
{
  int i = 0;
  while (indices[i] != -1) {
    if (indices[i] == n)
      return 1;
    i++;
  }
  return 0;
}

static uim_lisp
skk_get_entry(uim_lisp skk_dic_, uim_lisp head_, uim_lisp okuri_head_,
	      uim_lisp okuri_, uim_lisp numeric_conv_)
{
  struct skk_cand_array *ca;
  dic_info *skk_dic = NULL;

  if (PTRP(skk_dic_))
    skk_dic = C_PTR(skk_dic_);

  ca = find_cand_array_lisp(skk_dic, head_, okuri_head_, okuri_, 0, numeric_conv_);

  if (ca && ca->nr_cands > 0 && !is_purged_only(ca))
      return uim_scm_t();

  if (TRUEP(numeric_conv_))
    return skk_get_entry(skk_dic_, head_, okuri_head_, okuri_, uim_scm_f());

  return uim_scm_f();
}

static uim_lisp
skk_store_replaced_numeric_str(uim_lisp head_)
{
  const char *str;
  int len;

  int prev_is_num = 0;
  int i, numlen = 0, start = 0;
  char *numstr = NULL;
  uim_lisp lst = uim_scm_null();

  str = REFER_C_STR(head_);
  len = strlen(str);

  for (i = 0; i < len; i++) {
    if (isdigit((unsigned char)str[i])) {
      if (prev_is_num == 0) {
	start = i;
	numlen = 1;
      } else {
	numlen++;
      }
      prev_is_num = 1;
    } else {
      if (prev_is_num) {
	/* add number into list */
	if (!numstr)
	  numstr = uim_malloc(numlen + 1);
	else
	  numstr = uim_realloc(numstr, numlen + 1);
	strlcpy(numstr, &str[start], numlen + 1);
	lst = CONS(MAKE_STR(numstr), lst);
      }
      prev_is_num = 0;
    }
  }

  /*
   * Add last number into list if string is ended with numeric
   * character.
   */
  if (prev_is_num) {
    if (!numstr)
      numstr = uim_malloc(numlen + 1);
    else
      numstr = uim_realloc(numstr, numlen + 1);
    strlcpy(numstr, &str[start], numlen + 1);
    lst = CONS(MAKE_STR(numstr), lst);
  }
  free(numstr);

  return uim_scm_callf("reverse", "o", lst);
}

static char *wide_num_list[] =
  {"£°", "£±", "£²", "£³", "£´", "£µ", "£¶", "£·", "£¸", "£¹"};
static char *kanji_num_list[] =
  {"¡»", "°ì", "Æó", "»°", "»Í", "¸Þ", "Ï»", "¼·", "È¬", "¶å"};
static char *kanji_num_position_list[] =
  {NULL, "½½", "É´", "Àé", "Ëü", NULL, NULL, NULL, "²¯", NULL,
   NULL, NULL, "Ãû", NULL, NULL, NULL, "µþ", NULL, NULL, NULL};
static char *kanji_check_num_list[] =
  {"¡»", "°í", "Ð±", "»²", "»Í", "¸à", "Ï»", "¼·", "È¬", "¶å"};
static char *kanji_check_num_position_list[] =
  {NULL, "½¦", "É´", "ïô", "èß", NULL, NULL, NULL, "²¯", NULL,
   NULL, NULL, "Ãû", NULL, NULL, NULL, "µþ", NULL, NULL, NULL};

static char *
numeric_wide_or_kanji_conv(const char *numstr, int method)
{
  char *mbstr;
  int i, len;

  len = strlen(numstr);
  mbstr = uim_malloc(len * 2 + 1);

  for (i = 0; i < len; i++) {
    if (method == 1)
      strcpy(&mbstr[i * 2], wide_num_list[numstr[i] - '0']);
    else
      strcpy(&mbstr[i * 2], kanji_num_list[numstr[i] - '0']);
  }
  mbstr[len * 2] = '\0';

  return mbstr;
}

static char *
numeric_kanji_with_position_conv(const char *numstr)
{
  char *mbstr;
  int i, j, len, mblen;
  int position;
  int head_is_zero = 0;

  len = strlen(numstr);
  if (len > 20) /* too big number */
    return uim_strdup(numstr);

  mbstr = uim_malloc(len * 2 + 1);
  mblen = len * 2;

  for (i = 0, j = 0; j < len; i++, j++) {
    position = len - j - 1;
    if (numstr[j] == '0') {
      i--;
      mblen -= 2;
      /* check zero at the head */
      if (j == 0) {
	head_is_zero = 1;
      } else {
	/* add Ëü, ²¯, Ãû, µþ for zero */
	if ((position >= 4) && ((position % 4) == 0) && !head_is_zero) {
	  int use_position = 0;
	  if (j >= 3) {
	    if (!((numstr[j - 1] == '0') && (numstr[j - 2] == '0') &&
				    (numstr[j - 3] == '0')))
	      use_position = 1;
	  } else if (j == 2) {
	    if (!((numstr[j - 1] == '0') && (numstr[j - 2] == '0')))
	      use_position = 1;
	  } else if (j == 1) {
	    if (!(numstr[j - 1] == '0'))
	      use_position = 1;
	  }
	  if (use_position) {
	    i++;
	    mblen += 2;
	    if (mblen > len * 2)
	      mbstr = uim_realloc(mbstr, mblen + 2);
	    strcpy(&mbstr[i * 2], kanji_num_position_list[position]);
	  }
	}
      }
    } else {
      if (head_is_zero == 1)
	head_is_zero = 0;

      /* replace numstr[j] with kanji number */
      if (numstr[j] == '1') {
	/*
	 * use "°ì" only for the one at the place of °ì, Ëü, ²¯, Ãû,
	 * µþ or °ìÀéËü
	 */
	if (((position % 4) == 0) ||
	    ((position >= 7) &&
	     ((position % 4) == 3) &&
	     (numstr[j + 1] == '0') &&
	     (numstr[j + 2] == '0') &&
	     (numstr[j + 3] == '0'))) {
	  strcpy(&mbstr[i * 2], kanji_num_list[1]);
	} else {
	  i--;
	  mblen -= 2;
	}
      } else {
	strcpy(&mbstr[i * 2], kanji_num_list[numstr[j] - '0']);
      }

      /* add ½½, É´, Àé for number whose place is exceeded Ëü */
      if (position > 4) {
	if ((position % 4) != 0) {
	  i++;
	  mblen += 2;
	  if (mblen > len * 2)
	    mbstr = uim_realloc(mbstr, mblen + 2);
	  strcpy(&mbstr[i * 2], kanji_num_position_list[position % 4]);
	}
      }

      /* add position */
      if (kanji_num_position_list[position]) {
	i++;
	mblen += 2;
	if (mblen > len * 2)
	  mbstr = uim_realloc(mbstr, mblen + 2);
	strcpy(&mbstr[i * 2], kanji_num_position_list[position]);
      }
    }
  }

  /* in case of zero */
  if (head_is_zero) {
    strcpy(&mbstr[0], kanji_num_list[0]);
    mblen = 2;
  }

  mbstr[mblen] = '\0';
  return mbstr;
}

static char *
numeric_kanji_for_check_conv(const char *numstr)
{
  char *mbstr;
  int i, j, len, mblen;
  int position;
  int head_is_zero = 0;

  len = strlen(numstr);
  if (len > 20) /* too big number */
    return uim_strdup(numstr);

  mbstr = uim_malloc(len * 2 + 1);
  mblen = len * 2;

  for (i = 0, j = 0; j < len; i++, j++) {
    position = len - j - 1;
    if (numstr[j] == '0') {
      i--;
      mblen -= 2;
      /* check zero at the head */
      if (j == 0) {
	head_is_zero = 1;
      } else {
	/* add èß, ²¯, Ãû, µþ for zero */
	if ((position >= 4) && ((position % 4) == 0) && !head_is_zero) {
	  int use_position = 0;
	  if (j >= 3) {
	    if (!((numstr[j - 1] == '0') && (numstr[j - 2] == '0') &&
				    (numstr[j - 3] == '0')))
	      use_position = 1;
	  } else if (j == 2) {
	    if (!((numstr[j - 1] == '0') && (numstr[j - 2] == '0')))
	      use_position = 1;
	  } else if (j == 1) {
	    if (!((numstr[j - 1] == '0')))
	      use_position = 1;
	  }
	  if (use_position) {
	    i++;
	    mblen += 2;
	    if (mblen > len * 2)
	      mbstr = uim_realloc(mbstr, mblen + 2);
	    strcpy(&mbstr[i * 2], kanji_check_num_position_list[position]);
	  }
	}
      }
    } else {
      if (head_is_zero == 1)
	head_is_zero = 0;

      /* replace numstr[j] with kanji number */
      strcpy(&mbstr[i * 2], kanji_check_num_list[numstr[j] - '0']);

      /* add ½¦, É´, ïô for number whose place is exceeded èß */
      if (position > 4) {
	if ((position % 4) != 0) {
	  i++;
	  mblen += 2;
	  if (mblen > len * 2)
	    mbstr = uim_realloc(mbstr, mblen + 2);
	  strcpy(&mbstr[i * 2], kanji_check_num_position_list[position % 4]);
	}
      }

      /* add position */
      if (kanji_check_num_position_list[position]) {
	i++;
	mblen += 2;
	if (mblen > len * 2)
	  mbstr = uim_realloc(mbstr, mblen + 2);
	strcpy(&mbstr[i * 2], kanji_check_num_position_list[position]);
      }
    }
  }

  /* in case of zero */
  if (head_is_zero) {
    strcpy(&mbstr[0], kanji_check_num_list[0]);
    mblen = 2;
  }

  mbstr[mblen] = '\0';
  return mbstr;
}

static char *
numeric_shogi_conv(const char *numstr)
{
  char *mbstr;
  int len;

  len = strlen(numstr);
  if (len != 2) /* allow two digit number only */
    return uim_strdup(numstr);

  mbstr = uim_malloc(5);
  strcpy(&mbstr[0], wide_num_list[numstr[0] - '0']);
  strcpy(&mbstr[2], kanji_num_list[numstr[1] - '0']);
  mbstr[4] = '\0';

  return mbstr;
}

/* returns string with malloc() */
static char *
numeric_convert(const char *numstr, int method)
{
  char *ret;

  /*
   *  method #4 is already handled in skk_get_nth_candidate()
   */
  switch (method) {
  case 0:
    ret = uim_strdup(numstr);
    break;
  case 1: /* Á´³Ñ¿ô»ú */
  case 2: /* ´Á¿ô»ú °Ì¼è¤êÌµ¤· */
    ret = numeric_wide_or_kanji_conv(numstr, method);
    break;
  case 3: /* ´Á¿ô»ú °Ì¼è¤êÍ­¤ê */
    ret = numeric_kanji_with_position_conv(numstr);
    break;
  case 5: /* ¾®ÀÚ¼êÉ½µ­ */
    ret = numeric_kanji_for_check_conv(numstr);
    break;
  case 9: /* ¾­´ýÉ½µ­ */
    ret = numeric_shogi_conv(numstr);
    break;
  default:
    ret = uim_strdup(numstr);
    break;
  }
  return ret;
}

static uim_lisp
skk_merge_replaced_numeric_str(uim_lisp str_, uim_lisp numlst_)
{
  char *str;
  int i, j, len, newlen;
  int method;
  int convlen;
  const char *numstr;
  char *convstr;

  if (str_ == uim_scm_null())
    return uim_scm_null();

  str = C_STR(str_);
  len = strlen(str);
  newlen = len;

  for (i = 0, j = 0; j < len; i++, j++) {
    if (str[i] == '#') {
      method = str[i + 1] - '0';
      if (NULLP(numlst_))
	 break;

      numstr = REFER_C_STR(CAR(numlst_));

      convstr = numeric_convert(numstr, method);
      convlen = strlen(convstr);

      newlen = newlen - 2 + convlen;
      str = uim_realloc(str, newlen + 1);
      memmove(&str[i + convlen], &str[i + 2], newlen - i - convlen + 1);
      memcpy(&str[i], convstr, convlen);
      i = i - 2 + convlen;

      numlst_ = CDR(numlst_);
    }
  }

  return MAKE_STR_DIRECTLY(str);
}

static char *
replace_numeric(const char *str)
{
  char *newstr;
  int prev_is_num = 0;
  int i, j, len, newlen;

  newstr = uim_strdup(str);
  len = newlen = strlen(newstr);

  for (i = 0, j = 0; j < len; i++, j++) {
    if (isdigit((unsigned char)newstr[i])) {
      if (prev_is_num == 0) {
	newstr[i] = '#';
      } else {
	memmove(&newstr[i], &newstr[i + 1], newlen - i);
	newlen--;
	i--;
      }
      prev_is_num = 1;
    } else {
      prev_is_num = 0;
    }
  }
  return newstr;
}

static uim_lisp
skk_replace_numeric(uim_lisp head_)
{
  char *str;
  
  str = replace_numeric(REFER_C_STR(head_));

  return MAKE_STR_DIRECTLY(str);
}

static char *
find_numeric_conv_method4_mark(const char *cand, int *nth)
{
  int i, len;
  char *p;

  len = strlen(cand);

  p = strstr(cand, "#4");
  if (p) {
    for (i = 0; i < len; i++) {
      if (cand[i] == '#' && isdigit((unsigned char)cand[i + 1])) {
	(*nth)++;
	if (cand[i + 1] == '4')
	  break;
      }
    }
  }
  return p;
}

static int
has_numeric_in_head(uim_lisp head_)
{
  const char *str;
  int i = 0;

  str = REFER_C_STR(head_);

  while (str[i] != '\0') {
    if (isdigit((unsigned char)str[i]))
      return 1;
    i++;
  }

  return 0;
}

static uim_lisp
get_nth(int nth, uim_lisp lst_)
{
  int i;
  /* nth start from 1 */
  for (i = 1; i < nth; i++) {
    if (NULLP(lst_)) {
      return uim_scm_null();
    }
    lst_ = CDR(lst_);
  }
  return CAR(lst_);
}

static int
get_purged_cand_index(struct skk_cand_array *ca)
{
  int i, n = -1;

  if (!ca)
    return -1;

  for (i = 0; i < ca->nr_real_cands; i++) {
    if (is_purged_cand(ca->cands[i])) {
      n = i;
      break;
    }
  }
  return n;
}

static int
get_ignoring_indices(struct skk_cand_array *ca, int indices[])
{
  int i, j, k = 0;
  int purged_cand_index;

  purged_cand_index= get_purged_cand_index(ca);

  if (purged_cand_index != -1) {
    char **purged_words = get_purged_words(ca->cands[purged_cand_index]);
    int nr_purged = nr_purged_words(purged_words);

    indices[k] = purged_cand_index;
    k++;

    for (i = ca->nr_real_cands; i < ca->nr_cands; i++) {
      if (k >= IGNORING_WORD_MAX)
	break;
      for (j = 0; j < nr_purged; j++) {
	if (!strcmp(ca->cands[i], purged_words[j])) {
	  indices[k] = i;
	  k++;
	}
      }
    }
    indices[k] = -1;
    free_allocated_purged_words(purged_words);
  } else {
    indices[0] = -1;
  }
  return k;
}

static uim_lisp
skk_get_nth_candidate(uim_lisp skk_dic_, uim_lisp nth_,
		      uim_lisp head_and_okuri_head_,
		      uim_lisp okuri_,
		      uim_lisp numeric_conv_)
{
  int n;
  struct skk_cand_array *ca, *subca;
  int i, j, k = 0;
  const char *cands = NULL;
  char *p;
  const char *numstr;
  int method_place = 0;
  int sublen, newlen;
  int mark;
  uim_lisp str_ = uim_scm_null();
  uim_lisp numlst_ = uim_scm_null();
  int ignoring_indices[IGNORING_WORD_MAX + 1];
  dic_info *skk_dic = NULL;
  uim_lisp head_ = CAR(head_and_okuri_head_);
  uim_lisp okuri_head_ = CDR(head_and_okuri_head_);

  if (PTRP(skk_dic_))
    skk_dic = C_PTR(skk_dic_);

  if (TRUEP(numeric_conv_))
    numlst_ = skk_store_replaced_numeric_str(head_);

  n = C_INT(nth_);

  if (!NULLP(numlst_))
    ca = find_cand_array_lisp(skk_dic, head_, okuri_head_, okuri_, 0, numeric_conv_);
  else
    ca = find_cand_array_lisp(skk_dic, head_, okuri_head_, okuri_, 0, uim_scm_f());

  get_ignoring_indices(ca, ignoring_indices);

  if (ca) {
    /* handle #4 method of numeric conversion */
    if (!NULLP(numlst_)) {
      for (i = 0; i < ca->nr_cands; i++) {
	if (match_to_discarding_index(ignoring_indices, i))
	  continue;

	if ((p = find_numeric_conv_method4_mark(ca->cands[i], &method_place))) {
	  numstr = REFER_C_STR(get_nth(method_place, numlst_));
	  subca = find_cand_array(skk_dic, numstr, 0, NULL, 0);
	  if (subca) {
	    for (j = 0; j < subca->nr_cands; j++) {
	      if (k == n) {
		char *str;
		str = uim_strdup(ca->cands[i]);
		sublen = strlen(subca->cands[j]);
		newlen = strlen(ca->cands[i]) - 2 + sublen;
		mark = p - ca->cands[i];

		str = uim_realloc(str, newlen + 1);
		memmove(&str[mark + sublen],
			&str[mark + 2],
			newlen - mark - sublen + 1);
		memcpy(&str[mark], subca->cands[j], sublen);

		str_ = MAKE_STR_DIRECTLY(str);
		return skk_merge_replaced_numeric_str(str_, numlst_);
	      }
	      k++;
	    }
	  }
	} else {
	   if (k == n) {
	     cands = ca->cands[i];
	     break;
	   }
	   k++;
	}
      }
    } else {
      for (i = 0; i < ca->nr_cands; i++) {
	if (match_to_discarding_index(ignoring_indices, i))
	  continue;
	if (k == n) {
	  cands = ca->cands[i];
	  break;
	}
	k++;
      }
    }
  }

  /* check non-numeric conversion */
  if (!cands && n >= k && !NULLP(numlst_))
    return skk_get_nth_candidate(skk_dic_, MAKE_INT(n - k),
		    head_and_okuri_head_, okuri_, uim_scm_f());

  if (cands)
    str_ = MAKE_STR(cands);

  if (!NULLP(numlst_))
    return skk_merge_replaced_numeric_str(str_, numlst_);
  else
    return str_;
}

static uim_lisp
skk_get_nr_candidates(uim_lisp skk_dic_, uim_lisp head_, uim_lisp okuri_head_, uim_lisp okuri_, uim_lisp numeric_conv_)
{
  struct skk_cand_array *ca, *subca;
  int n = 0;
  int i, nr_cands = 0;
  const char *numstr;
  int method_place = 0;
  uim_lisp numlst_ = uim_scm_null();
  int ignoring_indices[IGNORING_WORD_MAX + 1];
  dic_info *skk_dic = NULL;

  if (PTRP(skk_dic_))
    skk_dic = C_PTR(skk_dic_);

  if (TRUEP(numeric_conv_))
    numlst_ = skk_store_replaced_numeric_str(head_);

  if (!NULLP(numlst_))
    ca = find_cand_array_lisp(skk_dic, head_, okuri_head_, okuri_, 0, numeric_conv_);
  else
    ca = find_cand_array_lisp(skk_dic, head_, okuri_head_, okuri_, 0, uim_scm_f());

  if (ca)
    n = ca->nr_cands;
  nr_cands = n;
  nr_cands -= get_ignoring_indices(ca, ignoring_indices);

  /* handle #4 method of numeric conversion */
  if (!NULLP(numlst_)) {
    for (i = 0; i < n; i++) {
      if (match_to_discarding_index(ignoring_indices, i))
	continue;

      if (find_numeric_conv_method4_mark(ca->cands[i], &method_place)) {
	numstr = REFER_C_STR(get_nth(method_place, numlst_));
	nr_cands--;
	subca = find_cand_array(skk_dic, numstr, 0, NULL, 0);
	if (subca)
	  nr_cands += subca->nr_cands;
	break;
      }
    }
  }

  /* add non-numeric conversion */
  if (!NULLP(numlst_))
    return MAKE_INT(nr_cands +
		    C_INT(skk_get_nr_candidates(skk_dic_, head_, okuri_head_,
				    okuri_, uim_scm_f())));

  return MAKE_INT(nr_cands);
}

static struct skk_comp_array *
make_comp_array_from_cache(dic_info *di, const char *s, uim_lisp use_look_)
{
  struct skk_line *sl;
  struct skk_comp_array *ca;

  if (!di)
    return NULL;

  ca = uim_malloc(sizeof(struct skk_comp_array));
  ca->nr_comps = 0;
  ca->refcount = 0;
  ca->comps = NULL;
  ca->head = NULL;
  ca->next = NULL;

  /* search from cache */
  for (sl = di->head.next; sl; sl = sl->next) {
    if (/* string 's' is part of sl->head */
	!strncmp(sl->head, s, strlen(s)) && strcmp(sl->head, s) &&
	/* and sl is okuri-nasi line */
	sl->okuri_head == '\0' &&
	/* exclude some entries */
	sl->state & SKK_LINE_USE_FOR_COMPLETION) {
      ca->nr_comps++;
      ca->comps = uim_realloc(ca->comps, sizeof(char *) * ca->nr_comps);
      ca->comps[ca->nr_comps - 1] = uim_strdup(sl->head);
    }
  }

  if (TRUEP(use_look_))
    look_get_comp(ca, s);

  if (ca->nr_comps == 0) {
    free(ca);
    ca = NULL;
  } else {
    ca->head = uim_strdup(s);
    ca->next = skk_comp;
    skk_comp = ca;
  }
  return ca;
}

static struct skk_comp_array *
append_comp_array_from_server(struct skk_comp_array *ca, dic_info *di, const char *s, uim_lisp use_look_)
{
  char r;
  struct skk_line *sl;
  int n = 0, ret, len;
  int i;
  char buf[SKK_SERV_BUFSIZ];
  char *line;
  ssize_t nr;
  struct pollfd pfd[1];

  if (!di) {
    return ca;
  }
  if (!(di->skkserv_state & SKK_SERV_CONNECTED)) {
    if (!((di->skkserv_state |= open_skkserv(di->skkserv_hostname,
					     di->skkserv_portnum,
					     di->skkserv_family)) &
	  SKK_SERV_CONNECTED))
      return ca;
  }

  fprintf(wserv, "4%s \n", s);
  ret = fflush(wserv);
  if (ret != 0 && errno == EPIPE) {
    skkserv_disconnected(di);
    return ca;
  }

  /* check server response to see the capability of completion */
  pfd[0].fd = skkservsock;
  pfd[0].events = POLLIN;
  ret = poll(pfd, 1, di->skkserv_completion_timeout);
  if (ret == -1) {
    skkserv_disconnected(di);
    return ca;
  } else if (ret == 0) {
    uim_notify_info(N_("SKK server without completion capability\n"));
    /* don't try server completion further any more */
    di->skkserv_state &= ~SKK_SERV_TRY_COMPLETION;
    return ca;
  }

  if ((nr = read(skkservsock, &r, 1)) == -1 || nr == 0) {
    skkserv_disconnected(di);
    return ca;
  }

  if (r == '1') {
    char sep = '\0';
    uim_asprintf(&line, "%s ", s);
    while (1) {
      if ((nr = read(skkservsock, &r, 1)) == -1 || nr == 0) {
        skkserv_disconnected(di);
        free(line);
        return ca;
      }

      if (r == '\n') {
        len = strlen(line) + n;
        line = uim_realloc(line, len + 1);
        strlcat(line, buf, len + 1);
        break;
      }

      /* FIXME: should handle word with '/' properly */
      if (n == 0 && sep == '\0') {
	sep = r;
      } else {
	if (sep == ' ' && r == ' ') {
	  r = '/';
	}
      }

      buf[n] = r;
      buf[n + 1] = '\0';
      if (n == SKK_SERV_BUFSIZ - 2) {
        len = strlen(line) + n + 1;
        line = uim_realloc(line, len + 1);
        strlcat(line, buf, len + 1);
        n = 0;
      } else {
        n++;
      }
    }
    sl = compose_line(di, s, '\0', line);
    free(line);

    if (!ca) {
      ca = uim_malloc(sizeof(struct skk_comp_array));
      ca->nr_comps = 0;
      ca->refcount = 0;
      ca->comps = NULL;
      ca->head = NULL;
      ca->next = NULL;
    }
    for (i = 0; i < sl->cands[0].nr_cands; i++) {
      if (strcmp(s, sl->cands[0].cands[i]) != 0) {
        ca->nr_comps++;
        ca->comps = uim_realloc(ca->comps, sizeof(char *) * ca->nr_comps);
        ca->comps[ca->nr_comps - 1] = uim_strdup(sl->cands[0].cands[i]);
      }
    }
    free_skk_line(sl);
    if (ca->nr_comps == 0) {
      free(ca);
      ca = NULL;
    } else if (ca->head == NULL) {
      ca->head = uim_strdup(s);
      ca->next = skk_comp;
      skk_comp = ca;
    }
  } else {
    while ((nr = read(skkservsock, &r, 1)) != -1 && nr != 0 && r != '\n');
  }

  return ca;
}

static struct skk_comp_array *
find_comp_array(dic_info *di, const char *s, uim_lisp use_look_)
{
  struct skk_comp_array *ca;

  if (strlen(s) == 0)
    return NULL;

  for (ca = skk_comp; ca; ca = ca->next) {
    if (!strcmp(ca->head, s))
      break;
  }
  if (ca == NULL) {
    ca = make_comp_array_from_cache(di, s, use_look_);
    if (di->skkserv_state & SKK_SERV_TRY_COMPLETION)
      ca = append_comp_array_from_server(ca, di, s, use_look_);
  }

  return ca;
}

static struct skk_comp_array *
find_comp_array_lisp(dic_info *skk_dic, uim_lisp head_, uim_lisp numeric_conv_, uim_lisp use_look_)
{
  const char *hs;
  struct skk_comp_array *ca;
  char *rs = NULL;

  hs = REFER_C_STR(head_);

  if (TRUEP(numeric_conv_))
    rs = replace_numeric(hs);

  if (!rs)
    ca = find_comp_array(skk_dic, hs, use_look_);
  else {
    ca = find_comp_array(skk_dic, rs, use_look_);
    free(rs);
  }
  return ca;
}

static uim_lisp
skk_get_completion(uim_lisp skk_dic_, uim_lisp head_, uim_lisp numeric_conv_, uim_lisp use_look_)
{
  struct skk_comp_array *ca;
  dic_info *skk_dic = NULL;

  if (PTRP(skk_dic_))
    skk_dic = C_PTR(skk_dic_);

  ca = find_comp_array_lisp(skk_dic, head_, numeric_conv_, use_look_);
  if (ca) {
    ca->refcount++;
    return uim_scm_t();
  }

  if (TRUEP(numeric_conv_) && has_numeric_in_head(head_))
    return skk_get_completion(skk_dic_, head_, uim_scm_f(), use_look_);

  return uim_scm_f();
}

static uim_lisp
skk_get_nth_completion(uim_lisp skk_dic_, uim_lisp nth_, uim_lisp head_,
		uim_lisp numeric_conv_, uim_lisp use_look_)
{
  int n;
  struct skk_comp_array *ca;
  char *str;
  uim_lisp numlst_ = uim_scm_null();
  dic_info *skk_dic = NULL;

  if (PTRP(skk_dic_))
    skk_dic = C_PTR(skk_dic_);

  if (TRUEP(numeric_conv_))
    numlst_ = skk_store_replaced_numeric_str(head_);

  if (!NULLP(numlst_))
    ca = find_comp_array_lisp(skk_dic, head_, numeric_conv_, use_look_);
  else
    ca = find_comp_array_lisp(skk_dic, head_, uim_scm_f(), use_look_);

  if (!ca) {
    if (!NULLP(numlst_))
      return skk_get_nth_completion(skk_dic_, nth_, head_, uim_scm_f(), use_look_);
    else
      return MAKE_STR("");
  }

  n = C_INT(nth_);
  if (ca->nr_comps > n) {
    str = ca->comps[n];
    if (!NULLP(numlst_))
      return restore_numeric(str, numlst_);
    else
      return MAKE_STR(str);
  }

  if (!NULLP(numlst_) && n >= ca->nr_comps)
    return skk_get_nth_completion(skk_dic_, MAKE_INT(n - ca->nr_comps),
		    head_, uim_scm_f(), use_look_);

  return MAKE_STR("");
}

static uim_lisp
skk_get_nr_completions(uim_lisp skk_dic_, uim_lisp head_, uim_lisp numeric_conv_, uim_lisp use_look_)
{
  int n = 0;
  struct skk_comp_array *ca;
  dic_info *skk_dic = NULL;

  if (PTRP(skk_dic_))
    skk_dic = C_PTR(skk_dic_);

  ca = find_comp_array_lisp(skk_dic, head_, numeric_conv_, use_look_);
  if (ca)
    n = ca->nr_comps;

  if (TRUEP(numeric_conv_) && has_numeric_in_head(head_))
    return MAKE_INT(n +
		    C_INT(skk_get_nr_completions(skk_dic_, head_, uim_scm_f(), use_look_))); 

  return MAKE_INT(n);
}

static uim_lisp
skk_clear_completions(uim_lisp head_, uim_lisp numeric_conv_)
{
  int i;
  struct skk_comp_array *ca, *ca_prev;
  const char *hs;
  char *rs = NULL;

  hs = REFER_C_STR(head_);

  if (TRUEP(numeric_conv_))
    rs = replace_numeric(hs);

  if (!rs)
    for (ca = skk_comp; ca; ca = ca->next) {
      if (!strcmp(ca->head, hs)) {
	ca->refcount--;
	break;
      }
    }
  else {
    for (ca = skk_comp; ca; ca = ca->next) {
      if (!strcmp(ca->head, rs)) {
	ca->refcount--;
	break;
      }
    }
    free(rs);
  }

  if (ca && ca->refcount == 0) {
    for (i = 0; i < ca->nr_comps; i++) {
      free(ca->comps[i]);
    }
    free(ca->comps);
    free(ca->head);

    if (ca == skk_comp) {
      skk_comp = ca->next;
      free(ca);
    } else {
      ca_prev = skk_comp;
      while (ca_prev->next != ca) {
	ca_prev = ca_prev->next;
      }
      ca_prev->next = ca->next;
      free(ca);
    }
  }

  if (TRUEP(numeric_conv_) && has_numeric_in_head(head_))
    skk_clear_completions(head_, uim_scm_f());

  return uim_scm_t();
}

static uim_lisp
restore_numeric(const char *s, uim_lisp numlst_)
{
  int i, j, len, newlen, numstrlen;
  const char *numstr;
  char *str;

  str = uim_strdup(s);
  newlen = len = strlen(str);

  for (i = 0, j = 0; j < len; i++, j++) {
    if (str[i] == '#') {
      if (NULLP(numlst_))
	break;

      numstr  = REFER_C_STR(CAR(numlst_));
      numstrlen = strlen(numstr);
      newlen = newlen - 1 + numstrlen;
      str = uim_realloc(str, newlen + 1);
      memmove(&str[i + numstrlen], &str[i + 1], newlen - i - numstrlen + 1);
      memcpy(&str[i], numstr, numstrlen);
      i = i  - 1 + numstrlen;

      numlst_ = CDR(numlst_);
    }
  }
  return MAKE_STR_DIRECTLY(str);
}

static uim_lisp
skk_get_dcomp_word(uim_lisp skk_dic_, uim_lisp head_, uim_lisp numeric_conv_, uim_lisp use_look_)
{
  const char *hs;
  struct skk_line *sl;
  int len;
  uim_lisp numlst_, look_;
  char *rs = NULL;
  dic_info *skk_dic = NULL;

  if (PTRP(skk_dic_))
    skk_dic = C_PTR(skk_dic_);

  numlst_ = uim_scm_null();
  hs = REFER_C_STR(head_);

  if (TRUEP(numeric_conv_))
    numlst_ = skk_store_replaced_numeric_str(head_);

  if (!NULLP(numlst_)) {
    rs = replace_numeric(hs);
    len = strlen(rs);
  } else
    len = strlen(hs);

  if (len != 0) {
    /* Search from cache using same way as in make_comp_array_from_cache(). */
    if (!rs) {
      for (sl = skk_dic->head.next; sl; sl = sl->next) {
	if (!strncmp(sl->head, hs, len) && strcmp(sl->head, hs) &&
			sl->okuri_head == '\0' &&
			sl->state & SKK_LINE_USE_FOR_COMPLETION)
	  return MAKE_STR(sl->head);
      }
      if (TRUEP(use_look_)) {
	look_ = look_get_top_word(hs);
	if (TRUEP(look_))
	  return look_;
      }
    } else {
      for (sl = skk_dic->head.next; sl; sl = sl->next) {
	if (!strncmp(sl->head, rs, len) && strcmp(sl->head, rs) &&
			sl->okuri_head == '\0' &&
			sl->state & SKK_LINE_USE_FOR_COMPLETION) {
	  free(rs);
	  return restore_numeric(sl->head, numlst_);
	}
      }
      if (TRUEP(use_look_)) {
	look_ = look_get_top_word(rs);
	free(rs);
	if (TRUEP(look_))
	  return look_;
      } else {
	free(rs);
      }
      return skk_get_dcomp_word(skk_dic_, head_, uim_scm_f(), use_look_);
    }
  }
  return MAKE_STR("");
}

static void
reorder_candidate(dic_info *skk_dic, struct skk_cand_array *ca, const char *str)
{
  int i;
  int nth = 0;
  char *tmp;
  /* find index of the candidate */
  for (i = 0; i < ca->nr_cands; i++) {
    if (!strcmp(str, ca->cands[i])) {
      nth = i;
      break;
    }
  }

  /* shift array */
  tmp = ca->cands[nth];
  if (nth) {
    for (i = nth; i > 0; i--)
      ca->cands[i] = ca->cands[i - 1];
    ca->cands[0] = tmp;
    skk_dic->cache_modified = 1;
  }
  /* */
  if (nth >= ca->nr_real_cands)
    ca->nr_real_cands++;
}

static void push_purged_word(dic_info *skk_dic, struct skk_cand_array *ca, int nth, int append, char *word)
{
  char *cand = ca->cands[nth];
  int len, oldlen = strlen(cand);
  char *p = sanitize_word(word, NULL);

  if (!p)
    return;

  if (append) {
    /* check whether the word is already registerd */
    char **purged_words = get_purged_words(cand);
    int nr_purged = nr_purged_words(purged_words);
    int j;
    for (j = 0; j < nr_purged; j++) {
      if (!strcmp(purged_words[j], word)) {
	free_allocated_purged_words(purged_words);
	return;
      }
    }
    free_allocated_purged_words(purged_words);

    len = oldlen + strlen(p) + 3;
    cand = uim_realloc(cand, len + 1);
    if (cand) {
      cand[oldlen - 1] = '\0';
      strcat(cand, " \"");
      strcat(cand, p);
      strcat(cand, "\")");
      ca->cands[nth] = cand;
      skk_dic->cache_modified = 1;
    }
  } else {
    len = strlen("(skk-ignore-dic-word \"\")") + strlen(p) + 1;
    cand = uim_realloc(cand, len);
    if (cand) {
      snprintf(cand, len, "(skk-ignore-dic-word \"%s\")", p);
      ca->cands[nth] = cand;
      skk_dic->cache_modified = 1;
    }
  }
}

static void remove_candidate_from_array(dic_info *skk_dic, struct skk_cand_array *ca, int nth)
{
  int i;

  free(ca->cands[nth]);
  for (i = nth; i < ca->nr_cands - 1; i++)
    ca->cands[i] = ca->cands[i + 1];
  if (nth < ca->nr_real_cands)
    ca->nr_real_cands--;
  ca->nr_cands--;
  skk_dic->cache_modified = 1;
}

static void
merge_word_to_real_cand_array(struct skk_cand_array *ca, const char *word)
{
  int i, nth = -1;
  char *tmp;

  push_back_candidate_to_array(ca, word);
  nth = ca->nr_cands - 1;

  /* move word at the end of real cand array */
  tmp = ca->cands[nth];
  if (nth >= ca->nr_real_cands) {
    for (i = nth; i > ca->nr_real_cands; i--)
      ca->cands[i] = ca->cands[i - 1];
    ca->cands[ca->nr_real_cands] = tmp;
    ca->nr_real_cands++;
  }
}

static int exist_in_purged_cand(struct skk_cand_array *ca,
		const char *word)
{
  int i, purged_cand_index;
  char **purged_words;
  int nr_purged;

  purged_cand_index = get_purged_cand_index(ca);
  if (purged_cand_index == -1)
    return 0;

  purged_words = get_purged_words(ca->cands[purged_cand_index]);
  nr_purged = nr_purged_words(purged_words);

  for (i = 0; i < nr_purged; i++) {
    if (!strcmp(purged_words[i], word)) {
      free_allocated_purged_words(purged_words);
      return 1;
    }
  }
  free_allocated_purged_words(purged_words);
  return 0;
}

static int index_in_real_cands(struct skk_cand_array *ca, const char *str)
{
  int i;
  for (i = 0; i < ca->nr_real_cands; i++) {
    if (!strcmp(ca->cands[i], str))
      return i;
  }
  return -1;
}

static void
remove_purged_words_from_dst_cand_array(dic_info *skk_dic,
		struct skk_cand_array *src_ca,
		struct skk_cand_array *dst_ca, const char *purged_cand)
{
  char **purged_words;
  int nr_words;
  int i, j;

  purged_words = get_purged_words(purged_cand);
  nr_words = nr_purged_words(purged_words);

  for (i = 0; i < nr_words; i++) {
    int dup = 0;

    if (index_in_real_cands(src_ca, purged_words[i]) != -1)
      continue;

    for (j = 0; j < dst_ca->nr_real_cands; j++) {
       if (!strcmp(purged_words[i], dst_ca->cands[j])) {
	 dup = 1;
	 break;
       }
    }
    if (dup)
      remove_candidate_from_array(skk_dic, dst_ca, j);
  }
  free_allocated_purged_words(purged_words);
}

static void
merge_purged_cands(dic_info *skk_dic, struct skk_cand_array *src_ca,
		   struct skk_cand_array *dst_ca, int src_nth, int dst_nth)
{
  char *src_cand = src_ca->cands[src_nth];
  char *dst_cand = dst_ca->cands[dst_nth];
  char **dst_purged_words, **src_purged_words;
  int nr_dst_purged_words, nr_src_purged_words;
  int i, j;

  src_purged_words = get_purged_words(src_cand);
  dst_purged_words = get_purged_words(dst_cand);
  nr_src_purged_words = nr_purged_words(src_purged_words);
  nr_dst_purged_words = nr_purged_words(dst_purged_words);

  for (i = 0; i < nr_src_purged_words; i++) {
    int dup = 0;
    for (j = 0; j < nr_dst_purged_words; j++) {
      if (!strcmp(src_purged_words[i], dst_purged_words[j])) {
	dup = 1;
	break;
      }
    }
    if (!dup) {
      push_purged_word(skk_dic, dst_ca, dst_nth, 1, src_purged_words[i]);
      remove_purged_words_from_dst_cand_array(skk_dic, src_ca, dst_ca, src_ca->cands[src_nth]);
    }
  }
  free_allocated_purged_words(dst_purged_words);
  free_allocated_purged_words(src_purged_words);
}

static void
merge_purged_cand_to_dst_array(dic_info *skk_dic,
		struct skk_cand_array *src_ca,
		struct skk_cand_array *dst_ca, char *purged_cand)
{
  remove_purged_words_from_dst_cand_array(skk_dic, src_ca, dst_ca, purged_cand);
  merge_word_to_real_cand_array(dst_ca, purged_cand);
}

static void
merge_word_to_dst_cand_array_with_purged_words(struct skk_cand_array *dst_ca,
		struct skk_cand_array *src_ca, const char *src_cand)
{
  int i, nth;
  char *tmp;

  if (exist_in_purged_cand(dst_ca, src_cand) && !exist_in_purged_cand(src_ca, src_cand))
    return;

  push_back_candidate_to_array(dst_ca, src_cand);
  nth = dst_ca->nr_cands - 1;

  /* move word at the end of real cand array */
  tmp = dst_ca->cands[nth];
  if (nth >= dst_ca->nr_real_cands) {
    for (i = nth; i > dst_ca->nr_real_cands; i--)
      dst_ca->cands[i] = dst_ca->cands[i - 1];
    dst_ca->cands[dst_ca->nr_real_cands] = tmp;
    dst_ca->nr_real_cands++;
  }
}

static void
merge_real_candidate_array(dic_info *skk_dic,
		           struct skk_cand_array *src_ca,
			   struct skk_cand_array *dst_ca)
{
  int i, j;
  int src_nr_real_cands = src_ca->nr_real_cands;
  int dst_nr_real_cands = dst_ca->nr_real_cands;

  if (!src_ca || !dst_ca)
    return ;

  for (i = 0; i < src_nr_real_cands; i++) {
    int dup = 0;
    int src_purged_cand_index = -1;
    int dst_purged_cand_index = -1;

    if (is_purged_cand(src_ca->cands[i]))
      src_purged_cand_index = i;

    for (j = 0; j < dst_nr_real_cands; j++) {
      if (dst_purged_cand_index == -1 && is_purged_cand(dst_ca->cands[j]))
	dst_purged_cand_index = j;
      if (!strcmp(src_ca->cands[i], dst_ca->cands[j]))
	dup = 1;
    }

    if (!dup) {
      /* be careful! */
      if (src_purged_cand_index != -1 && dst_purged_cand_index != -1)
	merge_purged_cands(skk_dic, src_ca, dst_ca, src_purged_cand_index,
			dst_purged_cand_index);
      else if (src_purged_cand_index != -1 && dst_purged_cand_index == -1)
	merge_purged_cand_to_dst_array(skk_dic, src_ca, dst_ca,
			src_ca->cands[src_purged_cand_index]);
      else if (src_purged_cand_index == -1 && dst_purged_cand_index != -1)
	merge_word_to_dst_cand_array_with_purged_words(dst_ca, src_ca,
			src_ca->cands[i]);
      else
	merge_word_to_real_cand_array(dst_ca, src_ca->cands[i]);
    }
  }
}

static uim_lisp
skk_commit_candidate(uim_lisp skk_dic_, uim_lisp head_and_okuri_head_,
		     uim_lisp okuri_, uim_lisp nth_, uim_lisp numeric_conv_)
{
  int nth;
  struct skk_cand_array *ca, *subca;
  char *str = NULL;
  int i, j, k = 0;
  uim_lisp numstr_;
  const char *numstr;
  int method_place = 0;
  uim_lisp numlst_ = uim_scm_null();
  int ignoring_indices[IGNORING_WORD_MAX + 1];
  dic_info *skk_dic = NULL;
  uim_lisp head_ = CAR(head_and_okuri_head_);
  uim_lisp okuri_head_ = CDR(head_and_okuri_head_);

  if (PTRP(skk_dic_))
    skk_dic = C_PTR(skk_dic_);

  if (TRUEP(numeric_conv_))
    numlst_ = skk_store_replaced_numeric_str(head_);

  nth = C_INT(nth_);

  if (!NULLP(numlst_))
    ca = find_cand_array_lisp(skk_dic, head_, okuri_head_, okuri_, 0, numeric_conv_);
  else
    ca = find_cand_array_lisp(skk_dic, head_, okuri_head_, okuri_, 0, uim_scm_f());

  if (!ca) {
    if (!NULLP(numlst_))
      return skk_commit_candidate(skk_dic_, head_and_okuri_head_, okuri_, nth_,
		      uim_scm_f());
    return uim_scm_f();
  }

  get_ignoring_indices(ca, ignoring_indices);

  /* handle #4 method of numeric conversion */
  if (!NULLP(numlst_)) {
    for (i = 0; i < ca->nr_cands; i++) {
      if (match_to_discarding_index(ignoring_indices, i))
	continue;

      if (find_numeric_conv_method4_mark(ca->cands[i], &method_place)) {
	numstr_ = get_nth(method_place, numlst_);
	numstr = REFER_C_STR(numstr_);
	subca = find_cand_array(skk_dic, numstr, 0, NULL, 0);
	if (subca) {
	  for (j = 0; j < subca->nr_cands; j++) {
	    if (k == nth) {
	      str = ca->cands[i];
	      /* reorder sub candidate */
	      skk_commit_candidate(skk_dic_, CONS(numstr_, uim_scm_null()),
			           uim_scm_null(), MAKE_INT(j), uim_scm_f());
	      break;
	    }
	    k++;
	  }
	}
	if (str)
	  break;
      } else {
	if (k == nth) {
	   str = ca->cands[i];
	   break;
	}
	k++;
      }
    }
    if (!str) {
      if (nth >= k)
	return skk_commit_candidate(skk_dic_, head_and_okuri_head_, okuri_,
			MAKE_INT(nth - k), uim_scm_f());
      return uim_scm_f();
    }
  } else {
    for (i = 0; i < ca->nr_cands; i++) {
      if (match_to_discarding_index(ignoring_indices, i))
	continue;
      if (k == nth) {
	str = ca->cands[i];
	break;
      }
      k++;
    }
    if (!str)
      return uim_scm_f();
  }
  reorder_candidate(skk_dic, ca, str);

  if (okuri_ != uim_scm_null()) {
    struct skk_line *sl;
    const char *okuri;
    int found = 0;

    okuri = REFER_C_STR(okuri_);
    sl = ca->line;
    for (i = 1; i < sl->nr_cand_array; i++) {
      if (!strcmp(okuri, sl->cands[i].okuri)) {
	found = 1;
	break;
      }
    }
    if (!found) {
      if (!NULLP(numlst_))
	ca = find_cand_array_lisp(skk_dic, head_, okuri_head_, okuri_, 1, numeric_conv_);
      else
	ca = find_cand_array_lisp(skk_dic, head_, okuri_head_, okuri_, 1, uim_scm_f());
      reorder_candidate(skk_dic, ca, str);
    } else {
      /* also reorder base candidate array */
      reorder_candidate(skk_dic, &sl->cands[0], str);
    }
  }

  ca->line->state = SKK_LINE_NEED_SAVE | SKK_LINE_USE_FOR_COMPLETION;
  move_line_to_cache_head(skk_dic, ca->line);

  return uim_scm_f();
}

static void purge_candidate(dic_info *skk_dic, struct skk_cand_array *ca, int nth)
{
    char *str;
    int i;

    if (nth == -1)
      return;

    str = uim_strdup(ca->cands[nth]);

    if ((i = get_purged_cand_index(ca)) == -1) {
      /* new purged cand in the array */
      push_purged_word(skk_dic, ca, nth, 0, str);
    } else {
      /* append the word to already existing purged cand and remove it own */
      push_purged_word(skk_dic, ca, i, 1, str);
      remove_candidate_from_array(skk_dic, ca, nth);
    }

#if 0
    /* Disabled since we use okuri specific ignoing words */
    if (ca->okuri) {
      /* also purge the word in the base cand array */
      int index = index_in_real_cands(&ca->line->cands[0], str);
      if (index != -1)
	purge_candidate(skk_dic, &ca->line->cands[0], index);
    }
#endif
    free(str);
}

static uim_lisp
skk_purge_candidate(uim_lisp skk_dic_, uim_lisp head_and_okuri_head_,
		    uim_lisp okuri_, uim_lisp nth_, uim_lisp numeric_conv_)
{
  int nth = C_INT(nth_);
  struct skk_cand_array *ca, *subca;
  char *str = NULL;
  int i, j, k = 0;
  uim_lisp numstr_;
  const char *numstr;
  int method_place = 0;
  uim_lisp numlst_ = uim_scm_null();
  int ignoring_indices[IGNORING_WORD_MAX + 1];
  dic_info *skk_dic = NULL;
  uim_lisp head_ = CAR(head_and_okuri_head_);
  uim_lisp okuri_head_ = CDR(head_and_okuri_head_);

  if (PTRP(skk_dic_))
    skk_dic = C_PTR(skk_dic_);

  if (TRUEP(numeric_conv_))
    numlst_ = skk_store_replaced_numeric_str(head_);

  if (!NULLP(numlst_))
    ca = find_cand_array_lisp(skk_dic, head_, okuri_head_, okuri_, 0, numeric_conv_);
  else
    ca = find_cand_array_lisp(skk_dic, head_, okuri_head_, okuri_, 0, uim_scm_f());

  if (!ca) {
    if (!NULLP(numlst_))
      return skk_purge_candidate(skk_dic_, head_and_okuri_head_, okuri_, nth_,
		      uim_scm_f());
    return uim_scm_f(); /* shouldn't happen */
  }

  get_ignoring_indices(ca, ignoring_indices);

  /* handle #4 method of numeric conversion */
  if (!NULLP(numlst_)) {
    for (i = 0; i < ca->nr_cands; i++) {
      if (match_to_discarding_index(ignoring_indices, i))
	continue;

      if (find_numeric_conv_method4_mark(ca->cands[i], &method_place)) {
	numstr_ = get_nth(method_place, numlst_);
	numstr = REFER_C_STR(numstr_);
	subca = find_cand_array(skk_dic, numstr, 0, NULL, 0);
	if (subca) {
	  for (j = 0; j < subca->nr_cands; j++) {
	    if (k == nth) {
	      str = ca->cands[i];
	      /*
	       * don't purge word in sub candidate array
	       * skk_purge_candidate(skk_dic_, numstr_, uim_scm_null(), uim_scm_null(), MAKE_INT(j), uim_scm_null());
	       */
	      break;
	    }
	    k++;
	  }
	}
	if (str)
	  break;
      } else {
	if (k == nth) {
	   str = ca->cands[i];
	   break;
	}
	k++;
      }
    }
    if (!str) {
      if (nth >= k)
	skk_purge_candidate(skk_dic_, head_and_okuri_head_, okuri_,
			MAKE_INT(nth - k), uim_scm_f());
      return uim_scm_f();
    }
  } else {
    for (i = 0; i < ca->nr_cands; i++) {
      if (match_to_discarding_index(ignoring_indices, i))
	continue;
      if (k == nth)
	break;
      k++;
    }
  }
  if (i < ca->nr_real_cands)
    purge_candidate(skk_dic, ca, i);

  return uim_scm_t();
}

static void
learn_word_to_cand_array(dic_info *skk_dic, struct skk_cand_array *ca, const char *word)
{
  int i, nth = -1;
  for (i = 0; i < ca->nr_cands; i++) {
    if (!strcmp(word, ca->cands[i])) {
      nth = i;
      break;
    }
  }
  if (nth == -1)
    push_back_candidate_to_array(ca, word);

  reorder_candidate(skk_dic, ca, word);
  ca->line->state = SKK_LINE_NEED_SAVE | SKK_LINE_USE_FOR_COMPLETION;
}

static char *
quote_word(const char *word, const char *prefix)
{
  char *str;
  const char *p;
  int len;

  if (prefix)
    str = uim_strdup(prefix);
  else
    str = uim_strdup("");

  for (p = word; *p; p++) {
    len = strlen(str);

    switch (*p) {
    case '/':
	    str = uim_realloc(str, len + strlen("\\057") + 1);
	    strcat(str, "\\057");
	    break;
    case '[':
	    str = uim_realloc(str, len + strlen("[") + 1);
	    strcat(str, "[");
	    break;
    case ']':
	    str = uim_realloc(str, len + strlen("]") + 1);
	    strcat(str, "]");
	    break;
    case '\n':
	    str = uim_realloc(str, len + strlen("\\n") + 1);
	    strcat(str, "\\n");
	    break;
    case '\r':
	    str = uim_realloc(str, len + strlen("\\r") + 1);
	    strcat(str, "\\r");
	    break;
    case '\\':
	    str = uim_realloc(str, len + strlen("\\\\") + 1);
	    strcat(str, "\\\\");
	    break;
    case ';':
	    str = uim_realloc(str, len + strlen("\\073") + 1);
	    strcat(str, "\\073");
	    break;
    case '"':
	    str = uim_realloc(str, len + strlen("\\\"") + 1);
	    strcat(str, "\\\"");
	    break;
    default:
	    str = uim_realloc(str, len + 2);
	    str[len] = *p;
	    str[len + 1] = '\0';
	    break;
    }
  }
  len = strlen(str);
  if (prefix) {
    str = uim_realloc(str, len + strlen("\")") + 1);
    strcat(str, "\")");
  }

  return str;
}

static char *
sanitize_word(const char *str, const char *prefix)
{
  const char *p;
  int is_space_only = 1;

  if (!str || !strlen(str)) {
    return NULL;
  }
  for (p = str; *p; p++) {
    switch (*p) {
    case '/':
    case '[':
    case ']':
    case '\n':
    case '\r':
    case '\\':
    case ';':
    case '"':
      return quote_word(str, prefix);
    case ' ':
      break;
    default:
      is_space_only = 0;
      break;
    }
  }
  if (is_space_only)
    return NULL;

  return uim_strdup(str);
}

static uim_lisp
skk_learn_word(uim_lisp skk_dic_, uim_lisp head_and_okuri_head_,
	       uim_lisp okuri_, uim_lisp word_, uim_lisp numeric_conv_)
{
  struct skk_cand_array *ca;
  char *word;
  const char *tmp;
  dic_info *skk_dic = NULL;
  uim_lisp head_ = CAR(head_and_okuri_head_);
  uim_lisp okuri_head_ = CDR(head_and_okuri_head_);

  if (PTRP(skk_dic_))
    skk_dic = C_PTR(skk_dic_);

  tmp = REFER_C_STR(word_);
  word = sanitize_word(tmp, "(concat \"");
  if (!word)
    return uim_scm_f();

  ca = find_cand_array_lisp(skk_dic, head_, okuri_head_, okuri_, 1, numeric_conv_);
  if (ca)
    learn_word_to_cand_array(skk_dic, ca, word);

  tmp = REFER_C_STR(okuri_);
  if (strlen(tmp)) {
    ca = find_cand_array_lisp(skk_dic, head_, okuri_head_, uim_scm_null(), 1, numeric_conv_);
    if (ca)
      learn_word_to_cand_array(skk_dic, ca, word);
  }
  free(word);
  return uim_scm_f();
}

static void
reverse_cache(dic_info *di)
{
  struct skk_line *sl, *prev, *next;

  prev= NULL;
  sl = di->head.next;
  while (sl) {
    next = sl->next;
    sl->next = prev;
    prev = sl;
    sl = next;
  }
  di->head.next = prev;
}

static void
parse_dic_line(dic_info *di, char *line, int is_personal)
{
  char *buf, *sep;
  struct skk_line *sl;
  int i;

  buf = uim_strdup(line);
  sep = strchr(buf, ' ');

  if (!sep || (sep == buf)) {
    free(buf);
    return;
  }

  *sep = '\0';
  if ((!skk_isascii(buf[0]) || buf[0] == '>') && skk_islower(sep[-1])) {
    /* okuri-ari entry */
    char okuri_head = sep[-1];
    sep[-1] = '\0';
    sl = compose_line(di, buf, okuri_head, line);
  } else {
    sl = compose_line(di, buf, 0, line);
  }
  if (is_personal) {
    sl->state = SKK_LINE_NEED_SAVE | SKK_LINE_USE_FOR_COMPLETION;
    /* set nr_real_cands for the candidate array from personal dictionaly */
    for (i = 0; i < sl->nr_cand_array; i++)
      sl->cands[i].nr_real_cands = sl->cands[i].nr_cands;
  } else {
    sl->state = SKK_LINE_USE_FOR_COMPLETION;
  }
  add_line_to_cache_head(di, sl);
  free(buf);
}

static void
write_out_array(FILE *fp, struct skk_cand_array *ca)
{
  int i;
  if (ca->okuri) {
    fprintf(fp, "[%s/", ca->okuri);
    for (i = 0; i < ca->nr_real_cands; i++)
      fprintf(fp, "%s/", ca->cands[i]);
    fprintf(fp, "]/");
  } else {
    for (i = 0; i < ca->nr_real_cands; i++)
      fprintf(fp, "%s/", ca->cands[i]);
  }
}

static void
write_out_line(FILE *fp, struct skk_line *sl)
{
  struct skk_cand_array *ca;
  int i;

  fprintf(fp, "%s", sl->head);
  if (sl->okuri_head) {
    fprintf(fp, "%c /", sl->okuri_head);
  } else {
    fprintf(fp, " /");
  }
  for (i = 0; i < sl->nr_cand_array; i++) {
    ca = &sl->cands[i];
    write_out_array(fp, ca);
  }
  fprintf(fp, "\n");
}

static int
open_lock(const char *name, int type)
{
  int fd;
  struct flock fl;
  char lock_fn[MAXPATHLEN];

  snprintf(lock_fn, sizeof(lock_fn), "%s.lock", name);

  fd = open(lock_fn, O_CREAT | O_RDWR, S_IRUSR | S_IWUSR);
  if (fd == -1)
    return fd;

  fl.l_type = type;
  fl.l_whence = SEEK_SET;
  fl.l_start = 0;
  fl.l_len = 0;
  if (fcntl(fd, F_SETLKW, &fl) == -1) {
    close(fd);
    fd = -1;
  }

  return fd;
}

static void
close_lock(int fd)
{
  struct flock fl;

  if (fd < 0)
    return;

  fl.l_type = F_UNLCK;
  fl.l_whence = SEEK_SET;
  fl.l_start = 0;
  fl.l_len = 0;

  fcntl(fd, F_SETLKW, &fl);
  close(fd);
}

static int
read_dictionary_file(dic_info *di, const char *fn, int is_personal)
{
  struct stat st;
  FILE *fp;
  char buf[4096]; /* XXX */
  int err_flag = 0;
  int lock_fd;

  if (!di)
    return 0;

  lock_fd = open_lock(fn, F_RDLCK);

  if (stat(fn, &st) == -1) {
    close_lock(lock_fd);
    return 0;
  }

  fp = fopen(fn, "r");
  if (!fp) {
    close_lock(lock_fd);
    return 0;
  }

  di->personal_dic_timestamp = st.st_mtime;

  while (fgets(buf, 4096, fp)) { /* XXX */
    int len = strlen(buf);
    if (buf[len - 1] == '\n') {
      if (err_flag == 0) {
	if (buf[0] != ';') {
	  buf[len - 1] = '\0';
	  parse_dic_line(di, buf, is_personal);
	}
      } else {
	/* erroneous line ends here */
	err_flag = 0;
      }
    } else {
      err_flag = 1;
    }
  }
  fclose(fp);
  close_lock(lock_fd);
  reverse_cache(di);
  return 1;
}

static uim_lisp
skk_read_personal_dictionary(uim_lisp skk_dic_, uim_lisp fn_)
{
  const char *fn;
  struct stat st;
  uim_lisp ret;
  dic_info *skk_dic = NULL;

  if (PTRP(skk_dic_))
    skk_dic = C_PTR(skk_dic_);

  fn = REFER_C_STR(fn_);
  ret = (stat(fn, &st) != -1) ? uim_scm_t() : uim_scm_f();

  update_personal_dictionary_cache_with_file(skk_dic, fn, 1);
#if USE_SKK_JISYO_S_BUF
  update_personal_dictionary_cache_with_file(skk_dic, SKK_JISYO_S, 0);
#endif

  return ret;
}

static void push_back_candidate_array_to_sl(struct skk_line *sl,
				      struct skk_cand_array *src_ca)
{
  int i;
  struct skk_cand_array *ca;

  sl->nr_cand_array++;
  sl->cands = uim_realloc(sl->cands,
		  sizeof(struct skk_cand_array) * sl->nr_cand_array);
  ca = &sl->cands[sl->nr_cand_array - 1];
  ca->is_used = src_ca->is_used;
  ca->nr_cands = src_ca->nr_cands;
  ca->cands = uim_malloc(sizeof(char *) * src_ca->nr_cands);
  for (i = 0; i < ca->nr_cands; i++)
    ca->cands[i] = uim_strdup(src_ca->cands[i]);

  ca->nr_real_cands = src_ca->nr_real_cands;
  ca->okuri = uim_strdup(src_ca->okuri);
  ca->line = sl;
}

static void compare_and_merge_skk_line(dic_info *skk_dic,
		                       struct skk_line *dst_sl,
				       struct skk_line *src_sl)
{
  int i, j;
  struct skk_cand_array *dst_ca, *src_ca;

  if (dst_sl == NULL || src_sl == NULL)
    return;

  src_ca = &src_sl->cands[0];
  dst_ca = &dst_sl->cands[0];
  /*
   * check all candidate array since purged words may exist.
   */
  /* if (src_ca->nr_real_cands >= dst_ca->nr_real_cands) */
    merge_real_candidate_array(skk_dic, src_ca, dst_ca);

  for (i = 1; i < src_sl->nr_cand_array; i++) {
    int dup = 0;
    src_ca = &src_sl->cands[i];

    for (j = 1; j < dst_sl->nr_cand_array; j++) {
      dst_ca = &dst_sl->cands[j];
      if (!strcmp(src_ca->okuri, dst_ca->okuri)) {
	dup = 1;
      /* if (src_ca->nr_real_cands >= dst_ca->nr_real_cands) */
	  merge_real_candidate_array(skk_dic, src_ca, dst_ca);
      }
    }
    if (!dup)
      push_back_candidate_array_to_sl(dst_sl, src_ca);
  }

  dst_sl->state |= src_sl->state;
}

/* for merge sort */
static int
compare_entry(struct skk_line *p, struct skk_line *q)
{
  int ret;
  ret = strcmp(p->head, q->head);

  if (ret != 0)
    return ret;
  else
    return p->okuri_head - q->okuri_head;
}

/*
 * Retern lines with differential "midashi-go" between two personal
 * dictionaly caches.  Also merge candidate arrays for line with same
 * "midashi-go".  p and q are needed to be sorted.
 */
static struct skk_line *
cache_line_diffs(dic_info *skk_dic, struct skk_line *p, struct skk_line *q, int *len)
{
  struct skk_line *r, *s, head;
  int cmp;

  for (r = &head; p && q; ) {
    cmp = compare_entry(p, q);
    if (cmp < 0) {
      p = p->next;
    } else if (cmp > 0) {
      s = copy_skk_line(q);
      r->next = s;
      r = s;
      q = q->next;
      (*len)++;
    } else {
      compare_and_merge_skk_line(skk_dic, p, q);
      p = p->next;
      q = q->next;
    }
  }
  while (q) {
    s = copy_skk_line(q);
    r->next = s;
    r = s;
    q = q->next;
    (*len)++;
  }
  r->next = NULL;
  return head.next;
}

/* for merge sort */
static struct skk_line *
lmerge(struct skk_line *p, struct skk_line *q)
{
  struct skk_line *r, head;

  for (r = &head; p && q; ) {
    if (compare_entry(p, q) < 0) {
      r->next = p;
      r = p;
      p = p->next;
    } else {
      r->next = q;
      r = q;
      q = q->next;
    }
  }
  r->next = (p ? p : q);
  return head.next;
}

/* merge sort */
static struct skk_line *
lsort(struct skk_line *p)
{
  struct skk_line *q, *r;

  if (p) {
    q = p;
    for (r = q->next; r && (r = r->next) != NULL; r = r->next)
      q = q->next;
    r = q->next;
    q->next = NULL;
    if (r)
      p = lmerge(lsort(r), lsort(p));
  }
  return p;
}

static void
update_personal_dictionary_cache_with_file(dic_info *skk_dic, const char *fn,
		                           int is_personal)
{
  dic_info *di;
  struct skk_line *sl, *tmp, *diff, **cache_array;
  int i, diff_len = 0;

  di = (dic_info *)uim_malloc(sizeof(dic_info));
  di->cache_len = 0;
  di->head.next = NULL;

  if (!read_dictionary_file(di, fn, is_personal)) {
    free(di);
    return;
  }

  /* If no cache is available, just use new one. */
  if (!skk_dic->head.next) {
    skk_dic->head.next = di->head.next;
    skk_dic->cache_len = di->cache_len;
    skk_dic->cache_modified = di->cache_modified;
    skk_dic->personal_dic_timestamp = di->personal_dic_timestamp;
    free(di);
    return;
  }

  /* keep original sequence of cache */
  cache_array = (struct skk_line **)uim_malloc(sizeof(struct skk_line *)
		  * skk_dic->cache_len);

  i = 0;
  sl = skk_dic->head.next;
  while (sl) {
    cache_array[i] = sl;
    sl = sl->next;
    i++;
  }

  /* get differential lines and merge candidate */
  di->head.next = lsort(di->head.next);
  skk_dic->head.next = lsort(skk_dic->head.next);
  diff = cache_line_diffs(skk_dic, skk_dic->head.next, di->head.next, &diff_len);

  /* revert sequence of the cache */
  if (skk_dic->cache_len) {
    sl = skk_dic->head.next = cache_array[0];
    for (i = 0; i < skk_dic->cache_len - 1; i++) {
      sl->next = cache_array[i + 1];
      sl = sl->next;
    }
    sl->next = NULL;
  }

  if (is_personal) {
    /* prepend differential lines at the top of the cache */
    if (diff != NULL) {
      sl = diff;
      while (sl->next) {
	sl = sl->next;
      }
      sl->next = skk_dic->head.next;
      skk_dic->head.next = diff;
      skk_dic->cache_len += diff_len;
    }
  } else {
    /* append differential lines at the bottom of the cache */
    if (skk_dic->head.next)
      sl->next = diff;
    else
      skk_dic->head.next = diff;
    skk_dic->cache_len += diff_len;
  }

  skk_dic->cache_modified = 1;

  sl = di->head.next;
  while (sl) {
    tmp = sl;
    sl = sl->next;
    free_skk_line(tmp);
  }
  free(di);
  free(cache_array);
}

static uim_lisp
skk_save_personal_dictionary(uim_lisp skk_dic_, uim_lisp fn_)
{
  FILE *fp;
  const char *fn = REFER_C_STR(fn_);
  char tmp_fn[MAXPATHLEN];
  struct skk_line *sl;
  struct stat st;
  int lock_fd = -1;
  mode_t umask_val;
  dic_info *skk_dic = NULL;

  if (PTRP(skk_dic_))
    skk_dic = C_PTR(skk_dic_);

  if (!skk_dic || skk_dic->cache_modified == 0)
    return uim_scm_f();

  if (fn) {
    if (stat(fn, &st) != -1) {
      if (st.st_mtime != skk_dic->personal_dic_timestamp)
	update_personal_dictionary_cache_with_file(skk_dic, fn, 1);
    }

    lock_fd = open_lock(fn, F_WRLCK);

    snprintf(tmp_fn, sizeof(tmp_fn), "%s.tmp", fn);
    umask_val = umask(S_IRGRP | S_IROTH | S_IWGRP | S_IWOTH);
    fp = fopen(tmp_fn, "w");
    umask(umask_val);
    if (!fp)
      goto error;

  } else {
    fp = stdout;
  }

  for (sl = skk_dic->head.next; sl; sl = sl->next) {
    if (sl->state & SKK_LINE_NEED_SAVE)
      write_out_line(fp, sl);
  }

  if (fflush(fp) != 0)
    goto error;

  if (fsync(fileno(fp)) != 0)
    goto error;

  if (fclose(fp) != 0)
    goto error;

  if (rename(tmp_fn, fn) != 0)
    goto error;

  if (stat(fn, &st) != -1) {
    skk_dic->personal_dic_timestamp = st.st_mtime;
    skk_dic->cache_modified = 0;
  }

error:
  close_lock(lock_fd);
  return uim_scm_f();
}

static uim_lisp
skk_get_annotation(uim_lisp str_)
{
  const char *str, *sep;
  uim_lisp res;

  if (str_ == uim_scm_null())
    return uim_scm_null();

  str = REFER_C_STR(str_);
  sep = strrchr(str, ';');
  if (sep && (*(++sep) != '\0')) {
    res = MAKE_STR(sep);
  } else {
    res = MAKE_STR("");
  }
  return res;
}

static uim_lisp
skk_remove_annotation(uim_lisp str_)
{
  char *str, *sep;

  if (str_ == uim_scm_null())
    return uim_scm_null();

  str = C_STR(str_);
  sep = strrchr(str, ';');
  if (sep && (*(sep + 1) != '\0')) {
    *sep = '\0';
  }
  return MAKE_STR_DIRECTLY(str);
}

static char *
eval_candidate_with_concat(const char *cand)
{
  char *p, *q, *str;
  char *expanded_str;
  size_t len;

  if ((p = strstr(cand, "(concat \"")) == NULL)
    return NULL;

  /* check close paren */
  q = strrchr(p, ')');
  if (!q || (strstr(p, "\")") == NULL))
    return NULL;

  /* ignore make-string */
  if (strstr(p, "make-string"))
    return NULL;

  /* get quoted str  */
  len = (q - p + 1) - strlen("(concat \"\")");
  str = uim_malloc(len + 1);
  strlcpy(str, p + strlen("(concat \""), len + 1);

  expanded_str = expand_str(str);
  if (!expanded_str) {
    free(str);
    return NULL;
  }
  
  /* get evaluated candidate */
  len = p - cand + strlen(expanded_str);
  if (len > strlen(str))
    str = uim_realloc(str, len + 1);

  if (p != cand) {
    strlcpy(str, cand, p - cand + 1);
    strcat(str, expanded_str);
  } else {
    strcpy(str, expanded_str);
  }

  free(expanded_str);
  return str;
}

static uim_lisp
skk_eval_candidate(uim_lisp str_)
{
  const char *cand;
  char *str;

  if (str_ == uim_scm_null())
    return uim_scm_null();

  cand = REFER_C_STR(str_);

  /* eval concat only for now */
  str = eval_candidate_with_concat(cand);
  if (!str)
    return str_;

  return MAKE_STR_DIRECTLY(str);
}

/* only for siod */
static uim_lisp
skk_substring(uim_lisp str_, uim_lisp start_, uim_lisp end_)
{
  const char *str;
  char *s;
  int start;
  int end;
  int len;
  int i, j = 0;

  str = REFER_C_STR(str_);
  start = C_INT(start_);
  end = C_INT(end_);

  if (!str || start < 0 || start > end)
    return MAKE_STR("");

  len = strlen(str);

  if (end > len)
    return MAKE_STR("");

  s = uim_malloc(end - start + 1);

  for (i = start; i < end; i++) {
    s[j] = str[i];
    j++;
  }
  s[j] = '\0';
  return MAKE_STR_DIRECTLY(s);
}

static uim_lisp
skk_look_open(uim_lisp fn_)
{
  const char *fn = REFER_C_STR(fn_);

  if (use_look == 1 && skk_look_ctx)
    uim_look_finish(skk_look_ctx);

  if ((skk_look_ctx = uim_look_init()) == NULL) {
    use_look = 0;
    uim_fatal_error("uim_look_init() failed");
    return uim_scm_f();
  }

  if (!uim_look_open_dict(fn, skk_look_ctx)) {
    uim_look_finish(skk_look_ctx);
    skk_look_ctx = NULL;
    use_look = 0;
    return uim_scm_f();
  }

  use_look = 1;
  return uim_scm_t();
}

static uim_lisp
skk_look_close()
{
  if (use_look && skk_look_ctx) {
    uim_look_finish(skk_look_ctx);
    skk_look_ctx = NULL;
    use_look = 0;
  }

  return uim_scm_f();
}

static uim_lisp
look_get_top_word(const char *str)
{
  char buf[512], *dict_str;
  int i = 0;
  size_t len;
  uim_lisp ret_ = uim_scm_f();

  while (str[i] != '\0') {
    if (!skk_isalpha(str[i]))
      return ret_;
    i++;
  }

  if (!use_look)
    return ret_;

  dict_str = uim_strdup(str);

  uim_look_reset(skk_look_ctx);
  if (uim_look(dict_str, skk_look_ctx) != 0) {
    len = strlen(str);
    uim_look_set(skk_look_ctx);
    while (uim_look_get(dict_str, buf, sizeof(buf), skk_look_ctx) != 0) {
      /* don't use the word itself */
      if (strcasecmp(buf, dict_str) != 0) {
	/* overwrite upper and lower case */
	if (len < strlen(buf))
	  memcpy(buf, str, len);
	ret_ = MAKE_STR(buf);
	break;
      }
    }
  }
  free(dict_str);
  return ret_;
}

static void
look_get_comp(struct skk_comp_array *ca, const char *str)
{
  char buf[512], *dict_str;
  int i = 0, nr_pre;
  int *matched;
  size_t len;

  while (str[i] != '\0') {
    if (!skk_isalpha(str[i]))
      return;
    i++;
  }

  if (!use_look)
    return ;

  dict_str = uim_strdup(str);

  uim_look_reset(skk_look_ctx);
  if (uim_look(dict_str, skk_look_ctx) == 0)
    return;

  nr_pre = ca->nr_comps;
  matched = uim_malloc(sizeof(int) * nr_pre);
  for (i = 0; i < nr_pre; i++)
    matched[i] = 0;

  uim_look_set(skk_look_ctx);
  len = strlen(str);
  while (uim_look_get(dict_str, buf, sizeof(buf), skk_look_ctx) != 0) {
    int match = 0;

    /* don't use the word itself */
    if (strcasecmp(buf, dict_str) == 0)
      continue;

    /* overwrite upper and lower case */
    if (len < strlen(buf))
      memcpy(buf, str, len);

    /* skip words already in the cache */
    for (i = 0; i < nr_pre; i++) {
      if (matched[i])
	continue;
      if (!strcasecmp(ca->comps[i], buf)) {
	matched[i] = 1;
	match = 1;
	break;
      }
    }
    if (!match) {
      ca->nr_comps++;
      ca->comps = uim_realloc(ca->comps, sizeof(char *) * ca->nr_comps);
      ca->comps[ca->nr_comps - 1] = uim_strdup(buf);
    }
  }

  free(matched);
  free(dict_str);
}

void
uim_plugin_instance_init(void)
{
  uim_scm_init_proc5("skk-lib-dic-open", skk_dic_open);
  uim_scm_init_proc1("skk-lib-free-dic", skk_free_dic);
  uim_scm_init_proc2("skk-lib-read-personal-dictionary", skk_read_personal_dictionary);
  uim_scm_init_proc2("skk-lib-save-personal-dictionary", skk_save_personal_dictionary);
  uim_scm_init_proc5("skk-lib-get-entry", skk_get_entry);
  uim_scm_init_proc1("skk-lib-store-replaced-numstr", skk_store_replaced_numeric_str);
  uim_scm_init_proc2("skk-lib-merge-replaced-numstr", skk_merge_replaced_numeric_str);
  uim_scm_init_proc1("skk-lib-replace-numeric", skk_replace_numeric);
  uim_scm_init_proc5("skk-lib-get-nth-candidate", skk_get_nth_candidate);
  uim_scm_init_proc5("skk-lib-get-nr-candidates", skk_get_nr_candidates);
  uim_scm_init_proc5("skk-lib-commit-candidate", skk_commit_candidate);
  uim_scm_init_proc5("skk-lib-purge-candidate", skk_purge_candidate);
  uim_scm_init_proc5("skk-lib-learn-word", skk_learn_word);
  uim_scm_init_proc1("skk-lib-get-annotation", skk_get_annotation);
  uim_scm_init_proc1("skk-lib-remove-annotation", skk_remove_annotation);
  uim_scm_init_proc4("skk-lib-get-completion", skk_get_completion);
  uim_scm_init_proc5("skk-lib-get-nth-completion", skk_get_nth_completion);
  uim_scm_init_proc4("skk-lib-get-nr-completions", skk_get_nr_completions);
  uim_scm_init_proc2("skk-lib-clear-completions", skk_clear_completions);
  uim_scm_init_proc4("skk-lib-get-dcomp-word", skk_get_dcomp_word);
  uim_scm_init_proc1("skk-lib-eval-candidate", skk_eval_candidate);
  uim_scm_init_proc3("skk-lib-substring", skk_substring);
  uim_scm_init_proc1("skk-lib-look-open", skk_look_open);
  uim_scm_init_proc0("skk-lib-look-close", skk_look_close);
}

void
uim_plugin_instance_quit(void)
{
}

/* skkserv related */
static int
open_skkserv(const char *hostname, int portnum, int family)
{
  int sock = -1;
  struct addrinfo hints, *aitop, *ai;
  char port[BUFSIZ];
  int error;
  int enable_completion;

  (void)snprintf(port, sizeof(port), "%d", portnum);

  memset(&hints, 0, sizeof(hints));
  hints.ai_family = family;
  hints.ai_flags = AI_PASSIVE;
  hints.ai_socktype = SOCK_STREAM;

  if ((error = getaddrinfo(hostname, port, &hints, &aitop))) {
    uim_notify_fatal("uim-skk: %s", gai_strerror(error));
    return 0;
  }

  for (ai = aitop; ai; ai = ai->ai_next) {
    if (ai->ai_family != AF_INET && ai->ai_family != AF_INET6)
      continue;

    if ((sock = socket(ai->ai_family, ai->ai_socktype, ai->ai_protocol)) < 0)
      continue;

    if (connect(sock, ai->ai_addr, ai->ai_addrlen) == 0)
      break;

    close(sock);
    sock = -1;
  }

  freeaddrinfo(aitop);

  if (sock == -1) {
    uim_notify_fatal(_("uim-skk: connect to %s port %s failed"), hostname, port);
    return 0;
  }

#if 0
  uim_notify_info("uim-skk: SKKSERVER=%s", hostname);
#endif
  skkservsock = sock;
  rserv = fdopen(sock, "r");
  wserv = fdopen(sock, "w");

  enable_completion =
    uim_scm_symbol_value_bool("skk-skkserv-enable-completion?") ?
      SKK_SERV_TRY_COMPLETION : 0;
  return SKK_SERV_CONNECTED | enable_completion;
}

static void
close_skkserv()
{
  if (skkservsock >= 0) {
    fprintf(wserv, "0\n");
    fflush(wserv);
    close(skkservsock);
    skkservsock = -1;
  }
}

static void
reset_is_used_flag_of_cache(dic_info *di)
{
  struct skk_line *sl;
  int i;

  sl = di->head.next;
  while (sl) {
    for (i = 0; i < sl->nr_cand_array; i++) {
      struct skk_cand_array *ca = &sl->cands[i];
      ca->is_used = 0;
    }
    sl = sl->next;
  }
}

static void
skkserv_disconnected(dic_info *di)
{
  di->skkserv_state &= ~SKK_SERV_CONNECTED;
  reset_is_used_flag_of_cache(di);
}
