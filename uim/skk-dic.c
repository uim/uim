/*

  Copyright (c) 2003-2005 uim Project http://uim.freedesktop.org/

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
#include <sys/types.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>

#include "uim-scm.h"
#include "context.h"
#include "plugin.h"

/*
 * cand : candidate
 */


/* candidate array for each okurigana
 *
 * |C0|C1| .. |Cnr_real_cands| ..              |Cnr_cands|
 * <-------should be saved --><-- cache of master dict -->
 */
struct skk_cand_array {
  /* okurigana string */
  char *okuri;

  int nr_cands;/* length of cands array allocated */
  int nr_real_cands;/* length of read from file part */
  /* candidate string */
  char **cands;

  /* this array was used and merged with okuri-nasi entry array */
  int is_used;
  /* link to its parent line */
  struct skk_line *line;
};

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
  /* modified or read from file */
  int need_save;
  /* link to next entry in the list */
  struct skk_line *next;
};

/* skk dictionary file */
static struct dic_info {
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
  /* timestamp of personal dictonary */
  time_t personal_dic_timestamp;
  /* whether cached lines are modified or not */
  int cache_modified;
  /* length of cached lines */
  int cache_len;
} *skk_dic;

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
  if (!b) {
    return 0;
  }
  /* check previous character */
  b--;
  if (isalpha((unsigned char)*b)) {
    return 1;
  }
  return 0;
}


static int
find_first_line(struct dic_info *di)
{
  char *s = di->addr;
  int off = 0;
  while ( off < di->size && s[off] == ';' ) {
    int l = calc_line_len(&s[off]);
    off += l + 1;
  }
  return off;
}

static int
find_border(struct dic_info *di, int size)
{
  char *s = di->addr;
  int off = 0;
  while (off < size) {
    int l = calc_line_len(&s[off]);
    if (s[off] == ';') {
      off += l + 1;
      continue;
    }
    if (!is_okuri(&s[off])) {
      return off;
    }
    off += l + 1;
  }
  /* every entry is okuri-ari, it may not happen. */
  return size - 1;
}

static struct dic_info *
open_dic(const char *fn)
{
  struct dic_info *di;
  struct stat st;
  int fd;
  void *addr = NULL;
  int success = 0;

  fd = open(fn, O_RDONLY);
  if (fd != -1) {
    if (fstat(fd, &st) != -1) {
      addr = mmap(0, st.st_size, PROT_READ, MAP_SHARED, fd, 0);
      if (addr != MAP_FAILED) {
	success = 1;
      }
    }
  }
  close(fd);
  di = (struct dic_info *)malloc(sizeof(struct dic_info));
  di->addr = success ? addr : NULL;
  di->size = success ? st.st_size : 0;
  di->first = success ? find_first_line(di) : 0;
  di->border = success ? find_border(di, st.st_size) : 0;
  di->head.next = NULL;
  di->personal_dic_timestamp = 0;
  di->cache_modified = 0;
  di->cache_len = 0;
  return di;
}

static const char *
find_line(struct dic_info *di, int off)
{
  char *ptr = di->addr;
  while (off > 0 && (ptr[off] != '\n' || ptr[off+1] == ';')) {
    off --;
  }
  if (off) {
    off ++;
  }
  return &ptr[off];
}

static char *
extract_line_index(struct dic_info *di, int off, char *buf, int len)
{
  const char *p = find_line(di, off);
  int i;
  if (p[0] == ';') {
    return NULL;
  }
  for (i = 0; i < len && p[i] != ' '; i++) {
    buf[i] = p[i];
  }
  buf[i] = 0;
  return buf;
}

static int
do_search_line(struct dic_info *di, const char *s, int min,
	       int max, int d)
{
  char buf[256];
  char *r;
  int idx = (min + max)/2;
  int c = 0;

  if (abs(max-min) < 4) {
    return -1;
  }
  r = extract_line_index(di, idx, buf, 256);
  if (r) {
    c = strcmp(s, r);
  } else {
    return -1;
  }

  if (!c) {
    return idx;
  }
  if (c * d> 0) {
    return do_search_line(di, s, idx, max, d);
  } else {
    return do_search_line(di, s, min, idx, d);
  }
  return -1;
}

/* This funciton name is temporary. I want a better name. */
static char *
first_space(char *str)
{
  while (*str && (*str != ' ')) {
    str++;
  }
  return str;
}

static char *
next_cand_slash(char *str)
{
  int p = 0;
  int i = 0;
  while (*str && (*str != '/' || p == 1)) {
    if (*str == '[' && i == 0) {
      p = 1;
    }
    if (p == 1 && *str == ']' && *(str + 1) == '/') {
      p = 0;
    }
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

  p = strdup(str);
  term = next_slash_in_bracket(p);
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
    if (*str == '/') {
      str++;
    }
  }
  if (!str) {
    return NULL;
  }
  if (*str == '/') {
    str++;
  }
  p = strdup(str);
  term = next_cand_slash(p);
  *term = '\0';
  return p;
}

static uim_lisp
skk_dic_open(uim_lisp fn_)
{
  const char *fn = uim_scm_refer_c_str(fn_);
  if (!skk_dic) {
    skk_dic = open_dic(fn);
  }
  return uim_scm_f();
}

static void
free_skk_line(struct skk_line *sl)
{
  int i, j;
  if (!sl) {
    return ;
  }
  for (i = 0; i < sl->nr_cand_array; i++) {
    struct skk_cand_array *ca = &sl->cands[i];
    for (j = 0; j < ca->nr_cands; j++) {
      free(ca->cands[j]);
    }
    free(ca->okuri);
    free(ca->cands);
  }
  free(sl->head);
  free(sl->cands);
}

static struct skk_cand_array *
find_candidate_array_from_line(struct skk_line *sl, const char *okuri,
			       int create_if_notfound)
{
  int i;
  struct skk_cand_array *ca;
  if (!okuri || !strlen(okuri)) {
    return &sl->cands[0];
  }
  for (i = 1; i < sl->nr_cand_array; i++) {
    if (okuri && !strcmp(okuri, sl->cands[i].okuri)) {
      return &sl->cands[i];
    }
  }
  if (!create_if_notfound) {
    return &sl->cands[0];
  }
  /* allocate now */
  sl->nr_cand_array ++;
  sl->cands = realloc(sl->cands,
		      sizeof(struct skk_cand_array) * sl->nr_cand_array);
  ca = &sl->cands[sl->nr_cand_array - 1];
  ca->is_used = 0;
  ca->cands = NULL;
  ca->nr_cands = 0;
  ca->nr_real_cands = 0;
  ca->okuri = strdup(okuri);
  ca->line = sl;
  return ca;
}

static void
push_back_candidate_to_array(struct skk_cand_array *ca, char *cand)
{
  ca->nr_cands++;
  if (ca->cands)
    ca->cands = realloc(ca->cands, sizeof(char *) * ca->nr_cands);
  else
    ca->cands = malloc(sizeof(char *));
  ca->cands[ca->nr_cands - 1] = strdup(cand);
}

static void
merge_base_candidates_to_array(struct skk_line *sl,
			       struct skk_cand_array *dst_ca)
{
  int i, j;
  struct skk_cand_array *src_ca;
  if (!sl) {
    return ;
  }
  src_ca = &sl->cands[0];
  if (src_ca == dst_ca) {
    return ;
  }
  for (i = 0; i < src_ca->nr_cands; i++) {
    int dup = 0;
    for (j = 0; j < dst_ca->nr_cands; j++) {
      if (!strcmp(src_ca->cands[i], dst_ca->cands[j])) {
	dup = 1;
      }
    }
    if (!dup) {
      push_back_candidate_to_array(dst_ca, src_ca->cands[i]);
    }
  }
}

static void
compose_line_parts(struct dic_info *di, struct skk_line *sl,
		   char *okuri, char *line)
{
  int nth;
  char *tmp;
  struct skk_cand_array *ca = find_candidate_array_from_line(sl, okuri, 1);

  nth = 0;
  do {
    tmp = nth_candidate(line, nth);
    if (tmp && strlen(tmp)) {
      if (tmp[0] == '[') {
	tmp[0] = ' '; /* create first_space */
	compose_line_parts(di, sl, okuri_in_bracket(&tmp[1]), &tmp[0]);
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
  sl = malloc(sizeof(struct skk_line));
  sl->need_save = 0;
  sl->head = strdup(word);
  sl->okuri_head = okuri_head;
  sl->nr_cand_array = 1;
  sl->cands = malloc(sizeof(struct skk_cand_array));
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

  sl = malloc(sizeof(struct skk_line));
  sl->need_save = p->need_save;
  sl->head = strdup(p->head);
  sl->okuri_head = p->okuri_head;
  sl->nr_cand_array = p->nr_cand_array;
  sl->cands = malloc(sizeof(struct skk_cand_array) * sl->nr_cand_array);
  for (i = 0; i < sl->nr_cand_array; i++) {
    struct skk_cand_array *ca = &sl->cands[i];
    struct skk_cand_array *q = &p->cands[i];

    ca->okuri = q->okuri ? strdup(q->okuri) : NULL;
    ca->nr_cands = q->nr_cands;
    ca->nr_real_cands = q->nr_real_cands;
    ca->cands = malloc(sizeof(char *) * ca->nr_cands);
    for (j = 0; j < ca->nr_cands; j++) {
      ca->cands[j] = strdup(q->cands[j]);
    }
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
compose_line(struct dic_info *di, const char *word, char okuri_head, char *entry)
{
  struct skk_line *sl;

  sl = alloc_skk_line(word, okuri_head);

  /* parse */
  compose_line_parts(di, sl, NULL, entry);

  return sl;
}

static void
add_line_to_cache_head(struct dic_info *di, struct skk_line *sl)
{
  sl->next = di->head.next;
  di->head.next = sl;

  di->cache_len++;
  di->cache_modified = 1;
}

static void
move_line_to_cache_head(struct dic_info *di, struct skk_line *sl)
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
add_line_to_cache_last(struct dic_info *di, struct skk_line *sl)
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
skk_search_line_from_file(struct dic_info *di, const char *s, char okuri_head)
{
  int n;
  const char *p;
  int len;
  char *line;
  char *idx = alloca(strlen(s) + 2);
  struct skk_line *sl;

  if (!di->addr) {
    return NULL;
  }
  sprintf(idx, "%s%c",s, okuri_head);
  if (okuri_head) {
    n = do_search_line(di, idx, di->first, di->border - 1, -1);
  } else {
    n = do_search_line(di, idx, di->border, di->size - 1, 1);
  }
  if (n == -1) {
    return NULL;
  }

  p = find_line(di, n);
  len = calc_line_len(p);
  line = malloc(len+1);
  line[0] = 0;
  strncat(line, p, len);
  sl = compose_line(di, s, okuri_head, line);
  free(line);
  return sl;
}

static struct skk_line *
skk_search_line_from_cache(struct dic_info *di, const char *s, char okuri_head)
{
  struct skk_line *sl;

  if (!di) {
    return NULL;
  }
  /* search from cache */
  for (sl = di->head.next; sl; sl = sl->next) {
    if (!strcmp(sl->head, s) &&
	sl->okuri_head == okuri_head) {
      return sl;
    }
  }
  return NULL;
}


static struct skk_cand_array *
find_cand_array(struct dic_info *di, const char *s,
		char okuri_head, const char *okuri,
		int create_if_not_found)
{
  struct skk_line *sl, *sl_file;
  struct skk_cand_array *ca;
  int from_file = 0;

  sl = skk_search_line_from_cache(di, s, okuri_head);
  if (!sl) {
    sl = skk_search_line_from_file(di, s, okuri_head);
    if (!sl) {
      if (!create_if_not_found) {
	return NULL;
      }
      /**/
      sl = alloc_skk_line(s, okuri_head);
    }
    from_file = 1;
    add_line_to_cache_head(di, sl);
  }

  ca = find_candidate_array_from_line(sl, okuri, create_if_not_found);

  if (!ca->is_used) {
    merge_base_candidates_to_array(sl, ca);
    ca->is_used = 1;
    if (!from_file) {
      sl_file = skk_search_line_from_file(di, s, okuri_head);
      merge_base_candidates_to_array(sl_file, ca);
      free_skk_line(sl_file);
    }
  }

  return ca;
}

static struct skk_cand_array *
find_cand_array_lisp(uim_lisp head_, uim_lisp okuri_head_, uim_lisp okuri_,
		     int create_if_not_found)
{
  char o;
  const char *hs;
  const char *okuri = NULL;
  struct skk_cand_array *ca;

  hs = uim_scm_refer_c_str(head_);
  if (okuri_ != uim_scm_null_list()) {
    okuri = uim_scm_refer_c_str(okuri_);
  }
  if (okuri_head_ == uim_scm_null_list()) {
    o = 0;
  } else {
    const char *os= uim_scm_refer_c_str(okuri_head_);
    o = os[0];
  }

  ca = find_cand_array(skk_dic, hs, o, okuri, create_if_not_found);
  return ca;
}


static uim_lisp
skk_get_entry(uim_lisp head_, uim_lisp okuri_head_, uim_lisp okuri_)
{
  struct skk_cand_array *ca;
  ca = find_cand_array_lisp(head_, okuri_head_, okuri_, 0);
  if (ca) {
    return uim_scm_t();
  }
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
  uim_lisp lst = uim_scm_null_list();

  str = uim_scm_refer_c_str(head_);
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
	  numstr = malloc(numlen + 1);
	else
	  numstr = realloc(numstr, numlen + 1);
	strncpy(numstr, &str[start], numlen);
	numstr[numlen] = '\0';
	lst = uim_scm_cons(uim_scm_make_str(numstr), lst);
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
      numstr = malloc(numlen + 1);
    else
      numstr = realloc(numstr, numlen + 1);
    strncpy(numstr, &str[start], numlen);
    numstr[numlen] = '\0';
    lst = uim_scm_cons(uim_scm_make_str(numstr), lst);
  }
  free(numstr);
 
  return uim_scm_reverse(lst);
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
  mbstr = malloc(len * 2 + 1);

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
    return strdup(numstr);

  mbstr = malloc(len * 2 + 1);
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
	if (position >= 4) {
	  if ((position % 4) == 0 && !head_is_zero) {
	    i++;
	    mblen += 2;
	    if (mblen > len * 2)
	      mbstr = realloc(mbstr, mblen + 2);
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
	    mbstr = realloc(mbstr, mblen + 2);
	  strcpy(&mbstr[i * 2], kanji_num_position_list[position % 4]);
	}
      }

      /* add position */
      if (kanji_num_position_list[position]) {
	i++;
	mblen += 2;
	if (mblen > len * 2)
	  mbstr = realloc(mbstr, mblen + 2);
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
    return strdup(numstr);

  mbstr = malloc(len * 2 + 1);
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
	if (position >= 4) {
	  if ((position % 4) == 0 && !head_is_zero) {
	    i++;
	    mblen += 2;
	    if (mblen > len * 2)
	      mbstr = realloc(mbstr, mblen + 2);
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
	    mbstr = realloc(mbstr, mblen + 2);
	  strcpy(&mbstr[i * 2], kanji_check_num_position_list[position % 4]);
	}
      }

      /* add position */
      if (kanji_check_num_position_list[position]) {
	i++;
	mblen += 2;
	if (mblen > len * 2)
	  mbstr = realloc(mbstr, mblen + 2);
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
    return strdup(numstr);
    
  mbstr = malloc(5);
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
    ret = strdup(numstr);
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
    ret = strdup(numstr);
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
  uim_lisp merged_str;

  if (str_ == uim_scm_null_list())
    return uim_scm_null_list();

  str = uim_scm_c_str(str_);
  len = strlen(str);
  newlen = len;

  for (i = 0, j = 0; j < len; i++, j++) {
    if (str[i] == '#') {
      method = str[i + 1] - '0';
      if (uim_scm_nullp(numlst_))
	 break;

      numstr = uim_scm_refer_c_str(uim_scm_car(numlst_));

      convstr = numeric_convert(numstr, method);
      convlen = strlen(convstr);

      newlen = newlen - 2 + convlen;
      str = realloc(str, newlen + 1);
      memmove(&str[i + convlen], &str[i + 2], newlen - i - convlen + 1);
      memcpy(&str[i], convstr, convlen);
      i = i - 2 + convlen;

      numlst_ = uim_scm_cdr(numlst_);
    }
  }

  merged_str = uim_scm_make_str(str);
  free(str);
  return merged_str;
}

static uim_lisp
skk_replace_numeric(uim_lisp head_)
{
  char *str;
  int prev_is_num = 0;
  int i, j, len, newlen;
  uim_lisp result;

  str = uim_scm_c_str(head_);
  len = strlen(str);
  newlen = len;

  for (i = 0, j = 0; j < len; i++, j++) {
    if (isdigit((unsigned char)str[i])) {
      if (prev_is_num == 0) {
	str[i] = '#';
      } else {
	memmove(&str[i], &str[i + 1], newlen - i);
	newlen--;
	i--;
      }
      prev_is_num = 1;
    } else {
      prev_is_num = 0;
    }
  }
  result = uim_scm_make_str(str);
  free(str);
  return result;
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

static uim_lisp
get_nth(int nth, uim_lisp lst_)
{
  int i;
  for (i = 1; i < nth; i++) {
    if (uim_scm_nullp(lst_)) {
      return uim_scm_null_list();
    }
    lst_ = uim_scm_cdr(lst_);
  }
  return uim_scm_car(lst_);
}

static uim_lisp
skk_get_nth_candidate(uim_lisp nth_, uim_lisp head_, uim_lisp okuri_head_, uim_lisp okuri_,
		      uim_lisp numlst_)
{
  int n;
  struct skk_cand_array *ca, *subca;
  int i, j, k = 0;
  char *cands = NULL;
  char *p;
  const char *numstr;
  int method_place = 0;
  int sublen, newlen;
  int mark;
  uim_lisp str_ = uim_scm_null_list();

  n = uim_scm_c_int(nth_);
  ca = find_cand_array_lisp(head_, okuri_head_, okuri_, 0);

  if (ca) {
    /* handle #4 method of numeric conversion */
    if (!uim_scm_nullp(numlst_)) {
      for (i = 0; i < ca->nr_cands; i++) {
	if ((p = find_numeric_conv_method4_mark(ca->cands[i], &method_place))) {
	  numstr = uim_scm_refer_c_str(get_nth(method_place, numlst_)); 
	  subca = find_cand_array(skk_dic, numstr, 0, NULL, 0);
	  if (subca) {
	    for (j = 0; j < subca->nr_cands; j++) {
	      if (k == n) {
		cands = strdup(ca->cands[i]);
		sublen = strlen(subca->cands[j]);
		newlen = strlen(ca->cands[i]) - 2 + sublen;
		mark = p - ca->cands[i];

		cands = realloc(cands, newlen + 1);
		memmove(&cands[mark + sublen],
			&cands[mark + 2],
			newlen - mark - sublen + 1);
		memcpy(&cands[mark], subca->cands[j], sublen);

		str_ = uim_scm_make_str(cands);
		free(cands);
		return str_;
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
	if (ca->nr_cands > n)
	  cands = ca->cands[n];
    }

  }

  if (cands)
    str_ = uim_scm_make_str(cands);
  return str_;
}

static uim_lisp
skk_get_nr_candidates(uim_lisp head_, uim_lisp okuri_head_, uim_lisp okuri_, uim_lisp numlst_)
{
  struct skk_cand_array *ca, *subca;
  int n = 0;
  int i, nr_cands = 0;
  const char *numstr;
  int method_place = 0;

  ca = find_cand_array_lisp(head_, okuri_head_, okuri_, 0);
  if (ca) {
    n = ca->nr_cands;
  }
  nr_cands = n;

  /* handle #4 method of numeric conversion */
  if (!uim_scm_nullp(numlst_)) {
    for (i = 0; i < n; i++) {
      if (find_numeric_conv_method4_mark(ca->cands[i], &method_place)) {
	numstr = uim_scm_refer_c_str(get_nth(method_place, numlst_)); 
	nr_cands--;
	subca = find_cand_array(skk_dic, numstr, 0, NULL, 0);
	if (subca)
	  nr_cands += subca->nr_cands;
	break;
      }
    }
  }
  return uim_scm_make_int(nr_cands);
}

static struct skk_comp_array *
skk_make_comp_array_from_cache(struct dic_info *di, const char *s)
{
  struct skk_line *sl;
  struct skk_comp_array *ca;

  if (!di) {
    return NULL;
  }
  ca = malloc(sizeof(struct skk_comp_array));
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
        (sl->okuri_head == '\0')) {
      ca->nr_comps++;
      ca->comps = realloc(ca->comps, sizeof(char *) * ca->nr_comps);
      ca->comps[ca->nr_comps - 1] = strdup(sl->head);
    }
  }

  if (ca->nr_comps == 0) {
    free(ca);
    ca = NULL;
  } else {
    ca->head = strdup(s);
    ca->next = skk_comp;
    skk_comp = ca;
  }
  return ca;
}

static struct skk_comp_array *
find_comp_array(struct dic_info *di, const char *s)
{
  struct skk_comp_array *ca;

  if (strlen(s) == 0)
    return NULL;

  for (ca = skk_comp; ca; ca = ca->next) {
    if (!strcmp(ca->head, s))
      break;
  }
  if (ca == NULL) {
    ca = skk_make_comp_array_from_cache(skk_dic, s);
  }

  return ca;
}

static struct skk_comp_array *
find_comp_array_lisp(uim_lisp head_)
{
  const char *hs;
  struct skk_comp_array *ca;
  
  hs = uim_scm_refer_c_str(head_);
  ca = find_comp_array(skk_dic, hs);
  return ca;
}

static uim_lisp
skk_get_completion(uim_lisp head_)
{
  struct skk_comp_array *ca;
  ca = find_comp_array_lisp(head_);
  if (ca) {
    ca->refcount++;
    return uim_scm_t();
  }
  return uim_scm_f();
}

static uim_lisp
skk_get_nth_completion(uim_lisp nth_, uim_lisp head_)
{
  int n;
  struct skk_comp_array *ca;
  char *str;

  ca = find_comp_array_lisp(head_);
  n = uim_scm_c_int(nth_);
  if (ca && ca->nr_comps > n) {
    str = ca->comps[n];
    return uim_scm_make_str(str);
  }
  return uim_scm_null_list();
}

static uim_lisp
skk_get_nr_completions(uim_lisp head_)
{
  int n = 0;
  struct skk_comp_array *ca;

  ca = find_comp_array_lisp(head_);
  if (ca) {
    n = ca->nr_comps;
  }
  return uim_scm_make_int(n);
}

static uim_lisp
skk_clear_completions(uim_lisp head_)
{
  int i;
  struct skk_comp_array *ca, *ca_prev;
  const char *hs;

  hs = uim_scm_refer_c_str(head_);
  for (ca = skk_comp; ca; ca = ca->next) {
    if (!strcmp(ca->head, hs)) {
      ca->refcount--;
      break;
    }
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
  return uim_scm_t();
}

static void
reorder_candidate(struct skk_cand_array *ca, char *str)
{
  int i;
  int nth = 0;
  char *tmp;
  /* find index of the candidate */
  for (i = 0; i < ca->nr_cands; i++) {
    if (!strcmp(str, ca->cands[i])) {
      nth = i;
    }
  }

  /* shift array */
  tmp = ca->cands[nth];
  if (nth) {
    for (i = nth; i > 0; i--) {
      ca->cands[i] = ca->cands[i - 1];
    }
    ca->cands[0] = tmp;
    skk_dic->cache_modified = 1;
  }
  /**/
  if (nth >= ca->nr_real_cands) {
    ca->nr_real_cands ++;
  }
}

static void
merge_word_to_cand_array(struct skk_cand_array *ca, char *word)
{
  int i, nth = -1;
  char *tmp;
  for (i = 0; i < ca->nr_cands; i++) {
    if (!strcmp(word, ca->cands[i])) {
      nth = i;
    }
  }
  if (nth == -1) {
    push_back_candidate_to_array(ca, word);
    nth = ca->nr_cands - 1;
  }

  /* move word at the end of real cand array */
  tmp = ca->cands[nth];
  if (nth >= ca->nr_real_cands) {
    for (i = nth; i > ca->nr_real_cands; i--) {
      ca->cands[i] = ca->cands[i - 1];
    }
    ca->cands[ca->nr_real_cands] = tmp;
    ca->nr_real_cands++;
  }
}

static void
merge_real_candidate_array(struct skk_cand_array *src_ca,
			   struct skk_cand_array *dst_ca)
{
  int i, j;
  if (!src_ca || !dst_ca) {
    return ;
  }
  for (i = 0; i < src_ca->nr_real_cands; i++) {
    int dup = 0;
    for (j = 0; j < dst_ca->nr_real_cands; j++) {
      if (!strcmp(src_ca->cands[i], dst_ca->cands[j]))
	dup = 1;
    }
    if (!dup)
      merge_word_to_cand_array(dst_ca, src_ca->cands[i]);
  }
}

static uim_lisp
skk_commit_candidate(uim_lisp head_, uim_lisp okuri_head_,
		     uim_lisp okuri_, uim_lisp nth_, uim_lisp numlst_)
{
  int nth;
  struct skk_cand_array *ca, *subca;
  char *str = NULL;
  int i, j, k = 0;
  int nr_cands = 0;
  uim_lisp numstr_;
  const char *numstr;
  int method_place = 0;

  nth = uim_scm_c_int(nth_);

  ca = find_cand_array_lisp(head_, okuri_head_, okuri_, 0);
  if (!ca) {
    return uim_scm_f();
  }

  nr_cands = ca->nr_cands;

  /* handle #4 method of numeric conversion */
  if (!uim_scm_nullp(numlst_)) {
    for (i = 0; i < ca->nr_cands; i++) {
      if (find_numeric_conv_method4_mark(ca->cands[i], &method_place)) {
	numstr_ = get_nth(method_place, numlst_); 
	numstr = uim_scm_refer_c_str(numstr_);
	subca = find_cand_array(skk_dic, numstr, 0, NULL, 0);
	if (subca) {
	  nr_cands += subca->nr_cands;
	  for (j = 0; j < subca->nr_cands; j++) {
	    if (k == nth) {
	      str = ca->cands[i];
	      /* reorder sub candidate */
	      skk_commit_candidate(numstr_, uim_scm_null_list(), uim_scm_null_list(), uim_scm_make_int(j), uim_scm_null_list());
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
    if (!str)
      return uim_scm_f();
  } else {
    if (nr_cands <= nth)
      return uim_scm_f();
    str = ca->cands[nth];
  }
  reorder_candidate(ca, str);

  if (okuri_ != uim_scm_null_list()) {
    struct skk_line *sl;
    const char *okuri;
    int found = 0;

    okuri = uim_scm_refer_c_str(okuri_);
    sl = ca->line;
    for (i = 1; i < sl->nr_cand_array; i++) {
      if (!strcmp(okuri, sl->cands[i].okuri)) {
	found = 1;
	break;
      }
    }
    if (!found) {
      ca = find_cand_array_lisp(head_, okuri_head_, okuri_, 1);
      reorder_candidate(ca, str);
    } else {
      /* also reorder base candidate array */
      reorder_candidate(&sl->cands[0], str);
    }
  }

  ca->line->need_save = 1;
  move_line_to_cache_head(skk_dic, ca->line);

  return uim_scm_f();
}

static void
learn_word_to_cand_array(struct skk_cand_array *ca, char *word)
{
  int i, nth = -1;
  for (i = 0; i < ca->nr_cands; i++) {
    if (!strcmp(word, ca->cands[i])) {
      nth = i;
    }
  }
  if (nth == -1) {
    push_back_candidate_to_array(ca, word);
  }
  reorder_candidate(ca, word);
  ca->line->need_save = 1;
}

static char *
quote_word(const char *word)
{
  char *str;
  const char *p;
  int len;

  str= strdup("(concat \"");
  for (p = word; *p; p++) {
    len = strlen(str);

    switch (*p) {
    case '/':
	    str = realloc(str, len + strlen("\\057") + 1);
	    strcat(str, "\\057");
	    break;
    case '[':
	    str = realloc(str, len + strlen("[") + 1);
	    strcat(str, "[");
	    break;
    case ']':
	    str = realloc(str, len + strlen("]") + 1);
	    strcat(str, "]");
	    break;
    case '\n':
	    str = realloc(str, len + strlen("\\n") + 1);
	    strcat(str, "\\n");
	    break;
    case '\r':
	    str = realloc(str, len + strlen("\\r") + 1);
	    strcat(str, "\\r");
	    break;
    case '\\':
	    str = realloc(str, len + strlen("\\\\") + 1);
	    strcat(str, "\\\\");
	    break;
    case ';':
	    str = realloc(str, len + strlen("\\073") + 1);
	    strcat(str, "\\073");
	    break;
    case '"':
	    str = realloc(str, len + strlen("\\\"") + 1);
	    strcat(str, "\\\"");
	    break;
    default:
	    str = realloc(str, len + 2);
	    str[len] = *p;
	    str[len + 1] = '\0';
	    break;
    }
  }
  len = strlen(str);
  str = realloc(str, len + strlen("\")") + 1);
  strcat(str, "\")");

  return str;
}

static char *
sanitize_word(const char *arg)
{
  const char *p;
  int is_space_only = 1;

  if (!arg || !strlen(arg)) {
    return NULL;
  }
  for (p = arg; *p; p++) {
    switch (*p) {
    case '/':
    case '[':
    case ']':
    case '\n':
    case '\r':
    case '\\':
    case ';':
    case '"':
      return quote_word(arg);
    case ' ':
      break;
    default:
      is_space_only = 0;
      break;
    }
  }
  if (is_space_only)
    return NULL;

  return strdup(arg);
}

static uim_lisp
skk_learn_word(uim_lisp head_, uim_lisp okuri_head_, uim_lisp okuri_, uim_lisp word_)
{
  struct skk_cand_array *ca;
  char *word;
  const char *tmp;

  tmp = uim_scm_refer_c_str(word_);
  word = sanitize_word(tmp);
  if (!word) {
    return uim_scm_f();
  }

  ca = find_cand_array_lisp(head_, okuri_head_, okuri_, 1);
  if (ca) {
    learn_word_to_cand_array(ca, word);
  }

  tmp = uim_scm_refer_c_str(okuri_);
  if (strlen(tmp)) {
    ca = find_cand_array_lisp(head_, okuri_head_, uim_scm_null_list(), 1);
    if (ca) {
      learn_word_to_cand_array(ca, word);
    }
  }
  free(word);
  return uim_scm_f();
}

static void
reverse_cache(struct dic_info *di)
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
parse_dic_line(struct dic_info *di, char *line)
{
  char *buf, *sep;
  struct skk_line *sl;
  int i;

  buf = alloca(strlen(line)+1);
  strcpy(buf, line);
  sep = strchr(buf, ' ');
  if (!sep) {
    return ;
  }
  if (sep == buf) {
    return ;
  }
  *sep = '\0';
  if (!isalpha((unsigned char)buf[0]) && islower((unsigned char)sep[-1])) { /* okuri-ari entry */
    char okuri_head = sep[-1];
    sep[-1] = 0;
    sl = compose_line(di, buf, okuri_head, line);
  } else {
    sl = compose_line(di, buf, 0, line);
  }
  sl->need_save = 1;
  /**/
  for (i = 0; i < sl->nr_cand_array; i++) {
    sl->cands[i].nr_real_cands = sl->cands[i].nr_cands;
  }
  /**/
  add_line_to_cache_head(di, sl);
}

static void
write_out_array(FILE *fp, struct skk_cand_array *ca)
{
  int i;
  if (ca->okuri) {
    fprintf(fp, "[%s/", ca->okuri);
    for (i = 0; i < ca->nr_real_cands; i++) {
      fprintf(fp, "%s/", ca->cands[i]);
    }
    fprintf(fp, "]/");
  } else {
    for (i = 0; i < ca->nr_real_cands; i++) {
      fprintf(fp, "%s/", ca->cands[i]);
    }
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
  char *lock_fn;

  lock_fn = malloc(sizeof(char) * (strlen(name) + strlen(".lock") + 1));
  if (lock_fn == NULL)
    return -1;
  sprintf(lock_fn, "%s.lock", name);

  fd = open(lock_fn, O_CREAT | O_RDWR, S_IRUSR | S_IWUSR);
  if (fd == -1) {
    free(lock_fn);
    return fd;
  }

  fl.l_type = type;
  fl.l_whence = SEEK_SET;
  fl.l_start = 0;
  fl.l_len = 0;
  if (fcntl(fd, F_SETLKW, &fl) == -1) {
    close(fd);
    fd = -1;
  }

  free(lock_fn);
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

static uim_lisp
skk_read_personal_dictionary(struct dic_info *di, const char *fn)
{
  struct stat st;
  FILE *fp;
  char buf[4096];
  int err_flag = 0;
  int lock_fd;

  lock_fd = open_lock(fn, F_RDLCK);

  if (stat(fn, &st) == -1) {
    close_lock(lock_fd);
    return uim_scm_f();
  }

  fp = fopen(fn, "r");
  if (!fp) {
    close_lock(lock_fd);
    return uim_scm_f();
  }

  di->personal_dic_timestamp = st.st_mtime;

  while (fgets(buf, 4096, fp)) {
    int len = strlen(buf);
    if (buf[len-1] == '\n') {
      if (err_flag == 0) {
	if (buf[0] != ';') {
	  buf[len-1] = 0;
	  parse_dic_line(di, buf);
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
  return uim_scm_t();
}

static uim_lisp
skk_lib_read_personal_dictionary(uim_lisp fn_)
{
  const char *fn = uim_scm_refer_c_str(fn_);
  return skk_read_personal_dictionary(skk_dic, fn);
}

static void push_back_candidate_array_to_sl(struct skk_line *sl,
				      struct skk_cand_array *src_ca)
{
  int i;
  struct skk_cand_array *ca;

  sl->nr_cand_array++;
  sl->cands = realloc(sl->cands,
		  sizeof(struct skk_cand_array) * sl->nr_cand_array);
  ca = &sl->cands[sl->nr_cand_array - 1];
  ca->is_used = src_ca->is_used;
  ca->nr_cands = src_ca->nr_cands;
  ca->cands = malloc(sizeof(char *) * src_ca->nr_cands);
  for (i = 0; i < ca->nr_cands; i++) {
    ca->cands[i] = strdup(src_ca->cands[i]);
  }

  ca->nr_real_cands = src_ca->nr_real_cands;
  ca->okuri = strdup(src_ca->okuri);
  ca->line = sl;
}

static void compare_and_merge_skk_line(struct skk_line *dst_sl,
				       struct skk_line *src_sl)
{
  int i, j;
  struct skk_cand_array *dst_ca, *src_ca;

  if (dst_sl == NULL || src_sl == NULL)
    return;

  src_ca = &src_sl->cands[0];
  dst_ca = &dst_sl->cands[0];
  if (src_ca->nr_real_cands >= dst_ca->nr_real_cands)
    merge_real_candidate_array(src_ca, dst_ca);

  for (i = 1; i < src_sl->nr_cand_array; i++) {
    int dup = 0;
    src_ca = &src_sl->cands[i];

    for (j = 1; j < dst_sl->nr_cand_array; j++) {
      dst_ca = &dst_sl->cands[j];
      if (!strcmp(src_ca->okuri, dst_ca->okuri)) {
	dup = 1;
	if (src_ca->nr_real_cands >= dst_ca->nr_real_cands)
	  merge_real_candidate_array(src_ca, dst_ca);
      }
    }
    if (!dup)
      push_back_candidate_array_to_sl(dst_sl, src_ca);
  }
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
cache_line_diffs(struct skk_line *p, struct skk_line *q, int *len)
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
      compare_and_merge_skk_line(p, q);
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
update_personal_dictionary_cache(const char *fn)
{
  struct dic_info *di;
  struct skk_line *sl, *tmp, *diff, **cache_array;
  int i, diff_len = 0;

  di = (struct dic_info *)malloc(sizeof(struct dic_info));
  if (di == NULL)
    return;
  di->head.next = NULL;
  skk_read_personal_dictionary(di, fn);
  di->head.next = lsort(di->head.next);

  /* keep original sequence of cache */
  cache_array = (struct skk_line **)malloc(sizeof(struct skk_line *)
		  * skk_dic->cache_len);
  if (cache_array == NULL)
    return;
  i = 0;
  sl = skk_dic->head.next;
  while (sl) {
    cache_array[i] = sl;
    sl = sl->next;
    i++;
  }

  skk_dic->head.next = lsort(skk_dic->head.next);

  /* get differential lines and merge candidate */
  diff = cache_line_diffs(skk_dic->head.next, di->head.next, &diff_len);

  /* revert sequence of the cache */
  if (cache_array[0]) {
    sl = skk_dic->head.next = cache_array[0];
    for (i = 0; i < skk_dic->cache_len - 1; i++) {
      sl->next = cache_array[i + 1];
      sl = sl->next;
    }
    sl->next = NULL;
  }

  /* add differential lines at the top of the cache */
  if (diff != NULL) {
    sl = diff;
    while (sl->next) {
      sl = sl->next;
    }
    sl->next = skk_dic->head.next;
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
skk_lib_save_personal_dictionary(uim_lisp fn_)
{
  FILE *fp;
  const char *fn = uim_scm_refer_c_str(fn_);
  struct skk_line *sl;
  struct stat st;
  int lock_fd = -1;

  if (fn) {
    if (stat(fn, &st) != -1) {
      if (st.st_mtime != skk_dic->personal_dic_timestamp)
	update_personal_dictionary_cache(fn);
    }
    if (skk_dic->cache_modified == 0) {
      return uim_scm_f();
    }
    lock_fd = open_lock(fn, F_WRLCK);
    fp = fopen(fn, "w");
    if (!fp) {
      close_lock(lock_fd);
      return uim_scm_f();
    }
  } else {
    fp = stdout;
  }

  for (sl = skk_dic->head.next; sl; sl = sl->next) {
    if (sl->need_save) {
      write_out_line(fp, sl);
    }
  }
  fclose(fp);

  if (stat(fn, &st) != -1)
    skk_dic->personal_dic_timestamp = st.st_mtime;

  close_lock(lock_fd);
  skk_dic->cache_modified = 0;

  return uim_scm_f();
}

static uim_lisp
skk_lib_get_annotation(uim_lisp str_)
{
  const char *str, *sep;
  uim_lisp res;

  if (str_ == uim_scm_null_list())
    return uim_scm_null_list();

  str = uim_scm_refer_c_str(str_);
  sep = strrchr(str, ';');
  if (sep && (*(++sep) != '\0')) {
    res = uim_scm_make_str(sep);
  } else {
    res = uim_scm_make_str("");
  }
  return res;
}

static uim_lisp
skk_lib_remove_annotation(uim_lisp str_)
{
  char *str, *sep;
  uim_lisp res;

  if (str_ == uim_scm_null_list())
    return uim_scm_null_list();

  str = uim_scm_c_str(str_);
  sep = strrchr(str, ';');
  if (sep && (*(sep + 1) != '\0')) {
    *sep = '\0';
  }
  res = uim_scm_make_str(str);
  free(str);
  return res;
}

static uim_lisp
skk_eval_candidate(uim_lisp str_)
{
  const char *cand, *evaluated_str;
  char *p, *q, *str;
  size_t len;
  uim_lisp cand_, return_val;

  if (str_ == uim_scm_null_list())
    return uim_scm_null_list();

  cand = uim_scm_refer_c_str(str_);

  /* eval concat only for now */
  if ((p = strstr(cand, "(concat \"")) == NULL)
    return str_;

  /* check close paren */
  q = strrchr(p, ')');
  if (!q || (strstr(p, "\")") == NULL))
    return str_;

  /* ignore make-string */
  if (strstr(p, "make-string"))
    return str_;

  len = q - p + 1;
  /* replace elisp's concat with string-append */
  str = malloc(len + strlen("string-append") - strlen("concat") + 1);
  strcpy(str, "(string-append");
  strncat(str, p + strlen("(concat"), q - (p + strlen("(concat")) + 1);

  UIM_EVAL_FSTRING1(NULL, "%s", str);
  return_val = uim_scm_return_value();
  if (return_val == uim_scm_null_list()) {
    free(str);
    return str_;
  }
  evaluated_str = uim_scm_refer_c_str(return_val);

  /* get evaluated candidate */
  len = p - cand + strlen(evaluated_str);
  if (len > strlen(str))
    str = realloc(str, len + 1);

  if (p != cand) {
    strncpy(str, cand, p - cand);
    str[p - cand] = '\0';
    strcat(str, evaluated_str);
  } else {
    strcpy(str, evaluated_str);
  }

  cand_ = uim_scm_make_str(str);
  free(str);
  return cand_;
}


void
uim_plugin_instance_init(void)
{
  uim_scm_init_subr_1("skk-lib-dic-open", skk_dic_open);
  uim_scm_init_subr_1("skk-lib-read-personal-dictionary", skk_lib_read_personal_dictionary);
  uim_scm_init_subr_1("skk-lib-save-personal-dictionary", skk_lib_save_personal_dictionary);
  uim_scm_init_subr_3("skk-lib-get-entry", skk_get_entry);
  uim_scm_init_subr_1("skk-lib-store-replaced-numstr", skk_store_replaced_numeric_str);
  uim_scm_init_subr_2("skk-lib-merge-replaced-numstr", skk_merge_replaced_numeric_str);
  uim_scm_init_subr_1("skk-lib-replace-numeric", skk_replace_numeric);
  uim_scm_init_subr_5("skk-lib-get-nth-candidate", skk_get_nth_candidate);
  uim_scm_init_subr_4("skk-lib-get-nr-candidates", skk_get_nr_candidates);
  uim_scm_init_subr_5("skk-lib-commit-candidate", skk_commit_candidate);
  uim_scm_init_subr_4("skk-lib-learn-word", skk_learn_word);
  uim_scm_init_subr_1("skk-lib-get-annotation", skk_lib_get_annotation);
  uim_scm_init_subr_1("skk-lib-remove-annotation", skk_lib_remove_annotation);
  uim_scm_init_subr_1("skk-lib-get-completion", skk_get_completion);
  uim_scm_init_subr_2("skk-lib-get-nth-completion", skk_get_nth_completion);
  uim_scm_init_subr_1("skk-lib-get-nr-completions", skk_get_nr_completions);
  uim_scm_init_subr_1("skk-lib-clear-completions", skk_clear_completions);
  uim_scm_init_subr_1("skk-lib-eval-candidate", skk_eval_candidate);
}

void
uim_plugin_instance_quit(void)
{
  struct skk_line *sl, *tmp;

  if (!skk_dic)
    return;

  if (skk_dic->addr) {
    munmap(skk_dic->addr, skk_dic->size);
  }
  sl = skk_dic->head.next;
  while (sl) {
    tmp = sl;
    sl = sl->next;
    free_skk_line(tmp);
  }
  free(skk_dic);
  skk_dic = NULL;
}
