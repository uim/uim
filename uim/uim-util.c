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

#include "config.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <string.h>
#include <locale.h>
#include <langinfo.h>
#include "context.h"
#include "gettext.h"
#include "uim-scm.h"
#include "uim-compat-scm.h"
#include "uim-util.h"

#ifndef HAVE_SETENV
int setenv(const char *, const char *, int);
#endif
#ifndef HAVE_UNSETENV
void unsetenv(const char *);
#endif

extern char *uim_return_str;

/* define constants as procedure to ensure unmodifiable */
static uim_lisp
sys_libdir()
{
  return uim_scm_make_str(LIBDIR);
}

static uim_lisp
sys_pkglibdir()
{
  return uim_scm_make_str(PKGLIBDIR);
}

static uim_lisp
sys_datadir()
{
  return uim_scm_make_str(DATADIR);
}

static uim_lisp
sys_pkgdatadir()
{
  return uim_scm_make_str(PKGDATADIR);
}

static uim_lisp
file_stat_mode(uim_lisp filename, mode_t mode)
{
  struct stat st;
  const char *c_filename;

  if (!uim_scm_stringp(filename))
    return uim_scm_f();

  c_filename = uim_scm_refer_c_str(filename);
  if (stat(c_filename, &st) < 0) {
    return uim_scm_f();
  } else {
    return ((st.st_mode & mode) == mode) ? uim_scm_t() : uim_scm_f();
  }
}

static uim_lisp
file_readablep(uim_lisp filename)
{
  return file_stat_mode(filename, S_IRUSR);
}

static uim_lisp
file_writablep(uim_lisp filename)
{
  return file_stat_mode(filename, S_IWUSR);
}

static uim_lisp
file_executablep(uim_lisp filename)
{
  return file_stat_mode(filename, S_IXUSR);
}

static uim_lisp
file_regularp(uim_lisp filename)
{
  return file_stat_mode(filename, S_IFREG);
}

static uim_lisp
file_directoryp(uim_lisp filename)
{
  return file_stat_mode(filename, S_IFDIR);
}

static uim_lisp
charcode2string(uim_lisp x)
{
  char buf[2];
  if (uim_scm_integerp(x)) {
    buf[0] = uim_scm_c_int(x);
  } else {
    buf[0] = 0;
  }
  buf[1] = 0;
  return uim_scm_make_str(buf);
}

static uim_lisp
string2charcode(uim_lisp x)
{
  const char *buf = uim_scm_refer_c_str(x);

  if (buf) {
    return uim_scm_make_int(*buf);
  }
  return uim_scm_f();
}

static uim_lisp
digit2string(uim_lisp x)
{
  if (uim_scm_integerp(x)) {
    int i;

    i = uim_scm_c_int(x);
    UIM_EVAL_FSTRING1(NULL, "\"%d\"", i);

    return uim_scm_return_value();
  } else {
    return uim_scm_f();
  }
}

static uim_lisp
nthcdr(uim_lisp nth_, uim_lisp lst)
{
  int nth = uim_scm_c_int(nth_);
  int i;
  for (i = 0; i < nth; i++) {
    if (uim_scm_nullp(lst)) {
      /* something bad happened */
      return uim_scm_f();
    }
    lst = uim_scm_cdr(lst);
  }
  return lst;
}

static uim_lisp
str_seq_equal(uim_lisp seq, uim_lisp rule)
{
  int sl = uim_scm_c_int(uim_scm_length(seq));
  int rl = uim_scm_c_int(uim_scm_length(rule));
  int i;
  if (sl != rl) {
    return uim_scm_f();
  }
  for (i = 0; i < sl; i++) {
    if (!uim_scm_string_equal(uim_scm_car(seq), uim_scm_car(rule))) {
      return uim_scm_f();
    }
    seq = uim_scm_cdr(seq);
    rule = uim_scm_cdr(rule);
  }
  return uim_scm_t();
}

/*
 * Partial -> first string of remaining sequence
 *  eg. ("a" "b") ("a" "b" "c") -> "c"
 * Not partial -> #f
 *
 */
static uim_lisp
str_seq_partial(uim_lisp seq, uim_lisp rule)
{
  int sl = uim_scm_c_int(uim_scm_length(seq));
  int rl = uim_scm_c_int(uim_scm_length(rule));
  int i;

  if (sl >= rl) {
    return uim_scm_f();
  }
  /* Obviously. sl < rl */
  for (i = 0; i < sl; i++) {
    if (!uim_scm_string_equal(uim_scm_car(seq), uim_scm_car(rule))) {
      return uim_scm_f();
    }
    seq = uim_scm_cdr(seq);
    rule = uim_scm_cdr(rule);
  }
  if (rule && uim_scm_car(rule)) {
    return uim_scm_car(rule);
  }
  /* never reach here */
  return uim_scm_f();
}

static uim_lisp
rk_find_seq(uim_lisp seq, uim_lisp rules)
{
  for (; !uim_scm_nullp(rules); rules = uim_scm_cdr(rules)) {
    uim_lisp rule = uim_scm_car(rules);
    uim_lisp key = uim_scm_car(uim_scm_car(rule));
    if NFALSEP(str_seq_equal(seq, key)) {
      return rule;
    }
  }
  return uim_scm_f();
}

static uim_lisp
rk_find_partial_seq(uim_lisp seq, uim_lisp rules)
{
  for (; !uim_scm_nullp(rules); rules = uim_scm_cdr(rules)) {
    uim_lisp rule = uim_scm_car(rules);
    uim_lisp key = uim_scm_car(uim_scm_car(rule));
    if NFALSEP(str_seq_partial(seq, key)) {
      return rule;
    }
  }
  return uim_scm_f();
}

/*
 * returns possible next characters
 * (rk-lib-expect-seq '("k" "y") ja-rk-rule) -> ("o" "e" "u" "i" "a")
 */
static uim_lisp
rk_expect_seq(uim_lisp seq, uim_lisp rules)
{
  uim_lisp cur, res = uim_scm_null_list();
  for (cur = rules; !uim_scm_nullp(cur); cur = uim_scm_cdr(cur)) {
    uim_lisp rule = uim_scm_car(cur);
    uim_lisp key = uim_scm_caar(rule);
    uim_lisp e = str_seq_partial(seq, key);
    if NFALSEP(e) {
      res = uim_scm_cons(e, res);
    }
  }
  return res;  /* don't return uim_scm_f() */
}

static uim_lisp
c_getenv(uim_lisp str_)
{
  const char *str = uim_scm_refer_c_str(str_);
  char *val;

  if (!str) {
    return uim_scm_f();
  }

  val = getenv(str);
  if (val) {
    return uim_scm_make_str(val);
  } else {
    return uim_scm_f();
  }
}

static uim_lisp
c_setenv(uim_lisp name_, uim_lisp val_, uim_lisp overwrite_)
{
  const char *name = uim_scm_refer_c_str(name_);
  const char *val = uim_scm_refer_c_str(val_);
  int overwrite = NFALSEP(overwrite_);
  int err;

  if (!name || !val) {
    return uim_scm_f();
  }
  err = setenv(name, val, overwrite);
  return (err) ? uim_scm_f() : uim_scm_t();
}

static uim_lisp
c_unsetenv(uim_lisp name_)
{
  const char *name = uim_scm_refer_c_str(name_);

  if (!name) {
    return uim_scm_f();
  }
  unsetenv(name);
  return uim_scm_t();
}

static char **
uim_strsplit(const char *splittee, const char *splitter)
{
  const char *cur, *tmp;
  int nr_token = 0;
  int in_token = 0;
  char **res;
  int len;
  int i;

  if (!splittee || !splitter)
    return NULL;


  /* count the number of token */
  cur = splittee;
  while (*cur) {
    if (strchr(splitter, *cur)) {
      in_token = 0;
    } else {
      if (!in_token) {
	nr_token ++;
      }
      in_token = 1;
    }
    cur ++;
  }
  /* allocate buffer */
  res = (char **)malloc(sizeof(char *) * (nr_token + 1) );
  if (!res) {
    return NULL;
  }
  /**/
  cur = splittee;
  for (i = 0; i < nr_token; i++) {
    /* find current token's start */
    while (strchr(splitter, *cur)) {
      cur ++;
    }
    /* calc length */
    len = 0;
    tmp = cur;
    while (!strchr(splitter, *tmp)) {
      len ++;
      tmp ++;
    }
    /* store */
    res[i] = malloc(sizeof(char) * (len + 1));
    strncpy(res[i], cur, len);
    res[i][len] = 0;
    cur = tmp;
  }
  /**/
  res[nr_token] = NULL;

  return res;
}

static uim_lisp
uim_split_string(uim_lisp _splittee, uim_lisp _splitter)
{
  const char *splittee = uim_scm_refer_c_str(_splittee);
  const char *splitter = uim_scm_refer_c_str(_splitter);
  char **strs;
  uim_lisp l = uim_scm_null_list();
  int i;
  int n_strs;

  if (!uim_scm_stringp(_splittee) || !uim_scm_stringp(_splitter))
    return uim_scm_f();

  if (splittee == NULL || splitter == NULL)
    return uim_scm_f();

  strs = uim_strsplit(splittee, splitter);

  if (!strs || !*strs)
    return uim_scm_f();

  for (n_strs = 0; strs[n_strs] != '\0'; n_strs++);

  l = uim_scm_c_strs_into_list(n_strs, (const char *const *)strs);
  for (i = n_strs - 1; i >= 0; i--) {
    free(strs[i]);
  }
  free(strs);
  return l;
}

static uim_lisp
eucjp_string_to_list(uim_lisp str_)
{
  const char *str = uim_scm_refer_c_str(str_);
  const unsigned char *cur = (const unsigned char *)str;
  uim_lisp res = uim_scm_null_list();
  while (*cur) {
    char buf[3];
    int len;
    buf[2] = 0;
    if (*cur > 127) {
      /* 2 bytes */
      buf[0] = cur[0];
      buf[1] = cur[1];
      len = 2;
      cur ++;
    } else {
      buf[0] = cur[0];
      buf[1] = 0;
      len = 1;
    }
    res = uim_scm_cons(uim_scm_make_str((char *)buf), res);
    cur ++;
  }
  return res;
}

static uim_lisp
string_prefixp_internal(uim_lisp prefix_, uim_lisp str_,
			int cmp(const char *, const char *, size_t))
{
  const char *prefix, *str;
  size_t len;

  if (!uim_scm_stringp(prefix_) || !uim_scm_stringp(str_))
    return uim_scm_f();

  prefix = uim_scm_refer_c_str(prefix_);
  str = uim_scm_refer_c_str(str_);
  len = strlen(prefix);

  return cmp(prefix, str, len) ? uim_scm_f() : uim_scm_t();
}

static uim_lisp
string_prefixp(uim_lisp prefix_, uim_lisp str_)
{
  return string_prefixp_internal(prefix_, str_, strncmp);
}

static uim_lisp
string_prefix_cip(uim_lisp prefix_, uim_lisp str_)
{
  return string_prefixp_internal(prefix_, str_, strncasecmp);
}

static uim_lisp
shift_elems(uim_lisp lists)
{
  uim_lisp elms, rests, list;

  if (uim_scm_nullp(lists))
    return uim_scm_f();

  elms = rests = uim_scm_null_list();
  for (; !uim_scm_nullp(lists); lists = uim_scm_cdr(lists)) {
    list = uim_scm_car(lists);
    if (uim_scm_nullp(list))
      return uim_scm_f();

    elms = uim_scm_cons(uim_scm_car(list), elms);
    rests = uim_scm_cons(uim_scm_cdr(list), rests);
  }

  return uim_scm_cons(uim_scm_reverse(elms),
		      uim_scm_reverse(rests));
}

static uim_lisp
iterate_lists(uim_lisp mapper, uim_lisp seed, uim_lisp lists)
{
  uim_lisp elms, rest, rests, mapped, res, termp, pair, form;
  uim_bool single_listp;

  single_listp = (uim_scm_length(lists) == 1) ? UIM_TRUE : UIM_FALSE;
  res = seed;
  if (single_listp) {
    rest = uim_scm_car(lists);
  } else {
    rests = lists;
  }
  do {
    if (single_listp) {
      /* fast path */
      elms = uim_scm_list1(uim_scm_car(rest));
      rest = uim_scm_cdr(rest);
    } else {
      pair = shift_elems(rests);
      if (FALSEP(pair)) {
	elms = rests = uim_scm_null_list();
      } else {
	elms = uim_scm_car(pair);
	rests = uim_scm_cdr(pair);
      }
    }

    form = uim_scm_list3(mapper,
			 uim_scm_quote(res),
			 uim_scm_quote(elms));
    mapped = uim_scm_eval(form);
    termp = uim_scm_car(mapped);
    res = uim_scm_cdr(mapped);
  } while (FALSEP(termp));

  return res;
}

static uim_lisp
find_tail(uim_lisp pred, uim_lisp lst)
{
  uim_lisp form, elem;

  for (; !uim_scm_nullp(lst); lst = uim_scm_cdr(lst)) {
    elem = uim_scm_car(lst);
    form = uim_scm_list2(pred, uim_scm_quote(elem));
    if (NFALSEP(uim_scm_eval(form)))
      return lst;
  }

  return uim_scm_f();
}

/* Following is utility functions for C world */
struct _locale_language_table {
  char *locale;
  char *language;
};

static struct _locale_language_table locale_language_table[] = {
#include "iso-639-1.def"
};
#define NR_LOCALE_LANGUAGE \
        (sizeof(locale_language_table) / sizeof(struct _locale_language_table))

static const char *
get_language_name_from_locale(const char *localename)
{
  unsigned int i;
  for (i = 0; i < NR_LOCALE_LANGUAGE; i++) {
    if (strcmp(locale_language_table[i].locale, localename) == 0) {
      return locale_language_table[i].language;
    }
  }
  return NULL;
}

const char *
uim_get_language_name_from_locale(const char *localename)
{
#if 1
  /* performs adhoc "zh_TW:zh_HK" style locale handling as temporary
     specification of this function for backward compatibility
  */
  UIM_EVAL_FSTRING1(NULL, "(langgroup-primary-lang-code \"%s\")", localename);
  localename = uim_return_str;  /* will be free() automatically */
#endif
  return get_language_name_from_locale(localename);
}

const char *
uim_get_language_code_from_language_name(const char *language_name)
{
  unsigned int i;
  for (i = 0; i < NR_LOCALE_LANGUAGE; i++) {
    if (strcmp(locale_language_table[i].language, language_name) == 0) {
      return locale_language_table[i].locale;
    }
  }
  return NULL;
}

static uim_lisp
lang_code_to_lang_name_raw(uim_lisp code_)
{
  const char *code = uim_scm_refer_c_str(code_);
  const char *name;

  if (!code)
    return uim_scm_f();
  name = get_language_name_from_locale(code);
  return (name) ? uim_scm_make_str(name) : uim_scm_f();
}

static uim_lisp
is_setugidp(void)
{
  if (is_setugid()) {
    return uim_scm_t();
  }
  return uim_scm_f();
}

void
uim_init_util_subrs(void)
{
  uim_scm_init_subr_0("sys-libdir", sys_libdir);
  uim_scm_init_subr_0("sys-pkglibdir", sys_pkglibdir);
  uim_scm_init_subr_0("sys-datadir", sys_datadir);
  uim_scm_init_subr_0("sys-pkgdatadir", sys_pkgdatadir);
  uim_scm_init_subr_1("file-readable?", file_readablep);
  uim_scm_init_subr_1("file-writable?", file_writablep);
  uim_scm_init_subr_1("file-executable?", file_executablep);
  uim_scm_init_subr_1("file-regular?", file_regularp);
  uim_scm_init_subr_1("file-directory?", file_directoryp);
  uim_scm_init_subr_2("nthcdr", nthcdr);
  uim_scm_init_subr_1("charcode->string", charcode2string);
  uim_scm_init_subr_1("string->charcode", string2charcode);
  uim_scm_init_subr_1("digit->string", digit2string);
  uim_scm_init_subr_2("str-seq-equal?", str_seq_equal);
  uim_scm_init_subr_2("str-seq-partial?", str_seq_partial);
  uim_scm_init_subr_2("rk-lib-find-seq", rk_find_seq);
  uim_scm_init_subr_2("rk-lib-find-partial-seq", rk_find_partial_seq);
  uim_scm_init_subr_2("rk-lib-expect-seq", rk_expect_seq);
  uim_scm_init_subr_1("getenv", c_getenv);
  uim_scm_init_subr_3("setenv", c_setenv);
  uim_scm_init_subr_1("unsetenv", c_unsetenv);
  uim_scm_init_subr_2("string-split", uim_split_string);
  uim_scm_init_subr_1("string-to-list", eucjp_string_to_list);
  uim_scm_init_subr_2("string-prefix?", string_prefixp);
  uim_scm_init_subr_2("string-prefix-ci?", string_prefix_cip);
  uim_scm_init_subr_3("iterate-lists", iterate_lists);
  uim_scm_init_subr_2("find-tail", find_tail);
  uim_scm_init_subr_1("lang-code->lang-name-raw", lang_code_to_lang_name_raw);
  uim_scm_init_subr_0("is-set-ugid?", is_setugidp);
}
