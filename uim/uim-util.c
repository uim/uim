/*

  Copyright (c) 2003,2004 uim Project http://uim.freedesktop.org/

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

  THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
  OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
  SUCH DAMAGE.

*/

#include "config.h"

#include <stdlib.h>
#include <string.h>
#include <locale.h>
#include <langinfo.h>
#include "context.h"
#include "gettext.h"
#include "siod.h"
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

static LISP
string_equal(LISP x, LISP y)
{
  long xl, yl;
  char *xs, *ys;
  xs = get_c_string_dim(x, &xl);
  ys = get_c_string_dim(y, &yl);
  if (xl != yl) {
    return false_sym;
  }
  if (!strncmp(xs, ys, xl)) {
    return (true_sym);
  }
  return false_sym;
}

static LISP
charcode2string(LISP x)
{
  char buf[2];
  if (INTNUMP(x)) {
    buf[0] = INTNM(x);
  } else {
    buf[0] = 0;
  }
  buf[1] = 0;
  return strcons (1,buf);
}

static LISP
string2charcode(LISP x)
{
  char *buf = get_c_string(x);

  if (buf) {
    return intcons(*buf);
  }
  return false_sym;
}

static LISP
digit2string(LISP x)
{
  char buf[10];
  int i;

  i = get_c_int(x);

  sprintf(buf,"%d",i);
  return strcons (strlen(buf),buf);
}

static LISP
nthcdr(LISP nth_, LISP lst)
{
  int nth = get_c_int(nth_);
  int i;
  for (i = 0; i < nth; i++) {
    if NULLP(lst) {
      /* something bad happened */
      return false_sym;
    }
    lst = CDR(lst);
  }
  return lst;
}

/* may be deprecated. use uim_scm_c_str() instead */
char *
uim_get_c_string(LISP str)
{
  char *s;
  long len;
  char *buf;
  s = get_c_string_dim(str, &len);
  buf = (char *)malloc(sizeof(char)*(len + 1));
  strncpy(buf, s, len);
  buf[len] = 0;
  return buf;
}

static LISP
str_seq_equal(LISP seq, LISP rule)
{
  int sl = nlength(seq);
  int rl = nlength(rule);
  int i;
  if (sl != rl) {
    return false_sym;
  }
  for (i = 0; i < sl; i++) {
    if FALSEP(string_equal(CAR(seq), CAR(rule))) {
      return false_sym;
    }
    seq = CDR(seq);
    rule = CDR(rule);
  }
  return true_sym;
}

/*
 * Partial -> first string of remaining sequence
 *  eg. ("a" "b") ("a" "b" "c") -> "c"
 * Not partial -> #f
 *
 */
static LISP
str_seq_partial(LISP seq, LISP rule)
{
  int sl = nlength(seq);
  int rl = nlength(rule);
  int i;

  if (sl >= rl) {
    return false_sym;
  }
  /* Obviously. sl < rl */
  for (i = 0; i < sl; i++) {
    if FALSEP(string_equal(CAR(seq), CAR(rule))) {
      return false_sym;
    }
    seq = CDR(seq);
    rule = CDR(rule);
  }
  if (rule && CAR(rule)) {
    return CAR(rule);
  }
  /* never reach here */
  return false_sym;
}

static LISP
rk_find_seq(LISP seq, LISP rules)
{
  for (; NNULLP(rules); rules = CDR(rules)) {
    LISP rule = CAR(rules);
    LISP key = CAR(CAR(rule));
    if NFALSEP(str_seq_equal(seq, key)) {
      return rule;
    }
  }
  return false_sym;
}

static LISP
rk_find_partial_seq(LISP seq, LISP rules)
{
  for (; NNULLP(rules); rules = CDR(rules)) {
    LISP rule = CAR(rules);
    LISP key = CAR(CAR(rule));
    if NFALSEP(str_seq_partial(seq, key)) {
      return rule;
    }
  }
  return false_sym;
}

/*
 * returns possible next characters
 * (rk-lib-expect-seq '("k" "y") ja-rk-rule) -> ("o" "e" "u" "i" "a")
 */
static LISP
rk_expect_seq(LISP seq, LISP rules)
{
  LISP cur, res = NIL;
  for (cur = rules; NNULLP(cur); cur = CDR(cur)) {
    LISP rule = CAR(cur);
    LISP key = CAR(CAR(rule));
    LISP e = str_seq_partial(seq, key);
    if NFALSEP(e) {
      res = cons(e, res);
    }
  }
  return res;  /* don't return false_sym */
}

static LISP
c_getenv(LISP str_)
{
  char *str = get_c_string(str_);
  char *val;

  if (!str) {
    return false_sym;
  }
  val = getenv(str);
  if (val) {
    return strcons(strlen(val), val);
  } else {
    return false_sym;
  }
}

static LISP
c_setenv(LISP name_, LISP val_, LISP overwrite_)
{
  char *name = get_c_string(name_);
  char *val = get_c_string(val_);
  int overwrite = NFALSEP(overwrite_);
  int err;

  if (!name || !val) {
    return false_sym;
  }
  err = setenv(name, val, overwrite);
  return (err) ? false_sym : true_sym;
}

static LISP
c_unsetenv(LISP name_)
{
  char *name = get_c_string(name_);

  if (!name) {
    return false_sym;
  }
  unsetenv(name);
  return true_sym;
}

static char **
uim_strsplit(char *splittee, char *splitter)
{
  char *cur, *tmp;
  int nr_token = 0;
  int in_token = 0;
  char **res;
  int len;
  int i;

  if(!splittee || !splitter)
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

static LISP
uim_split_string(LISP _splittee, LISP _splitter)
{
  char *splittee = get_c_string(_splittee);
  char *splitter = get_c_string(_splitter);
  char **strs;
  LISP l = NIL;
  int i;
  int n_strs;

  if(_splittee == NULL || _splitter == NULL)
    return false_sym;

  if(splittee == NULL || splitter == NULL)
    return false_sym;

  strs = uim_strsplit(splittee, splitter);

  if(!strs || !*strs)
    return false_sym;

  for (n_strs = 0; strs[n_strs] != '\0'; n_strs++);

  l = uim_scm_c_strs_into_list(n_strs, (const char *const *)strs);
  for (i = n_strs - 1; i >= 0; i--) {
    free(strs[i]);
  }
  free(strs);
  return l;
}

static LISP
eucjp_string_to_list(LISP str_)
{
  char *str = get_c_string(str_);
  unsigned char *cur = (unsigned char *)str;
  LISP res = NIL;
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
    res = cons (strcons(len, (char *)buf), res);
    cur ++;
  }
  return res;
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
  for(i = 0; i < NR_LOCALE_LANGUAGE; i++) {
    if(strcmp(locale_language_table[i].locale, localename) == 0) {
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

static LISP
lang_code_to_lang_name_raw(LISP code_)
{
  const char *code = get_c_string(code_);
  const char *name;
  int unknown_strlen = -1;

  if (!code)
    return false_sym;
  name = get_language_name_from_locale(code);
  return (name) ? strcons(unknown_strlen, name) : false_sym;
}

void
uim_init_util_subrs()
{
  init_subr_2("string=?", string_equal);
  init_subr_2("nthcdr", nthcdr);
  init_subr_1("charcode->string", charcode2string);
  init_subr_1("string->charcode", string2charcode);
  init_subr_1("digit->string", digit2string);
  init_subr_2("str-seq-equal?", str_seq_equal);
  init_subr_2("str-seq-partial?", str_seq_partial);
  init_subr_2("rk-lib-find-seq", rk_find_seq);
  init_subr_2("rk-lib-find-partial-seq", rk_find_partial_seq);
  init_subr_2("rk-lib-expect-seq", rk_expect_seq);
  init_subr_1("getenv", c_getenv);
  init_subr_3("setenv", c_setenv);
  init_subr_1("unsetenv", c_unsetenv);
  init_subr_2("string-split", uim_split_string);
  init_subr_1("string-to-list", eucjp_string_to_list);
  init_subr_1("lang-code->lang-name-raw", lang_code_to_lang_name_raw);
}
