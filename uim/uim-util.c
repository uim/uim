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

#include <stdlib.h>
#include <string.h>
#include <locale.h>
#include <langinfo.h>
#include "context.h"
#include "siod.h"
#include "config.h"
#include "gettext.h"
#include "uim-util.h"


#define TRUEP(x) EQ(x, true_sym)
#define FALSEP(x) EQ(x, false_sym)
 
#define NTRUEP(x) NEQ(x, true_sym)
#define NFALSEP(x) NEQ(x, false_sym)

extern char *uim_return_str;

static LISP true_sym;
static LISP false_sym;
static LISP quote_sym;

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

/* Scheme interpreter interface functions: "uim_scm" prefix is not
 * stable name. More discussion is required. These functions may be
 * moved to independent file (such as uim-scm.c).
 */

int
uim_scm_c_int(uim_lisp integer) {
  return get_c_int((LISP)integer);
}

int
uim_scm_symbol_value_int(const char *symbol_str)
{
  uim_lisp val_;
  int val;
  val_ = uim_scm_symbol_value(symbol_str);

  if NNULLP(val_) {
    val = uim_scm_c_int(val_);
  } else {
    val = 0;
  }
  return val;
}

uim_lisp
uim_scm_int_from_c_int(int integer) {
  return (uim_lisp)intcons(integer);
}

char *
uim_scm_c_str(uim_lisp str) {
  return strdup(get_c_string((LISP)str));
}

char *
uim_scm_symbol_value_str(const char *symbol_str)
{
  uim_lisp val_;
  char *val;
  val_ = uim_scm_symbol_value(symbol_str);

  if NNULLP(val_) {
    val = uim_scm_c_str(val_);
  } else {
    val = NULL;
  }
  return val;
}

/* backward compatibility */
char *
uim_symbol_value_str(const char *symbol_str) {
  return uim_scm_symbol_value_str(symbol_str);
}

uim_lisp
uim_scm_str_from_c_str(const char *str) {
  int unknown_strlen = -1;
  return (uim_lisp)strcons(unknown_strlen, str);
}

uim_lisp
uim_scm_c_strs_into_list(int n_strs, const char *const *strs) {
  LISP lst = NIL, str = NIL;
  const char *c_str;
  int i, unknown_strlen = -1;

  for (i = n_strs - 1; 0 <= i; i--) {
    c_str = strs[i];
    str = strcons(unknown_strlen, c_str);
    lst = cons(str, lst);
  }

  return lst;
}

uim_lisp
uim_scm_symbol_value(const char *symbol_str)
{
  LISP symbol_str_ = rintern(symbol_str);
  
  if TRUEP(symbol_boundp(symbol_str_, NIL)) {
    return (uim_lisp)symbol_value(symbol_str_, NIL);         
  } else {
    return (uim_lisp)false_sym;
  }
}

uim_lisp
uim_scm_intern_c_str(const char *str)
{
  return (uim_lisp)rintern(str);
}

uim_lisp
uim_scm_qintern_c_str(const char *str)
{
  return uim_scm_quote(uim_scm_intern_c_str(str));
}

void
uim_scm_gc_protect(uim_lisp *location)
{
  gc_protect(location);
}

long
uim_scm_repl_c_string(char *str, long want_init, long want_print)
{
  return repl_c_string(str, want_init, want_print);
}

long
uim_scm_get_verbose_level(void)
{
  return siod_verbose_level;
}

void
uim_scm_set_verbose_level(long new_value)
{
  siod_verbose_level = new_value;
}

void
uim_scm_load_file(const char *fn)
{
  if(!fn)
    return;

  UIM_EVAL_FSTRING1(NULL, "(*catch 'errobj (load \"%s\" #f #f))", fn);
}

uim_lisp
uim_scm_t(void) {
  return (uim_lisp)true_sym;
}

uim_lisp
uim_scm_f(void) {
  return (uim_lisp)false_sym;
}

uim_lisp
uim_scm_null_list(void) {
  return (uim_lisp)NIL;
}

int
uim_scm_nullp(uim_lisp obj) {
  return NULLP(obj);
}

int
uim_scm_eq(uim_lisp a, uim_lisp b) {
  return EQ(a, b);
}

int
uim_scm_string_equal(uim_lisp a, uim_lisp b) {
  uim_lisp form, p;
  form = uim_scm_list3(uim_scm_intern_c_str("string=?"),
		       a,
		       b);
  p = uim_scm_eval(form);
  return TRUEP(p);
}

uim_lisp
uim_scm_eval(uim_lisp obj) {
  return (uim_lisp)leval((LISP)obj, NIL);
}

uim_lisp
uim_scm_quote(uim_lisp obj) {
  return uim_scm_list2(quote_sym, obj);
}

uim_lisp
uim_scm_car(uim_lisp cell) {
  return (uim_lisp)car((LISP)cell);
}

uim_lisp
uim_scm_cdr(uim_lisp cell) {
  return (uim_lisp)cdr((LISP)cell);
}

uim_lisp
uim_scm_cadr(uim_lisp cell) {
  return (uim_lisp)cadr((LISP)cell);
}

uim_lisp
uim_scm_caar(uim_lisp cell) {
  return (uim_lisp)caar((LISP)cell);
}

uim_lisp
uim_scm_cdar(uim_lisp cell) {
  return (uim_lisp)cdar((LISP)cell);
}

uim_lisp
uim_scm_cddr(uim_lisp cell) {
  return (uim_lisp)cddr((LISP)cell);
}

uim_lisp
uim_scm_cons(uim_lisp car, uim_lisp cdr) {
  return (uim_lisp)cons((LISP)car, (LISP)cdr);
}

uim_lisp
uim_scm_nth(uim_lisp n, uim_lisp lst) {
  uim_lisp form;
  form = uim_scm_list3(uim_scm_intern_c_str("nth"),
		       n,
		       lst);
  return uim_scm_eval(form);
}

uim_lisp
uim_scm_list1(uim_lisp elm1) {
  uim_lisp lst;
  lst = (uim_lisp)listn(1, (LISP)elm1);
  return lst;
}

uim_lisp
uim_scm_list2(uim_lisp elm1, uim_lisp elm2) {
  uim_lisp lst;
  lst = (uim_lisp)listn(2, (LISP)elm1, (LISP)elm2);
  return lst;
}

uim_lisp
uim_scm_list3(uim_lisp elm1, uim_lisp elm2, uim_lisp elm3) {
  uim_lisp lst;
  lst = (uim_lisp)listn(3, (LISP)elm1, (LISP)elm2, (LISP)elm3);
  return lst;
}

uim_lisp
uim_scm_list4(uim_lisp elm1, uim_lisp elm2, uim_lisp elm3, uim_lisp elm4) {
  uim_lisp lst;
  lst = (uim_lisp)listn(4, (LISP)elm1, (LISP)elm2, (LISP)elm3, (LISP)elm4);
  return lst;
}

uim_lisp
uim_scm_list5(uim_lisp elm1, uim_lisp elm2, uim_lisp elm3, uim_lisp elm4,
              uim_lisp elm5)
{
  uim_lisp lst;
  lst = (uim_lisp)listn(5, (LISP)elm1, (LISP)elm2, (LISP)elm3, (LISP)elm4,
			(LISP)elm5);
  return lst;
}

uim_lisp
uim_scm_reverse(uim_lisp cell)
{
  return (uim_lisp)reverse((LISP)cell);
}

uim_lisp
uim_scm_nreverse(uim_lisp cell)
{
  return (uim_lisp)nreverse((LISP)cell);
}


/* Customize interface functions: They are not appropriate to be
 * here. More discussion is required.
 */

uim_lisp
uim_custom_value(uim_lisp custom_sym) {
  uim_lisp form;

  form = uim_scm_list2(uim_scm_intern_c_str("custom-value"),
		       uim_scm_quote(custom_sym));

  return uim_scm_eval(form);
}

int
uim_custom_value_as_bool(uim_lisp custom_sym) {
  uim_lisp val;
  int result = 0;

  if (uim_scm_eq(uim_custom_type(custom_sym),
		 uim_scm_intern_c_str("boolean")))
  {
    val = uim_custom_value(custom_sym);
    result = uim_scm_eq(val, uim_scm_f()) ? 0 : 1;
  }

  return result;
}

int
uim_custom_value_as_int(uim_lisp custom_sym) {
  uim_lisp val;
  int result = 0;

  if (uim_scm_eq(uim_custom_type(custom_sym),
		 uim_scm_intern_c_str("integer")))
  {
    val = uim_custom_value(custom_sym);
    result = uim_scm_c_int(val);
  }

  return result;
}

char *
uim_custom_value_as_str(uim_lisp custom_sym) {
  uim_lisp val;
  char *result = NULL;

  if (uim_scm_eq(uim_custom_type(custom_sym),
		 uim_scm_intern_c_str("string")))
  {
    val = uim_custom_value(custom_sym);
    result = uim_scm_c_str(val);
  }

  return result;
}

char *
uim_custom_value_as_path(uim_lisp custom_sym) {
  uim_lisp val;
  char *result = NULL;

  if (uim_scm_eq(uim_custom_type(custom_sym),
		 uim_scm_intern_c_str("pathname")))
  {
    val = uim_custom_value(custom_sym);
    result = uim_scm_c_str(val);
  }

  return result;
}

uim_lisp
uim_custom_value_as_symbol(uim_lisp custom_sym) {
  uim_lisp val;

  if (uim_scm_eq(uim_custom_type(custom_sym),
		 uim_scm_intern_c_str("symbol")))
  {
    val = uim_custom_value(custom_sym);
  } else {
    val = uim_scm_f();
  }

  return val;
}

void
uim_custom_set(uim_lisp custom_sym, uim_lisp custom_val) {
  uim_lisp form;

  form = uim_scm_list3(uim_scm_intern_c_str("custom-set!"),
		       uim_scm_quote(custom_sym),
		       custom_val);
  uim_scm_eval(form);
}

char *
uim_custom_symbol_label(uim_lisp custom_sym, uim_lisp val_sym) {
  uim_lisp form, label;

  form = uim_scm_list3(uim_scm_intern_c_str("custom-symbol-label"),
		       uim_scm_quote(custom_sym),
		       uim_scm_quote(val_sym));
  label = uim_scm_eval(form);

  return uim_scm_c_str(label);
}

char *
uim_custom_symbol_desc(uim_lisp custom_sym, uim_lisp val_sym) {
  uim_lisp form, desc;

  form = uim_scm_list3(uim_scm_intern_c_str("custom-symbol-desc"),
		       uim_scm_quote(custom_sym),
		       uim_scm_quote(val_sym));
  desc = uim_scm_eval(form);

  return uim_scm_c_str(desc);
}

uim_lisp
uim_custom_label(uim_lisp custom_sym) {
  uim_lisp form;

  form = uim_scm_list2(uim_scm_intern_c_str("custom-label"),
		       uim_scm_quote(custom_sym));

  return uim_scm_eval(form);
}

uim_lisp
uim_custom_desc(uim_lisp custom_sym) {
  uim_lisp form;

  form = uim_scm_list2(uim_scm_intern_c_str("custom-desc"),
		       uim_scm_quote(custom_sym));

  return uim_scm_eval(form);
}

uim_lisp
uim_custom_type(uim_lisp custom_sym) {
  uim_lisp form;

  form = uim_scm_list2(uim_scm_intern_c_str("custom-type"),
		       uim_scm_quote(custom_sym));

  return uim_scm_eval(form);
}

uim_lisp
uim_custom_default_value(uim_lisp custom_sym) {
  uim_lisp form;

  form = uim_scm_list2(uim_scm_intern_c_str("custom-default-value"),
		       uim_scm_quote(custom_sym));

  return uim_scm_eval(form);
}

int
uim_custom_ctype(uim_lisp custom_sym) {
  uim_lisp type;
  int result;

  type = uim_custom_type(custom_sym);
  if (uim_scm_eq(type,
		 uim_scm_intern_c_str("boolean")))
  {
    result = UCustom_Bool;
  } else if (uim_scm_eq(type,
			uim_scm_intern_c_str("integer")))
  {
    result = UCustom_Int;
  } else if (uim_scm_eq(type,
			uim_scm_intern_c_str("string")))
  {
    result = UCustom_Str;
  } else if (uim_scm_eq(type,
			uim_scm_intern_c_str("pathname")))
  {
    result = UCustom_Path;
  } else if (uim_scm_eq(type,
			uim_scm_intern_c_str("symbol")))
  {
    result = UCustom_Symbol;
  } else if (uim_scm_eq(type,
			uim_scm_intern_c_str("key")))
  {
    result = UCustom_Key;
  } else {
    result = UCustom_Bool;
  }

  return result;
}

uim_lisp
uim_custom_range(uim_lisp custom_sym) {
  uim_lisp form;

  form = uim_scm_list2(uim_scm_intern_c_str("custom-range"),
		       uim_scm_quote(custom_sym));

  return uim_scm_eval(form);
}

char *
uim_custom_group_label(uim_lisp group_sym) {
  uim_lisp form, label;

  form = uim_scm_list2(uim_scm_intern_c_str("custom-group-label"),
		       uim_scm_quote(group_sym));
  label = uim_scm_eval(form);

  return uim_scm_c_str(label);
}

char *
uim_custom_group_desc(uim_lisp group_sym) {
  uim_lisp form, desc;

  form = uim_scm_list2(uim_scm_intern_c_str("custom-group-desc"),
		       uim_scm_quote(group_sym));
  desc = uim_scm_eval(form);

  return uim_scm_c_str(desc);
}

uim_lisp
uim_custom_group_subgroups(uim_lisp group_sym) {
  uim_lisp form, subgrps;

  form = uim_scm_list2(uim_scm_intern_c_str("custom-group-subgroups"),
		       uim_scm_quote(group_sym));
  subgrps = uim_scm_eval(form);

  return subgrps;
}

uim_lisp
uim_custom_list_groups(void) {
  uim_lisp form, groups;

  form = uim_scm_list1(uim_scm_intern_c_str("custom-list-groups"));
  groups = uim_scm_eval(form);

  return groups;
}

uim_lisp
uim_custom_list_primary_groups(void) {
  uim_lisp form, groups;

  form = uim_scm_list1(uim_scm_intern_c_str("custom-list-primary-groups"));
  groups = uim_scm_eval(form);

  return groups;
}

uim_lisp
uim_custom_collect_by_group(uim_lisp group_sym) {
  uim_lisp form, customs;

  form = uim_scm_list2(uim_scm_intern_c_str("custom-collect-by-group"),
		       uim_scm_quote(group_sym));
  customs = uim_scm_eval(form);

  return customs;
}

char *
uim_custom_value_as_string(uim_lisp sym) {
  uim_lisp form, value;

  form = uim_scm_list2(uim_scm_intern_c_str("custom-canonical-value-as-string"),
		       uim_scm_quote(sym));
  value = uim_scm_eval(form);

  return uim_scm_c_str(value);
}

char *
uim_custom_definition_as_string(uim_lisp sym) {
  uim_lisp form, definition;

  form = uim_scm_list2(uim_scm_intern_c_str("custom-as-string"),
		       uim_scm_quote(sym));
  definition = uim_scm_eval(form);

  return uim_scm_c_str(definition);
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
  true_sym  = siod_true_value();
#if 0
  false_sym = siod_false_value();
#else
  /* false_sym has to be NIL until bug #617 and #642 are fixed
   * -- YamaKen
   */
  false_sym = NIL;
#endif
  quote_sym = uim_scm_intern_c_str("quote");
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
