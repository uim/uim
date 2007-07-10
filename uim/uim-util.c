/*

  Copyright (c) 2003-2007 uim Project http://code.google.com/p/uim/

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

#include <config.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "uim-internal.h"
#include "uim-scm.h"
#include "uim-compat-scm.h"
#include "uim-scm-abbrev.h"
#include "uim-util.h"

static uim_lisp protected;

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
file_mtime(uim_lisp f)
{
  const char *filename = uim_scm_refer_c_str(f);
  struct stat buf;

  if(stat(filename, &buf) == 0) {
    return uim_scm_make_int(buf.st_mtime);
  } else {
    /* FIXME: Write error handling code. */
    return uim_scm_make_int(0);
  }
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
  int overwrite = UIM_SCM_NFALSEP(overwrite_);
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
    strlcpy(res[i], cur, len + 1);
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

  if (!strs)
    return uim_scm_f();

  if (!*strs)
    return uim_scm_null_list();

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
			int (*cmp)(const char *, const char *, size_t))
{
  const char *prefix, *str;
  size_t len;

  if (!uim_scm_stringp(prefix_) || !uim_scm_stringp(str_))
    return uim_scm_f();

  prefix = uim_scm_refer_c_str(prefix_);
  str = uim_scm_refer_c_str(str_);
  len = strlen(prefix);

  return (*cmp)(prefix, str, len) ? uim_scm_f() : uim_scm_t();
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

  return uim_scm_cons(uim_scm_callf("reverse", "o", elms),
                      uim_scm_callf("reverse", "o", rests));
}

static uim_lisp
iterate_lists(uim_lisp mapper, uim_lisp seed, uim_lisp lists)
{
  uim_lisp elms, rest, rests, mapped, res, termp, pair, form;
  uim_bool single_listp;

  single_listp = uim_scm_nullp(uim_scm_cdr(lists));
  rest = rests = uim_scm_null_list();
  res = seed;

  if (single_listp) {
    rest = uim_scm_car(lists);
  } else {
    rests = lists;
  }
  do {
    if (single_listp) {
      /* fast path */
      if (uim_scm_nullp(rest)) {
	elms = uim_scm_null_list();
      } else {
	elms = uim_scm_list1(uim_scm_car(rest));
	rest = uim_scm_cdr(rest);
      }
    } else {
      pair = shift_elems(rests);
      if (UIM_SCM_FALSEP(pair)) {
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
  } while (UIM_SCM_FALSEP(termp));

  return res;
}

const char *
uim_get_language_name_from_locale(const char *locale)
{
  uim_lisp lang_code, lang_name;

  assert(uim_scm_gc_any_contextp());
  assert(locale);

  /* Performs adhoc "zh_TW:zh_HK" style locale handling as temporary
   * specification of this function for backward compatibility. */
  protected =
    lang_code = uim_scm_callf("langgroup-primary-lang-code", "s", locale);
  protected =
    lang_name = uim_scm_callf("lang-code->lang-name", "o", lang_code);
  return uim_scm_refer_c_str(lang_name);
}

const char *
uim_get_language_code_from_language_name(const char *language_name)
{
  uim_lisp lang_code;

  assert(uim_scm_gc_any_contextp());
  assert(language_name);

  protected =
    lang_code = uim_scm_callf("lang-name->lang-code", "s", language_name);
  return uim_scm_refer_c_str(lang_code);
}

static uim_lisp
setugidp(void)
{
  assert(uim_scm_gc_any_contextp());

  if (uim_issetugid()) {
    return uim_scm_t();
  }
  return uim_scm_f();
}

void
uim_init_util_subrs(void)
{
  protected = uim_scm_f();
  uim_scm_gc_protect(&protected);

  uim_scm_init_subr_0("sys-libdir", sys_libdir);
  uim_scm_init_subr_0("sys-pkglibdir", sys_pkglibdir);
  uim_scm_init_subr_0("sys-datadir", sys_datadir);
  uim_scm_init_subr_0("sys-pkgdatadir", sys_pkgdatadir);
  uim_scm_init_subr_1("file-readable?", file_readablep);
  uim_scm_init_subr_1("file-writable?", file_writablep);
  uim_scm_init_subr_1("file-executable?", file_executablep);
  uim_scm_init_subr_1("file-regular?", file_regularp);
  uim_scm_init_subr_1("file-directory?", file_directoryp);
  uim_scm_init_subr_1("file-mtime", file_mtime);
  uim_scm_init_subr_0("setugid?", setugidp);
  uim_scm_init_subr_1("getenv", c_getenv);
  uim_scm_init_subr_3("setenv", c_setenv);
  uim_scm_init_subr_1("unsetenv", c_unsetenv);

  /* these procedures should be replaced with standard ones of R5RS or SRFIs */
  uim_scm_init_subr_1("charcode->string", charcode2string);
  uim_scm_init_subr_1("string->charcode", string2charcode);
  uim_scm_init_subr_2("string-split", uim_split_string);
  uim_scm_init_subr_1("string-to-list", eucjp_string_to_list);
  uim_scm_init_subr_2("string-prefix?", string_prefixp);
  uim_scm_init_subr_2("string-prefix-ci?", string_prefix_cip);
  uim_scm_init_subr_3("iterate-lists", iterate_lists);
}
