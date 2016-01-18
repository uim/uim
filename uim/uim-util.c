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

#include <config.h>

#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "uim-internal.h"
#include "uim-scm.h"
#include "uim-scm-abbrev.h"
#include "uim-util.h"

static uim_lisp protected;

/* define constants as procedure to ensure unmodifiable */
static uim_lisp
uim_version()
{
  return MAKE_STR(PACKAGE_VERSION);
}

static uim_lisp
sys_libdir()
{
  return MAKE_STR(LIBDIR);
}

static uim_lisp
sys_pkglibdir()
{
  return MAKE_STR(PKGLIBDIR);
}

static uim_lisp
sys_datadir()
{
  return MAKE_STR(DATADIR);
}

static uim_lisp
sys_pkgdatadir()
{
  return MAKE_STR(PKGDATADIR);
}

/* Limited version of SRFI-13 string-contains. The number of args are
 * fixed to 3. */
static uim_lisp
string_contains(uim_lisp s1_, uim_lisp s2_, uim_lisp start1_)
{
  const char *s1, *s2, *found;
  long start1;
  size_t s1len;

  s1 = REFER_C_STR(s1_);
  s2 = REFER_C_STR(s2_);
  start1 = C_INT(start1_);
  s1len = strlen(s1);

  if (start1 < 0 || s1len < (size_t)start1)
    ERROR("string-contains: invalid range");

  found = strstr(&s1[start1], s2);

  return (found) ? MAKE_INT(found - s1) : uim_scm_f();
}

static uim_lisp
string_prefixp_internal(uim_lisp prefix_, uim_lisp str_,
			int (*cmp)(const char *, const char *, size_t))
{
  const char *prefix, *str;
  size_t len;

  prefix = REFER_C_STR(prefix_);
  str = REFER_C_STR(str_);
  len = strlen(prefix);

  return MAKE_BOOL((*cmp)(prefix, str, len) == 0);
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

/* Limited version of SRFI-43 vector-copy. Only accepts 1st arg. */
static uim_lisp
vector_copy(uim_lisp src)
{
  long len, i;
  uim_lisp elm, copied;

  len = uim_scm_vector_length(src);
  copied = uim_scm_callf("make-vector", "l", len);
  for (i = 0; i < len; i++) {
    elm = VECTOR_REF(src, i);
    VECTOR_SET(copied, i, elm);
  }

  return copied;
}

const char *
uim_get_language_name_from_locale(const char *locale)
{
  uim_lisp lang_code, lang_name;
  const char *name;

  if (UIM_CATCH_ERROR_BEGIN())
    return "-";

  assert(uim_scm_gc_any_contextp());
  assert(locale);

  /* Performs adhoc "zh_TW:zh_HK" style locale handling as temporary
   * specification of this function for backward compatibility. */
  protected =
    lang_code = uim_scm_callf("langgroup-primary-lang-code", "s", locale);
  protected =
    lang_name = uim_scm_callf("lang-code->lang-name", "o", lang_code);
  name = REFER_C_STR(lang_name);

  UIM_CATCH_ERROR_END();

  return name;
}

const char *
uim_get_language_code_from_language_name(const char *language_name)
{
  uim_lisp lang_code;
  const char *name;

  if (UIM_CATCH_ERROR_BEGIN())
    return "-";

  assert(uim_scm_gc_any_contextp());
  assert(language_name);

  protected =
    lang_code = uim_scm_callf("lang-name->lang-code", "s", language_name);
  name = REFER_C_STR(lang_code);

  UIM_CATCH_ERROR_END();

  return name;
}

void
uim_init_util_subrs(void)
{
  protected = uim_scm_f();
  uim_scm_gc_protect(&protected);

  uim_scm_init_proc0("uim-version", uim_version);

  uim_scm_init_proc0("sys-libdir", sys_libdir);
  uim_scm_init_proc0("sys-pkglibdir", sys_pkglibdir);
  uim_scm_init_proc0("sys-datadir", sys_datadir);
  uim_scm_init_proc0("sys-pkgdatadir", sys_pkgdatadir);

  /* SRFI-13 */
  uim_scm_init_proc3("string-contains", string_contains);
  uim_scm_init_proc2("string-prefix?", string_prefixp);
  uim_scm_init_proc2("string-prefix-ci?", string_prefix_cip);

  /* SRFI-43 */
  uim_scm_init_proc1("vector-copy", vector_copy);
}
