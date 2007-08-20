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
#include "uim-scm-abbrev.h"
#include "uim-util.h"

static uim_lisp protected;

/* define constants as procedure to ensure unmodifiable */
static uim_lisp
uim_version()
{
  return uim_scm_make_str(PACKAGE_VERSION);
}

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

/* Limited version of SRFI-13 string-contains. The number of args are
 * fixed to 3. */
static uim_lisp
string_contains(uim_lisp s1_, uim_lisp s2_, uim_lisp start1_)
{
  const char *s1, *s2, *found;
  int start1;
  size_t s1len;

  if (!uim_scm_stringp(s1_) || !uim_scm_stringp(s2_))
    return uim_scm_f();  /* FIXME: uim_scm_error() */

  s1 = uim_scm_refer_c_str(s1_);
  s2 = uim_scm_refer_c_str(s2_);
  start1 = uim_scm_c_int(start1_);
  s1len = strlen(s1);

  if (start1 < 0 || s1len < (size_t)start1)
    return uim_scm_f();  /* FIXME: uim_scm_error() */

  found = strstr(&s1[start1], s2);

  return (found) ? uim_scm_make_int(found - s1) : uim_scm_f();
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
  name = uim_scm_refer_c_str(lang_name);

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
  name = uim_scm_refer_c_str(lang_code);

  UIM_CATCH_ERROR_END();

  return name;
}

static uim_lisp
setugidp(void)
{
  assert(uim_scm_gc_any_contextp());

  return uim_scm_make_bool(uim_issetugid());
}

void
uim_init_util_subrs(void)
{
  protected = uim_scm_f();
  uim_scm_gc_protect(&protected);

  uim_scm_init_subr_0("uim-version", uim_version);

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

  /* SRFI-13 */
  uim_scm_init_subr_3("string-contains", string_contains);
  uim_scm_init_subr_2("string-prefix?", string_prefixp);
  uim_scm_init_subr_2("string-prefix-ci?", string_prefix_cip);
}
