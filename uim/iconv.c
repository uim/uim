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
#include <iconv.h>
#include <assert.h>
#include <errno.h>

#ifdef HAVE_ALLOCA_H
# include <alloca.h>
#endif

#include "uim.h"
#include "uim-internal.h"
#include "uim-util.h"
#include "uim-scm.h"
#include "uim-scm-abbrev.h"
#include "uim-iconv.h"

#define MBCHAR_LEN_MAX 6  /* assumes CESU-8 */

static void *uim_iconv_open(const char *tocode, const char *fromcode);
static int uim_iconv_is_convertible(const char *tocode, const char *fromcode);
static void *uim_iconv_create(const char *tocode, const char *fromcode);
static char *uim_iconv_code_conv(void *obj, const char *str);
static void uim_iconv_release(void *obj);

static int check_encoding_equivalence(const char *tocode,
                                      const char *fromcode);
static const char **uim_get_encoding_alias(const char *encoding);


static struct uim_code_converter uim_iconv_tbl = {
  uim_iconv_is_convertible,
  uim_iconv_create,
  uim_iconv_code_conv,
  uim_iconv_release
};
struct uim_code_converter *uim_iconv = &uim_iconv_tbl;

#include "encoding-table.c"


static int
check_encoding_equivalence(const char *tocode, const char *fromcode)
{
  const char **alias_tocode;
  const char **alias_fromcode;
  int i, j;
  int alias_tocode_alloced = 0;
  int alias_fromcode_alloced = 0;
  int found = 0;

  assert(tocode);
  assert(fromcode);

  alias_tocode = uim_get_encoding_alias(tocode);
  alias_fromcode = uim_get_encoding_alias(fromcode);

  if (!alias_tocode) {
    alias_tocode = uim_malloc(sizeof(char *) * 2);
    alias_tocode[0] = tocode;
    alias_tocode[1] = NULL;
    alias_tocode_alloced = 1;
  }
  if (!alias_fromcode) {
    alias_fromcode = uim_malloc(sizeof(char *) * 2);
    alias_fromcode[0] = fromcode;
    alias_fromcode[1] = NULL;
    alias_fromcode_alloced = 1;
  }

  for (i = 0; alias_tocode[i]; i++) {
    for (j = 0; alias_fromcode[j]; j++) {
      if (!strcmp(alias_tocode[i], alias_fromcode[j])) {
        found = 1;
	break;
      }
    }
    if (found)
      break;
  }

  if (alias_tocode_alloced)
    free(alias_tocode);
  if (alias_fromcode_alloced)
    free(alias_fromcode);
  return found;
}

static int
uim_iconv_is_convertible(const char *tocode, const char *fromcode)
{
  iconv_t ic;
  uim_bool result;

  if (UIM_CATCH_ERROR_BEGIN())
    return UIM_FALSE;

  assert(tocode);
  assert(fromcode);

  do {
    if (check_encoding_equivalence(tocode, fromcode)) {
      result = UIM_TRUE;
      break;
    }

    /* TODO cache the result */
    ic = (iconv_t)uim_iconv_open(tocode, fromcode);
    if (ic == (iconv_t)-1) {
      result = UIM_FALSE;
      break;
    }
    iconv_close(ic);
    result = UIM_TRUE;
  } while (/* CONSTCOND */ 0);

  UIM_CATCH_ERROR_END();

  return result;
}

static const char **
uim_get_encoding_alias(const char *encoding)
{
  int i, j;
  const char **alias;

  assert(encoding);

  for (i = 0; (alias = uim_encoding_list[i]); i++) {
    for (j = 0; alias[j]; j++) {
      if (!strcmp(alias[j], encoding))
        return alias;
    }
  }
  return NULL;
}

static void *
uim_iconv_open(const char *tocode, const char *fromcode)
{
  iconv_t cd = (iconv_t)-1;
  int i, j;
  const char **alias_tocode, **alias_fromcode;
  int alias_tocode_alloced = 0;
  int alias_fromcode_alloced = 0;
  int opened = 0;

  assert(tocode);
  assert(fromcode);

  alias_tocode = uim_get_encoding_alias(tocode);
  alias_fromcode = uim_get_encoding_alias(fromcode);

  if (!alias_tocode) {
    alias_tocode = uim_malloc(sizeof(char *) * 2);
    alias_tocode[0] = tocode;
    alias_tocode[1] = NULL;
    alias_tocode_alloced = 1;
  }
  if (!alias_fromcode) {
    alias_fromcode = uim_malloc(sizeof(char *) * 2);
    alias_fromcode[0] = fromcode;
    alias_fromcode[1] = NULL;
    alias_fromcode_alloced = 1;
  }

  for (i = 0; alias_tocode[i]; i++) {
    for (j = 0; alias_fromcode[j]; j++) {
      cd = iconv_open(alias_tocode[i], alias_fromcode[j]);
      if (cd != (iconv_t)-1) {
	opened = 1;
	break;
      }
    }
    if (opened)
      break;
  }

  if (alias_tocode_alloced)
    free(alias_tocode);
  if (alias_fromcode_alloced)
    free(alias_fromcode);
  return (void *)cd;
}

static void *
uim_iconv_create(const char *tocode, const char *fromcode)
{
  iconv_t ic;

  if (UIM_CATCH_ERROR_BEGIN())
    return NULL;

  assert(tocode);
  assert(fromcode);

  do {
    if (check_encoding_equivalence(tocode, fromcode)) {
      ic = (iconv_t)0;
      break;
    }

    ic = (iconv_t)uim_iconv_open(tocode, fromcode);
    if (ic == (iconv_t)-1) {
      /* since iconv_t is not explicit pointer, use 0 instead of NULL */
      ic = (iconv_t)0;
    }
  } while (/* CONSTCOND */ 0);

  UIM_CATCH_ERROR_END();

  return (void *)ic;
}

static char *
uim_iconv_code_conv(void *obj, const char *instr)
{
  iconv_t cd = (iconv_t)obj;
  size_t ins;
  const char *in;
  size_t outbufsiz, outs;
  char   *outbuf = NULL, *out;
  size_t ret = 0;
  size_t nconv = 0;
  size_t idx = 0;
  char *str = NULL;

  if (UIM_CATCH_ERROR_BEGIN())
    return NULL;

  if (!instr)
    goto err;

  if (!obj) {
    UIM_CATCH_ERROR_END();
    return uim_strdup(instr);
  }

  ins = strlen(instr);
  in = instr;

  outbufsiz = (ins + sizeof("")) * MBCHAR_LEN_MAX;
  out = outbuf = uim_malloc(outbufsiz);

  while (ins > 0) {
    out = outbuf;
    outs = outbufsiz;

    ret = iconv(cd, (ICONV_CONST char **)&in, &ins, &out, &outs);
    nconv = outbufsiz - outs;
    if (ret == (size_t)-1) {
      switch (errno) {
      case EINVAL:
	goto err;
      case E2BIG:
	outbufsiz *= 2;
	out = uim_realloc(outbuf, outbufsiz);
	outbuf = out;
	break;
      default:
	goto err;
      }
    } else {
      /* XXX: irreversible characters */
    }
    if (nconv > 0) {
      if (str == NULL)
	str = uim_malloc(nconv + 1);
      else
	str = uim_realloc(str, idx + nconv + 1);
      memcpy(&str[idx], outbuf, nconv);
      idx += nconv;
    }
  }
  do {
    out = outbuf;
    outs = outbufsiz;

    ret = iconv(cd, NULL, NULL, &out, &outs);
    nconv = outbufsiz - outs;

    if (ret == (size_t)-1) {
      outbufsiz *= 2;
      out = uim_realloc(outbuf, outbufsiz);
      outbuf = out;
    } else {
      /* XXX: irreversible characters */
    }
    if (nconv > 0) {
      if (str == NULL)
	str = uim_malloc(nconv + 1);
      else
	str = uim_realloc(str, idx + nconv + 1);
      memcpy(&str[idx], outbuf, nconv);
      idx += nconv;
    }
  } while (ret == (size_t)-1);

  if (str == NULL)
    str = uim_strdup("");
  else
    str[idx] = '\0';
  free(outbuf);

  UIM_CATCH_ERROR_END();

  return str;

 err:

  free(str);
  free(outbuf);

  UIM_CATCH_ERROR_END();

  return uim_strdup("");
}

static void
uim_iconv_release(void *obj)
{
  if (UIM_CATCH_ERROR_BEGIN())
    return;

  if (obj)
    iconv_close((iconv_t)obj);

  UIM_CATCH_ERROR_END();
}

static uim_lisp
uim_ext_iconv_open(uim_lisp tocode_, uim_lisp fromcode_)
{
  const char *tocode = REFER_C_STR(tocode_);
  const char *fromcode = REFER_C_STR(fromcode_);
  iconv_t ic;

  ic = uim_iconv_create(tocode, fromcode);
  if (!ic)
    return uim_scm_f();

  return MAKE_PTR(ic);
}

static uim_lisp
uim_ext_iconv_code_conv(uim_lisp ic_, uim_lisp inbuf_)
{
  char *outbuf;

  outbuf = uim_iconv_code_conv(C_PTR(ic_), REFER_C_STR(inbuf_));
  if (!outbuf)
    return uim_scm_f();

  return MAKE_STR_DIRECTLY(outbuf);
}

static uim_lisp
uim_ext_iconv_release(uim_lisp ic_)
{
  uim_iconv_release(C_PTR(ic_));
  return uim_scm_t();
}

void
uim_init_iconv_subrs(void)
{
  uim_scm_init_proc2("iconv-open", uim_ext_iconv_open);
  uim_scm_init_proc2("iconv-code-conv", uim_ext_iconv_code_conv);
  uim_scm_init_proc1("iconv-release", uim_ext_iconv_release);
}
