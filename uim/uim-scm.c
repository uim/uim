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
#include "siod.h"
#include "uim-scm.h"
#include "uim-compat-scm.h"
#include "context.h"

#if 0
/*
  To avoid namespace pollution, all siod functions should be static
  and wrapped into uim-scm.c by direct inclusion rather than linked
  via public symbols. After uim_scm_* abstraction, the Scheme
  interpreter implementation can be switched to another one such as
  uim-scm-tinyscheme.c or uim-scm-gauche.c. But uim/*.[hc] and
  scm/*.scm are still depending on siod in several ways. At least full
  test suite for *.scm files are required to migrate to another Scheme
  implementation.  -- YamaKen 2004-12-21
*/
#include "slib.c"
#endif

#if 1
/* will be deprecated. use uim_scm_t() and uim_scm_f() for new design */
uim_lisp true_sym;
uim_lisp false_sym;
#endif


uim_bool
uim_scm_c_bool(uim_lisp val)
{
  return NFALSEP(val);
}

uim_lisp
uim_scm_make_bool(uim_bool val)
{
  return (val) ? uim_scm_t() : uim_scm_f();
}

int
uim_scm_c_int(uim_lisp integer)
{
  return get_c_int((LISP)integer);
}

uim_lisp
uim_scm_make_int(int integer)
{
  return (uim_lisp)intcons(integer);
}

char *
uim_scm_c_str(uim_lisp str)
{
  return strdup(get_c_string((LISP)str));
}

uim_lisp
uim_scm_make_str(const char *str)
{
  int unknown_strlen = -1;
  return (uim_lisp)strcons(unknown_strlen, str);
}

char *
uim_scm_c_symbol(uim_lisp symbol)
{
  /* siod dependent */
  return uim_scm_c_str(symbol);
}

uim_lisp
uim_scm_make_symbol(const char *str)
{
  return (uim_lisp)rintern(str);
}

void
uim_scm_gc_protect(uim_lisp *location)
{
  gc_protect((LISP *)location);
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
  return NULLP((LISP)obj);
}

int
uim_scm_eq(uim_lisp a, uim_lisp b) {
  return EQ(a, b);
}

int
uim_scm_string_equal(uim_lisp a, uim_lisp b) {
  uim_lisp form, p;
  form = uim_scm_list3(uim_scm_make_symbol("string=?"),
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
uim_scm_eval_c_string(const char *str) {
  repl_c_string((char *)str, 0, 0);
  return uim_scm_return_value();
}

uim_lisp
uim_scm_return_value(void)
{
  return (uim_lisp)siod_return_value();
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

void
uim_scm_init_subr_0(char *name, uim_lisp (*fcn)(void))
{
  init_subr(name, tc_subr_0, (SUBR_FUNC)fcn);
}

void
uim_scm_init_subr_1(char *name, uim_lisp (*fcn)(uim_lisp))
{
  init_subr(name, tc_subr_1, (SUBR_FUNC)fcn);
}

void
uim_scm_init_subr_2(char *name, uim_lisp (*fcn)(uim_lisp, uim_lisp))
{
  init_subr(name, tc_subr_2, (SUBR_FUNC)fcn);
}

void
uim_scm_init_subr_3(char *name, uim_lisp (*fcn)(uim_lisp, uim_lisp, uim_lisp))
{
  init_subr(name, tc_subr_3, (SUBR_FUNC)fcn);
}

void
uim_scm_init_subr_4(char *name, uim_lisp (*fcn)(uim_lisp, uim_lisp, uim_lisp,
						uim_lisp))
{
  init_subr(name, tc_subr_4, (SUBR_FUNC)fcn);
}

void
uim_scm_init_subr_5(char *name, uim_lisp (*fcn)(uim_lisp, uim_lisp, uim_lisp,
						uim_lisp, uim_lisp))
{
  init_subr(name, tc_subr_5, (SUBR_FUNC)fcn);
}

void
uim_init_scm_subrs()
{
  uim_scm_gc_protect(&true_sym);
  uim_scm_gc_protect(&false_sym);

  true_sym  = (uim_lisp)siod_true_value();
#if 0
  false_sym = (uim_lisp)siod_false_value();
#else
  /* false_sym has to be NIL until bug #617 and #642 are fixed
   * -- YamaKen
   */
  false_sym = (uim_lisp)NIL;
#endif
}
