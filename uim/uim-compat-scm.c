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

#include "siod.h"
#include "uim-compat-scm.h"
#include "context.h"


static uim_lisp quote_sym;


long
uim_scm_repl_c_string(char *str, long want_init, long want_print)
{
  return repl_c_string(str, want_init, want_print);
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
  return uim_scm_make_int(integer);
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
  return uim_scm_make_str(str);
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
  return uim_scm_make_symbol(str);
}

uim_lisp
uim_scm_qintern_c_str(const char *str)
{
  return uim_scm_quote(uim_scm_intern_c_str(str));
}

uim_lisp
uim_scm_quote(uim_lisp obj) {
  return uim_scm_list2(quote_sym, obj);
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

void
uim_init_compat_scm_subrs(void)
{
  quote_sym = uim_scm_intern_c_str("quote");
}
