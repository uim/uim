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

#include <string.h>

#include "uim-internal.h"
#include "uim-scm.h"
#include "uim-scm-abbrev.h"


static uim_bool
string_equalp(uim_lisp x, uim_lisp y)
{
  return (strcmp(uim_scm_refer_c_str(x), uim_scm_refer_c_str(y)) == 0);
}

static uim_lisp
str_seq_equal(uim_lisp seq, uim_lisp rule)
{
  int sl = uim_scm_length(seq);
  int rl = uim_scm_length(rule);
  int i;
  if (sl != rl) {
    return uim_scm_f();
  }
  for (i = 0; i < sl; i++) {
    if (!string_equalp(uim_scm_car(seq), uim_scm_car(rule))) {
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
  int sl = uim_scm_length(seq);
  int rl = uim_scm_length(rule);
  int i;

  if (sl >= rl) {
    return uim_scm_f();
  }
  /* Obviously. sl < rl */
  for (i = 0; i < sl; i++) {
    if (!string_equalp(uim_scm_car(seq), uim_scm_car(rule))) {
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
    if (TRUEP(str_seq_equal(seq, key))) {
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
    if (TRUEP(str_seq_partial(seq, key))) {
      return rule;
    }
  }
  return uim_scm_f();
}

static uim_lisp
rk_find_partial_seqs(uim_lisp seq, uim_lisp rules)
{
  uim_lisp ret = uim_scm_null();

  for (; !uim_scm_nullp(rules); rules = uim_scm_cdr(rules)) {
    uim_lisp rule = uim_scm_car(rules);
    uim_lisp key = uim_scm_car(uim_scm_car(rule));
    if (TRUEP(str_seq_partial(seq, key))) {
      ret = uim_scm_cons(rule, ret);
    }
  }
  return uim_scm_callf("reverse", "o", ret);
}

/*
 * returns possible next characters
 * (rk-lib-expect-seq '("k" "y") ja-rk-rule) -> ("o" "e" "u" "i" "a")
 */
static uim_lisp
rk_expect_seq(uim_lisp seq, uim_lisp rules)
{
  uim_lisp cur, res = uim_scm_null();
  for (cur = rules; !uim_scm_nullp(cur); cur = uim_scm_cdr(cur)) {
    uim_lisp rule = uim_scm_car(cur);
    uim_lisp key = CAR(CAR(rule));
    uim_lisp e = str_seq_partial(seq, key);
    if (TRUEP(e)) {
      res = uim_scm_cons(e, res);
    }
  }
  return res;  /* don't return uim_scm_f() */
}

/*
 * returns #t if key is expected
 * (rk-lib-expect-seq-for-key '("k" "y") ja-rk-rule "o") -> #t
 * (rk-lib-expect-seq-for-key '("k" "y") ja-rk-rule "y") -> #f
 */
static uim_lisp
rk_expect_key_for_seq(uim_lisp seq, uim_lisp rules, uim_lisp key)
{
  uim_lisp cur;
  for (cur = rules; !uim_scm_nullp(cur); cur = uim_scm_cdr(cur)) {
    uim_lisp rule = uim_scm_car(cur);
    uim_lisp seq_in_rule = CAR(CAR(rule));
    uim_lisp e = str_seq_partial(seq, seq_in_rule);
    if (TRUEP(e) && string_equalp(e, key)) {
      return uim_scm_t();
    }
  }
  return uim_scm_f();
}


void
uim_init_rk_subrs(void)
{
  uim_scm_init_proc2("str-seq-equal?", str_seq_equal);
  uim_scm_init_proc2("str-seq-partial?", str_seq_partial);
  uim_scm_init_proc2("rk-lib-find-seq", rk_find_seq);
  uim_scm_init_proc2("rk-lib-find-partial-seq", rk_find_partial_seq);
  uim_scm_init_proc2("rk-lib-find-partial-seqs", rk_find_partial_seqs);
  uim_scm_init_proc2("rk-lib-expect-seq", rk_expect_seq);
  uim_scm_init_proc3("rk-lib-expect-key-for-seq?", rk_expect_key_for_seq);
}
