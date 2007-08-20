/*

  Copyright (c) 2007 uim Project http://code.google.com/p/uim/

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

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS
  IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
  THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
  PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR
  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
  OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
  WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
  OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
  ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

*/

#ifndef _uim_scm_abbrev_h_included_
#define _uim_scm_abbrev_h_included_

#ifdef __cplusplus
extern "C" {
#endif

#define MAKE_BOOL uim_scm_make_bool
#define MAKE_INT  uim_scm_make_int
#define MAKE_CHAR uim_scm_make_char
#define MAKE_STR  uim_scm_make_str
#define MAKE_STR_DIRECTLY uim_scm_make_str_directly
#define MAKE_SYM  uim_scm_make_symbol
#define MAKE_PTR  uim_scm_make_ptr
#define MAKE_FPTR uim_scm_make_func_ptr

#define INTP      uim_scm_integerp
#define CHARP     uim_scm_charp
#define STRP      uim_scm_stringp
#define SYMP      uim_scm_symbolp
#define PTRP      uim_scm_ptrp
#define FPTRP     uim_scm_func_ptrp
#define NULLP     uim_scm_nullp
#define CONSP     uim_scm_consp

#define CAR       uim_scm_car
#define CDR       uim_scm_cdr
#define SET_CAR   uim_scm_set_car
#define SET_CDR   uim_scm_set_cdr
#define CONS      uim_scm_cons
#define LIST1     uim_scm_list1
#define LIST2     uim_scm_list2
#define LIST3     uim_scm_list3
#define LIST4     uim_scm_list4
#define LIST5     uim_scm_list5
#define QUOTE     uim_scm_quote

#ifdef __cplusplus
}
#endif
#endif /* _uim_scm_abbrev_h_included_ */
