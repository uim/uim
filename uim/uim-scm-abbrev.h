/*

  Copyright (c) 2007-2013 uim Project https://github.com/uim/uim

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

#ifndef UIM_SCM_ABBREV_H
#define UIM_SCM_ABBREV_H

#ifdef __cplusplus
extern "C" {
#endif

#define FOR_EACH    UIM_SCM_FOR_EACH

#define ERROR       uim_scm_error
#define ERROR_OBJ   uim_scm_error_obj
#define ENSURE      UIM_SCM_ENSURE
#define ENSURE_OBJ  UIM_SCM_ENSURE_OBJ
#define ENSURE_TYPE UIM_SCM_ENSURE_TYPE

#define C_BOOL uim_scm_c_bool
#define C_INT  uim_scm_c_int
#define C_CHAR uim_scm_c_char
#define C_STR  uim_scm_c_str
#define REFER_C_STR uim_scm_refer_c_str
#define C_SYM  uim_scm_c_symbol
#define C_PTR  uim_scm_c_ptr
#define C_FPTR uim_scm_c_func_ptr

#define MAKE_BOOL uim_scm_make_bool
#define MAKE_INT  uim_scm_make_int
#define MAKE_CHAR uim_scm_make_char
#define MAKE_STR  uim_scm_make_str
#define MAKE_STR_DIRECTLY uim_scm_make_str_directly
#define MAKE_SYM  uim_scm_make_symbol
#define MAKE_PTR  uim_scm_make_ptr
#define MAKE_FPTR uim_scm_make_func_ptr

#define INTP      uim_scm_intp
#define CHARP     uim_scm_charp
#define VECTORP   uim_scm_vectorp
#define STRP      uim_scm_strp
#define SYMP      uim_scm_symbolp
#define PTRP      uim_scm_ptrp
#define FPTRP     uim_scm_func_ptrp
#define NULLP     uim_scm_nullp
#define CONSP     uim_scm_consp
#define LISTP     uim_scm_listp    /* does not detect circular list */
#define TRUEP     uim_scm_truep    /* (if obj #t #f) */
#define FALSEP    uim_scm_falsep
#define EQ        uim_scm_eq

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

#define VECTOR_REF uim_scm_vector_ref
#define VECTOR_SET uim_scm_vector_set

#ifdef __cplusplus
}
#endif
#endif /* UIM_SCM_ABBREV_H */
