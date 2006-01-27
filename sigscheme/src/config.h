/*===========================================================================
 *  FileName : config.h
 *  About    : build configuration file
 *
 *  Copyright (C) 2005-2006 Kazuki Ohta <mover AT hct.zaq.ne.jp>
 *
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions
 *  are met:
 *
 *  1. Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *  3. Neither the name of authors nor the names of its contributors
 *     may be used to endorse or promote products derived from this software
 *     without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS IS''
 *  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 *  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 *  ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR CONTRIBUTORS BE
 *  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 *  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
 *  GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 *  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 *  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 *  OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 *  SUCH DAMAGE.
===========================================================================*/
#ifndef __SIGSCHEME_CONFIG_H
#define __SIGSCHEME_CONFIG_H

#ifdef HAVE_CONFIG_H
#  include "../config.h"
#endif

/*===========================================================================
  Optional Features Written in C
===========================================================================*/
#define SCM_USE_DEEP_CADRS      1  /* use all c[ad]+r defined in R5RS */
#define SCM_USE_NONSTD_FEATURES 1  /* use Non-R5RS standard features such as "require" */

#define SCM_USE_SRFI1           1  /* use SRFI-1  list library */
#define SCM_USE_SRFI2           1  /* use SRFI-2  'and-let*' */
#define SCM_USE_SRFI6           1  /* use SRFI-6  basic string ports */
#define SCM_USE_SRFI8           1  /* use SRFI-8  'receive' */
#define SCM_USE_SRFI22          1  /* use SRFI-22 running scheme scripts on Unix */
#define SCM_USE_SRFI23          1  /* use SRFI-23 'error' */
#define SCM_USE_SRFI34          1  /* use SRFI-34 exception handling for programs */
#define SCM_USE_SRFI38          1  /* use SRFI-38 'write-with-shared-structure' */
#define SCM_USE_SRFI60          1  /* use SRFI-60 integers as bits */
#define SCM_USE_SRFI75_NAMED_CHARS 1  /* use named characters of SRFI-75 R6RS unicode data */
#define SCM_USE_SRFI75          1  /* use SRFI-75 R6RS unicode data */

#define SCM_COMPAT_SIOD         1  /* use SIOD compatible features */
#define SCM_COMPAT_SIOD_BUGS    1  /* emulate the buggy behaviors of SIOD */

/*===========================================================================
  Character Encoding Handlers
===========================================================================*/
/* Support for each encoding will be compiled in if the corresponding
 * macro is defined as nonzero. */
#define SCM_USE_UTF8            1
#define SCM_USE_EUCCN           1
#define SCM_USE_EUCJP           1
#define SCM_USE_EUCKR           1
#define SCM_USE_SJIS            1

/* choose exclusively. fallbacks to the unibyte encoding if nothing chosen. */
#define SCM_USE_UTF8_AS_DEFAULT  1
#define SCM_USE_EUCCN_AS_DEFAULT 0
#define SCM_USE_EUCJP_AS_DEFAULT 0
#define SCM_USE_EUCKR_AS_DEFAULT 0
#define SCM_USE_SJIS_AS_DEFAULT  0

/*===========================================================================
  Internal Behaviors
===========================================================================*/
#define SCM_STRICT_R5RS         0  /* use strict R5RS check */
#define SCM_STRICT_NULL_FORM    0  /* disallow quote-less () */
#define SCM_STRICT_VECTOR_FORM  1  /* disallow quote-less vector literal */
#define SCM_STRICT_ARGCHECK     1  /* enable strict argument check */
#define SCM_STRICT_DEFINE_PLACEMENT 1 /* reject invalid internal definitions */
#define SCM_STRICT_ENCODING_CHECK 1 /* do all feasible encoding error checks */
#define SCM_CONST_LIST_LITERAL  1  /* make list literal immutable */
#define SCM_CONST_VECTOR_LITERAL 1 /* make vector literal immutable */
#define SCM_ACCESSOR_ASSERT     0  /* enable strict type check with accessor */
#define SCM_USE_VALUECONS       1  /* use experimental values passing */
#define SCM_VOLATILE_OUTPUT     0  /* always flush files on write */
#define SCM_USE_NULL_CAPABLE_STRING 1  /* enable experimental null character in a middle of a string */
#define SCM_OBJ_COMPACT         0  /* object representation compaction (experimental) */

#define SCM_GCC4_READY_GC       1  /* use experimental gcc4-ready stack protection */

/*===========================================================================
  Memory configurations
===========================================================================*/
/* on-stack initial token buffer size for parser */
#define SCM_INITIAL_STRING_BUF_SIZE 64
#define SCM_INITIAL_SYMBOL_BUF_SIZE 64

/* token buffer size extender function */
#define SCM_LBUF_F_STRING scm_lbuf_f_linear
#define SCM_LBUF_F_SYMBOL scm_lbuf_f_linear

#define SCM_DEFAULT_HEAP_SIZE            0x4000
#define SCM_DEFAULT_HEAP_ALLOC_THRESHOLD (SCM_DEFAULT_HEAP_SIZE / 2)
#undef  SCM_DEFAULT_N_HEAPS_MAX
#define SCM_DEFAULT_N_HEAPS_INIT         1
#define SCM_DEFAULT_SYMBOL_HASH_SIZE     0x400

#define SCM_USE_64BIT_FIXNUM    0 /* use int64_t  as scm_int_t */
#define SCM_USE_32BIT_FIXNUM    0 /* use int32_t  as scm_int_t */
#define SCM_USE_INT_FIXNUM      0 /* use int      as scm_int_t */
#define SCM_USE_LONG_FIXNUM     0 /* use long     as scm_int_t (default) */

#define SCM_USE_64BIT_SCMREF    0 /* use int64_t  as scm_intref_t */
#define SCM_USE_32BIT_SCMREF    0 /* use int32_t  as scm_intref_t */
#define SCM_USE_INTPTR_SCMREF   0 /* use intptr_t as scm_intref_t (default) */

/*===========================================================================
  Debugging
===========================================================================*/
#define SCM_DEBUG               1  /* enable debugging features */
#define SCM_CHICKEN_DEBUG       1  /* allow survival recovery */
#define SCM_DEBUG_GC            0  /* enable GC debugging */
#define SCM_DEBUG_PORT          0  /* enable port debugging */
#define SCM_DEBUG_PARSER        0  /* enable parser debugging */
#define SCM_DEBUG_ENCODING      0  /* debug encoding-related functions */
#define SCM_DEBUG_BACKTRACE_SEP 1  /* enable frame-separator on backtrace */
#define SCM_DEBUG_BACKTRACE_VAL 1  /* enable values printing on backtrace */

/*===========================================================================
  Platform Dependency
===========================================================================*/
#define SCM_NEWLINE_STR         "\n"   /* UNIX flavors */
#if 0
#define SCM_NEWLINE_STR         "\r\n" /* Windows/DOS */
#define SCM_NEWLINE_STR         "\r"   /* Mac OS */
#endif

/*===========================================================================
  Dependency Resolution
===========================================================================*/
/* FIXME: provide replace functions */
#if (!HAVE_ASPRINTF || !HAVE_VASPRINTF)
#error "This platform is not supported yet"
#endif

#if (!SCM_DEBUG && SCM_USE_NULL_CAPABLE_STRING)
#error "Don't enable dangerous SCM_USE_NULL_CAPABLE_STRING for production code"
#endif

#if SCM_STRICT_R5RS
#undef SCM_STRICT_NULL_FORM
#define SCM_STRICT_NULL_FORM    1
#undef SCM_STRICT_VECTOR_FORM
#define SCM_STRICT_VECTOR_FORM  1
#undef SCM_STRICT_ARGCHECK
#define SCM_STRICT_ARGCHECK     1
#undef SCM_STRICT_DEFINE_PLACEMENT
#define SCM_STRICT_DEFINE_PLACEMENT 1
#endif /* SCM_STRICT_R5RS */

#if SCM_COMPAT_SIOD
#undef SCM_USE_NONSTD_FEATURES
#define SCM_USE_NONSTD_FEATURES 1
#undef SCM_USE_SRFI60
#define SCM_USE_SRFI60          1
#else /* SCM_COMPAT_SIOD */
#undef SCM_COMPAT_SIOD_BUGS
#define SCM_COMPAT_SIOD_BUGS    0
#endif /* SCM_COMPAT_SIOD */

#if SCM_USE_SRFI34
#undef SCM_USE_SRFI23
#define SCM_USE_SRFI23          1
#endif /* SCM_USE_SRFI34 */

#if SCM_USE_SRFI75
#define SCM_USE_SRFI75_NAMED_CHARS 1
#endif

#if SCM_DEBUG
#undef SCM_VOLATILE_OUTPUT
#define SCM_VOLATILE_OUTPUT     1
#endif /* SCM_DEBUG */

#if SCM_OBJ_COMPACT
#undef SCM_USE_VALUECONS
#define SCM_USE_VALUECONS       0
#endif /* SCM_OBJ_COMPACT */

#if (SCM_USE_UTF8 || SCM_USE_EUCCN || SCM_USE_EUCJP || SCM_USE_EUCKR || SCM_USE_SJIS)
#define SCM_USE_MULTIBYTE_CHAR  1
#else
#define SCM_USE_MULTIBYTE_CHAR  0
#endif

#if (   SCM_USE_UTF8_AS_DEFAULT  && !SCM_USE_UTF8                            \
     || SCM_USE_EUCCN_AS_DEFAULT && !SCM_USE_EUCCN                           \
     || SCM_USE_EUCJP_AS_DEFAULT && !SCM_USE_EUCJP                           \
     || SCM_USE_EUCKR_AS_DEFAULT && !SCM_USE_EUCKR                           \
     || SCM_USE_SJIS_AS_DEFAULT  && !SCM_USE_SJIS)
#error "disabled character encoding is chosen as default"
#endif

/* for scm_eval_c_string_internal() */
#undef SCM_USE_SRFI6
#define SCM_USE_SRFI6           1

#endif /* __SIGSCHEME_CONFIG_H */
