/*===========================================================================
 *  Filename : config-old.h
 *  About    : build configuration file (going to be removed)
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
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS
 *  IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
 *  THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 *  PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR
 *  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 *  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 *  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
 *  OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 *  WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
 *  OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
 *  ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
===========================================================================*/

/* FIXME: port all things into configure.in and remove this file */

#ifndef __SIGSCHEME_CONFIG_OLD_H
#define __SIGSCHEME_CONFIG_OLD_H

/*===========================================================================
  R5RS Features
===========================================================================*/

/*===========================================================================
  Optional Features
===========================================================================*/

/*===========================================================================
  Character Encoding Handlers
===========================================================================*/

/*===========================================================================
  Internal Behaviors
===========================================================================*/

/*===========================================================================
  Storage configurations
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


/*===========================================================================
  Debugging
===========================================================================*/
#define SCM_CHICKEN_DEBUG       1  /* allow survival recovery */
#define SCM_DEBUG_GC            0  /* enable GC debugging */
#define SCM_DEBUG_PORT          0  /* enable port debugging */
#define SCM_DEBUG_PARSER        0  /* enable parser debugging */
#define SCM_DEBUG_ENCODING      0  /* debug encoding-related functions */
#define SCM_DEBUG_BACKTRACE_SEP 0  /* enable frame-separator on backtrace */
#define SCM_DEBUG_BACKTRACE_VAL 1  /* enable values printing on backtrace */
#define SCM_DEBUG_MACRO         1  /* debug macro and pattern matching */

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
#if ((defined(__SYMBIAN32__) && !defined(EKA2)) \
     || BREW_MAJ_VER)  /* FIXME: inappropriate detection method */
#define SCM_HAVE_WRITABLE_GLOBAL_VARS 0
#else
#define SCM_HAVE_WRITABLE_GLOBAL_VARS 1
#endif

#if SCM_HAVE_WRITABLE_GLOBAL_VARS
#if !SCM_USE_AGGREGATED_GLOBAL_VARS
#define SCM_USE_AGGREGATED_GLOBAL_VARS 0
#endif /* !SCM_USE_AGGREGATED_GLOBAL_VARS */
#else /* SCM_HAVE_WRITABLE_GLOBAL_VARS */
#undef SCM_USE_AGGREGATED_GLOBAL_VARS
#define SCM_USE_AGGREGATED_GLOBAL_VARS 1
#endif /* SCM_HAVE_WRITABLE_GLOBAL_VARS */

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

#if (SCM_USE_READER || SCM_USE_WRITER)
#undef SCM_USE_PORT
#define SCM_USE_PORT            1
#endif /* (SCM_USE_READER || SCM_USE_WRITER) */

#if SCM_USE_WRITER
#undef SCM_USE_RAW_C_FORMAT
#define SCM_USE_RAW_C_FORMAT    1
#endif /* SCM_USE_WRITER */

#if SCM_USE_SYNTAX_CASE
#undef SCM_USE_UNHYGIENIC_MACRO
#define SCM_USE_UNHYGIENIC_MACRO 1
#endif /* SCM_USE_SYNTAX_CASE */

#if SCM_USE_SSCM_FORMAT_EXTENSION
#undef SCM_USE_SRFI48
#define SCM_USE_SRFI48          1
#endif /* SCM_USE_SSCM_FORMAT_EXTENSION */
#if SCM_USE_SRFI48
#undef SCM_USE_SRFI28
#define SCM_USE_SRFI28          1
#endif /* USE_SRFI48 */

#if SCM_COMPAT_SIOD
#undef SCM_USE_SSCM_EXTENSIONS
#define SCM_USE_SSCM_EXTENSIONS 1
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

#if SCM_USE_STORAGE_COMPACT
#undef SCM_USE_VALUECONS
#define SCM_USE_VALUECONS       0
#endif /* SCM_USE_STORAGE_COMPACT */

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

#if SCM_USE_EVAL_C_STRING
#undef SCM_USE_SRFI6
#define SCM_USE_SRFI6           1
#endif


/*===========================================================================
  New configuration variables (FIXME: autoconf support needed)
===========================================================================*/
/* FIXME: remove SCM_STRICT_DEFINE_PLACEMENT and replace with
 * SCM_USE_INTERNAL_DEFINITIONS */
#if SCM_STRICT_DEFINE_PLACEMENT
#undef SCM_STRICT_DEFINE_PLACEMENT
#define SCM_USE_INTERNAL_DEFINITIONS 1
#endif

#define SCM_STRICT_TOPLEVEL_DEFINITIONS 1

#define SCM_USE_BACKTRACE 1  /* does not require SCM_DEBUG */
/* SCM_DEBUG requires SCM_USE_FORMAT */
/* SCM_DEBUG_BACKTRACE_VAL requires SCM_USE_FORMAT */

#endif /* __SIGSCHEME_CONFIG_OLD_H */
