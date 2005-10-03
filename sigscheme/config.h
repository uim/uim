/*===========================================================================
 *  FileName : config.h
 *  About    : build configuration file
 *
 *  Copyright (C) 2005      by Kazuki Ohta (mover@hct.zaq.ne.jp)
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

/*===========================================================================
  Character Encoding
===========================================================================*/
#define SCM_USE_EUCJP           1  /* use EUC-JP as internal encoding */

/*===========================================================================
  SRFI (Scheme Request for Implementation) procedures written in C
===========================================================================*/
#define SCM_USE_SRFI1           1  /* use SRFI-1  list library */
#define SCM_USE_SRFI2           1  /* use SRFI-2  'and-let*' */
#define SCM_USE_SRFI8           1  /* use SRFI-8  'receive' */
#define SCM_USE_SRFI23          1  /* use SRFI-23 'error' */
#define SCM_USE_SRFI34          1  /* use SRFI-34 exception handling for programs */
#define SCM_USE_SRFI38          1  /* use SRFI-38 'write-with-shared-structure' */
#define SCM_USE_SRFI60          1  /* use SRFI-60 integers as bits */

/*===========================================================================
  General Extensions
===========================================================================*/
#define SCM_USE_NONSTD_FEATURES 1  /* use Non-R5RS standard features such as "require" */
#define SCM_USE_DEEP_CADRS      1  /* use all c[ad]+r defined in R5RS */

/*===========================================================================
  SIOD (Scheme In One Defun) Compatiblity
===========================================================================*/
#define SCM_COMPAT_SIOD         1  /* use SIOD compatible features */
#define SCM_COMPAT_SIOD_BUGS    1  /* emulate the buggy behaviors of SIOD */

/*===========================================================================
  Miscellaneous
===========================================================================*/
#define SCM_STRICT_R5RS         0  /* use strict R5RS check */
#define SCM_STRICT_ARGCHECK     1  /* enable strict argument check */
#define SCM_ACCESSOR_ASSERT     0  /* enable strict type check with accessor */
#define SCM_GCC4_READY_GC       1  /* use experimental gcc4-ready stack protection */
#define SCM_USE_VALUECONS       1  /* use experimental values passing */
#define SCM_VOLATILE_OUTPUT     0  /* always flush files on write */

/*===========================================================================
  Debugging
===========================================================================*/
#define SCM_DEBUG               1  /* enable debugging features */
#define SCM_DEBUG_GC            0  /* enable GC debugging */
#define SCM_DEBUG_PARSER        0  /* enable parser debugging */
#define SCM_DEBUG_BACKTRACE_SEP 1  /* enable frame-separator on backtrace */
#define SCM_DEBUG_BACKTRACE_VAL 1  /* enable values printing on backtrace */

/*===========================================================================
  Dependency Resolution
===========================================================================*/
#if SCM_COMPAT_SIOD
#undef SCM_USE_NONSTD_FEATURES
#define SCM_USE_NONSTD_FEATURES 1
#undef SCM_USE_SRFI60
#define SCM_USE_SRFI60          1
#else /* SCM_COMPAT_SIOD */
#undef SCM_COMPAT_SIOD_BUGS
#define SCM_COMPAT_SIOD_BUGS    0
#endif /* SCM_COMPAT_SIOD */

#endif /* __SIGSCHEME_CONFIG_H */
