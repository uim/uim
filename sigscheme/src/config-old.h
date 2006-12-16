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
#ifndef SCM_USE_AGGREGATED_GLOBAL_VARS
#define SCM_USE_AGGREGATED_GLOBAL_VARS 0
#endif /* !SCM_USE_AGGREGATED_GLOBAL_VARS */
#else /* SCM_HAVE_WRITABLE_GLOBAL_VARS */
#undef SCM_USE_AGGREGATED_GLOBAL_VARS
#define SCM_USE_AGGREGATED_GLOBAL_VARS 1
#endif /* SCM_HAVE_WRITABLE_GLOBAL_VARS */


#endif /* __SIGSCHEME_CONFIG_OLD_H */
