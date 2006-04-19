/*===========================================================================
 *  FileName : encoding-config.h
 *  About    : Client-adaptation configuration file for the encoding.[hc]
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

#ifndef __SCM_ENCODING_CONFIG_H
#define __SCM_ENCODING_CONFIG_H

#if SCM_ENCODING_USE_WITH_SIGSCHEME
#include "sigscheme.h"
#else /* SCM_ENCODING_USE_WITH_SIGSCHEME */
#include <stdlib.h>
#endif /* SCM_ENCODING_USE_WITH_SIGSCHEME */

#ifdef __cplusplus
extern "C" {
#endif

/*=======================================
  Macro Definitions
=======================================*/
#if SCM_ENCODING_USE_WITH_SIGSCHEME
#define SCM_ENCODING_ASSERT(cond) (SCM_ASSERT(cond))
#define SCM_ENCODING_ERROR(msg)   (scm_plain_error(msg))
#define SCM_ENCODING_CDBG(args)   SCM_CDBG(args)
#define SCM_ENCODING_DBG(args)    SCM_DBG(args)
#else /* SCM_ENCODING_USE_WITH_SIGSCHEME */
#define SCM_USE_UTF8  1
#define SCM_USE_EUCJP 1
#define SCM_USE_EUCCN 1
#define SCM_USE_EUCKR 1
#define SCM_USE_SJIS  1

/* choose exclusively. fallbacks to the unibyte encoding if nothing chosen. */
#define SCM_USE_UTF8_AS_DEFAULT  1
#define SCM_USE_EUCCN_AS_DEFAULT 0
#define SCM_USE_EUCJP_AS_DEFAULT 0
#define SCM_USE_EUCKR_AS_DEFAULT 0
#define SCM_USE_SJIS_AS_DEFAULT  0

#define SCM_ENCODING_ASSERT(cond) SCM_EMPTY_EXPR
#define SCM_ENCODING_ERROR(msg)   (exit(EXIT_FAILURE))
#define SCM_ENCODING_CDBG(args)   SCM_EMPTY_EXPR
#define SCM_ENCODING_DBG(args)    SCM_EMPTY_EXPR
#endif /* SCM_ENCODING_USE_WITH_SIGSCHEME */

/*=======================================
  Type Definitions
=======================================*/

/*=======================================
  Variable Declarations
=======================================*/

/*=======================================
  Function Declarations
=======================================*/


#ifdef __cplusplus
}
#endif

#endif /* __SCM_ENCODING_CONFIG_H */
