/*===========================================================================
 *  Filename : scmport-config.h
 *  About    : Client-adaptation configuration file for the scmport codes
 *
 *  Copyright (C) 2006 YamaKen <yamaken AT bp.iij4u.or.jp>
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

#ifndef __SCM_SCMPORT_CONFIG_H
#define __SCM_SCMPORT_CONFIG_H

#if SCM_SCMPORT_USE_WITH_SIGSCHEME
#include "sigscheme.h"
#else /* SCM_SCMPORT_USE_WITH_SIGSCHEME */
#include <stdlib.h>
#include <string.h>
#endif /* SCM_SCMPORT_USE_WITH_SIGSCHEME */

#ifdef __cplusplus
extern "C" {
#endif

/*=======================================
  Macro Definitions
=======================================*/
#if SCM_SCMPORT_USE_WITH_SIGSCHEME
#define SCM_PORT_MALLOC(size)          (scm_malloc(size))
#define SCM_PORT_CALLOC(number, size)  (scm_calloc((number), (size)))
#define SCM_PORT_REALLOC(ptr, size)    (scm_realloc((ptr), (size)))
#define SCM_PORT_STRDUP(str)           (scm_strdup(str))
#define SCM_CHARPORT_ERROR(cport, msg) (scm_plain_error(msg))
#define SCM_BYTEPORT_ERROR(bport, msg) (scm_plain_error(msg))
#else /* SCM_SCMPORT_USE_WITH_SIGSCHEME */
/* Allocation error handling in the macros is strongly recommended. */
#define SCM_PORT_MALLOC(size)          (malloc(size))
#define SCM_PORT_CALLOC(number, size)  (calloc(number, size))
#define SCM_PORT_REALLOC(ptr, size)    (realloc(ptr, size))
/* FIXME: Support platforms lacking strdup(3) */
#define SCM_PORT_STRDUP(str)           (strdup(str))

/*
 * Define appropriate error handling such as exception to override these. The
 * macro MUST NOT return. The replacement expression should indicate that it
 * will not return, in compiler specific way such as noreturn attribute of GCC.
 */
#define SCM_CHARPORT_ERROR(cport, msg) (exit(EXIT_FAILURE))
#define SCM_BYTEPORT_ERROR(bport, msg) (exit(EXIT_FAILURE))
#endif /* SCM_SCMPORT_USE_WITH_SIGSCHEME */

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

#endif /* __SCM_SCMPORT_CONFIG_H */
