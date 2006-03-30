/*===========================================================================
 *  FileName : scmport-config.h
 *  About    : Client-dependent configuration file for scmport codes
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

/* This file configures the scmport codes to adapt to SigScheme */

#ifndef __SCM_SCMPORT_CONFIG_H
#define __SCM_SCMPORT_CONFIG_H

/*=======================================
  System Include
=======================================*/

/*=======================================
  Local Include
=======================================*/
#include "sigscheme.h"

/*=======================================
  Macro Definitions
=======================================*/
#define SCM_PORT_MALLOC(size)          (scm_malloc(size))
#define SCM_PORT_CALLOC(number, size)  (scm_calloc((number), (size)))
#define SCM_PORT_REALLOC(ptr, size)    (scm_realloc((ptr), (size)))
#define SCM_PORT_STRDUP(str)           (scm_strdup(str))
#define SCM_CHARPORT_ERROR(cport, msg) (scm_plain_error(msg))
#define SCM_BYTEPORT_ERROR(bport, msg) (scm_plain_error(msg))

/*=======================================
  Type Definitions
=======================================*/

/*=======================================
  Variable Declarations
=======================================*/

/*=======================================
  Function Declarations
=======================================*/


#endif /* __SCM_SCMPORT_CONFIG_H */
