/*===========================================================================
 *  Filename : scmport-str.h
 *  About    : A ScmBytePort implementation for string
 *
 *  Copyright (C) 2005-2006 YAMAMOTO Kengo <yamaken AT bp.iij4u.or.jp>
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

/*
 * This file is intended to be portable. Don't depend on SigScheme and don't
 * merge into another file.
 */

#ifndef __SCM_SCMPORT_STR_H
#define __SCM_SCMPORT_STR_H

#include "scmport.h"

#ifdef __cplusplus
extern "C" {
#endif

/*=======================================
  Macro Definitions
=======================================*/

/*=======================================
  Type Definitions
=======================================*/
typedef void (*ScmInputStrPort_finalizer)(char **str, scm_bool ownership,
                                          void **opaque);
typedef void (*ScmOutputStrPort_finalizer)(char **str, size_t buf_size,
                                           void **opaque);

/*=======================================
  Variable Declarations
=======================================*/
SCM_EXTERN(const ScmBytePortVTbl *const ScmInputStrPort_vptr);
SCM_EXTERN(const ScmBytePortVTbl *const ScmOutputStrPort_vptr);

/*=======================================
  Function Declarations
=======================================*/
SCM_EXPORT void scm_strport_init(void);

SCM_EXPORT ScmBytePort *ScmInputStrPort_new(char *str, ScmInputStrPort_finalizer finalize);
SCM_EXPORT ScmBytePort *ScmInputStrPort_new_copying(const char *str, ScmInputStrPort_finalizer finalize);
SCM_EXPORT ScmBytePort *ScmInputStrPort_new_const(const char *str, ScmInputStrPort_finalizer finalize);
SCM_EXPORT void **ScmInputStrPort_ref_opaque(ScmBytePort *bport);

SCM_EXPORT ScmBytePort *ScmOutputStrPort_new(ScmOutputStrPort_finalizer finalize);
SCM_EXPORT const char *ScmOutputStrPort_str(ScmBytePort *bport);
SCM_EXPORT size_t ScmOutputStrPort_c_strlen(ScmBytePort *bport);
SCM_EXPORT void **ScmOutputStrPort_ref_opaque(ScmBytePort *bport);

#ifdef __cplusplus
}
#endif

#endif /* __SCM_SCMPORT_STR_H */
