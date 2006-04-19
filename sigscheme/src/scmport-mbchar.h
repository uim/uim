/*===========================================================================
 *  FileName : scmport-mbchar.h
 *  About    : A ScmCharPort implementation for multibyte character stream
 *
 *  Copyright (C) 2005-2006 YamaKen <yamaken AT bp.iij4u.or.jp>
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

#ifndef __SCM_SCMPORT_MBCHAR_H
#define __SCM_SCMPORT_MBCHAR_H

#include "encoding.h"
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
typedef struct ScmMultiByteCharPort_ ScmMultiByteCharPort;

/*=======================================
  Variable Declarations
=======================================*/
SCM_EXTERN(const ScmCharPortVTbl *const ScmMultiByteCharPort_vptr);

/*=======================================
  Function Declarations
=======================================*/
SCM_EXPORT void scm_mbcport_init(void);

SCM_EXPORT void ScmMultiByteCharPort_construct(ScmMultiByteCharPort *port,
                                               const ScmCharPortVTbl *vptr,
                                               ScmBytePort *bport, ScmCharCodec *codec);
SCM_EXPORT ScmCharPort *ScmMultiByteCharPort_new(ScmBytePort *bport,
                                                 ScmCharCodec *codec);

SCM_EXPORT void ScmMultiByteCharPort_set_codec(ScmCharPort *cport,
                                               ScmCharCodec *codec);

#ifdef __cplusplus
}
#endif

#endif /* __SCM_SCMPORT_MBCHAR_H */
