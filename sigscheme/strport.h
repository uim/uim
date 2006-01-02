/*===========================================================================
 *  FileName : strport.h
 *  About    : A ScmBytePort implementation for string
 *
 *  Copyright (C) 2005 YamaKen <yamaken  AT bp.iij4u.or.jp>
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

#ifndef __SCM_STRPORT_H
#define __SCM_STRPORT_H

/*=======================================
  System Include
=======================================*/

/*=======================================
  Local Include
=======================================*/
#include "baseport.h"

/*=======================================
  Macro Definitions
=======================================*/

/*=======================================
  Type Definitions
=======================================*/
typedef void (*ScmInputStrPort_finalizer)(char **str, int ownership,
                                          void **opaque);
typedef void (*ScmOutputStrPort_finalizer)(char **str, size_t buf_size,
                                           void **opaque);

/*=======================================
   Variable Declarations
=======================================*/
extern const ScmBytePortVTbl *ScmInputStrPort_vptr;
extern const ScmBytePortVTbl *ScmOutputStrPort_vptr;

/*=======================================
   Function Declarations
=======================================*/
void scm_strport_init(void);

ScmBytePort *ScmInputStrPort_new(char *str,
                                 ScmInputStrPort_finalizer finalize);
ScmBytePort *ScmInputStrPort_new_copying(const char *str,
                                         ScmInputStrPort_finalizer finalize);
ScmBytePort *ScmInputStrPort_new_const(const char *str,
                                       ScmInputStrPort_finalizer finalize);
void **ScmInputStrPort_ref_opaque(ScmBytePort *bport);

ScmBytePort *ScmOutputStrPort_new(ScmOutputStrPort_finalizer finalize);
const char *ScmOutputStrPort_str(ScmBytePort *bport);
void **ScmOutputStrPort_ref_opaque(ScmBytePort *bport);


#endif /* __SCM_STRPORT_H */
