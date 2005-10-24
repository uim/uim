/*===========================================================================
 *  FileName : sbcport.c
 *  About    : A ScmCharPort implementation for singlebyte character stream
 *
 *  Copyright (C) 2005      by YamaKen (yamaken AT bp.iij4u.or.jp)
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
 * - This file is intended to be portable. Don't depend on SigScheme.
 * - To isolate and hide implementation-dependent things, don't merge this file
 *   into another
 */

/*=======================================
  System Include
=======================================*/
#include <stdlib.h>
#include <stdarg.h>

/*=======================================
  Local Include
=======================================*/
#include "baseport.h"
#include "sbcport.h"

/*=======================================
  File Local Macro Definitions
=======================================*/

/*=======================================
  File Local Type Definitions
=======================================*/
typedef struct ScmSingleByteCharPort_ ScmSingleByteCharPort;

struct ScmSingleByteCharPort_ {  /* inherits ScmBaseCharPort */
    const ScmCharPortVTbl *vptr;

    ScmBytePort *bport;  /* protected */
};

/*=======================================
  File Local Function Declarations
=======================================*/
static ScmCharPort *basecport_dyn_cast(ScmCharPort *cport,
                                       const ScmCharPortVTbl *dst_vptr);
static int basecport_close(ScmCharPort *cport);
static int basecport_get_char(ScmCharPort *cport);
static int basecport_peek_char(ScmCharPort *cport);
static int basecport_char_readyp(ScmCharPort *cport);
static int basecport_vprintf(ScmCharPort *cport, const char *str,
                             va_list args);
static int basecport_flush(ScmCharPort *cport);

static ScmCharPort *sbcport_dyn_cast(ScmCharPort *cport,
                                     const ScmCharPortVTbl *dst_vptr);
static const char *sbcport_encoding(ScmCharPort *cport);
static int sbcport_put_char(ScmCharPort *cport, int ch);

/*=======================================
  Variable Declarations
=======================================*/
static const ScmCharPortVTbl ScmBaseCharPort_vtbl = {
    &basecport_dyn_cast,
    &basecport_close,
    NULL,
    &basecport_get_char,
    &basecport_peek_char,
    &basecport_char_readyp,
    &basecport_vprintf,
    NULL,
    &basecport_flush
};
const ScmCharPortVTbl *ScmBaseCharPort_vptr = &ScmBaseCharPort_vtbl;

static ScmCharPortVTbl ScmSingleByteCharPort_vtbl;
const ScmCharPortVTbl *ScmSingleByteCharPort_vptr = &ScmSingleByteCharPort_vtbl;

/*=======================================
  Function Implementations
=======================================*/
static ScmCharPort *
basecport_dyn_cast(ScmCharPort *cport, const ScmCharPortVTbl *dst_vptr)
{
    ScmCharPort *cast;

    cast = (dst_vptr == ScmBaseCharPort_vptr) ? cport : NULL;
    if (!cast)
        SCM_CHARPORT_ERROR(cport, "invalid object is passed to a ScmBaseCharPort method");

    return cast;
}

static int
basecport_close(ScmCharPort *cport)
{
    ScmBaseCharPort *basecport;

    basecport = (ScmBaseCharPort *)cport;
    return SCM_BYTEPORT_CLOSE(basecport->bport);
}

static int
basecport_get_char(ScmCharPort *cport)
{
    ScmBaseCharPort *basecport;

    basecport = (ScmBaseCharPort *)cport;
    return SCM_BYTEPORT_GET_BYTE(basecport->bport);
}

static int
basecport_peek_char(ScmCharPort *cport)
{
    ScmBaseCharPort *basecport;

    basecport = (ScmBaseCharPort *)cport;
    return SCM_BYTEPORT_PEEK_BYTE(basecport->bport);
}

static int
basecport_char_readyp(ScmCharPort *cport)
{
    ScmBaseCharPort *basecport;

    basecport = (ScmBaseCharPort *)cport;
    return SCM_BYTEPORT_BYTE_READYP(basecport->bport);
}

static int
basecport_vprintf(ScmCharPort *cport, const char *str, va_list args)
{
    ScmBaseCharPort *basecport;

    basecport = (ScmBaseCharPort *)cport;
    return SCM_BYTEPORT_VPRINTF(basecport->bport, str, args);
}

static int
basecport_flush(ScmCharPort *cport)
{
    ScmBaseCharPort *basecport;

    basecport = (ScmBaseCharPort *)cport;
    return SCM_BYTEPORT_FLUSH(basecport->bport);
}


void
Scm_sbcport_init(void)
{
    ScmSingleByteCharPort_vtbl = *ScmBaseCharPort_vptr;
    ScmSingleByteCharPort_vtbl.dyn_cast = &sbcport_dyn_cast;
    ScmSingleByteCharPort_vtbl.encoding = &sbcport_encoding;
    ScmSingleByteCharPort_vtbl.put_char = &sbcport_put_char;
}

ScmCharPort *
ScmSingleByteCharPort_new(ScmBytePort *bport)
{
    ScmSingleByteCharPort *cport;

    cport = malloc(sizeof(ScmSingleByteCharPort));
    if (cport) {
        cport->vptr = ScmSingleByteCharPort_vptr;
        cport->bport = bport;
    }

    return (ScmCharPort *)cport;
}

static ScmCharPort *
sbcport_dyn_cast(ScmCharPort *cport, const ScmCharPortVTbl *dst_vptr)
{
    ScmCharPort *cast;

    cast = (dst_vptr == ScmBaseCharPort_vptr
            || dst_vptr == ScmSingleByteCharPort_vptr) ? cport : NULL;
    if (!cast)
        SCM_CHARPORT_ERROR(cport, "invalid object is passed to a ScmSingleByteCharPort method");

    return cast;
}

static const char *
sbcport_encoding(ScmCharPort *cport)
{
    ScmSingleByteCharPort *sbcport;

    sbcport = (ScmSingleByteCharPort *)cport;
    return "US-ASCII";
}

static int
sbcport_put_char(ScmCharPort *cport, int ch)
{
    ScmSingleByteCharPort *sbcport;
    char buf[1];

    buf[0] = ch;
    sbcport = (ScmSingleByteCharPort *)cport;
    return SCM_BYTEPORT_WRITE(sbcport->bport, 1, buf);
}
