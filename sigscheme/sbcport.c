/*===========================================================================
 *  FileName : sbcport.c
 *  About    : A ScmCharPort implementation for singlebyte character stream
 *
 *  Copyright (C) 2005-2006 YamaKen <yamaken  AT bp.iij4u.or.jp>
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
struct ScmSingleByteCharPort_ {  /* inherits ScmBaseCharPort */
    const ScmCharPortVTbl *vptr;

    ScmBytePort *bport;  /* protected */
};

/*=======================================
  File Local Function Declarations
=======================================*/
static ScmCharPort *sbcport_dyn_cast(ScmCharPort *cport,
                                     const ScmCharPortVTbl *dst_vptr);
static ScmCharCodec *sbcport_codec(ScmSingleByteCharPort *port);
static char *sbcport_inspect(ScmSingleByteCharPort *port);
static int sbcport_put_char(ScmSingleByteCharPort *port, int ch);

/*=======================================
  Variable Declarations
=======================================*/
static ScmCharPortVTbl ScmSingleByteCharPort_vtbl;
const ScmCharPortVTbl *ScmSingleByteCharPort_vptr = &ScmSingleByteCharPort_vtbl;

static ScmCharCodec *codec;

/*=======================================
  Function Implementations
=======================================*/
void
scm_sbcport_init(void)
{
    ScmCharPortVTbl *vptr;

    ScmSingleByteCharPort_vtbl = *ScmBaseCharPort_vptr;

    vptr = &ScmSingleByteCharPort_vtbl;
    vptr->dyn_cast = (ScmCharPortMethod_dyn_cast)&sbcport_dyn_cast;
    vptr->codec    = (ScmCharPortMethod_codec)&sbcport_codec;
    vptr->inspect  = (ScmCharPortMethod_inspect)&sbcport_inspect;
    vptr->put_char = (ScmCharPortMethod_put_char)&sbcport_put_char;

    codec = scm_mb_find_codec("ISO-8859-1");
}

void
ScmSingleByteCharPort_construct(ScmSingleByteCharPort *port,
                                const ScmCharPortVTbl *vptr,
                                ScmBytePort *bport)
{
    ScmBaseCharPort_construct((ScmBaseCharPort *)port, vptr, bport);
}

ScmCharPort *
ScmSingleByteCharPort_new(ScmBytePort *bport)
{
    ScmSingleByteCharPort *cport;

    cport = SCM_PORT_MALLOC(sizeof(ScmSingleByteCharPort));
    ScmSingleByteCharPort_construct(cport, ScmSingleByteCharPort_vptr, bport);

    return (ScmCharPort *)cport;
}

static ScmCharPort *
sbcport_dyn_cast(ScmCharPort *cport, const ScmCharPortVTbl *dst_vptr)
{
    return (dst_vptr == ScmBaseCharPort_vptr
            || dst_vptr == ScmSingleByteCharPort_vptr) ? cport : NULL;
}

static ScmCharCodec *
sbcport_codec(ScmSingleByteCharPort *port)
{
    return codec;
}

static char *
sbcport_inspect(ScmSingleByteCharPort *port)
{
    return ScmBaseCharPort_inspect((ScmBaseCharPort *)port, "sb");
}

static int
sbcport_put_char(ScmSingleByteCharPort *port, int ch)
{
    char buf[1];

    buf[0] = ch;
    return SCM_BYTEPORT_WRITE(port->bport, 1, buf);
}
