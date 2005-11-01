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
struct ScmSingleByteCharPort_ {  /* inherits ScmBaseCharPort */
    const ScmCharPortVTbl *vptr;

    ScmBytePort *bport;  /* protected */
};

/*=======================================
  File Local Function Declarations
=======================================*/
static ScmCharPort *basecport_dyn_cast(ScmCharPort *cport,
                                       const ScmCharPortVTbl *dst_vptr);
static int basecport_close(ScmBaseCharPort *port);
static const char *basecport_encoding(ScmBaseCharPort *port);
static int basecport_get_char(ScmBaseCharPort *port);
static int basecport_peek_char(ScmBaseCharPort *port);
static int basecport_char_readyp(ScmBaseCharPort *port);
static int basecport_vprintf(ScmBaseCharPort *port, const char *str,
                             va_list args);
static int basecport_puts(ScmBaseCharPort *port, const char *str);
static int basecport_put_char(ScmBaseCharPort *port, int ch);
static int basecport_flush(ScmBaseCharPort *port);

static ScmCharPort *sbcport_dyn_cast(ScmCharPort *cport,
                                     const ScmCharPortVTbl *dst_vptr);
static const char *sbcport_encoding(ScmSingleByteCharPort *port);
static int sbcport_put_char(ScmSingleByteCharPort *port, int ch);

/*=======================================
  Variable Declarations
=======================================*/
static const ScmCharPortVTbl ScmBaseCharPort_vtbl = {
    (ScmCharPortMethod_dyn_cast)   &basecport_dyn_cast,
    (ScmCharPortMethod_close)      &basecport_close,
    (ScmCharPortMethod_encoding)   &basecport_encoding,
    (ScmCharPortMethod_get_char)   &basecport_get_char,
    (ScmCharPortMethod_peek_char)  &basecport_peek_char,
    (ScmCharPortMethod_char_readyp)&basecport_char_readyp,
    (ScmCharPortMethod_vprintf)    &basecport_vprintf,
    (ScmCharPortMethod_puts)       &basecport_puts,
    (ScmCharPortMethod_put_char)   &basecport_put_char,
    (ScmCharPortMethod_flush)      &basecport_flush
};
const ScmCharPortVTbl *ScmBaseCharPort_vptr = &ScmBaseCharPort_vtbl;

static ScmCharPortVTbl ScmSingleByteCharPort_vtbl;
const ScmCharPortVTbl *ScmSingleByteCharPort_vptr = &ScmSingleByteCharPort_vtbl;

/*=======================================
  Function Implementations
=======================================*/
void
ScmBaseCharPort_construct(ScmBaseCharPort *port, const ScmCharPortVTbl *vptr,
                          ScmBytePort *bport)
{
    port->vptr = ScmSingleByteCharPort_vptr;
    port->bport = bport;
#if SCM_DEBUG
    port->linenum = 1;
#else
    port->linenum = -1;
#endif
}

int
ScmBaseCharPort_line_number(ScmBaseCharPort *port)
{
    return port->linenum;
}

static ScmCharPort *
basecport_dyn_cast(ScmCharPort *cport, const ScmCharPortVTbl *dst_vptr)
{
    if (dst_vptr != ScmBaseCharPort_vptr)
        SCM_PORT_ERROR_INVALID_TYPE(CHAR, cport, ScmBaseCharPort);

    return cport;
}

static int
basecport_close(ScmBaseCharPort *port)
{
    return SCM_BYTEPORT_CLOSE(port->bport);
}

static const char *
basecport_encoding(ScmBaseCharPort *port)
{
    SCM_PORT_ERROR_INVALID_OPERATION(CHAR, port, ScmBaseCharPort);
    /* NOTREACHED */
}

static int
basecport_get_char(ScmBaseCharPort *port)
{
    int ch;

    ch = SCM_BYTEPORT_GET_BYTE(port->bport);
#if SCM_DEBUG
    if (ch == '\n')
        port->linenum++;
#endif

    return ch;
}

static int
basecport_peek_char(ScmBaseCharPort *port)
{
    return SCM_BYTEPORT_PEEK_BYTE(port->bport);
}

static int
basecport_char_readyp(ScmBaseCharPort *port)
{
    return SCM_BYTEPORT_BYTE_READYP(port->bport);
}

static int
basecport_vprintf(ScmBaseCharPort *port, const char *str, va_list args)
{
    return SCM_BYTEPORT_VPRINTF(port->bport, str, args);
}

static int
basecport_puts(ScmBaseCharPort *port, const char *str)
{
    return SCM_BYTEPORT_PUTS(port->bport, str);
}

static int
basecport_put_char(ScmBaseCharPort *port, int ch)
{
    SCM_PORT_ERROR_INVALID_OPERATION(CHAR, port, ScmBaseCharPort);
    /* NOTREACHED */
}

static int
basecport_flush(ScmBaseCharPort *port)
{
    return SCM_BYTEPORT_FLUSH(port->bport);
}


void
Scm_sbcport_init(void)
{
    ScmSingleByteCharPort_vtbl = *ScmBaseCharPort_vptr;
    ScmSingleByteCharPort_vtbl.dyn_cast = (ScmCharPortMethod_dyn_cast)&sbcport_dyn_cast;
    ScmSingleByteCharPort_vtbl.encoding = (ScmCharPortMethod_encoding)&sbcport_encoding;
    ScmSingleByteCharPort_vtbl.put_char = (ScmCharPortMethod_put_char)&sbcport_put_char;
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

    SCM_PORT_ALLOC(CHAR, cport, ScmSingleByteCharPort);
    ScmSingleByteCharPort_construct(cport, ScmSingleByteCharPort_vptr, bport);

    return (ScmCharPort *)cport;
}

static ScmCharPort *
sbcport_dyn_cast(ScmCharPort *cport, const ScmCharPortVTbl *dst_vptr)
{
    if (dst_vptr != ScmBaseCharPort_vptr
        && dst_vptr != ScmSingleByteCharPort_vptr)
        SCM_PORT_ERROR_INVALID_TYPE(CHAR, cport, ScmSingleByteCharPort);

    return cport;
}

static const char *
sbcport_encoding(ScmSingleByteCharPort *port)
{
    return "US-ASCII";
}

static int
sbcport_put_char(ScmSingleByteCharPort *port, int ch)
{
    char buf[1];

    buf[0] = ch;
    return SCM_BYTEPORT_WRITE(port->bport, 1, buf);
}
