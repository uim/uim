/*===========================================================================
 *  FileName : basecport.c
 *  About    : ScmBaseCharPort implementation
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
#include <stdio.h>
#include <stdarg.h>
#include <string.h>

/*=======================================
  Local Include
=======================================*/
#include "baseport.h"

/*=======================================
  File Local Macro Definitions
=======================================*/

/*=======================================
  File Local Type Definitions
=======================================*/

/*=======================================
  File Local Function Declarations
=======================================*/
static ScmCharPort *basecport_dyn_cast(ScmCharPort *cport,
                                       const ScmCharPortVTbl *dst_vptr);
static int basecport_close(ScmBaseCharPort *port);
static const char *basecport_encoding(ScmBaseCharPort *port);
static char *basecport_inspect(ScmBaseCharPort *port);
static int basecport_get_char(ScmBaseCharPort *port);
static int basecport_peek_char(ScmBaseCharPort *port);
static int basecport_char_readyp(ScmBaseCharPort *port);
static int basecport_vprintf(ScmBaseCharPort *port, const char *str,
                             va_list args);
static int basecport_puts(ScmBaseCharPort *port, const char *str);
static int basecport_put_char(ScmBaseCharPort *port, int ch);
static int basecport_flush(ScmBaseCharPort *port);

/*=======================================
  Variable Declarations
=======================================*/
static const ScmCharPortVTbl ScmBaseCharPort_vtbl = {
    (ScmCharPortMethod_dyn_cast)   &basecport_dyn_cast,
    (ScmCharPortMethod_close)      &basecport_close,
    (ScmCharPortMethod_encoding)   &basecport_encoding,
    (ScmCharPortMethod_inspect)    &basecport_inspect,
    (ScmCharPortMethod_get_char)   &basecport_get_char,
    (ScmCharPortMethod_peek_char)  &basecport_peek_char,
    (ScmCharPortMethod_char_readyp)&basecport_char_readyp,
    (ScmCharPortMethod_vprintf)    &basecport_vprintf,
    (ScmCharPortMethod_puts)       &basecport_puts,
    (ScmCharPortMethod_put_char)   &basecport_put_char,
    (ScmCharPortMethod_flush)      &basecport_flush
};
const ScmCharPortVTbl *ScmBaseCharPort_vptr = &ScmBaseCharPort_vtbl;

/*=======================================
  Function Implementations
=======================================*/
void
ScmBaseCharPort_construct(ScmBaseCharPort *port, const ScmCharPortVTbl *vptr,
                          ScmBytePort *bport)
{
    port->vptr = vptr;
    port->bport = bport;
#if SCM_DEBUG
    port->linenum = 1;
#else
    port->linenum = -1;
#endif
}

char *
ScmBaseCharPort_inspect(ScmBaseCharPort *port, const char *header)
{
    const char *encoding;
    char *bport_part, *combined;
    size_t size;

    encoding = SCM_CHARPORT_ENCODING((ScmCharPort *)port);
    bport_part = SCM_BYTEPORT_INSPECT((ScmBytePort *)port->bport);
    size = strlen(header) + strlen(encoding) + strlen(bport_part)
        + sizeof("  ");
    combined = malloc(size);
    snprintf(combined, size, "%s %s %s", header, encoding, bport_part);
    free(bport_part);

    return combined;
}

int
ScmBaseCharPort_line_number(ScmBaseCharPort *port)
{
    return port->linenum;
}

static ScmCharPort *
basecport_dyn_cast(ScmCharPort *cport, const ScmCharPortVTbl *dst_vptr)
{
    return (dst_vptr == ScmBaseCharPort_vptr) ? cport : NULL;
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

static char *
basecport_inspect(ScmBaseCharPort *port)
{
    return ScmBaseCharPort_inspect(port, "unknown");
}

static int
basecport_get_char(ScmBaseCharPort *port)
{
    int ch;

    ch = SCM_BYTEPORT_GET_BYTE(port->bport);
#if SCM_DEBUG
    if (ch == SCM_NEWLINE_STR[0])
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
