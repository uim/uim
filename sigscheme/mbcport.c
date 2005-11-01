/*===========================================================================
 *  FileName : mbcport.c
 *  About    : A ScmCharPort implementation for multibyte character stream
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

/*=======================================
  Local Include
=======================================*/
#include "baseport.h"
#include "sbcport.h"
#include "encoding.h"
#include "mbcport.h"

/*=======================================
  File Local Macro Definitions
=======================================*/
#define HANDLE_MBC_START 0

#if SCM_USE_STATEFUL_ENCODING
#define SCM_MBCPORT_CLEAR_STATE(port) (port->state = NULL)
#else
#define SCM_MBCPORT_CLEAR_STATE(port)
#endif

/*=======================================
  File Local Type Definitions
=======================================*/
struct ScmMultiByteCharPort_ {  /* inherits ScmBaseCharPort */
    const ScmCharPortVTbl *vptr;

    ScmBytePort *bport;  /* protected */
    ScmCharCodec *codec;
#if SCM_USE_STATEFUL_ENCODING
    ScmMultibyteState state;
#endif
    char rbuf[SCM_MB_MAX_LEN + sizeof((char)'\0')];
};

/*=======================================
  File Local Function Declarations
=======================================*/
static ScmCharPort *mbcport_dyn_cast(ScmCharPort *cport,
                                     const ScmCharPortVTbl *dst_vptr);
static const char *mbcport_encoding(ScmMultiByteCharPort *port);
static int mbcport_get_char(ScmMultiByteCharPort *port);
static int mbcport_peek_char(ScmMultiByteCharPort *port);
static int mbcport_char_readyp(ScmMultiByteCharPort *port);
static int mbcport_put_char(ScmMultiByteCharPort *port, int ch);

static ScmMultibyteCharInfo mbcport_fill_rbuf(ScmMultiByteCharPort *port,
                                              int block);

/*=======================================
  Variable Declarations
=======================================*/
static ScmCharPortVTbl ScmMultiByteCharPort_vtbl;
const ScmCharPortVTbl *ScmMultiByteCharPort_vptr = &ScmMultiByteCharPort_vtbl;

/*=======================================
  Function Implementations
=======================================*/
void
Scm_mbcport_init(void)
{
    ScmCharPortVTbl *vptr;

    Scm_sbcport_init();

    ScmMultiByteCharPort_vtbl = *ScmSingleByteCharPort_vptr;

    vptr = &ScmMultiByteCharPort_vtbl;
    vptr->dyn_cast    = (ScmCharPortMethod_dyn_cast)&mbcport_dyn_cast;
    vptr->encoding    = (ScmCharPortMethod_encoding)&mbcport_encoding;
    vptr->get_char    = (ScmCharPortMethod_get_char)&mbcport_get_char;
    vptr->peek_char   = (ScmCharPortMethod_peek_char)&mbcport_peek_char;
    vptr->char_readyp = (ScmCharPortMethod_char_readyp)&mbcport_char_readyp;
    vptr->put_char    = (ScmCharPortMethod_put_char)&mbcport_put_char;
}

void
ScmMultiByteCharPort_construct(ScmMultiByteCharPort *port,
                               const ScmCharPortVTbl *vptr,
                               ScmBytePort *bport, ScmCharCodec *codec)
{
    ScmBaseCharPort_construct((ScmBaseCharPort *)port, vptr, bport);

    cport->codec = codec;
    cport->rbuf[0] = '\0';
    SCM_MBCPORT_CLEAR_STATE(cport);
}

ScmCharPort *
ScmMultiByteCharPort_new(ScmBytePort *bport, ScmCharCodec *codec)
{
    ScmMultiByteCharPort *cport;

    SCM_PORT_ALLOC(CHAR, cport, ScmMultiByteCharPort);
    ScmMultiByteCharPort_construct(cport, ScmMultiByteCharPort_vptr,
                                   bport, codec);

    return (ScmCharPort *)cport;
}

static ScmCharPort *
mbcport_dyn_cast(ScmCharPort *cport, const ScmCharPortVTbl *dst_vptr)
{
    if (dst_vptr != ScmBaseCharPort_vptr
        && dst_vptr != ScmMultiByteCharPort_vptr)
        SCM_PORT_ERROR_INVALID_TYPE(CHAR, cport, ScmMultiByteCharPort);

    return cport;
}

static const char *
mbcport_encoding(ScmMultiByteCharPort *port)
{
    return SCM_CHARCODEC_ENCODING(port->codec);
}

static int
mbcport_get_char(ScmMultiByteCharPort *port)
{
    int ch;

    ch = mbcport_peek_char(port);
    port->rbuf[0] = '\0';
    SCM_MBCPORT_CLEAR_STATE(cport)

    return ch;
}

static int
mbcport_peek_char(ScmMultiByteCharPort *port)
{
    ScmMultibyteCharInfo mbc;
    int size, ch;

    mbc = mbcport_fill_rbuf(port, TRUE);
    size = SCM_MBCINFO_GET_SIZE(mbc);
    ch = (size) ? SCM_CHARCODEC_STR2INT(port->codec, port->rbuf, size) : EOF;

    return ch;
}

static int
mbcport_char_readyp(ScmMultiByteCharPort *port)
{
    ScmMultibyteCharInfo mbc;

    mbc = mbcport_fill_rbuf(port, FALSE);
    return !SCM_MBCINFO_INCOMPLETEP(mbc);
}

static int
mbcport_put_char(ScmMultiByteCharPort *port, int ch)
{
    char wbuf[SCM_MB_MAX_LEN + sizeof((char)'\0')];
    char *end;

    end = SCM_CHARCODEC_INT2STR(port->codec, wbuf, ch);
    if (!end)
        SCM_CHARPORT_ERROR(port, "ScmMultibyteCharPort: invalid character");
    return SCM_BYTEPORT_WRITE(port->bport, end - wbuf, wbuf);
}

static ScmMultibyteCharInfo
mbcport_fill_rbuf(ScmMultiByteCharPort *port, int block)
{
    char *end;
    int byte;
    ScmMultibyteString mbs;
    ScmMultibyteCharInfo mbc;

    end = strchr(port->rbuf, '\0');
    SCM_MBS_SET_STATE(mbs, port->state);
    do {
        SCM_MBS_SET_STR(mbs, port->rbuf);
        SCM_MBS_SET_SIZE(mbs, strlen(port->rbuf));

        mbc = SCM_CHARCODEC_SCAN_CHAR(port->codec, mbs);
        
        if (SCM_MBCINFO_ERRORP(mbc))
            SCM_CHARPORT_ERROR(port, "ScmMultibyteCharPort: broken character");
        if (!SCM_MBCINFO_INCOMPLETEP(mbc))
            break;
        if (SCM_MBS_GET_SIZE(mbs) == SCM_MB_MAX_LEN)
            SCM_CHARPORT_ERROR(port, "ScmMultibyteCharPort: broken scanner");

        byte = SCM_BYTEPORT_GET_BYTE(port->bport);
        if (byte == EOF) {
            SCM_MBCINFO_INIT(mbc);
            port->rbuf[0] = '\0';
#if HANDLE_MBC_START
            mbc->start = port->rbuf;
#endif
            break;
        }
        *end++ = byte;
        *end = '\0';
    } while (block || SCM_BYTEPORT_BYTE_READYP(port->bport));

    return mbc;
}
