/*===========================================================================
 *  FileName : scmport-mbchar.c
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
 * - This file is intended to be portable. Don't depend on SigScheme.
 * - To isolate and hide implementation-dependent things, don't merge this file
 *   into another
 */

#include "config.h"

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
#include "scmint.h"
#include "encoding.h"
#include "scmport.h"
#include "scmport-mbchar.h"

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
    int linenum;         /* protected */

    ScmCharCodec *codec;
    ScmMultibyteState state;
    scm_byte_t rbuf[SCM_MB_MAX_LEN + sizeof("")];
};

/*=======================================
  File Local Function Declarations
=======================================*/
static ScmCharPort *mbcport_dyn_cast(ScmCharPort *cport,
                                     const ScmCharPortVTbl *dst_vptr);
static ScmCharCodec *mbcport_codec(ScmMultiByteCharPort *port);
static char *mbcport_inspect(ScmMultiByteCharPort *port);
static scm_ichar_t mbcport_get_char(ScmMultiByteCharPort *port);
static scm_ichar_t mbcport_peek_char(ScmMultiByteCharPort *port);
static scm_bool mbcport_char_readyp(ScmMultiByteCharPort *port);
static int mbcport_put_char(ScmMultiByteCharPort *port, scm_ichar_t ch);

static ScmMultibyteCharInfo mbcport_fill_rbuf(ScmMultiByteCharPort *port,
                                              scm_bool blockp);

/*=======================================
  Variable Declarations
=======================================*/
static ScmCharPortVTbl ScmMultiByteCharPort_vtbl;
const ScmCharPortVTbl *ScmMultiByteCharPort_vptr = &ScmMultiByteCharPort_vtbl;

/*=======================================
  Function Implementations
=======================================*/
void
scm_mbcport_init(void)
{
    ScmCharPortVTbl *vptr;

    ScmMultiByteCharPort_vtbl = *ScmBaseCharPort_vptr;

    vptr = &ScmMultiByteCharPort_vtbl;
    vptr->dyn_cast    = (ScmCharPortMethod_dyn_cast)&mbcport_dyn_cast;
    vptr->codec       = (ScmCharPortMethod_codec)&mbcport_codec;
    vptr->inspect     = (ScmCharPortMethod_inspect)&mbcport_inspect;
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

    port->codec = codec;
    port->rbuf[0] = '\0';
    SCM_MBCPORT_CLEAR_STATE(port);
}

ScmCharPort *
ScmMultiByteCharPort_new(ScmBytePort *bport, ScmCharCodec *codec)
{
    ScmMultiByteCharPort *cport;

    cport = SCM_PORT_MALLOC(sizeof(ScmMultiByteCharPort));
    ScmMultiByteCharPort_construct(cport, ScmMultiByteCharPort_vptr,
                                   bport, codec);

    return (ScmCharPort *)cport;
}

void
ScmMultiByteCharPort_set_codec(ScmCharPort *cport, ScmCharCodec *codec)
{
    ScmMultiByteCharPort *mbcport;

    mbcport = SCM_BYTEPORT_DYNAMIC_CAST(ScmMultiByteCharPort, cport);
    mbcport->codec = codec;
    SCM_MBCPORT_CLEAR_STATE(mbcport);
    /* only one byte can be preserved for new codec. otherwise cleared */
    if (1 < strlen((char *)mbcport->rbuf))
        mbcport->rbuf[0] = '\0';
}

static ScmCharPort *
mbcport_dyn_cast(ScmCharPort *cport, const ScmCharPortVTbl *dst_vptr)
{
    return (dst_vptr == ScmBaseCharPort_vptr
            || dst_vptr == ScmMultiByteCharPort_vptr) ? cport : NULL;
}

static ScmCharCodec *
mbcport_codec(ScmMultiByteCharPort *port)
{
    return port->codec;
}

static char *
mbcport_inspect(ScmMultiByteCharPort *port)
{
    return ScmBaseCharPort_inspect((ScmBaseCharPort *)port, "mb");
}

static scm_ichar_t
mbcport_get_char(ScmMultiByteCharPort *port)
{
    scm_ichar_t ch;
#if SCM_USE_STATEFUL_ENCODING
    ScmMultibyteCharInfo mbc;
    ScmMultibyteState next_state;

    mbc = mbcport_fill_rbuf(port, scm_true);
    next_state = SCM_MBCINFO_GET_STATE(mbc);
#endif

    ch = mbcport_peek_char(port);
    port->rbuf[0] = '\0';
#if SCM_USE_STATEFUL_ENCODING
    SCM_MBCPORT_SET_STATE(port, next_state)
#endif
#if SCM_DEBUG
    if (ch == SCM_NEWLINE_STR[0])
        port->linenum++;
#endif

    return ch;
}

static scm_ichar_t
mbcport_peek_char(ScmMultiByteCharPort *port)
{
    ScmMultibyteCharInfo mbc;
    size_t size;
    scm_ichar_t ch;

    mbc = mbcport_fill_rbuf(port, scm_true);
    size = SCM_MBCINFO_GET_SIZE(mbc);
    if (size)
        ch = SCM_CHARCODEC_STR2INT(port->codec, (char *)port->rbuf, size,
                                   port->state);
    else
        ch = EOF;

    return ch;
}

static scm_bool
mbcport_char_readyp(ScmMultiByteCharPort *port)
{
    ScmMultibyteCharInfo mbc;

    mbc = mbcport_fill_rbuf(port, scm_false);
    return !SCM_MBCINFO_INCOMPLETEP(mbc);
}

static int
mbcport_put_char(ScmMultiByteCharPort *port, scm_ichar_t ch)
{
    char *end;
    char wbuf[SCM_MB_MAX_LEN + sizeof("")];

    /* FIXME: set updated state to port->state */
    end = SCM_CHARCODEC_INT2STR(port->codec, wbuf, ch, port->state);
    if (!end)
        SCM_CHARPORT_ERROR(port, "ScmMultibyteCharPort: invalid character");
    return SCM_BYTEPORT_WRITE(port->bport, end - wbuf, wbuf);
}

static ScmMultibyteCharInfo
mbcport_fill_rbuf(ScmMultiByteCharPort *port, scm_bool blockp)
{
    scm_byte_t *end;
    scm_ichar_t byte;
    ScmMultibyteString mbs;
    ScmMultibyteCharInfo mbc;

    end = (scm_byte_t *)strchr((char *)port->rbuf, '\0');
    SCM_MBS_SET_STATE(mbs, port->state);
    do {
        SCM_MBS_SET_STR(mbs, (char *)port->rbuf);
        SCM_MBS_SET_SIZE(mbs, end - port->rbuf);

        mbc = SCM_CHARCODEC_SCAN_CHAR(port->codec, mbs);

        if (SCM_MBCINFO_ERRORP(mbc))
            SCM_CHARPORT_ERROR(port, "ScmMultibyteCharPort: broken character");
        if (!SCM_MBCINFO_INCOMPLETEP(mbc) && SCM_MBCINFO_GET_SIZE(mbc))
            break;
        if (SCM_MBS_GET_SIZE(mbs) == SCM_MB_MAX_LEN)
            SCM_CHARPORT_ERROR(port, "ScmMultibyteCharPort: broken scanner");

        byte = SCM_BYTEPORT_GET_BYTE(port->bport);
        SCM_MBCINFO_SET_STATE(mbc, SCM_MBS_GET_STATE(mbs));
        if (byte == EOF) {
            SCM_MBCINFO_INIT(mbc);
            port->rbuf[0] = '\0';
#if HANDLE_MBC_START
            mbc->start = (char *)port->rbuf;
#endif
            break;
        }
        *end++ = byte;
        *end = '\0';
    } while (blockp || SCM_BYTEPORT_BYTE_READYP(port->bport));

    return mbc;
}
