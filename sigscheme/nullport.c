/*===========================================================================
 *  FileName : nullport.c
 *  About    : A ScmBytePort implementation for null read/write
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
#include "nullport.h"

/*=======================================
  File Local Macro Definitions
=======================================*/
#define OK 0

/*=======================================
  File Local Type Definitions
=======================================*/
typedef ScmBytePort ScmNullPort;

/*=======================================
  File Local Function Declarations
=======================================*/
static ScmBytePort *nullport_dyn_cast(ScmBytePort *bport,
                                      const ScmBytePortVTbl *dest_vptr);
static int nullport_close(ScmNullPort *bport);
static char *nullport_inspect(ScmNullPort *port);
static int nullport_get_byte(ScmNullPort *bport);
static int nullport_peek_byte(ScmNullPort *bport);
static int nullport_byte_readyp(ScmNullPort *bport);
static int nullport_vprintf(ScmNullPort *bport, const char *str, va_list args);
static int nullport_puts(ScmNullPort *bport, const char *str);
static size_t nullport_write(ScmNullPort *bport,
                             size_t nbytes, const char *buf);
static int nullport_flush(ScmNullPort *bport);

/*=======================================
  Variable Declarations
=======================================*/
static const ScmBytePortVTbl ScmNullPort_vtbl = {
    (ScmBytePortMethod_dyn_cast)   &nullport_dyn_cast,
    (ScmBytePortMethod_close)      &nullport_close,
    (ScmBytePortMethod_inspect)    &nullport_inspect,
    (ScmBytePortMethod_get_byte)   &nullport_get_byte,
    (ScmBytePortMethod_peek_byte)  &nullport_peek_byte,
    (ScmBytePortMethod_byte_readyp)&nullport_byte_readyp,
    (ScmBytePortMethod_vprintf)    &nullport_vprintf,
    (ScmBytePortMethod_puts)       &nullport_puts,
    (ScmBytePortMethod_write)      &nullport_write,
    (ScmBytePortMethod_flush)      &nullport_flush
};
const ScmBytePortVTbl *ScmNullPort_vptr = &ScmNullPort_vtbl;

/*=======================================
  Function Implementations
=======================================*/

/*
 * Client code must call this first even if current implementation does not
 * contain actual code.
 */
void 
scm_nullport_init(void)
{
}

ScmBytePort *
ScmNullPort_new(void)
{
    ScmNullPort *port;

    port = SCM_PORT_MALLOC(sizeof(ScmNullPort));

    port->vptr = ScmNullPort_vptr;

    return (ScmBytePort *)port;
}

static ScmBytePort *
nullport_dyn_cast(ScmBytePort *bport, const ScmBytePortVTbl *dst_vptr)
{
    return (dst_vptr == ScmNullPort_vptr) ? bport : NULL;
}

static int
nullport_close(ScmNullPort *port)
{
    return OK;
}

static char *
nullport_inspect(ScmNullPort *port)
{
    return strdup("null");
}

static int
nullport_get_byte(ScmNullPort *port)
{
    return EOF;
}

static int
nullport_peek_byte(ScmNullPort *port)
{
    return EOF;
}

static int
nullport_byte_readyp(ScmNullPort *port)
{
    return TRUE;
}

static int
nullport_vprintf(ScmNullPort *port, const char *str, va_list args)
{
    return 0;
}

static int
nullport_puts(ScmNullPort *port, const char *str)
{
    return 0;
}

static size_t
nullport_write(ScmNullPort *port, size_t nbytes, const char *buf)
{
    return nbytes;
}

static int
nullport_flush(ScmNullPort *port)
{
    return OK;
}
