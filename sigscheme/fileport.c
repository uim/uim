/*===========================================================================
 *  FileName : fileport.c
 *  About    : A ScmBytePort implementation for file stream of the standard C
 *             library
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
#include "fileport.h"

/*=======================================
  File Local Macro Definitions
=======================================*/

/*=======================================
  File Local Type Definitions
=======================================*/
typedef struct ScmFilePort_ ScmFilePort;

/* inherits ScmBytePort */
struct ScmFilePort_ {
    const ScmBytePortVTbl *vptr;

    FILE *file;
};

/*=======================================
  File Local Function Declarations
=======================================*/
static ScmBytePort *fileport_dyn_cast(ScmBytePort *bport,
                                      const ScmBytePortVTbl *dest_vptr);
static int fileport_close(ScmFilePort *bport);
static int fileport_get_byte(ScmFilePort *bport);
static int fileport_peek_byte(ScmFilePort *bport);
static int fileport_byte_readyp(ScmFilePort *bport);
static int fileport_vprintf(ScmFilePort *bport, const char *str, va_list args);
static int fileport_puts(ScmFilePort *bport, const char *str);
static size_t fileport_write(ScmFilePort *bport,
                             size_t nbytes, const char *buf);
static int fileport_flush(ScmFilePort *bport);

/*=======================================
  Variable Declarations
=======================================*/
static const ScmBytePortVTbl ScmFilePort_vtbl = {
    (ScmBytePortMethod_dyn_cast)   &fileport_dyn_cast,
    (ScmBytePortMethod_close)      &fileport_close,
    (ScmBytePortMethod_get_byte)   &fileport_get_byte,
    (ScmBytePortMethod_peek_byte)  &fileport_peek_byte,
    (ScmBytePortMethod_byte_readyp)&fileport_byte_readyp,
    (ScmBytePortMethod_vprintf)    &fileport_vprintf,
    (ScmBytePortMethod_puts)       &fileport_puts,
    (ScmBytePortMethod_write)      &fileport_write,
    (ScmBytePortMethod_flush)      &fileport_flush
};
const ScmBytePortVTbl *ScmFilePort_vptr = &ScmFilePort_vtbl;

/*=======================================
  Function Implementations
=======================================*/

/*
 * Client code must call this first even if current implementation does not
 * contain actual code.
 */
void Scm_fileport_init(void)
{
    return;
}

ScmBytePort *
ScmFilePort_new(FILE *file)
{
    ScmFilePort *port;

    port = malloc(sizeof(ScmFilePort));
    if (!port)
        SCM_PORT_ERROR_NOMEM(BYTE, NULL, ScmFilePort);

    port->vptr = ScmFilePort_vptr;
    port->file = file;

    return (ScmBytePort *)port;
}

static ScmBytePort *
fileport_dyn_cast(ScmBytePort *bport, const ScmBytePortVTbl *dst_vptr)
{
    if (dst_vptr != ScmFilePort_vptr)
        SCM_PORT_ERROR_INVALID_TYPE(BYTE, bport, ScmFilePort);

    return bport;
}

static int
fileport_close(ScmFilePort *port)
{
    int err;

    err = fclose(port->file);
    free(port);

    return err;
}

static int
fileport_get_byte(ScmFilePort *port)
{
    return getc(port->file);
}

static int
fileport_peek_byte(ScmFilePort *port)
{
    int ch;

    ch = getc(port->file);
    return ungetc(ch, port->file);
}

static int
fileport_byte_readyp(ScmFilePort *port)
{
    /* does not support a FILE based on a pipe, or opened by fdopen(3) */
    /* FIXME: support stdin properly */
    return TRUE;
}

static int
fileport_vprintf(ScmFilePort *port, const char *str, va_list args)
{
    return vfprintf(port->file, str, args);
}

static int
fileport_puts(ScmFilePort *port, const char *str)
{
    return fputs(str, port->file);
}

static size_t
fileport_write(ScmFilePort *port, size_t nbytes, const char *buf)
{
    return fwrite(buf, 1, nbytes, port->file);
}

static int
fileport_flush(ScmFilePort *port)
{
    return fflush(port->file);
}
