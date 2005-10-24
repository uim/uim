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
static int fileport_close(ScmBytePort *bport);
static int fileport_get_byte(ScmBytePort *bport);
static int fileport_peek_byte(ScmBytePort *bport);
static int fileport_byte_readyp(ScmBytePort *bport);
static int fileport_vprintf(ScmBytePort *bport, const char *str, va_list args);
static int fileport_puts(ScmBytePort *bport, const char *str);
static size_t fileport_write(ScmBytePort *bport, size_t nbytes, const char *buf);
static int fileport_flush(ScmBytePort *bport);

/*=======================================
  Variable Declarations
=======================================*/
static const ScmBytePortVTbl ScmFilePort_vtbl = {
    &fileport_dyn_cast,
    &fileport_close,
    &fileport_get_byte,
    &fileport_peek_byte,
    &fileport_byte_readyp,
    &fileport_vprintf,
    &fileport_puts,
    &fileport_write,
    &fileport_flush
};
const ScmBytePortVTbl *ScmFilePort_vptr = &ScmFilePort_vtbl;

/*=======================================
  Function Implementations
=======================================*/

ScmBytePort *
ScmFilePort_new(FILE *file)
{
    ScmFilePort *port;

    port = malloc(sizeof(ScmFilePort));
    if (port) {
        port->vptr = ScmFilePort_vptr;
        port->file = file;
    }

    return (ScmBytePort *)port;
}

static ScmBytePort *
fileport_dyn_cast(ScmBytePort *bport, const ScmBytePortVTbl *dst_vptr)
{
    ScmBytePort *cast;

    cast = (dst_vptr == ScmFilePort_vptr) ? bport : NULL;
    if (!cast)
        SCM_BYTEPORT_ERROR(bport, "invalid object is passed to a ScmFilePort method");

    return cast;
}

static int
fileport_close(ScmBytePort *bport)
{
    ScmFilePort *fport;
    int err;

    fport = (ScmFilePort *)bport;
    err = fclose(fport->file);
    free(fport);

    return err;
}

static int
fileport_get_byte(ScmBytePort *bport)
{
    ScmFilePort *fport;

    fport = (ScmFilePort *)bport;
    return getc(fport->file);
}

static int
fileport_peek_byte(ScmBytePort *bport)
{
    ScmFilePort *fport;
    int ch;

    fport = (ScmFilePort *)bport;
    ch = getc(fport->file);
    return ungetc(ch, fport->file);
}

static int
fileport_byte_readyp(ScmBytePort *bport)
{
    /* does not support a FILE based on a pipe, or opened by fdopen(3) */
    /* FIXME: support stdin properly */
    return 1;
}

static int
fileport_vprintf(ScmBytePort *bport, const char *str, va_list args)
{
    ScmFilePort *fport;

    fport = (ScmFilePort *)bport;
    return vfprintf(fport->file, str, args);
}

static int
fileport_puts(ScmBytePort *bport, const char *str)
{
    ScmFilePort *fport;

    fport = (ScmFilePort *)bport;
    return fputs(str, fport->file);
}

static size_t
fileport_write(ScmBytePort *bport, size_t nbytes, const char *buf)
{
    ScmFilePort *fport;

    fport = (ScmFilePort *)bport;
    return fwrite(buf, 1, nbytes, fport->file);
}

static int
fileport_flush(ScmBytePort *bport)
{
    ScmFilePort *fport;

    fport = (ScmFilePort *)bport;
    return fflush(fport->file);
}
