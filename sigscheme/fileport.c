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
#include <string.h>

/*=======================================
  Local Include
=======================================*/
#include "baseport.h"
#include "fileport.h"

/*=======================================
  File Local Macro Definitions
=======================================*/
#ifndef FALSE
#define FALSE 0
#endif /* FALSE */
#ifndef TRUE
#define TRUE  (!FALSE)
#endif /* TRUE */

#define OK 0

/*=======================================
  File Local Type Definitions
=======================================*/
typedef struct ScmFilePort_ ScmFilePort;

struct ScmFilePort_ {  /* inherits ScmBytePort */
    const ScmBytePortVTbl *vptr;

    FILE *file;
    char *aux_info;  /* human readable auxilialy information about the file */
    int ownership;   /* whether close the file at fileport_close() */
};

/*=======================================
  File Local Function Declarations
=======================================*/
static ScmBytePort *fileport_new_internal(FILE *file, const char *aux_info,
                                          int ownership);

static ScmBytePort *fileport_dyn_cast(ScmBytePort *bport,
                                      const ScmBytePortVTbl *dest_vptr);
static int fileport_close(ScmFilePort *bport);
static char *fileport_inspect(ScmFilePort *port);
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
    (ScmBytePortMethod_inspect)    &fileport_inspect,
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

static ScmBytePort *
fileport_new_internal(FILE *file, const char *aux_info, int ownership)
{
    ScmFilePort *port;

    SCM_PORT_ALLOC(BYTE, port, ScmFilePort);

    port->vptr = ScmFilePort_vptr;
    port->file = file;
    port->aux_info = strdup(aux_info);
    port->ownership = ownership;

    return (ScmBytePort *)port;
}

ScmBytePort *
ScmFilePort_new(FILE *file, const char *aux_info)
{
    return fileport_new_internal(file, aux_info, TRUE);
}

ScmBytePort *
ScmFilePort_new_shared(FILE *file, const char *aux_info)
{
    return fileport_new_internal(file, aux_info, FALSE);
}

ScmBytePort *
ScmFilePort_open_input_file(const char *path)
{
    FILE *file;

    file = fopen(path, "rb");
    return (file) ? ScmFilePort_new(file, path) : NULL;
}

ScmBytePort *
ScmFilePort_open_output_file(const char *path)
{
    FILE *file;

    file = fopen(path, "wb");
    return (file) ? ScmFilePort_new(file, path) : NULL;
}

static ScmBytePort *
fileport_dyn_cast(ScmBytePort *bport, const ScmBytePortVTbl *dst_vptr)
{
    return (dst_vptr == ScmFilePort_vptr) ? bport : NULL;
}

static int
fileport_close(ScmFilePort *port)
{
    int err;

    err = (port->ownership) ? fclose(port->file) : OK;
    free(port->aux_info);
    free(port);

    return err;
}

static char *
fileport_inspect(ScmFilePort *port)
{
    char *combined;
    size_t size;

    if (port->aux_info) {
        size = sizeof("file ") + strlen(port->aux_info);
        combined = malloc(size);
        snprintf(combined, size, "file %s", port->aux_info);
        return combined;
    } else {
        return strdup("file");
    }
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
