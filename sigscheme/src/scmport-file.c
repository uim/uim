/*===========================================================================
 *  FileName : scmport-file.c
 *  About    : A ScmBytePort implementation for file stream of the standard C
 *             library
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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "scmint.h"
#include "scmport-config.h"
#include "scmport.h"
#include "scmport-file.h"

/*=======================================
  File Local Macro Definitions
=======================================*/
#define OK 0

/*=======================================
  File Local Type Definitions
=======================================*/
typedef struct ScmFilePort_ ScmFilePort;

struct ScmFilePort_ {  /* inherits ScmBytePort */
    const ScmBytePortVTbl *vptr;

    FILE *file;
    char *aux_info;  /* human readable auxilialy information about the file */
    scm_bool ownership;  /* whether close the file at fileport_close() */
};

/*=======================================
  File Local Function Declarations
=======================================*/
static ScmBytePort *fileport_new_internal(FILE *file, const char *aux_info,
                                          scm_bool ownership);

static ScmBytePort *fileport_dyn_cast(ScmBytePort *bport,
                                      const ScmBytePortVTbl *dest_vptr);
static int fileport_close(ScmFilePort *bport);
static char *fileport_inspect(ScmFilePort *port);
static scm_ichar_t fileport_get_byte(ScmFilePort *bport);
static scm_ichar_t fileport_peek_byte(ScmFilePort *bport);
static scm_bool fileport_byte_readyp(ScmFilePort *bport);
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
    (ScmBytePortMethod_puts)       &fileport_puts,
    (ScmBytePortMethod_write)      &fileport_write,
    (ScmBytePortMethod_flush)      &fileport_flush
};
SCM_EXPORT const ScmBytePortVTbl *const ScmFilePort_vptr = &ScmFilePort_vtbl;

/*=======================================
  Function Implementations
=======================================*/

/*
 * Client code must call this first even if current implementation does not
 * contain actual code.
 */
SCM_EXPORT void
scm_fileport_init(void)
{
}

static ScmBytePort *
fileport_new_internal(FILE *file, const char *aux_info, scm_bool ownership)
{
    ScmFilePort *port;

    port = SCM_PORT_MALLOC(sizeof(ScmFilePort));

    port->vptr = ScmFilePort_vptr;
    port->file = file;
    port->aux_info = SCM_PORT_STRDUP(aux_info);
    port->ownership = ownership;

    return (ScmBytePort *)port;
}

SCM_EXPORT ScmBytePort *
ScmFilePort_new(FILE *file, const char *aux_info)
{
    return fileport_new_internal(file, aux_info, scm_true);
}

SCM_EXPORT ScmBytePort *
ScmFilePort_new_shared(FILE *file, const char *aux_info)
{
    return fileport_new_internal(file, aux_info, scm_false);
}

SCM_EXPORT ScmBytePort *
ScmFilePort_open_input_file(const char *path)
{
    FILE *file;

    file = fopen(path, "rb");
    return (file) ? ScmFilePort_new(file, path) : NULL;
}

SCM_EXPORT ScmBytePort *
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
        combined = SCM_PORT_MALLOC(size);
        sprintf(combined, "file %s", port->aux_info);
        return combined;
    } else {
        return SCM_PORT_STRDUP("file");
    }
}

static scm_ichar_t
fileport_get_byte(ScmFilePort *port)
{
    return getc(port->file);
}

static scm_ichar_t
fileport_peek_byte(ScmFilePort *port)
{
    scm_ichar_t ch;

    ch = getc(port->file);
    return ungetc(ch, port->file);
}

static scm_bool
fileport_byte_readyp(ScmFilePort *port)
{
    /* FIXME: does not support a FILE based on a pipe, or opened by
     * fdopen(3) */
#if HAVE_FILENO
    if (fileno(port->file) >= 0)
        SCM_BYTEPORT_ERROR(port, "known bug: ready? operation is not supported on this port");
#endif
    return scm_true;
}

static int
fileport_puts(ScmFilePort *port, const char *str)
{
    return fputs(str, port->file);
}

static size_t
fileport_write(ScmFilePort *port, size_t nbytes, const char *buf)
{
    return fwrite(buf, sizeof(char), nbytes, port->file);
}

static int
fileport_flush(ScmFilePort *port)
{
    return fflush(port->file);
}
