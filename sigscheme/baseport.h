/*===========================================================================
 *  FileName : baseport.h
 *  About    : Abstract base of port implementation
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
 * This file is intended to be portable. Don't depend on SigScheme and don't
 * merge into another file.
 */

#ifndef __SCM_BASEPORT_H
#define __SCM_BASEPORT_H

/*=======================================
  System Include
=======================================*/
#include <stdlib.h>
#include <stdarg.h>

/*=======================================
  Local Include
=======================================*/

/*=======================================
  Macro Definitions
=======================================*/
/* define appropriate error handling such as exception to override these */
#ifndef SCM_CHARPORT_ERROR
#define SCM_CHARPORT_ERROR(cport, msg) (exit(EXIT_FAILURE))
#endif /* SCM_CHARPORT_ERROR */
#ifndef SCM_BYTEPORT_ERROR
#define SCM_BYTEPORT_ERROR(bport, msg) (exit(EXIT_FAILURE))
#endif /* SCM_BYTEPORT_ERROR */

#define SCM_PORT_DYNAMIC_CAST(type, obj)                                     \
     ((type *)(*obj->vptr->dyn_cast)(obj, &type##_vtbl))

#define SCM_CHARPORT_CLOSE(cport)        ((*cport->vptr->close)(cport))
#define SCM_CHARPORT_ENCODING(cport)     ((*cport->vptr->encoding)(cport))
#define SCM_CHARPORT_GET_CHAR(cport)     ((*cport->vptr->get_char)(cport))
#define SCM_CHARPORT_PEEK_CHAR(cport)    ((*cport->vptr->peek_char)(cport))
#define SCM_CHARPORT_CHAR_READYP(cport)  ((*cport->vptr->char_readyp)(cport))
#define SCM_CHARPORT_VPRINTF(cport, str, args)                               \
    ((*cport->vptr->vprintf)(cport, str, args))
#define SCM_CHARPORT_PUT_CHAR(cport, ch) ((*cport->vptr->put_char)(cport, ch))
#define SCM_CHARPORT_FLUSH(cport)        ((*cport->vptr->flush)(cport))

#define SCM_BYTEPORT_CLOSE(bport)        ((*bport->vptr->close)(bport))
#define SCM_BYTEPORT_GET_BYTE(bport)     ((*bport->vptr->get_byte)(bport))
#define SCM_BYTEPORT_PEEK_BYTE(bport)    ((*bport->vptr->peek_byte)(bport))
#define SCM_BYTEPORT_BYTE_READYP(bport)  ((*bport->vptr->byte_readyp)(bport))
#define SCM_BYTEPORT_VPRINTF(bport, str, args)                               \
    ((*bport->vptr->vprintf)(bport, str, args))
#define SCM_BYTEPORT_PUTS(bport, str)    ((*bport->vptr->puts)(bport, str))
#define SCM_BYTEPORT_WRITE(bport, nbytes, buf)                               \
    ((*bport->vptr->write)(bport, nbytes, buf))
#define SCM_BYTEPORT_FLUSH(bport)        ((*bport->vptr->flush)(bport))

/*=======================================
  Type Definitions
=======================================*/
typedef struct ScmCharPortVTbl_ ScmCharPortVTbl;
typedef struct ScmCharPort_     ScmCharPort;
typedef struct ScmBaseCharPort_ ScmBaseCharPort;
typedef struct ScmBytePortVTbl_ ScmBytePortVTbl;
typedef struct ScmBytePort_     ScmBytePort;


struct ScmCharPortVTbl_ {
    ScmCharPort *(*dyn_cast)(ScmCharPort *cport, const ScmCharPortVTbl *dst_vptr);
    int (*close)(ScmCharPort *cport);
    /* returns "UTF-8", "eucJP" and so on */
    const char *(*encoding)(ScmCharPort *cport);

    /* input */
    int (*get_char)(ScmCharPort *cport);
    int (*peek_char)(ScmCharPort *cport);
    int (*char_readyp)(ScmCharPort *cport);

    /* output */
    int (*vprintf)(ScmCharPort *cport, const char *str, va_list args); /* tmp */
    int (*put_char)(ScmCharPort *cport, int ch);
    int (*flush)(ScmCharPort *cport);
};

struct ScmCharPort_ {
    const ScmCharPortVTbl *vptr;
};

struct ScmBaseCharPort_ {
    const ScmCharPortVTbl *vptr;

    ScmBytePort *bport;
};

struct ScmBytePortVTbl_ {
    ScmBytePort *(*dyn_cast)(ScmBytePort *bport, const ScmBytePortVTbl *dst_vptr);
    int (*close)(ScmBytePort *bport);

    /* input */
    int (*get_byte)(ScmBytePort *bport);
    int (*peek_byte)(ScmBytePort *bport);
    int (*byte_readyp)(ScmBytePort *bport);

    /* output */
    int (*vprintf)(ScmBytePort *bport, const char *str, va_list args); /* tmp */
    int (*puts)(ScmBytePort *bport, const char *str);
    size_t (*write)(ScmBytePort *bport, size_t nbytes, const char *buf);
    int (*flush)(ScmBytePort *bport);
};

struct ScmBytePort_ {
    const ScmBytePortVTbl *vptr;
};

/*=======================================
   Variable Declarations
=======================================*/

/*=======================================
   Function Declarations
=======================================*/


#endif /* __SCM_BASEPORT_H */
