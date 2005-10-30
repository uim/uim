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

/*
 * To allow safe method invocation (contains from subclasses), all non-standard
 * method must call SCM_PORT_DYNAMIC_CAST() explicitly.
 */
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
typedef struct ScmBytePortVTbl_ ScmBytePortVTbl;
typedef struct ScmBytePort_     ScmBytePort;

/*
 * char port
 */
typedef ScmCharPort *(*ScmCharPortMethod_dyn_cast)(ScmCharPort *cport, const ScmCharPortVTbl *dst_vptr);
typedef int (*ScmCharPortMethod_close)(ScmCharPort *cport);
/* returns "UTF-8", "eucJP" and so on */
typedef const char *(*ScmCharPortMethod_encoding)(ScmCharPort *cport);

/* input */
typedef int (*ScmCharPortMethod_get_char)(ScmCharPort *cport);
typedef int (*ScmCharPortMethod_peek_char)(ScmCharPort *cport);
typedef int (*ScmCharPortMethod_char_readyp)(ScmCharPort *cport);

/* output */
typedef int (*ScmCharPortMethod_vprintf)(ScmCharPort *cport,
                                         const char *str, va_list args);
typedef int (*ScmCharPortMethod_put_char)(ScmCharPort *cport, int ch);
typedef int (*ScmCharPortMethod_flush)(ScmCharPort *cport);

struct ScmCharPortVTbl_ {
    ScmCharPortMethod_dyn_cast    dyn_cast;
    ScmCharPortMethod_close       close;
    ScmCharPortMethod_encoding    encoding;
    ScmCharPortMethod_get_char    get_char;
    ScmCharPortMethod_peek_char   peek_char;
    ScmCharPortMethod_char_readyp char_readyp;
    ScmCharPortMethod_vprintf     vprintf;  /* tmp */
    ScmCharPortMethod_put_char    put_char;
    ScmCharPortMethod_flush       flush;
};

struct ScmCharPort_ {
    const ScmCharPortVTbl *vptr;
};

/*
 * byte port
 */
typedef ScmBytePort *(*ScmBytePortMethod_dyn_cast)(ScmBytePort *bport, const ScmBytePortVTbl *dst_vptr);
typedef int (*ScmBytePortMethod_close)(ScmBytePort *bport);

/* input */
typedef int (*ScmBytePortMethod_get_byte)(ScmBytePort *bport);
typedef int (*ScmBytePortMethod_peek_byte)(ScmBytePort *bport);
typedef int (*ScmBytePortMethod_byte_readyp)(ScmBytePort *bport);

/* output */
typedef int (*ScmBytePortMethod_vprintf)(ScmBytePort *bport,
                                         const char *str, va_list args);
typedef int (*ScmBytePortMethod_puts)(ScmBytePort *bport, const char *str);
typedef size_t (*ScmBytePortMethod_write)(ScmBytePort *bport,
                                          size_t nbytes, const char *buf);
typedef int (*ScmBytePortMethod_flush)(ScmBytePort *bport);

struct ScmBytePortVTbl_ {
    ScmBytePortMethod_dyn_cast    dyn_cast;
    ScmBytePortMethod_close       close;
    ScmBytePortMethod_get_byte    get_byte;
    ScmBytePortMethod_peek_byte   peek_byte;
    ScmBytePortMethod_byte_readyp byte_readyp;
    ScmBytePortMethod_vprintf     vprintf;  /* tmp */
    ScmBytePortMethod_puts        puts;
    ScmBytePortMethod_write       write;
    ScmBytePortMethod_flush       flush;
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
