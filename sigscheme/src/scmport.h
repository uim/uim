/*===========================================================================
 *  Filename : scmport.h
 *  About    : Abstract base of port implementation
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
 * This file is intended to be portable. Don't depend on SigScheme and don't
 * merge into another file.
 */

#ifndef __SCM_SCMPORT_H
#define __SCM_SCMPORT_H

#include <stddef.h>

#include "sigscheme-stdint.h"
#include "scmint.h"
#include "global.h"
#include "encoding.h"

#ifdef __cplusplus
extern "C" {
#endif

/*=======================================
  Macro Definitions
=======================================*/
#ifndef SCM_DEBUG_PORT
#define SCM_DEBUG_PORT 0
#endif

#define SCM_PORT_ERROR_INVALID_TYPE(klass, port, type)                       \
    SCM_##klass##PORT_ERROR((port), #type ": invalid object is passed to")
#define SCM_PORT_ERROR_INVALID_OPERATION(klass, port, type)                  \
    SCM_##klass##PORT_ERROR((port), #type ": invalid operation")
#define SCM_PORT_ERROR_NOMEM(klass, port, type)                              \
    SCM_##klass##PORT_ERROR((port), #type ": Out of memory")

/*
 * To allow safe method invocation (contains from subclasses), all non-standard
 * method must call SCM_PORT_*DYNAMIC_CAST() explicitly.
 */
#define SCM_CHARPORT_DYNAMIC_CAST(type, obj)                                 \
    (SCM_PORT_DYNAMIC_CAST(CHAR, type, (obj)))
#define SCM_BYTEPORT_DYNAMIC_CAST(type, obj)                                 \
    (SCM_PORT_DYNAMIC_CAST(BYTE, type, (obj)))
#define SCM_PORT_DYNAMIC_CAST(klass, type, obj)                              \
    ((SCM_PORT_TRY_DYNAMIC_CAST(type, (obj))) ?                              \
     ((type *)(obj)) : (SCM_PORT_ERROR_INVALID_TYPE(klass, (obj), type), NULL))
#define SCM_PORT_TRY_DYNAMIC_CAST(type, obj)                                 \
    ((type *)(*(obj)->vptr->dyn_cast)((obj), type##_vptr))

#define SCM_CHARPORT_CLOSE(cport)        ((*(cport)->vptr->close)(cport))
#define SCM_CHARPORT_CODEC(cport)        ((*(cport)->vptr->codec)(cport))
#define SCM_CHARPORT_ENCODING(cport)                                         \
    (SCM_CHARCODEC_ENCODING(SCM_CHARPORT_CODEC(cport)))
#define SCM_CHARPORT_CCS(cport)                                              \
    (SCM_CHARCODEC_CCS(SCM_CHARPORT_CODEC(cport)))
#define SCM_CHARPORT_INSPECT(cport)      ((*(cport)->vptr->inspect)(cport))
#define SCM_CHARPORT_GET_CHAR(cport)     ((*(cport)->vptr->get_char)(cport))
#define SCM_CHARPORT_PEEK_CHAR(cport)    ((*(cport)->vptr->peek_char)(cport))
#define SCM_CHARPORT_CHAR_READYP(cport)  ((*(cport)->vptr->char_readyp)(cport))
#define SCM_CHARPORT_PUTS(cport, str)                                        \
    ((*(cport)->vptr->puts)((cport), (str)))
#define SCM_CHARPORT_PUT_CHAR(cport, ch)                                     \
    ((*(cport)->vptr->put_char)((cport), (ch)))
#define SCM_CHARPORT_FLUSH(cport)        ((*(cport)->vptr->flush)(cport))

#define SCM_BYTEPORT_CLOSE(bport)        ((*(bport)->vptr->close)(bport))
#define SCM_BYTEPORT_INSPECT(bport)      ((*(bport)->vptr->inspect)(bport))
#define SCM_BYTEPORT_GET_BYTE(bport)     ((*(bport)->vptr->get_byte)(bport))
#define SCM_BYTEPORT_PEEK_BYTE(bport)    ((*(bport)->vptr->peek_byte)(bport))
#define SCM_BYTEPORT_BYTE_READYP(bport)  ((*(bport)->vptr->byte_readyp)(bport))
#define SCM_BYTEPORT_PUTS(bport, str)                                        \
    ((*(bport)->vptr->puts)((bport), (str)))
#define SCM_BYTEPORT_WRITE(bport, nbytes, buf)                               \
    ((*(bport)->vptr->write)((bport), (nbytes), (buf)))
#define SCM_BYTEPORT_FLUSH(bport)        ((*(bport)->vptr->flush)(bport))

/*=======================================
  Type Definitions
=======================================*/
typedef struct ScmCharPortVTbl_ ScmCharPortVTbl;
typedef struct ScmCharPort_     ScmCharPort;
typedef struct ScmBaseCharPort_ ScmBaseCharPort;
typedef struct ScmBytePortVTbl_ ScmBytePortVTbl;
typedef struct ScmBytePort_     ScmBytePort;

/*
 * char port
 */
typedef ScmCharPort *(*ScmCharPortMethod_dyn_cast)(ScmCharPort *cport, const ScmCharPortVTbl *dst_vptr);
typedef int (*ScmCharPortMethod_close)(ScmCharPort *cport);
typedef ScmCharCodec *(*ScmCharPortMethod_codec)(ScmCharPort *cport);
/* returns brief information */
typedef char *(*ScmCharPortMethod_inspect)(ScmCharPort *cport);

/* input */
typedef scm_ichar_t (*ScmCharPortMethod_get_char)(ScmCharPort *cport);
typedef scm_ichar_t (*ScmCharPortMethod_peek_char)(ScmCharPort *cport);
typedef scm_bool (*ScmCharPortMethod_char_readyp)(ScmCharPort *cport);

/* output */
typedef int (*ScmCharPortMethod_puts)(ScmCharPort *cport, const char *str);
typedef int (*ScmCharPortMethod_put_char)(ScmCharPort *cport, scm_ichar_t ch);
typedef int (*ScmCharPortMethod_flush)(ScmCharPort *cport);

struct ScmCharPortVTbl_ {
    ScmCharPortMethod_dyn_cast    dyn_cast;
    ScmCharPortMethod_close       close;
    ScmCharPortMethod_codec       codec;
    ScmCharPortMethod_inspect     inspect;
    ScmCharPortMethod_get_char    get_char;
    ScmCharPortMethod_peek_char   peek_char;
    ScmCharPortMethod_char_readyp char_readyp;
    ScmCharPortMethod_puts        puts;
    ScmCharPortMethod_put_char    put_char;
    ScmCharPortMethod_flush       flush;
};

struct ScmCharPort_ {
    const ScmCharPortVTbl *vptr;
};

struct ScmBaseCharPort_ {  /* inherits ScmCharPort */
    const ScmCharPortVTbl *vptr;

    ScmBytePort *bport;  /* protected */
    int linenum;         /* protected */
};

/*
 * byte port
 */
typedef ScmBytePort *(*ScmBytePortMethod_dyn_cast)(ScmBytePort *bport, const ScmBytePortVTbl *dst_vptr);
typedef int (*ScmBytePortMethod_close)(ScmBytePort *bport);
/* returns brief information */
typedef char *(*ScmBytePortMethod_inspect)(ScmBytePort *bport);

/* input */
typedef scm_ichar_t (*ScmBytePortMethod_get_byte)(ScmBytePort *bport);
typedef scm_ichar_t (*ScmBytePortMethod_peek_byte)(ScmBytePort *bport);
typedef scm_bool (*ScmBytePortMethod_byte_readyp)(ScmBytePort *bport);

/* output */
typedef int (*ScmBytePortMethod_puts)(ScmBytePort *bport, const char *str);
typedef size_t (*ScmBytePortMethod_write)(ScmBytePort *bport,
                                          size_t nbytes, const char *buf);
typedef int (*ScmBytePortMethod_flush)(ScmBytePort *bport);

struct ScmBytePortVTbl_ {
    ScmBytePortMethod_dyn_cast    dyn_cast;
    ScmBytePortMethod_close       close;
    ScmBytePortMethod_inspect     inspect;
    ScmBytePortMethod_get_byte    get_byte;
    ScmBytePortMethod_peek_byte   peek_byte;
    ScmBytePortMethod_byte_readyp byte_readyp;
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
SCM_EXTERN(const ScmCharPortVTbl *const ScmBaseCharPort_vptr);

/*=======================================
  Function Declarations
=======================================*/
SCM_EXPORT void ScmBaseCharPort_construct(ScmBaseCharPort *port,
                                          const ScmCharPortVTbl *vptr,
                                          ScmBytePort *bport);
SCM_EXPORT char *ScmBaseCharPort_inspect(ScmBaseCharPort *port,
                                         const char *header);
SCM_EXPORT int ScmBaseCharPort_line_number(ScmBaseCharPort *port);

SCM_EXPORT ScmObj scm_make_port(ScmCharPort *cport, enum ScmPortFlag flag);


#ifdef __cplusplus
}
#endif

#endif /* __SCM_SCMPORT_H */
