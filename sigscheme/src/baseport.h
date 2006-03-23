/*===========================================================================
 *  FileName : baseport.h
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

#ifndef __SCM_BASEPORT_H
#define __SCM_BASEPORT_H

#ifdef __cplusplus
extern "C" {
#endif

/*=======================================
  System Include
=======================================*/
#include <stdlib.h>
#include <stdio.h>  /* for EOF */
#include <stdarg.h>

/*=======================================
  Local Include
=======================================*/
#include "my-stdint.h"
#include "encoding.h"

/*=======================================
  Macro Definitions
=======================================*/
#ifndef SCM_DEBUG_PORT
#define SCM_DEBUG_PORT 0
#endif

/*
 * Define appropriate error handling such as exception to override these. The
 * macro MUST NOT return. The replacement expression should indicate that it
 * will not return, in compiler specific way such as noreturn attribute of GCC.
 */
#ifndef SCM_CHARPORT_ERROR
#define SCM_CHARPORT_ERROR(cport, msg) (exit(EXIT_FAILURE))
#endif /* SCM_CHARPORT_ERROR */
#ifndef SCM_BYTEPORT_ERROR
#define SCM_BYTEPORT_ERROR(bport, msg) (exit(EXIT_FAILURE))
#endif /* SCM_BYTEPORT_ERROR */

#define SCM_PORT_ERROR_INVALID_TYPE(klass, port, type)                       \
    SCM_##klass##PORT_ERROR((port), #type ": invalid object is passed to")
#define SCM_PORT_ERROR_INVALID_OPERATION(klass, port, type)                  \
    SCM_##klass##PORT_ERROR((port), #type ": invalid operation")
#define SCM_PORT_ERROR_NOMEM(klass, port, type)                              \
    SCM_##klass##PORT_ERROR((port), #type ": Out of memory")

/* Allocation error handling in the macros is strongly recommended. */
#ifndef SCM_PORT_MALLOC
#define SCM_PORT_MALLOC(size) (malloc(size))
#endif /* SCM_PORT_MALLOC */
#ifndef SCM_PORT_CALLOC
#define SCM_PORT_CALLOC(number, size) (calloc(number, size))
#endif /* SCM_PORT_CALLOC */
#ifndef SCM_PORT_REALLOC
#define SCM_PORT_REALLOC(ptr, size) (realloc(ptr, size))
#endif /* SCM_PORT_REALLOC */

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
#ifndef SCM_BOOL_DEFINED
typedef int scm_bool;
#define scm_false 0
#define scm_true  1
#define SCM_BOOL_DEFINED
#endif /* SCM_BOOL_DEFINED */

#ifndef SCM_ICHAR_T_DEFINED
typedef int32_t            scm_ichar_t;
#define SIZEOF_SCM_ICHAR_T SIZEOF_INT32_T
#define SCM_ICHAR_T_MAX    INT32_MAX
#define SCM_ICHAR_T_MIN    INT32_MIN
#if (EOF < SCM_ICHAR_T_MIN || SCM_ICHAR_T_MAX < EOF)
#error "scm_ichar_t cannot represent EOF on this platform"
#endif
#define SCM_ICHAR_T_DEFINED
#endif /* SCM_ICHAR_T_DEFINED */

#ifndef SCM_BYTE_T_DEFINED
#define SCM_BYTE_T_DEFINED
typedef unsigned char      scm_byte_t;
#define SIZEOF_SCM_BYTE_T  1
#endif /* SCM_BYTE_T_DEFINED */

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
extern const ScmCharPortVTbl *ScmBaseCharPort_vptr;

/*=======================================
   Function Declarations
=======================================*/
void ScmBaseCharPort_construct(ScmBaseCharPort *port,
                               const ScmCharPortVTbl *vptr,
                               ScmBytePort *bport);
char *ScmBaseCharPort_inspect(ScmBaseCharPort *port, const char *header);
int ScmBaseCharPort_line_number(ScmBaseCharPort *port);

#ifdef __cplusplus
}
#endif

#endif /* __SCM_BASEPORT_H */
