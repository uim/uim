/*===========================================================================
 *  Filename : scmint.h
 *  About    : Integer types for Scheme implementation
 *
 *  Copyright (C) 2005-2006 YAMAMOTO Kengo <yamaken AT bp.iij4u.or.jp>
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
 * This file is independent of SigScheme and can be used to implement Scheme
 * implementation-neutral generic subordinate parts, such as ports and
 * character encoding handlers. In our short-term development, this separation
 * aims that making the underlying port implementations and encoding handlers
 * directly usable from libuim without SigScheme. It is needed to make libuim
 * Scheme-implementation independent without problems caused by differences of
 * implementation-specific character encoding handling behaviors.
 *
 * The copyright will be succeeded to the uim Project once the subordinate
 * parts are completely separated from SigScheme.
 *
 *   -- YamaKen 2006-03-30
 */

#ifndef __SCM_SCMINT_H
#define __SCM_SCMINT_H

#include <sigscheme/config.h>

#include <limits.h>
#include <stdio.h>  /* for EOF */

#include "sigscheme-stdint.h"

#ifdef __cplusplus
extern "C" {
#endif

/*=======================================
  Macro Definitions
=======================================*/
#ifndef SCM_EMPTY_EXPR
#define SCM_EMPTY_EXPR ((void)0)
#endif

/*=======================================
  Type Definitions
=======================================*/
/*
 * My own Boolean type
 *
 * libsscm does not use C99 stdbool, its autoconf equivalent or popular
 * combination of {int, TRUE, FALSE}, to avoid system-dependent ABI
 * incompatibility (such as size difference) and client-dependent problem (such
 * as an unexpected assumption about TRUE value).
 *
 * The definition use plain typedef and macro definition to avoid
 * misrecognition about the usage of the type, such as enum-related ones.
 *
 *                           *** IMPORTANT ***
 *
 * Do not test a value with (val == scm_true). The scm_true is only A TYPICAL
 * VALUE FOR TRUE. Use (val) or (val != scm_false) instead.
 *
 */
typedef int scm_bool;
#define scm_false 0
#define scm_true  1

/*
 * Fixed bit width numbers
 *
 * This types define internal representation corresponding to each number
 * objects of Scheme.
 *
 * The configuration alters both ABI and storage implementation of
 * libsscm. Although it specifies the bit width, actual width varies for each
 * underlying storage implementation. Refer SCM_INT_BITS, SCM_INT_MAX and so
 * on to know such values.
 *
 * The integer type defaults to 64-bit on LP64 platforms.
 */
#if SCM_USE_64BIT_FIXNUM
typedef int64_t           scm_int_t;
typedef uint64_t          scm_uint_t;
#define SIZEOF_SCM_INT_T  SIZEOF_INT64_T
#define SIZEOF_SCM_UINT_T SIZEOF_INT64_T
#define SCM_INT_T_MAX     INT64_MAX
#define SCM_INT_T_MIN     INT64_MIN
#define SCM_UINT_T_MAX    UINT64_MAX
#elif SCM_USE_32BIT_FIXNUM
typedef int32_t           scm_int_t;
typedef uint32_t          scm_uint_t;
#define SIZEOF_SCM_INT_T  SIZEOF_INT32_T
#define SIZEOF_SCM_UINT_T SIZEOF_INT32_T
#define SCM_INT_T_MAX     INT32_MAX
#define SCM_INT_T_MIN     INT32_MIN
#define SCM_UINT_T_MAX    UINT32_MAX
#elif SCM_USE_INT_FIXNUM
typedef int               scm_int_t;
typedef unsigned int      scm_uint_t;
#define SIZEOF_SCM_INT_T  SIZEOF_INT
#define SIZEOF_SCM_UINT_T SIZEOF_INT
#define SCM_INT_T_MAX     INT_MAX
#define SCM_INT_T_MIN     INT_MIN
#define SCM_UINT_T_MAX    UINT_MAX
#else
#undef  SCM_USE_LONG_FIXNUM
#define SCM_USE_LONG_FIXNUM 1
typedef long              scm_int_t;
typedef unsigned long     scm_uint_t;
#define SIZEOF_SCM_INT_T  SIZEOF_LONG
#define SIZEOF_SCM_UINT_T SIZEOF_LONG
#define SCM_INT_T_MAX     LONG_MAX
#define SCM_INT_T_MIN     LONG_MIN
#define SCM_UINT_T_MAX    ULONG_MAX
#endif

/*
 * Integer representation of abstract reference to ScmObj
 *
 * This types define sufficient width integer which is capable of holding any
 * ScmRef that is used in currently selected storage implementation.
 *
 * A ScmRef is abstract reference to a ScmObj. It is usually a pointer, but do
 * not assume it since another representation may be used. For instance, a pair
 * of heap index and object index in the heap can be a ScmRef. In such case,
 * scm_uintref_t can address any object in a heap scattered in full 64-bit
 * address space even if the bit width of the reference is smaller than a
 * 64-bit pointer. So any size assumption between pointer and the reference
 * must not be coded.
 *
 * The integer representation is intended for low-level bitwise processing. Use
 * ScmRef instead for higher-level code.
 *
 * Since actual representation is entirely controlled in each storage
 * implementation, this configuration only specifies the ABI about maximum size
 * of reference objects. Deal with particular storage implementation if fine
 * tuning is required. Otherwise simply keep untouched.
 *
 * The type defaults to direct pointer represenation, so *LP64 gets 64-bit.
 */
#if SCM_USE_64BIT_SCMREF
typedef int64_t              scm_intref_t;
typedef uint64_t             scm_uintref_t;
#define SIZEOF_SCM_INTREF_T  SIZEOF_INT64_T
#define SIZEOF_SCM_UINTREF_T SIZEOF_INT64_T
#elif SCM_USE_32BIT_SCMREF
typedef int32_t              scm_intref_t;
typedef uint32_t             scm_uintref_t;
#define SIZEOF_SCM_INTREF_T  SIZEOF_INT32_T
#define SIZEOF_SCM_UINTREF_T SIZEOF_INT32_T
#else
#undef  SCM_USE_INTPTR_SCMREF
#define SCM_USE_INTPTR_SCMREF 1
typedef intptr_t             scm_intref_t;
typedef uintptr_t            scm_uintref_t;
#define SIZEOF_SCM_INTREF_T  SIZEOF_INTPTR_T
#define SIZEOF_SCM_UINTREF_T SIZEOF_INTPTR_T
#endif

/*
 * Integer representation of ScmObj
 *
 * This types define sufficient width integer which is capable of holding the
 * ScmObj that is used in currently selected storage implementation.
 *
 * A ScmObj is abstract Scheme object. Its represenation and size vary for each
 * storage implementations. But the size is surely defined as larger one of
 * scm_uint_t and scm_uintref_t. It can be assumed on coding.
 *
 * The integer representation is intended for low-level bitwise processing. Use
 * ScmObj instead for higher-level code.
 *
 * This configuration is passively chosen in accordance with the fixnum size
 * and reference size. And of course alters the ABI.
 */
#if (SIZEOF_SCM_INT_T < SIZEOF_SCM_INTREF_T)
typedef scm_intref_t         scm_intobj_t;
typedef scm_uintref_t        scm_uintobj_t;
#define SIZEOF_SCM_INTOBJ_T  SIZEOF_SCM_INTREF_T
#define SIZEOF_SCM_UINTOBJ_T SIZEOF_SCM_UINTREF_T
#else
typedef scm_int_t            scm_intobj_t;
typedef scm_uint_t           scm_uintobj_t;
#define SIZEOF_SCM_INTOBJ_T  SIZEOF_SCM_INT_T
#define SIZEOF_SCM_UINTOBJ_T SIZEOF_SCM_UINT_T
#endif

/*
 * Internal integer representation of Scheme character object
 *
 * The type is used to pass a Scheme-level character object in C codes.
 *
 * It is distinguished from the element of fixed-width character string
 * (scm_wchar_t). This integer type is defined as broad as capable of any
 * multibyte char, and not configurable, to keep ABI stable regardless of
 * configuration about scm_wchar_t.
 *
 * Actual bit width varies for each storage implementation. Refer
 * SCM_CHAR_BITS, SCM_CHAR_MAX and SCM_CHAR_MIN if needed.
 */
typedef int_fast32_t       scm_ichar_t;
#define SIZEOF_SCM_ICHAR_T SIZEOF_INT_FAST32_T
#define SCM_ICHAR_T_MAX    INT_FAST32_MAX
#define SCM_ICHAR_T_MIN    INT_FAST32_MIN
#if (EOF < SCM_ICHAR_T_MIN || SCM_ICHAR_T_MAX < EOF)
#error "scm_ichar_t cannot represent EOF on this platform"
#endif

/*
 * Definitive byte type
 *
 * To avoid the sign-extension problem, platform-dependent signedness variation
 * (for example, ARM compilers treat 'char' as 'unsigned char'), use this type
 * for raw strings and so on.
 */
typedef unsigned char      scm_byte_t;
#define SIZEOF_SCM_BYTE_T  1
#define SCM_BYTE_T_MAX     UCHAR_MAX
#define SCM_BYTE_T_MIN     0

/*
 * Constant-width character for strings (not used yet)
 */
#if SCM_HAS_4OCT_WCHAR
typedef uint32_t           scm_wchar_t;
#define SIZEOF_SCM_WCHAR_T SIZEOF_INT32_T
#elif SCM_HAS_2OCT_WCHAR
typedef uint16_t           scm_wchar_t;
#define SIZEOF_SCM_WCHAR_T SIZEOF_INT16_T
#else
typedef scm_byte_t         scm_wchar_t;
#define SIZEOF_SCM_WCHAR_T SIZEOF_SCM_BYTE_T
#endif

/* size constraints */
#if !(   SIZEOF_SCM_INT_T    == SIZEOF_SCM_UINT_T                            \
      && SIZEOF_SCM_INTREF_T == SIZEOF_SCM_UINTREF_T                         \
      && SIZEOF_SCM_INTOBJ_T == SIZEOF_SCM_UINTOBJ_T                         \
      && SIZEOF_SCM_INTREF_T <= SIZEOF_SCM_INTOBJ_T                          \
      && SIZEOF_SCM_INT_T    <= SIZEOF_SCM_INTOBJ_T                          \
      && (   SIZEOF_SCM_INTREF_T <= SIZEOF_SCM_INT_T                         \
          || SIZEOF_SCM_INTREF_T >= SIZEOF_SCM_INT_T)                        \
      && SIZEOF_SCM_WCHAR_T  <= SIZEOF_SCM_ICHAR_T                           \
      && SIZEOF_SCM_ICHAR_T  <= SIZEOF_SCM_INT_T)
#error "size constraints of primitive types are broken"
#endif

/*=======================================
  Variable Declarations
=======================================*/

/*=======================================
  Function Declarations
=======================================*/

#ifdef __cplusplus
}
#endif

#endif /* __SCM_SCMINT_H */
