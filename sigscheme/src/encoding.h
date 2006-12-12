/*===========================================================================
 *  Filename : encoding.h
 *  About    : Character encoding handling
 *
 *  Copyright (C) 2005-2006 Kazuki Ohta <mover AT hct.zaq.ne.jp>
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

#ifndef __SCM_ENCODING_H
#define __SCM_ENCODING_H

#include <sigscheme/config.h>

#include <stddef.h>

#include "scmint.h"
#include "global.h"

#ifdef __cplusplus
extern "C" {
#endif

/*=======================================
  Macro Definitions
=======================================*/
#define SCM_MB_MAX_LEN 4
#define SCM_MB_CHAR_BUF_SIZE (SCM_MB_MAX_LEN + sizeof(""))

#define SCM_HAS_2OCT_WCHAR 0
#define SCM_HAS_4OCT_WCHAR 0

#define SCM_MBCINFO_SET_SIZE(inf, siz)   ((inf).size = (siz))
#define SCM_MBCINFO_GET_SIZE(inf)        ((inf).size)
#if SCM_USE_STATEFUL_ENCODING
#define SCM_MBCINFO_CLEAR_STATE(inf)     ((inf).state = 0)
#define SCM_MBCINFO_SET_STATE(inf, stat) ((inf).state = (stat))
#define SCM_MBCINFO_GET_STATE(inf)       ((inf).state)
#else /* SCM_USE_STATEFUL_ENCODING */
#define SCM_MBCINFO_CLEAR_STATE(inf)     SCM_EMPTY_EXPR
#define SCM_MBCINFO_SET_STATE(inf, stat) SCM_EMPTY_EXPR
#define SCM_MBCINFO_GET_STATE(inf)       SCM_MB_STATELESS
#endif /* SCM_USE_STATEFUL_ENCODING */
#define SCM_MBCINFO_CLEAR_FLAG(inf)      ((inf).flag = 0)
#define SCM_MBCINFO_SET_ERROR(inf)       ((inf).flag |= 1)
#define SCM_MBCINFO_SET_INCOMPLETE(inf)  ((inf).flag |= 2)
#define SCM_MBCINFO_ERRORP(inf)          ((inf).flag & 1)
#define SCM_MBCINFO_INCOMPLETEP(inf)     ((inf).flag & 2)
#define SCM_MBCINFO_INIT(inf)                                                \
    do {                                                                     \
        SCM_MBCINFO_SET_SIZE((inf), 0);                                      \
        SCM_MBCINFO_CLEAR_STATE(inf);                                        \
        SCM_MBCINFO_CLEAR_FLAG(inf);                                         \
    } while (/* CONSTCOND */ 0)


#define SCM_MBS_SET_STR(mbs, s)       ((mbs).str = (s))
#define SCM_MBS_GET_STR(mbs)          ((mbs).str)
#define SCM_MBS_SET_SIZE(mbs, siz)    ((mbs).size = (siz))
#define SCM_MBS_GET_SIZE(mbs)         ((mbs).size)
#if SCM_USE_STATEFUL_ENCODING
#define SCM_MBS_GET_STATE(mbs)        ((mbs).state)
#define SCM_MBS_SET_STATE(mbs, stat)  ((mbs).state = (stat))
#define SCM_MBS_CLEAR_STATE(mbs)      ((mbs).state = 0)
#else
#define SCM_MBS_GET_STATE(mbs)        SCM_MB_STATELESS
#define SCM_MBS_SET_STATE(mbs, stat)  SCM_EMPTY_EXPR
#define SCM_MBS_CLEAR_STATE(mbs)      SCM_EMPTY_EXPR
#endif
#define SCM_MBS_INIT(mbs)                                                    \
    do {                                                                     \
        SCM_MBS_SET_STR((mbs), NULL);                                        \
        SCM_MBS_SET_SIZE((mbs), 0);                                          \
        SCM_MBS_CLEAR_STATE(mbs);                                            \
    } while (/* CONSTCOND */ 0)
#define SCM_MBS_INIT2(mbs, s, siz)                                           \
    do {                                                                     \
        SCM_MBS_SET_STR((mbs), (s));                                         \
        SCM_MBS_SET_SIZE((mbs), (siz));                                      \
        SCM_MBS_CLEAR_STATE(mbs);                                            \
    } while (/* CONSTCOND */ 0)
#define SCM_MBS_SKIP_CHAR(mbs, inf)                                          \
    do {                                                                     \
        SCM_MBS_SET_STR((mbs),                                               \
                        SCM_MBS_GET_STR(mbs) + SCM_MBCINFO_GET_SIZE(inf));   \
        SCM_MBS_SET_SIZE((mbs),                                              \
                         SCM_MBS_GET_SIZE(mbs) - SCM_MBCINFO_GET_SIZE(inf)); \
        SCM_MBS_SET_STATE((mbs), SCM_MBCINFO_GET_STATE(inf));                \
    } while (/* CONSTCOND */ 0)

#define SCM_CHARCODEC_STATEFULP(codec)          ((*(codec)->statefulp)())
#define SCM_CHARCODEC_ENCODING(codec)           ((*(codec)->encoding)())
#define SCM_CHARCODEC_CCS(codec)                ((*(codec)->ccs)())
#define SCM_CHARCODEC_CHAR_LEN(codec, ch)       ((*(codec)->char_len)(ch))
#define SCM_CHARCODEC_SCAN_CHAR(codec, mbs)     ((*(codec)->scan_char)(mbs))
#define SCM_CHARCODEC_STR2INT(codec, src, len, state)                        \
    ((*(codec)->str2int)((src), (len), (state)))
#define SCM_CHARCODEC_INT2STR(codec, dst, ch, state)                         \
    ((*(codec)->int2str)((dst), (ch), (state)))
#define SCM_CHARCODEC_READ_CHAR(codec, mbs)                                  \
    (scm_charcodec_read_char((codec), &(mbs), SCM_MANGLE(name)))

/*=======================================
  Type Definitions
=======================================*/
enum ScmCodedCharSet {
    SCM_CCS_UNKNOWN   = 0,
    SCM_CCS_UCS2      = 1,
    SCM_CCS_UCS4      = 2,
    SCM_CCS_ISO8859_1 = 10,
    SCM_CCS_JIS       = 30  /* ASCII + JIS X 0208, 0212, 0213 */
};

/* This type will actually contain some encoding-dependent enum value.
 * It might as well be defined as mbstate_t if we're using libc. */
typedef int ScmMultibyteState;
#define SCM_MB_STATELESS 0

/* Metadata of a multibyte character.  These are usually allocated on
   stack or register, so we'll make liberal use of space. */
typedef struct {
#if 0
#if 0
    /* will be changed to this */
    const scm_byte_t *start;
#else
    const char *start;
#endif
#endif
    size_t size;
    int flag;

#if SCM_USE_STATEFUL_ENCODING
    /* Shift state at the *end* of the described character. */
    ScmMultibyteState state;
#endif
} ScmMultibyteCharInfo;

typedef struct {
#if 0
    /* will be changed to this */
    const scm_byte_t *str;
#else
    const char *str;
#endif

    /* Only the size is stored because ScmObj caches the length, and
     * we'll have to traverse from the beginning all the time
     * anyway. */
    size_t size;
#if SCM_USE_STATEFUL_ENCODING
    ScmMultibyteState state;
#endif
} ScmMultibyteString;

typedef struct ScmCharCodecVTbl_ ScmCharCodecVTbl;
typedef const ScmCharCodecVTbl ScmCharCodec;

typedef scm_bool (*ScmCharCodecMethod_statefulp)(void);
typedef const char *(*ScmCharCodecMethod_encoding)(void);
typedef enum ScmCodedCharSet (*ScmCharCodecMethod_ccs)(void);
typedef int (*ScmCharCodecMethod_char_len)(scm_ichar_t ch);
typedef ScmMultibyteCharInfo (*ScmCharCodecMethod_scan_char)(ScmMultibyteString mbs);
typedef scm_ichar_t (*ScmCharCodecMethod_str2int)(const char *src, size_t len,
                                                  ScmMultibyteState state);
typedef char *(*ScmCharCodecMethod_int2str)(char *dst, scm_ichar_t ch,
                                            ScmMultibyteState state);

struct ScmCharCodecVTbl_ {
    ScmCharCodecMethod_statefulp statefulp;
    ScmCharCodecMethod_encoding  encoding;
    ScmCharCodecMethod_ccs       ccs;
    ScmCharCodecMethod_char_len  char_len;
    ScmCharCodecMethod_scan_char scan_char;
    ScmCharCodecMethod_str2int   str2int;
    ScmCharCodecMethod_int2str   int2str;
};

/*=======================================
  Variable Declarations
=======================================*/
SCM_GLOBAL_VARS_BEGIN(encoding);
ScmCharCodec *scm_current_char_codec;
SCM_GLOBAL_VARS_END(encoding);
#define scm_current_char_codec SCM_GLOBAL_VAR(encoding, scm_current_char_codec)
SCM_DECLARE_EXPORTED_VARS(encoding);

/*=======================================
  Function Declarations
=======================================*/
SCM_EXPORT void scm_encoding_init(void);

SCM_EXPORT size_t scm_mb_strlen(ScmCharCodec *codec, ScmMultibyteString mbs);
SCM_EXPORT size_t scm_mb_bare_c_strlen(ScmCharCodec *codec, const char *str);
SCM_EXPORT ScmMultibyteString scm_mb_substring(ScmCharCodec *codec,
                                               ScmMultibyteString str,
                                               size_t i, size_t len);
#define scm_mb_strref(codec, str, i) (scm_mb_substring((codec), (str), (i), 1))
SCM_EXPORT ScmCharCodec *scm_mb_find_codec(const char *encoding);
SCM_EXPORT scm_ichar_t scm_charcodec_read_char(ScmCharCodec *codec,
                                               ScmMultibyteString *mbs,
                                               const char *caller);

#ifdef __cplusplus
}
#endif

#endif /* __SCM_ENCODING_H */
