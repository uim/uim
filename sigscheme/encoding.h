/*===========================================================================
 *  FileName : encoding.h
 *  About    : Character encoding handling
 *
 *  Copyright (C) 2005      by Kazuki Ohta (mover@hct.zaq.ne.jp)
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

/* This file is going to be portable. Don't depend on SigScheme if possible. */

#ifndef __SCM_ENCODING_H
#define __SCM_ENCODING_H

/*=======================================
  System Include
=======================================*/

/*=======================================
  Local Include
=======================================*/

/*=======================================
  Macro Definitions
=======================================*/
#define SCM_ENCODING_USE_WITH_SIGSCHEME 1

#define SCM_MB_MAX_LEN 4

#define SCM_MBS_SET_STR(mbs, s)         ((mbs).str = (s))
#define SCM_MBS_GET_STR(mbs)            ((mbs).str)
#define SCM_MBS_SET_SIZE(mbs, siz)      ((mbs).size = (siz))
#define SCM_MBS_GET_SIZE(mbs)           ((mbs).size)

#define SCM_MBCINFO_SET_SIZE SCM_MBS_SET_SIZE
#define SCM_MBCINFO_GET_SIZE SCM_MBS_GET_SIZE
#define SCM_MBCINFO_CLEAR_STATE SCM_MBS_CLEAR_STATE
#define SCM_MBCINFO_SET_STATE SCM_MBS_SET_STATE
#define SCM_MBCINFO_GET_STATE SCM_MBS_GET_STATE
#define SCM_MBCINFO_CLEAR_FLAG(inf)     ((inf).flag = 0)
#define SCM_MBCINFO_SET_ERROR(inf)      ((inf).flag |= 1)
#define SCM_MBCINFO_SET_INCOMPLETE(inf) ((inf).flag |= 2)
#define SCM_MBCINFO_ERRORP(inf)         ((inf).flag & 1)
#define SCM_MBCINFO_INCOMPLETEP(inf)    ((inf).flag & 2)
#define SCM_MBCINFO_INIT(inf)  (SCM_MBCINFO_SET_SIZE((inf), 0),  \
                                SCM_MBCINFO_CLEAR_STATE(inf),    \
                                SCM_MBCINFO_CLEAR_FLAG(inf))


#if SCM_USE_STATEFUL_ENCODING
#define SCM_MBS_GET_STATE(mbs)        ((mbs).state)
#define SCM_MBS_SET_STATE(mbs, stat)  ((mbs).state = (stat))
#define SCM_MBS_CLEAR_STATE(mbs)      ((mbs).state = 0)
#else
#define SCM_MBS_GET_STATE(mbs)        0
#define SCM_MBS_SET_STATE(mbs, stat)  0
#define SCM_MBS_CLEAR_STATE(mbs)      0
#endif
#define SCM_MBS_INIT(mbs)  (SCM_MBS_SET_STR((mbs), NULL), \
                            SCM_MBS_SET_SIZE((mbs), 0),   \
                            SCM_MBS_CLEAR_STATE(mbs))
#define SCM_MBS_SKIP_CHAR(mbs, inf)                                           \
    (SCM_MBS_SET_STR((mbs), SCM_MBS_GET_STR(mbs) + SCM_MBCINFO_GET_SIZE(inf)),\
     SCM_MBS_SET_SIZE((mbs),                                                  \
                      SCM_MBS_GET_SIZE(mbs) - SCM_MBCINFO_GET_SIZE(inf)),     \
     SCM_MBS_SET_STATE((mbs), SCM_MBCINFO_GET_STATE(inf)))

#define SCM_CHARCODEC_ENCODING(codec)           ((*codec->encoding)())
#define SCM_CHARCODEC_SCAN_CHAR(codec, mbs)     ((*codec->scan_char)(mbs))
#define SCM_CHARCODEC_STR2INT(codec, start, nbytes)                          \
    ((*codec->str2int)(start, nbytes))
#define SCM_CHARCODEC_INT2STR(codec, start, ch) ((*codec->int2str)(start, ch))

/*=======================================
  Type Definitions
=======================================*/
/* This type will actually contain some encoding-dependent enum value.
 * It might as well be defined as mbstate_t if we're using libc. */
typedef int ScmMultibyteState;

/* Metadata of a multibyte character.  These are usually allocated on
   stack or register, so we'll make liberal use of space. */
typedef struct {
    const char *start;
    int flag;
    int size;

#if SCM_USE_STATEFUL_ENCODING
    /* Shift state at the *end* of the described character. */
    ScmMultibyteState state;
#endif
} ScmMultibyteCharInfo;

typedef struct {
    const char *str;

    /* Only the size is stored because ScmObj caches the length, and
     * we'll have to traverse from the beginning all the time
     * anyway. */
    int size;
#if SCM_USE_STATEFUL_ENCODING
    ScmMultibyteState state;
#endif
} ScmMultibyteString;

typedef struct ScmCharCodecVTbl_ ScmCharCodecVTbl;
typedef ScmCharCodecVTbl ScmCharCodec;

typedef const char *(*ScmCharCodecMethod_encoding)(void);
typedef ScmMultibyteCharInfo (*ScmCharCodecMethod_scan_char)(ScmMultibyteString mbs);
typedef int (*ScmCharCodecMethod_str2int)(const char *start, size_t nbytes);
typedef char *(*ScmCharCodecMethod_int2str)(char *start, int ch);

struct ScmCharCodecVTbl_ {
    ScmCharCodecMethod_encoding  encoding;
    ScmCharCodecMethod_scan_char scan_char;
    ScmCharCodecMethod_str2int   str2int;
    ScmCharCodecMethod_int2str   int2str;
};

/*=======================================
   Variable Declarations
=======================================*/
extern ScmMultibyteCharInfo (*Scm_mb_scan_char)(ScmMultibyteString mbs);

/*=======================================
   Function Declarations
=======================================*/
int Scm_mb_strlen(ScmMultibyteString mbs);
int Scm_mb_bare_c_strlen(const char *str);
ScmMultibyteString Scm_mb_substring(ScmMultibyteString str, int i, int len);
#define Scm_mb_strref(str, i) (Scm_mb_substring((str), (i), 1))


#endif /* __SCM_ENCODING_H */
