/*===========================================================================
 *  FileName : encoding.c
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
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS IS''
 *  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 *  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 *  ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR CONTRIBUTORS BE
 *  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 *  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
 *  GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 *  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 *  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 *  OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 *  SUCH DAMAGE.
===========================================================================*/

/* Acknowledgement: much information was gained from the
 * i18n-introduction of the debian project.  Many thanks to its
 * authors, Tomohiro KUBOTA, et al. */


/* This file is going to be portable. Don't depend on SigScheme if possible. */

/*=======================================
  System Include
=======================================*/
#include <stdlib.h>
#include <stdio.h>  /* for EOF */
#include <string.h>

/*=======================================
  Local Include
=======================================*/
#include "encoding.h"
#if SCM_ENCODING_USE_WITH_SIGSCHEME
#include "sigscheme.h"
#include "sigschemeinternal.h"
#endif

/*=======================================
  File Local Type Definitions
=======================================*/
/* FIXME: replace with C99-independent stdint */
typedef unsigned char uchar;
typedef unsigned int  uint;

/*=======================================
  File Local Functions
=======================================*/
static int pred_always_true(void);
static int pred_always_false(void);

#if SCM_USE_EUCJP
static const char *eucjp_encoding(void);
static enum ScmCodedCharSet eucjp_ccs(void);
static int eucjp_char_len(int ch);
static ScmMultibyteCharInfo eucjp_scan_char(ScmMultibyteString mbs);
static int eucjp_str2int(const uchar *src, size_t len,
                         ScmMultibyteState state);
static uchar *eucjp_int2str(uchar *dst, int ch, ScmMultibyteState state);
#endif

#if SCM_USE_ISO2022KR
static ScmMultibyteCharInfo iso2022kr_scan_char(ScmMultibyteString mbs);
static ScmMultibyteCharInfo iso2022kr_scan_input_char(ScmMultibyteString mbs);
#endif

#if SCM_USE_ISO2022JP
static ScmMultibyteCharInfo iso2022jp_scan_char(ScmMultibyteString mbs);
static ScmMultibyteCharInfo iso2022jp_scan_input_char(ScmMultibyteString mbs);
#endif

#if SCM_USE_SJIS
static const char *sjis_encoding(void);
 static enum ScmCodedCharSet sjis_ccs(void);
 static int sjis_char_len(int ch);
static ScmMultibyteCharInfo sjis_scan_char(ScmMultibyteString mbs);
static uchar *sjis_int2str(uchar *dst, int ch, ScmMultibyteState state);
#endif

#if (SCM_USE_EUCCN || SCM_USE_EUCKR || SCM_USE_SJIS)
/* generic double-byte char */
static int dbc_str2int(const uchar *src, size_t len, ScmMultibyteState state);
#endif

#if (SCM_USE_EUCCN || SCM_USE_EUCKR)
/* shared by EUCCN and EUCKR */
static int euc_char_len(int ch);
static uchar *euc_int2str(uchar *dst, int ch, ScmMultibyteState state);
#endif

#if SCM_USE_EUCCN
static const char *euccn_encoding(void);
static enum ScmCodedCharSet euccn_ccs(void);
static ScmMultibyteCharInfo euccn_scan_char(ScmMultibyteString mbs);
#endif

#if SCM_USE_EUCKR
static const char *euckr_encoding(void);
static enum ScmCodedCharSet euckr_ccs(void);
static ScmMultibyteCharInfo euckr_scan_char(ScmMultibyteString mbs);
#endif

#if SCM_USE_UTF8
static const char *utf8_encoding(void);
static enum ScmCodedCharSet utf8_ccs(void);
static int utf8_char_len(int ch);
static ScmMultibyteCharInfo utf8_scan_char(ScmMultibyteString mbs);
static int utf8_str2int(const uchar *src, size_t len, ScmMultibyteState state);
static uchar *utf8_int2str(uchar *dst, int ch, ScmMultibyteState state);
#endif

static const char *unibyte_encoding(void);
static enum ScmCodedCharSet unibyte_ccs(void);
static int unibyte_char_len(int ch);
static ScmMultibyteCharInfo unibyte_scan_char(ScmMultibyteString mbs);
static int unibyte_str2int(const uchar *src, size_t len,
                           ScmMultibyteState state);
static uchar *unibyte_int2str(uchar *dst, int ch, ScmMultibyteState state);

/*=======================================
  Local Variables
=======================================*/
#if SCM_USE_UTF8
static const ScmCharCodecVTbl utf8_codec_vtbl = {
    &pred_always_false,
    &utf8_encoding,
    &utf8_ccs,
    &utf8_char_len,
    &utf8_scan_char,
    (ScmCharCodecMethod_str2int)&utf8_str2int,
    (ScmCharCodecMethod_int2str)&utf8_int2str
};
#define utf8_codec (&utf8_codec_vtbl)
#endif

#if SCM_USE_EUCCN
static const ScmCharCodecVTbl euccn_codec_vtbl = {
    &pred_always_false,
    &euccn_encoding,
    &euccn_ccs,
    &euc_char_len,
    &euccn_scan_char,
    (ScmCharCodecMethod_str2int)&dbc_str2int,
    (ScmCharCodecMethod_int2str)&euc_int2str
};
#define euccn_codec (&euccn_codec_vtbl)
#endif

#if SCM_USE_EUCJP
static const ScmCharCodecVTbl eucjp_codec_vtbl = {
    &pred_always_false,
    &eucjp_encoding,
    &eucjp_ccs,
    &eucjp_char_len,
    &eucjp_scan_char,
    (ScmCharCodecMethod_str2int)&eucjp_str2int,
    (ScmCharCodecMethod_int2str)&eucjp_int2str
};
#define eucjp_codec (&eucjp_codec_vtbl)
#endif

#if SCM_USE_EUCKR
static const ScmCharCodecVTbl euckr_codec_vtbl = {
    &pred_always_false,
    &euckr_encoding,
    &euckr_ccs,
    &euc_char_len,
    &euckr_scan_char,
    (ScmCharCodecMethod_str2int)&dbc_str2int,
    (ScmCharCodecMethod_int2str)&euc_int2str
};
#define euckr_codec (&euckr_codec_vtbl)
#endif

#if SCM_USE_SJIS
static const ScmCharCodecVTbl sjis_codec_vtbl = {
    &pred_always_false,
    &sjis_encoding,
    &sjis_ccs,
    &sjis_char_len,
    &sjis_scan_char,
    (ScmCharCodecMethod_str2int)&dbc_str2int,
    (ScmCharCodecMethod_int2str)&sjis_int2str
};
#define sjis_codec (&sjis_codec_vtbl)
#endif

static const ScmCharCodecVTbl unibyte_codec_vtbl = {
    &pred_always_false,
    &unibyte_encoding,
    &unibyte_ccs,
    &unibyte_char_len,
    &unibyte_scan_char,
    (ScmCharCodecMethod_str2int)&unibyte_str2int,
    (ScmCharCodecMethod_int2str)&unibyte_int2str
};
#define unibyte_codec (&unibyte_codec_vtbl)

static ScmCharCodec *available_codecs[] = {
#if SCM_USE_UTF8
    utf8_codec,
#endif
#if SCM_USE_EUCJP
    eucjp_codec,
#endif
#if SCM_USE_EUCCN
    euccn_codec,
#endif
#if SCM_USE_EUCKR
    euckr_codec,
#endif
#if SCM_USE_SJIS
    sjis_codec,
#endif
    unibyte_codec,
    NULL
};

/*=======================================
  Global Variables
=======================================*/
/* temporary solution */
ScmCharCodec *scm_current_char_codec
#if SCM_USE_UTF8_AS_DEFAULT
    = utf8_codec;
#elif SCM_USE_EUCJP_AS_DEFAULT
    = eucjp_codec;
#elif SCM_USE_EUCCN_AS_DEFAULT
    = euccn_codec;
#elif SCM_USE_EUCKR_AS_DEFAULT
    = euckr_codec;
#elif SCM_USE_SJIS_AS_DEFAULT
    = sjis_codec;
#else
    = unibyte_codec;
#endif

/*=======================================
  Public API
=======================================*/

int
scm_mb_strlen(ScmMultibyteString mbs)
{
    int len = 0;
    ScmMultibyteCharInfo c;

    CDBG((SCM_DBG_ENCODING, "mb_strlen: size = %d; str = %s;",
          SCM_MBS_GET_SIZE(mbs), SCM_MBS_GET_STR(mbs)));

    while (SCM_MBS_GET_SIZE(mbs)) {
        c = SCM_CHARCODEC_SCAN_CHAR(scm_current_char_codec, mbs);
        CDBG((SCM_DBG_ENCODING, "%d, %d;", SCM_MBCINFO_GET_SIZE(c), c.flag));
        SCM_MBS_SKIP_CHAR(mbs, c);
        len++;
    }

    CDBG((SCM_DBG_ENCODING, "len=%d\n", len));
    return len;
}

/* FIXME: pick a better name. */
int
scm_mb_bare_c_strlen(const char *s)
{
    ScmMultibyteString mbs;
    SCM_MBS_INIT(mbs);
    SCM_MBS_SET_STR(mbs, s);
    SCM_MBS_SET_SIZE(mbs, strlen(s));
    return scm_mb_strlen(mbs);
}

ScmMultibyteString
scm_mb_substring(ScmMultibyteString mbs, int i, int len)
{
    ScmMultibyteString ret;
    ScmMultibyteString end;
    ScmMultibyteCharInfo c;

    ret = mbs;

    while (i--) {
        c = SCM_CHARCODEC_SCAN_CHAR(scm_current_char_codec, ret);
        SCM_MBS_SKIP_CHAR(ret, c);
    }

    end = ret;

    while (len--) {
        c = SCM_CHARCODEC_SCAN_CHAR(scm_current_char_codec, end);
        SCM_MBS_SKIP_CHAR(end, c);
    }

    SCM_MBS_SET_SIZE(ret, SCM_MBS_GET_STR(end) - SCM_MBS_GET_STR(ret));
    return ret;
}

/* TODO: support encoding name canonicalization */
ScmCharCodec *
scm_mb_find_codec(const char *encoding)
{
    ScmCharCodec **codecp;

    for (codecp = &available_codecs[0]; *codecp; codecp++) {
        if (strcmp(SCM_CHARCODEC_ENCODING(*codecp), encoding) == 0)
            return *codecp;
    }

    return NULL;
}

/*=======================================
  Encoding-specific functions
=======================================*/

static int
pred_always_true(void)
{
    return 1;
}

static int
pred_always_false(void)
{
    return 0;
}

/* Every encoding implements the <encoding name>_scan_char()
 * primitive.  Its job is to determine the length of the first
 * character in the given string.  Stateful encodings should save
 * their state *at exit*, that is, the state right after reading the
 * first character (so don't omit it).  */

/* Convenience macros.  Start with ENTER and return with RETURN*.
 * EXPECT_SIZE() declares the expected length of the character.  We'll
 * use it to return information on how many octets are missing.  It
 * also serves as documentation.  */
#define ENTER   ScmMultibyteCharInfo _ret;  SCM_MBCINFO_INIT(_ret)
#define RETURN(n)  do { SCM_MBCINFO_SET_SIZE(_ret, n); return _ret; } while (0)
#define RETURN_ERROR() do { SCM_MBCINFO_SET_ERROR(_ret); RETURN(1); } while (0)
#define RETURN_INCOMPLETE(n) do { SCM_MBCINFO_SET_INCOMPLETE(_ret); RETURN(n); } while (0)
#define SAVE_STATE(stat) (SCM_MBCINFO_SET_STATE(_ret, (stat)))
#define EXPECT_SIZE(size) /* Currently ignored. */

/* Encodings based on ISO/IEC 2022. */

/* Control regions. */
#define IN_CL(c)   ((uchar)(c) < 0x20)
#define IN_CR(c)   (0x80 <= (uchar)(c) && (uchar)(c) <= 0x9F)

/* General purpose regions. */
#define IN_GL94(c) (0x21 <= (uchar)(c) && (uchar)(c) <= 0x7E)
#define IN_GL96(c) (0x20 <= (uchar)(c) && (uchar)(c) <= 0x7F)
#define IN_GR94(c) (0xA1 <= (uchar)(c) && (uchar)(c) <= 0xFE)
#define IN_GR96(c) (0xA0 <= (uchar)(c) && (uchar)(c) <= 0xFF)

#define IS_ASCII(c) ((uint)(c) <= 0x7F)
#define IS_GR_SPC_OR_DEL(c)  ((uchar)(c) == 0xA0 || (uchar)(c) == 0xFF)

#define CHAR_BITS    8
#define BYTE_MASK    0xFF
#define IS_1BYTE(e)  ((uint)(e) <= 0x7F)
#define IS_2BYTES(e) ((uint)(e) <= 0xFFFF)
#define IS_3BYTES(e) ((uint)(e) <= ((SS3 << CHAR_BITS * 2) | 0xFFFF))

#define ESC 0x1B
#define SO  0x0E
#define SI  0x0F
#define SS2 0x8E
#define SS3 0x8F

#if SCM_USE_EUCJP
static const char *
eucjp_encoding(void)
{
    return "EUC-JP";
}

enum ScmCodedCharSet
eucjp_ccs(void)
{
    return SCM_CCS_JIS;
}

/* FIXME: Optimize */
int
eucjp_char_len(int ch)
{
    char buf[SCM_MB_MAX_LEN + sizeof("")];

    return (eucjp_int2str((uchar *)buf, ch, SCM_MB_STATELESS)) ? strlen(buf) : 0;
}

/* G0 <- (96) ASCII (or was it JIS X 0201 Roman?)
 * G1 <- (94x94) JIS X 0208 kanji/kana
 * G2 <- (94) JIS X 0201 Katakana ("half-width katakana")
 * G3 <- (94x94) JIS X 0212 kanji, or JIS X 0213 kanji plane 2
 *
 * GL <- G0 (ASCII)
 * GR <- G1 (JIS X 0208)
 * CL <- JIS X 0211 C0
 * CR <- JIS X 0211 C1 */
static ScmMultibyteCharInfo
eucjp_scan_char(ScmMultibyteString mbs)
{
    const char *str = SCM_MBS_GET_STR(mbs);
    const int size  = SCM_MBS_GET_SIZE(mbs);
    ENTER;

    if (!size)
        RETURN(0);

    if (IN_CL(str[0]) || IN_GL96(str[0]))
        RETURN(1);
    else if (IN_GR94(str[0]) || (uchar)str[0] == SS2) {
        EXPECT_SIZE(2);
        if (size < 2)         RETURN_INCOMPLETE(1);
#if SCM_STRICT_ENCODING_CHECK
        if (!IN_GR96(str[1])) RETURN_ERROR();
#endif
        RETURN(2);
    } else if ((uchar)str[0] == SS3) {
        EXPECT_SIZE(3);
#if SCM_STRICT_ENCODING_CHECK
        if (size < 2)         RETURN_INCOMPLETE(size);
        if (IS_GR_SPC_OR_DEL(str[1]))
            RETURN(2);
        if (!IN_GR94(str[1])) RETURN_ERROR();
        if (size < 3)         RETURN_INCOMPLETE(size);
        if (!IN_GR94(str[2])) RETURN_ERROR();
        RETURN(3);
#else  /* not SCM_STRICT_ENCODING_CHECK */
        if (size < 3)
            RETURN_INCOMPLETE(size);
        RETURN(3);
#endif /* not SCM_STRICT_ENCODING_CHECK */
    }

    RETURN_ERROR();
}

static int
eucjp_str2int(const uchar *src, size_t len, ScmMultibyteState state)
{
    int ch;

    switch (len) {
    case 1:
        ch = src[0];
        break;

    case 2:
        ch  = src[0] << CHAR_BITS;
        ch |= src[1];
        break;

    case 3:
        ch  = src[0] << CHAR_BITS * 2;
        ch |= src[1] << CHAR_BITS;
        ch |= src[2];
        break;

    default:
        return EOF;
    }

    return ch;
}

/* TODO: migrate to a canonical form shared with ISO-2022 variants that contain
   absolute character set identifier instead of raw encoding-dependent
   shifts */
static uchar *
eucjp_int2str(uchar *dst, int ch, ScmMultibyteState state)
{
#if SCM_STRICT_ENCODING_CHECK
    uchar seq[3];
#endif

    if (IS_1BYTE(ch)) {
        *dst++ = ch;
    } else if (IS_2BYTES(ch)) {
#if SCM_STRICT_ENCODING_CHECK
        seq[0] = ch >> CHAR_BITS;
        seq[1] = ch & BYTE_MASK;
        if ((!IN_GR94(seq[0]) && seq[0] != SS2)
            || !IN_GR96(seq[1]))
            return NULL;
#endif
        *dst++ = ch >> CHAR_BITS;
        *dst++ = ch & BYTE_MASK;
    } else if (IS_3BYTES(ch)) {
#if SCM_STRICT_ENCODING_CHECK
        seq[0] = ch >> CHAR_BITS * 2;
        seq[1] = (ch >> CHAR_BITS) & BYTE_MASK;
        seq[2] = ch & BYTE_MASK;
        if (seq[0] != SS3 || !IN_GR94(seq[1]) || !IN_GR94(seq[2]))
            return NULL;
#endif
        *dst++ = ch >> CHAR_BITS * 2;
        *dst++ = (ch >> CHAR_BITS) & BYTE_MASK;
        *dst++ = ch & BYTE_MASK;
    } else {
        return NULL;
    }
    *dst = '\0';

    return dst;
}
#endif /* SCM_USE_EUCJP */

#if (SCM_USE_EUCCN || SCM_USE_EUCKR || SCM_USE_SJIS)
/* generic double-byte char */
static int
dbc_str2int(const uchar *src, size_t len, ScmMultibyteState state)
{
    int ch;

    switch (len) {
    case 1:
        ch = src[0];
        break;

    case 2:
        ch  = src[0] << CHAR_BITS;
        ch |= src[1];
        break;

    default:
        return EOF;
    }

    return ch;
}
#endif /* (SCM_USE_EUCCN || SCM_USE_EUCKR || SCM_USE_SJIS) */

#if (SCM_USE_EUCCN || SCM_USE_EUCKR)
/* FIXME: Optimize */
int
euc_char_len(int ch)
{
    char buf[SCM_MB_MAX_LEN + sizeof("")];

    return (euc_int2str((uchar *)buf, ch, SCM_MB_STATELESS)) ? strlen(buf) : 0;
}

static uchar *
euc_int2str(uchar *dst, int ch, ScmMultibyteState state)
{
#if SCM_STRICT_ENCODING_CHECK
    uchar seq[2];
#endif

    if (IS_1BYTE(ch)) {
        *dst++ = ch;
    } else if (IS_2BYTES(ch)) {
#if SCM_STRICT_ENCODING_CHECK
        seq[0] = ch >> CHAR_BITS;
        seq[1] = ch & BYTE_MASK;
        if (!IN_GR94(seq[0]) || !IN_GR96(seq[1]))
            return NULL;
#endif
        *dst++ = ch >> CHAR_BITS;
        *dst++ = ch & BYTE_MASK;
    } else {
        return NULL;
    }
    *dst = '\0';

    return dst;
}
#endif /* (SCM_USE_EUCCN || SCM_USE_EUCKR) */

#if SCM_USE_EUCCN
static const char *
euccn_encoding(void)
{
    return "EUC-CN";
}

enum ScmCodedCharSet
euccn_ccs(void)
{
    return SCM_CCS_UNKNOWN;
}

/* FIXME: NOT TESTED!
 *
 * G0 <- ASCII (or GB 1988?)
 * G1 <- GB2312
 *
 * GL <- G0 (ASCII)
 * GR <- G1 (GB2312) */
static ScmMultibyteCharInfo
euccn_scan_char(ScmMultibyteString mbs)
{
    /* TODO: maybe we can make this an alias of eucjp_scan_char()? */
    const char *str = SCM_MBS_GET_STR(mbs);
    const int size  = SCM_MBS_GET_SIZE(mbs);
    ENTER;

    if (!size)
        RETURN(0);
    if (IS_ASCII(str[0]))
        RETURN(1);
    if (IN_GR94(str[0])) {
        EXPECT_SIZE(2);
        if (size < 2)
            RETURN_INCOMPLETE(size);
#if SCM_STRICT_ENCODING_CHECK
        if (!IN_GR96(str[1]))
            RETURN_ERROR();
#endif
        RETURN(2);
    }
    RETURN_ERROR();
}
#endif

#if SCM_USE_EUCKR
enum ScmCodedCharSet
euckr_ccs(void)
{
    return SCM_CCS_UNKNOWN;
}

static const char *
euckr_encoding(void)
{
    return "EUC-KR";
}

/* FIXME: NOT TESTED!  I'm not sure about this encoding.  There's also
 * a Microsoft variant called CP949, which is not supported (yet).
 * RFC 1557 says KS X 1001 is 94x94.
 *
 * G0 <- ASCII
 * G1 <- KS X 1001 (aka KSC 5601)
 *
 * GL <- G0
 * GR <- G1 */
static ScmMultibyteCharInfo
euckr_scan_char(ScmMultibyteString mbs)
{
    const char *str = SCM_MBS_GET_STR(mbs);
    const int size  = SCM_MBS_GET_SIZE(mbs);
    ENTER;

    if (!size)
        RETURN(0);
    if (IS_ASCII(str[0]))
        RETURN(1);
    if (IN_GR94(str[0])) {
        EXPECT_SIZE(2);
        if (size < 2)
            RETURN_INCOMPLETE(size);
#if SCM_STRICT_ENCODING_CHECK
        if (!IN_GR96(str[1]))
            RETURN_ERROR();
#endif
        RETURN(2);
    }
    RETURN_ERROR();
}
#endif /* SCM_USE_EUCKR */

/*==== Encodings for Unicode ====*/
#define IN_OCT_BMP(u)  ((uint)(u) <= 0x7ff)
#define IN_BMP(u)      ((uint)(u) <= 0xffff)
#define IN_SMP(u)      ((uint)(u) <= 0x10ffff && !IN_BMP(u))

#if SCM_USE_UTF8
/* RFC 3629 */
#define MASK(n)        ((LEN_CODE(n) >> 1) | 0x80)
#define LEN_CODE(n)    (((1 << (n))-1) << (8-n))
#define IS_LEN(c, n)   ((MASK(n) & (c)) == LEN_CODE(n))
#define IS_TRAILING(c) (IS_LEN((c), 1))

#define LEN_CODE_BITS(n)    (n + 1)
#define TRAILING_CODE_BITS  LEN_CODE_BITS(1)
#define TRAILING_VAL_BITS   (CHAR_BITS - TRAILING_CODE_BITS)
#define LEADING_VAL_BITS(n) (CHAR_BITS - LEN_CODE_BITS(n))
#define LEADING_VAL(u, n)   ((u) >> TRAILING_VAL_BITS * ((n) - 1))
#define TRAILING_VAL(u, i)  (~MASK(1) & ((u) >> TRAILING_VAL_BITS * (i)))

static const char *
utf8_encoding(void)
{
    return "UTF-8";
}

enum ScmCodedCharSet
utf8_ccs(void)
{
    return SCM_CCS_UCS4;
}

/* FIXME: Optimize */
int
utf8_char_len(int ch)
{
    char buf[SCM_MB_MAX_LEN + sizeof("")];

    return (utf8_int2str((uchar *)buf, ch, SCM_MB_STATELESS)) ? strlen(buf) : 0;
}

static ScmMultibyteCharInfo
utf8_scan_char(ScmMultibyteString mbs)
{
    const char *str = SCM_MBS_GET_STR(mbs);
    const int size  = SCM_MBS_GET_SIZE(mbs);
    int len;
    ENTER;

    if (!size)
        RETURN(0);
    if (IS_ASCII(str[0]))
        RETURN(1);

    if (IS_LEN(str[0], 2))       len = 2;
    else if (IS_LEN(str[0], 3))  len = 3;
    else if (IS_LEN(str[0], 4))  len = 4;
    else                         RETURN_ERROR();

#if SCM_STRICT_ENCODING_CHECK
    {
        int i;
        for (i=1; i < len; i++) {
            if (size <= i)
                RETURN_INCOMPLETE(size);
            if (!IS_TRAILING(str[i]))
                RETURN_ERROR();
        }
    }
#else  /* not SCM_STRICT_ENCODING_CHECK */
    if (size < len)
        RETURN_INCOMPLETE(size);
#endif /* not SCM_STRICT_ENCODING_CHECK */

    RETURN(len);

}

static int
utf8_str2int(const uchar *src, size_t len, ScmMultibyteState state)
{
    int ch;

    switch (len) {
    case 1:
        ch = src[0];
        break;

    case 2:
        ch  = (~MASK(2) & src[0]) << TRAILING_VAL_BITS;
        ch |= (~MASK(1) & src[1]);
        break;

    case 3:
        ch  = (~MASK(3) & src[0]) << TRAILING_VAL_BITS * 2;
        ch |= (~MASK(1) & src[1]) << TRAILING_VAL_BITS;
        ch |= (~MASK(1) & src[2]);
        break;

    case 4:
        ch  = (~MASK(4) & src[0]) << TRAILING_VAL_BITS * 3;
        ch |= (~MASK(1) & src[1]) << TRAILING_VAL_BITS * 2;
        ch |= (~MASK(1) & src[2]) << TRAILING_VAL_BITS;
        ch |= (~MASK(1) & src[3]);
        break;

    default:
        return EOF;
    }

    return ch;
}

static uchar *
utf8_int2str(uchar *dst, int ch, ScmMultibyteState state)
{
    if (IS_ASCII(ch)) {
        *dst++ = ch;
    } else if (IN_OCT_BMP(ch)) {
        *dst++ = LEN_CODE(2) | LEADING_VAL(ch, 2);
        *dst++ = LEN_CODE(1) | TRAILING_VAL(ch, 0);
    } else if (IN_BMP(ch)) {
        *dst++ = LEN_CODE(3) | LEADING_VAL(ch, 3);
        *dst++ = LEN_CODE(1) | TRAILING_VAL(ch, 1);
        *dst++ = LEN_CODE(1) | TRAILING_VAL(ch, 0);
    } else if (IN_SMP(ch)) {
        *dst++ = LEN_CODE(4) | LEADING_VAL(ch, 4);
        *dst++ = LEN_CODE(1) | TRAILING_VAL(ch, 2);
        *dst++ = LEN_CODE(1) | TRAILING_VAL(ch, 1);
        *dst++ = LEN_CODE(1) | TRAILING_VAL(ch, 0);
    } else {
        return NULL;
    }
    *dst = '\0';

    return dst;
}
#undef MASK
#undef LEN_CODE
#undef IS_LEN
#undef IS_TRAILING
#undef LEN_CODE_BITS
#undef TRAILING_CODE_BITS
#undef TRAILING_VAL_BITS
#undef LEADING_VAL_BITS
#undef LEADING_VAL
#undef TRAILING_VAL
#endif /* SCM_USE_UTF8 */

/*==== Other encodings ====*/

#if SCM_USE_SJIS
/* The cwazy Japanese encoding.  This function implements the JIS X
 * 0213 variant.
 *
 * 0 .. 0x7F: ASCII
 * 0x80: undefined
 * 0x81 .. 0x9F: lead byte of 2-byte char
 * 0xA0: undefined
 * 0xA1 .. 0xDF: JIS X 0201 katakana (1 byte)
 * 0xE0 .. 0xEF: lead byte of 2-byte char
 * 0xF0 .. 0xFC: lead byte of 2-byte char if JIS X 0213 is used
 * 0xFD .. 0xFF: undefined
 *
 * 0x40 .. 0x7E: trailing byte of 2-byte char
 * 0x80 .. 0xFC: trailing byte of 2-byte char
 */
#define IS_KANA(c) (0xA1 <= (uchar)(c) && (uchar)(c) <= 0xDF)
#define IS_LEAD(c)        \
    (0x81 <= (uchar)(c)   \
    && !IS_KANA(c)        \
    && (uchar)(c) <= 0xFC \
    && (uchar)(c) != 0xA0)
#define IS_TRAIL(c) (0x40 <= (uchar)(c) && (uchar)(c) <= 0xFC && (c) != 0x7E)

static const char *
sjis_encoding(void)
{
    return "SHIFT_JIS";
}

enum ScmCodedCharSet
sjis_ccs(void)
{
    return SCM_CCS_UNKNOWN;
}

/* FIXME: Optimize */
int
sjis_char_len(int ch)
{
    char buf[SCM_MB_MAX_LEN + sizeof("")];

    return (sjis_int2str((uchar *)buf, ch, SCM_MB_STATELESS)) ? strlen(buf) : 0;
}

static ScmMultibyteCharInfo
sjis_scan_char(ScmMultibyteString mbs)
{
    const char *str = SCM_MBS_GET_STR(mbs);
    const int  size = SCM_MBS_GET_SIZE(mbs);
    ENTER;
    if (!size)
        RETURN(0);
    if (IS_LEAD(str[0])) {
        EXPECT_SIZE(2);
        if (size < 2)
            RETURN_INCOMPLETE(size);
#if SCM_STRICT_ENCODING_CHECK
        if (!IS_TRAIL(str[1]))
            RETURN_ERROR();
#endif
        RETURN(2);
    }
    RETURN(1);
}

static uchar *
sjis_int2str(uchar *dst, int ch, ScmMultibyteState state)
{
    uchar high, low;

#if SCM_STRICT_ENCODING_CHECK
    if (ch >> CHAR_BITS * 2)
        return NULL;
#endif
    high = ch >> CHAR_BITS;
    low  = ch & BYTE_MASK;

    if (IS_LEAD(high)) {
#if SCM_STRICT_ENCODING_CHECK
        if (!IS_TRAIL(high))
            return NULL;
#endif
        *dst++ = high;
    }
    *dst++ = low;
    *dst = '\0';

    return dst;
}
#undef IS_KANA
#undef IS_LEAD
#undef IS_TRAIL
#endif /* SCM_USE_SJIS */

/* Single-byte encodings.  Please add any that you know are missing.
 * Sorted alphabetically.
 *
 * ASCII
 * ISO 646
 * ISO-8859-*
 * VISCII
 */
static const char *
unibyte_encoding(void)
{
    /* conventional assumption */
    return "ISO-8859-1";
}

enum ScmCodedCharSet
unibyte_ccs(void)
{
    /* conventional assumption */
    return SCM_CCS_ISO8859_1;
}

int
unibyte_char_len(int ch)
{
    return (0 < ch && ch <= 0xff) ? 1 : 0;
}

static ScmMultibyteCharInfo
unibyte_scan_char(ScmMultibyteString mbs)
{
    ENTER;
    if (SCM_MBS_GET_SIZE(mbs))
        RETURN(1);
    RETURN(0);
}

static int
unibyte_str2int(const uchar *src, size_t len, ScmMultibyteState state)
{
#if SCM_STRICT_ENCODING_CHECK
    if (len != 1)
        return EOF;
#endif
    return src[0];
}

static uchar *
unibyte_int2str(uchar *dst, int ch, ScmMultibyteState state)
{
#if SCM_STRICT_ENCODING_CHECK
    if (ch & ~BYTE_MASK)
        return NULL;
#endif
    *dst++ = ch;
    return dst;
}
