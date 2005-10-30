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
  File Local Functions
=======================================*/
#if SCM_USE_EUCJP
static ScmMultibyteCharInfo eucjp_scan_char(ScmMultibyteString mbs);
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
static ScmMultibyteCharInfo sjis_scan_char(ScmMultibyteString mbs);
#endif

#if SCM_USE_EUCCN
static ScmMultibyteCharInfo euccn_scan_char(ScmMultibyteString mbs);
#endif

#if SCM_USE_EUCKR
static ScmMultibyteCharInfo euckr_scan_char(ScmMultibyteString mbs);
#endif

#if SCM_USE_UTF8
static ScmMultibyteCharInfo utf8_scan_char(ScmMultibyteString mbs);
#endif

static ScmMultibyteCharInfo unibyte_scan_char(ScmMultibyteString mbs);

typedef unsigned char uchar;

/*=======================================
  Global Variables
=======================================*/
/* TODO: add some mechanism to dynamically switch between encodings. */
ScmMultibyteCharInfo (*Scm_mb_scan_char)(ScmMultibyteString mbs)
#if SCM_USE_UTF8
    = utf8_scan_char;
#else
    = unibyte_scan_char;
#endif

/*=======================================
  Public API
=======================================*/

int Scm_mb_strlen(ScmMultibyteString mbs)
{
    int len = 0;
    ScmMultibyteCharInfo c;

    CDBG((SCM_DBG_ENCODING, "mb_strlen: size = %d; str = %s;",
          SCM_MBS_GET_SIZE(mbs), SCM_MBS_GET_STR(mbs)));

    while (SCM_MBS_GET_SIZE(mbs)) {
        c = Scm_mb_scan_char(mbs);
        CDBG((SCM_DBG_ENCODING, "%d, %d;", SCM_MBCINFO_GET_SIZE(c), c.flag));
        SCM_MBS_SKIP_CHAR(mbs, c);
        len++;
    }

    CDBG((SCM_DBG_ENCODING, "len=%d\n", len));
    return len;
}

/* FIXME: pick a better name. */
int Scm_mb_bare_c_strlen(const char *s)
{
    ScmMultibyteString mbs;
    SCM_MBS_INIT(mbs);
    SCM_MBS_SET_STR(mbs, s);
    SCM_MBS_SET_SIZE(mbs, strlen(s));
    return Scm_mb_strlen(mbs);
}

ScmMultibyteString Scm_mb_substring(ScmMultibyteString mbs, int i, int len)
{
    ScmMultibyteString ret;
    ScmMultibyteString end;
    ScmMultibyteCharInfo c;

    ret = mbs;

    while (i--) {
        c = Scm_mb_scan_char(ret);
        SCM_MBS_SKIP_CHAR(ret, c);
    }

    end = ret;

    while (len--) {
        c = Scm_mb_scan_char(end);
        SCM_MBS_SKIP_CHAR(end, c);
    }

    SCM_MBS_SET_SIZE(ret, SCM_MBS_GET_STR(end) - SCM_MBS_GET_STR(ret));
    return ret;
}


/*=======================================
  Encoding-specific functions
=======================================*/

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

#define IS_ASCII(c) ((uchar)(c) <= 0x7F)
#define IS_GR_SPC_OR_DEL(c)  ((uchar)(c) == 0xA0 || (uchar)(c) == 0xFF)

#define ESC 0x1B
#define SO  0x0E
#define SI  0x0F
#define SS2 0x8E
#define SS3 0x8F


#if SCM_USE_EUCJP
/* G0 <- (96) ASCII (or was it JIS X 0201 Roman?)
 * G1 <- (94x94) JIS X 0208 kanji/kana
 * G2 <- (94) JIS X 0201 Katakana ("half-width katakana")
 * G3 <- (94x94) JIS X 0212 kanji, or JIS X 0213 kanji plane 2
 *
 * GL <- G0 (ASCII)
 * GR <- G1 (JIS X 0208)
 * CL <- JIS X 0211 C0
 * CR <- JIS X 0211 C1 */
static ScmMultibyteCharInfo eucjp_scan_char(ScmMultibyteString mbs)
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
#endif /* SCM_USE_EUCJP */

#if SCM_USE_EUCCN
/* FIXME: NOT TESTED!
 * 
 * G0 <- ASCII (or GB 1988?)
 * G1 <- GB2312
 *
 * GL <- G0 (ASCII)
 * GR <- G1 (GB2312) */
static ScmMultibyteCharInfo euccn_scan_char(ScmMultibyteString mbs)
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
/* FIXME: NOT TESTED!  I'm not sure about this encoding.  There's also
 * a Microsoft variant called CP949, which is not supported (yet).
 * RFC 1557 says KS X 1001 is 94x94.
 *
 * G0 <- ASCII
 * G1 <- KS X 1001 (aka KSC 5601)
 *
 * GL <- G0
 * GR <- G1 */
static ScmMultibyteCharInfo euckr_scan_char(ScmMultibyteString mbs)
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
#if SCM_USE_UTF8
/* RFC 3629 */
#define MASK(n)        ((LEN_CODE(n) >> 1) | 0x80)
#define LEN_CODE(n)    (((1 << (n))-1) << (8-n))
#define IS_LEN(c, n)   ((MASK(n) & (c)) == LEN_CODE(n))
#define IS_TRAILING(c) (IS_LEN((c), 1))

static ScmMultibyteCharInfo utf8_scan_char(ScmMultibyteString mbs)
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

#undef MASK
#undef LEN_CODE
#undef IS_LEN
#undef IS_TRAILING
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
static ScmMultibyteCharInfo sjis_scan_char(ScmMultibyteString mbs)
{
#define IS_KANA(c) (0xA1 <= (uchar)(c) && (uchar)(c) <= 0xDF)
#define IS_LEAD(c)        \
    (0x81 <= (uchar)(c)   \
    && !IS_KANA(c)        \
    && (uchar)(c) <= 0xFC \
    && (uchar)(c) != 0xA0)
#define IS_TRAIL(c) (0x40 <= (uchar)(c) && (uchar)(c) <= 0xFC && (c) != 0x7E)

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

#undef IS_KANA
#undef IS_LEAD
#undef IS_TRAIL
}
#endif /* SCM_USE_SJIS */

/* Single-byte encodings.  Please add any that you know are missing.
 * Sorted alphabetically.
 * 
 * ASCII
 * ISO 646
 * ISO-8859-*
 * VISCII
 */
static ScmMultibyteCharInfo unibyte_scan_char(ScmMultibyteString mbs)
{
    ENTER;
    if (SCM_MBS_GET_SIZE(mbs))
        RETURN(1);
    RETURN(0);
}
