/*

  Copyright (c) 2003-2013 uim Project https://github.com/uim/uim

  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions
  are met:

  1. Redistributions of source code must retain the above copyright
     notice, this list of conditions and the following disclaimer.
  2. Redistributions in binary form must reproduce the above copyright
     notice, this list of conditions and the following disclaimer in the
     documentation and/or other materials provided with the distribution.
  3. Neither the name of authors nor the names of its contributors
     may be used to endorse or promote products derived from this software
     without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS IS'' AND
  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE
  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
  OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
  SUCH DAMAGE.
*/

// Locale dependent routines

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <cerrno>
#include <clocale>
#include <cstdio>
#include <cstring>
#include <iconv.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "ximserver.h"
#include "util.h"
#include "uim/uim-util.h"
#ifndef __GNUC__
# ifdef HAVE_ALLOCA_H
#  include <alloca.h>
# endif
#endif

// Return code if invalid. (utf8_mbtowc, utf8_wctomb)
#define RET_ILSEQ	0
// Return code if only a shift sequence of n bytes was read. (utf8_mbtowc)
#define RET_TOOFEW(n)	(-1-(n))

// Cache of all available locales in working system
static char *all_locale_names;

// This table is composed from language of m17n-libs,
// locale.dir in /usr/X11R6/lib/locale, and im's language of uim.
static struct {
    const char *lang;
    const char *localename;
    const char *supplemental_encoding;
} locale_map[] = {
    {"af", "af_ZA", "ISO8859-1:UTF-8"},
    {"am", "am_ET", "UTF-8"},
    {"ar", "ar_AA:ar_BH:ar_DZ:ar_EG:ar_IQ:ar_JO:ar_KW:ar_LB:ar_LY:ar_MA:ar_OM:ar_QA:ar_SA:ar_SD:ar_SY:ar_TN:ar_YE", "ISO8859-6:UTF-8"},
    // {"as", "as", NULL},
    {"az", "az_AZ", "ISO8859-9E:UTF-8"},
    {"be", "be_BY", "CP1251:UTF-8"},
    {"bg", "bg_BG", "ISO8859-5:CP1251:KOI8-R:UTF-8"},
    {"bn", "bn_BD:bn_IN", "UTF-8"},
    // {"bo", "bo", NULL},
    {"br", "br_FR:br_FR@euro", "ISO8859-1:ISO8859-14:ISO8859-15:UTF-8"},
    {"ca", "ca_ES:ca_ES@euro", "ISO8859-1:ISO8859-15:UTF-8"},
    {"cs", "cs_CZ", "ISO8859-2:UTF-8"},
    {"cy", "cy_GB", "ISO8859-1:ISO8859-14:ISO8859-15:UTF-8"},
    {"cz", "cz_CZ", "ISO8859-2"},
    {"da", "da_DK", "ISO8859-1:ISO8859-15:UTF-8"},
    {"de", "de_DE:de_DE@euro:de_AT:de_AT@euro:de_BE:de_BE@euro:de_CH:de_LI:de_LU:de_LU@euro", "ISO8859-1:ISO8859-15:UTF-8"},
    {"el", "el_GR:el_GR@euro", "ISO8859-7:ISO8859-15:UTF-8"},
    {"en", "en_US:en_AU:en_BE:en_BZ:en_BW:en_CA:en_GB:en_HK:en_IE:en_IE@euro:en_IN:en_JM:en_NZ:en_PH:en_SG:en_TT:en_UK:en_ZA", "ISO8859-1:ISO8859-15:UTF-8"},
    {"eo", "eo_XX:eo_EO", "ISO8859-3"},
    {"es", "es_ES:es_ES@euro:es_AR:es_BO:es_CL:es_CO:es_CR:es_DO:es_EC:es_GT:es_HN:es_MX:es_NI:es_PA:es_PE:es_PR:es_PY:es_SV:es_US:es_UY:es_VE", "ISO8859-1:ISO8859-15:UTF-8"},
    {"et", "et_EE", "ISO8859-15:ISO8859-1:ISO8859-4:UTF-8"},
    {"eu", "eu_ES:eu_ES@euro", "ISO8859-1:ISO8859-15:UTF-8"},
    {"fa", "fa_IR", "UTF-8:ISIRI-3342"},
    {"fi", "fi_FI:fi_FI@euro", "ISO8859-15:ISO8859-1:UTF-8"},
    {"fo", "fo_FO", "ISO8859-1:ISO8859-15:UTF-8"},
    {"fr", "fr_FR:fr_FR@euro:fr_BE:fr_BE@euro:fr_CA:fr_CH:fr_LU:fr_LU@euro", "ISO8859-1:ISO8859-15:UTF-8"},
    {"ga", "ga_IE:ga_IE@euro", "ISO8859-1:ISO8859-14:ISO8859-15:UTF-8"},
    {"gd", "gd_GB", "ISO8859-1:ISO8859-14:ISO8859-15:UTF-8"},
    {"gl", "gl_ES:gl_ES@euro", "ISO8859-1:ISO8859-15:UTF-8"},
    // {"gu", "gu", NULL},
    {"gv", "gv_GB", "ISO8859-1:ISO8859-14:ISO8859-15:UTF-8"},
    {"he", "he_IL", "ISO8859-8:CP1255:UTF-8"},
    {"hi", "hi_IN", "ISCII-DEV:UTF-8"},
    {"hr", "hr_HR", "ISO8859-2:UTF-8"},
    {"hu", "hu_HU", "ISO8859-2:UTF-8"},
    {"hy", "hy_AM", NULL},
    {"id", "id_ID", NULL},
    {"is", "is_IS", "ISO8859-1:ISO8859-15:UTF-8"},
    {"it", "it_IT:it_IT@euro:it_CH", "ISO8859-1:ISO8859-15:UTF-8"},
    {"ja", "ja_JP", "eucJP:EUC:SJIS:UTF-8"},
    {"ka", "ka_GE", "GEORGIAN-ACADEMY:GEORGIAN-PS:UTF-8"},
    // {"kk", "kk", NULL},
    {"kl", "kl_GL", "ISO8859-1:ISO8859-15:UTF-8"},
    // {"kn", "kn", NULL},
    {"ko", "ko_KR", "eucKR:EUC:UTF-8"},
    {"kw", "kw_GB", "ISO8859-1:ISO8859-14:ISO8859-15:UTF-8"},
    {"lo", "lo_LA", "MULELAO-1:IBM-CP1133:UTF-8"},
    {"lt", "lt_LT", "ISO8859-13:ISO8859-4:UTF-8"},
    {"lv", "lv_LV", "ISO8859-13:UTF-8"},
    {"mi", "mi_NZ", "ISO8859-1:ISO8859-5:ISO8859-13:UTF-8"},
    {"mk", "mk_MK", "ISO8859-5:CP1251:UTF-8"},
    // {"ml", "ml", NULL},
    {"ms", "ms_MY", "ISO8859-1:UTF-8"},
    {"mt", "mt_MT", "ISO8859-3:UTF-8"},
    {"nb", "nb_NO", "ISO8859-1:ISO8859-15:UTF-8"},
    {"nl", "nl_NL:nl_NL@euro:nl_BE:nl_BE@euro", "ISO8859-1:ISO8859-15:UTF-8"},
    {"nn", "nn_NO", "ISO8859-1:ISO8859-15:UTF-8"},
    {"no", "no_NO", "ISO8859-1:ISO8859-15:UTF-8"},
    {"ny", "ny_NO", "ISO8859-1:ISO8859-15"},
    {"oc", "oc_FR", "ISO8859-1:ISO8859-15:UTF-8"},
    // {"or", "or", NULL},
    // {"pa", "pa", NULL},
    {"pd", "pd_DE", "ISO8859-1:ISO8859-15"},
    {"ph", "ph_PH", "ISO8859-1"},
    {"pl", "pl_PL", "ISO8859-2:UTF-8"},
    {"pp", "pp_AN", "ISO8859-1"},
    {"pt", "pt_PT:pt_PT@euro:pt_BR", "ISO8859-1:ISO8859-15:UTF-8"},
    {"ro", "ro_RO", "ISO8859-2:UTF-8"},
    {"ru", "ru_RU:ru_UA", "KOI8-R:ISO8859-5:CP1251:KOI8-U:UTF-8"},
    {"sh", "sh_YU", "ISO8859-2:UTF-8"},
    {"sk", "sk_SK", "ISO8859-2:UTF-8"},
    {"sl", "sl_SI", "ISO8859-2:UTF-8"},
    {"sp", "sp_YU", "ISO8859-5"},
    {"sq", "sq_AL", "ISO8859-2:UTF-8"},
    {"sr", "sr_YU:sr_YU@cyrillic:sr_SP", "ISO8859-2:ISO8859-5:CP1251:UTF-8"},
    {"sv", "sv_SE:sv_SE@euro:sv_FI:sv_FI@euro", "ISO8859-1:ISO8859-15:UTF-8"},
    {"ta", "ta_IN", "TSCII-0:UTF-8"},
    {"te", "te_IN", "UTF-8"},
    {"tg", "tg_TJ", "KOI8-C:KOI8-T:UTF-8"},
    {"th", "th_TH", "ISO8859-11:TIS620:UTF-8"},
    {"tl", "tl_PH", "ISO8859-1:UTF-8"},
    {"tr", "tr_TR", "ISO8859-9:UTF-8"},
    {"tt", "tt_RU", "TATAR-CYR:KOI8-C:UTF-8"},
    {"uk", "uk_UA", "KOI8-U:ISO8859-5:CP1251:UTF-8"},
    {"ur", "ur_PK", "CP1256:UTF-8"},
    {"vi", "vi_VN", "TCVN:VISCII:UTF-8"},
    {"wa", "wa_BE:wa_BE@euro", "ISO8859-1:ISO8859-15:UTF-8"},
    {"yi", "yi_US", "CP1255:UTF-8"},
    {"zh_CN", "zh_CN", "gb2312:eucCN:gbk:UTF-8"},	// from uim-py and uim-pyunihan
    {"zh_TW:zh_HK", "zh_TW:zh_HK", "big5:eucTW:big5hkscs:UTF-8"},	// from uim-pinyin-big5
    {"zh", "zh_CN:zh_TW:zh_HK", NULL},	// this entry must be here since its encoding is assigned as NULL
    {NULL, NULL, NULL}
};


static char *
ustring_to_utf8_str(uString *s)
{
    uString::iterator i;
    int l = 0, nbyte;
    unsigned char utf8[6];
    // count the length
    for (i = s->begin(); i != s->end(); ++i) {
	nbyte = utf8_wctomb(utf8, *i);
	l += nbyte;
    }
    char *c = (char *)malloc(l + 1);
    c[l] = 0;
    l = 0;
    for (i = s->begin(); i != s->end(); ++i) {
	nbyte = utf8_wctomb(utf8, *i);
	int j;
	for (j = 0; j < nbyte; j++) {
	    c[l] = utf8[j];
	    l++;
	}
    }
    return c;
}

Locale::~Locale()
{
}

bool
Locale::supportOverTheSpot()
{
    return false;
}

class UTF8_Locale : public Locale {
public:
    UTF8_Locale(const char *encoding);
    virtual ~UTF8_Locale();
    virtual char *utf8_to_native_str(char *str);
    virtual char *uStringToCtext(uString *us) {
	char *str = ustring_to_utf8_str(us);
	XTextProperty prop;

	if (!strcmp(mEncoding, "UTF-8")) {
	    XmbTextListToTextProperty(XimServer::gDpy, &str, 1,
			    XCompoundTextStyle, &prop);
	    free(str);
	} else {
	    char *native_str;
	    
	    native_str = utf8_to_native_str(str);
	    free(str);
	    if (!native_str)
		return NULL;

	    XmbTextListToTextProperty(XimServer::gDpy, &native_str, 1,
			    XCompoundTextStyle, &prop);
	    free(native_str);
	}
	char *res = strdup((char *)prop.value);
	XFree(prop.value);
	return res;
    }
    virtual bool supportOverTheSpot() {
	return true;
    }
private:
    char *mEncoding;
    iconv_t m_iconv_cd;
};

UTF8_Locale::UTF8_Locale(const char *encoding)
{
    mEncoding = strdup(encoding);
    if (uim_iconv->is_convertible(encoding, "UTF-8"))
	m_iconv_cd = (iconv_t)uim_iconv->create(encoding, "UTF-8");
    else
	m_iconv_cd = (iconv_t)-1;
}

UTF8_Locale::~UTF8_Locale()
{
    free(mEncoding);
    if (m_iconv_cd != (iconv_t)-1 && m_iconv_cd)
        uim_iconv->release(m_iconv_cd);
}

char *UTF8_Locale::utf8_to_native_str(char *utf8)
{
    char *str;

    if (m_iconv_cd == (iconv_t)-1)
	return NULL;

    str = uim_iconv->convert(m_iconv_cd, utf8);

    if (strlen(str) == 0) {
	    free(str);
	    return NULL;
    }
    return str;
}


static const char *
get_valid_locales(const char *locales)
{
    char *valid_locales = NULL;
    char *validated;
    char *locale;
    char *tmp, *tmpp;
    int len = 0;

    tmp = tmpp = strdup(locales);
    char *orig_locale = strdup(setlocale(LC_CTYPE, NULL));

    // locales is separated with ':'
    while ((locale = strsep(&tmpp, ":")) != NULL) {
	if (setlocale(LC_CTYPE, locale) != NULL) {
	    if (asprintf(&validated, "%s:", locale) == -1) {
                free(validated);
                continue;
            }
	    len += static_cast<int>(strlen(validated));
	    if (valid_locales) {
		valid_locales = (char *)realloc(valid_locales, len + 1);
		strcat(valid_locales, validated);
	    } else
		valid_locales = strdup(validated);

	    free(validated);
	} else {
	    // retry with supplemental encodings
	    int i;
	    for (i = 0; locale_map[i].localename; i++) {
		if (is_locale_included(locale_map[i].localename, locale))
		    break;
	    }
	    if (locale_map[i].supplemental_encoding) {
		char *encs, *encsp, *encoding;
		encs = encsp = strdup(locale_map[i].supplemental_encoding);

		while ((encoding = strsep(&encsp, ":")) != NULL) {
		    char *test_locale = strdup(locale);
		    test_locale = (char *)realloc(test_locale, strlen(test_locale) + strlen(encoding) + 2);
		    strcat(test_locale, ".");
		    strcat(test_locale, encoding);

		    if (setlocale(LC_CTYPE, test_locale) != NULL) {
			if (asprintf(&validated, "%s:", locale) == -1) {
                            free(validated);
                            continue;
                        }
			len += static_cast<int>(strlen(validated));

			if (valid_locales) {
			    valid_locales = (char *)realloc(valid_locales, len + 1);
			    strcat(valid_locales, validated);
			} else
			    valid_locales = strdup(validated);

			free(validated);
			free(test_locale);
			break;
		    } else
			free(test_locale);
		}
		free(encs);
	    }
	}
    }
    if (valid_locales)
	valid_locales[len - 1] = '\0'; // remove trailing ':'
    else
	valid_locales = strdup(""); // There is no valid locale or im-lang is
    				    // "".  These im will be used with
				    // en_US.UTF-8.

    setlocale(LC_CTYPE, orig_locale);
    free(orig_locale);
    free(tmp);

    return valid_locales;
}

static const char *
all_locales(void)
{
    int i, len = 0;
    char *locales = NULL, *tmp;
    const char *valid_locales;

    // check cache
    if (all_locale_names)
	return all_locale_names;

    for (i = 0; locale_map[i].lang; i++) {
	// exclude languages of which uim has its own version.
	if (!strcmp(locale_map[i].lang, "zh"))
	    continue;

	valid_locales = get_valid_locales(locale_map[i].localename);
	if (!strcmp(valid_locales, "")) {
	    // There is no valid locale.
	    free((char *)valid_locales);
	    continue;
	}

	if (asprintf(&tmp, "%s:", valid_locales) == -1) {
	    free((char *)valid_locales);
            free(tmp);
            continue;
        }
	free((char *)valid_locales);

	if (locales == NULL) {
	    len = static_cast<int>(strlen(tmp));
	    locales = strdup(tmp);
	} else {
	    len += static_cast<int>(strlen(tmp));
	    locales = (char *)realloc(locales, len + 1);
	    strcat(locales, tmp);
	}
	free(tmp);
    }
    // remove trailing ":"
    if (locales)
	locales[len - 1] = '\0';

    // assign result into the cache
    all_locale_names = locales;

    return locales;
}

const char *
compose_localenames_from_im_lang(const char *im_lang)
{
    int i;
    const char *name = NULL;

    for (i = 0; locale_map[i].lang; i++) {
	if (!strcmp(im_lang, locale_map[i].lang)) {
	    name = locale_map[i].localename;
	    break;
	}
    }

    if (name == NULL) {
	// No lang in locale_map.
	if (!strcmp(im_lang, "*")) // im with lang "*" will be enabled for
				   // all locales
	    name = all_locales();
	else if (!strcmp(im_lang, ""))
	    name = "";	// im with lang "" will be only enabled in UTF-8
			// clients
	else
	    name = "en_US";	// shouldn't happen
    }

    return name;
}

bool
is_locale_included(const char *locales, const char *locale)
{
    char *sep, *tmp, *first;
    tmp = strdup(locales);
    first = tmp;

    while ((sep = strchr(tmp, ':')) != NULL) {
	*sep = '\0';
	if (!strcmp(tmp, locale)) {
	    free(first);
	    return true;
	}
	tmp = sep + 1;
    }
    if (!strcmp(tmp, locale)) {
	free(first);
	return true;
    }
    free(first);

    return false;
}

char *
get_prefered_locale(const char *locales)
{
    char *valid_locales;
    char *locale;
    char *sep;

    valid_locales = (char *)get_valid_locales(locales);
    if (!strcmp(valid_locales, "")) {
	// use en_US for im with lang "" and im without valid locale
	free(valid_locales);
	locale = strdup("en_US");
    } else {
	locale = valid_locales;
	sep = strchr(locale, ':');
	if (sep)
	    *sep = '\0';
    }

    return locale;
}

Locale *createLocale(const char *encoding)
{
    return new UTF8_Locale(encoding);
}

int
utf8_mbtowc(uchar *wc, const unsigned char *src, int src_len)
{
    if (!wc)
	return 0;

    unsigned char c = src[0];
    if (c < 0x80) {
	*wc = c;
	return 1;
    } else if (c < 0xc2) {
	return RET_ILSEQ;
    } else if (c < 0xe0) {
	if (src_len < 2)
	    return RET_TOOFEW(0);
	if (!((src[1] ^ 0x80) < 0x40))
	    return RET_ILSEQ;
	*wc = ((uchar)(c & 0x1f) << 6) | (uchar)(src[1] ^ 0x80);
	return 2;
    } else if (c < 0xf0) {
	if (src_len < 3)
	    return RET_TOOFEW(0);
	if (!((src[1] ^ 0x80) < 0x40 &&
	      (src[2] ^ 0x80) < 0x40 &&
	      (c >= 0xe1 || src[1] >= 0xa0)))
	    return RET_ILSEQ;
	*wc = ((uchar)(c & 0x0f) << 12) |
	      ((uchar)(src[1] ^ 0x80) << 6) |
	      (uchar)(src[2] ^ 0x80);
	return 3;
    } else if (c < 0xf8) {
	if (src_len < 4)
	    return RET_TOOFEW(0);
	if (!((src[1] ^ 0x80) < 0x40 &&
	      (src[2] ^ 0x80) < 0x40 &&
	      (src[3] ^ 0x80) < 0x40 &&
	      (c >= 0xf1 || src[1] >= 0x90)))
	    return RET_ILSEQ;
	*wc = ((uchar)(c & 0x07) << 18) |
	      ((uchar)(src[1] ^ 0x80) << 12) |
	      ((uchar)(src[2] ^ 0x80) << 6) |
	      (uchar)(src[3] ^ 0x80);
	return 4;
    } else if (c < 0xfc) {
	if (src_len < 5)
	    return RET_TOOFEW(0);
	if (!((src[1] ^ 0x80) < 0x40 &&
	      (src[2] ^ 0x80) < 0x40 &&
	      (src[3] ^ 0x80) < 0x40 &&
	      (src[4] ^ 0x80) < 0x40 &&
	      (c >= 0xf9 || src[1] >= 0x88)))
	    return RET_ILSEQ;
	*wc = ((uchar)(c & 0x03) << 24) |
	      ((uchar)(src[1] ^ 0x80) << 18) |
	      ((uchar)(src[2] ^ 0x80) << 12) |
	      ((uchar)(src[3] ^ 0x80) << 6) |
	      (uchar)(src[4] ^ 0x80);
	return 5;
    } else if (c < 0xfe) {
	if (src_len < 6)
	    return RET_TOOFEW(0);
	if (!((src[1] ^ 0x80) < 0x40 &&
	      (src[2] ^ 0x80) < 0x40 &&
	      (src[3] ^ 0x80) < 0x40 &&
	      (src[4] ^ 0x80) < 0x40 &&
	      (src[5] ^ 0x80) < 0x40 &&
	      (c >= 0xfd || src[1] >= 0x84)))
	    return RET_ILSEQ;
	*wc = ((uchar)(c & 0x01) << 30) |
	      ((uchar)(src[1] ^ 0x80) << 24) |
	      ((uchar)(src[2] ^ 0x80) << 18) |
	      ((uchar)(src[3] ^ 0x80) << 12) |
	      ((uchar)(src[4] ^ 0x80) << 6) |
	      (uchar)(src[5] ^ 0x80);
	return 6;
    } else
	return RET_ILSEQ;
}

int
utf8_wctomb(unsigned char *dest, uchar wc)
{
    if (!dest)
	return 0;

    int count;
    if (wc < 0x80)
	count = 1;
    else if (wc < 0x800)
	count = 2;
    else if (wc < 0x10000)
	count = 3;
    else if (wc < 0x200000)
	count = 4;
    else if (wc < 0x4000000)
	count = 5;
    else if (wc <= 0x7fffffff)
	count = 6;
    else
	return RET_ILSEQ;
    switch (count) { // note: falls through cases (no break)
    case 6:
	dest[5] = (unsigned char)(0x80 | (wc & 0x3f));
	wc = wc >> 6; wc |= 0x4000000;
    case 5:
	dest[4] = (unsigned char)(0x80 | (wc & 0x3f));
	wc = wc >> 6; wc |= 0x200000;
    case 4:
	dest[3] = (unsigned char)(0x80 | (wc & 0x3f));
	wc = wc >> 6; wc |= 0x10000;
    case 3:
	dest[2] = (unsigned char)(0x80 | (wc & 0x3f));
	wc = wc >> 6; wc |= 0x800;
    case 2:
	dest[1] = (unsigned char)(0x80 | (wc & 0x3f));
	wc = wc >> 6; wc |= 0xc0;
    case 1:
	dest[0] = (unsigned char)wc;
    }
    return count;
}
