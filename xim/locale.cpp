/*

  Copyright (c) 2003,2004 uim Project http://uim.freedesktop.org/

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

  THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
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
# include "config.h"
#endif

#include <locale.h>
#include <iconv.h>
#include <errno.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "ximserver.h"
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
} locale_map[] = {
    {"af", "af_ZA"},
    {"am", "am_ET"},
    {"ar", "ar_AA:ar_BH:ar_DZ:ar_EG:ar_IQ:ar_JO:ar_KW:ar_LB:ar_LY:ar_MA:ar_OM:ar_QA:ar_SA:ar_SD:ar_SY:ar_TN:ar_YE"},
    // {"as", "as"},
    {"az", "az_AZ"},
    {"be", "be_BY"},
    {"bg", "bg_BG"},
    {"bn", "bn_BD"},
    // {"bo", "bo"},
    {"br", "br_FR"},
    {"ca", "ca_ES"},
    {"cs", "cs_CZ"},
    {"cy", "cy_GB"},
    {"cz", "cz_CZ"},
    {"da", "da_DK"},
    {"de", "de_DE:de_AT:de_BE:de_CH:de_LI:de_LU"},
    {"el", "el_GR"},
    {"en", "en_US:en_AU:en_BE:en_BZ:en_BW:en_CA:en_GB:en_HK:en_IE:en_IN:en_JM:en_NZ:en_PH:en_SG:en_TT:en_UK:en_ZA"},
    {"eo", "eo_XX:eo_EO"},
    {"es", "es_ES:es_AR:es_BO:es_CL:es_CO:es_CR:es_DO:es_EC:es_GT:es_HN:es_MX:es_NI:es_PA:es_PE:es_PR:es_PY:es_SV:es_US:es_UY:es_VE"},
    {"et", "et_EE"},
    {"eu", "eu_ES"},
    {"fa", "fa_IR"},
    {"fi", "fi_FI"},
    {"fo", "fo_FO"},
    {"fr", "fr_FR:fr_BE:fr_CA:fr_CH:fr_LU"},
    {"ga", "ga_IE"},
    {"gd", "gd_GB"},
    {"gl", "gl_ES"},
    // {"gu", "gu"},
    {"gv", "gv_GB"},
    {"he", "he_IL"},
    {"hi", "hi_IN"},
    {"hr", "hr_HR"},
    {"hy", "hy_AM"},
    {"id", "id_ID"},
    {"is", "is_IS"},
    {"it", "it_IT:it_CH"},
    {"ja", "ja_JP"},
    {"ka", "ka_GE"},
    // {"kk", "kk"},
    {"kl", "kl_GL"},
    // {"kn", "kn"},
    {"ko", "ko_KR"},
    {"kw", "kw_GB"},
    {"lo", "lo_LA"},
    {"lt", "lt_LT"},
    {"lv", "lv_LV"},
    {"mi", "mi_NZ"},
    {"mk", "mk_MK"},
    // {"ml", "ml"},
    {"ms", "ms_MY"},
    {"mt", "mt_MT"},
    {"nb", "nb_NO"},
    {"nl", "nl_NL:nl_BE"},
    {"nn", "nn_NO"},
    {"no", "no_NO"},
    {"ny", "ny_NO"},
    {"oc", "oc_FR"},
    // {"or", "or"},
    // {"pa", "pa"},
    {"pd", "pd_DE"},
    {"ph", "ph_PH"},
    {"pl", "pl_PL"},
    {"pp", "pp_AN"},
    {"pt", "pt_PT:pt_BR"},
    {"ro", "ro_RO"},
    {"ru", "ru_RU:ru_UA"},
    {"sh", "sh_YU"},
    {"sk", "sk_SK"},
    {"sl", "sl_SI"},
    {"sp", "sp_YU"},
    {"sq", "sq_AL"},
    {"sr", "sr_YU:sr_SP"},
    {"sv", "sv_SE:sv_FI"},
    {"ta", "ta_IN"},
    {"te", "te_IN"},
    {"tg", "tg_TJ"},
    {"th", "th_TH"},
    {"tl", "tl_PH"},
    {"tr", "tr_TR"},
    {"tt", "tt_RU"},
    {"uk", "uk_UA"},
    {"ur", "ur_PK"},
    {"vi", "vi_VN"},
    {"wa", "wa_BE"},
    {"yi", "yi_US"},
    {"zh", "zh_CN:zh_TW:zh_HK"},
    {"zh_CN", "zh_CN"},	// from uim-py and uim-pyunihan
    {"zh_TW:zh_HK", "zh_TW:zh_HK"},	// from uim-pinyin-big5
    {NULL, NULL}
};


static char *
ustring_to_utf8_str(uString *s)
{
    uString::iterator i;
    int l = 0, nbyte;
    unsigned char utf8[6];
    // count the length
    for (i = s->begin(); i != s->end(); i++) {
	nbyte = utf8_wctomb(utf8, *i);
	l += nbyte;
    }
    char *c = (char *)malloc(l + 1);
    c[l] = 0;
    l = 0;
    for (i = s->begin(); i != s->end(); i++) {
	nbyte = utf8_wctomb(utf8, *i);
	int j;
	for (j = 0; j < nbyte; j++) {
	    c[l] = utf8[j];
	    l++;
	}
    }
    return c;
}

bool
Locale::supportOverTheSpot()
{
    return false;
}

char *utf8_to_native_str(char *utf8, const char *enc) {
    iconv_t cd;
    size_t outbufsize = BUFSIZ;
    char *inbuf, *outbuf, *convstr = NULL;
    char *inchar;
    char *outchar;
    size_t inbytesleft, outbytesleft;
    size_t ret_val;
    
    cd = iconv_open(enc, "UTF-8");
    if (cd == (iconv_t)-1) {
	perror("error in iconv_open");
	return NULL;
    }

    inbuf = strdup(utf8);
    if (!inbuf) {
	iconv_close(cd);
	return NULL;
    }
    outbuf = (char *)malloc(outbufsize);
    if (!outbuf) {
	iconv_close(cd);
	free(inbuf);
	return NULL;
    }
    inchar = inbuf;
    outchar = outbuf;
    inbytesleft = strlen(inbuf);
    outbytesleft = outbufsize;
    ret_val = iconv(cd, (ICONV_CONST char **)&inchar, &inbytesleft, &outchar, &outbytesleft);

    if (ret_val == (size_t)-1 && errno != E2BIG) {
	//perror("error in iconv");
	iconv_close(cd);
	free(inbuf);
	free(outbuf);
	return NULL;
    }
    iconv_close(cd);
    convstr = (char *)malloc(outbufsize - outbytesleft + 1);
    if (!convstr) {
	free(inbuf);
	free(outbuf);
	return NULL;
    }
    strncpy(convstr, outbuf, outbufsize - outbytesleft);
    convstr[outbufsize - outbytesleft] = '\0';
    free(outbuf);
    free(inbuf);
    return convstr;
}

class UTF8_Locale : public Locale {
public:
    UTF8_Locale::UTF8_Locale(const char *lang);
    virtual char *uStringToCtext(uString *us, const char *encoding) {
	char *str = ustring_to_utf8_str(us);
	XTextProperty prop;

	if (!strcmp(encoding, "UTF-8")) {
	    XmbTextListToTextProperty(XimServer::gDpy, &str, 1,
			    XCompoundTextStyle, &prop);
	    free(str);
	} else {
	    char *native_str;
	    
	    native_str = utf8_to_native_str(str, encoding);
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
    virtual void set_localename_from_im_lang(const char *im_lang);
private:
    char *mLocaleName;
};

UTF8_Locale::UTF8_Locale(const char *im_lang)
{
    mLocaleName = strdup(compose_localenames_from_im_lang(im_lang));
}

static const char *
get_valid_locales(const char *locales)
{
    char *valid_locales = NULL;
    char *validated;
    char *locale;
    int len = 0;

    char *tmp = strdup(locales);

    char *orig_locale = strdup(setlocale(LC_CTYPE, NULL));

    // locales is separated with ':'
    while ((locale = strsep(&tmp, ":")) != NULL) {
	if (setlocale(LC_CTYPE, locale) != NULL) {
	    asprintf(&validated, "%s:", locale);
	    len += strlen(validated);
	    if (valid_locales) {
		valid_locales = (char *)realloc(valid_locales, len + 1);
	        strcat(valid_locales, validated);
	    } else
		valid_locales = strdup(validated);

	    free(validated);
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
	// exclude uim specific languages.
	if (!strcmp(locale_map[i].lang, "zh_CN") ||
	    !strcmp(locale_map[i].lang, "zh_TW:zh_HK"))
	    continue;

	valid_locales = get_valid_locales(locale_map[i].localename);
	if (!strcmp(valid_locales, "")) {
	    // There is no valid locale.
	    free((char *)valid_locales);
	    continue;
	}

	asprintf(&tmp, "%s:", valid_locales);
	free((char *)valid_locales);

	if (locales == NULL) {
	    len = strlen(tmp);
	    locales = strdup(tmp);
	} else {
	    len += strlen(tmp);
	    locales = (char *)realloc(locales, len + 1);
	    strcat(locales, tmp);
	}
	free(tmp);
    }
    // remove trailing ":"
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

void
UTF8_Locale::set_localename_from_im_lang(const char *im_lang)
{
    const char *name;
    name = compose_localenames_from_im_lang(im_lang);
    
    if (mLocaleName)
	free(mLocaleName);
    mLocaleName = strdup(name);
}

Locale *createLocale(const char *im_lang)
{
    return new UTF8_Locale(im_lang);
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
	dest[5] = 0x80 | (wc & 0x3f);
	wc = wc >> 6; wc |= 0x4000000;
    case 5:
	dest[4] = 0x80 | (wc & 0x3f);
	wc = wc >> 6; wc |= 0x200000;
    case 4:
	dest[3] = 0x80 | (wc & 0x3f);
	wc = wc >> 6; wc |= 0x10000;
    case 3:
	dest[2] = 0x80 | (wc & 0x3f);
	wc = wc >> 6; wc |= 0x800;
    case 2:
	dest[1] = 0x80 | (wc & 0x3f);
	wc = wc >> 6; wc |= 0xc0;
    case 1:
	dest[0] = wc;
    }
    return count;
}
