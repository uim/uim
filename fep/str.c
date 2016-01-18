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

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <stdio.h>
#if (!defined(DEBUG) && !defined(NDEBUG))
#define NDEBUG
#endif
#ifdef HAVE_ASSERT_H
#include <assert.h>
#endif
#ifdef HAVE_LOCALE_H
#include <locale.h>
#endif
#ifdef HAVE_WCHAR_H
#include <wchar.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_STRINGS_H
#include <strings.h>
#endif
#ifdef HAVE_CTYPE_H
#include <ctype.h>
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_LANGINFO_CODESET
#include <langinfo.h>
#endif
#include "uim-fep.h"
#include "str.h"

#define min(a, b) ((a) < (b) ? (a) : (b))

static int s_utf8;

static int str2wcstr(const char *str, wchar_t **wcstr);
static int byte2width(char *str, int n);
static int byte2width2(char *str, int n);

void init_str(void)
{
  const char *enc;

  if (setlocale(LC_CTYPE, "") == NULL) {
    printf("locale not supported\n");
    exit(EXIT_FAILURE);
  }
  
  enc = get_enc();
  s_utf8 = (strcasecmp(enc, "UTF-8") == 0 || strcasecmp(enc, "UTF8") == 0);
}

/*
 * setlocaleで得られるエンコーディングを返す
 * 設定されていない場合は"UTF-8"を返す
 */
const char *get_enc(void)
{
#ifdef __CYGWIN32__
  return "EUC-JP";
#else
#ifdef HAVE_LANGINFO_CODESET
  return nl_langinfo(CODESET);
#else
  char *locale;

  locale = setlocale(LC_CTYPE, NULL);
  assert(locale != NULL);

  if (strcasecmp(locale, "ja") == 0) {
    return "EUC-JP";
  } else {
    char *ptr;
    ptr = strstr(locale, ".");
    return ptr != NULL ? ptr + 1 : "UTF-8";
  }
#endif
#endif
}

static int str2wcstr(const char *str, wchar_t **wcstr)
{
  int str_byte;
  int nr_wchars;

  assert(str != NULL);

  str_byte = strlen(str);

  if (str_byte == 0) {
    *wcstr = NULL;
    return 0;
  }

  *wcstr = uim_malloc(sizeof(wchar_t) * (str_byte + 1));
  nr_wchars = mbstowcs(*wcstr, str, str_byte);
  assert((size_t)nr_wchars != (size_t)-1);
  (*wcstr)[str_byte] = 0;

  return nr_wchars;
}

/*
 * str1とstr2の先頭からの共通部分文字列の幅を返す
 * compare_str("a", "b") = 0
 * compare_str("a", "ab") = 1
 * compare_str("aあ", "aあ") = 3
 * compare_str("い(0xa4a4)", "あ(0xa4a2)") = 0
 */
int compare_str(char *str1, char *str2)
{
  int i;
  int len1;
  int len2;

  assert(str1 != NULL && str2 != NULL);

  len1 = strlen(str1);
  len2 = strlen(str2);

  for (i = 0; i < min(len1, len2); i++) {
    if (str1[i] != str2[i]) {
      break;
    }
  }

  return byte2width(str1, i);
}

/*
 * str1とstr2の末尾からの共通部分文字列の幅を返す
 * compare_str_rev("a", "b") = 0
 * compare_str_rev("a", "ba") = 1
 * compare_str_rev("aあ", "baあ") = 3
 * compare_str_rev("□(0xa2a2)", "あ(0xa4a2)") = 0
 */
int compare_str_rev(char *str1, char *str2)
{
  int i;
  int len1;
  int len2;
  int width1;
  int width2;

  assert(str1 != NULL && str2 != NULL);

  len1 = strlen(str1);
  len2 = strlen(str2);

  for (i = 1; i <= min(len1, len2); i++) {
    if (str1[len1 - i] != str2[len2 - i]) {
      break;
    }
  }

  width1 = strwidth(str1) - byte2width2((char *)str1, len1 - i + 1);
  width2 = strwidth(str2) - byte2width2((char *)str2, len2 - i + 1);
  return (width1 == width2) ? width1 : 0;
}

/*
 * 文字列の幅を返す
 * strwidth("abc") = 3
 * strwidth("あa") = 3
 * strwidth("")    = 0
 */
#if defined(HAVE_WCSWIDTH) && !defined(__CYGWIN32__)
int strwidth(const char *str)
{
  int width;
  wchar_t *wcstr;
  int nr_wchars;

  assert(str != NULL);

  nr_wchars = str2wcstr(str, &wcstr);

  if (nr_wchars == 0) {
    return 0;
  }

  width = wcswidth(wcstr, nr_wchars);
  assert(width != -1);
  free(wcstr);
  return width;
}
#else
int strwidth(const char *str)
{
  int width = 0;

  assert(str != NULL);

  while (*str != '\0') {
    if (isascii((unsigned char)*str)) {
      width++;
      str++;
    } else {
      if (s_utf8) {
        width += 2;
        str += 3;
      } else {
        /* euc-jp */
        if ((unsigned char)*str == 0x8e) {
          /* 半角カタカナ */
          width++;
          str += 2;
        } else if ((unsigned char)*str == 0x8f) {
          /* G3 */
          width += 2;
          str += 3;
        } else {
          width += 2;
          str += 2;
        }
      }
    }
  }
  return width;
}
#endif

/*
 * substr = strのnバイト以下の先頭からの最長部分文字列として、
 * strwidth(substr)を返す。
 * byte2width("abc", 2)  = 2
 * byte2width("ああ", 3) = 2 (euc)
 * byte2width("ああ", 4) = 4 (euc)
 * byte2width("ああ", 6) = 4 (euc)
 * byte2width("ああ", 4) = 2 (utf8)
 * byte2width("ああ", 5) = 2 (utf8)
 * byte2width("ああ", 6) = 4 (utf8)
 */
#if defined(HAVE_WCSWIDTH) && !defined(__CYGWIN32__)
static int byte2width(char *str, int n)
{
  int width;
  int str_byte;
  char save_char;
  char *save_str;
  wchar_t *wcstr;
  int nr_wchars;

  assert(str != NULL);

  if (n <= 0) {
    return 0;
  }

  str_byte = strlen(str);
  if (str_byte == 0) {
    return 0;
  }

  if (n > str_byte) {
    n = str_byte;
  }

  wcstr = uim_malloc(sizeof(wchar_t) * str_byte);

  save_str = str;

  save_char = str[n];
  str[n] = '\0';
  nr_wchars = mbsrtowcs(wcstr, (const char **)&str, str_byte, NULL);
  save_str[n] = save_char;

  if ((size_t)nr_wchars != (size_t)(-1)) {
    width = wcswidth(wcstr, nr_wchars);
  } else {
    save_char = str[0];
    str[0] = '\0';
    width = strwidth(save_str);
    str[0] = save_char;
  }
  free(wcstr);
  assert(width >= 0);
  return width;
}
#else
static int byte2width(char *str, int n)
{
  int width = 0;
  int byte = 0;
  int char_width;
  int char_byte;

  assert(str != NULL);

  if (n <= 0) {
    return 0;
  }

  while (*str != '\0') {
    if (isascii((unsigned char)*str)) {
      char_width = 1;
      char_byte = 1;
    } else {
      if (s_utf8) {
        char_byte = 3;
        char_width = 2;
      } else {
        /* euc-jp */
        if ((unsigned char)*str == 0x8e) {
          /* 半角カタカナ */
          char_width = 1;
          char_byte = 2;
        } else if ((unsigned char)*str == 0x8f) {
          /* G3 */
          char_width = 2;
          char_byte = 3;
        } else {
          char_width = 2;
          char_byte = 2;
        }
      }
    }
    byte += char_byte;
    if (byte == n) {
      width += char_width;
      break;
    } else if (byte > n) {
      break;
    }
    width += char_width;
    str += char_byte;
  }
  return width;
}
#endif

/*
 * substr = strのnバイト以上の先頭からの最短部分文字列として、
 * strwidth(substr)を返す。
 * n > strlen(str)の場合は substr = str
 * byte2width2("abc", 2)  = 2
 * byte2width2("ああ", 3) = 4 (euc)
 * byte2width2("ああ", 4) = 4 (euc)
 * byte2width2("ああ", 6) = 4 (euc)
 * byte2width2("ああ", 4) = 4 (utf8)
 * byte2width2("ああ", 5) = 4 (utf8)
 * byte2width2("ああ", 6) = 4 (utf8)
 */
#if defined(HAVE_WCSWIDTH) && !defined(__CYGWIN32__)
static int byte2width2(char *str, int n)
{
  int width;
  int str_byte;
  char save_char;
  char *save_str;
  wchar_t *wcstr;
  int nr_wchars;
  
  assert(str != NULL);

  if (n <= 0) {
    return 0;
  }

  str_byte = strlen(str);
  if (str_byte == 0) {
    return 0;
  }

  if (n > str_byte) {
    n = str_byte;
  }

  wcstr = uim_malloc(sizeof(wchar_t) * str_byte);

  save_str = str;

  save_char = str[n];
  str[n] = '\0';
  nr_wchars = mbsrtowcs(wcstr, (const char **)&str, str_byte, NULL);
  save_str[n] = save_char;

  if ((size_t)nr_wchars != (size_t)(-1)) {
    width = wcswidth(wcstr, nr_wchars);
  } else {
    mbsrtowcs(wcstr, (const char **)&str, 1, NULL);
    /* strを最後まで変換するとNULLになる */
    assert(str != NULL);
    save_char = str[0];
    str[0] = '\0';
    width = strwidth(save_str);
    str[0] = save_char;
  }
  free(wcstr);
  assert(width >= 0);
  return width;
}
#else
static int byte2width2(char *str, int n)
{
  int width = 0;
  int byte = 0;
  int char_width;
  int char_byte;

  assert(str != NULL);

  if (n <= 0) {
    return 0;
  }

  while (*str != '\0') {
    if (isascii((unsigned char)*str)) {
      char_width = 1;
      char_byte = 1;
    } else {
      if (s_utf8) {
        char_byte = 3;
        char_width = 2;
      } else {
        /* euc-jp */
        if ((unsigned char)*str == 0x8e) {
          /* 半角カタカナ */
          char_width = 1;
          char_byte = 2;
        } else if ((unsigned char)*str == 0x8f) {
          /* G3 */
          char_width = 2;
          char_byte = 3;
        } else {
          char_width = 2;
          char_byte = 2;
        }
      }
    }
    byte += char_byte;
    width += char_width;
    if (byte >= n) {
      break;
    }
    str += char_byte;
  }
  return width;
}
#endif

/*
 * 返り値 rval[2]
 * substr = strの幅n以下の先頭からの最長部分文字列として、
 * rval[0] = substrのバイト
 * rval[1] = substrの幅
 * width2byte("ああ", 3) = [2, 2] (euc)
 * width2byte("ああ", 4) = [4, 4] (euc)
 * width2byte("ああ", 6) = [4, 4] (euc)
 * width2byte("ああ", 3) = [3, 2] (utf8)
 * width2byte("ああ", 4) = [6, 4] (utf8)
 */
#if defined(HAVE_WCSWIDTH) && !defined(__CYGWIN32__)
int *width2byte(const char *str, int n)
{
  int width = 0;
  int str_byte;
  wchar_t *wcstr;
  int nr_wchars;
  static int rval[2];
  int i;

  assert(str != NULL);

  if (n < 0) {
    n = 0;
  }

  str_byte = strlen(str);
  if (str_byte == 0) {
    rval[0] = rval[1] = 0;
    return rval;
  }

  if (n > str_byte) {
    n = str_byte;
  }

  nr_wchars = str2wcstr(str, &wcstr);

  for (i = nr_wchars; i >= 0; i--) {
    width = wcswidth(wcstr, i);
    if (width <= n) {
      wcstr[i] = '\0';
      str_byte = wcstombs(NULL, wcstr, 0);
      break;
    }
  }
  assert((size_t)str_byte != (size_t)-1 && width >= 0);
  rval[0] = str_byte;
  rval[1] = width;
  free(wcstr);
  return rval;
}
#else
int *width2byte(const char *str, int n)
{
  int width = 0;
  int byte = 0;
  int char_width;
  int char_byte;
  static int rval[2];

  assert(str != NULL);

  for (; *str != '\0'; str++) {
    if (isascii((unsigned char)*str)) {
      char_width = 1;
      char_byte = 1;
    } else {
      if (s_utf8) {
        char_byte = 3;
        char_width = 2;
      } else {
        if ((unsigned char)*str == 0x8e) {
          char_width = 1;
          char_byte = 2;
        } else if ((unsigned char)*str == 0x8f) {
          /* G3 */
          char_width = 2;
          char_byte = 3;
        } else {
          char_width = 2;
          char_byte = 2;
        }
      }
    }
    if (width + char_width == n) {
      width += char_width;
      byte += char_byte;
      break;
    } else if (width + char_width > n) {
      break;
    }
    width += char_width;
    str += char_byte - 1;
    byte += char_byte;
  }
  rval[0] = byte;
  rval[1] = width;
  return rval;
}
#endif

/*
 * 返り値 rval[2]
 * substr = strの幅n以上の先頭からの最短部分文字列として、
 * rval[0] = substrのバイト
 * rval[1] = substrの幅
 * n > strwidth(str)の場合は substr = str
 * width2byte2("ああ", 1) = [2, 2] (euc)
 * width2byte2("ああ", 3) = [4, 4] (euc)
 * width2byte2("ああ", 6) = [4, 4] (euc)
 * width2byte2("ああ", 1) = [3, 2] (utf8)
 * width2byte2("ああ", 4) = [6, 4] (utf8)
 */
#if defined(HAVE_WCSWIDTH) && !defined(__CYGWIN32__)
int *width2byte2(const char *str, int n)
{
  int width = 0;
  int str_byte;
  wchar_t *wcstr;
  int nr_wchars;
  static int rval[2];
  int i;

  assert(str != NULL);

  if (n < 0) {
    n = 0;
  }

  str_byte = strlen(str);
  if (str_byte == 0) {
    rval[0] = rval[1] = 0;
    return rval;
  }

  if (n > str_byte) {
    n = str_byte;
  }

  nr_wchars = str2wcstr(str, &wcstr);

  for (i = 0; i <= nr_wchars; i++) {
    width = wcswidth(wcstr, i);
    if (width >= n) {
      wcstr[i] = '\0';
      str_byte = wcstombs(NULL, wcstr, 0);
      break;
    }
  }
  assert((size_t)str_byte != (size_t)-1 && width >= 0);
  rval[0] = str_byte;
  rval[1] = width;
  free(wcstr);
  return rval;
}
#else
int *width2byte2(const char *str, int n)
{
  int width = 0;
  int byte = 0;
  int char_width;
  int char_byte;
  static int rval[2];

  assert(str != NULL);

  for (; *str != '\0'; str++) {
    if (isascii((unsigned char)*str)) {
      char_width = 1;
      char_byte = 1;
    } else {
      if (s_utf8) {
        char_byte = 3;
        char_width = 2;
      } else {
        if ((unsigned char)*str == 0x8e) {
          char_width = 1;
          char_byte = 2;
        } else if ((unsigned char)*str == 0x8f) {
          /* G3 */
          char_width = 2;
          char_byte = 3;
        } else {
          char_width = 2;
          char_byte = 2;
        }
      }
    }
    if (width + char_width >= n) {
      width += char_width;
      byte += char_byte;
      break;
    }
    width += char_width;
    str += char_byte - 1;
    byte += char_byte;
  }
  rval[0] = byte;
  rval[1] = width;
  return rval;
}
#endif

/*
 * substr = strの幅n以下の先頭からの最長部分文字列として、
 * str[strlne(substr)] = '\0'
 * strwidth(substr)を返す。
 * strhead("ああ", 3) = 2 , str = "あ"
 * strhead("ああ", 4) = 4 , str = "ああ"
 * strhead("ああ", 6) = 4 , str = "ああ"
 */
int strhead(char *str, int n)
{
  int *rval = width2byte(str, n);
  assert(0 <= rval[0] && rval[0] <= (int)strlen(str));
  str[rval[0]] = '\0';
  return rval[1];
}

/*
 * haystackの中で最も右に現われるneedleの次の文字列のポインタを返す
 * needleが空文字列の場合はNULLを返す
 * needleがNULLのときはNULLを返す
 */
char *rstrstr_len(const char *haystack, const char *needle, int haystack_len)
{
  const char *str = NULL;
  int needle_len;
  const char *new_haystack = haystack;
  assert(haystack != NULL);
  if (needle == NULL) {
    return NULL;
  }
  needle_len = strlen(needle);
  if (needle_len <= 0) {
    return NULL;
  }
  while ((new_haystack = strstr_len(haystack, needle, haystack_len)) != NULL) {
    new_haystack += needle_len;
    haystack_len -= (new_haystack - haystack);
    str = haystack = new_haystack;
  }
  return (char *)str;
}

/*
 * haystackに'\0'が含まれてもよいstrstr
 * haystackの長さはhaystack_len
 * haystackとneedleはNULLでない
 * needleが""のときはhaystackを返す
 */
char *strstr_len(const char *haystack, const char *needle, int haystack_len)
{
  int needle_len;
  int i, j;

  assert(haystack != NULL && needle != NULL);

  needle_len = strlen(needle);

  for (i = 0; i < haystack_len - needle_len + 1; i++) {
    for (j = 0; j < needle_len; j++) {
      if (haystack[i + j] != needle[j]) {
        break;
      }
    }
    if (j == needle_len) {
      return (char *)haystack + i;
    }
  }
  return NULL;
}

#define TAB_WIDTH 4
/*
 * tabstrのタブをTAB_WIDTH個のスペースに置き換える。
 * 返り値はfreeする。
 */
char *tab2space(const char *tabstr)
{
  char *spacestr;
  int tabstr_len = strlen(tabstr);
  int i, j;
  int tabcount = 0;

  for (i = 0; i < tabstr_len; i++) {
    if (tabstr[i] == '\t') {
      tabcount++;
    }
  }

  spacestr = uim_malloc((tabstr_len - tabcount) + (TAB_WIDTH * tabcount) + 1);

  for (i = 0, j = 0; i < tabstr_len + 1; i++, j++) {
    if (tabstr[i] == '\t') {
      int i2;
      for (i2 = 0; i2 < TAB_WIDTH; i2++, j++) {
        spacestr[j] = ' ';
      }
      j--;
    } else {
      spacestr[j] = tabstr[i];
    }
  }

  return spacestr;
}
