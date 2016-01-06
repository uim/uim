/*

  Copyright (c) 2005-2013 uim Project https://github.com/uim/uim

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

/*
 * Mostly from http://www.openi18n.org/subgroups/sa/locnameguide/final/CodesetAliasTable.html
 */
static const char *alias_us[] = {
  "US-ASCII", "ANSI_X3.4-1968", "ASCII", "CP367", "cp367", "IBM367",
  "ISO-IR-6", "ISO646-US", "ISO-646-US", "ANSI_X3.4-1986", "iso-ir-6",
  "ISO_646.irv:1991", "us", "csASCII", "646",
  NULL
};
static const char *alias_big5[] = {
  "Big5", "TCA-BIG5", "BIG5", "BIG5-CP950", "csBig5", "big5",
  NULL
};
static const char *alias_hkscs[] = {
  "Big5-HKSCS", "HKSCS-BIG5", "BIG5-HKSCS", "BIG5HKSCS", "big5hk",
  "big5-hkscs:unicode 3.0",
  NULL
};
static const char *alias_eucjp[] = {
  "EUC-JIS-2004", "EUC-JISX0213",
  "EUC-JP", "eucJP", "ujis",
  "Extended_UNIX_Code_Packed_Format_for_Japanese", "euc-jp",
  "csEUCPkdFmtJapanese",
  NULL
};
static const char *alias_utf8[] = {
  "UTF-8", "utf-8", "unicode-1-utf-8", "utf8", "UTF8",
  NULL
};
static const char *alias_euckr[] = {
  "EUC-KR", "csEUCKR", "5601", "ksc-5601", "ksc-5601-1987",
  "ksc-5601_1987", "ksc5601",
  NULL
};
static const char *alias_euctw[] = {
  "EUC-TW", "cns11643", "ibm-euctw",
  NULL
};
static const char *alias_gb18030[] = {
  "GB-18030", "GB18030", "ibm1392", "ibm-1392", "gb18030-2000",
  NULL
};
static const char *alias_gb2312[] = {
  "GB2312", "GB-2312", "csGB2312", "EUC_CN", "gb2312-80",
  "gb2312-1980", "euccn", "euc-cn",
  NULL
};
static const char *alias_gbk[] = {
  "GB-K", "GBK",
  NULL
};
static const char *alias_iso88591[] = {
  "ISO-8859-1", "ISO-IR-100", "ISO_8859-1:1987", "ISO_8859-1",
  "LATIN1", "L1", "latin1", "l1", "IBM819", "CP819", "csISOLatin1"
  "819", "iso8859-1", "8859-1", "iso8859_1", "iso_8859_1",
  NULL
};
static const char *alias_iso88592[] = {
  "ISO-8859-2", "ISO-IR-101", "ISO_8859-2:1987", "ISO_8859-2",
  "LATIN2", "L2", "csISOLatin2", "912", "cp912", "ibm-912", "ibm912",
  "iso8859-2", "8859-2", "iso8859_2", "iso_8859_2",
  NULL
};
static const char *alias_iso88593[] = {
  "ISO-8859-3", "ISO-IR-109", "ISO_8859-3:1988", "ISO_8859-3",
  "LATIN3", "L3", "csISOLatin3", "913", "cp913", "ibm-913", "ibm913",
  "iso8859-3", "8859-3", "iso8859_3", "iso_8859_3",
  NULL
};
static const char *alias_iso88594[] = {
  "ISO-8859-4", "ISO-IR-110", "ISO_8859-4:1988", "ISO_8859-4",
  "LATIN4", "L4", "csISOLatin4", "914", "cp914", "ibm-914", "ibm914",
  "iso8859-4", "8859-4", "iso8859_4", "iso_8859_4",
  NULL
};
static const char *alias_iso88595[] = {
  "ISO-8859-5", "ISO-IR-144", "ISO_8859-5:1988", "ISO_8859-5",
  "CYRILLIC", "csISOLatinCyrillic", "915", "cp915", "ibm-915",
  "ibm915", "iso8859-5", "8859-5", "iso8859_5", "iso_8859_5",
  NULL
};
static const char *alias_iso88596[] = {
  "ISO-8859-6", "ISO-IR-127", "ISO_8859-6:1987", "ISO_8859-6",
  "ECMA-114", "ASMO-708", "ARABIC", "csISOLatinArabic", "1089",
  "cp1089", "ibm-1089", "ibm1089", "iso8859-6", "8859-6", "iso8859_6",
  "iso_8859_6",
  NULL
};
static const char *alias_iso88597[] = {
  "ISO-8859-7", "ISO-IR-126", "ISO_8859-7:1987", "ISO_8859-7",
  "ELOT_928", "ECMA-118", "greek", "greek8", "csISOLatinGreek", "813",
  "cp813", "ibm-813", "ibm813", "iso8859-7", "8859-7", "iso8859_7",
  "iso_8859_7",
  NULL
};
static const char *alias_iso88598[] = {
  "ISO-8859-8", "ISO-IR-138", "ISO_8859-8:1988", "ISO_8859-8",
  "hebrew", "csISOLatinHebrew", "916", "cp916", "ibm-916", "ibm916",
  "iso8859-8", "8859-8", "iso8859_8", "iso_8859_8",
  NULL
};
static const char *alias_iso88599[] = {
  "ISO-8859-9", "ISO-IR-148", "ISO_8859-9:1989", "ISO_8859-9",
  "latin5", "l5", "csISOLatin5", "920", "cp920", "ibm-920", "ibm920",
  "iso8859-9", "8859-9", "iso8859_9", "iso_8859_9",
  NULL
};
static const char *alias_iso885913[] = {
  "ISO-8859-13", "ISO-IR-179", "LATIN7", "L7", "iso_8859-13",
  "iso8859-13", "8859-13", "iso8859_13", "iso_8859_13",
  NULL
};
static const char *alias_iso885914[] = {
  "ISO-8859-14", "LATIN8", "L8", "ISO-8859-14", "iso-ir-199",
  "ISO_8859-14:1998", "ISO_8859-14", "iso-celtic",
  NULL
};
static const char *alias_iso885915[] = {
  "ISO-8859-15", "csisolatin9", "csisolatin0", "latin9", "latin0",
  "923", "cp923", "ibm-923", "ibm923", "iso8859-15", "iso_8859-15",
  "8859-15", "iso_8859-15_FDIS", "L9",
  NULL
};
static const char *alias_iso885916[] = {
  "ISO-8859-16", "ISO-IR-226", "LATIN10", "L10",
  NULL
};
static const char *alias_koi8r[] = {
  "KOI8-R", "csKOI8R", "koi8",
  NULL
};
static const char *alias_koi8u[] = {
  "KOI-8-U",
  NULL
};
static const char *alias_koi8t[] = {
  "KOI-8-T",
  NULL
};
static const char *alias_sjis[] = {
  "Shift_JIS", "SHIFT-JIS", "SHIFTJIS", "SJIS", "sjis", "MS_Kanji",
  "csShiftJIS", "pck", "PCK",
  NULL
};
static const char *alias_viscii[] = {
  "VISCII",
  NULL
};
static const char *alias_cp437[] = {
  "CP-437", "IBM437", "CP437", "437", "csPC8CodePage437", "ibm-437",
  NULL
};
static const char *alias_cp850[] = {
  "CP-850", "IBM850", "cp850", "850", "csPC850Multilingual",
  "ibm-850",
  NULL
};
static const char *alias_cp851[] = {
  "CP-851", "IBM851", "cp851", "851", "csIBM851",
  NULL
};
static const char *alias_cp852[] = {
  "CP-852", "IBM852", "cp852", "852", "csPCp852", "ibm-852",
  NULL
};
static const char *alias_cp855[] = {
  "CP-855", "IBM855", "cp855", "855", "csIBM855", "cspcp855",
  "ibm-855",
  NULL
};
static const char *alias_cp857[] = {
  "CP-857", "IBM857", "cp857", "857", "csIBM857", "ibm-857",
  NULL
};
static const char *alias_cp860[] = {
  "CP-860", "IBM860", "cp860", "860", "csIBM860", "ibm-860",
  NULL
};
static const char *alias_cp861[] = {
  "CP-861", "IBM861", "cp861", "861", "cp-is", "csIBM861", "ibm-861",
  NULL
};
static const char *alias_cp862[] = {
  "CP-862", "IBM862", "cp862", "862", "csPC862LatinHebrew", "ibm-862",
  NULL
};
static const char *alias_cp863[] = {
  "CP-863", "IBM863", "cp863", "863", "csIBM863", "ibm-863",
  NULL
};
static const char *alias_cp864[] = {
  "CP-864", "IBM864", "cp864", "csIBM864", "ibm-864",
  NULL
};
static const char *alias_cp865[] = {
  "CP-865", "IBM865", "cp865", "865", "csIBM865", "ibm-865",
  NULL
};
static const char *alias_cp866[] = {
  "CP-866", "IBM866", "cp866", "866", "csIBM866", "ibm-866",
  NULL
};
static const char *alias_cp868[] = {
  "CP-868", "IBM868", "CP868", "cp-ar", "csIBM868", "ibm-868",
  NULL
};
static const char *alias_cp869[] = {
  "CP-869", "IBM869", "cp869", "869", "cp-gr", "csIBM869",
  NULL
};
static const char *alias_cp891[] = {
  "CP-891", "IBM891", "cp891", "csIBM891",
  NULL
};
static const char *alias_cp903[] = {
  "CP-903", "IBM903", "cp903", "csIBM903",
  NULL
};
static const char *alias_cp904[] = {
  "CP-904", "IBM904", "cp904", "904", "csIBM904",
  NULL
};
static const char *alias_cp1251[] = {
  "CP-1251", "CP1251", "MS-CYRL", "windows-1251", "Cp1251",
  NULL
};
static const char *alias_cp1255[] = {
  "CP-1255", "CP1255", "MS-HEBR", "windows-1255",
  NULL
};
static const char *alias_tis620[] = {
  "TIS-620", "TIS620", "TIS620-0", "TIS620.2529-1", "TIS620.2533-0",
  "ISO-IR-166", "TIS620.2533",
  NULL
};
static const char *alias_georgianps[] = {
  "GEORGIAN-PS",
  NULL
};

static const char **uim_encoding_list[] = {
  alias_us,
  alias_big5,
  alias_hkscs,
  alias_eucjp,
  alias_utf8,
  alias_euckr,
  alias_euctw,
  alias_gb18030,
  alias_gb2312,
  alias_gbk,
  alias_iso88591,
  alias_iso88592,
  alias_iso88593,
  alias_iso88594,
  alias_iso88595,
  alias_iso88596,
  alias_iso88597,
  alias_iso88598,
  alias_iso88599,
  alias_iso885913,
  alias_iso885914,
  alias_iso885915,
  alias_iso885916,
  alias_koi8r,
  alias_koi8u,
  alias_koi8t,
  alias_sjis,
  alias_viscii,
  alias_cp437,
  alias_cp850,
  alias_cp851,
  alias_cp852,
  alias_cp855,
  alias_cp857,
  alias_cp860,
  alias_cp861,
  alias_cp862,
  alias_cp863,
  alias_cp864,
  alias_cp865,
  alias_cp866,
  alias_cp868,
  alias_cp869,
  alias_cp891,
  alias_cp903,
  alias_cp904,
  alias_cp1251,
  alias_cp1255,
  alias_tis620,
  alias_georgianps,
  NULL
};
