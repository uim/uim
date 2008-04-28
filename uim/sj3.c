/*
  Copyright (c) 2008 uim Project http://uim.freedesktop.org/

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

#include <config.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/param.h>

#include <sj3lib.h>

#include "uim.h"
#include "uim-scm.h"
#include "uim-scm-abbrev.h"
#include "uim-helper.h"
#include "uim-notify.h"
#include "gettext.h"
#include "dynlib.h"

#ifndef MAXHOSTNAMELEN
#define MAXHOSTNAMELEN 256
#endif
#ifndef MAXLOGNAME
#define MAXLOGNAME 32
#endif

static int uim_sj3_is_open = 0;

struct uim_sj3_error {
  int errno;
  char *error_sym;
  char *error_str;
};

static uim_lisp
uim_sj3_make_error_pair(char *sym, char *str)
{
  char error_str[BUFSIZ];

  snprintf(error_str, sizeof(error_str), "%s%s", _("In uim-sj3, "), _(str));
  uim_notify_fatal(error_str);
  return MAKE_SYM(sym);
}

static uim_lisp
uim_sj3_make_single_error(char *sym, char *str)
{
  return CONS(MAKE_SYM("error"), LIST1(uim_sj3_make_error_pair(sym, str)));
}

#define uim_sj3_server_down_error() uim_sj3_make_single_error("*SJ3-SERVER-DOWN-ERROR*" , N_("Serverdown."))
#define uim_sj3_undefined_error()   uim_sj3_make_single_error("*SJ3-UNDEFINED-ERROR*"   , N_("Undefined error."))
#define uim_sj3_internal_error()    uim_sj3_make_single_error("*SJ3-UIM-INTERNAL-ERROR*", N_("Internal error."))

static uim_lisp
uim_sj3_select_error(int errno, const struct uim_sj3_error *error)
{
  uim_lisp ret_ = uim_scm_null();

  while (error->error_sym != NULL) {
    if (errno & error->errno)
      ret_ = CONS(uim_sj3_make_error_pair(error->error_sym, error->error_str), ret_);
    error++;
  }

  if (NULLP(ret_))
    return uim_sj3_internal_error();

  return ret_ = CONS(MAKE_SYM("error"),
		     uim_scm_callf("reverse", "o", ret_));
}


const static struct uim_sj3_error uim_sj3_open_error[] = {
  { SJ3_NORMAL_END,        "*SJ3-NORMAL-END*"       , N_("Normal end.") },
  { SJ3_SERVER_DEAD,       "*SJ3-SERVER-DEAD*"      , N_("Server is dead.") },
  { SJ3_CONNECT_ERROR,     "*SJ3-CONNECT-ERROR*"    , N_("Connect failed.") },
  { SJ3_ALREADY_CONNECTED, "*SJ3-ALREADY-CONNECTED*", N_("Already connected.") },
  { SJ3_CANNOT_OPEN_MDICT, "*SJ3-CANNOT-OPEN-MDICT*", N_("Cannot open main dictionary file.") },
  { SJ3_CANNOT_OPEN_UDICT, "*SJ3-CANNOT-OPEN-UDICT*", N_("Cannot open user dictionary file.") },
  { SJ3_CANNOT_OPEN_STUDY, "*SJ3-CANNOT-OPEN-STUDY*", N_("Cannot open study file.") },
  { SJ3_CANNOT_MAKE_UDIR,  "*SJ3-CANNOT-MAKE-UDIR*",  N_("Cannot make user dictionary directory.") },
  { SJ3_CANNOT_MAKE_UDICT, "*SJ3-CANNOT-MAKE-UDICT*", N_("Cannot make user dictionary file.") },
  { SJ3_CANNOT_MAKE_STUDY, "*SJ3-CANNOT-MAKE-STUDY*", N_("Cannot make study file.")},
  { -1, NULL, NULL }
};

static uim_lisp
uim_sj3_open(uim_lisp sname_, uim_lisp uname_)
{
  char sname[MAXHOSTNAMELEN];
  char uname[MAXLOGNAME];
  int ret;

  if (strlcpy(sname, REFER_C_STR(sname_), sizeof(sname)) >= sizeof(sname))
    return uim_sj3_make_single_error("*SJ3-SERVER-NAME-TOO-LONG*", N_("Server name is too long."));

  if (strlcpy(uname, REFER_C_STR(uname_), sizeof(uname)) >= sizeof(uname))
    return uim_sj3_make_single_error("*SJ3-USER-NAME-TOO-LONG*", N_("User name is too long."));

  ret = sj3_open(sname, uname);

  if (ret == 0) {
    uim_sj3_is_open = 1;
    return uim_scm_t();
  }

  return uim_sj3_select_error(ret, uim_sj3_open_error);
}

static uim_lisp
uim_sj3_open_with_list(uim_lisp sname_, uim_lisp uname_, uim_lisp dict_list_)
{
  char sname[MAXHOSTNAMELEN];
  char uname[MAXLOGNAME];
  int dict_num;
  char **dict_list;
  int *err_num = NULL;
  int **err_index = NULL;
  int i;
  int ret;
  uim_lisp ret_;

  if (strlcpy(sname, REFER_C_STR(sname_), sizeof(sname)) >= sizeof(sname))
    return uim_sj3_make_single_error("*SJ3-SERVER-NAME-TOO-LONG*", N_("Server name is too long."));

  if (strlcpy(uname, REFER_C_STR(uname_), sizeof(uname)) >= sizeof(uname))
    return uim_sj3_make_single_error("*SJ3-USER-NAME-TOO-LONG*", N_("User name is too long."));

  dict_num = uim_scm_length(dict_list_);

  dict_list = uim_malloc(sizeof(char *) * dict_num);

  for (i = 0; i < dict_num; i++) {
    dict_list[i] = uim_strdup(REFER_C_STR(CAR(dict_list_)));
    dict_list_ = CDR(dict_list_);
  }

  ret = sj3_open_with_list(sname, uname, dict_num, dict_list, err_num, err_index);

  if (ret != 0) {
    for (i = 0; i < dict_num; i++)
      free(dict_list[i]);
    free(dict_list);
    return uim_sj3_select_error(ret, uim_sj3_open_error);
  }

  ret_ = uim_scm_null();
  for (i = 0; i < *err_num; i++)
    ret_ = CONS(MAKE_STR(dict_list[*err_index[i]]), ret_);

  for (i = 0; i < dict_num; i++)
    free(dict_list[i]);
  free(dict_list);

  return uim_scm_callf("reverse", "o", ret_);
}


const static struct uim_sj3_error uim_sj3_close_error[] = {
  { SJ3_NORMAL_END,        "*SJ3-NORMAL-END*"       , N_("Normal end.") },
  { SJ3_SERVER_DEAD,       "*SJ3-SERVER-DEAD*"      , N_("Server is dead.") },
  { SJ3_DISCONNECT_ERROR,  "*SJ3-DISCONNECT-ERROR*" , N_("Server is disconnected.") },
  { SJ3_NOT_CONNECTED,     "*SJ3-NOT-CONNECTED*"    , N_("Server is not connected.") },
  { SJ3_NOT_OPENED_MDICT,  "*SJ3-NOT-OPENED-MDICT*" , N_("Main dictionary file is not opened.") },
  { SJ3_NOT_OPENED_UDICT,  "*SJ3-NOT-OPENED-UDICT*" , N_("User dictionary file is not opened.") },
  { SJ3_NOT_OPENED_STUDY,  "*SJ3-NOT-OPENED-STUDY*" , N_("Study file is not opened.") },
  { SJ3_CLOSE_MDICT_ERROR, "*SJ3-CLOSE-MDICT-ERROR*", N_("Main dictionary file cannot close.") },
  { SJ3_CLOSE_UDICT_ERROR, "*SJ3-CLOSE-UDICT-ERROR*", N_("User dictionary file cannot close.") },
  { SJ3_CLOSE_STUDY_ERROR, "*SJ3-CLOSE-STUDY-ERROR*", N_("Study file cannot close.") },
  { 0, NULL, NULL }
};

static uim_lisp
uim_sj3_close(void)
{
  int ret;

  ret = sj3_close();

  if (ret == 0) {
    uim_sj3_is_open = 0;
    return uim_scm_t();
  }

  return uim_sj3_select_error(ret, uim_sj3_close_error);
}

/*
 * return: (kanji ((bunsetu-yomi1 bunsetu-kanji1) (bunsetu-yomi2 bunsetu-kanji2) ...))
 */
static uim_lisp
uim_sj3_getkan(uim_lisp yomi_)
{
  const char *yomi = REFER_C_STR(yomi_);
  int bunsetu_cnt;
  struct bunsetu bun[BUFSIZ / 2];
  char kanji[BUFSIZ];
  int i;
  uim_lisp ret_ = uim_scm_f();

  if (255 < strlen(yomi))
    return uim_sj3_make_single_error("*SJ3-YOMI-STRING-TOO-LONG*", N_("Yomi string is too long."));

  bunsetu_cnt = sj3_getkan_euc((unsigned char *)yomi, bun, (unsigned char *)kanji, sizeof(kanji));

  if (bunsetu_cnt == -1)
    return uim_sj3_server_down_error();

  if (bun[bunsetu_cnt - 1].destlen == 0) /* too large? */
    return uim_sj3_make_single_error("*SJ3-TOO-SHORT-BUFFER-SIZE*", N_("Buffer size is too short."));

  if (bunsetu_cnt == 0)
    return MAKE_STR("");

  ret_ = uim_scm_null();
  for (i = 0; i < bunsetu_cnt; i++) {
    char *yomi_str;
    char *kanji_str;

    yomi_str = uim_malloc(bun[i].srclen + 1);
    strlcpy(yomi_str, (const char *)bun[i].srcstr, bun[i].srclen + 1);

    kanji_str = uim_malloc(bun[i].destlen + 1);

    strlcpy(kanji_str, (const char *)bun[i].deststr, bun[i].destlen + 1);

    ret_ = CONS(LIST3(MAKE_STR(yomi_str),
		      MAKE_STR(kanji_str),
		      MAKE_PTR(&bun[i].dcid)),
		ret_);

    free(yomi_str);
    free(kanji_str);
  }

  ret_ = uim_scm_callf("reverse", "o", ret_);
  return CONS(MAKE_STR(kanji), ret_);
}

static uim_lisp
uim_sj3_douoncnt(uim_lisp yomi_)
{
  const char *yomi = REFER_C_STR(yomi_);
  int ret;

  if (63 < strlen(yomi))
    return uim_sj3_make_single_error("*SJ3-YOMI-STRING-TOO-LONG*", N_("Yomi string is too long."));

  ret = sj3_douoncnt_euc((unsigned char *)yomi);
  if (ret == -1)
    return uim_sj3_server_down_error();

  return MAKE_INT(ret);
}

static uim_lisp
uim_sj3_getdouon(uim_lisp yomi_)
{
  const char *yomi = REFER_C_STR(yomi_);
  int douon_cnt;
  struct douon douon[BUFSIZ];
  int i;
  uim_lisp ret_ = uim_scm_f();

  if (255 < strlen(yomi))
    return uim_sj3_make_single_error("*SJ3-YOMI-STRING-TOO-LONG*", N_("Yomi string is too long."));

  douon_cnt = sj3_getdouon_euc((unsigned char *)yomi, douon);
  if (douon_cnt == -1)
    return uim_sj3_server_down_error();

  ret_ = uim_scm_null();
  for (i = 0; i < douon_cnt; i++)
	  ret_ = CONS(LIST2(MAKE_STR((char *)douon[i].ddata), MAKE_PTR(&douon[i].dcid)), ret_);
  return uim_scm_callf("reverse", "o", ret_);
}

static uim_lisp
uim_sj3_getnthdouon(uim_lisp yomi_, uim_lisp nth_)
{
  const char *yomi = REFER_C_STR(yomi_);
  int douon_cnt;
  struct douon douon[BUFSIZ];
  int nth = C_INT(nth_);

  if (255 < strlen(yomi))
    return uim_sj3_make_single_error("*SJ3-YOMI-STRING-TOO-LONG*", N_("Yomi string is too long."));

  douon_cnt = sj3_getdouon_euc((unsigned char *)yomi, douon);
  if (douon_cnt == -1)
    return uim_sj3_server_down_error();

  if (douon_cnt < nth)
	  return uim_scm_f();
  return LIST2(MAKE_STR((char *)douon[nth].ddata), MAKE_PTR(&douon[nth].dcid));
}

static uim_lisp
uim_sj3_gakusyuu(uim_lisp dcid_)
{
  struct studyrec *dcid = C_PTR(dcid_);
  int ret;

  ret = sj3_gakusyuu(dcid);

  if (ret == -1)
    return uim_sj3_server_down_error();

  if (ret == 0)
    return uim_scm_t();

  return uim_sj3_undefined_error();
}

static uim_lisp
uim_sj3_gakusyuu2(uim_lisp yomi1_, uim_lisp yomi2_, uim_lisp dcid_)
{
  const char *yomi1 = REFER_C_STR(yomi1_);
  const char *yomi2;
  struct studyrec *dcid;
  int ret;

  if (STRP(yomi2_)) {
    /* split bunsetu |yomi1yomi2| -> |yomi1|yomi2| */
    yomi2 = REFER_C_STR(yomi2_);
    dcid = C_PTR(dcid_);
  } else {
    /* merge bunsetu |yomi1|yomi2| -> |yomi1| */
    yomi2 = NULL;
    dcid = NULL;
  }

  ret = sj3_gakusyuu2_euc((unsigned char *)yomi1, (unsigned char *)yomi2, dcid);

  if (ret == -1)
    return uim_sj3_server_down_error();

  if (ret == 0)
    return uim_scm_t();

  return uim_sj3_undefined_error();
}


const static struct {
  char *name;
  int val;
} uim_sj3_hinsi[] = {
  { "sj3-hinshi-nrmnoun", SJ3_H_NRMNOUN },
  { "sj3-hinshi-pronoun", SJ3_H_PRONOUN },
  { "sj3-hinshi-lname",   SJ3_H_LNAME   },
  { "sj3-hinshi-fname",   SJ3_H_FNAME   },
  { "sj3-hinshi-locname", SJ3_H_LOCNAME },
  { "sj3-hinshi-prefic",  SJ3_H_PREFIC  },
  { "sj3-hinshi-rentai",  SJ3_H_RENTAI  },
  { "sj3-hinshi-conjunc", SJ3_H_CONJUNC },
  { "sj3-hinshi-subnum",  SJ3_H_SUBNUM  },
  { "sj3-hinshi-numeral", SJ3_H_NUMERAL },
  { "sj3-hinshi-prefix",  SJ3_H_PREFIX  },
  { "sj3-hinshi-postfix", SJ3_H_POSTFIX },
  { "sj3-hinshi-adverb",  SJ3_H_ADVERB  },
  { "sj3-hinshi-adject",  SJ3_H_ADJECT  },
  { "sj3-hinshi-adjverb", SJ3_H_ADJVERB },
  { "sj3-hinshi-silverb", SJ3_H_SILVERB },
  { "sj3-hinshi-zilverb", SJ3_H_ZILVERB },
  { "sj3-hinshi-oneverb", SJ3_H_ONEVERB },
  { "sj3-hinshi-kaverb",  SJ3_H_KAVERB  },
  { "sj3-hinshi-gaverb",  SJ3_H_GAVERB  },
  { "sj3-hinshi-saverb",  SJ3_H_SAVERB  },
  { "sj3-hinshi-taverb",  SJ3_H_TAVERB  },
  { "sj3-hinshi-naverb",  SJ3_H_NAVERB  },
  { "sj3-hinshi-baverb",  SJ3_H_BAVERB  },
  { "sj3-hinshi-maverb",  SJ3_H_MAVERB  },
  { "sj3-hinshi-raverb",  SJ3_H_RAVERB  },
  { "sj3-hinshi-waverb",  SJ3_H_WAVERB  },
  { "sj3-hinshi-single",  SJ3_H_SINGLE  },
  { NULL, 0 }
};

const static struct uim_sj3_error uim_sj3_touroku_syoukyo_error[] = {
  { SJ3_DICT_ERROR,     "*SJ3-DICT-ERROR*"    , N_("Dictionary error.") },
  { SJ3_DICT_LOCKED,    "*SJ3-DICT-LOCKED*"   , N_("Dictionary is locked.") },
  { SJ3_BAD_YOMI_STR,   "*SJ3-BAD-YOMI-STR*"  , N_("Invalid yomi string.") },
  { SJ3_BAD_KANJI_STR,  "*SJ3-BAD-KANJI-STR*" , N_("Invalid kanji string.") },
  { SJ3_BAD_HINSI_CODE, "*SJ3-BAD-HINSI-CODE*", N_("Invalid hinsi code.") },
  { SJ3_WORD_EXIST,     "*SJ3-WORD-EXIST*"    , N_("Word exist.") },
  { SJ3_DOUON_FULL,     "*SJ3-DOUON-FULL*"    , N_("Douon is full.") },
  { SJ3_DICT_FULL,      "*SJ3-DICT-FULL*"     , N_("Dictionary is full.") },
  { SJ3_INDEX_FULL,     "*SJ3-INDEX-FULL*"    , N_("Index is full.") },
  { SJ3_TOUROKU_FAILED, "*SJ3-TOUROKU-FAILED*", N_("Touroku failed.") },
  { SJ3_WORD_NOT_EXIST, "*SJ3-WORD-NOT-EXIST*", N_("Word does not exist.") },
  { SJ3_SYOUKYO_FAILED, "*SJ3-SYOUKYO-FAILED*", N_("Syoukyo failed.") },
  { 0, NULL, NULL }
};

static uim_lisp
uim_sj3_touroku(uim_lisp yomi_, uim_lisp kanji_, uim_lisp hinsi_)
{
  const char *yomi = REFER_C_STR(yomi_);
  const char *kanji = REFER_C_STR(kanji_);
  const char *hinsi_str = REFER_C_STR(hinsi_);
  int i = 0;
  int ret;

  if (31 < strlen(yomi) || 31 < strlen(kanji))
    return uim_sj3_make_single_error("*SJ3-KANJI-STRING-TOO-LONG*", N_("Kanji string is too long."));

  while (1) {
    if (uim_sj3_hinsi[i].name == NULL)
      return uim_sj3_internal_error();
    if (strcmp(uim_sj3_hinsi[i].name, hinsi_str) == 0)
      break;
    i++;
  }

  ret = sj3_touroku_euc((unsigned char*)yomi, (unsigned char*)kanji, uim_sj3_hinsi[i].val);

  if (ret == -1)
    return uim_sj3_server_down_error();

  if (ret == 0)
    return uim_scm_t();

  return uim_sj3_select_error(ret, uim_sj3_touroku_syoukyo_error);
}

static uim_lisp
uim_sj3_syoukyo(uim_lisp yomi_, uim_lisp kanji_, uim_lisp hinsi_)
{
  const char *yomi = REFER_C_STR(yomi_);
  const char *kanji = REFER_C_STR(kanji_);
  const char *hinsi_str = REFER_C_STR(hinsi_);
  int i = 0;
  int ret;

  if (31 < strlen(yomi) || 31 < strlen(kanji))
    return uim_sj3_make_single_error("*SJ3-KANJI-STRING-TOO-LONG*", N_("Kanji string is too long."));

  while (1) {
    if (uim_sj3_hinsi[i].name == NULL)
      return uim_sj3_internal_error();
    if (strcmp(uim_sj3_hinsi[i].name, hinsi_str) == 0)
      break;
    i++;
  }

  ret = sj3_syoukyo_euc((unsigned char*)yomi, (unsigned char*)kanji, uim_sj3_hinsi[i].val);

  if (ret == -1)
    return uim_sj3_server_down_error();

  if (ret == 0)
    return uim_scm_t();

  return uim_sj3_select_error(ret, uim_sj3_touroku_syoukyo_error);
}

static uim_lisp
uim_sj3_lockserv(void)
{
  int ret;

  ret = sj3_lockserv();

  if (ret == -1)
    return uim_sj3_server_down_error();

  if (ret == 0)
    return uim_scm_t();

  return uim_sj3_undefined_error();
}

static uim_lisp
uim_sj3_unlockserv(void)
{
  int ret;

  ret = sj3_unlockserv();

  if (ret == -1)
    return uim_sj3_server_down_error();

  if (ret == 0)
    return uim_scm_t();

  return uim_sj3_undefined_error();
}

void
uim_plugin_instance_init(void)
{
  uim_scm_init_proc2("sj3-lib-open", uim_sj3_open);
  uim_scm_init_proc3("sj3-lib-open-with-list", uim_sj3_open_with_list);
  uim_scm_init_proc0("sj3-lib-close", uim_sj3_close);
  uim_scm_init_proc1("sj3-lib-getkan", uim_sj3_getkan);
  uim_scm_init_proc1("sj3-lib-douoncnt", uim_sj3_douoncnt);
  uim_scm_init_proc1("sj3-lib-getdouon", uim_sj3_getdouon);
  uim_scm_init_proc2("sj3-lib-get-nth-douon", uim_sj3_getnthdouon);
  uim_scm_init_proc1("sj3-lib-gakusyuu", uim_sj3_gakusyuu);
  uim_scm_init_proc3("sj3-lib-gakusyuu2", uim_sj3_gakusyuu2);
  uim_scm_init_proc3("sj3-lib-touroku", uim_sj3_touroku);
  uim_scm_init_proc3("sj3-lib-syoukyo", uim_sj3_syoukyo);
  uim_scm_init_proc0("sj3-lib-lockserv", uim_sj3_lockserv);
  uim_scm_init_proc0("sj3-lib-unlockserv", uim_sj3_unlockserv);
}

void
uim_plugin_instance_quit(void)
{
  if (uim_sj3_is_open)
    sj3_close();
  uim_sj3_is_open = 0;
}
