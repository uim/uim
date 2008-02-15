/* 
  Copyright (c) 2003-2008 uim Project http://uim.freedesktop.org/

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

#include <sj3lib.h>

#include "uim.h"
#include "uim-scm.h"
#include "uim-scm-abbrev.h"
#include "uim-helper.h"
#include "plugin.h"

static int uim_sj3_is_open = 0;

struct uim_sj3_error {
  int errno;
  char *error;
};

static uim_lisp
uim_sj3_make_error(char *error)
{
  return CONS(MAKE_SYM("error"), MAKE_SYM(error));
}

#define uim_sj3_server_down_error() uim_sj3_make_error("*SJ3-SERVER-DOWN-ERROR*")
#define uim_sj3_undefined_error()   uim_sj3_make_error("*SJ3-UNDEFINED-ERROR*")
#define uim_sj3_internal_error()    uim_sj3_make_error("*SJ3-UIM-INTERNAL-ERROR*")

static uim_lisp
uim_sj3_select_error(int errno, const struct uim_sj3_error *error)
{
  uim_lisp ret_ = uim_scm_null();

  while (error->error != NULL) {
    if (errno & error->errno)
      ret_ = CONS(MAKE_SYM(error->error), ret_);
    error++;
  }

  if (NULLP(ret_))
    return uim_sj3_internal_error();

  return ret_ = CONS(MAKE_SYM("error"),
		     uim_scm_callf("reverse", "o", ret_));
}


const static struct uim_sj3_error uim_sj3_open_error[] = {
  { SJ3_NORMAL_END,        "*SJ3-NORMAL-END*"        },
  { SJ3_SERVER_DEAD,       "*SJ3-SERVER-DEAD*"       },
  { SJ3_CONNECT_ERROR,     "*SJ3-CONNECT-ERROR*"     },
  { SJ3_ALREADY_CONNECTED, "*SJ3-ALREADY-CONNECTED*" },
  { SJ3_CANNOT_OPEN_MDICT, "*SJ3-CANNOT-OPEN-MDICT*" },
  { SJ3_CANNOT_OPEN_UDICT, "*SJ3-CANNOT-OPEN-UDICT*" },
  { SJ3_CANNOT_OPEN_STUDY, "*SJ3-CANNOT-OPEN-STUDY*" },
  { SJ3_CANNOT_MAKE_UDIR,  "*SJ3-CANNOT-MAKE-UDIR*"  },
  { SJ3_CANNOT_MAKE_UDICT, "*SJ3-CANNOT-MAKE-UDICT*" },
  { SJ3_CANNOT_MAKE_STUDY, "*SJ3-CANNOT-MAKE-STUDY*" },
  { -1, NULL }
};

static uim_lisp
uim_sj3_open(uim_lisp sname_, uim_lisp uname_)
{
  char *sname = strdup(REFER_C_STR(sname_));
  char *uname = strdup(REFER_C_STR(uname_));
  int ret;

  if (!sname || !uname)
    return uim_scm_f();

  ret = sj3_open(sname, uname);

  if (!sname)
    free(sname);
  if (!uname)
    free(uname);

  if (ret == 0) {
    uim_sj3_is_open = 1;
    return uim_scm_t();
  }

  return uim_sj3_select_error(ret, uim_sj3_open_error);
}

static uim_lisp
uim_sj3_open_with_list(uim_lisp sname_, uim_lisp uname_, uim_lisp dict_list_)
{
  char *sname = strdup(REFER_C_STR(sname_));
  char *uname = strdup(REFER_C_STR(uname_));
  int dict_num;
  char **dict_list;
  int *err_num = NULL;
  int **err_index = NULL;
  int i;
  int ret;
  uim_lisp ret_;

  if (!sname || !uname)
    return uim_scm_f();

  dict_num = uim_scm_length(dict_list_);

  dict_list = malloc(sizeof(char *) * dict_num);

  for (i = 0; i < dict_num; i++) {
    dict_list[i] = strdup(REFER_C_STR(uim_scm_car(dict_list_)));
    dict_list_ = CDR(dict_list_);
  }

  ret = sj3_open_with_list(sname, uname, dict_num, dict_list, err_num, err_index);

  if (!sname)
    free(sname);
  if (!uname)
  free(uname);

  if (ret != 0) {
    for (i = 0; i < dict_num; i++) {
      if (!dict_list[i])
	free(dict_list[i]);
    }
    if (!dict_list)
      free(dict_list);
    return uim_sj3_select_error(ret, uim_sj3_open_error);
  }

  ret_ = uim_scm_null();
  for (i = 0; i < *err_num; i++) {
    ret_ = CONS(MAKE_STR(dict_list[*err_index[i]]), ret_);
  }

  for (i = 0; i < dict_num; i++) {
    if (!dict_list[i])
      free(dict_list[i]);
  }
  if (!dict_list)
    free(dict_list);

  return uim_scm_callf("reverse", "o", ret_);
}


const static struct uim_sj3_error uim_sj3_close_error[] = {
  { SJ3_NORMAL_END,        "*SJ3-NORMAL-END*"        },
  { SJ3_SERVER_DEAD,       "*SJ3-SERVER-DEAD*"       },
  { SJ3_DISCONNECT_ERROR,  "*SJ3-DISCONNECT-ERROR*"  },
  { SJ3_NOT_CONNECTED,     "*SJ3-NOT-CONNECTED*"     },
  { SJ3_NOT_OPENED_MDICT,  "*SJ3-NOT-OPENED-MDICT*"  },
  { SJ3_NOT_OPENED_UDICT,  "*SJ3-NOT-OPENED-UDICT*"  },
  { SJ3_NOT_OPENED_STUDY,  "*SJ3-NOT-OPENED-STUDY*"  },
  { SJ3_CLOSE_MDICT_ERROR, "*SJ3-CLOSE-MDICT-ERROR*" },
  { SJ3_CLOSE_UDICT_ERROR, "*SJ3-CLOSE-UDICT-ERROR*" },
  { SJ3_CLOSE_STUDY_ERROR, "*SJ3-CLOSE-STUDY-ERROR*" },
  { 0, NULL }
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
    return uim_sj3_make_error("*SJ3-YOMI-STRING-TOO-LONG*");

  bunsetu_cnt = sj3_getkan_euc((unsigned char *)yomi, bun, (unsigned char *)kanji, sizeof(kanji));

  if (bunsetu_cnt == -1)
    return uim_sj3_server_down_error();

  if (bun[bunsetu_cnt - 1].destlen == 0) /* too large? */
    return uim_sj3_make_error("*SJ3-TOO-SHORT-BUFFER-SIZE*");

  if (bunsetu_cnt == 0)
    return MAKE_STR("");

  ret_ = uim_scm_null();
  for (i = 0; i < bunsetu_cnt; i++) {
    char *yomi_str;
    char *kanji_str;

    yomi_str = malloc(bun[i].srclen + 1);
    if (!yomi_str)
      return uim_sj3_internal_error(); /* XXX: fatal */
    strlcpy(yomi_str, (const char *)bun[i].srcstr, bun[i].srclen + 1);

    kanji_str = malloc(bun[i].destlen + 1);
    if (!kanji_str)
      return uim_sj3_internal_error(); /* XXX: fatal */
    strlcpy(kanji_str, (const char *)bun[i].deststr, bun[i].destlen + 1);

    ret_ = CONS(LIST3(uim_scm_make_str(yomi_str),
		      uim_scm_make_str(kanji_str),
		      uim_scm_make_ptr(&bun[i].dcid)),
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
    return uim_sj3_make_error("*SJ3-YOMI-STRING-TOO-LONG*");

  ret = sj3_douoncnt_euc((unsigned char *)yomi);
  if (ret == -1)
    return uim_sj3_server_down_error();

  return uim_scm_make_int(ret);
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
    return uim_sj3_make_error("*SJ3-YOMI-STRING-TOO-LONG*");

  douon_cnt = sj3_getdouon_euc((unsigned char *)yomi, douon);
  if (douon_cnt == -1)
    return uim_sj3_server_down_error();

  ret_ = uim_scm_null();
  for (i = 0; i < douon_cnt; i++)
	  ret_ = CONS(LIST2(MAKE_STR((char *)douon[i].ddata), MAKE_PTR(&douon[i].dcid)), ret_);
  return uim_scm_callf("reverse", "o", ret_);
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

  if (uim_scm_stringp(yomi2_)) {
    yomi2 = REFER_C_STR(yomi2_);
    dcid = C_PTR(dcid_);
  } else {
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
  { SJ3_DICT_ERROR,     "*SJ3-DICT-ERROR*"     },
  { SJ3_DICT_LOCKED,    "*SJ3-DICT-LOCKED*"    },
  { SJ3_BAD_YOMI_STR,   "*SJ3-BAD-YOMI-STR*"   },
  { SJ3_BAD_KANJI_STR,  "*SJ3-BAD-KANJI-STR*"  },
  { SJ3_BAD_HINSI_CODE, "*SJ3-BAD-HINSI-CODE*" },
  { SJ3_WORD_EXIST,     "*SJ3-WORD-EXIST*"     },
  { SJ3_DOUON_FULL,     "*SJ3-DOUON-FULL*"     },
  { SJ3_DICT_FULL,      "*SJ3-DICT-FULL*"      },
  { SJ3_INDEX_FULL,     "*SJ3-INDEX-FULL*"     },
  { SJ3_TOUROKU_FAILED, "*SJ3-TOUROKU-FAILED*" },
  { SJ3_WORD_NOT_EXIST, "*SJ3-WORD-NOT-EXIST*" },
  { SJ3_SYOUKYO_FAILED, "*SJ3-SYOUKYO-FAILED*" }
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
    return uim_sj3_make_error("*SJ3-KANJI-STRING-TOO-LONG*");

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
    return uim_sj3_make_error("*SJ3-KANJI-STRING-TOO-LONG*");

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
