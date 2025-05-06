/*
 *  Copyright (c) 2006-2013 uim Project https://github.com/uim/uim
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
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS IS'' AND
 *  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 *  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 *  ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE
 *  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 *  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 *  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 *  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 *  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 *  OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 *  SUCH DAMAGE.
 */

#include <config.h>

#include <string.h>
#include <pwd.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/param.h>

#define CANNA_NEW_WCHAR_AWARE
#include <canna/RK.h>

#include <stdlib.h>
#include <stdio.h>

#include "gettext.h"

#include "canna.h"
#include "canna-cclass.h"

extern int RkGetWordTextDic(int, unsigned char *, unsigned char *,
			    unsigned char *, int);
extern int RkCreateDic(int, unsigned char *, int);

static char *g_cannaserver;

/* Canna support */
static uim_dict *uim_dict_canna_open(const char *identifier);
static void uim_dict_canna_close(uim_dict *dict);
static int uim_dict_canna_add_word(uim_dict *dict, uim_word *word);
static int uim_dict_canna_change_word(uim_dict *dict, uim_word *word);
static int uim_dict_canna_remove_word(uim_dict *dict, uim_word *word);
static void uim_dict_canna_refresh(uim_dict *dict);

uim_dict_class uim_dict_class_canna = {
  "Canna",	/* type */
  NULL,
  NULL,

  NULL,
  NULL,

  uim_dict_canna_open,	/* open */

  NULL,
  NULL,
  NULL,

  uim_dict_canna_close, /* close */

  uim_dict_canna_add_word,    /* add_word */
  uim_dict_canna_change_word, /* change_word */
  uim_dict_canna_remove_word, /* remove_word */
  uim_dict_canna_refresh,     /* refresh */
};

static const char *identifiers[] = {
  N_("Canna private dictionary"),
};

static int
dict_canna_init(void)
{
  return RkInitialize(g_cannaserver);
}

static int
dict_canna_exit(void)
{
  RkFinalize();

  return 0;
}

static int
dict_canna_get_priv_dic_dir(char *dirname, size_t len)
{
  struct passwd *pw;
  char dir[] = ":user/";

  if ((pw = getpwuid(getuid())) != NULL) {
    /* dirname := ":user/username" */
    snprintf(dirname, len, "%s%s", dir, pw->pw_name);
    endpwent();
    return 1;
  }
  endpwent();

  return 0;
}

static void
canna_word_append(uim_word **head, char *phon, char *cclass_native, char *desc,
		  int freq)
{
  int pos;
  const char *cclass_code = NULL;

  if (phon && desc && cclass_native) {
    for (pos = 0; pos < NR_POS; pos++) {
      cclass_code = find_desc_from_code_with_type(cclass_native, pos);
      if (cclass_code)
	break;
    }

    word_append(head, WORD_TYPE_CANNA, "EUC-JP",
		phon, desc, cclass_code, cclass_native,
		freq, 0, NULL);
  }
}

static void parse_canna_priv_dic_buf(char *buf, uim_word **head)
{
  char *p, *q, *r, *s;
  char *phon, *desc, *cclass_native;
  int len, freq = 1;

  phon = desc = cclass_native = NULL;
  len = strlen(buf);
  if (buf[len - 1] == '\n')
    buf[len - 1] = '\0';

  p = strchr(buf, ' ');
  if (p) {
    *p = '\0';
    p++;
  }
  phon = strdup(buf);

  for (;;) {
    if (!p)
      break;
    q = strchr(p, ' ');
    if (q) {
      *q = '\0';
    }

    if (p[0] == '#') {
      freq = 1;
      r = strdup(p);
      s = strchr(r, '*');
      if (s != NULL) {
	*s = '\0';
	s++;
	freq = atoi(s);
      }
      cclass_native = r;
    } else {
      desc = strdup(p);
      canna_word_append(head, phon, cclass_native, desc, freq);
      free(cclass_native);
      free(desc);
      cclass_native = desc = NULL;
    }

    if (q == NULL)
      break;
    p = q;
    p++;
  }
  free(phon);
}

static int
dict_canna_create_priv_dic()
{
  unsigned char dicname[] = CANNA_DEFAULT_PRIV_DICNAME;

  return RkCreateDic(CANNA_STD_CONTEXT, dicname, RK_USR_DIC);
}

static int
dict_canna_read_priv_dic_list(uim_word **head)
{
  int status = 0;

  unsigned char buf[CANNA_PRIV_DIC_BUFLEN];
  unsigned char dicname[] = CANNA_DEFAULT_PRIV_DICNAME;
  char dirname[MAXPATHLEN];

  if (!dict_canna_get_priv_dic_dir(dirname, sizeof(dirname)))
    return status;

  status = RkGetWordTextDic(CANNA_STD_CONTEXT, (unsigned char *)dirname,
			    dicname, buf, CANNA_PRIV_DIC_BUFLEN);

  if (status == -2)
    return status;


  while (status > 0)  {
    parse_canna_priv_dic_buf((char *)buf, head);
    status = RkGetWordTextDic(CANNA_STD_CONTEXT, (unsigned char *)"",
			      (unsigned char *)"", buf, CANNA_PRIV_DIC_BUFLEN);
  }

  return status;
}

static int
dict_canna_add_entry_to_priv_dic(char *phon, char *desc, char *cclass_native,
				 int freq)
{
  int status;
  char dicname[] = CANNA_DEFAULT_PRIV_DICNAME;
  char *entry;

  if (strlen(phon) == 0 || strlen(desc) == 0 || strlen(cclass_native) == 0)
    return 0;

  RkInitialize(g_cannaserver);
  status = RkMountDic(CANNA_STD_CONTEXT, dicname, 0);

  if (status == 0) {
    if (asprintf(&entry, "%s %s %s\n", phon, cclass_native, desc) < 0 || entry == NULL) {
      status = RkDefineDic(CANNA_STD_CONTEXT, dicname, entry);
      free(entry);
    }
    RkUnmountDic(CANNA_STD_CONTEXT, dicname);
  }
  RkFinalize();

  return status ? 0 : 1;
}

static int
dict_canna_delete_entry_from_priv_dic(char *phon, char *desc,
				      char *cclass_native)
{
  int status;
  char dicname[] = CANNA_DEFAULT_PRIV_DICNAME;
  char *entry;

  if (strlen(phon) == 0 || strlen(desc) == 0 || strlen(cclass_native) == 0)
    return 0;

  RkInitialize(g_cannaserver);
  status = RkMountDic(CANNA_STD_CONTEXT, dicname, 0);

  if (status == 0) {
    if (asprintf(&entry, "%s %s %s\n", phon, cclass_native, desc) < 0 || entry == NULL) {
      status = RkDeleteDic(CANNA_STD_CONTEXT, dicname, entry);
      free(entry);
    }
    RkUnmountDic(CANNA_STD_CONTEXT, dicname);
  }
  RkFinalize();

  return status ? 0 : 1;
}

static uim_dict *
uim_dict_canna_open(const char *identifier)
{
  uim_dict *dict;
  int status;

  if (!identifier)
    return NULL;

  if (strcmp(identifier, identifiers[0]) != 0)
    return NULL;

  if (dict_canna_init() == -1)
    return NULL;

  dict = malloc(sizeof(uim_dict));
  if (!dict) {
    RkFinalize();
    return NULL;
  }

  dict->funcs = &uim_dict_class_canna;
  dict->identifier = strdup(identifier);
  dict->filename = NULL;
  dict->charset = strdup("EUC-JP");
  dict->ref_count = 0; /* at this point, no window refers this */
  dict->word_list = NULL;

  status = dict_canna_read_priv_dic_list(&dict->word_list);

  if (status == -2) {
    if (dict_canna_create_priv_dic() < 0) {
      free(dict->identifier);
      free(dict->charset);
      free(dict);
      RkFinalize();
      return NULL;
    }
  }
  RkFinalize();

  return dict;
}

static void
uim_dict_canna_close(uim_dict *dict)
{
  if (dict == NULL)
    return;

  word_free_list(dict->word_list);

  free(dict->identifier);
  free(dict->filename);
  free(dict->charset);
  free(dict);

  dict_canna_exit();
}

static int
uim_dict_canna_add_word(uim_dict *dict, uim_word *word)
{
  if (dict == NULL || word == NULL)
    return 0;

  return dict_canna_add_entry_to_priv_dic(word->phon, word->desc,
					  word->cclass_native, word->freq);
}

static int
uim_dict_canna_change_word(uim_dict *dict, uim_word *word)
{
  return 0;
}

static int
uim_dict_canna_remove_word(uim_dict *dict, uim_word *word)
{
  if (dict == NULL)
    return 0;

  return dict_canna_delete_entry_from_priv_dic(word->phon, word->desc,
				      word->cclass_native);
}

static void
uim_dict_canna_refresh(uim_dict *dict)
{
  if (dict == NULL)
    return;

  dict->word_list = NULL;
  word_free_list(dict->word_list);

  RkInitialize(g_cannaserver);
  dict_canna_read_priv_dic_list(&dict->word_list);
  RkFinalize();
}
