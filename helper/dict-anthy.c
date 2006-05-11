/*
 *  $Id:$
 *  Copyright (c) 2003,2004 Masahito Omote <omote@utyuuzin.net>
 *                2005-2006 uim Project http://uim.freedesktop.org/
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

#include <dlfcn.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <glib.h>

#include <anthy/dicutil.h>

#include "uim/gettext.h"
#include "dict-anthy.h"
#include "dict-canna.h"

static uim_dict *uim_dict_anthy_open        (const char *identifier);
static void      uim_dict_anthy_close       (uim_dict *dict);
static int       uim_dict_anthy_add_word    (uim_dict *dict, uim_word *word);
static int       uim_dict_anthy_change_word (uim_dict *dict, uim_word *word);
static int       uim_dict_anthy_remove_word (uim_dict *dict, uim_word *word);
static void      uim_dict_anthy_refresh     (uim_dict *dict);

#if 0 /* should be accessed through functions */
static
#endif
uim_dict_class uim_dict_class_anthy = {
  "Anthy",                    /* type */

  NULL,                       /* init */
  NULL,                       /* exit */

  NULL,                       /* list_known_dict */
  NULL,                       /* default_priv_dict */

  uim_dict_anthy_open,        /* open */

  NULL,                       /* is_supported */
  NULL,                       /* load_file */
  NULL,                       /* save_file */

  uim_dict_anthy_close,       /* close */

  uim_dict_anthy_add_word,    /* add_word */
  uim_dict_anthy_change_word, /* change_word */
  uim_dict_anthy_remove_word, /* remove_word */
  uim_dict_anthy_refresh,     /* refresh */
};

static const char *identifiers[] = {
  N_("Anthy private dictionary"),
};

static int
dict_anthy_init(void)
{
    anthy_dic_util_init();
    return 0;
}

static int
dict_anthy_exit(void)
{
  anthy_dic_util_quit();
  return 0;
}

static int
dict_anthy_read_priv_dic_list(uim_word **head)
{
    char phon[100], desc[100], cclass_code[100];
    int ret = 0;

    if (anthy_priv_dic_select_first_entry() == -1) {
	*head = NULL;
	return -1;
    }

    while (ret == 0) {
	if (anthy_priv_dic_get_index(phon, sizeof(phon))
	   && anthy_priv_dic_get_wtype(cclass_code, sizeof(cclass_code))
	   && anthy_priv_dic_get_word(desc, sizeof(desc))) {
	    gint pos;
	    char *cclass_desc = NULL;

	    for (pos = 0; pos < NR_POS; pos++) {
	        cclass_desc = find_desc_from_code(cclass_code, pos);
		if (cclass_desc) break;
	    }

	    word_append(head, WORD_TYPE_ANTHY,
			"EUC-JP",
			phon, desc, cclass_desc,
			anthy_priv_dic_get_freq(),
			0, NULL);
	}
	ret = anthy_priv_dic_select_next_entry();
    }
    return 0;
}

static int
dict_anthy_add_priv_dic_with_flags(char *phon, char *desc,
				   char *cclass_code, int freq)
{
    if ((strlen(phon) == 0) ||
       (strlen(desc) == 0) ||
       (strlen(cclass_code) == 0)) {
	return -1;
    }

    return anthy_priv_dic_add_entry(phon, desc,
					    cclass_code, freq);
}

static int
dict_anthy_delete_priv_dic(char *phon, char *desc, char *cclass_code)
{
    return anthy_priv_dic_add_entry(phon, desc,
					    cclass_code, 0);
}

static uim_dict *
uim_dict_anthy_open(const char *identifier)
{
  uim_dict *dict;

  if (dict_anthy_init() == -1)
    return NULL;

  if (identifier == NULL)
    return NULL;

  if (strcmp(identifier, identifiers[0]) != 0)
    return NULL;

  dict = malloc(sizeof(uim_dict));
  if (dict == NULL)
    return NULL;

  dict->funcs      = &uim_dict_class_anthy;
  dict->identifier = strdup(identifier);
  dict->filename   = NULL;
  dict->charset    = strdup("EUC-JP");
  dict->ref_count  = 1;
  dict->word_list  = NULL;

  dict_anthy_read_priv_dic_list(&dict->word_list);

  return dict;
}

static void
uim_dict_anthy_close(uim_dict *dict)
{
  if (dict == NULL)
    return;

  word_free_list(dict->word_list);

  free(dict->identifier);
  free(dict->filename);
  free(dict->charset);
  free(dict);

  dict_anthy_exit();
}

static int
uim_dict_anthy_add_word(uim_dict *dict, uim_word *word)
{
  int ret;

  if (dict == NULL || word == NULL)
    return -1;

  /* FIXME! refresh word list */
  ret = dict_anthy_add_priv_dic_with_flags(word->phon, word->desc,
					   word->cclass_code, word->freq);

  return ret;
}

static int
uim_dict_anthy_change_word(uim_dict *dict, uim_word *word)
{
  return -1;
}

static int
uim_dict_anthy_remove_word(uim_dict *dict, uim_word *word)
{
  int ret;

  if (dict == NULL)
    return 0;

  ret = dict_anthy_delete_priv_dic(word->phon, word->desc,
				   word->cclass_code);

  return ret;
}

static void
uim_dict_anthy_refresh(uim_dict *dict)
{
  if (dict == NULL)
    return;

  dict->word_list = NULL;
  word_free_list(dict->word_list);
  dict_anthy_read_priv_dic_list(&dict->word_list);
}
