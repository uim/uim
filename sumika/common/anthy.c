/*
 *  $Id:$
 *  Copyright (c) 2003,2004 Masahito Omote <omote@utyuuzin.net>
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
 *  THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 *  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 *  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 *  ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
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

#include "anthy.h"

struct _anthy_dic_api {
    void (*util_init)();
    void (*util_set_personality)(char *);
    const char *(*util_get_anthydir)(void);

    void (*priv_dic_delete)(void);
    int (*priv_dic_select_first_entry)(void);
    int (*priv_dic_select_next_entry)(void);
    int (*priv_dic_select_entry)(char *);

    char *(*priv_dic_get_index)(char *, int);
    int (*priv_dic_get_freq)(void);
    char *(*priv_dic_get_wtype)(char *, int);
    char *(*priv_dic_get_word)(char *, int);

    int (*priv_dic_add_entry)(char *, char *, char *, int);
};
static struct _anthy_dic_api anthy_dic_api;
static void *anthy_dic_lib;

static int get_anthydic_api() {
    anthy_dic_lib = dlopen("libanthydic.so.0", RTLD_GLOBAL |RTLD_NOW);
    if(anthy_dic_lib == NULL) {
	return -1;
    }

    anthy_dic_api.util_init = dlsym(anthy_dic_lib, "anthy_dic_util_init");
    anthy_dic_api.util_set_personality = dlsym(anthy_dic_lib, "anthy_dic_util_set_personality");
    anthy_dic_api.util_get_anthydir = dlsym(anthy_dic_lib, "anthy_dic_util_get_anthydir");

    anthy_dic_api.priv_dic_delete = dlsym(anthy_dic_lib, "anthy_priv_dic_delete");
    anthy_dic_api.priv_dic_select_first_entry = dlsym(anthy_dic_lib, "anthy_priv_dic_select_first_entry");
    anthy_dic_api.priv_dic_select_next_entry = dlsym(anthy_dic_lib, "anthy_priv_dic_select_next_entry");
    anthy_dic_api.priv_dic_select_entry = dlsym(anthy_dic_lib, "anthy_priv_dic_select_entry");

    anthy_dic_api.priv_dic_get_index = dlsym(anthy_dic_lib, "anthy_priv_dic_get_index");
    anthy_dic_api.priv_dic_get_freq = dlsym(anthy_dic_lib, "anthy_priv_dic_get_freq");
    anthy_dic_api.priv_dic_get_wtype = dlsym(anthy_dic_lib, "anthy_priv_dic_get_wtype");
    anthy_dic_api.priv_dic_get_word = dlsym(anthy_dic_lib, "anthy_priv_dic_get_word");

    anthy_dic_api.priv_dic_add_entry = dlsym(anthy_dic_lib, "anthy_priv_dic_add_entry");

    if(!anthy_dic_api.util_init && !anthy_dic_api.util_set_personality
       && !anthy_dic_api.util_get_anthydir && !anthy_dic_api.priv_dic_delete
       && !anthy_dic_api.priv_dic_select_first_entry
       && !anthy_dic_api.priv_dic_select_next_entry
       && !anthy_dic_api.priv_dic_select_entry
       && !anthy_dic_api.priv_dic_get_index
       && !anthy_dic_api.priv_dic_get_freq
       && !anthy_dic_api.priv_dic_get_wtype
       && !anthy_dic_api.priv_dic_get_word
       && !anthy_dic_api.priv_dic_add_entry) {
	dlclose(anthy_dic_lib);
	return -1;
    }
    return 0;
}

int anthydic_init(void) {
    if(get_anthydic_api() == -1)
	return -1;
    anthy_dic_api.util_init();
    return 0;
}

int anthydic_close(void) {
    if(anthy_dic_lib)
	return dlclose(anthy_dic_lib);
    else
	return -1;
}

int read_anthy_priv_dic_list(word **head) {
    char phon[100], desc[100], cclass_code[100];
    int ret = 0;

    if(anthy_dic_api.priv_dic_select_first_entry() == -1) {
	*head = NULL;
	return -1;
    }

    while(ret == 0) {
	if(anthy_dic_api.priv_dic_get_index(phon, 100)
	   && anthy_dic_api.priv_dic_get_wtype(cclass_code, 100)
	   && anthy_dic_api.priv_dic_get_word(desc, 100)) {

	    word_append(head, WORD_TYPE_ANTHY,
			phon, desc, cclass_code,
			anthy_dic_api.priv_dic_get_freq(),
			0, NULL);
	}
	ret = anthy_dic_api.priv_dic_select_next_entry();
    }
    return 0;
}

GList *anthy_read_privatediclist(void) {
    GList *list = NULL;
    gchar euc_literal[100], euc_phonetic[100], euc_pos[100];
    gchar *literal, *phonetic, *pos;
    gchar cclass[100], score[100];
    gint ret = 0, rbytes, wbytes;

    if(anthy_dic_api.priv_dic_select_first_entry() == -1) {
	return NULL;
    }
#if 0
    while(ret == 0) {
	if(anthy_dic_api.priv_dic_get_index(euc_phonetic, 100)
	   && anthy_dic_api.priv_dic_get_wtype(euc_pos, 100)
	   && anthy_dic_api.priv_dic_get_word(euc_literal, 100)) {
	    phonetic = g_convert(euc_phonetic, -1, "UTF-8", "EUC-JP", &rbytes, &wbytes, NULL);
	    literal = g_convert(euc_literal, -1, "UTF-8", "EUC-JP", &rbytes, &wbytes, NULL);
	    pos = g_convert(euc_pos, -1, "UTF-8", "EUC-JP", &rbytes, &wbytes, NULL);
	    g_snprintf(score, 100, "%d", anthy_dic_api.priv_dic_get_freq());
	    cclass[0] = '\0'; /* XXX */
	    list = dixchange_append_word(list, literal, phonetic, pos, cclass, score);
	    g_free(phonetic);
	    g_free(literal);
	    g_free(pos);
	}
	ret = anthy_dic_api.priv_dic_select_next_entry();
    }
#endif /* 0 */
    return list;
}

int add_anthy_priv_dic_with_flags(char *phon, char *desc,
				  char *cclass_code, int freq)
{
    if((strlen(phon) == 0) ||
       (strlen(desc) == 0) ||
       (strlen(cclass_code) == 0)) {
	return -1;
    }

    return anthy_dic_api.priv_dic_add_entry(phon, desc,
					    cclass_code, freq);
}

int delete_anthy_priv_dic(char *phon, char *desc, char *cclass_code) {
    return anthy_dic_api.priv_dic_add_entry(phon, desc,
					    cclass_code, 0);
}
