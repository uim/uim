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
 
#include "cannadic.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <glib.h>

void cannadic_parse_line(unsigned char *buf, word **head) {
    /*
     * cannadic format:
     *
     * phon pos desc (desc desc ... ) (pos desc (desc ...) ....)
     *
     * phon  : must
     * pos   : at least 1 entry
     * desc  : at least 1 entry
     *
     * pos and desc must be associated with each other.
     *
     */
    unsigned char *pos = NULL, *phonetic = NULL, *literal = NULL;
    unsigned char *cur = NULL, *cur1 = NULL;
    unsigned char *tmp1 = NULL, *tmp2 = NULL;
    int freq = 1;

    if(buf == NULL) {
	*head = NULL;
	return;
    }

    if(buf[strlen(buf) - 1] == '\n') buf[strlen(buf) - 1] = '\0';

    cur = strchr(buf, ' ');
    if(cur != NULL) {
	*cur = '\0';
	cur++;
    }
    phonetic = strdup(buf);

    while(1) {
	cur1 = strchr(cur, ' ');
	if(cur1 != NULL) {
	    *cur1 = '\0';
	}

	if(cur[0] == '#') {
	    freq = 1;
	    tmp1 = strdup(cur);
	    tmp2 = strchr(tmp1, '*');
	    if(tmp2 != NULL) {
		*tmp2 = '\0';
		tmp2++;
		freq = atoi(tmp2);
	    }
	    pos = tmp1;
	} else {
	    literal = cur;
	    word_append(head, WORD_TYPE_CANNA,
			phonetic, literal, pos,
			    0, 0, NULL);
	}
	if(cur1 == NULL) break;
	
	cur = cur1;
	cur++;
    }
}

GList *cannadic_parse_line_glist(unsigned char *buf, GList *list) {
    /*
     * cannadic format:
     *
     * phon pos desc (desc desc ... ) (pos desc (desc ...) ....)
     *
     * phon  : must
     * pos   : at least 1 entry
     * desc  : at least 1 entry
     *
     * pos and desc must be associated with each other.
     *
     * XXX: pos is a part of speech in this document. But pos uses as cursor
     *      in this code.
     */
    unsigned char *pos = NULL, *phonetic = NULL, *literal = NULL;
    unsigned char *cur = NULL, *cur1 = NULL;
    unsigned char *tmp1 = NULL, *tmp2 = NULL;
    gchar *utf8_literal, *utf8_phonetic;
    gchar score[1024], cclass[1024];
    gint rbytes, wbytes;
    int freq = 1;
#if 0
    if(buf == NULL)
	return NULL;

    if(buf[strlen(buf) - 1] == '\n') buf[strlen(buf) - 1] = '\0';

    cur = strchr(buf, ' ');
    if(cur != NULL) {
	*cur = '\0';
	cur++;
    }
    phonetic = strdup(buf);

    while(1) {
	cur1 = strchr(cur, ' ');
	if(cur1 != NULL) {
	    *cur1 = '\0';
	}

	if(cur[0] == '#') {
	    freq = 1;
	    tmp1 = strdup(cur);
	    tmp2 = strchr(tmp1, '*');
	    if(tmp2 != NULL) {
		*tmp2 = '\0';
		tmp2++;
		freq = atoi(tmp2);
	    }
	    pos = tmp1;
	} else {
	    GError *err1, *err2;
	    literal = cur;
	    utf8_phonetic = g_convert(phonetic, -1, "UTF-8", "EUC-JP", &rbytes, &wbytes, &err1);
	    utf8_literal = g_convert(literal, -1, "UTF-8", "EUC-JP", &rbytes, &wbytes, &err2);
	    snprintf(score, sizeof(score), "%d", freq);
	    cclass[0] = '\0';

	    if(err1->code != G_CONVERT_ERROR_FAILED && err2->code != G_CONVERT_ERROR_FAILED)
		list = dixchange_append_word(list, literal, phonetic, pos, cclass, score);

	    if(utf8_phonetic != NULL)
		g_free(utf8_phonetic);
	    if(utf8_literal != NULL)
		g_free(utf8_literal);
	    if(tmp1 != NULL)
		g_free(tmp1);
	}
	if(cur1 == NULL) break;
	cur = cur1;
	cur++;
    }
#endif
    return list;
}

int cannadic_import(const char* filename, int imtype)
{
    FILE *fp;
    char buf[1024];
    int len;
    word *list = NULL, *pos = NULL;
	
    if(filename != NULL && filename[0] != '\0') {
	fp = fopen(filename, "r");
	if(!fp) {
	    return -1;
	}
	while(fgets(buf, sizeof(buf), fp)) {
	    len = strlen(buf);
	    buf[len-1] = '\0';
	    if(buf[0] != '#') {
		cannadic_parse_line(buf, &list);
	    }
	}
	for(pos = list; pos != NULL; pos = pos->next) {
	    switch (imtype) {
	    case 0: /* Anthy */
		add_anthy_priv_dic_with_flags(pos->phon,
					      pos->desc,
					      pos->cclass_code, 1);
		break;
	    case 1: /* Canna */
	      /*
		snprintf(buf, sizeof(buf), "%s %s %s", pos->phon, pos->cclass_code, pos->desc);
		canna_define_dic("user", buf);*/
		break;
	    case 2: /* PRIME */
	    case 3: /* SKK */
	    default:
		break;
	    }
	}
	fclose(fp);
    }
}

char *find_pos_from_code(const char *code, int type) {
    /* need to be more smart */
    int i = 0, j = 0;
    char *pos = NULL;
    category_code *category[] = {
	substantive_code,
	verb_code,
	adjective_code,
	adverb_code,
	etc_code,
	NULL,
    };
    int num[] = { /* XXX */
	sizeof(substantive_code) / sizeof(substantive_code[0]),
	sizeof(verb_code) / sizeof(verb_code[0]),
	sizeof(adjective_code) / sizeof(adjective_code[0]),
	sizeof(adverb_code) / sizeof(adverb_code[0]),
	sizeof(etc_code) / sizeof(etc_code[0]),
	0,
    };

    do {
	for(j = 0; j < num[i]; j++) { /* XXX */
	    if(strcmp(code, (category[i])[j].code) == 0
	       && (category[i])[j].type & type == type)
		pos = strdup((category[i])[j].desc);
	}
	i++;
    } while(category[i] != NULL);

    return pos;
}

char *find_code_from_pos(const char *pos, int type) {
    /* need to be more smart */
    int i = 0, j = 0;
    char *code = NULL;
    category_code *category[] = {
	substantive_code,
	verb_code,
	adjective_code,
	adverb_code,
	etc_code,
	NULL,
    };
    int num[] = { /* XXX */
	sizeof(substantive_code) / sizeof(substantive_code[0]),
	sizeof(verb_code) / sizeof(verb_code[0]),
	sizeof(adjective_code) / sizeof(adjective_code[0]),
	sizeof(adverb_code) / sizeof(adverb_code[0]),
	sizeof(etc_code) / sizeof(etc_code[0]),
	0,
    };

    do {
	for(j = 0; j < num[i]; j++) { /* XXX */
	    if(strcmp(pos, (category[i])[j].desc) == 0
	       && (category[i])[j].type & type == type)
		code = strdup((category[i])[j].code);
	}
	i++;
    } while(category[i] != NULL);

    return code;
}

int cannadic_export(const char *filename, int type)
{
#if 0
    GList *list, *pos;
    FILE *fp;

    switch(type) {
    case 0: /* Anthy */
	list = anthy_read_privatediclist();
	break;
    case 1: /* Canna */
	list = canna_read_privatediclist();
	break;
    default:
	break;
    }

    fp = fopen(filename, "w");
    if(fp == NULL)
	return -1;

    for(pos = g_list_first(list); pos != NULL; pos = g_list_next(pos))
    {
	char line[1024];
	char *euc_line;
	int score, rbytes, wbytes;
	dixchange_word *word;

	word = pos->data;
	if(word->score != 1)
	{
	    snprintf(line, sizeof(line), "%s %s*%s %s",
		     word->phonetic, word->pos, word->score, word->literal);
	} else {
	    snprintf(line, sizeof(line), "%s %s %s",
		     word->phonetic, word->pos, word->literal);
	}
	euc_line = g_convert(line, -1, "EUC-JP", "UTF-8", &rbytes, &wbytes, NULL);
	fprintf(fp, "%s\n", euc_line);
	free(euc_line);
    }
    fclose(fp);

    return 0;
#endif
    return -1;
}
