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

#include <stdlib.h>
#include <string.h>

#include "word.h"

void word_append(word **head, word_type type,
		 char *phon, char *desc,
		 char *cclass_code, int freq,
		 int okuri, char *annotation)
{
    word *entry, *pos;
    entry = malloc(sizeof(word));
    if(entry != NULL) {
	/* If each arguments is NULL, allocate '\0' */
	entry->type = type;

	if(phon != NULL)
	    entry->phon = strdup(phon);
	else
	    entry->phon = strdup("");

	if(desc != NULL)
	    entry->desc = strdup(desc);
	else
	    entry->desc = strdup("");

	if(cclass_code != NULL)
	    entry->cclass_code = strdup(cclass_code);
	else
	    entry->cclass_code = strdup("");

	entry->freq = freq;

	/* SKK specific */
	entry->okuri = okuri;
	if(annotation != NULL)
	    entry->annotation = strdup(annotation);
	else
	    entry->annotation = strdup("");

	entry->next = NULL;

	if(*head == NULL) {
	    *head = entry;
	} else {
	    pos = word_last(*head);
	    pos->next = entry;
	}
    }
}

void word_free_list(word *head) {
    word *pos, *pos_prev;
    for(pos = head; pos != NULL; ) {
	if(pos->phon != NULL)
	    free(pos->phon);
	if(pos->desc != NULL)
	    free(pos->desc);
	if(pos->cclass_code != NULL)
	    free(pos->cclass_code);
	if(pos->annotation != NULL)
	    free(pos->annotation);

	pos_prev = pos;
	pos = pos->next;
	free(pos_prev);
    }
}

word *word_last(word *list) {
    if(list != NULL) {
	while(list->next) {
	    list = list->next;
	}
    }
    return list;
}
