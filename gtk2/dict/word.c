/*
 *  $Id:$
 *  Copyright (c) 2003,2004 Masahito Omote <omote@utyuuzin.net>
 *                2005-2013 uim Project https://github.com/uim/uim
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

#include <stdlib.h>
#include <string.h>

#include "gettext.h"

#include "word.h"
#include "canna-cclass.h"

void word_append(uim_word **head, uim_word_type type,
		 char *charset,
		 char *phon, char *desc,
		 const char *cclass_code,
		 const char *cclass_native,
		 int freq,
		 int okuri, char *annotation)
{
    uim_word *entry, *pos;
    entry = malloc(sizeof(uim_word));
    if (entry != NULL) {
	/* If each arguments is NULL, allocate '\0' */
	entry->type = type;

	if (charset != NULL)
	    entry->charset = strdup(charset);
	else
	    entry->charset = strdup("");

	if (phon != NULL)
	    entry->phon = strdup(phon);
	else
	    entry->phon = strdup("");

	if (desc != NULL)
	    entry->desc = strdup(desc);
	else
	    entry->desc = strdup("");

	if (cclass_code != NULL) {
	    entry->cclass_code = strdup(cclass_code);
	} else
	    entry->cclass_code = strdup("");

	if (cclass_native != NULL) {
	    entry->cclass_native = strdup(cclass_native);
	} else
	    entry->cclass_native = strdup("");

	entry->freq = freq;

	/* SKK specific */
	entry->okuri = okuri;
	if (annotation != NULL)
	    entry->annotation = strdup(annotation);
	else
	    entry->annotation = strdup("");

	entry->next = NULL;

	if (*head == NULL) {
	    *head = entry;
	} else {
	    pos = word_last(*head);
	    pos->next = entry;
	}
    }
}

void word_free_list(uim_word *head) {
    uim_word *pos, *pos_prev;
    for (pos = head; pos != NULL; ) {
        free(pos->charset);
        free(pos->phon);
        free(pos->desc);
        free(pos->cclass_code);
        free(pos->cclass_native);
        free(pos->annotation);

	pos_prev = pos;
	pos = pos->next;
	free(pos_prev);
    }
}

uim_word *word_last(uim_word *list) {
    if (list != NULL) {
	while (list->next) {
	    list = list->next;
	}
    }
    return list;
}

uim_word_type
dict_identifier_to_word_type(char *identifier)
{
  uim_word_type type;

  if (!strcmp(identifier, N_("Anthy private dictionary")))
    type = WORD_TYPE_ANTHY;
  else if (!strcmp(identifier, N_("Canna private dictionary")))
    type = WORD_TYPE_CANNA;
  else
    type = WORD_TYPE_ANTHY; /* XXX */
    
  return type;
}

int
dict_identifier_to_support_type(char *identifier)
{
  int type;

  if (!strcmp(identifier, N_("Anthy private dictionary")))
    type = SUPPORT_ANTHY;
  else if (!strcmp(identifier, N_("Canna private dictionary")))
    type = SUPPORT_CANNA;
  else
    type = SUPPORT_ANTHY; /* XXX */
    
  return type;
}
