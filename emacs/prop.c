/*
  Copyright (c) 2005-2013 uim Project https://github.com/uim/uim

  All rights reserved.

  Redistribution and use in source and binary forms, with or
  without modification, are permitted provided that the
  following conditions are met:

  1. Redistributions of source code must retain the above
     copyright notice, this list of conditions and the
     following disclaimer.
  2. Redistributions in binary form must reproduce the above
     copyright notice, this list of conditions and the
     following disclaimer in the documentation and/or other
     materials provided with the distribution.
  3. Neither the name of authors nor the names of its
     contributors may be used to endorse or promote products
     derived from this software without specific prior written
     permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
  CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
  INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
  MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
  NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
  OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include "prop.h"

property *
create_prop()
{
  property *prop;

  prop = uim_malloc(sizeof(property));

  prop->valid = 0;

  prop->list = NULL;
  prop->list_update = 0;

  return prop;
}

void
update_prop_list(property *prop, const char *encoding, const char *str)
{
  debug_printf(DEBUG_NOTE, "update_prop_list\n");

  prop->valid = 1;

  free(prop->list);

  prop->list = uim_strdup(str);
  
  prop->list_update = 1;
}


void
announce_prop_list_update(property *prop, const char *encoding)
{
  char *buf;

  if (prop->list == NULL) {
	debug_printf(DEBUG_ERROR, "no prop_list\n");
	return;
  }

#define PROP_LIST_FORMAT "prop_list_update\ncharset=%s\n%s"

  uim_asprintf(&buf, PROP_LIST_FORMAT, encoding, prop->list);

  helper_send_message(buf);
  free(buf);

#undef PROP_LIST_FORMAT
}


int
show_prop(property *prop)
{
  char *buf;
  char *head, *tail;
  char *p[6] = {0};
  char *indication_id = NULL, *iconic_label =NULL, *label_string = NULL;
  int check_leaf = 0;
  
  /* output new prop_list for Emacs */

  if (prop->list == NULL) {
	debug_printf(DEBUG_ERROR, "no prop_list\n");
	a_printf(" ( e ) ");
	return 0;
  }

  a_printf(" ( l ");

  head = buf = uim_strdup(prop->list);

#define PART_BRANCH "branch"
#define PART_LEAF   "leaf"
#define ACTION_ID_IMSW "action_imsw_"

  while (head && *head) { 

	/* 
	 * head: beginning of each line
	 * tail: end of each line 
	 * p[n]: token
	 */
	tail = strchr(head, '\n');

	if (tail)
	  *tail = '\0';
	else
	  break;

	/* head always not equal NULL */
	if (strlen(head) >= strlen(PART_BRANCH)
		&& strncmp(head, PART_BRANCH, strlen(PART_BRANCH)) == 0) {
	  if ((p[0] = strchr(head, '\t')) 
		  && (p[1] = strchr(p[0] + 1, '\t'))
		  && (p[2] = strchr(p[1] + 1, '\t'))) {
		*p[0] = *p[1] = *p[2] = '\0';
		indication_id = p[0] + 1;
		iconic_label = p[1] + 1;
		label_string = p[2] + 1;

		check_leaf = 1; /* check next leaf */
		/*a_printf(" ( \"%s\" \"%s\" \"%s\" ) ", p[0] + 1, p[1] + 1, p[2] + 1);*/
	  }
	} else if (strlen(head) >= strlen(PART_LEAF) 
			   && strncmp(head, PART_LEAF, strlen(PART_LEAF)) == 0) {
	  if (check_leaf && indication_id && iconic_label && label_string) {
		check_leaf = 0;
		/* im_switcher detection */
		if ((p[0] = strchr(head, '\t')) 
			&& (p[1] = strchr(p[0] + 1, '\t'))
			&& (p[2] = strchr(p[1] + 1, '\t'))
			&& (p[3] = strchr(p[2] + 1, '\t'))
			&& (p[4] = strchr(p[3] + 1, '\t'))
			&& (p[5] = strchr(p[4] + 1, '\t')))
		  *p[0] = *p[1] = *p[2] = *p[3] = *p[4] = *p[5] = '\0';

		  if (strlen(p[4] + 1) >= strlen(ACTION_ID_IMSW)
			  && strncmp(p[4] + 1, ACTION_ID_IMSW, strlen(ACTION_ID_IMSW)) == 0)
			a_printf(" ( \"im-name\" \"%s\" \"%s\" \"%s\" ) ", 
					 indication_id, iconic_label, label_string);
		  else
			a_printf(" ( \"im-mode\" \"%s\" \"%s\" \"%s\" ) ", 
					 indication_id, iconic_label, label_string);
	  }
	}
	head = tail + 1;
  }

  free(buf);

  a_printf(" ) ");

  return 1;

#undef PART_BRANCH
#undef PART_LEAF
#undef ACTION_ID_IMSW
}


