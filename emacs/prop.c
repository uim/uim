/*
  Copyright (c) 2005-2006 uim Project http://uim.freedesktop.org/

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

  prop = (property *)malloc(sizeof(property));

  prop->valid = 0;

  prop->list = NULL;
  prop->list_update = 0;

  return prop;
}

void
update_prop_list(property *prop, const char *str)
{

  prop->valid = 1;

  if (prop->list != NULL) free(prop->list);

  prop->list = strdup(str);
  
  debug_printf(DEBUG_NOTE, "prop->list: %s\n", prop->list);  

  prop->list_update = 1;

}


void
announce_prop_list_update(property *prop, const char *encoding)
{
  unsigned len;
  char *buf;

  if (prop->list == NULL) {
	debug_printf(DEBUG_ERROR, "no prop_list\n");
	return;
  }

#define PROP_LIST_FORMAT "prop_list_update\ncharset=%s\n%s"

  len = strlen(PROP_LIST_FORMAT) + strlen(encoding)
	+ strlen(prop->list) + 1;

  buf = (char *)malloc(len);

  snprintf(buf, len, PROP_LIST_FORMAT, encoding, prop->list);

  uim_helper_send_message(helper_fd, buf);

  free(buf);

#undef PROP_LIST_FORMAT
  
}


int
show_prop(property *prop)
{
  char *buf;
  char *head, *tail;
  char *p[4];
  
  /* output new prop_list for Emacs */

  if (prop->list == NULL) {
	debug_printf(DEBUG_ERROR, "no prop_list\n");
	a_printf(" ( e ) ");
	return 0;
  }

  a_printf(" ( l ");


  buf = (char *)malloc(strlen(prop->list) + 1);
  strcpy(buf, prop->list);

  head = buf;

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
	if (strlen(head) >= 6 && strncmp(head, "branch", 6) == 0) {
	  if ((p[0] = strchr(head, '\t')) 
		  && (p[1] = strchr(p[0] + 1, '\t'))
		  && (p[2] = strchr(p[1] + 1, '\t'))) {
		*p[0] = *p[1] = *p[2] = '\0';
		a_printf(" ( \"%s\" \"%s\" \"%s\" ) ", p[0] + 1, p[1] + 1, p[2] + 1);
	  }
	}
	head = tail + 1;
  }

  free(buf);

  a_printf(" ) ");

  return 1;

}


