/*
  Copyright (c) 2005 uim Project http://uim.freedesktop.org/

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


void
update_prop_list(property *prop, const char *str)
{
  if (prop->list != NULL) free(prop->list);

  prop->list = strdup(str);
  
  prop->list_update = 1;

}

void
update_prop_label(property *prop, const char *str)
{
  if (prop->label != NULL) free(prop->label);

  prop->label = strdup(str);

  prop->label_update = 1;

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


void
output_prop_list(property *prop, const char *im)
{
  char *buf;
  char *p[4];
  
  /* output new prop_list for Emacs */

  if (prop->list == NULL) {
	debug_printf(DEBUG_ERROR, "no prop_list\n");
	return;
  }


  if (im)
	a_printf(" ( l \"%s\"", im);
  else
	a_printf(" ( l \"\" ");

  buf = (char *)malloc(strlen(prop->list) + 1);
  strcpy(buf, prop->list);

  p[0] = buf;

  while (p[0] && *p[0]) { 

	p[1] = strchr(p[0], '\n');

	if (p[1]) 
	  *p[1] = '\0';
	else
	  break;

	/* p[0] always not equal NULL */
	if (strlen(p[0]) >= 6 && strncmp(p[0], "branch", 6) == 0) {
	  if ((p[2] = strchr(p[0], '\t')) && (p[3] = strchr(p[2] + 1, '\t'))) {
		*p[2] = *p[3] = '\0';
		a_printf(" ( \"%s\" \"%s\" ) ", p[2] + 1, p[3] + 1);
	  }
	}
	p[0] = p[1] + 1;
  }

  free(buf);


  a_printf(" ) ");

}


void
announce_prop_label_update(property *prop, const char *encoding)
{
  unsigned len;
  char *buf;

  if (prop->label == NULL) {
	debug_printf(DEBUG_ERROR, "no prop_label\n");
	return;
  }

#define PROP_LABEL_FORMAT "prop_label_update\ncharset=%s\n%s"

  len = strlen(PROP_LABEL_FORMAT) + strlen(encoding) 
	+ strlen(prop->label) + 1;
  
  buf = (char *)malloc(len);

  snprintf(buf, len, PROP_LABEL_FORMAT, encoding, prop->label);

  uim_helper_send_message(helper_fd, buf);


  free(buf);

#undef PROP_LABEL_FORMAT

}
