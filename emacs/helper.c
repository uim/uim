/*
  Copyright (c) 2005, Konosuke Watanabe <nosuke@csc.ne.jp>
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

#include "helper.h"

void
helper_send_im_list(void)
{
  int nim, i;
  int len = 0, buflen;
  char *buf;
  const char *current_im_name;

  debug_printf(DEBUG_NOTE, "helper_send_im_list\n");

  if (current == NULL || current->context == NULL) return;

  nim = uim_get_nr_im(current->context);

  uim_get_current_im_name(current->context);
  current_im_name = uim_get_current_im_name(current->context);

#define HEADER_FORMAT "im_list\ncharset=%s\n"

  len += strlen(HEADER_FORMAT) + strlen(current->encoding);
  len += strlen("selected") ;

  for (i = 0; i < uim_get_nr_im(current->context); i++) {
	const char *name, *lang, *shortd;
	name = uim_get_im_name(current->context, i);
	lang = uim_get_im_language(current->context, i);
	shortd = uim_get_im_short_desc(current->context, i);

	len += name ? strlen(name) : 0;
	len += lang ? strlen(lang) : 0;
	len += shortd ? strlen(shortd) : 0;
	len += strlen( "\t\t\t\n");
  }

  len++ ;

  buf = (char *)malloc(sizeof(char) * len);

  buflen = snprintf(buf, len, HEADER_FORMAT, current->encoding);


  for (i = 0 ; i < uim_get_nr_im(current->context); i++) {
	const char *name, *lang, *shortd;	
	name = uim_get_im_name(current->context, i);
	lang = uim_get_im_language(current->context, i);
	shortd = uim_get_im_short_desc(current->context, i);

	debug_printf(DEBUG_NOTE, " [%d] = %s %s %s\n", i, name, lang, shortd);

	buflen += snprintf(buf + buflen, len - buflen,
					   "%s\t%s\t%s\t%s\n",
					   name ? name : "" ,
					   lang ? lang : "" ,
					   shortd ? shortd : "" ,
					   strcmp(name, 
							  (current_im_name == NULL ? "" : current_im_name))
					   == 0 ? "selected" : "");
  }

  uim_helper_send_message(helper_fd, buf);

  debug_printf(DEBUG_NOTE, " im_list = \"%s\"\n", buf);
  
  free(buf);
}



void
helper_im_changed(char *request, char *engine_name)
{
  uim_agent_context_list *ptr;

  debug_printf(DEBUG_NOTE, "helper_im_changed: %s\n", engine_name);

  if (strcmp(request, "im_change_this_text_area_only") == 0) {

    if (current) {
      switch_context_im(current, engine_name, get_im_encoding(engine_name));
	  if(current->im) free(current->im);
	  current->im = strdup(engine_name);
      uim_prop_label_update(current->context);
      uim_prop_list_update(current->context);
    }

  } else if (strcmp(request, "im_change_whole_desktop") == 0 
			 || strcmp(request, "im_change_this_application_only") == 0) {

	char *quot_engine_name;

	/* change default */
	update_default_engine(engine_name);
	  /* if (default_engine_name) free(default_engine_name);
		 default_engine_name = strdup(engine_name); */

	/* check focus state when change IM of current application */
	if (strcmp(request, "im_change_whole_desktop") == 0 || current) {
	  quot_engine_name = (char *)malloc(strlen(engine_name) + 2);
	  quot_engine_name[0] = '\'';
	  quot_engine_name[1] = '\0';
	  strcat(quot_engine_name, engine_name);
	  for (ptr = agent_context_list_head; ptr != NULL; ptr = ptr->next) {

		switch_context_im(ptr->agent_context, 
						  engine_name, get_im_encoding(engine_name));

		uim_prop_update_custom(ptr->agent_context->context,
							   "custom-preserved-default-im-name",
							   quot_engine_name);

		if (ptr->agent_context->im) free(ptr->agent_context->im);
		ptr->agent_context->im = strdup(engine_name);
		
		if (current && ptr->agent_context == current) {
		  uim_prop_label_update(ptr->agent_context->context);
		  uim_prop_list_update(ptr->agent_context->context);
		}
	  }
	  free(quot_engine_name);
    }
  }
}



/* handle messages from helper */
void
helper_handler(void)
{
  char *message;

  uim_helper_read_proc(helper_fd);

  debug_printf(DEBUG_NOTE, "helper_handler\n");

  while ((message = uim_helper_get_message())) {

    char *line = message;
    char *eol;

	int current_exist = (current != NULL && current->context != NULL);

    if ((eol = strchr(line, '\n')) != NULL) {
	  *eol = '\0';
    } else {
	  free(message);
	  break;
	}

	debug_printf(DEBUG_NOTE, " message \"%s\"\n", message);

	if (strcmp("prop_list_get", line) == 0) { 
	  /* for current context */

	  debug_printf(DEBUG_NOTE, "prop_list_get\n");
	  if (current_exist)
		uim_prop_list_update(current->context);
	  else
		debug_printf(DEBUG_NOTE, " ignored helper message: %s\n", message);
		
	} else if (strcmp("prop_label_get", line) == 0) { 
	  /* for current context */

	  if (current_exist)
		uim_prop_label_update(current->context);
	  else
		debug_printf(DEBUG_NOTE, " ignored helper message: %s\n", message);

	} else if (strcmp("prop_activate", line) == 0) { 
	  /* for current context */

	  if (current_exist) {
		line = eol + 1;
		if ((eol = strchr(line, '\n')) != NULL) {
		  *eol = '\0';
		  uim_prop_activate(current->context, line);
		} else {
		  debug_printf(DEBUG_WARNING, 
					   " invalid message(prop_activate): \"%s\"\n", message);
		}
	  } else {
		debug_printf(DEBUG_NOTE, " ignored helper message: %s\n", message);
	  }

	} else if (strcmp("im_list_get", line) == 0) {
	  /* for current context */

	  if (current_exist) {
		debug_printf(DEBUG_NOTE, " im_list_get\n");
		helper_send_im_list();
	  } else {
		debug_printf(DEBUG_NOTE, " ignored helper message: %s\n", message);
	  }
		
	} else if (strncmp("focus_in", line, 8) == 0) {

	  if (current_exist) {
		debug_printf(DEBUG_NOTE, " focus_in\n"); 
		if (current) unfocused();
	  } else {
		debug_printf(DEBUG_NOTE, " ignored helper message: %s\n", message);
	  }

	} else if (strcmp("commit_string", line) == 0) { /* for current context */

	  if (current_exist) {
		line = eol + 1;
		if ((eol = strchr(line, '\n')) != NULL) {
		  *eol = '\0';
		  /* do nothing */
		  debug_printf(DEBUG_NOTE, 
					   " commit_string \"%s\"\n", line);
		} else {
		  debug_printf(DEBUG_WARNING, 
					   " invalid message(commit_string) \"%s\"\n", message);
		}
	  } else {
		debug_printf(DEBUG_NOTE, " ignored helper message: %s\n", message);
	  }

	} else if (strncmp(line, "im_change_", 10) == 0) {

	  char *engine;

	  engine = eol + 1;
	  if ((eol = strchr(engine, '\n')) != NULL) {
		*eol = '\0';
		helper_im_changed(line, engine);
	  } else {
		debug_printf(DEBUG_WARNING,
					 " invalid message(im_change) \"%s\"\n", message);
	  }

	} else if (strcmp("prop_update_custom", line) == 0) {

	  /* message from uim-pref-{gtk,qt} */
	  char *custom;
	  char *val;

	  uim_agent_context_list *ptr;

	  line = eol + 1;
	  custom = line;

	  if ((eol = strchr(line, '\n')) != NULL) {
		*eol = '\0';
		val = eol + 1;

		if ((eol = strchr(val, '\n')) != NULL) {
		  *eol = '\0';
		  for (ptr = agent_context_list_head; ptr != NULL; ptr = ptr->next) {
			uim_prop_update_custom(ptr->agent_context->context, custom, val);
		  }
		} else {
		  debug_printf(DEBUG_WARNING,
					   " invalid message val of prop_update_custom: \"%s\"\n",
					   val);
		}

	  } else {
		debug_printf(DEBUG_WARNING,
					 " invalid message: ",
					 "custom of prop_update_custom: \"%s\"\n",
					 line);
	  }

	} else if (strcmp("prop_list_update", line) == 0) {
	  /* ignore */
	  debug_printf(DEBUG_NOTE, " prop_list_update (helper)\n");
	} else if ( strcmp("prop_label_update", line) == 0) {
	  /* ignore */
	  debug_printf(DEBUG_NOTE, " prop_label_update (helper)\n");
	} else {
	  debug_printf(DEBUG_WARNING, " undefined helper message: %s\n", message);
	}

	free(message);
  }
}


/* focus in to a buffer */
int
focused(uim_agent_context *ua)
{
  debug_printf(DEBUG_NOTE, "focused\n");

  if (ua == NULL || ua->context == NULL) return -1;

  uim_helper_client_focus_in(ua->context);

  current = ua;

  uim_prop_label_update(ua->context);
  uim_prop_list_update(ua->context);

  return ua->context_id;
}


/* focus out from current buffer */
int
unfocused(void)
{
  int ret;

  debug_printf(DEBUG_NOTE, "unfocused\n");

  if (current == NULL || current->context == NULL) return -1;

  ret = current->context_id;
  uim_helper_client_focus_out(current->context);

  current = NULL;

  return ret;
}

