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

#include "helper.h"

static void
helper_send_im_list(void)
{
  int nim, i;
  int buflen;
  char *buf;
  const char *current_im_name;
  uim_agent_context *ua;
  int dummy_agent_context = 0;

  debug_printf(DEBUG_NOTE, "helper_send_im_list\n");

  /* Use 1st context */
  if (agent_context_list_head) {
	ua = agent_context_list_head->agent_context;
  } else {
	dummy_agent_context = 1;
	ua = new_uim_agent_context(1, NULL);
  }

  nim = uim_get_nr_im(ua->context);

  uim_get_current_im_name(ua->context);
  current_im_name = uim_get_current_im_name(ua->context);

#define HEADER_FORMAT "im_list\ncharset=%s\n"

  buflen = uim_asprintf(&buf,  HEADER_FORMAT, ua->encoding);

#undef HEADER_FORMAT

  for (i = 0 ; i < nim; i++) {
	const char *name, *lang, *shortd;
	char *tmpbuf;

	name = uim_get_im_name(ua->context, i);
	lang = uim_get_im_language(ua->context, i);
	shortd = uim_get_im_short_desc(ua->context, i);

	debug_printf(DEBUG_NOTE, " [%d] = %s %s %s\n", i, name, lang, shortd);
	if (uim_asprintf(&tmpbuf, "%s\t%s\t%s\t%s\n",
					   name ? name : "" ,
					   lang ? lang : "" ,
					   shortd ? shortd : "" ,
					   strcmp(name,
							  (current_im_name == NULL ? "" : current_im_name))
					    == 0 ? "selected" : "") < 0 || tmpbuf == NULL)
		break;

	strlcat(buf, tmpbuf, buflen);
	free(tmpbuf);
  }

  helper_send_message(buf);

  debug_printf(DEBUG_NOTE, " im_list = \"%s\"\n", buf);

  free(buf);

  if (dummy_agent_context)
	release_uim_agent_context(1);

}



static int
helper_im_changed(char *request, char *engine_name)
{

  debug_printf(DEBUG_NOTE, "helper_im_changed: %s\n", engine_name);

  if (focused && strcmp(request, "im_change_this_text_area_only") == 0) {

    if (current)
	  switch_context_im(current, engine_name);

  } else if (strcmp(request, "im_change_whole_desktop") == 0 
			 || (focused && strcmp(request, "im_change_this_application_only") == 0)) {

	/* change default */
	update_default_engine(engine_name);

	/* check focus state when change IM of current application */
	if (strcmp(request, "im_change_whole_desktop") == 0 || current)
	  switch_context_im_all(engine_name);
  } else {

    return 0;
  }

  return 1;
}



/* handle messages from helper */
int
helper_handler(uim_agent_context *ua, char *helper_message)
{
  char *p, *message = helper_message;

  debug_printf(DEBUG_NOTE, "helper_handler\n");

  debug_printf(DEBUG_NOTE, " message \"%s\"\n", message);

  if (focused && strcmp("focus_in", message) == 0) {

	/* some other window is focused */

	debug_printf(DEBUG_NOTE, " focus_in\n"); 

	focused = 0;

  } else if (focused && strncmp("prop_activate", message, 13) == 0) {

	debug_printf(DEBUG_NOTE, " prop_activate\n"); 
	
	if (ua && ua->context != NULL) {
	  if ((p = strchr(message, ' ')) != NULL) {
		*p = '\0';
		p ++;
		debug_printf(DEBUG_NOTE, "  %s\n", p);
		uim_prop_activate(ua->context, p);
		} else {
		  debug_printf(DEBUG_WARNING, 
					   " invalid message(prop_activate): \"%s\"\n", message);
		}
	  }


  } else if (focused && strcmp("prop_list_get", message) == 0) { 

	debug_printf(DEBUG_NOTE, " prop_list_get\n");

	if (ua && ua->context != NULL)
	  uim_prop_list_update(ua->context);

  } else if (strncmp(message, "im_change_", 10) == 0) {

	char *engine;

	if ((p = strchr(message, ' ')) != NULL) {
	  *p = '\0';
	  engine = p + 1;
	  return helper_im_changed(message, engine);
	}

  } else if (focused && strcmp("im_list_get", message) == 0) {

		debug_printf(DEBUG_NOTE, " im_list_get\n");

		helper_send_im_list();
		
  } else if (focused && strncmp("commit_string", message, 13) == 0) {

	char *encoding, *str;

	debug_printf(DEBUG_NOTE, " commit_string\n");

	if (ua && ua->context != NULL) {

	  if ((p = strchr(message, ' ')) != NULL) {
		*p = '\0';
		encoding = p + 1;

		if ((p = strchr(encoding, ' ')) != NULL) {
		  *p = '\0';
		  str = helper_message_decode(p + 1);
		
		  if (uim_iconv->is_convertible(ua->encoding, encoding)) {
			char *comstr;
			void *cd = uim_iconv->create(ua->encoding, encoding);
			comstr = uim_iconv->convert(cd, str);

		  debug_printf(DEBUG_NOTE, 
						 " commit_string \"%s\"\n", comstr);

			commit_cb(ua, comstr);
			free(comstr);
		  }

		  free(str);

		}
	    }
	  }

  } else if (strncmp("prop_update_custom", message, 18) == 0) {

	  /* message from uim-pref-{gtk,qt} */

	  char *custom;
	  char *val;

	  uim_agent_context_list *ptr;

	if ((p = strchr(message, ' ')) != NULL) {
	  *p = '\0';
	  custom = p + 1;

	  if ((p = strchr(custom, ' ')) != NULL) {
		*p = '\0';
		val = p + 1;
		  for (ptr = agent_context_list_head; ptr != NULL; ptr = ptr->next) {
			uim_prop_update_custom(ptr->agent_context->context, custom, val);
		  }
		}
	  }
  } else if (strcmp("custom_reload_notify", message) == 0) {

	debug_printf(DEBUG_NOTE, "reload\n");

	uim_prop_reload_configs();

	} else {

	  debug_printf(DEBUG_WARNING, " undefined helper message: %s\n", message);
	  return 0;

	}

  return 1;
}




void
helper_send_im_change_whole_desktop(const char *name)
{
  char *buf;

#define HEADER_FORMAT "im_change_whole_desktop\n%s\n"

  uim_asprintf(&buf, HEADER_FORMAT, name ? name : "");

  helper_send_message(buf);

  free(buf);

#undef HEADER_FORMAT
 }
