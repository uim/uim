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

#include "context.h"

/* current focused context */
uim_agent_context *current;

/* global foscu */
int focused = 0;

uim_agent_context_list *agent_context_list_head = NULL;
uim_agent_context_list *agent_context_list_tail = NULL;

static void
update_context_im(uim_agent_context *ua)
{
  debug_printf(DEBUG_NOTE, "update_context_im\n");

  uim_switch_im(ua->context, ua->im);

  if (ua->im && strcmp(ua->im, uim_get_current_im_name(ua->context)) != 0) {
	debug_printf(DEBUG_ERROR,
				 "update_context_im: failed to switch IM to %s\n", ua->im);
	free(ua->im);
	ua->im = strdup(uim_get_current_im_name(ua->context));
  }
}

static void
update_context_encoding(uim_agent_context *ua)
{

  debug_printf(DEBUG_NOTE, "update_context_encoding\n");

  /* discard current context */
  clear_candidate(ua->cand);
  clear_preedit(ua->pe);
  
  uim_set_client_encoding(ua->context, ua->encoding);
}


/* search context */
uim_agent_context *
get_uim_agent_context(int id)
{
  uim_agent_context_list *ptr;

  debug_printf(DEBUG_NOTE, "get_uim_agent_context (%d)\n", id);
  
  for (ptr = agent_context_list_head; ptr != NULL; ptr = ptr->next) {
	if (ptr->agent_context->context_id == id)
	  return ptr->agent_context;
  }

  return NULL;
}



uim_agent_context *
switch_context_im(uim_agent_context *ua, const char *im)
{
  const char *encoding;

  debug_printf(DEBUG_NOTE, "switch_context_im\n");

  encoding = get_im_encoding(im);

  /* update IM name */
  free(ua->im);

  if (im)
	ua->im = uim_strdup(im);
  else
	ua->im = NULL;

  if (strcmp(ua->encoding, encoding) == 0) {
	/* encodings are same */

	debug_printf(DEBUG_NOTE, "same encoding %s %s\n", ua->im, im);

	update_context_im(ua);

	uim_prop_list_update(ua->context);

  } else {
	/* encodings are different */

	debug_printf(DEBUG_NOTE, 
				 "different encoding %s %s\n", ua->encoding, encoding);

	free(ua->encoding);
	ua->encoding = uim_strdup(encoding);

	update_context_encoding(ua);
	update_context_im(ua);
  }

  uim_prop_list_update(ua->context);

  return ua;

}


void
switch_context_im_all(const char *im)
{
  char *quot_im_name;
  uim_agent_context_list *ptr;

  /* change default IM  */
  update_default_engine(im);

  /* check focus state when change IM of current application */
  quot_im_name = uim_malloc(strlen(im) + 2);
  quot_im_name[0] = '\'';
  quot_im_name[1] = '\0';
  strcat(quot_im_name, im);

  if (agent_context_list_head)
	/* update default IM name in libuim? should be called only one time? */
	uim_prop_update_custom(agent_context_list_head->agent_context->context,
						   "custom-preserved-default-im-name",
						   quot_im_name);

  for (ptr = agent_context_list_head; ptr != NULL; ptr = ptr->next) {
	switch_context_im(ptr->agent_context, im);
  }

  free(quot_im_name);
}



uim_context
create_context(const char *encoding, uim_agent_context *ptr)
{
  uim_context context;

  context = uim_create_context(ptr,
							   encoding,
							   NULL,
							   NULL,  /*default_engine_name,*/
							   uim_iconv,
							   commit_cb);

  uim_set_preedit_cb(context,
					 preedit_clear_cb,
					 preedit_pushback_cb,
					 preedit_update_cb);

  uim_set_candidate_selector_cb(context,
								candidate_activate_cb,
								candidate_select_cb,
								candidate_shift_page_cb,
								candidate_deactivate_cb);

  uim_set_prop_list_update_cb(context,
							  prop_list_update_cb);


  uim_set_configuration_changed_cb(context,
								   configuration_changed_cb);

  uim_set_im_switch_request_cb(context,
							   switch_app_global_im_cb,
							   switch_system_global_im_cb);

  
  return context;

}


uim_agent_context *
create_uim_agent_context(const char *encoding)
{

  uim_agent_context *ret;
  const char *im;

  debug_printf(DEBUG_NOTE, "create_uim_agent_context\n");

  ret = uim_malloc(sizeof(uim_agent_context));

  if (encoding) {
	ret->encoding = uim_strdup(encoding);
  } else {
	if (debug_level > 0)
	  ret->encoding = uim_strdup("EUC-JP");
	else
	  ret->encoding = uim_strdup("UTF-8");
  }

  ret->context = create_context(ret->encoding, ret);

  if ((im = uim_get_default_im_name(setlocale(LC_CTYPE, NULL))))
	ret->im = uim_strdup(im);
  else
	ret->im = NULL;

  ret->pe = create_preedit();
  ret->cand = create_candidate();
  ret->prop = create_prop();

  ret->comstr = (char *)NULL;

  return ret;
}




/* add context to context list */
uim_agent_context *
new_uim_agent_context(int id, const char *encoding)
{
  uim_agent_context_list *ptr;
  
  debug_printf(DEBUG_NOTE, "add_uim_agent_context(%d)\n", id);

  ptr = uim_malloc(sizeof(uim_agent_context_list));

  ptr->agent_context = create_uim_agent_context(encoding);
  ptr->next = NULL;
  ptr->prev = NULL;

  ptr->agent_context->context_id = id;

  if (agent_context_list_tail != NULL) {
	agent_context_list_tail->next = ptr;
	ptr->prev = agent_context_list_tail;
  }

  agent_context_list_tail = ptr;

  if (agent_context_list_head == NULL)
	agent_context_list_head = ptr;

  return ptr->agent_context;
}


/* release context from context list */
int
release_uim_agent_context(int context_id)
{
  uim_agent_context_list *ptr;
  
  for (ptr = agent_context_list_head; ptr != NULL; ptr = ptr->next) {
	if (ptr->agent_context->context_id == context_id) {

	  uim_agent_context *ua = ptr->agent_context;

	  /* clear current */
	  if (current == ua)
		clear_current_uim_agent_context();
	  
	  /* release */
	  uim_release_context(ua->context);

	  /* clear candidate */
	  clear_candidate(ua->cand);
	  free(ua->cand);

	  /* clear preedit */
	  clear_preedit(ua->pe);
	  free(ua->pe);

	  /* free others */
	  free(ua->encoding);
	  free(ua->im);
	  free(ua->prop->list);
	  free(ua->prop);
	  free(ua->comstr);

	  /* rebuild list */
	  if (ptr->next != NULL)
		ptr->next->prev = ptr->prev;
	  else
		agent_context_list_tail = ptr->prev;

	  if (ptr->prev != NULL)
		ptr->prev->next = ptr->next;
	  else
		agent_context_list_head = ptr->next;

	  free(ua);
	  free(ptr);

	  return context_id;
	}
  }

  return -1;
}





int
set_current_uim_agent_context(uim_agent_context *ua)
{
  debug_printf(DEBUG_NOTE, "set_current_context\n");

  if (ua == NULL || ua->context == NULL) {
	debug_printf(DEBUG_ERROR, "set_current_context: invalid context\n");
	return -1;
  }

  helper_send_message("focus_in\n");

  current = ua;
  focused = 1;

  uim_prop_list_update(ua->context);

  return ua->context_id;
}


int
clear_current_uim_agent_context(void)
{

  debug_printf(DEBUG_NOTE, "unfocused\n");

  if (current == NULL || current->context == NULL) return -1;

  /*focused = 0;*/

  helper_send_message("focus_out\n");

  debug_printf(DEBUG_NOTE, " focused %d\n", focused);
  
  return current->context_id;
}



/* handle configuration change */
void
update_context_configuration(uim_agent_context *ua)
{

  /* configuration of context has changed at uim side */
  debug_printf(DEBUG_NOTE, "update_context_configuration\n");
  
  /* update IM name */
  free(ua->im);
  ua->im = uim_strdup(uim_get_current_im_name(ua->context));

  debug_printf(DEBUG_NOTE, "ua->im %s\n", ua->im);

  free(ua->encoding);
  ua->encoding = uim_strdup(get_im_encoding(ua->im));

  debug_printf(DEBUG_NOTE, "ua->encoding %s\n", ua->encoding);

  /* switch IM again */
  update_context_encoding(ua);
}


int
show_commit_string_uim_agent_context(uim_agent_context *ua)
{
  int ret;
  if (ua == NULL) {
	return -1;
  } else {
	ret = show_commit_string(ua->comstr);
	if (ret > 0) {
	  reset_commit_string(ua->comstr);
	  ua->comstr = NULL;
	}
	return ret;
  }
}

int
show_preedit_uim_agent_context(uim_agent_context *ua)
{
  if (ua && ua->cand->valid)
	return show_preedit_force(ua->pe);
  else if (ua == NULL || !ua->pe->valid) 
	return -1;
  else
	return show_preedit(ua->pe);
}


int
show_candidate_uim_agent_context(uim_agent_context *ua)
{
  if (ua == NULL || !ua->cand->valid)
	return -1;
  else if (focused)
	return show_candidate(ua->cand);
  else
	return 0;
}


int
show_prop_uim_agent_context(uim_agent_context *ua)
{
  if (ua == NULL || !ua->prop->valid)
	return -1;
  else 
	return show_prop(ua->prop);
}


int
show_im_uim_agent_context(uim_agent_context *ua)
{
  if (ua == NULL)
	return -1;
  else
	return show_im(ua->im);
}

