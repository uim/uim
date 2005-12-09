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

#include "context.h"

/* current focused context */
uim_agent_context *current;

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
  clear_candidate(ua->pe->cand);
  clear_preedit(ua->pe);
  uim_release_context(ua->context);
  
  ua->context = create_context(ua->encoding, ua);

  update_context_im(ua);
  
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
switch_context_im(uim_agent_context *ua, const char *im, const char *encoding)
{
  debug_printf(DEBUG_NOTE, "switch_context_im\n");

  if (encoding == NULL) {
	/* unspported engine or not ready for use from Emacs  */
	debug_printf(DEBUG_WARNING,
				 "switch_context_im: encoding of %s is NULL\n", im);
	return ua;
  }

  if (ua->im) free(ua->im);

  if (im)
	ua->im = strdup(im);
  else
	ua->im = NULL;

  if (strcmp(ua->encoding, encoding) == 0) {
	/* encodings are same */

	debug_printf(DEBUG_NOTE,
				 "same encoding %s %s\n", ua->im, im);

	update_context_im(ua);

	uim_prop_label_update(ua->context);
	uim_prop_list_update(ua->context);

  } else {
	/* encodings are different */

	debug_printf(DEBUG_NOTE, 
				 "different encoding %s %s\n", ua->encoding, encoding);

	if (ua->encoding) free(ua->encoding);
	ua->encoding = strdup(encoding);

	update_context_encoding(ua);

  }

  uim_prop_label_update(ua->context);
  uim_prop_list_update(ua->context);

  return ua;

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

  uim_set_prop_label_update_cb(context,
							   prop_label_update_cb);

  uim_set_configuration_changed_cb(context,
								   configuration_changed_cb);
  
  return context;

}


uim_agent_context *
create_uim_agent_context(const char *encoding)
{

  uim_agent_context *ret;
  const char *im;

  debug_printf(DEBUG_NOTE, "create_uim_agent_context\n");

  ret = (uim_agent_context *)malloc(sizeof(uim_agent_context));

  if (encoding) {
	ret->encoding = strdup(encoding);
  } else {
	if (debug_level > 0)
	  ret->encoding = strdup("EUC-JP");
	else
	  ret->encoding = strdup("UTF-8");
  }

  ret->context = create_context(ret->encoding, ret);

  if ((im = uim_get_default_im_name(setlocale(LC_ALL, NULL))))
	ret->im = strdup(im);
  else
	ret->im = NULL;

  ret->pe = (preedit *)malloc(sizeof(preedit));

  ret->pe->head = ret->pe->tail = NULL;

  ret->pe->cand = (candidate_info *)malloc(sizeof(candidate_info));
  ret->pe->cand->valid = 0;

  ret->prop = (property *)malloc(sizeof(property));
  ret->prop->list = NULL;
  ret->prop->label = NULL;
  ret->prop->list_update = 0;
  ret->prop->label_update = 0;

  uim_prop_list_update(ret->context);

  return ret;
}




/* add context to context list */
uim_agent_context *
new_uim_agent_context(int id, const char *encoding)
{
  uim_agent_context_list *ptr;
  
  debug_printf(DEBUG_NOTE, "add_uim_agent_context(%d)\n", id);

  ptr = (uim_agent_context_list *)malloc(sizeof(uim_agent_context_list));

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
release_uim_agent_context(int id)
{
  uim_agent_context_list *ptr;
  
  for (ptr = agent_context_list_head; ptr != NULL; ptr = ptr->next) {
	if (ptr->agent_context->context_id == id) {

	  preedit_buffer *p, *ptmp;

	  if (current == ptr->agent_context)
		current = NULL;
	  
	  /* release */
	  uim_release_context(ptr->agent_context->context);

	  clear_candidate(ptr->agent_context->pe->cand);
	  free(ptr->agent_context->pe->cand);

	  p = ptr->agent_context->pe->head; 

	  while (p) {
	  	ptmp = p;
		p = p->next;
		if (ptmp->str) free(ptmp->str);
		free (ptmp);
	  }

	  free(ptr->agent_context->encoding);

	  /* rebuild list */
	  if (ptr->next != NULL)
		ptr->next->prev = ptr->prev;
	  else
		agent_context_list_tail = ptr->prev;

	  if (ptr->prev != NULL)
		ptr->prev->next = ptr->next;
	  else
		agent_context_list_head = ptr->next;

	  free(ptr);

	  return id;
	}
  }

  return -1;
}



/* handle configuration change */
void
update_context_configuration(uim_agent_context *ua)
{

  /* configuration of context has changed at uim side */
  debug_printf(DEBUG_NOTE, "update_context_configuration\n");
  
  /* update IM name */
  if (ua->im) free(ua->im);
  ua->im = strdup(uim_get_current_im_name(ua->context));

  debug_printf(DEBUG_NOTE, "ua->im %s\n", ua->im);

  if (ua->encoding) free(ua->encoding);
  ua->encoding = strdup(get_im_encoding(ua->im));

  debug_printf(DEBUG_NOTE, "ua->encoding %s\n", ua->encoding);

  /* switch IM again to update encoding for Emacs...orz */
  update_context_encoding(ua);

}
