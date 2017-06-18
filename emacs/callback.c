
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

#include "callback.h"

void
commit_cb(void *ptr, const char *str)
{
  uim_agent_context *ua = (uim_agent_context *)ptr;

  debug_printf(DEBUG_NOTE, "commit_cb\n");

  ua->comstr = add_commit_string(ua->comstr, str);
}


void
preedit_clear_cb(void *ptr)
{
  uim_agent_context *ua = (uim_agent_context *)ptr;

  debug_printf(DEBUG_NOTE, "preedit_clear_cb\n");

  clear_preedit(ua->pe);
}


void
preedit_pushback_cb(void *ptr, int attr, const char * str)
{
  uim_agent_context *ua = (uim_agent_context *)ptr;

  debug_printf(DEBUG_NOTE,
			   "preedit_pushback_cb \"%s\" (attr: %d)\n", str, attr); 

  add_preedit(ua->pe, attr, str);
}


void
preedit_update_cb(void *ptr)
{
  uim_agent_context *ua = (uim_agent_context *)ptr;

  debug_printf(DEBUG_NOTE, "preedit_update_cb (ua = %p)\n", ua);

  /* do nothing */
}


void
candidate_activate_cb(void *ptr, int num, int limit)
{
  /* 
	 num:   total number of candidates
	 limit: candidates per page
  */
  uim_agent_context *ua = (uim_agent_context *)ptr;

  debug_printf(DEBUG_NOTE, 
			   "candidate_activate_cb (num=%d,limit=%d)\n", num, limit);

  new_candidate(ua->context, ua->cand, num, limit);
}



void
candidate_select_cb(void *ptr, int index)
{
  uim_agent_context *ua = (uim_agent_context *)ptr;

  debug_printf(DEBUG_NOTE, "candidate_select_cb (index: %d)\n", index);

#if !UIM_EL_USE_NEW_PAGE_HANDLING
  ua->cand->index = index;
#else
  select_candidate(ua->context, ua->cand, index);
#endif
}


void
candidate_shift_page_cb(void *ptr, int direction)
{
  uim_agent_context *ua = (uim_agent_context *)ptr;

  debug_printf(DEBUG_NOTE, "candidate_shift_page_cb\n");

  shift_candidate_page(ua->context, ua->cand, direction);
}


void
candidate_deactivate_cb(void *ptr)
{
  uim_agent_context *ua = (uim_agent_context *)ptr;

  debug_printf(DEBUG_NOTE, "candidate_deactivate_cb\n");

  clear_candidate(ua->cand);
}


void
prop_list_update_cb(void *ptr, const char *str)
{
  uim_agent_context *ua = (uim_agent_context *)ptr;

  debug_printf(DEBUG_NOTE, "prop_list_update_cb\n");

  update_prop_list(ua->prop, ua->encoding, str);
}


void
configuration_changed_cb(void *ptr)
{
  uim_agent_context *ua = (uim_agent_context *)ptr;

  /* configuration of context has changed at uim side */
  debug_printf(DEBUG_NOTE, "configuration_changed_cb\n");

  update_context_configuration(ua);
}


void
switch_app_global_im_cb(void *ptr, const char *name)
{
  /* change default */
  update_default_engine(name);

  switch_context_im_all(name);
}


void
switch_system_global_im_cb(void *ptr, const char *name)
{
  /* change default */
  update_default_engine(name);

  switch_context_im_all(name);

  helper_send_im_change_whole_desktop(name);
}

