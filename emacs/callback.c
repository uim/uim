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

#include "callback.h"

void
commit_cb(void *ptr, const char *str)
{
  debug_printf(DEBUG_NOTE, "commit_cb\n");

  do_commit(str);
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

  new_candidate(ua->context, ua->pe->cand, num, limit);
}



void
candidate_select_cb(void *ptr, int index)
{
  uim_agent_context *ua = (uim_agent_context *)ptr;

  debug_printf(DEBUG_NOTE, "candidate_select_cb (index: %d)\n", index);

  ua->pe->cand->index = index;
}


void
candidate_shift_page_cb(void *ptr, int direction)
{
  uim_agent_context *ua = (uim_agent_context *)ptr;

  debug_printf(DEBUG_NOTE, "candidate_shift_page_cb\n");

  shift_candidate_page(ua->context, ua->pe->cand, direction);
}


void
candidate_deactivate_cb(void *ptr)
{
  uim_agent_context *ua = (uim_agent_context *)ptr;

  debug_printf(DEBUG_NOTE, "candidate_deactivate_cb\n");

  clear_candidate(ua->pe->cand);
}


/*
 *
 *  This function is called after key event... maybe...
 *     If it's true, "ptr" is equal to "current" and 
 *    S-expression with new prop_label should be outputted at end
 *    of key processing.
 *
 *  This function is called when IM mode has changed... maybe...
 * 
 *  To notify current IM status to other processes which are 
 *  displaying IM's status (such as uim-toolbar-gtk), 
 *  uim_helper_send_message should be called... maybe...
 *
 */

void
prop_list_update_cb(void *ptr, const char *str)
{
  uim_agent_context *ua = (uim_agent_context *)ptr;

  debug_printf(DEBUG_NOTE, "prop_list_update_cb\n");

  update_prop_list(ua->prop, str);
}


void
prop_label_update_cb(void *ptr , const char *str)
{
  uim_agent_context *ua = (uim_agent_context *)ptr;

  debug_printf(DEBUG_NOTE, "prop_label_update_cb\n");

  update_prop_label(ua->prop, str);
}

