/*
  uim-im-switcher.h: provides optional input method switching feature

  Copyright (c) 2003-2013 uim Project https://github.com/uim/uim

  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions
  are met:

  1. Redistributions of source code must retain the above copyright
     notice, this list of conditions and the following disclaimer.
  2. Redistributions in binary form must reproduce the above copyright
     notice, this list of conditions and the following disclaimer in the
     documentation and/or other materials provided with the distribution.
  3. Neither the name of authors nor the names of its contributors
     may be used to endorse or promote products derived from this software
     without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS IS'' AND
  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE
  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
  OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
  SUCH DAMAGE.

*/

/* This API is still preliminary and under discussion. See 'New
   IM-switching related API' section of doc/COMPATIBILITY for further
   information
*/

#ifndef UIM_IM_SWITCHER_H
#define UIM_IM_SWITCHER_H

#include "uim.h"

#ifdef __cplusplus
extern "C" {
#endif

/**
 * Get the name of current input method.
 * A uim_context has several input methods, only one of them is
 * selected. This function returns the name of currently selected
 * input method.
 * 
 * @warning you must not free the result.
 * @warning This function may return NULL.
 *
 * @param uc input context
 *
 * @return name of currently selected input method
 */
const char *uim_get_current_im_name(uim_context uc);

/**
 * Switch input method engine of the uc. (We should use the word
 * "module" here?)
 *
 * @param uc input context to switch the input method of
 * @param engine name of input method engine
 */
void
uim_switch_im(uim_context uc, const char *engine);

/*
 * Set callback functions called when IM-switching of specific set of context
 * is requested.
 *
 * When the functions are called back, bridges should re-initialize the
 * specified input contexts with the IM specified by 2nd argument
 * 'name'. Since the re-initialization method of specified contexts vary for
 * each IM environment, it is delegated to bridges via the callback. For
 * example, ordinary desktop system should send the helper message
 * im_change_whole_desktop in response to @a sw_system_im_cb. But in embedded
 * systems such as Qtopia, nothing to do with @a sw_system_im_cb since only
 * one input context is running on the window system and so system-global.
 *
 * @param uc input context

 * @param sw_app_im_cb called when re-initialization of all contexts within
 *        the application that @a uc belongs to, with specified IM is
 *        requested. 1st argument "ptr" corresponds to the 1st argument of
 *        uim_create_context, and 2nd "name" is requested idname of IM. The
 *        originating context is supposed to already be switched, and must not
 *        switched by the callback.
 * @param sw_system_im_cb called when re-initialization of all contexts
 *        running on the system that @a uc is running on, with specified IM is
 *        requested. The originating context is supposed to already be
 *        switched.
 */
void uim_set_im_switch_request_cb(uim_context uc,
				  void (*sw_app_im_cb)(void *ptr,
						       const char *name),
				  void (*sw_system_im_cb)(void *ptr,
							  const char *name));

#ifdef __cplusplus
}
#endif
#endif
