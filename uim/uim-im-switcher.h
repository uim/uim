/*
  uim-im-switcher.h: provides optional input method switching feature

  Copyright (c) 2003-2006 uim Project http://uim.freedesktop.org/

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

#ifndef _uim_im_switcher_h_included_
#define _uim_im_switcher_h_included_

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

#ifdef __cplusplus
}
#endif
#endif
