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

#ifndef _uim_agent_helper_h_included_
#define _uim_agent_helper_h_included_

#include <string.h>
#include <stdlib.h>

#include <uim/uim.h>
#include <uim/uim-helper.h>
#include <uim/uim-im-switcher.h>

#include "debug.h"

#include "context.h"
#include "encoding.h"
#include "im.h"
#include "helper-server.h"

void helper_send_im_list(void);
void helper_im_changed(char *request, char *engine_name);

void helper_handler(void);

int focused(uim_agent_context *ua);
int unfocused(void);

#endif
