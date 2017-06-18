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

#ifndef UIM_EMACS_UIM_EL_AGENT_H
#define UIM_EMACS_UIM_EL_AGENT_H

#include <config.h>

#include "debug.h"
#include "output.h"
#include "uim-el-types.h"
#include "candidate.h"
#include "preedit.h"
#include "context.h"
#include "encoding.h"
#include "im.h"
#include "key.h"
#include "helper.h"
#include "callback.h"
#include "prop.h"

static int cmd_release(int context_id);
static int cmd_helper(int context_id, char *message);
static int cmd_unfocused(int context_id);
static int cmd_focused(int context_id);
static int cmd_hide(int context_id);
static int cmd_show(int context_id);
static int cmd_new(int context_id, const char *encoding);
static int cmd_reset(int context_id);
static int cmd_change(int context_id, const char *im);
static int cmd_list(void);
static int cmd_setenc(char *opt);
static int cmd_prop(int context_id, const char *prop);
static int cmd_nop(int context_id);
static int cmd_error(void);

static void check_default_engine(void);

static void check_prop_list_update(uim_agent_context *ua);

static int process_command(int serial, int cid, char *cmd);
static int process_keyvector(int serial, int cid,
							 uim_key ukey, const char *keyname);
static int analyze_keyvector(char *vector, uim_key *ukey, char *keyname, size_t keyname_len);

void cleanup(void);

#endif
