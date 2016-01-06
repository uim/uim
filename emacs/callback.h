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

#ifndef UIM_EMACS_CALLBACK_H
#define UIM_EMACS_CALLBACK_H

#include <config.h>

#include <stdlib.h>
#include <string.h>
#include <locale.h>

#include <uim/uim.h>

#include "debug.h"
#include "output.h"
#include "uim-el-types.h"
#include "context.h"
#include "candidate.h"
#include "preedit.h"
#include "helper.h"
#include "im.h"
#include "commit.h"
#include "prop.h"

void commit_cb(void *ptr, const char *str);

void preedit_clear_cb(void *ptr);
void preedit_pushback_cb(void *ptr, int attr, const char * str);
void preedit_update_cb(void *ptr);

void candidate_activate_cb(void *ptr, int num, int limit);
void candidate_select_cb(void *ptr, int index);
void candidate_shift_page_cb(void *ptr, int direction);
void candidate_deactivate_cb(void *ptr);

void prop_list_update_cb(void *ptr, const char *str);

void configuration_changed_cb(void *ptr);

void switch_app_global_im_cb(void *ptr, const char *name);
void switch_system_global_im_cb(void *ptr, const char *name);

#endif
