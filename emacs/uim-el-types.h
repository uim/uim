/*
  Copyright (c) 2006-2013 uim Project https://github.com/uim/uim

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

#ifndef UIM_EMACS_UIM_EL_TYPES_H
#define UIM_EMACS_UIM_EL_TYPES_H

#include <config.h>
#define UIM_EL_USE_NEW_PAGE_HANDLING	1

typedef struct candidate {
  char *str;
  char *label;
} candidate;


typedef struct candidate_info {
  int valid;
  int num;
  int disp_limit;
  int index;
#if UIM_EL_USE_NEW_PAGE_HANDLING
  int page_index;
#endif
  candidate *cand_array;
} candidate_info;


typedef struct im_encoding {
  char *im;                  /* IM name */
  char *encoding;            /* Encoding name (in uim-encoding.h) */
  struct im_encoding *next;
} im_encoding;


typedef struct uim_key{
  int mod;
  int key;
} uim_key;


typedef struct preedit_buffer {
  char *str;
  int attr;
  unsigned cursor;
  struct preedit_buffer *next;
} preedit_buffer;


typedef struct preedit {
  int valid;
  int length;
  preedit_buffer *head;
  preedit_buffer *tail;
  /*  candidate_info *cand;*/
} preedit;


typedef struct property {
  int valid;
  int list_update;
  char *list;
} property;


typedef struct uim_agent_context {
  uim_context context;
  int context_id;
  char *encoding;
  char *im;
  preedit *pe;
  candidate_info *cand;
  property *prop;
  char *comstr;
} uim_agent_context;


typedef struct uim_agent_context_list {
  uim_agent_context *agent_context;
  struct uim_agent_context_list *next;
  struct uim_agent_context_list *prev;
} uim_agent_context_list;


#endif
