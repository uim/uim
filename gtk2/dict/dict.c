/*

  Copyright (c) 2004-2013 uim Project https://github.com/uim/uim

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

#include <config.h>

#include <string.h>

#include "dict.h"

/* FIXME! use function */
#ifdef USE_ANTHY
extern uim_dict_class uim_dict_class_anthy;
#endif
#ifdef USE_CANNA
extern uim_dict_class uim_dict_class_canna;
#endif

static uim_dict_class *classes[] = {
#ifdef USE_ANTHY
  &uim_dict_class_anthy,
#endif
#ifdef USE_CANNA
  &uim_dict_class_canna,
#endif
  NULL
};

uim_dict *
uim_dict_open(const char *identifier)
{
  unsigned int i;
  uim_dict *dict;

  /* FIXME! use hash table? */
  for (i = 0; classes[i]; i++) {
    dict = classes[i]->open(identifier);
    if (dict)
      return dict;
  }

  return NULL;
}

void
uim_dict_close(uim_dict *dict)
{
  if (!dict)
    return;

  if (!dict->funcs || !dict->funcs->close) {
    /* warning? */
    return;
  }

  dict->funcs->close(dict);
}

uim_dict *
uim_dict_ref(uim_dict *dict)
{
  if (!dict)
    return NULL;
  dict->ref_count++;

  return dict;
}

void
uim_dict_unref(uim_dict *dict)
{
  if (!dict)
    return;
  dict->ref_count--;

  if (dict->ref_count == 0)
    uim_dict_close(dict);
}

/* fail: 0, success: 1 */
int
uim_dict_add_word(uim_dict *dict, uim_word *word)
{
  if (!dict)
    return 0;

  if (!dict->funcs || !dict->funcs->add_word) {
    /* warning? */
    return 0;
  }

  return dict->funcs->add_word(dict, word);
}

/* fail: 0, success: 1 */
int
uim_dict_change_word(uim_dict *dict, uim_word *word)
{
  if (!dict)
    return 0;

  if (!dict->funcs || !dict->funcs->change_word) {
    /* warning? */
    return 0;
  }

  return dict->funcs->change_word(dict, word);
}

/* fail: 0, success: 1 */
int
uim_dict_remove_word(uim_dict *dict, uim_word *word)
{
  if (!dict)
    return 0;

  if (!dict->funcs || !dict->funcs->change_word) {
    /* warning? */
    return 0;
  }

  return dict->funcs->remove_word(dict, word);
}

void
uim_dict_refresh(uim_dict *dict)
{
  if (!dict)
    return;

  if (!dict->funcs || !dict->funcs->refresh) {
    /* warning? */
    return;
  }

  dict->funcs->refresh(dict);
}
