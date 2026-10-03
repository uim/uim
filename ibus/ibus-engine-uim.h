/*
  Copyright (c) 2026 uim Project https://github.com/uim/uim

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

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS
  IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
  THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
  PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR
  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
  OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
  WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
  OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
  ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

/*
 * ibus-engine-uim: an IBus engine that hands the keys to uim. See
 * ibus-engine-uim.c.
 */

#pragma once

#include <ibus.h>

#include <uim/uim.h>

/* IBus key codes are evdev codes, which fit in this. */
#define IBUS_UIM_MAX_KEYCODE 768

typedef struct {
  int attr;
  char *str;
} PreeditSegment;

typedef struct {
  IBusEngine parent;

  uim_context uc;

  GArray *segments; /* of PreeditSegment */
  gboolean preedit_shown;

  IBusLookupTable *table;
  int nr;
  int display_limit;
  int page;
  int index;

  /* The pressed keys uim took, so their releases don't reach the
   * application either. */
  guint8 consumed_keys[IBUS_UIM_MAX_KEYCODE / 8];

  /* The field takes no composed text, so the keys go around uim. */
  gboolean bypassed;
  /* The pressed keys that went around uim, so their releases do too. */
  guint8 bypassed_keys[IBUS_UIM_MAX_KEYCODE / 8];

  /* The properties last told to the panel. */
  IBusPropList *props;
  /* Whether the panel has been told the properties since the focus
   * came in, so changes can go as updates. */
  gboolean props_registered;

  gboolean focused;
  /* Focused on ibus-daemon's own context, which it focuses while no
   * application has the focus. */
  gboolean in_daemon_context;
} IBusUimEngine;

typedef struct {
  IBusEngineClass parent;
} IBusUimEngineClass;

GType ibus_uim_engine_get_type(void);

void ibus_uim_engine_commit_string(IBusUimEngine *engine, const char *str);

/* helper.c */
void ibus_uim_helper_add_engine(IBusUimEngine *engine);
void ibus_uim_helper_remove_engine(IBusUimEngine *engine);
void ibus_uim_helper_focus_in(IBusUimEngine *engine);
void ibus_uim_helper_focus_out(IBusUimEngine *engine);
void ibus_uim_helper_prop_list_update(IBusUimEngine *engine, const char *str);
void ibus_uim_helper_disconnect(void);

/* property.c */
void ibus_uim_property_update(IBusUimEngine *engine, const char *str);
void ibus_uim_property_activate(IBusUimEngine *engine, const char *key,
                                guint state);
