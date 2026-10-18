/*

  uim-toolbar-widget.h: engine-agnostic uim toolbar widget for GTK 4

  Copyright (c) 2003-2026 uim Project https://github.com/uim/uim

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

#ifndef UIM_TOOLBAR_WIDGET_H
#define UIM_TOOLBAR_WIDGET_H

#include <gtk/gtk.h>

G_BEGIN_DECLS

/*
 * uim's toolbar is intentionally engine-agnostic: everything it shows
 * (the current input method's icon, its property/state menu such as
 * hiragana/katakana or a candidate-conversion mode, and its sub menu
 * entries) is obtained exclusively through the generic uim-helper
 * "prop_list_update" wire protocol (see helper_toolbar_prop_list_update()
 * in uim-toolbar-widget.c). No input method name (such as "anthy" or
 * "mozc") ever appears in this file: any engine that speaks the uim
 * helper protocol renders identically here, and switching the active
 * input method requires no toolbar-side change.
 *
 * The row of small launcher buttons (switch IM / preference / dictionary
 * / input pad / handwriting pad / help) is the one part that is a fixed
 * list of external helper commands. That list lives in a single
 * data-driven table (uim_toolbar_command_table in uim-toolbar-widget.c)
 * instead of being wired ad-hoc per frontend, so adding, removing or
 * reordering a launcher is a one-line table edit rather than a code
 * change scattered across the toolbar implementation.
 */

typedef enum {
  UIM_TOOLBAR_KIND_STANDALONE,
  UIM_TOOLBAR_KIND_APPLET,
  UIM_TOOLBAR_KIND_ICON
} UimToolbarKind;

#define UIM_TYPE_TOOLBAR (uim_toolbar_get_type())
G_DECLARE_FINAL_TYPE(UimToolbar, uim_toolbar, UIM, TOOLBAR, GtkBox)

GtkWidget *uim_toolbar_new(UimToolbarKind kind);

/* Repositions/resizes bookkeeping used by a standalone shell window;
 * harmless no-op for applet/icon kinds. */
void uim_toolbar_set_host_window(UimToolbar *toolbar, GtkWindow *window);

G_END_DECLS

#endif /* UIM_TOOLBAR_WIDGET_H */
