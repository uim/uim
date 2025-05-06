/*

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

/*
 * gtk+-immodule
 */
#include <config.h>

#include <gtk/gtk.h>
#include <gtk/gtkimmodule.h>
#ifdef GDK_WINDOWING_X11
#include <gdk/gdkx.h>
#endif

#include "uim/uim.h"

#include "uim-cand-win-gtk.h"
#ifdef GDK_WINDOWING_X11
#include "compose.h"
#endif

/* select either of these two, or filter key event will be used */
#define IM_UIM_USE_SNOOPER      0
#define IM_UIM_USE_TOPLEVEL     1

/* enable per page candidates handling */
#define IM_UIM_USE_NEW_PAGE_HANDLING	1
/* enable delay showing candidate window */
#define IM_UIM_USE_DELAY	1

typedef struct _IMUIMContext {
  struct _GtkIMContext parent;
  struct _GtkIMContext *slave;
  uim_context uc;
  UIMCandWinGtk *cwin;
  gboolean cwin_is_active;
  int nr_psegs;
  int prev_preedit_len;
  struct preedit_segment *pseg;

  GdkWindow *win;

  GtkWidget *caret_state_indicator;
  GdkRectangle preedit_pos;

  /* following two members are used when use_preedit == FALSE */
  GtkWidget *preedit_window;
  gulong preedit_handler_id;

  GtkWidget *widget;
#if IM_UIM_USE_TOPLEVEL
  /*
   * event_rec is used to check the incoming event is already handled
   * in our toplevel handler.  Some widgets (e.g. OOo2.0's vcl plugin)
   * have already connected key press/release event handlers to the
   * toplevel window before our handler attempts to connect.
   */
  GdkEventKey event_rec;
#endif
#ifdef GDK_WINDOWING_X11
  Compose *compose;
#endif

  struct _IMUIMContext *prev, *next;
} IMUIMContext;

void im_uim_commit_string(void *ptr, const char *str);
