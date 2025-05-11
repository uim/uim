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

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif
#include <uim/uim.h>
#include <uim/uim-helper.h>
#include <uim/uim-internal.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>

#include "../immodule/caret-state-indicator.h"
#include "../immodule/uim-cand-win-gtk.h"
#ifdef UIM_CANDWIN_STYLE_TABLE
#  include "../immodule/uim-cand-win-tbl-gtk.h"
#elif defined(UIM_CANDWIN_STYLE_HORIZONTAL)
#  include "../immodule/uim-cand-win-horizontal-gtk.h"
#else
#  include "../immodule/uim-cand-win-vertical-gtk.h"
#endif

static UIMCandWinGtk *cwin;
static GtkWidget *caret_state_indicator;

static void index_changed_cb(UIMCandWinGtk *cwin)
{
  fprintf(stdout, "index\n");
  fprintf(stdout, "%d\n\n", uim_cand_win_gtk_get_index(cwin));
  fflush(stdout);
}

static void
init_candidate_win(void)
{
#ifdef UIM_CANDWIN_STYLE_TABLE
  cwin = UIM_CAND_WIN_GTK(uim_cand_win_tbl_gtk_new());
#elif defined(UIM_CANDWIN_STYLE_HORIZONTAL)
  cwin = UIM_CAND_WIN_GTK(uim_cand_win_horizontal_gtk_new());
#else
  cwin = UIM_CAND_WIN_GTK(uim_cand_win_vertical_gtk_new());
#endif
  g_signal_connect(G_OBJECT(cwin), "index-changed",
		   G_CALLBACK(index_changed_cb), NULL);
  caret_state_indicator = caret_state_indicator_new();
}

static void
quit_candidate_win(void)
{
  g_object_unref(caret_state_indicator);
  g_object_unref(cwin);
}

static void
candidate_free(void *data)
{
  uim_candidate candidate = data;
  g_free(candidate->str);
  g_free(candidate->heading_label);
  g_free(candidate->annotation);
  g_free(candidate);
}

static GSList *
candwin_collect_candidates(const gchar *charset, gchar **raw_candidates)
{
  gsize rbytes, wbytes;
  GSList *candidates = NULL;
  for (; *raw_candidates; raw_candidates++) {
    const gchar *raw_candidate = *raw_candidates;

    if (strcmp(raw_candidate, "") == 0) {
      break;
    }
    uim_candidate candidate;
    gchar *utf8_str = g_convert(raw_candidate,
                                -1,
                                "UTF-8",
                                charset,
                                &rbytes, &wbytes, NULL);
    gchar **column = g_strsplit(utf8_str, "\a", 3);
    candidate = g_new0(struct uim_candidate_, 1);
    candidate->heading_label = g_strdup(column[0]);
    if (column[1]) {
      candidate->str = g_strdup(column[1]);
      if (column[2]) {
        candidate->annotation = g_strdup(column[2]);
      }
    }
    g_strfreev(column);
    g_free(utf8_str);

    candidates = g_slist_prepend(candidates, candidate);
  }
  return g_slist_reverse(candidates);
}

static void
candwin_activate(gchar **str)
{
  gint i = 1;
  const gchar *charset = "UTF-8";
  gboolean have_display_limit;
  guint display_limit = 0;
  GSList *candidates = NULL;

  if (!strncmp(str[i], "charset=", 8)) {
    charset = str[i] + 8;
    i++;
  }

  have_display_limit = !strncmp(str[i], "display_limit=", 14);
  if (have_display_limit) {
    display_limit = atoi(str[2] + 14);
    i++;
  }

  candidates = candwin_collect_candidates(charset, str + i);
  if (!have_display_limit)
    display_limit = g_slist_length(candidates);
  uim_cand_win_gtk_set_candidates(cwin, display_limit, candidates);
  if (!candidates)
    return;

  g_slist_free_full(candidates, candidate_free);
  uim_cand_win_gtk_set_index(cwin, 0);
  gtk_widget_show_all(GTK_WIDGET(cwin));
}

static void
candwin_update(gchar **str)
{
  int index;
  sscanf(str[1], "%d", &index);

  uim_cand_win_gtk_set_index(cwin, index);
}

static void
candwin_move(char **str)
{
  int pos_x, pos_y;
  sscanf(str[1], "%d", &pos_x);
  sscanf(str[2], "%d", &pos_y);

  gtk_window_move(GTK_WINDOW(cwin), pos_x, pos_y);
}

static void
candwin_show(void)
{
  if (uim_cand_win_gtk_get_nr_candidates(cwin) > 0) {
    gtk_widget_show_all(GTK_WIDGET(cwin));
    if (cwin->sub_window.active)
      gtk_widget_show(cwin->sub_window.window);
  }
}

static void
candwin_deactivate(void)
{
  gtk_widget_hide(GTK_WIDGET(cwin));
  if (cwin->sub_window.window)
    gtk_widget_hide(cwin->sub_window.window);
  uim_cand_win_gtk_clear_candidates(cwin);
}

static void
caret_state_show(gchar **str)
{
  int timeout;
  int root_x, root_y;

  sscanf(str[1], "%d", &timeout);
  gtk_window_get_position(GTK_WINDOW(cwin), &root_x, &root_y);
  caret_state_indicator_update(caret_state_indicator, root_x, root_y, str[2]);
  if (timeout != 0)
    caret_state_indicator_set_timeout(caret_state_indicator, timeout * 1000);
  gtk_widget_show_all(GTK_WIDGET(caret_state_indicator));
}

static void
caret_state_update()
{
  int root_x, root_y;
  gtk_window_get_position(GTK_WINDOW(cwin), &root_x, &root_y);
  caret_state_indicator_update(caret_state_indicator, root_x, root_y, NULL);
}

static void
caret_state_hide()
{
  gtk_widget_hide(caret_state_indicator);
}

static void
candwin_set_nr_candidates(gchar **str)
{
  guint nr, display_limit;

  sscanf(str[1], "%ud", &nr);
  sscanf(str[2], "%ud", &display_limit);

  uim_cand_win_gtk_set_nr_candidates(cwin, nr, display_limit);
}

static void
candwin_set_page_candidates(gchar **str)
{
  gint i = 1;
  const gchar *charset = "UTF-8";
  GSList *candidates = NULL;
  int page;

  if (!strncmp(str[i], "charset=", 8)) {
    charset = str[i] + 8;
    i++;
  }

  if (!strncmp(str[i], "page=", 5)) {
    page = atoi(str[i] + 5);
    i++;
  } else {
    /* shouldn't happen */
    page = 0;
  }

  candidates = candwin_collect_candidates(charset, str + i);
  uim_cand_win_gtk_set_page_candidates(cwin, page, candidates);
  if (!candidates)
    return;
  g_slist_free_full(candidates, candidate_free);
  uim_cand_win_gtk_set_page(cwin, page);
}

static void
candwin_show_page(gchar **str)
{
  int page;

  sscanf(str[1], "%d", &page);

  uim_cand_win_gtk_set_page(cwin, page);
  gtk_widget_show_all(GTK_WIDGET(cwin));
  gtk_widget_queue_resize_no_redraw(cwin->view);
}

static void
str_parse(gchar *str)
{
  gchar **tmp;
  gchar *command;

  tmp = g_strsplit(str, "\f", 0);
  command = tmp[0];

  if (command) {
    if (strcmp("activate", command) == 0) {
      candwin_activate(tmp);
    } else if (strcmp("select", command) == 0) {
      candwin_update(tmp);
    } else if (strcmp("show", command) == 0) {
      candwin_show();
    } else if (strcmp("hide", command) == 0) {
      gtk_widget_hide(GTK_WIDGET(cwin));
      if (cwin->sub_window.window)
        gtk_widget_hide(cwin->sub_window.window);
    } else if (strcmp("move", command) == 0) {
      candwin_move(tmp);
    } else if (strcmp("deactivate", command) == 0) {
      candwin_deactivate();
    } else if (strcmp("show_caret_state", command) == 0) {
      caret_state_show(tmp);
    } else if (strcmp("update_caret_state", command) == 0) {
      caret_state_update();
    } else if (strcmp("hide_caret_state", command) == 0) {
      caret_state_hide();
    } else if (strcmp("set_nr_candidates", command) == 0) {
      candwin_set_nr_candidates(tmp);
    } else if (strcmp("set_page_candidates", command) == 0) {
      candwin_set_page_candidates(tmp);
    } else if (strcmp("show_page", command) == 0) {
      candwin_show_page(tmp);
    }
  }
  g_strfreev(tmp);
}

#define CANDIDATE_BUFFER_SIZE	4096
static gboolean
read_cb(GIOChannel *channel, GIOCondition c, gpointer p)
{
  char buf[CANDIDATE_BUFFER_SIZE];
  char *read_buf = strdup("");
  int i = 0;
  int n;
  gchar **tmp;
  int fd = g_io_channel_unix_get_fd(channel);

  while (uim_helper_fd_readable(fd) > 0) {
    n = read(fd, buf, CANDIDATE_BUFFER_SIZE - 1);
    if (n == 0) {
      close(fd);
      exit(EXIT_FAILURE);
    }
    if (n == -1)
      return TRUE;
    buf[n] = '\0';
    read_buf = realloc(read_buf, strlen(read_buf) + n + 1);
    strcat(read_buf, buf);
  }

  tmp = g_strsplit(read_buf, "\f\f", 0);

  while (tmp[i]) {
    str_parse(tmp[i]);
    i++;
  }
  g_strfreev(tmp);
  free(read_buf);
  return TRUE;
}

int
main(int argc, char *argv[])
{
  GIOChannel *channel;
  guint read_tag;

  /* disable uim context in annotation window */
  setenv("GTK_IM_MODULE", "gtk-im-context-simple", 1);

  gtk_init(&argc, &argv);
  if (uim_init() < 0)
    return 0;

  init_candidate_win();

  channel = g_io_channel_unix_new(0);
  read_tag = g_io_add_watch(channel, G_IO_IN | G_IO_HUP | G_IO_ERR,
			    read_cb, 0);

  gtk_main();

  g_source_remove(read_tag);
  g_io_channel_unref(channel);

  quit_candidate_win();

  uim_quit();

  return 0;
}
