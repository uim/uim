/*

  Copyright (c) 2006-2013 uim Project https://github.com/uim/uim

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

#include <gtk/gtk.h>
#include <glib.h>
#include <glib/gprintf.h>

#include <string.h>
#include <unistd.h>

#include "uim/uim.h"

#include "gtk-im-uim.h"
#include "text-util.h"

static int
acquire_text_in_gtk_text_view(GtkTextView *text_view, enum UTextOrigin origin,
			      int former_req_len, int latter_req_len,
			      char **former, char **latter)
{
  GtkTextIter current, start, end;

  if (!gtk_text_view_get_buffer(text_view))
    return -1;

  gtk_text_buffer_get_iter_at_mark(gtk_text_view_get_buffer(text_view),
      &current,
      gtk_text_buffer_get_mark(gtk_text_view_get_buffer(text_view), "insert"));
  switch (origin) {
  case UTextOrigin_Cursor:
    start = current;
    end = current;

    if (former_req_len >= 0) {
      gtk_text_iter_backward_chars(&start, former_req_len);
    } else {
      if (former_req_len == UTextExtent_Full)
	gtk_text_buffer_get_start_iter(gtk_text_view_get_buffer(text_view), &start);
      else if (former_req_len == UTextExtent_Line)
	gtk_text_view_backward_display_line_start(text_view, &start);
      else
	return -1;
    }
    *former = gtk_text_iter_get_slice(&start, &current);

    if (latter_req_len >= 0)
      gtk_text_iter_forward_chars(&end, latter_req_len);
    else {
      if (latter_req_len == UTextExtent_Full)
	gtk_text_buffer_get_end_iter(gtk_text_view_get_buffer(text_view), &end);
      else if (latter_req_len == UTextExtent_Line)
	gtk_text_view_forward_display_line_end(text_view, &end);
      else {
	g_free(*former);
	return -1;
      }
    }
    *latter = gtk_text_iter_get_slice(&current, &end);
    break;

  case UTextOrigin_Beginning:
    gtk_text_buffer_get_start_iter(gtk_text_view_get_buffer(text_view), &start);
    end = start;

    *former = NULL;

    if (latter_req_len >= 0)
      gtk_text_iter_forward_chars(&end, latter_req_len);
    else {
      if (latter_req_len == UTextExtent_Full)
	gtk_text_buffer_get_end_iter(gtk_text_view_get_buffer(text_view), &end);
      else if (latter_req_len == UTextExtent_Line)
	gtk_text_view_forward_display_line_end(text_view, &end);
      else
        return -1;
    }
    *latter = gtk_text_iter_get_slice(&start, &end);
    break;

  case UTextOrigin_End:
    gtk_text_buffer_get_end_iter(gtk_text_view_get_buffer(text_view), &end);
    start = end;

    if (former_req_len >= 0) {
      gtk_text_iter_backward_chars(&start, former_req_len);
    } else {
      if (former_req_len == UTextExtent_Full)
	gtk_text_buffer_get_start_iter(gtk_text_view_get_buffer(text_view), &start);
      else if (former_req_len == UTextExtent_Line)
	gtk_text_view_backward_display_line_start(text_view, &start);
      else
	return -1;
    }
    *former = gtk_text_iter_get_slice(&start, &end);

    *latter = NULL;
    break;

  case UTextOrigin_Unspecified:
  default:
    return -1;
  }

  return 0;
}

int
im_uim_acquire_primary_text(IMUIMContext *uic, enum UTextOrigin origin,
			    int former_req_len, int latter_req_len,
			    char **former, char **latter)
{
  gchar *text, *former_start, *p;
  gint cursor_index, len, precedence_len, following_len;
  gboolean success;
  int offset, err = 0;

  /*
   * We may try a specific way for GtkTextView since
   * gtk_im_context_get_surrounding cannot get text with multiple lines.
   */
  if (GTK_IS_TEXT_VIEW(uic->widget))
    return acquire_text_in_gtk_text_view(GTK_TEXT_VIEW(uic->widget), origin,
					 former_req_len, latter_req_len,
					 former, latter);

  /* cursor_index is represented with byte index */
  success = gtk_im_context_get_surrounding(GTK_IM_CONTEXT(uic), &text,
					   &cursor_index);
  if (!success)
    return -1;

  len = strlen(text);
  precedence_len = g_utf8_strlen(text, cursor_index);
  following_len = g_utf8_strlen(text + cursor_index, strlen(text) -
				cursor_index);
  switch (origin) {
  case UTextOrigin_Cursor:
    offset = 0;
    if (former_req_len >= 0) {
      if (precedence_len > former_req_len)
        offset = precedence_len - former_req_len;
    } else {
      if (!(~former_req_len & (~UTextExtent_Line | ~UTextExtent_Full))) {
	g_free(text);
	return -1;
      }
    }
    former_start = g_utf8_offset_to_pointer(text, offset);
    *former = g_strndup(former_start, text - former_start + cursor_index);

    offset = 0;
    if (latter_req_len >= 0) {
      if (following_len > latter_req_len)
	offset = strlen(g_utf8_offset_to_pointer(text, precedence_len +
						 latter_req_len));
    } else {
      if (!(~latter_req_len & (~UTextExtent_Line | ~UTextExtent_Full))) {
	g_free(text);
	g_free(*former);
	return -1;
      }
    }
    *latter = g_strndup(text + cursor_index, len - cursor_index - offset);
    if (latter_req_len == UTextExtent_Line) {
      gchar *p = strchr(*latter, '\n');
      if (p)
	*p = '\0';
    }
    break;

  case UTextOrigin_Beginning:
    *former = NULL;

    offset = 0;
    if (latter_req_len >= 0) {
      if ((precedence_len + following_len) > latter_req_len)
	offset = text + len - g_utf8_offset_to_pointer(text, latter_req_len);
    } else {
      if (!(~latter_req_len & (~UTextExtent_Line | ~UTextExtent_Full))) {
	g_free(text);
	return -1;
      }
    }
    *latter = g_strndup(text, len - offset); 
    if (latter_req_len == UTextExtent_Line &&
	(p = strchr(*latter, '\n')))
      *p = '\0';
    break;

  case UTextOrigin_End:
    offset = 0;
    if (former_req_len >= 0) {
      if ((precedence_len + following_len) > former_req_len)
        offset = precedence_len + following_len - former_req_len;
    } else {
      if (!(~former_req_len & (~UTextExtent_Line | ~UTextExtent_Full))) {
	g_free(text);
	return -1;
      }
    }
    former_start = g_utf8_offset_to_pointer(text, offset);
    if (former_req_len == UTextExtent_Line &&
	(p = strrchr(former_start, '\n')))
      *former = g_strdup(p + 1);
    else
      *former = g_strndup(former_start, text + len - former_start);

    *latter = NULL;
    break;

  case UTextOrigin_Unspecified:
  default:
    err = -1;
    break;
  }
  g_free(text);

  return err;
}

int
im_uim_acquire_selection_text(IMUIMContext *uic, enum UTextOrigin origin,
			      int former_req_len, int latter_req_len,
			      char **former, char **latter)
{
  gchar *former_start, *text = NULL, *p;
  gint len, text_len;
  int offset, err = 0;
  gboolean cursor_at_beginning = FALSE;

  if (GTK_IS_ENTRY(uic->widget)) {
    gint start, end, current;

    if (gtk_editable_get_selection_bounds(GTK_EDITABLE(uic->widget),
					  &start, &end)) {
      text = gtk_editable_get_chars(GTK_EDITABLE(uic->widget), start, end);
      current = gtk_editable_get_position(GTK_EDITABLE(uic->widget));
      if (current == start)
	cursor_at_beginning = TRUE;
    }
  } else if (GTK_IS_TEXT_VIEW(uic->widget)) {
    GtkTextIter start, end, current;

    if (gtk_text_view_get_buffer(GTK_TEXT_VIEW(uic->widget)) &&
	gtk_text_buffer_get_selection_bounds(gtk_text_view_get_buffer(GTK_TEXT_VIEW(uic->widget)), &start, &end)) {
      text = gtk_text_iter_get_visible_text(&start, &end);
      gtk_text_buffer_get_iter_at_mark(gtk_text_view_get_buffer(GTK_TEXT_VIEW(uic->widget)),
				       &current,
				       gtk_text_buffer_get_mark(gtk_text_view_get_buffer(GTK_TEXT_VIEW(uic->widget)), "insert"));
      if (gtk_text_iter_compare(&start, &current) == 0)
	cursor_at_beginning = TRUE;
    }
  } else {
    /*
     * We use GDK_SELECTION_PRIMARY for the rest of widget, which means it is
     * impossible to guarantee whether the obtained one is the selected text on
     * the target application.
     */ 
    text = gtk_clipboard_wait_for_text(gtk_widget_get_clipboard(GTK_WIDGET(uic->widget), GDK_SELECTION_PRIMARY));
  }

  if (!text)
    return -1;

  len = strlen(text);
  text_len = g_utf8_strlen(text, -1);

  if (origin == UTextOrigin_Beginning ||
      (origin == UTextOrigin_Cursor && cursor_at_beginning)) {
    *former = NULL;

    offset = 0;
    if (latter_req_len >= 0) {
      if (latter_req_len < text_len)
	offset = text + len - g_utf8_offset_to_pointer(text, latter_req_len);
    } else {
      if (!(~latter_req_len & (~UTextExtent_Line | ~UTextExtent_Full))) {
	g_free(text);
	return -1;
      }
    }
    *latter = g_strndup(text, len - offset);
    if (latter_req_len == UTextExtent_Line && (p = strchr(*latter, '\n')))
      *p = '\0';

  } else if (origin == UTextOrigin_End ||
	     (origin == UTextOrigin_Cursor && !cursor_at_beginning)) {
    offset = 0;
    if (former_req_len >= 0) {
      if (former_req_len < text_len)
	offset = text_len - former_req_len;
    } else {
      if (!(~former_req_len & (~UTextExtent_Line | ~UTextExtent_Full))) {
	g_free(text);
	return -1;
      }
    }
    former_start = g_utf8_offset_to_pointer(text, offset);
    if (former_req_len == UTextExtent_Line &&
	(p = strrchr(former_start, '\n')))
      *former = g_strdup(p + 1);
    else
      *former = g_strndup(former_start, text + len - former_start);

    *latter = NULL;

  } else {
    err = -1;
  }
  g_free(text);

  return err;
}

int
im_uim_acquire_clipboard_text(IMUIMContext *uic, enum UTextOrigin origin,
			      int former_req_len, int latter_req_len,
			      char **former, char **latter)
{
  gchar *former_start, *text = NULL, *p;
  gint len, text_len;
  int offset, err = 0;

  text = gtk_clipboard_wait_for_text(gtk_widget_get_clipboard(GTK_WIDGET(uic->widget), GDK_SELECTION_CLIPBOARD));

  if (!text)
    return -1;

  len = strlen(text);
  text_len = g_utf8_strlen(text, -1);

  /* treat cursor position is virtually at the end for UTextArea_Clipboard */
  switch (origin) {
  case UTextOrigin_Cursor:
  case UTextOrigin_End:
    offset = 0;
    if (former_req_len >= 0) {
      if (former_req_len < text_len)
	offset = text_len - former_req_len;
    } else {
      if (!(~former_req_len & (~UTextExtent_Line | ~UTextExtent_Full))) {
	g_free(text);
	return -1;
      }
    }
    former_start = g_utf8_offset_to_pointer(text, offset);
    if (former_req_len == UTextExtent_Line &&
	(p = strrchr(former_start, '\n')))
      *former = g_strdup(p + 1);
    else
      *former = g_strndup(former_start, text + len - former_start);
    *latter = NULL;
    break;
  case UTextOrigin_Beginning:
    offset = 0;
    if (latter_req_len >= 0) {
      if (latter_req_len < text_len)
	offset = text + len - g_utf8_offset_to_pointer(text, latter_req_len);
    } else {
      if (!(~latter_req_len & (~UTextExtent_Line | ~UTextExtent_Full))) {
	g_free(text);
	return -1;
      }
      if (latter_req_len == UTextExtent_Line && (p = strchr(text, '\n')))
        offset = text + len - p;
    }
    *latter = g_strndup(text, len - offset);
    *former = NULL;
    break;
  case UTextOrigin_Unspecified:
  default:
    err = -1;
    break;
  }
  g_free(text);

  return err;
}

static int
delete_text_in_gtk_entry(GtkEntry *entry, enum UTextOrigin origin,
			 int former_req_len, int latter_req_len)
{
  gint start_pos, end_pos, current_pos;

  current_pos = gtk_editable_get_position(GTK_EDITABLE(entry));

  switch (origin) {
  case UTextOrigin_Cursor:
    if (former_req_len >= 0) {
      start_pos = current_pos - former_req_len;
    } else {
      if (!(~former_req_len & (~UTextExtent_Line | ~UTextExtent_Full)))
	return -1;
      start_pos = 0;
    }

    if (latter_req_len >= 0)
      end_pos = current_pos + latter_req_len;
    else {
      if (!(~latter_req_len & (~UTextExtent_Line | ~UTextExtent_Full)))
	return -1;
      end_pos = gtk_entry_get_text_length(entry);
    }
    break;

  case UTextOrigin_Beginning:
    start_pos = 0;

    if (latter_req_len >= 0)
      end_pos = latter_req_len;
    else {
      if (!(~latter_req_len & (~UTextExtent_Line | ~UTextExtent_Full)))
	return -1;
      end_pos = gtk_entry_get_text_length(entry);
    }
    break;

  case UTextOrigin_End:
    if (former_req_len >= 0)
      start_pos = gtk_entry_get_text_length(entry) - former_req_len;
    else {
      if (!(~former_req_len & (~UTextExtent_Line | ~UTextExtent_Full)))
	return -1;
      start_pos = 0;
    }

    end_pos = gtk_entry_get_text_length(entry);
    break;

  case UTextOrigin_Unspecified:
  default:
    return -1;
  }

  gtk_editable_delete_text(GTK_EDITABLE(entry), start_pos, end_pos);

  return 0;
}

static int
delete_text_in_gtk_text_view(GtkTextView *text_view, enum UTextOrigin origin,
			     int former_req_len, int latter_req_len)
{
  GtkTextIter current, start, end;

  if (!gtk_text_view_get_buffer(text_view))
    return -1;

  gtk_text_buffer_get_iter_at_mark(gtk_text_view_get_buffer(text_view),
      &current,
      gtk_text_buffer_get_mark(gtk_text_view_get_buffer(text_view), "insert"));
  start = current;
  end = current;

  switch (origin) {
  case UTextOrigin_Cursor:
    if (former_req_len >= 0) {
      gtk_text_iter_backward_chars(&start, former_req_len);
    } else {
      if (former_req_len == UTextExtent_Full)
	gtk_text_buffer_get_start_iter(gtk_text_view_get_buffer(text_view), &start);
      else if (former_req_len == UTextExtent_Line)
	gtk_text_view_backward_display_line_start(text_view, &start);
      else
	return -1;
    }

    if (latter_req_len >= 0)
      gtk_text_iter_forward_chars(&end, latter_req_len);
    else {
      if (latter_req_len == UTextExtent_Full)
	gtk_text_buffer_get_end_iter(gtk_text_view_get_buffer(text_view), &end);
      else if (latter_req_len == UTextExtent_Line)
	gtk_text_view_forward_display_line_end(text_view, &end);
      else
	return -1;
    }
    break;

  case UTextOrigin_Beginning:
    gtk_text_buffer_get_start_iter(gtk_text_view_get_buffer(text_view), &start);
    end = start;

    if (latter_req_len >= 0)
      gtk_text_iter_forward_chars(&end, latter_req_len);
    else {
      if (latter_req_len == UTextExtent_Full)
	gtk_text_buffer_get_end_iter(gtk_text_view_get_buffer(text_view), &end);
      else if (latter_req_len == UTextExtent_Line)
	gtk_text_view_forward_display_line_end(text_view, &end);
      else
	return -1;
    }
    break;

  case UTextOrigin_End:
    gtk_text_buffer_get_end_iter(gtk_text_view_get_buffer(text_view), &end);
    start = end;

    if (former_req_len >= 0) {
      gtk_text_iter_backward_chars(&start, former_req_len);
    } else {
      if (former_req_len == UTextExtent_Full)
	gtk_text_buffer_get_start_iter(gtk_text_view_get_buffer(text_view), &start);
      else if (former_req_len == UTextExtent_Line)
	gtk_text_view_backward_display_line_start(text_view, &start);
      else
	return -1;
    }
    break;

  case UTextOrigin_Unspecified:
  default:
    return -1;
  }

  gtk_text_buffer_delete_interactive(gtk_text_view_get_buffer(text_view),
      &start, &end, gtk_text_view_get_editable(text_view));

  return 0;
}

int
im_uim_delete_primary_text(IMUIMContext *uic, enum UTextOrigin origin,
			   int former_req_len, int latter_req_len)
{
  gboolean success;
  gint offset, n_chars;

  /* specific widgets handling */
  if (GTK_IS_ENTRY(uic->widget))
    return delete_text_in_gtk_entry(GTK_ENTRY(uic->widget), origin,
				    former_req_len, latter_req_len);
  else if (GTK_IS_TEXT_VIEW(uic->widget))
    return delete_text_in_gtk_text_view(GTK_TEXT_VIEW(uic->widget), origin,
					former_req_len, latter_req_len);
  /*
   * For the rest of widget, we use delete_surrounding, which  means explicit
   * value for former_len and latter_len is required and its origin must be the
   * cursor.
   */
  offset = n_chars = 0;

  switch (origin) {
  case UTextOrigin_Cursor:
    if (former_req_len >= 0) {
      offset = -former_req_len;
      n_chars = former_req_len;
    } else {
	return -1;
    }

    if (latter_req_len >= 0)
      n_chars += latter_req_len;
    else
      return -1;
    break;

  case UTextOrigin_Beginning:
  case UTextOrigin_End:
  case UTextOrigin_Unspecified:
  default:
    return -1;
  }

  success = gtk_im_context_delete_surrounding(GTK_IM_CONTEXT(uic), offset,
					      n_chars);
  return success ? 0 : -1;
}

static int
delete_selection_in_gtk_entry(GtkEntry *entry, enum UTextOrigin origin,
			      int former_req_len, int latter_req_len)
{
  gint start, end, current_pos;
  gboolean cursor_at_beginning = FALSE;

  if (!gtk_editable_get_selection_bounds(GTK_EDITABLE(entry), &start, &end))
    return -1;

  current_pos = gtk_editable_get_position(GTK_EDITABLE(entry));
  if (current_pos == start)
    cursor_at_beginning = TRUE;

  if (origin == UTextOrigin_Beginning ||
      (origin == UTextOrigin_Cursor && cursor_at_beginning)) {
    if (latter_req_len >= 0) {
      if (latter_req_len < end - start)
	end = start + latter_req_len;
    } else {
      if (!(~latter_req_len & (~UTextExtent_Line | ~UTextExtent_Full)))
	return -1;
    }
  } else if (origin == UTextOrigin_End ||
	     (origin == UTextOrigin_Cursor && !cursor_at_beginning)) {
    if (former_req_len >= 0) {
      if (former_req_len < end - start)
	start = end - former_req_len;
    } else {
      if (!(~former_req_len & (~UTextExtent_Line | ~UTextExtent_Full)))
	return -1;
    }
  } else {
    return -1;
  }

  gtk_editable_delete_text(GTK_EDITABLE(entry), start, end);

  return 0;
}

static int
delete_selection_in_gtk_text_view(GtkTextView *text_view,
				  enum UTextOrigin origin, int former_req_len,
				  int latter_req_len)
{
  GtkTextIter current, start, end, tmp_start, tmp_end;
  gboolean cursor_at_beginning = FALSE;

  if (!gtk_text_view_get_buffer(text_view))
    return -1;

  if (gtk_text_buffer_get_selection_bounds(gtk_text_view_get_buffer(text_view),
      &start, &end)) {
    gtk_text_buffer_get_iter_at_mark(gtk_text_view_get_buffer(text_view),
        &current,
        gtk_text_buffer_get_mark(gtk_text_view_get_buffer(text_view),
        "insert"));
    if (gtk_text_iter_compare(&start, &current) == 0)
      cursor_at_beginning = TRUE;
  } else {
    return -1;
  }

  if (origin == UTextOrigin_Beginning ||
      (origin == UTextOrigin_Cursor && cursor_at_beginning)) {
    tmp_start = start;
    tmp_end = start;

    if (latter_req_len >= 0) {
      gtk_text_iter_forward_chars(&tmp_end, latter_req_len);
      if (gtk_text_iter_compare(&tmp_end, &end) < 0)
	end = tmp_end;
    } else {
      if (latter_req_len == UTextExtent_Line) {
	gtk_text_view_forward_display_line_end(text_view, &tmp_end);
	if (gtk_text_iter_compare(&tmp_end, &end) < 0)
	  end = tmp_end;
      } else {
	if (!(latter_req_len == UTextExtent_Full))
	  return -1;
      }
    }

  } else if (origin == UTextOrigin_End ||
	     (origin == UTextOrigin_Cursor && !cursor_at_beginning)) {
    tmp_start = end;
    tmp_end = end;

    if (former_req_len >= 0) {
      gtk_text_iter_backward_chars(&tmp_start, former_req_len);
      if (gtk_text_iter_compare(&tmp_start, &start) > 0)
	start = tmp_start;
    } else {
      if (former_req_len == UTextExtent_Line) {
	gtk_text_view_backward_display_line_start(text_view, &tmp_start);
	if (gtk_text_iter_compare(&tmp_start, &start) > 0)
	  start = tmp_start;
      } else {
	if (!(former_req_len == UTextExtent_Full))
	  return -1;
      }
    }

  } else {
    return -1;
  }

  gtk_text_buffer_delete_interactive(gtk_text_view_get_buffer(text_view),
      &start, &end, gtk_text_view_get_editable(text_view));

  return 0;
}

int
im_uim_delete_selection_text(IMUIMContext *uic, enum UTextOrigin origin,
			     int former_req_len, int latter_req_len)
{
  /* specific widgets handling */
  if (GTK_IS_ENTRY(uic->widget))
    return delete_selection_in_gtk_entry(GTK_ENTRY(uic->widget), origin,
				    former_req_len, latter_req_len);
  else if (GTK_IS_TEXT_VIEW(uic->widget))
    return delete_selection_in_gtk_text_view(GTK_TEXT_VIEW(uic->widget), origin,
					former_req_len, latter_req_len);
  /*
   * How can we delete a selected text?
   * We just expect the selected text will be overridden by a newly committed
   * text.
   */
  
  return -1;
}
