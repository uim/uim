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
 * The text around the cursor, which the application sends through
 * ibus-daemon and uim asks for to convert what has already been typed.
 * IBus counts it in characters.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "ibus-engine-uim.h"

#include <uim/uim-util.h>

typedef struct {
  gunichar *chars;
  glong length;
  glong cursor;
  glong anchor;
} Surrounding;

/* Only an application that sends the text has any. */
static gboolean
get_surrounding(IBusUimEngine *engine, Surrounding *surrounding)
{
  IBusEngine *ibus_engine = IBUS_ENGINE(engine);
  IBusText *text;
  guint cursor, anchor;

  if (!(ibus_engine->client_capabilities & IBUS_CAP_SURROUNDING_TEXT) ||
      !engine->has_surrounding_text)
    return FALSE;
  ibus_engine_get_surrounding_text(ibus_engine, &text, &cursor, &anchor);
  surrounding->chars = g_utf8_to_ucs4_fast(ibus_text_get_text(text), -1,
                                           &surrounding->length);
  g_object_unref(text);
  /* The positions come from another process, so don't trust them to be
   * inside the text. */
  surrounding->cursor = MIN((glong)cursor, surrounding->length);
  surrounding->anchor = MIN((glong)anchor, surrounding->length);
  return TRUE;
}

/* Walks back over n characters, or over a whole extent, stopping at
 * floor. */
static glong
step_back(const gunichar *chars, glong offset, glong floor, int n)
{
  if (n == UTextExtent_Full)
    return floor;
  if (n == UTextExtent_Line) {
    while (offset > floor && chars[offset - 1] != '\n')
      offset--;
    return offset;
  }
  return MAX(offset - n, floor);
}

/* Walks forward over n characters, or over a whole extent, stopping
 * at ceiling. */
static glong
step_forward(const gunichar *chars, glong offset, glong ceiling, int n)
{
  if (n == UTextExtent_Full)
    return ceiling;
  if (n == UTextExtent_Line) {
    while (offset < ceiling && chars[offset] != '\n')
      offset++;
    return offset;
  }
  return MIN(offset + n, ceiling);
}

static gboolean
supported_extent(int length)
{
  return length >= 0 || length == UTextExtent_Full ||
    length == UTextExtent_Line;
}

/* Finds the characters uim is asking for. from and to bracket them,
 * and middle is where the origin sits between the two halves. */
static gboolean
range_of(const Surrounding *surrounding,
         enum UTextArea text_id,
         enum UTextOrigin origin,
         int former_length,
         int latter_length,
         glong *from,
         glong *middle,
         glong *to)
{
  glong floor = 0;
  glong ceiling = surrounding->length;
  glong cursor = surrounding->cursor;

  if (!supported_extent(former_length) || !supported_extent(latter_length))
    return FALSE;

  switch (text_id) {
  case UTextArea_Primary:
    break;
  case UTextArea_Selection:
    if (surrounding->cursor == surrounding->anchor)
      return FALSE;
    floor = MIN(surrounding->cursor, surrounding->anchor);
    ceiling = MAX(surrounding->cursor, surrounding->anchor);
    cursor = floor;
    break;
  case UTextArea_Clipboard:
    /* IBus tells us nothing about the clipboard. */
  case UTextArea_Unspecified:
  default:
    return FALSE;
  }

  switch (origin) {
  case UTextOrigin_Cursor:
    *middle = cursor;
    break;
  case UTextOrigin_Beginning:
    *middle = floor;
    break;
  case UTextOrigin_End:
    *middle = ceiling;
    break;
  case UTextOrigin_Unspecified:
  default:
    return FALSE;
  }

  *from = step_back(surrounding->chars, *middle, floor, former_length);
  *to = step_forward(surrounding->chars, *middle, ceiling, latter_length);
  return TRUE;
}

/* libuim frees the result with free(). */
static char *
slice(const gunichar *chars, glong from, glong to)
{
  char *utf8 = g_ucs4_to_utf8(chars + from, to - from, NULL, NULL, NULL);
  char *copy = uim_strdup(utf8 ? utf8 : "");

  g_free(utf8);
  return copy;
}

int
ibus_uim_text_acquire(void *ptr,
                      enum UTextArea text_id,
                      enum UTextOrigin origin,
                      int former_length,
                      int latter_length,
                      char **former,
                      char **latter)
{
  Surrounding surrounding;
  glong from, middle, to;
  int result = -1;

  if (!get_surrounding(ptr, &surrounding))
    return -1;
  if (range_of(&surrounding, text_id, origin, former_length, latter_length,
               &from, &middle, &to)) {
    *former = slice(surrounding.chars, from, middle);
    *latter = slice(surrounding.chars, middle, to);
    result = 0;
  }
  g_free(surrounding.chars);
  return result;
}

int
ibus_uim_text_delete(void *ptr,
                     enum UTextArea text_id,
                     enum UTextOrigin origin,
                     int former_length,
                     int latter_length)
{
  Surrounding surrounding;
  glong from, middle, to;
  int result = -1;

  if (!get_surrounding(ptr, &surrounding))
    return -1;
  if (range_of(&surrounding, text_id, origin, former_length, latter_length,
               &from, &middle, &to)) {
    /* IBus updates the text it keeps for us too. */
    if (from < to)
      ibus_engine_delete_surrounding_text(IBUS_ENGINE(ptr),
                                          (gint)(from - surrounding.cursor),
                                          (guint)(to - from));
    result = 0;
  }
  g_free(surrounding.chars);
  return result;
}
