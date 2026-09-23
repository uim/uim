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

/* The text around the cursor, which the application sends through the
 * compositor and uim asks for to convert what has already been typed. */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdlib.h>
#include <string.h>

#include "uim-wayland.h"

#include <uim/uim-util.h>


void
uim_wayland_text_set_surrounding(struct uim_wayland *uw,
                                 const char *text,
                                 uint32_t cursor,
                                 uint32_t anchor)
{
  size_t length = text ? strlen(text) : 0;

  free(uw->surrounding_text);
  uw->surrounding_text = text ? uim_strdup(text) : NULL;
  /* The offsets come from another process, so don't trust them to be
   * inside the text. */
  uw->surrounding_cursor = cursor < length ? cursor : length;
  uw->surrounding_anchor = anchor < length ? anchor : length;
}

void
uim_wayland_text_forget_surrounding(struct uim_wayland *uw)
{
  free(uw->surrounding_text);
  uw->surrounding_text = NULL;
  uw->surrounding_cursor = 0;
  uw->surrounding_anchor = 0;
}

static bool
is_continuation(char byte)
{
  return ((unsigned char)byte & 0xc0) == 0x80;
}

/* Walks back over n characters, or over a whole extent, stopping at
 * floor. */
static size_t
step_back(const char *text, size_t offset, size_t floor, int n)
{
  int steps;

  if (n == UTextExtent_Full) {
    return floor;
  } else if (n == UTextExtent_Line) {
    while (offset > floor && text[offset - 1] != '\n')
      offset--;
    return offset;
  }

  for (steps = 0; steps < n && offset > floor; steps++) {
    offset--;
    while (offset > floor && is_continuation(text[offset]))
      offset--;
  }
  return offset;
}

/* Walks forward over n characters, or over a whole extent, stopping
 * at ceiling. */
static size_t
step_forward(const char *text, size_t offset, size_t ceiling, int n)
{
  int steps;

  if (n == UTextExtent_Full) {
    return ceiling;
  } else if (n == UTextExtent_Line) {
    while (offset < ceiling && text[offset] != '\n')
      offset++;
    return offset;
  }

  for (steps = 0; steps < n && offset < ceiling; steps++) {
    offset++;
    while (offset < ceiling && is_continuation(text[offset]))
      offset++;
  }
  return offset;
}

static bool
supported_extent(int length)
{
  return length >= 0 || length == UTextExtent_Full ||
    length == UTextExtent_Line;
}

static char *
slice(const char *text, size_t from, size_t to)
{
  size_t length = to - from;
  char *copy = uim_malloc(length + 1);

  memcpy(copy, text + from, length);
  copy[length] = '\0';
  return copy;
}

static bool
selection_of(struct uim_wayland *uw, size_t *from, size_t *to)
{
  if (uw->surrounding_cursor == uw->surrounding_anchor)
    return false;
  if (uw->surrounding_cursor < uw->surrounding_anchor) {
    *from = uw->surrounding_cursor;
    *to = uw->surrounding_anchor;
  } else {
    *from = uw->surrounding_anchor;
    *to = uw->surrounding_cursor;
  }
  return true;
}

/* Finds the bytes uim is asking for. from and to bracket them, and
 * middle is where the origin sits between the two halves. */
static bool
range_of(struct uim_wayland *uw,
         enum UTextArea text_id,
         enum UTextOrigin origin,
         int former_length,
         int latter_length,
         size_t *from,
         size_t *middle,
         size_t *to)
{
  const char *text = uw->surrounding_text;
  size_t floor = 0;
  size_t ceiling;
  size_t cursor;

  if (!text)
    return false;
  if (!supported_extent(former_length) || !supported_extent(latter_length))
    return false;
  ceiling = strlen(text);
  cursor = uw->surrounding_cursor;

  switch (text_id) {
  case UTextArea_Primary:
    break;
  case UTextArea_Selection:
    if (!selection_of(uw, &floor, &ceiling))
      return false;
    cursor = floor;
    break;
  case UTextArea_Clipboard:
    /* The compositor tells us nothing about the clipboard. */
  case UTextArea_Unspecified:
  default:
    return false;
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
    return false;
  }

  *from = step_back(text, *middle, floor, former_length);
  *to = step_forward(text, *middle, ceiling, latter_length);
  return true;
}

int
uim_wayland_text_acquire(void *ptr,
                         enum UTextArea text_id,
                         enum UTextOrigin origin,
                         int former_length,
                         int latter_length,
                         char **former,
                         char **latter)
{
  struct uim_wayland *uw = ptr;
  size_t from;
  size_t middle;
  size_t to;

  if (!range_of(uw, text_id, origin, former_length, latter_length,
                &from, &middle, &to))
    return -1;

  *former = slice(uw->surrounding_text, from, middle);
  *latter = slice(uw->surrounding_text, middle, to);
  return 0;
}

int
uim_wayland_text_delete(void *ptr,
                        enum UTextArea text_id,
                        enum UTextOrigin origin,
                        int former_length,
                        int latter_length)
{
  struct uim_wayland *uw = ptr;
  size_t from;
  size_t middle;
  size_t to;

  if (!uw->context)
    return -1;
  if (!range_of(uw, text_id, origin, former_length, latter_length,
                &from, &middle, &to))
    return -1;
  if (from == to)
    return 0;

  /* The offsets the application gets are relative to its own cursor,
   * which is where uim_wayland_text_set_surrounding() put ours. */
  zwp_input_method_context_v1_delete_surrounding_text(
    uw->context,
    (int32_t)from - (int32_t)uw->surrounding_cursor,
    (uint32_t)(to - from));
  /* A deletion is applied along with the commit that follows it, so
   * send one even though there is nothing to insert. */
  zwp_input_method_context_v1_commit_string(uw->context, uw->serial, "");

  /* What we knew about the text is out of date until the application
   * tells us again. */
  uim_wayland_text_forget_surrounding(uw);
  return 0;
}
