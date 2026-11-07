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
#include <config.h>
#endif

#include "vt.h"
#include "uim-fep.h"

#include <vterm.h>

#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>

#include "escseq.h"
#include "str.h"

/* Growable byte buffer used to build terminal output. */
struct byte_buffer {
  char *data;
  size_t len;
  size_t capacity;
};

static int append_data(struct byte_buffer *buffer, const char *data, size_t len)
{
  size_t capacity;
  char *new_data;

  if (len > SIZE_MAX - buffer->len) {
    return FALSE;
  }
  if (buffer->len + len > buffer->capacity) {
    capacity = buffer->capacity == 0 ? 256 : buffer->capacity;
    while (capacity < buffer->len + len) {
      if (capacity > SIZE_MAX / 2) {
        capacity = buffer->len + len;
        break;
      }
      capacity *= 2;
    }
    new_data = realloc(buffer->data, capacity);
    if (new_data == NULL) {
      return FALSE;
    }
    buffer->data = new_data;
    buffer->capacity = capacity;
  }
  memcpy(buffer->data + buffer->len, data, len);
  buffer->len += len;
  return TRUE;
}

static int append_format(struct byte_buffer *buffer, const char *format, ...)
{
  char data[96];
  va_list args;
  int len;

  va_start(args, format);
  len = vsnprintf(data, sizeof(data), format, args);
  va_end(args);
  if (len < 0 || (size_t)len >= sizeof(data)) {
    return FALSE;
  }
  return append_data(buffer, data, (size_t)len);
}

static int append_codepoint(struct byte_buffer *buffer, uint32_t codepoint)
{
  char data[4];
  size_t len;

  if (codepoint > 0x10ffff ||
      (codepoint >= 0xd800 && codepoint <= 0xdfff)) {
    codepoint = 0xfffd;
  }

  if (codepoint <= 0x7f) {
    data[0] = (char)codepoint;
    len = 1;
  } else if (codepoint <= 0x7ff) {
    data[0] = (char)(0xc0 | (codepoint >> 6));
    data[1] = (char)(0x80 | (codepoint & 0x3f));
    len = 2;
  } else if (codepoint <= 0xffff) {
    data[0] = (char)(0xe0 | (codepoint >> 12));
    data[1] = (char)(0x80 | ((codepoint >> 6) & 0x3f));
    data[2] = (char)(0x80 | (codepoint & 0x3f));
    len = 3;
  } else {
    data[0] = (char)(0xf0 | (codepoint >> 18));
    data[1] = (char)(0x80 | ((codepoint >> 12) & 0x3f));
    data[2] = (char)(0x80 | ((codepoint >> 6) & 0x3f));
    data[3] = (char)(0x80 | (codepoint & 0x3f));
    len = 4;
  }
  return append_data(buffer, data, len);
}

static VTerm *s_vterm;
static VTermScreen *s_screen;
static uint16_t s_cols;
static uint16_t s_rows;
static size_t s_bytes_fed;

static void disable_terminal(const char *operation)
{
  if (s_vterm == NULL) {
    return;
  }
  fprintf(stderr,
          "uim-fep: libvterm %s failed; background restoration disabled\n",
          operation);
  vterm_free(s_vterm);
  s_vterm = NULL;
  s_screen = NULL;
}

static int append_color(struct byte_buffer *buffer,
                        const VTermColor *color,
                        int is_default,
                        int sgr_code,
                        int default_code)
{
  if (is_default) {
    return append_format(buffer, ";%d", default_code);
  }
  if (VTERM_COLOR_IS_INDEXED(color)) {
    return append_format(buffer, ";%d;5;%u", sgr_code,
                         (unsigned int)color->indexed.idx);
  }
  return append_format(buffer, ";%d;2;%u;%u;%u", sgr_code,
                       (unsigned int)color->rgb.red,
                       (unsigned int)color->rgb.green,
                       (unsigned int)color->rgb.blue);
}

static int append_cell_style(struct byte_buffer *buffer,
                             const VTermScreenCell *cell)
{
  const VTermScreenCellAttrs *attrs = &cell->attrs;

  if (!append_data(buffer, "\033[0", 3)) {
    return FALSE;
  }
  if ((attrs->bold && !append_data(buffer, ";1", 2)) ||
      (attrs->italic && !append_data(buffer, ";3", 2)) ||
      (attrs->blink && !append_data(buffer, ";5", 2)) ||
      (attrs->reverse && !append_data(buffer, ";7", 2)) ||
      (attrs->conceal && !append_data(buffer, ";8", 2)) ||
      (attrs->strike && !append_data(buffer, ";9", 2))) {
    return FALSE;
  }
  if (attrs->small && attrs->baseline == VTERM_BASELINE_RAISE) {
    if (!append_data(buffer, ";73", 3)) {
      return FALSE;
    }
  } else if (attrs->small && attrs->baseline == VTERM_BASELINE_LOWER) {
    if (!append_data(buffer, ";74", 3)) {
      return FALSE;
    }
  }
  if (attrs->underline == VTERM_UNDERLINE_SINGLE) {
    if (!append_data(buffer, ";4", 2)) {
      return FALSE;
    }
  } else if (attrs->underline != VTERM_UNDERLINE_OFF) {
    if (!append_format(buffer, ";4:%d", (int)attrs->underline)) {
      return FALSE;
    }
  }
  return append_color(buffer, &cell->fg, VTERM_COLOR_IS_DEFAULT_FG(&cell->fg),
                      38, 39) &&
         append_color(buffer, &cell->bg, VTERM_COLOR_IS_DEFAULT_BG(&cell->bg),
                      48, 49) &&
         append_data(buffer, "m", 1);
}

static int append_cell_text(struct byte_buffer *buffer, const VTermScreenCell *cell)
{
  int i;

  if (cell->chars[0] == 0) {
    return append_data(buffer, " ", 1);
  }
  for (i = 0; i < VTERM_MAX_CHARS_PER_CELL && cell->chars[i] != 0; i++) {
    if (!append_codepoint(buffer, cell->chars[i])) {
      return FALSE;
    }
  }
  return TRUE;
}

/* libvterm 0.3.x reports the right half of a wide character with width 1
 * and chars[0] == (uint32_t)-1, so width alone cannot identify it. */
static int is_wide_tail(const VTermScreenCell *cell)
{
  return cell->chars[0] == (uint32_t)-1;
}

static int get_cell(int row, int col, VTermScreenCell *cell)
{
  VTermPos pos;

  pos.row = row;
  pos.col = col;
  return vterm_screen_get_cell(s_screen, pos, cell);
}

int vt_init(uint16_t cols, uint16_t rows)
{
  const char *encoding = get_enc();
  struct point_tag initial_cursor;
  char cursor_sequence[32];
  int cursor_sequence_len;

  if (strcasecmp(encoding, "UTF-8") != 0 && strcasecmp(encoding, "UTF8") != 0) {
    fprintf(stderr,
            "uim-fep: libvterm background restoration requires a UTF-8 locale\n");
    return FALSE;
  }
  if (cols == 0 || rows == 0) {
    fprintf(stderr, "uim-fep: cannot initialize libvterm with a zero-sized terminal\n");
    return FALSE;
  }
  initial_cursor = get_cursor_position();
  if (initial_cursor.row == UNDEFINED || initial_cursor.col == UNDEFINED) {
    fprintf(stderr,
            "uim-fep: cannot determine initial cursor position; background restoration disabled\n");
    return FALSE;
  }
  s_vterm = vterm_new(rows, cols);
  if (s_vterm == NULL) {
    fprintf(stderr, "uim-fep: libvterm initialization failed\n");
    return FALSE;
  }
  vterm_set_utf8(s_vterm, 1);
  s_screen = vterm_obtain_screen(s_vterm);
  vterm_screen_reset(s_screen, 1);

  cursor_sequence_len = snprintf(cursor_sequence, sizeof(cursor_sequence),
                                 "\033[%d;%dH",
                                 initial_cursor.row + 1,
                                 initial_cursor.col + 1);
  if (cursor_sequence_len < 0 ||
      (size_t)cursor_sequence_len >= sizeof(cursor_sequence)) {
    fprintf(stderr,
            "uim-fep: cannot synchronize libvterm cursor; background restoration disabled\n");
    vterm_free(s_vterm);
    s_vterm = NULL;
    s_screen = NULL;
    return FALSE;
  }
  vterm_input_write(s_vterm, cursor_sequence, (size_t)cursor_sequence_len);
  s_cols = cols;
  s_rows = rows;
  s_bytes_fed = 0;
  return TRUE;
}

void vt_feed(const char *data, size_t len)
{
  if (s_vterm != NULL && len > 0) {
    s_bytes_fed += len;
    vterm_input_write(s_vterm, data, len);
  }
}

void vt_resize(uint16_t cols, uint16_t rows)
{
  if (s_vterm == NULL) {
    return;
  }
  if (cols == 0 || rows == 0) {
    return;
  }
  vterm_set_size(s_vterm, rows, cols);
  s_cols = cols;
  s_rows = rows;
}

int vt_restore_region(int row, int col, int width)
{
  struct byte_buffer buffer = {0};
  struct byte_buffer style = {0};
  struct byte_buffer prev_style = {0};
  struct byte_buffer tmp_style;
  VTermScreenCell cell;
  int result = TRUE;
  int end_col;
  int x;

  if (s_vterm == NULL) {
    return FALSE;
  }
  if (row < 0 || row >= s_rows || width <= 0) {
    return TRUE;
  }
  if (col < 0) {
    width += col;
    col = 0;
  }
  if (col >= s_cols || width <= 0) {
    return TRUE;
  }
  if (col > 0) {
    /* If the range starts at the right half of a wide character,
     * restore from the left half too; otherwise half of the character remains. */
    if (!get_cell(row, col, &cell)) {
      disable_terminal("restore");
      return FALSE;
    }
    if (is_wide_tail(&cell)) {
      col--;
      width++;
    }
  }
  end_col = width > s_cols - col ? s_cols : col + width;

  if (!append_format(&buffer, "\033[%d;%dH", row + 1, col + 1)) {
    goto memory_error;
  }
  for (x = col; x < end_col; x++) {
    if (!get_cell(row, x, &cell)) {
      disable_terminal("restore");
      result = FALSE;
      goto cleanup;
    }
    /* Skip the right half of a wide character: outputting the left half
     * already advances the terminal cursor by two columns. */
    if (is_wide_tail(&cell)) {
      continue;
    }
    style.len = 0;
    if (!append_cell_style(&style, &cell)) {
      goto memory_error;
    }
    /* The terminal keeps the last SGR state, so emit the style only when it
     * differs from the previous cell's style. */
    if (style.len != prev_style.len ||
        memcmp(style.data, prev_style.data, style.len) != 0) {
      if (!append_data(&buffer, style.data, style.len)) {
        goto memory_error;
      }
      tmp_style = prev_style;
      prev_style = style;
      style = tmp_style;
    }
    if (!append_cell_text(&buffer, &cell)) {
      goto memory_error;
    }
  }

  if (buffer.len > 0 &&
      !put_terminal_vt_bytes_preserve_cursor(buffer.data, buffer.len)) {
    result = FALSE;
    goto cleanup;
  }
  goto cleanup;

memory_error:
  disable_terminal("restore");
  result = FALSE;

cleanup:
  free(buffer.data);
  free(style.data);
  free(prev_style.data);
  return result;
}

void vt_quit(void)
{
  if (s_vterm != NULL) {
    vterm_free(s_vterm);
    s_vterm = NULL;
    s_screen = NULL;
  }
}

