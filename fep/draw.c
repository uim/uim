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
 * プリエディットやステータスラインを描画する
 */
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#if (!defined(DEBUG) && !defined(NDEBUG))
#define NDEBUG
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_ASSERT_H
#include <assert.h>
#endif
#include <stdio.h>
#include <limits.h>

#define WINNOSIZE 5
#define MODESIZE 50

#include "uim-fep.h"
#include "callbacks.h"
#include "escseq.h"
#include "draw.h"
#include "vt.h"
#include "str.h"
#include "udsock.h"

/* コミットされてから出力されるまでTRUE */
int g_commit = FALSE;
/* s_preedit->width > 0と同じ */
int g_start_preedit = FALSE;

/* 現在のプリエディット */
static struct preedit_tag *s_preedit;
/* コミットされたときのプリエディットを保存する */
static struct preedit_tag s_save_preedit;
/* プリエディットの先頭の位置 */
static struct point_tag s_head;
/* 行数 -> 行末のカラム */
static int *s_line2width = NULL;
static int *s_prev_line2width = NULL;
/* プリエディットの行数 */
static int s_preedit_lines = 0;
static int s_prev_preedit_lines = 0;

/* 疑似端末マスタのファイル記述子 */
static int s_master;
/* モード状態文字列 */
static char s_modebuf[MODESIZE];
/* 候補一覧 */
static char s_candbuf[CANDSIZE];
/* GNU screenのWINDOW番号 */
static char s_win_no[WINNOSIZE];
/* モード状態を書き込むファイル */
static const char *s_path_getmode;
/* 端末サイズが変換したときTRUE */
static int s_winch = FALSE;

struct candidate_window_layout {
  int row;
  int col;
  int width;
  int height;
  /* Size used for placement, so the window does not move when it gets smaller
     while paging */
  int place_width;
  int place_height;
  int visible_count;
  int selected;
  int total;
  char **items;
  char *footer;
};

/*
 * The cells the candidate window is placed against: the preedit, or the cursor
 * as an empty one-cell preedit before one starts.
 */
struct candidate_anchor {
  struct point_tag head;
  int lines;
  const int *line2width;
  int cursor_col;
};

static struct {
  int visible;
  int row;
  int col;
  int width;
  int height;
} s_candidate_window;

/* Widest item text shown so far in the current candidate list */
static struct {
  int list_id;
  int text_width;
} s_candidate_window_size;

static void init_backtick(void);
static void start_preedit(void);
static void end_preedit(void);
static void draw_statusline(int force, int restore, int visible, int draw_background);
static void restore_candidate_window(void);
static void show_candidate_window(void);
static int render_candidate_window(const struct candidate_window_layout *layout);
static int candidate_window_position_available(const struct candidate_anchor *anchor,
                                               int row, int col, int width, int height);
static void draw_preedit(struct preedit_tag *preedit, struct preedit_tag *prev_preedit);
static int is_eq_region(void);
static void draw_subpreedit(struct preedit_tag *p, int start, int end);
static void draw_pseg(struct preedit_segment_tag *pseg, int start_width);
static int compare_preedit(struct preedit_tag *p1, struct preedit_tag *p2);
static int compare_preedit_rev(struct preedit_tag *p1, struct preedit_tag *p2);
static int line_start_col(int lineno);
static void restore_preedit_region_or_erase(int row, int col, int width, int guard_col);
static void erase_prev_preedit(void);
static void erase_preedit(void);
static void erase_displayed_preedit(const struct preedit_tag *preedit);
static void set_line2width(struct preedit_tag *preedit);
static void goto_char(int width);
static void goto_col(int width);
static struct point_tag width2point_char(int width);
static struct point_tag width2point_col(int width);
static int width2lineno_char(int width);
static int width2lineno_col(int width);

/*
 * Return the current on-screen preedit cursor position.
 */
int get_preedit_cursor_position(int *row, int *col)
{
  struct point_tag dest;

  if (!g_start_preedit || g_opt.no_report_cursor) {
    return FALSE;
  }

  dest = width2point_char(s_preedit->cursor);
  *row = dest.row;
  *col = dest.col;
  return TRUE;
}

/* Returns TRUE if (row, col) is a cell of the preedit text. */
static int is_candidate_window_preedit_cell(int row, int col)
{
  int line;

  for (line = 0; line < s_preedit_lines; line++) {
    int preedit_start = line == 0 ? s_head.col : 0;

    if (row == s_head.row + line && col >= preedit_start &&
        col < s_line2width[line]) {
      return TRUE;
    }
  }
  return FALSE;
}

/*
 * A window edge can cut a wide character whose left half is under the
 * preedit. Restoring or blanking that left half would erase the preedit, so
 * the guard makes vt_restore_region blank only the right half.
 */
static int candidate_window_restore_guard(int row, int col)
{
  if (col > 0 && is_candidate_window_preedit_cell(row, col - 1)) {
    return col - 1;
  }
  return INT_MAX;
}

/*
 * Checks whether the window's left edge at (row, col) cuts a wide character,
 * whose left half (col - 1) then needs a background space. A left half under
 * the preedit is left alone. Returns FALSE if the cell cannot be read.
 */
static int candidate_window_left_background_needed(int row, int col, int *needed)
{
  if (col > 0 && is_candidate_window_preedit_cell(row, col - 1)) {
    *needed = FALSE;
    return TRUE;
  }
  return vt_is_wide_tail(row, col, needed);
}

/*
 * Checks whether the column just past the window's right edge, (row, col), is
 * the right half of a wide character. Its left half is about to be covered,
 * so it needs a background space after the row is drawn. A cell under the
 * preedit is left alone. Returns FALSE if the cell cannot be read.
 */
static int candidate_window_right_background_needed(int row, int col,
                                                   int *needed)
{
  if (is_candidate_window_preedit_cell(row, col)) {
    *needed = FALSE;
    return TRUE;
  }
  return vt_is_wide_tail(row, col, needed);
}

/*
 * The cursor is saved once for all rows, because vt_restore_region moves it.
 */
static int restore_candidate_window_rows(int row, int col, int width, int height)
{
  int i;
  int result = TRUE;

  put_save_cursor();
  for (i = 0; i < height; i++) {
    if (!vt_restore_region(row + i, col, width,
                           candidate_window_restore_guard(row + i, col))) {
      result = FALSE;
      break;
    }
  }
  put_restore_cursor();
  return result;
}

/* Erases the window last drawn, restoring the cells it covered. */
static void restore_candidate_window(void)
{
  if (!s_candidate_window.visible) {
    return;
  }
  restore_candidate_window_rows(s_candidate_window.row, s_candidate_window.col,
                               s_candidate_window.width,
                               s_candidate_window.height);
  s_candidate_window.visible = FALSE;
}

/*
 * Returns TRUE if a window of width x height at (row, col) fits on the screen
 * and does not cover the anchor.
 */
static int candidate_window_position_available(const struct candidate_anchor *anchor,
                                               int row, int col, int width, int height)
{
  int line;

  if (row < 0 || col < 0 || row + height > g_win->ws_row ||
      col + width > g_win->ws_col) {
    return FALSE;
  }
  for (line = 0; line < anchor->lines; line++) {
    int preedit_row = anchor->head.row + line;
    int preedit_start = line == 0 ? anchor->head.col : 0;
    int preedit_end = anchor->line2width[line];

    if (preedit_row >= row && preedit_row < row + height &&
        col < preedit_end && col + width > preedit_start) {
      return FALSE;
    }
  }
  return TRUE;
}

/*
 * Returns the column where the window starts, following
 * candidate-window-position.
 */
static int get_candidate_window_base_col(const struct candidate_anchor *anchor)
{
  switch (g_opt.candidate_window_position) {
  case CANDWIN_POS_LEFT:
    return anchor->head.col;
  default:
    return anchor->cursor_col;
  }
}

/*
 * Places the window left of the anchor start, or right of the anchor end if
 * the left side does not fit. If neither side fits at full width, the wider
 * side is used (left on a tie) and the window is narrowed to its free columns.
 */
static int place_candidate_window_beside_preedit(
  const struct candidate_anchor *anchor, struct candidate_window_layout *layout)
{
  int end_row = anchor->head.row;
  int end_col = anchor->line2width[0];
  int start_row = anchor->head.row;
  int max_row = g_win->ws_row - layout->place_height;
  int start_col;
  int end_avail = g_win->ws_col - end_col;
  int start_avail = anchor->head.col;
  int use_start;
  int i;

  end_row = min(end_row, max_row);
  start_row = min(start_row, max_row);
  start_col = anchor->head.col - layout->place_width;

  for (i = 0; i < 2; i++) {
    use_start = i == 0;
    if (candidate_window_position_available(
          anchor,
          use_start ? start_row : end_row,
          use_start ? start_col : end_col,
          layout->place_width, layout->place_height)) {
      layout->row = use_start ? start_row : end_row;
      layout->col = use_start ? start_col : end_col;
      return TRUE;
    }
  }

  use_start = start_avail >= end_avail;
  layout->place_width = min(use_start ? start_avail : end_avail,
                            layout->place_width);
  if (layout->place_width < WINDOW_MIN_WIDTH) {
    return FALSE;
  }
  layout->width = min(layout->width, layout->place_width);
  layout->row = use_start ? start_row : end_row;
  layout->col = use_start ? anchor->head.col - layout->place_width : end_col;
  return candidate_window_position_available(anchor, layout->row, layout->col,
                                            layout->place_width,
                                            layout->place_height);
}

/* Returns the number of decimal digits in the non-negative integer n. */
static int decimal_width(int n)
{
  int width = 1;
  while (n >= 10) {
    n /= 10;
    width++;
  }
  return width;
}

/*
 * Fills anchor with the cells the window is placed against: the preedit once
 * one has started, and the cursor as an empty one-cell preedit before that.
 * Returns FALSE if the cursor position is unknown.
 */
static int get_candidate_anchor(struct candidate_anchor *anchor)
{
  int row;

  if (g_start_preedit) {
    anchor->head = s_head;
    anchor->lines = s_preedit_lines;
    anchor->line2width = s_line2width;
    return get_preedit_cursor_position(&row, &anchor->cursor_col);
  }

  anchor->head = get_cursor_position();
  if (anchor->head.row == UNDEFINED || anchor->head.col == UNDEFINED) {
    return FALSE;
  }
  anchor->cursor_col = anchor->head.col;
  anchor->lines = 1;
  /* The line ends at the cursor column, so its width is cursor_col. */
  anchor->line2width = &anchor->cursor_col;
  return TRUE;
}

/*
 * Draws the candidate window for the current page of candidates.
 * The window goes below the preedit, above it, or beside the cursor,
 * whichever fits first. It is silently skipped when it cannot be shown
 * (no reported cursor position, terminal too small, or no page).
 */
static void show_candidate_window(void)
{
  struct candidate_window_layout layout = {0};
  struct candidate_anchor anchor;
  int page_candidate_count;
  int selected;
  int page_first;
  int max_text_width;
  int page_size;
  int list_id;
  int visible_max;
  int col;
  int i;

  if (g_opt.no_report_cursor ||
      g_win->ws_row < WINDOW_MIN_ROWS ||
      g_win->ws_col < WINDOW_MIN_WIDTH ||
      !get_candidate_window_page(&page_first, &page_candidate_count,
                                &selected, &layout.total) ||
      page_candidate_count <= 0 ||
      !get_candidate_anchor(&anchor)) {
    return;
  }

  get_candidate_window_list_info(&list_id, &page_size);
  /* A new candidate list starts with a fresh width */
  if (list_id != s_candidate_window_size.list_id) {
    s_candidate_window_size.list_id = list_id;
    s_candidate_window_size.text_width = 0;
  }

  /* visible_max is the page capacity; visible_count is the rows this page
     fills */
  visible_max = min(page_size, layout.total);
  layout.visible_count = min(page_candidate_count, visible_max);
  if (selected != UNDEFINED &&
      (selected < 0 || selected >= page_candidate_count)) {
    selected = UNDEFINED;
  }
  layout.selected = selected;
  if (g_opt.ddskk) {
    uim_asprintf(&layout.footer, "[%s %d]", get_candidate_nokori_str(),
                 layout.total - page_first - layout.visible_count);
  } else {
    /* Pad to the page's largest number so the footer width stays fixed on 9 -> 10 */
    int index_width = decimal_width(page_first + page_candidate_count);

    if (selected == UNDEFINED) {
      uim_asprintf(&layout.footer, "%*s / %d", index_width, "-", layout.total);
    } else {
      uim_asprintf(&layout.footer, "%*d / %d", index_width,
                   page_first + selected + 1, layout.total);
    }
  }
  max_text_width = strwidth(layout.footer);

  layout.items = uim_malloc(sizeof(char *) * layout.visible_count);
  memset(layout.items, 0, sizeof(char *) * layout.visible_count);
  for (i = 0; i < layout.visible_count; i++) {
    int width;
    layout.items[i] = get_candidate_window_item(page_first + i);
    if (layout.items[i] == NULL) {
      goto done;
    }
    width = strwidth(layout.items[i]);
    max_text_width = max(max_text_width, width);
  }
  /* Keep the widest width seen in this list, so the window does not shrink
     while paging */
  s_candidate_window_size.text_width = max(s_candidate_window_size.text_width,
                                           max_text_width);
  layout.place_width = min(s_candidate_window_size.text_width + WINDOW_FRAME_COLS,
                           g_win->ws_col);
  /* Reserve a full page of height so the position does not move on a shorter
     last page */
  layout.place_height = visible_max + WINDOW_FOOTER_ROWS + WINDOW_BORDER_ROWS;
  layout.width = min(max_text_width + WINDOW_FRAME_COLS, layout.place_width);
  layout.height = layout.visible_count + WINDOW_FOOTER_ROWS + WINDOW_BORDER_ROWS;

  /* Try below the preedit, then above it, then beside the cursor */
  col = get_candidate_window_base_col(&anchor);
  col = min(col, g_win->ws_col - layout.place_width);
  if (candidate_window_position_available(
        &anchor, anchor.head.row + anchor.lines, col,
        layout.place_width, layout.place_height)) {
    layout.row = anchor.head.row + anchor.lines;
    layout.col = col;
  } else if (candidate_window_position_available(
               &anchor, anchor.head.row - layout.place_height, col,
               layout.place_width, layout.place_height)) {
    layout.row = anchor.head.row - layout.place_height;
    layout.col = col;
  } else if (!place_candidate_window_beside_preedit(&anchor, &layout)) {
    goto done;
  }

  render_candidate_window(&layout);

  /* items and footer are per-call buffers, so free them on every path */
done:
  if (layout.items != NULL) {
    for (i = 0; i < layout.visible_count; i++) {
      free(layout.items[i]);
    }
    free(layout.items);
  }
  free(layout.footer);
}

struct window_frame_glyphs {
  const char *vertical;
  const char *horizontal;
  const char *top_left;
  const char *top_right;
  const char *bottom_left;
  const char *bottom_right;
};

static const struct window_frame_glyphs s_box_frame_glyphs = {
  "│", "─", "╭", "╮", "╰", "╯"
};

/* ASCII glyphs are one column wide even where box drawing is ambiguous-width. */
static const struct window_frame_glyphs s_ascii_frame_glyphs = {
  "|", "-", "+", "+", "+", "+"
};

/*
 * Returns the ASCII border glyphs if the ascii window border option is set,
 * and the box-drawing glyphs otherwise.
 */
static const struct window_frame_glyphs *get_window_frame_glyphs(void)
{
  return g_opt.ascii_window_border ? &s_ascii_frame_glyphs : &s_box_frame_glyphs;
}

/*
 * Draws a row of the window at (row, col) spanning width cells, with text in
 * attr. If framed, the text is bracketed by vertical borders. A wide character
 * cut by either edge is repainted: a background space is written at col - 1
 * before the row, and at col + width after it.
 */
static int draw_candidate_window_row(int row, int col, int width,
                                     const char *text, int attr, int framed)
{
  const char *vertical = get_window_frame_glyphs()->vertical;
  int left_background = FALSE;
  int right_background = FALSE;
  int ok;

  if (!candidate_window_left_background_needed(row, col, &left_background) ||
      !candidate_window_right_background_needed(row, col + width,
                                                &right_background)) {
    return FALSE;
  }
  put_cursor_address(row, left_background ? col - 1 : col);
  ok = !left_background || vt_put_background_space(row, col - 1);
  if (ok) {
    if (framed) {
      put_uim_str(vertical, UPreeditAttr_None);
    }
    put_uim_str(text, attr);
    if (framed) {
      put_uim_str(vertical, UPreeditAttr_None);
    }
    ok = !right_background || vt_put_background_space(row, col + width);
  }
  return ok;
}

/*
 * Draws one body row of the window at (row, col): text clipped to the inner
 * width, padded with spaces, and framed by the vertical borders. attr sets
 * the text style (reverse video for the selected candidate).
 */
static int render_candidate_window_line(int row, int col, int width,
                                       const char *text, int attr)
{
  char *clipped = uim_strdup(text);
  char *line;
  int content_width = width - WINDOW_FRAME_COLS;
  int text_width = strwidth(clipped);
  int padding;
  size_t text_bytes;
  size_t line_bytes;
  int ok;

  if (text_width > content_width) {
    text_width = strhead(clipped, content_width);
  }
  padding = content_width - text_width;
  text_bytes = strlen(clipped);
  line_bytes = text_bytes + (size_t)padding + 3;
  line = uim_malloc(line_bytes);
  line[0] = ' ';
  memcpy(line + 1, clipped, text_bytes);
  memset(line + 1 + text_bytes, ' ', (size_t)padding + 1);
  line[line_bytes - 1] = '\0';

  ok = draw_candidate_window_row(row, col, width, line, attr, TRUE);
  free(line);
  free(clipped);
  return ok;
}

/*
 * Draws a horizontal border row of the window at (row, col), using the given
 * left and right corner glyphs and filling the width with horizontal glyphs.
 */
static int render_candidate_window_border(int row, int col, int width,
                                         const char *left, const char *right)
{
  const char *horizontal = get_window_frame_glyphs()->horizontal;
  size_t left_len = strlen(left);
  size_t horizontal_len = strlen(horizontal);
  size_t right_len = strlen(right);
  int fill = width - WINDOW_BORDER_COLS;
  size_t line_len = left_len + (size_t)fill * horizontal_len + right_len;
  char *line = uim_malloc(line_len + 1);
  size_t offset = 0;
  int i;
  int ok;

  memcpy(line + offset, left, left_len);
  offset += left_len;
  for (i = 0; i < fill; i++) {
    memcpy(line + offset, horizontal, horizontal_len);
    offset += horizontal_len;
  }
  memcpy(line + offset, right, right_len);
  offset += right_len;
  line[offset] = '\0';

  ok = draw_candidate_window_row(row, col, width, line, UPreeditAttr_None,
                                 FALSE);
  free(line);
  return ok;
}

/*
 * Draws the window described by layout: top border, the visible candidates
 * (the selected one in reverse video), the footer, and the bottom border.
 * Records the drawn area so it can be erased later. On a write error, the
 * partial window is erased and FALSE is returned.
 */
static int render_candidate_window(const struct candidate_window_layout *layout)
{
  const struct window_frame_glyphs *glyphs = get_window_frame_glyphs();
  int row = layout->row + 1;
  int i;

  s_candidate_window.visible = TRUE;
  s_candidate_window.row = layout->row;
  s_candidate_window.col = layout->col;
  s_candidate_window.width = layout->width;
  s_candidate_window.height = layout->height;
  put_save_cursor();
  put_cursor_invisible();
  if (!render_candidate_window_border(layout->row, layout->col, layout->width,
                                      glyphs->top_left, glyphs->top_right)) {
    goto output_error;
  }
  for (i = 0; i < layout->visible_count; i++) {
    int selected = layout->selected != UNDEFINED && i == layout->selected;
    if (!render_candidate_window_line(row + i, layout->col, layout->width,
                                      layout->items[i],
                                      selected ? UPreeditAttr_Reverse :
                                      UPreeditAttr_None)) {
      goto output_error;
    }
  }
  if (!render_candidate_window_line(row + layout->visible_count,
                                    layout->col, layout->width,
                                    layout->footer, UPreeditAttr_None)) {
    goto output_error;
  }
  if (!render_candidate_window_border(layout->row + layout->height - 1,
                                      layout->col, layout->width,
                                      glyphs->bottom_left, glyphs->bottom_right)) {
    goto output_error;
  }
  put_restore_cursor();
  put_cursor_normal();
  return TRUE;

output_error:
  put_restore_cursor();
  put_cursor_normal();
  restore_candidate_window();
  return FALSE;
}

/* Redraws the candidate window for the current state, if --candidate-window is enabled. */
void draw_candidate_window_refresh(void)
{
  if (!g_opt.candidate_window) {
    return;
  }
  restore_candidate_window();
  show_candidate_window();
}

/* Erases the candidate window, if one is shown. */
void draw_candidate_window_clear(void)
{
  restore_candidate_window();
}

#if defined(DEBUG) && DEBUG > 2
static void print_preedit(struct preedit_tag *p);
#endif

void init_draw(int master, const char *path_getmode)
{
  s_master = master;
  s_path_getmode = path_getmode;
  s_preedit = create_preedit();
  if (g_opt.status_type == BACKTICK) {
    init_backtick();
  }
}

void write_getmode(int mode)
{
  FILE *fp;

  if (s_path_getmode == NULL || s_path_getmode[0] == '\0') {
    return;
  }

  fp = fopen(s_path_getmode, "wt");
  if (fp != NULL) {
    fprintf(fp, "%d\n", mode);
    fclose(fp);
  }
}

static void init_backtick(void)
{
  if (getenv("WINDOW")) {
    snprintf(s_win_no, sizeof(s_win_no), "%s ", getenv("WINDOW"));
  }
}

void update_backtick(void)
{
  char sendbuf[CANDSIZE];
  if (s_candbuf[0] == '\0') {
    /* モード表示 */
    snprintf(sendbuf, sizeof(sendbuf), "%s%s", s_win_no, s_modebuf);
  } else {
    /* 候補一覧表示 */
    strlcpy(sendbuf,  s_candbuf, sizeof(sendbuf));
  }
  sendline(sendbuf);
}

/*
 * プリエディット，ステータスラインを描画する
 * 描写する必要がない場合はFALSEを返す
 */
int draw(void)
{
  char *commit_str;

  struct preedit_tag *prev_preedit;

  int i;

  if (!end_callbacks()) {
    return FALSE;
  }

  restore_candidate_window();

  prev_preedit = s_preedit;
  s_preedit = get_preedit();
  commit_str = get_commit_str();

  /* 端末サイズが変更されたときはs_headを変更し、前のpreeditがなかったことにする */
  if (s_winch && g_start_preedit) {
    if (g_opt.no_report_cursor) {
      s_preedit->cursor = 0;
    } else {
      s_head = get_cursor_position();
    }
    end_preedit();
    free_preedit(prev_preedit);
    prev_preedit = create_preedit();
  }
  s_winch = FALSE;

  debug2(("draw()\n"));
  debug2(("commit_str = \"%s\"\n", commit_str));
  debug2(("preedit->width = %d\n", s_preedit->width));

  /* 保存しておいたプリエディットはもういらない */
  for (i = 0; i < s_save_preedit.nr_psegs; i++) {
    free(s_save_preedit.pseg[i].str);
  }
  s_save_preedit.width = s_save_preedit.cursor = s_save_preedit.nr_psegs = 0;

  /* プリエディットが無ければカーソルを戻す */
  draw_statusline(FALSE, !g_start_preedit || g_opt.no_report_cursor, FALSE, FALSE);

  /* コミットされたか */
  if (commit_str[0] != '\0') {
    g_commit = TRUE;
    /* プリエディットを消す必要があるか */
    if (prev_preedit->width > 0) {
      put_cursor_invisible();
      erase_displayed_preedit(prev_preedit);
      end_preedit();
    }
    write(s_master, commit_str, strlen(commit_str));
  }
  if (!g_commit) {
    /* 描画または消去するプリエディットがあるか */
    if (s_preedit->width != 0 || prev_preedit->width != 0) {
      if (prev_preedit->width == 0) {
        start_preedit();
      }
      draw_preedit(s_preedit, prev_preedit);
      if (s_preedit->width == 0) {
        end_preedit();
      }
    }
  } else { /* if (g_commit) */
    if (s_preedit->width > 0) {
      /* コミットされたときにプリエディットがあったので，後から出力す
       * るためにプリエディットを保存する  */
      s_save_preedit = *s_preedit;
    }
    s_preedit->width = s_preedit->cursor = s_preedit->nr_psegs = 0;
  }

  if (g_opt.candidate_window) {
    show_candidate_window();
  }

  free_preedit(prev_preedit);
  free(commit_str);
  put_cursor_normal();

  debug2(("\ndraw end\n"));
  return TRUE;
}

/*
 * プリエディットを開始するときに呼ぶ
 */
static void start_preedit(void)
{
  if (!g_start_preedit) {
    debug2(("start_preedit()\n"));
    g_start_preedit = TRUE;
    if (g_opt.no_report_cursor) {
      return;
    }

    s_head = get_cursor_position();
    if (s_head.col == g_win->ws_col - 1) {
      s_head.col = 0;
      if (s_head.row == g_win->ws_row - 1) {
        put_crlf();
        vt_scroll_up(s_head.row, s_head.col);
      } else {
        s_head.row++;
      }
    }
    debug2(("s_head.row = %d s_head.col = %d\n", s_head.row, s_head.col));
    s_line2width = uim_malloc(sizeof(int));
    s_line2width[0] = s_head.col;
    s_preedit_lines = 1;
  }
}

/*
 * プリエディットを終了するときに呼ぶ
 * カーソルはプリエディット開始位置に移動
 */
static void end_preedit(void)
{
  debug2(("end_preedit()\n"));
  assert(g_start_preedit);
  g_start_preedit = FALSE;
  if (g_opt.no_report_cursor) {
    return;
  }

  put_cursor_address_p(&s_head);
  s_preedit_lines = 0;
  if (s_line2width != NULL) {
    free(s_line2width);
    s_line2width = NULL;
  }
  if (s_prev_line2width != NULL) {
    free(s_prev_line2width);
    s_prev_line2width = NULL;
  }
}

/*
 * コミットとプリエディットが同時に発生し，
 * その後，コミットを出力したときにTRUEを返す
 */
int is_commit_and_preedit(void)
{
  return !g_commit && s_save_preedit.width > 0;
}

/*
 * Return TRUE if a new preedit started with a commit is still waiting to
 * be shown by draw_commit_and_preedit(). Unlike is_commit_and_preedit(),
 * this is TRUE even before g_commit is reset.
 */
int has_pending_preedit(void)
{
  return s_save_preedit.width > 0;
}

/*
 * is_commit_and_preedit() == TRUEのときに呼ばれ，
 * プリエディットを出力する
 */
void draw_commit_and_preedit(void)
{
  assert(is_commit_and_preedit());
  assert(s_preedit->width == 0);
  *s_preedit = s_save_preedit;
  s_save_preedit.width = 0;
  s_save_preedit.cursor = 0;
  s_save_preedit.nr_psegs = 0;
  start_preedit();
  /* prev_preeditは空 */
  draw_preedit(s_preedit, &s_save_preedit);
  put_cursor_normal();
}

/*
 * ステータスラインを描画する
 * forceがTRUEのときは状態が変わってなくても描画する
 * restoreがTRUEのときはカーソルを開始位置に戻す
 * visibleがTRUEのときは終了後にカーソルを表示する
 * draw_backgroundがTRUEのときは背景色で塗りつぶす
 */
static void draw_statusline(int force, int restore, int visible, int draw_background)
{
  static char *statusline_str = NULL;
  static int statusline_str_width = 0;
  static char *candidate_str = NULL;
  static int candidate_col = UNDEFINED;
  static char *mode_str = NULL;
  static char *index_str = NULL;
  static int index_col = UNDEFINED;

  char *prev_statusline_str;
  int prev_statusline_str_width;
  char *prev_candidate_str;
  int prev_candidate_col;
  char *prev_mode_str;
  char *prev_index_str;
  int prev_index_col;

  /* static変数の初期化 1回しか実行されない */
  if (statusline_str == NULL) {
    statusline_str = uim_strdup("");
  }
  if (candidate_str == NULL) {
    candidate_str = uim_strdup("");
  }
  if (index_str == NULL) {
    index_str = uim_strdup("");
  }
  if (mode_str == NULL) {
    mode_str = uim_strdup("");
  }

  prev_statusline_str = statusline_str;
  prev_statusline_str_width = statusline_str_width;
  prev_candidate_str = candidate_str;
  prev_candidate_col = candidate_col;
  prev_mode_str = mode_str;
  prev_index_str = index_str;
  prev_index_col = index_col;

  mode_str = get_mode_str();
  /* With --candidate-window, candidates are shown only in the window, never in the statusline */
  if (g_opt.candidate_window) {
    statusline_str = uim_strdup("");
    candidate_str = uim_strdup("");
    candidate_col = UNDEFINED;
    index_str = uim_strdup("");
    index_col = UNDEFINED;
  } else {
    statusline_str = get_statusline_str();
    candidate_str = get_candidate_str();
    candidate_col = get_candidate_col();
    index_str = get_index_str();
    index_col = get_index_col();
  }

  debug2(("draw_statusline()\n"));
  debug2(("statusline_str = \"%s\"\n", statusline_str));
  debug2(("candidate_str = \"%s\"\n", candidate_str));
  debug2(("candidate_col = %d\n", candidate_col));
  debug2(("prev_mode_str = %s\n", prev_mode_str));
  debug2(("mode_str = %s\n", mode_str));
  debug2(("index_str = \"%s\"\n", index_str));
  debug2(("index_col = %d\n", index_col));

  /* 候補一覧を消去 */
  if (statusline_str[0] == '\0' && prev_statusline_str[0] != '\0') {
    if (g_opt.status_type == LASTLINE) {
      /* 候補一覧を消した後はモードを描画する必要がある */
      force = TRUE;
    } else if (g_opt.status_type == BACKTICK) {
      s_candbuf[0] = '\0';
    }
  } else {
    /* 新しい候補一覧か */ 
    if (strcmp(statusline_str, prev_statusline_str) != 0 || (force && statusline_str[0] != '\0')) {
      /* 新しい候補一覧なので前回の候補はない */
      prev_candidate_col = UNDEFINED;
      prev_index_col = UNDEFINED;
      if (g_opt.status_type == LASTLINE) {
        if (restore) {
          put_save_cursor();
        }
        put_cursor_invisible();
        put_goto_lastline(0);
        /* 候補が選択されているか */
        if (candidate_col != UNDEFINED) {
          int byte_cand = (width2byte(statusline_str, candidate_col))[0];
          int byte_index;
          put_uim_str_len(statusline_str, UPreeditAttr_None, byte_cand);
          put_uim_str(candidate_str, UPreeditAttr_Reverse);
          if (index_col == UNDEFINED) {
            put_uim_str(statusline_str + byte_cand + strlen(candidate_str), UPreeditAttr_None);
          } else {
            if (g_opt.ddskk) {
              put_uim_str(statusline_str + byte_cand + strlen(candidate_str), UPreeditAttr_None);
            } else {
              byte_index = (width2byte(statusline_str, index_col))[0];
              put_uim_str_len(statusline_str + byte_cand + strlen(candidate_str),
                  UPreeditAttr_None,
                  byte_index - byte_cand - strlen(candidate_str));
              put_uim_str(index_str, UPreeditAttr_None);
              put_uim_str(statusline_str + byte_index + strlen(index_str), UPreeditAttr_None);
            }
          }
        } else {
          put_uim_str(statusline_str, UPreeditAttr_None);
        }
        statusline_str_width = strwidth(statusline_str);
        if (draw_background) {
          put_clear_to_end_of_line(g_win->ws_col - statusline_str_width);
        } else if (statusline_str_width < prev_statusline_str_width) {
          put_clear_to_end_of_line(prev_statusline_str_width - statusline_str_width);
        }
        goto end_candidate;
      } else if (g_opt.status_type == BACKTICK) {
        strlcpy(s_candbuf, statusline_str, CANDSIZE);
      }
    }
    if (prev_candidate_col != candidate_col) {
      /* 前回の候補の反転を戻す */
      if (prev_candidate_col != UNDEFINED) {
        if (g_opt.status_type == LASTLINE) {
          if (restore) {
            put_save_cursor();
          }
          put_cursor_invisible();
          put_goto_lastline(prev_candidate_col);
          put_uim_str(prev_candidate_str, UPreeditAttr_None);
        }
      }
      /* 選択された候補を反転する */
      if (candidate_col != UNDEFINED) {
        if (g_opt.status_type == LASTLINE) {
          if (restore) {
            put_save_cursor();
          }
          put_cursor_invisible();
          put_goto_lastline(candidate_col);
          put_uim_str(candidate_str, UPreeditAttr_Reverse);
        } else if (g_opt.status_type == BACKTICK) {
          int byte;
          strlcpy(s_candbuf, statusline_str, CANDSIZE);
          byte = (width2byte(statusline_str, candidate_col))[0] + strlen(candidate_str);
          if (0 <= byte && byte <= CANDSIZE - 1) {
            s_candbuf[byte] = ']';
          }
          byte -= (strlen(candidate_str) + 1);
          if (0 <= byte && byte <= CANDSIZE - 1) {
            s_candbuf[byte] = '[';
          }
        }
      }
    }
    if (index_col != UNDEFINED && !g_opt.ddskk) {
      if (g_opt.status_type == LASTLINE) {
        int i = 0;
        if (restore) {
          put_save_cursor();
        }
        put_cursor_invisible();
        /* index_strはascii */
        if (prev_index_col != UNDEFINED) {
          for (i = 0; i < (int)strlen(index_str); i++) {
            if (index_str[i] != prev_index_str[i]) {
              break;
            }
          }
        }
        if (i < (int)strlen(index_str)) {
          put_goto_lastline(index_col + i);
          put_uim_str(index_str + i, UPreeditAttr_None);
        }
      } else if (g_opt.status_type == BACKTICK) {
        memcpy(s_candbuf + (width2byte(statusline_str, index_col))[0], index_str, strlen(index_str));
      }
    }
  }
end_candidate:

  if (force || strcmp(mode_str, prev_mode_str) != 0) {

    /* 現在のモードをUIM_FEP_GETMODEに書き込む */
    write_getmode(get_mode());

    if (g_opt.status_type != NONE && statusline_str[0] == '\0') {
      if (g_opt.status_type == LASTLINE) {
        int mode_str_width = strwidth(mode_str);

        statusline_str_width = mode_str_width;

        if (restore) {
          put_save_cursor();
        }
        put_cursor_invisible();

        /* draw_background ならば force である */
        /* 論理的には関係ないがそのような使われ方しかしていない */
        assert(!draw_background || force);

        if (force) {
          put_goto_lastline(0);
          put_uim_str(mode_str, UPreeditAttr_None);

          if (draw_background) {
            put_clear_to_end_of_line(g_win->ws_col - statusline_str_width);
          } else if (statusline_str_width < prev_statusline_str_width) {
            put_clear_to_end_of_line(prev_statusline_str_width - statusline_str_width);
          }

        } else {
          /* !force なので prev_statusline_str_width はモード表示の長さである */
          int eq_width = compare_str(mode_str, prev_mode_str);
          int eq_byte = width2byte(mode_str, eq_width)[0];
          int prev_mode_str_width = strwidth(prev_mode_str);

          put_goto_lastline(eq_width);
          if (mode_str_width == prev_mode_str_width) {
            int eq_width_rev = compare_str_rev(mode_str, prev_mode_str);

            if (eq_width_rev > 0) {
              int draw_byte = width2byte(mode_str + eq_byte, mode_str_width - eq_width - eq_width_rev)[0];
              put_uim_str_len(mode_str + eq_byte, UPreeditAttr_None, draw_byte);

            } else {
              put_uim_str(mode_str + eq_byte, UPreeditAttr_None);
            }
            
          } else {
            put_uim_str(mode_str + eq_byte, UPreeditAttr_None);
            if (statusline_str_width < prev_statusline_str_width) {
              put_clear_to_end_of_line(prev_statusline_str_width - statusline_str_width);
            }
          }
        }

      } else if (g_opt.status_type == BACKTICK) {
        strlcpy(s_modebuf, mode_str, sizeof(s_modebuf));
      }
    }
  }
  free(prev_candidate_str);
  free(prev_statusline_str);
  free(prev_index_str);
  free(prev_mode_str);
  if (restore) {
    put_restore_cursor();
  }
  if (g_opt.status_type == BACKTICK) {
    update_backtick();
  }
  if (visible) {
    put_cursor_normal();
  }
  debug2(("draw_statusline end\n"));
}

/*
 * ステータスラインのモード表示をmodeにする
 * カーソル位置は変わらない
 */
void draw_statusline_restore(void)
{
  if (!end_callbacks()) {
    if (g_opt.status_type == BACKTICK) {
      update_backtick();
    }
    return;
  }
  draw_statusline(FALSE, TRUE, TRUE, FALSE);
}

/*
 * 最下行を再描画する
 * カーソルは最下行に移動する
 */
void draw_statusline_force_no_restore(void)
{
  end_callbacks();
  draw_statusline(TRUE, FALSE, FALSE, TRUE);
}

/*
 * 最下行を再描画する
 * カーソル位置は変わらない
 */
void draw_statusline_force_restore(void)
{
  end_callbacks();
  draw_statusline(TRUE, TRUE, TRUE, TRUE);
}

/*
 * Erase the display state owned by uim-fep before switching to transparent
 * recovery mode.
 */
void recover_display(void)
{
  draw_candidate_window_clear();
  if (g_start_preedit) {
    put_cursor_invisible();
    erase_displayed_preedit(s_preedit);
    end_preedit();
    put_cursor_normal();
  }

  if (g_opt.status_type == BACKTICK) {
    clear_backtick();
  }
}

/*
 * 最下行を消す
 * カーソルは最下行に移動する
 */
void clear_lastline(void)
{
  assert(g_opt.status_type == LASTLINE);
  put_goto_lastline(0);
  put_clear_to_end_of_line(g_win->ws_col);
}

/*
 * backtickを消す
 */
void clear_backtick(void)
{
  assert(g_opt.status_type == BACKTICK);
  sendline("");
}

/*
 * プリエディットを描画する
 * 開始時のカーソル位置は任意
 * 終了時のカーソル位置はpreedit->cursor
 */
static void draw_preedit(struct preedit_tag *preedit, struct preedit_tag *prev_preedit)
{
  int eq_width;

  /* 端末サイズが変更されたときはprev_preeditは無視する */
  eq_width = compare_preedit(preedit, prev_preedit);

#if DEBUG > 2
  debug2(("\neq_width = %d\n", eq_width));
  debug2(("prev    "));
  print_preedit(prev_preedit);
  debug2(("\n"));
  debug2(("preedit "));
  print_preedit(preedit);
  debug2(("\n"));
#endif

  /* プリエディットの末尾以外を編集しているときかprev_preeditを変更するときはカーソルを消す */
  if (preedit->cursor != preedit->width || eq_width != prev_preedit->width) {
    put_cursor_invisible();
  }

  /* preedit == prev_preeditのときは、カーソルの移動だけ */
  if (eq_width == preedit->width && eq_width == prev_preedit->width && eq_width > 0) {
    if (g_opt.no_report_cursor) {
      put_move_cur(prev_preedit->cursor, preedit->cursor);
    } else {
      goto_char(preedit->cursor);
    }
    return;
  }

  if (!g_opt.no_report_cursor) {
    set_line2width(preedit);
  }

  /* 出力する位置に移動 */
  if (g_opt.no_report_cursor) {
    put_move_cur(prev_preedit->cursor, eq_width);
  } else {
    goto_col(eq_width);
  }

  /* 領域が変わっていないので変更部分だけ上書き */
  if ((g_opt.no_report_cursor && preedit->width == prev_preedit->width) || (!g_opt.no_report_cursor && is_eq_region())) {
    int eq_width_rev = compare_preedit_rev(preedit, prev_preedit);
    debug2(("eq_width_rev = %d\n", eq_width_rev));
    draw_subpreedit(preedit, eq_width, preedit->width - eq_width_rev);
    if (g_opt.no_report_cursor) {
      put_move_cur(preedit->width - eq_width_rev, preedit->cursor);
    } else {
      goto_char(preedit->cursor);
    }
    return;
  }

  if (g_opt.no_report_cursor && g_opt.on_the_spot && preedit->width > prev_preedit->width) {
    put_insert(preedit->width - prev_preedit->width);
  }
  draw_subpreedit(preedit, eq_width, preedit->width);

  if (g_opt.no_report_cursor) {
    if (preedit->width > prev_preedit->width) {
      put_cursor_left(preedit->width - preedit->cursor);
    } else {
      if (g_opt.on_the_spot) {
        put_delete(prev_preedit->width - preedit->width);
        put_cursor_left(preedit->width - preedit->cursor);
      } else {
        put_erase(prev_preedit->width - preedit->width);
        put_cursor_left(prev_preedit->width - preedit->cursor);
      }
    }
  } else {
    erase_prev_preedit();
    /* カーソルの位置に移動 */
    goto_char(preedit->cursor);
  }

}

static int is_eq_region(void)
{
  int lineno;
  if (s_preedit_lines != s_prev_preedit_lines) {
    return FALSE;
  }
  for (lineno = 0; lineno < s_preedit_lines; lineno++) {
    if (s_line2width[lineno] != s_prev_line2width[lineno]) {
      return FALSE;
    }
  }
  return TRUE;
}

/*
 * pの幅startの次の文字から幅endの文字まで出力する。
 */
static void draw_subpreedit(struct preedit_tag *p, int start, int end)
{
  int i = 0;
  int w = 0;
  int byte_offset = 0;
  int width = end - start;
  int save_i;

  if (width <= 0 || p->width <= start) {
    return;
  }

  /* startがどこかを調べる */
  if (start != 0) {
    for (i = 0; i < p->nr_psegs; i++) {
      char *seg_str = p->pseg[i].str;
      int seg_w = strwidth(seg_str);
      if (w + seg_w == start) {
        i++;
        break;
      } else if (w + seg_w > start) {
        byte_offset = (width2byte(seg_str, start - w))[0];
        break;
      }
      w += seg_w;
    }
  }

  assert(i < p->nr_psegs);

  w = 0;

  save_i = i;
  p->pseg[save_i].str += byte_offset;
  for (; i < p->nr_psegs; i++) {
    char *seg_str = p->pseg[i].str;
    int seg_w = strwidth(seg_str);
    if (w + seg_w <= width) {
      if (g_opt.no_report_cursor) {
        put_uim_str(seg_str, p->pseg[i].attr);
      } else {
        draw_pseg(&(p->pseg[i]), start + w);
      }
      w += seg_w;
      if (w == width) {
        break;
      }
    } else {
      int *byte_width = width2byte(seg_str, width - w);
      int byte = byte_width[0];
      int save_char = seg_str[byte];
      seg_str[byte] = '\0';
      if (g_opt.no_report_cursor) {
        put_uim_str(seg_str, p->pseg[i].attr);
      } else {
        draw_pseg(&(p->pseg[i]), start + w);
      }
      seg_str[byte] = save_char;
      w += byte_width[1];
      break;
    }
  }
  p->pseg[save_i].str -= byte_offset;
}

/*
 * psegを描画する
 * 幅start_widthの次の文字がpsegの先頭文字
 */
static void draw_pseg(struct preedit_segment_tag *pseg, int start_width)
{
  struct point_tag start_point = width2point_col(start_width);
  int margin = g_win->ws_col - start_point.col;
  int lineno = start_point.row - s_head.row;
  char *seg_str = pseg->str;
  int seg_w = strwidth(seg_str);

  assert(margin >= 0 && margin <= g_win->ws_col);

  while (TRUE) {
    int *byte_width;
    int byte;
    int width;

    if (g_opt.on_the_spot && s_line2width[lineno] > s_prev_line2width[lineno]) {
      int margin2 = margin - g_win->ws_col - s_prev_line2width[lineno];
      if (seg_w >= margin2) {
        byte_width = width2byte(seg_str, margin2);
        byte = byte_width[0];
        width = byte_width[1];
        put_uim_str_len(seg_str, pseg->attr, byte);
        if (width < margin2) {
          put_cursor_address(s_head.row + lineno, s_prev_line2width[lineno]);
          put_insert(s_line2width[lineno] - s_prev_line2width[lineno]);
          put_cursor_address(s_head.row + lineno, s_prev_line2width[lineno] - (margin2 - width));
        } else {
          put_insert(s_line2width[lineno] - s_prev_line2width[lineno]);
        }
        seg_str += byte;
        margin -= width;
        seg_w -= width;
        s_prev_line2width[lineno] = s_line2width[lineno];
      }
    }

    /* 折り返す必要がないか */
    if (seg_w < margin) {
      put_uim_str(seg_str, pseg->attr);
      break;
    }

    byte_width = width2byte(seg_str, margin);
    byte = byte_width[0];
    width = byte_width[1];

    /* 行末まで出力 */
    put_uim_str_len(seg_str, pseg->attr, byte);

    /* 右端の文字を消す必要があるか */
    if (s_line2width[lineno] < s_prev_line2width[lineno]) {
      int row = s_head.row + lineno;
      int col = s_line2width[lineno];
      restore_preedit_region_or_erase(row, col, s_prev_line2width[lineno] - col,
                                      line_start_col(lineno));
      s_prev_line2width[lineno] = col;
    }

    /* プリエディットが端末の右下から出るときはスクロールする */
    if (s_head.row + lineno == g_win->ws_row - 1 && s_head.row > 0) {
      put_crlf();
      s_head.row--;
      vt_scroll_up(s_head.row, s_head.col);
    } else {
      put_cursor_address(s_head.row + lineno + 1, 0);
    }

    seg_str += byte;
    seg_w -= width;
    margin = g_win->ws_col;
    lineno++;
  }
}

/*
 * p1とp2の先頭からの共通部分文字列(属性も等しい)の幅を返す
 */
static int compare_preedit(struct preedit_tag *p1, struct preedit_tag *p2)
{
  int i;
  int eq_width = 0;

  for (i = 0; i < min(p1->nr_psegs, p2->nr_psegs); i++) {
    struct preedit_segment_tag *pseg1 = &(p1->pseg[i]);
    struct preedit_segment_tag *pseg2 = &(p2->pseg[i]);
    if (pseg1->attr == pseg2->attr) {
      if (strcmp(pseg1->str, pseg2->str) == 0) {
        eq_width += strwidth(pseg1->str);
      } else {
        eq_width += compare_str(pseg1->str, pseg2->str);
        break;
      }
    } else {
      break;
    }
  }
  return eq_width;
}

/*
 * p1とp2の末尾からの共通部分文字列(属性も等しい)の幅を返す
 */
static int compare_preedit_rev(struct preedit_tag *p1, struct preedit_tag *p2)
{
  int i;
  int eq_width_rev = 0;

  for (i = 1; i <= min(p1->nr_psegs, p2->nr_psegs); i++) {
    struct preedit_segment_tag *pseg1 = &(p1->pseg[p1->nr_psegs - i]);
    struct preedit_segment_tag *pseg2 = &(p2->pseg[p2->nr_psegs - i]);
    if (pseg1->attr == pseg2->attr) {
      if (strcmp(pseg1->str, pseg2->str) == 0) {
        eq_width_rev += strwidth(pseg1->str);
      } else {
        eq_width_rev += compare_str_rev(pseg1->str, pseg2->str);
        break;
      }
    } else {
      break;
    }
  }
  return eq_width_rev;
}

static int line_start_col(int lineno)
{
  return lineno == 0 ? s_head.col : 0;
}

static void restore_preedit_region_or_erase(int row, int col, int width, int guard_col)
{
  int restored = FALSE;

  if (g_opt.on_the_spot) {
    put_delete(width);
  } else {
    if (!g_opt.no_report_cursor) {
      restored = vt_restore_region(row, col, width, guard_col);
    }
    if (!restored) {
      put_erase(width);
    }
  }
}

static void erase_prev_preedit(void)
{
  int lineno;
  for (lineno = 0; lineno < s_preedit_lines; lineno++) {
    int col = s_line2width[lineno];
    int width = s_prev_line2width[lineno] - col;
    assert(!g_opt.on_the_spot || width >= 0);
    if (width > 0) {
      int row = s_head.row + lineno;
      put_cursor_address(row, col);
      restore_preedit_region_or_erase(row, col, width, line_start_col(lineno));
    }
  }
  for (; lineno < s_prev_preedit_lines; lineno++) {
    int width = s_prev_line2width[lineno];
    if (width > 0) {
      int row = s_head.row + lineno;
      put_cursor_address(row, 0);
      restore_preedit_region_or_erase(row, 0, width, 0);
    }
  }
}

static void erase_preedit(void)
{
  s_prev_preedit_lines = s_preedit_lines;
  s_preedit_lines = 1;
  free(s_prev_line2width);
  s_prev_line2width = s_line2width;
  s_line2width = uim_malloc(sizeof(int));
  s_line2width[0] = s_head.col;
  erase_prev_preedit();
}

static void erase_displayed_preedit(const struct preedit_tag *preedit)
{
  if (g_opt.no_report_cursor) {
    put_cursor_left(preedit->cursor);
    if (g_opt.on_the_spot) {
      put_delete(preedit->width);
    } else {
      put_erase(preedit->width);
      put_cursor_left(preedit->width);
    }
  } else {
    erase_preedit();
  }
}

static void set_line2width(struct preedit_tag *preedit)
{
  int i;
  int line_width = s_head.col;
  int lineno = 0;

  free(s_prev_line2width);
  s_prev_line2width = s_line2width;
  s_prev_preedit_lines = s_preedit_lines;
  s_line2width = uim_malloc(sizeof(int));
  s_preedit_lines = 1;

  for (i = 0; i < preedit->nr_psegs; i++) {
    char *seg_str = preedit->pseg[i].str;
    int seg_w = strwidth(seg_str);
    while (line_width + seg_w >= g_win->ws_col) {
      if (lineno + 1 == s_preedit_lines) {
        s_line2width = uim_realloc(s_line2width, sizeof(int) * ++s_preedit_lines);
      }
      if (line_width + seg_w > g_win->ws_col) {
        int *byte_width = width2byte(seg_str, g_win->ws_col - line_width);
        s_line2width[lineno++] = line_width + byte_width[1];
        seg_str += byte_width[0];
        seg_w = strwidth(seg_str);
        debug2(("line = %d col = %d\n", lineno - 1, s_line2width[lineno - 1]));
      } else {
        s_line2width[lineno++] = g_win->ws_col;
        debug2(("line = %d col = %d\n", lineno - 1, s_line2width[lineno - 1]));
        seg_w = 0;
      }
      line_width = 0;
    }
    line_width += seg_w;
  }
  if (s_preedit_lines > s_prev_preedit_lines) {
    int i;
    s_prev_line2width = uim_realloc(s_prev_line2width, sizeof(int) * s_preedit_lines);
    for (i = s_prev_preedit_lines; i < s_preedit_lines; i++) {
      s_prev_line2width[i] = 0;
    }
  }
  s_line2width[lineno] = line_width;
  debug2(("line = %d col = %d\n", lineno, s_line2width[lineno]));
}

/*
 * プリエディットの幅widthの次の文字に移動する
 * goto_char(0) => 先頭の文字に移動
 */
static void goto_char(int width)
{
  struct point_tag dest = width2point_char(width);
  assert(width >= 0);
  assert(s_preedit_lines > 0);
  put_cursor_address(dest.row, dest.col);
}

/*
 * プリエディットの幅widthの次のcolumnに移動
 * goto_col(0) => 先頭に移動
 */
static void goto_col(int width)
{
  struct point_tag dest = width2point_col(width);
  assert(width >= 0);
  assert(s_preedit_lines > 0);
  put_cursor_address(dest.row, dest.col);
}

/*
 * プリエディットの幅widthの次の文字が属す行番号を返す
 * 行番号は0から始まる
 */
static int width2lineno_char(int width)
{
  int i;
  int w = -s_head.col;
  assert(s_preedit_lines > 0);
  for (i = 0; i < s_preedit_lines - 1; i++) {
    w += s_line2width[i];
    if (width < w) {
      return i;
    }
  }
  return s_preedit_lines - 1;
}

/*
 * プリエディットの幅widthの次のcolumnが属す行番号を返す
 * 行番号は0から始まる
 */
static int width2lineno_col(int width)
{
  int i;
  int w =  -s_head.col;
  assert(s_preedit_lines > 0);
  for (i = 0; i < s_preedit_lines - 1; i++) {
    if (width < w + g_win->ws_col) {
      return i;
    }
    w += s_line2width[i];
  }
  return s_preedit_lines - 1;
  /* 端末サイズが変更されたときはここにくることもある */
  /* assert(s_winch); */
  /* return s_preedit_lines; */
}

/*
 * プリエディットの幅widthの次の文字の位置を返す
 */
static struct point_tag width2point_char(int width)
{
  struct point_tag point;
  int lineno = width2lineno_char(width);
  int col = s_head.col + width;
  int i;
  assert(s_preedit_lines > 0);
  for (i = 0; i < lineno; i++) {
    col -= s_line2width[i];
  }
  point.row = s_head.row + lineno;
  point.col = col;
  return point;
}

/*
 * プリエディットの幅widthの次のcolumnの位置を返す
 */
static struct point_tag width2point_col(int width)
{
  struct point_tag point;
  int lineno = width2lineno_col(width);
  int col = s_head.col + width;
  int i;
  assert(s_preedit_lines > 0);
  for (i = 0; i < lineno; i++) {
    col -= s_line2width[i];
  }
  point.row = s_head.row + lineno;
  point.col = col;
  return point;
}

/*
 * 端末サイズが変更されたときに呼ぶ
 */
void draw_winch(struct winsize *prev_win)
{
  s_winch = TRUE;
  if (g_opt.status_type == LASTLINE) {
    put_save_cursor();
    put_cursor_invisible();
    put_change_scroll_region(0, g_win->ws_row - 1);
    if (g_win->ws_row > prev_win->ws_row) {
      struct winsize save_win = *g_win;
      *g_win = *prev_win;
      clear_lastline();
      *g_win = save_win;
    }
    draw_statusline_force_no_restore();
    put_restore_cursor();
    put_cursor_normal();
  }
  draw_candidate_window_refresh();
}

#if defined(DEBUG) && DEBUG > 2
static void print_preedit(struct preedit_tag *p)
{
  int i;
  debug2((" cursor = %d width = %d ", p->cursor, p->width));
  for (i = 0; i < p->nr_psegs; i++) {
    debug2((" under = %d rev = %d %s\t", p->pseg[i].attr & UPreeditAttr_UnderLine, p->pseg[i].attr & UPreeditAttr_Reverse, p->pseg[i].str));
  }
}
#endif
