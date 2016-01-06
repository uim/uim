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
#include <config.h>
#endif
#if (!defined(DEBUG) && !defined(NDEBUG))
#define NDEBUG
#endif
#include <stdio.h>
#ifdef HAVE_CURSES_H
#include <curses.h>
#endif
#ifdef HAVE_TERM_H
#include <term.h>
#elif HAVE_NCURSES_TERM_H
#include <ncurses/term.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_ASSERT_H
#include <assert.h>
#endif
#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_CTYPE_H
#include <ctype.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif

#include "uim-fep.h"
#include "draw.h"
#include "escseq.h"
#include "str.h"
#include "read.h"

#define my_putp(str) tputs(str, 1, my_putchar);


/* 初期化したらTRUE */
static int s_init = FALSE;
/* 現在のカーソル位置 */
static struct point_tag s_cursor = {UNDEFINED, UNDEFINED};
/* 取得したカーソルと実際のカーソルの差 */
static struct point_tag s_cursor_diff = {0, 0};
/* カーソルが消えているときTRUE */
static int s_cursor_invisible = FALSE;
/* カーソル位置を保存したときTRUE */
static int s_save = FALSE;
/* 保存したカーソル */
static struct point_tag s_save_cursor;
/* uim_modeになるときに出力するエスケープシーケンス */
static const char *s_enter_uim_mode;
/* enter_underline_modeに含まれる数字 */
static const char *s_enter_underline_num;
/* exit_underline_modeに含まれる数字 */
static const char *s_exit_underline_num;
/* enter_standout_modeに含まれる数字 */
static const char *s_enter_standout_num;
/* exit_standout_modeに含まれる数字 */
static const char *s_exit_standout_num;
/* enter_bold_modeに含まれる数字 */
static const char *s_bold_num;
/* enter_blink_modeに含まれる数字 */
static const char *s_blink_num;
/* orig_pairに含まれる数字 */
static const char *s_orig_pair_num;
/* orig_pairに含まれる最初の数字 */
static const char *s_orig_fore_num;
/* orig_pairに含まれる2番目の数字 */
static const char *s_orig_back_num;
/* 途中で切れているエスケープシーケンスを保存するバッファ */
static char *s_escseq_buf = NULL;

/* 属性なし */
static const struct attribute_tag s_attr_none = {
  FALSE,     /* underline */
  FALSE,     /* standout */
  FALSE,     /* bold */
  FALSE,     /* blink */
  FALSE,     /* foreground */
  FALSE      /* background */
};

/* uimモードの属性 */
static struct attribute_tag s_attr_uim;
/* ptyモードの属性 */
static struct attribute_tag s_attr_pty = {
  FALSE,     /* underline */
  FALSE,     /* standout */
  FALSE,     /* bold */
  FALSE,     /* blink */
  FALSE,     /* foreground */
  FALSE      /* background */
};

/* 現在の属性 */
static struct attribute_tag s_attr = {
  FALSE,     /* underline */
  FALSE,     /* standout */
  FALSE,     /* bold */
  FALSE,     /* blink */
  FALSE,     /* foreground */
  FALSE      /* background */
};

static void check_escseq(void);
static char *escseq2n(const char *escseq);
static void escseq2n2(const char *escseq, const char **first, const char **second);
#ifndef HAVE_CFMAKERAW
static int cfmakeraw(struct termios *termios_p);
#endif
static void change_background_attr(struct attribute_tag *from, struct attribute_tag to);
static const char *attr2escseq(const struct attribute_tag *attr);
static void set_attr(const char *str, int len);
static int my_putchar(int c);


void init_escseq(const struct attribute_tag *attr_uim)
{
  s_attr_uim = *attr_uim;

  s_enter_underline_num = escseq2n(enter_underline_mode);
  s_exit_underline_num = escseq2n(exit_underline_mode);
  s_enter_standout_num = escseq2n(enter_standout_mode);
  s_exit_standout_num = escseq2n(exit_standout_mode);
  s_bold_num = escseq2n(enter_bold_mode);
  s_blink_num = escseq2n(enter_blink_mode);
  s_orig_pair_num = escseq2n(orig_pair);
  escseq2n2(orig_pair, &s_orig_fore_num, &s_orig_back_num);
  s_enter_uim_mode = attr2escseq(&s_attr_uim);
  if (s_enter_uim_mode != NULL) {
    s_enter_uim_mode = uim_strdup(s_enter_uim_mode);
  }

  fixtty();
  check_escseq();
  s_init = TRUE;
}

/*
 * termcap/terminfoのエントリがあるか確認する
 */
static void check_escseq(void)
{
  if (enter_underline_mode == NULL) {
    printf("enter_underline_mode is not available\n");
    done(EXIT_FAILURE);
  }
  if (exit_underline_mode == NULL) {
    printf("exit_underline_mode is not available\n");
    done(EXIT_FAILURE);
  }
  if (enter_standout_mode == NULL) {
    printf("enter_standout_mode is not available\n");
    done(EXIT_FAILURE);
  }
  if (exit_standout_mode == NULL) {
    printf("exit_standout_mode is not available\n");
    done(EXIT_FAILURE);
  }
  if (exit_attribute_mode == NULL) {
    printf("exit_attribute_mode is not available\n");
    done(EXIT_FAILURE);
  }

  if (g_opt.status_type == LASTLINE) {
    if (cursor_address == NULL) {
      printf("cursor_address is not available\n");
      done(EXIT_FAILURE);
    }
    if (clr_eol == NULL) {
      printf("clr_eol is not available\n");
      done(EXIT_FAILURE);
    }
    if (change_scroll_region == NULL) {
      printf("change_scroll_region is not available\n");
      done(EXIT_FAILURE);
    }
    if (g_opt.no_report_cursor) {
      if (cursor_up == NULL) {
        printf("cursor_up is not available.\n");
        done(EXIT_FAILURE);
      }
    }
  }

  if (g_opt.no_report_cursor) {
    if (save_cursor == NULL) {
      printf("save_cursor is not available.\n");
      done(EXIT_FAILURE);
    }
    if (restore_cursor == NULL) {
      printf("restore_cursor is not available.\n");
      done(EXIT_FAILURE);
    }
    if (cursor_left == NULL) {
      printf("cursor_left is not available\n");
      done(EXIT_FAILURE);
    }
    if (cursor_right == NULL) {
      printf("cursor_right is not available\n");
      done(EXIT_FAILURE);
    }
  } else {
    if (cursor_address == NULL) {
      printf("cursor_address is not available\n");
      done(EXIT_FAILURE);
    }
  }

  if (g_opt.on_the_spot) {
    if (parm_ich == NULL) {
      printf("parm_ich is not available\n");
      done(EXIT_FAILURE);
    }
    if (parm_dch == NULL) {
      printf("parm_dch is not available\n");
      done(EXIT_FAILURE);
    }
  }
}

/*
 * ^[[7m$<2>から7を取り出す
 */
static char *escseq2n(const char *escseq)
{
  char *free_n;
  char *n;
  char *n2;
  if (escseq == NULL) {
    return NULL;
  }
  free_n = n = cut_padding(escseq);
  if ((n = strstr(n, "\033[")) == NULL) {
    free(free_n);
    return NULL;
  }
  n2 = n += 2;
  while (isdigit((unsigned char)n2[0])) {
    n2++;
  }
  if (n2[0] == 'm') {
    n2[0] = '\0';
    return n;
  } else {
    free(free_n);
    return NULL;
  }
}

/*
 * ^[[39;49mから39と49を取り出す
 */
static void escseq2n2(const char *escseq, const char **first, const char **second)
{
  char *free_n;
  char *n;
  char *n2;
  *first = *second = NULL;
  if (escseq == NULL) {
    return;
  }
  free_n = n = cut_padding(escseq);
  if ((n = strstr(n, "\033[")) == NULL) {
    free(free_n);
    return;
  }
  n2 = n += 2;
  while (isdigit((unsigned char)n2[0])) {
    n2++;
  }
  if (n2[0] == ';') {
    n2[0] = '\0';
    *first = n;
  } else {
    free(free_n);
    return;
  }
  n = ++n2;
  while (isdigit((unsigned char)n2[0])) {
    n2++;
  }
  if (n2[0] == 'm') {
    n2[0] = '\0';
    *second = n;
  } else {
    free(free_n);
    return;
  }
}

void quit_escseq(void)
{
  if (!s_init) {
    return;
  }
  put_exit_attribute_mode();
  if (g_opt.status_type == LASTLINE) {
    put_save_cursor();
    put_change_scroll_region(0, g_win->ws_row);
    put_goto_lastline(0);
    my_putp(clr_eol);
    put_restore_cursor();
  }
  put_restore_cursor();
  put_cursor_normal();
}

/*
 * 端末をrawモードにする
 */
void fixtty(void)
{
  struct point_tag start_cursor;
  struct point_tag cursor;
  struct point_tag cursor2;
  static struct termios tios;

  tcgetattr(g_win_in, &tios);
  cfmakeraw(&tios);
  /* read_stdinが戻るまでに読まなければならない最小の文字数 */
  tios.c_cc[VMIN] = 0;
  /* read_stdinが戻るまでのタイムアウト 0.1秒単位 */
  tios.c_cc[VTIME] = 3;
  tcsetattr(g_win_in, TCSANOW, &tios);

  if (s_init) {
    s_cursor.row = s_cursor.col = UNDEFINED;
    /* 前回終了時の位置に戻る */
    put_restore_cursor();
  }

  if (!g_opt.no_report_cursor) {
    /* 開始位置を保存 */
    start_cursor = get_cursor_position();
    if (start_cursor.row == UNDEFINED) {
      g_opt.no_report_cursor = TRUE;
    }
  }

  if (g_opt.no_report_cursor) {
    if (g_opt.status_type == LASTLINE) {
      put_cursor_invisible();
      put_crlf();
      my_putp(cursor_up);
      put_save_cursor();
      put_change_scroll_region(0, g_win->ws_row - 1);
      put_restore_cursor();
      put_cursor_normal();
    }
    draw_statusline_force_restore();
    return;
  }

  put_cursor_invisible();
  /* 最下行から開始したときのためにスクロール */
  if (g_opt.status_type == LASTLINE) {
    write(g_win_out, "\n", strlen("\n"));
  }

  if (!s_init) {
    s_cursor.row = s_cursor.col = UNDEFINED;
    /* 安全な位置に移動 */
    put_cursor_address(1, 1);
    s_cursor.row = s_cursor.col = UNDEFINED;
    /* カーソル位置を取得 */
    cursor = get_cursor_position();
    s_cursor.row = s_cursor.col = UNDEFINED;
    /* 取得したカーソル位置に移動 */
    put_cursor_address_p(&cursor);
    s_cursor.row = s_cursor.col = UNDEFINED;
    /* 同じカーソル位置が得られるか */
    cursor2 = get_cursor_position();
    /* 得られなかったら差分を調べる */
    s_cursor_diff.row = cursor2.row - cursor.row;
    s_cursor_diff.col = cursor2.col - cursor.col;
    start_cursor.row -= s_cursor_diff.row;
    start_cursor.col -= s_cursor_diff.col;
  }

  if (g_opt.status_type == LASTLINE) {
    put_change_scroll_region(0, g_win->ws_row - 1);
  }
  draw_statusline_force_no_restore();
  /* 開始位置に戻る */
  put_cursor_address_p(&start_cursor);
  put_cursor_normal();
}

#ifndef HAVE_CFMAKERAW
static int cfmakeraw(struct termios *termios_p)
{
  termios_p->c_iflag &= ~(IGNBRK | BRKINT | PARMRK | ISTRIP | INLCR | IGNCR | ICRNL | IXON);
  termios_p->c_oflag &= ~OPOST;
  termios_p->c_lflag &= ~(ECHO | ECHONL | ICANON | ISIG | IEXTEN);
  termios_p->c_cflag &= ~(CSIZE | PARENB);
  termios_p->c_cflag |= CS8;
  return 0;
}
#endif

void put_move_cur(int from, int to)
{
  if (to < from) {
    put_cursor_left(from - to);
  } else if (from < to) {
    put_cursor_right(to - from);
  }
}

void put_cursor_left(int n)
{
  int i;
  debug(("<left %d>", n));
  for (i = 0; i < n; i++) {
    my_putp(cursor_left);
  }
}

void put_cursor_right(int n)
{
  int i;
  debug(("<right %d>", n));
  for (i = 0; i < n; i++) {
    my_putp(cursor_right);
  }
}

/*
 * カーソル位置を保存する
 */
void put_save_cursor(void)
{
  if (!s_save) {
    s_save = TRUE;
    debug(("<put_save_cursor>"));
    if (g_opt.no_report_cursor) {
      my_putp(save_cursor);
      s_save_cursor = s_cursor;
      /* put_exit_attribute_mode(); */
    } else {
      s_save_cursor = get_cursor_position();
    }
  }
}

/*
 * 保存したカーソル位置を復元する
 */
void put_restore_cursor(void)
{
  if (s_save) {
    s_save = FALSE;
    debug(("<put_restore_cursor>"));
    if (g_opt.no_report_cursor) {
      my_putp(restore_cursor);
      /* DOSプロンプトでは1回のrestore_cursorでは戻らない */
      my_putp(restore_cursor);
      s_cursor = s_save_cursor;
      /* DOSプロンプトでは属性を保存しない */
      /* put_exit_attribute_mode(); */
    } else {
      put_cursor_address_p(&s_save_cursor);
    }
  }
}

#define range_check(i)                   \
do {                                     \
  if ((i) > escseq_len - 1) {            \
    goto retry;                          \
  }                                      \
} while(FALSE)

/*
 * 現在のカーソル位置を返す
 * 左上は0, 0
 * カーソル位置が得られなかったらUNDEFINEDを返す
 */
struct point_tag get_cursor_position(void)
{
  char ibuf[300];
  ssize_t len = 0;
  ssize_t read_len = 0;
  char *escseq = ibuf - 1;
  int loop_count = 0;

  assert(!g_opt.no_report_cursor);

  if (s_cursor.row != UNDEFINED) {
    return s_cursor;
  }

  write(g_win_out, "\033[6n", strlen("\033[6n"));

  while (TRUE) {
    char *next_escseq;
    retry:
    len += (read_len = read_stdin(ibuf + len, sizeof(ibuf) - len));
    if (read_len == 0) {
      debug(("loop = %d\n", loop_count + 1));
      if (++loop_count == 5) {
        break;
      }
    }

    debug2(("get = \""));
    debug_write2(ibuf, len);
    debug2(("\"\n"));

    if (escseq != ibuf - 1) {
      escseq--;
    }

    /* NULが入っているかもしれないので strchr ではなく memchr */
    while ((next_escseq = memchr(escseq + 1, ESCAPE_CODE, len - (escseq - ibuf) - 1)) != NULL) {
      int i = 1;
      int row = UNDEFINED;
      int col = UNDEFINED;
      char unget_buf[300];
      int unget_count = 0;
      int escseq_len;

      escseq = next_escseq;
      escseq_len = len - (escseq - ibuf);

      if ((int)strlen("\033[0;0R") > escseq_len) {
        break; /* goto retry */
      }

      /* n = sscanf(escseq, "\033[%d;%d%c", &(s_cursor.row), &(s_cursor.col), &R); */

      if (escseq[i] != '[') {
        unget_buf[unget_count++] = escseq[i++];
        if (escseq[i] != '[') {
          continue;
        }
      }
      
      if (escseq[i + 1] == '[') {
        unget_buf[unget_count++] = escseq[i++];
      }

      while (TRUE) {
        i++;
        range_check(i);
        if (!isdigit((unsigned char)escseq[i])) {
          if (row != UNDEFINED && escseq[i] == ';') {
            break;
          }
          unget_buf[unget_count++] = escseq[i++];
          range_check(i);
        }
        if (isdigit((unsigned char)escseq[i])) {
          if (row == UNDEFINED) {
            row = 0;
          }
          row = row * 10 + escseq[i] - '0';
        } else {
          break;
        }
      }

      if (row == UNDEFINED) {
        continue;
      }

      if (escseq[i] != ';') {
        continue;
      }

      range_check(i + 1);
      if (escseq[i + 1] == ';') {
        unget_buf[unget_count++] = escseq[i++];
      }

      while (TRUE) {
        i++;
        range_check(i);
        if (!isdigit((unsigned char)escseq[i])) {
          if (col != UNDEFINED && escseq[i] == 'R') {
            break;
          }
          unget_buf[unget_count++] = escseq[i++];
          range_check(i);
        }
        if (isdigit((unsigned char)escseq[i])) {
          if (col == UNDEFINED) {
            col = 0;
          }
          col = col * 10 + escseq[i] - '0';
        } else {
          break;
        }
      }

      if (col == UNDEFINED) {
        continue;
      }

      if (escseq[i] != 'R') {
        continue;
      }

      /* エスケープシーケンスの前に文字列があるか */
      if (escseq > ibuf) {
        unget_stdin(ibuf, escseq - ibuf);
      }

      /* エスケープシーケンスの中の文字列 */
      if (unget_count > 0) {
        unget_stdin(unget_buf, unget_count);
      }

      /* エスケープシーケンスの後の文字列 */
      if (i < escseq_len - 1) {
        unget_stdin(&escseq[i + 1], escseq_len - 1 - i);
      }

      row--;
      col--;
      row -= s_cursor_diff.row;
      col -= s_cursor_diff.col;

      s_cursor.row = row;
      s_cursor.col = col;

      /* GNU screen ではこうなることがある */
      if (s_cursor.col > g_win->ws_col - 1) {
        put_crlf();
      }
      debug(("<get row = %d col = %d>", s_cursor.row, s_cursor.col));
      return s_cursor;
    }
  }

  unget_stdin(ibuf, len);
  /* 失敗 */
  return s_cursor;
}

/*
 * カーソルを見えないようにする
 */
void put_cursor_invisible(void)
{
  if (g_opt.use_civis && !s_cursor_invisible && cursor_invisible != NULL && cursor_normal != NULL) {
    s_cursor_invisible = TRUE;
    my_putp(cursor_invisible);
    debug(("<invis>"));
  }
}

/*
 * カーソルを見えるようにする
 */
void put_cursor_normal(void)
{
  if (s_cursor_invisible) {
    s_cursor_invisible = FALSE;
    my_putp(cursor_normal);
    debug(("<norm>"));
  }
}

/*
 * standoutモード、underlineモードを終了する
 * 文字色、背景色を元に戻す
 */
void put_exit_attribute_mode(void)
{
  s_attr = s_attr_none;
  my_putp(exit_attribute_mode);
  debug(("<a}>"));
}

/*
 * 属性をfromからtoに変更し、from <= to
 */
static void change_attr(struct attribute_tag *from, const struct attribute_tag *to)
{
  const char *escseq;

  if (   (from->underline           && !to->underline          )
      || (from->standout            && !to->standout           )
      || (from->bold                && !to->bold               )
      || (from->blink               && !to->blink              )
      || (from->foreground != FALSE &&  to->foreground == FALSE)
      || (from->background != FALSE &&  to->background == FALSE)
      ) {
    put_exit_attribute_mode();
    escseq = attr2escseq(to);
  } else {
    struct attribute_tag attr = *to;
    if (from->underline == to->underline) {
      attr.underline = FALSE;
    }
    if (from->standout == to->standout) {
      attr.standout = FALSE;
    }
    if (from->bold == to->bold) {
      attr.bold = FALSE;
    }
    if (from->blink == to->blink) {
      attr.blink = FALSE;
    }
    if (from->foreground == to->foreground) {
      attr.foreground = FALSE;
    }
    if (from->background == to->background) {
      attr.background = FALSE;
    }
    escseq = attr2escseq(&attr);
  }
  if (escseq != NULL) {
    my_putp(escseq);
    *from = *to;
    debug(("change_attr %s\n", escseq));
  }
}

/*
 * 背景属性をfromからtoに変更し、from <= to
 * 文字色は変わらないかもしれない
 */
static void change_background_attr(struct attribute_tag *from, struct attribute_tag to)
{
  if (!(    (from->underline           && !to.underline          )
        ||  (from->standout            && !to.standout           )
        ||  (from->blink               && !to.blink              )
        ||  (from->background != FALSE &&  to.background == FALSE)
      )) {
    to.bold = from->bold;
    to.foreground = from->foreground;
  }
  change_attr(from, &to);
}

/*
 * attrに対応するエスケープシーケンスを返す
 * 返り値は静的なバッファ
 */
static const char *attr2escseq(const struct attribute_tag *attr)
{
  static char escseq[20];
  char numstr[20];
  int add_semicolon = FALSE;
  if (!attr->underline && !attr->standout && !attr->bold && !attr->blink
      && attr->foreground == FALSE && attr->background == FALSE) {
    return NULL;
  }
  strlcpy(escseq, "\033[", sizeof(escseq));
  if (attr->underline && s_enter_underline_num != NULL) {
    add_semicolon = TRUE;
    strlcat(escseq, s_enter_underline_num, sizeof(escseq));
  }
  if (attr->standout && s_enter_standout_num != NULL) {
    if (add_semicolon) {
      strlcat(escseq, ";", sizeof(escseq));
    } else {
      add_semicolon = TRUE;
    }
    strlcat(escseq, s_enter_standout_num, sizeof(escseq));
  }
  if (attr->bold && s_bold_num != NULL) {
    if (add_semicolon) {
      strlcat(escseq, ";", sizeof(escseq));
    } else {
      add_semicolon = TRUE;
    }
    strlcat(escseq, s_bold_num, sizeof(escseq));
  }
  if (attr->blink && s_blink_num != NULL) {
    if (add_semicolon) {
      strlcat(escseq, ";", sizeof(escseq));
    } else {
      add_semicolon = TRUE;
    }
    strlcat(escseq, s_blink_num, sizeof(escseq));
  }
  if (attr->foreground != FALSE) {
    if (add_semicolon) {
      strlcat(escseq, ";", sizeof(escseq));
    } else {
      add_semicolon = TRUE;
    }
    snprintf(numstr, sizeof(numstr), "%d", attr->foreground);
    strlcat(escseq, numstr, sizeof(escseq));
  }
  if (attr->background != FALSE) {
    if (add_semicolon) {
      strlcat(escseq, ";", sizeof(escseq));
    }
    snprintf(numstr, sizeof(numstr), "%d", attr->background);
    strlcat(escseq, numstr, sizeof(escseq));
  }
  strlcat(escseq, "m", sizeof(escseq));
  debug2(("attr2escseq underline = %d standout = %d bold = %d blink = %d fore = %d back = %d\n",
      attr->underline, attr->standout, attr->bold, attr->blink,
      attr->foreground, attr->background));
  debug2(("attr2escseq = %s\n", escseq));
  return escseq;
}

/*
 * strから属性を変更するエスケープシーケンスを探す
 * len > 0
 */
static void set_attr(const char *str, int len)
{
  /* エスケープシーケンスの開始 */
  const char *start;
  const char *free_str = NULL;
  /* TRUEのときfree_strをfreeする必要がある */
  int must_free = FALSE;
  const char *end = str + len;
  /* 前回の途中のバッファがあるか */
  if (s_escseq_buf != NULL) {
    const char *tmp_str = str;
    /* s_escseq_buf には'\0'は含まれない */
    int escseq_buf_len = strlen(s_escseq_buf);
    must_free = TRUE;
    free_str = str = uim_malloc(escseq_buf_len + len + 1);
    memcpy((char *)str, s_escseq_buf, escseq_buf_len);
    memcpy((char *)str + escseq_buf_len, tmp_str, len);
    ((char *)str)[escseq_buf_len + len] = '\0';
    free(s_escseq_buf);
    s_escseq_buf = NULL;
    end = str + escseq_buf_len + len;
  }

  while ((start = str = memchr(str, ESCAPE_CODE, end - str)) != NULL) {
    str++;
    if (str == end) {
      int escseq_buf_len = end - start;
      s_escseq_buf = uim_malloc(escseq_buf_len + 1);
      memcpy(s_escseq_buf, start, escseq_buf_len);
      s_escseq_buf[escseq_buf_len] = '\0';
    }

    else if (str[0] == '[') {
      int nr_params = 1;
      /* ^[[1;2;3の1, 2, 3を入れる */
      int *params = uim_malloc(sizeof(int));
      params[0] = 0;
      str++;

      while (isdigit((unsigned char)str[0]) || str[0] == ';') {
        if (isdigit((unsigned char)str[0])) {
          int n = 0;
          while (isdigit((unsigned char)str[0])) {
            n = n * 10 + str[0] - '0';
            str++;
          }
          params[nr_params - 1] = n;
        }
        if (str[0] == ';') {
          nr_params++;
          params = uim_realloc(params, sizeof(int) * nr_params);
          params[nr_params - 1] = 0;
          str++;
        }
      }

      if (str == end) {
        int escseq_buf_len = end - start;
        s_escseq_buf = uim_malloc(escseq_buf_len + 1);
        memcpy(s_escseq_buf, start, escseq_buf_len);
        s_escseq_buf[escseq_buf_len] = '\0';
      }

      else if (str[0] == 'm') {
        int i;
        str++;
        for (i = 0; i < nr_params; i++) {
          if (params[i] == 0) {
            /* 属性消去 */
            s_attr = s_attr_none;
          } else if (s_enter_underline_num != NULL && params[i] == atoi(s_enter_underline_num)) {
            s_attr.underline = TRUE;
          } else if (s_exit_underline_num != NULL && params[i] == atoi(s_exit_underline_num)) {
            s_attr.underline = FALSE;
          } else if (s_enter_standout_num != NULL && params[i] == atoi(s_enter_standout_num)) {
            s_attr.standout = TRUE;
          } else if (s_exit_standout_num != NULL && params[i] == atoi(s_exit_standout_num)) {
            s_attr.standout = FALSE;
          } else if (s_bold_num != NULL && params[i] == atoi(s_bold_num)) {
            s_attr.bold = TRUE;
          } else if (s_blink_num != NULL && params[i] == atoi(s_blink_num)) {
            s_attr.blink = TRUE;
          } else if (s_orig_pair_num != NULL && params[i] == atoi(s_orig_pair_num)) {
            s_attr.foreground = s_attr.background = FALSE;
          } else if (s_orig_fore_num != NULL && params[i] == atoi(s_orig_fore_num)) {
            s_attr.foreground = FALSE;
          } else if (s_orig_back_num != NULL && params[i] == atoi(s_orig_back_num)) {
            s_attr.background = FALSE;
          } else if (params[i] == 22) {
            s_attr.bold = FALSE;
          } else if (params[i] == 25) {
            s_attr.blink = FALSE;
          } else if ((30 <= params[i] && params[i] <= 37) || (90 <= params[i] && params[i] <= 97)) {
            s_attr.foreground = params[i];
          } else if ((40 <= params[i] && params[i] <= 47) || (100 <= params[i] && params[i] <= 107)) {
            s_attr.background = params[i];
          }
        }
      }
      free(params);
    }
  }
  if (must_free) {
    free((char *)free_str);
  }
  s_attr_pty = s_attr;
}

/*
 * row行 col列に移動する
 * 左上は0, 0
 */
void put_cursor_address(int row, int col)
{
  const char *tmp;
  if (row >= g_win->ws_row) {
    row = g_win->ws_row - 1;
  }
  /* 右端への移動は省略しない */
  if (row == s_cursor.row && col == s_cursor.col && col < g_win->ws_col - 2) {
    return;
  }
  tmp = tparm(cursor_address, row, col);
  my_putp(tmp);
  s_cursor.row = row;
  s_cursor.col = col;
  debug(("<go %d %d>", row, col));
}

void put_cursor_address_p(struct point_tag *p)
{
  put_cursor_address(p->row, p->col);
}

/*
 * n文字挿入する
 */
void put_insert(int n)
{
  const char *tmp;
  if (n <= 0) {
    return;
  }
  tmp = tparm(parm_ich, n);
  my_putp(tmp);
  debug(("<ins %d>", n));
}

/*
 * n文字消去する
 */
void put_delete(int n)
{
  const char *tmp;

  if (n <= 0) {
    return;
  }

  if (back_color_erase) {
    s_attr_uim.standout = FALSE;
    s_attr_uim.underline = FALSE;
    change_background_attr(&s_attr, s_attr_uim);
  }

  tmp = tparm(parm_dch, n);
  my_putp(tmp);
  debug(("<del %d>", n));
}

/*
 * 改行する
 */
void put_crlf(void)
{
  write(g_win_out, "\r\n", strlen("\r\n"));
  s_cursor.col = 0;
  s_cursor.row++;
  if (s_cursor.row >= g_win->ws_row) {
    s_cursor.row = g_win->ws_row - 1;
  }
  debug(("<crlf>"));
}

/*
 * 最下行のcolに移動する
 */
void put_goto_lastline(int col)
{
  const char *tmp;
  int row = g_win->ws_row;
  if (row == s_cursor.row && col == s_cursor.col) {
    return;
  }
  tmp = tparm(cursor_address, row, col);
  my_putp(tmp);
  s_cursor.row = row;
  s_cursor.col = col;
  debug(("<go %d %d>", row, col));
}

/*
 * underlineモードとstandoutモードを終了してn文字スペースを出力する
 * 画面の右端を越えてはいけない
 */
void put_erase(int n)
{
  int i;
  char *spaces;

  if (n <= 0) {
    return;
  }

  spaces = uim_malloc(n + 1);
  for (i = 0; i < n; i++) {
    spaces[i] = ' ';
  }
  spaces[n] = '\0';

  if (s_escseq_buf != NULL) {
    free(s_escseq_buf);
    s_escseq_buf = NULL;
  }

  s_attr_uim.standout = FALSE;
  s_attr_uim.underline = FALSE;
  change_background_attr(&s_attr, s_attr_uim);

  s_cursor.col += n;
  assert(s_cursor.col <= g_win->ws_col || g_opt.no_report_cursor);
  write(g_win_out, spaces, n);

  free(spaces);
  debug(("<put erase %d>", n));
}

/*
 * underlineモードとstandoutモードを終了して行末まで消去
 * カーソル位置は変わらない
 */
void put_clear_to_end_of_line(int width)
{
  if (s_attr_uim.background != FALSE && !back_color_erase) {
    put_erase(width);
    return;
  }
  if (back_color_erase) {
    s_attr_uim.standout = FALSE;
    s_attr_uim.underline = FALSE;
    change_background_attr(&s_attr, s_attr_uim);
  }
  my_putp(clr_eol);
  debug(("<clear>"));
}

/*
 * 端末の描画領域をstartからendに設定する
 */
void put_change_scroll_region(int start, int end)
{
  const char *tmp = tparm(change_scroll_region, start, end);
  my_putp(tmp);
  s_cursor.row = s_cursor.col = 0;
  debug(("<region %d %d>", start, end));
}

/*
 * strを端末に出力する
 * 画面の右端を越えてはいけない
 * エスケープシーケンスは含まない
 * 下線、反転はattrで指定
 */
void put_uim_str(const char *str, int attr)
{
  if (str[0] == '\0') {
    return;
  }
  if (s_escseq_buf != NULL) {
    free(s_escseq_buf);
    s_escseq_buf = NULL;
  }

  s_attr_uim.standout = attr & UPreeditAttr_Reverse;
  s_attr_uim.underline = attr & UPreeditAttr_UnderLine;
  change_attr(&s_attr, &s_attr_uim);

  s_cursor.col += strwidth(str);
  assert(s_cursor.col <= g_win->ws_col || g_opt.no_report_cursor);
  write(g_win_out, str, strlen(str));
  debug(("<put_uim_str \"%s\">", str));
}

/*
 * strのlenだけ出力するput_uim_str
 * const char *strとなっているが，一時的に書換えるのでstrを文字列定数
 * にしてはいけない．
 */
void put_uim_str_len(const char *str, int attr, int len)
{
  char save_char = str[len];
  ((char *)str)[len] = '\0';
  put_uim_str(str, attr);
  ((char *)str)[len] = save_char;
}

/*
 * strを色なしで出力するput_uim_str
 */
void put_uim_str_no_color(const char *str, int attr)
{
  struct attribute_tag save_attr = s_attr_uim;
  s_attr_uim = s_attr_none;
  put_uim_str(str, attr);
  s_attr_uim = save_attr;
}

/*
 * strのlenだけ出力するput_uim_str_no_color
 */
void put_uim_str_no_color_len(const char *str, int attr, int len)
{
  char save_char = str[len];
  ((char *)str)[len] = '\0';
  put_uim_str_no_color(str, attr);
  ((char *)str)[len] = save_char;
}

/*
 * ptyからの出力strを端末に出力する
 * エスケープシーケンスを含む場合がある
 */
void put_pty_str(const char *str, int len)
{
  if (len == 0) {
    return;
  }
  change_attr(&s_attr, &s_attr_pty);
  /* put_exit_uim_mode(); */
  write(g_win_out, str, len);
  set_attr(str, len);
  g_commit = FALSE;
  s_cursor.row = s_cursor.col = UNDEFINED;
  debug(("<put_pty_str \"%s\">", str));
}


/*
 * escseqから/$<\d+>/を取り除いたものを返す
 * escseqがNULLだったらNULLを返す
 */
char *cut_padding(const char *escseq)
{
  int i, j;
  char *cut_str;
  if (escseq == NULL) {
    return NULL;
  }
  cut_str = uim_malloc(strlen(escseq) + 1);
  for (i = 0, j = 0; escseq[i] != '\0';) {
    if (escseq[i] == '$' && escseq[i + 1] == '<') {
      int i2 = i + 1;
      while (isdigit((unsigned char)escseq[++i2]));
      if (escseq[i2] == '>') {
        i = i2 + 1;
        if (escseq[i] == '\0') {
          break;
        }
      }
    }
    cut_str[j++] = escseq[i++];
  }
  cut_str[j] = '\0';
  return cut_str;
}

/*
 * 端末サイズが変更されたときに呼ぶ
 */
void escseq_winch(void)
{
  s_cursor.row = s_cursor.col = UNDEFINED;
}

static int my_putchar(int c)
{
  char ch = c;
  write(g_win_out, &ch, 1);
  return c;
}
