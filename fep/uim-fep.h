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

#ifndef UIM_FEP_H
#define UIM_FEP_H

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#ifdef HAVE_SYS_IOCTL_H
#include <sys/ioctl.h>
#endif
/* solaris でwinsizeを使うために必要 */
#ifdef HAVE_CURSES_H
#include <curses.h>
#endif
#ifdef __CYGWIN32__
#ifdef HAVE_SYS_TERMIOS_H
#include <sys/termios.h>
#endif
#endif
#include <uim/uim.h>

#define FALSE 0
#define TRUE 1
#define NONE 2
#define LASTLINE 3
#define BACKTICK 4
#define UNDEFINED -1729
#define ESCAPE_CODE 27
#define WIN_OUT_FILENO STDOUT_FILENO
#define WIN_IN_FILENO  STDOUT_FILENO
#define PROC_FILENO    STDIN_FILENO

struct opt_tag {
  /* ステータスラインの種類 */
  int status_type;
  /* ddskkに似た候補の表示 */
  int ddskk;
  /* TRUEならカーソル位置を反転しない */
  int cursor_no_reverse;
  /* カーソルを消すか */
  int use_civis;
  /* プリエディットを挿入するか */
  int on_the_spot;
  /* ステータスラインの幅 */
  int statusline_width;
  /* ESCの後に何秒待つか */
  int timeout;
  /* レポートカーソル機能がないか */
  int no_report_cursor;
  int print_key;
};

extern struct opt_tag g_opt;
extern int g_win_in;
extern int g_win_out;
extern struct winsize *g_win;
extern uim_context g_context;

void done(int exit_value);

#ifdef DEBUG

#if DEBUG > 2
#define debug2(arg) _debug arg
#define debug(arg) _debug arg
#define debug_write2(str, len) _debug_write(str, len)
#define debug_write(str, len) _debug_write(str, len)
void _debug(const char *fmt, ...);
void _debug_write(const char *str, int len);

#elif DEBUG == 2
#define debug2(arg)
#define debug(arg) _debug arg
#define debug_write2(str, len)
#define debug_write(str, len) _debug_write(str, len)

void _debug(const char *fmt, ...);
void _debug_write(const char *str, int len);
#else
#define debug2(arg)
#define debug(arg)
#define debug_write2(str, len)
#define debug_write(str, len)
#endif

#define return_if_fail(arg) if (!(arg)) { printf("assertion failed %s %d", __FILE__, __LINE__); return; }

#else
#define debug2(arg)
#define debug(arg)
#define debug_write2(str, len)
#define debug_write(str, len)
#define return_if_fail(arg) if (!(arg)) { return; }
#endif

#endif
