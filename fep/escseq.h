/*

  Copyright (c) 2003,2004 uim Project http://uim.freedesktop.org/

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

  THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
  OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
  SUCH DAMAGE.

*/

#ifndef ESCSEQ_H
#define ESCSEQ_H

struct point_tag {
  int row;
  int col;
};

struct attribute_tag {
  int underline;
  int standout;
  int bold;
  int blink;
  int foreground;
  int background;
};


void init_escseq(int use_civis, int use_ins_del, int status_type, const struct attribute_tag *attr_uim);
void quit_escseq(void);
struct point_tag get_cursor_position(void);
void put_save_cursor(void);
void put_restore_cursor(void);
void put_cursor_invisible(void);
void put_cursor_normal(void);
void put_enter_underline_mode(void);
void put_exit_underline_mode(void);
void put_enter_standout_mode(void);
void put_exit_standout_mode(void);
void put_exit_attribute_mode(void);
void put_cursor_address(int row, int col);
void put_cursor_address_p(struct point_tag *p);
void put_insert(int n);
void put_delete(int n);
void put_crlf(void);
void put_goto_lastline(int col);
void put_erase(int n);
void put_clear_to_end_of_line(int width);
void put_change_scroll_region(int start, int end);
void put_uim_str(const char *str);
void put_uim_str_len(const char *str, int len);
void put_pty_str(const char *str);
char *cut_padding(const char *escseq);
void escseq_winch(void);
#endif
