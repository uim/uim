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

#ifndef UIM_FEP_CALLBACKS_H
#define UIM_FEP_CALLBACKS_H
#include <time.h>

/* Candidate window geometry shared by callbacks.c and draw.c */
#define WINDOW_BORDER_ROWS  2 /* top and bottom border */
#define WINDOW_FOOTER_ROWS  1 /* "index / total" line */
#define WINDOW_BORDER_COLS  2 /* left and right border */
#define WINDOW_PADDING_COLS 2 /* one space on each side of the text */
#define WINDOW_FRAME_COLS   (WINDOW_BORDER_COLS + WINDOW_PADDING_COLS)
#define WINDOW_MIN_WIDTH    5 /* frame plus one character */
#define WINDOW_MIN_ROWS     (WINDOW_BORDER_ROWS + WINDOW_FOOTER_ROWS + 1)

struct preedit_tag {
  /* 文字列の幅 */
  int width;
  /* カーソル位置の前にある文字列の幅 */
  int cursor;
  /* psegの大きさ */
  int nr_psegs;
  /* 隣りあうpsegのattrは異なるようにする */
  struct preedit_segment_tag {
    int attr;
    char *str;
  } *pseg;
};

void init_callbacks(void);
int press_key(int key, int key_state);
void start_callbacks(void);
int end_callbacks(void);
void cancel_delay_activating(void);
int delay_activating_pending(void);
int delay_activating_timeout(struct timespec *remaining);
void run_delay_activating(void);
char *get_commit_str(void);
char *get_statusline_str(void);
char *get_candidate_str(void);
int get_candidate_col(void);
int get_candidate_window_page(int *first, int *count, int *selected, int *total);
char *get_candidate_window_item(int index);
void get_candidate_window_list_info(int *list_id, int *page_size);
const char *get_candidate_nokori_str(void);
char *get_index_str(void);
int get_index_col(void);
struct preedit_tag *get_preedit(void);
int get_mode(void);
char *get_mode_str(void);
void commit_cb(void *ptr, const char *commit_str);
struct preedit_tag *create_preedit(void);
void free_preedit(struct preedit_tag *p);
void callbacks_winch(void);
void callbacks_set_mode(int mode);

#endif
