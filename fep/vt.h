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

#ifndef UIM_FEP_VT_H
#define UIM_FEP_VT_H

#include <stddef.h>
#include <stdint.h>

/* Terminal model API, implemented by vt.c when HAVE_LIBVTERM is defined.
 * Otherwise the calls expand to no-ops and background restoration is
 * disabled. */
#ifdef HAVE_LIBVTERM

int vt_init(uint16_t cols, uint16_t rows);
void vt_feed(const char *data, size_t len);
void vt_resize(uint16_t cols, uint16_t rows);
void vt_scroll_up(int row, int col);
int vt_restore_region(int row, int col, int width, int guard_col);
void vt_quit(void);

#else

#define vt_init(cols, rows) ((void)0)
#define vt_feed(data, len) ((void)0)
#define vt_resize(cols, rows) ((void)0)
#define vt_scroll_up(row, col) ((void)0)
#define vt_restore_region(row, col, width, guard_col) 0
#define vt_quit() ((void)0)

#endif

#endif
