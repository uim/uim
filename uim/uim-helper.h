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

#ifndef UIM_HELPER_H
#define UIM_HELPER_H

#ifdef __cplusplus
extern "C" {
#endif

/* API for input method user side */
int  uim_helper_init_client_fd(void (*disconnect_cb)(void));
void uim_helper_close_client_fd(int );
void uim_helper_client_focus_in(uim_context uc);
void uim_helper_client_focus_out(uim_context uc);
void uim_helper_client_get_prop_list(void);
void uim_helper_read_proc(int fd);
char *uim_helper_get_message(void);
void uim_helper_send_message(int fd, const char *message);

/* functions for libuim server/client's implementation */
uim_bool uim_helper_get_pathname(char *, int);
int uim_helper_str_terminated(const char *str);
int uim_helper_check_connection_fd(int fd);
int uim_helper_fd_readable(int fd);
int uim_helper_fd_writable(int fd);
char *uim_helper_buffer_append(char *buf,
			       const char *fragment, size_t fragment_size);
void uim_helper_buffer_shift(char *buf, int count);
char *uim_helper_buffer_get_message(char *buf);

uim_bool
uim_helper_is_setugid(void);

#ifdef __cplusplus
}
#endif
#endif
