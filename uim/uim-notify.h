/*
  Copyright (c) 2007-2013 uim Project https://github.com/uim/uim

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

/*                         !WARNING!

   This API is experimental and unstable.

*/

/*
 * It is notification plugin's responsibility to convert messages into
 * encoding suitable for the notification agents. Messages are assumed to
 * be tranlated by gettext into the encoding of working locale of the process.
 */

#ifndef UIM_NOTIFY_H
#define UIM_NOTIFY_H

#ifdef __cplusplus
extern "C" {
#endif

typedef struct uim_notify_desc_ {
  const char *name;
  const char *desc;
} uim_notify_desc;

/* API for notification facility users */
uim_bool uim_notify_init(void);
void uim_notify_quit(void);
uim_bool uim_notify_load(const char *name);

const uim_notify_desc *uim_notify_get_desc(void);
uim_bool uim_notify_info(const char *msg_fmt, ...);
uim_bool uim_notify_fatal(const char *msg_fmt, ...);

/* API for pluggable notification agent implementors */
uim_bool uim_notify_plugin_init(void);
void uim_notify_plugin_quit(void);
const uim_notify_desc *uim_notify_plugin_get_desc(void);
uim_bool uim_notify_plugin_info(const char *msg);
uim_bool uim_notify_plugin_fatal(const char *msg);

#ifdef __cplusplus
}
#endif

#endif  /* UIM_NOTIFY_H */
