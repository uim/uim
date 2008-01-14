/*
 * Copyright (c) 2007-2008 Iwata <iwata@quasiquote.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

/*                         !WARNING!

   This API is experimental.

*/

#ifndef _uim_notify_h_included_
#define _uim_notify_h_included_

#ifdef __cplusplus
extern "C" {
#endif

typedef struct uim_notify_desc_ {
  const char *name;
  const char *desc;
} uim_notify_desc;

/* API for notification facility users */
int uim_notify_init(void);
void uim_notify_quit(void);
int uim_notify_load(const char *name);

/* API for notification facility users */
const uim_notify_desc* uim_notify_get_desc(void);
int uim_notify_info(const char *msg_fmt, ...);
int uim_notify_fatal(const char *msg_fmt, ...);

/* API for pluggable notification mechanism providers */
int uim_notify_plugin_init(void);
void uim_notify_plugin_quit(void);
const uim_notify_desc* uim_notify_plugin_get_desc(void);
int uim_notify_plugin_info(const char *msg);
int uim_notify_plugin_fatal(const char *msg);

/* builtin notify module */
const uim_notify_desc* uim_notify_stderr_get_desc(void);

#ifdef __cplusplus
}
#endif

#endif  /* _uim_notify_h_included_ */
