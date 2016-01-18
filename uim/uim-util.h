/*
  uim-util.h utility function prototypes for uim.

  Copyright (c) 2004-2013 uim Project https://github.com/uim/uim

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

#ifndef UIM_UTIL_H
#define UIM_UTIL_H

#include <stdio.h>
#include <sys/types.h>

#include "uim.h"

#ifdef __cplusplus
extern "C" {
#endif

/**
 * Returns human-readable language name from a locale string.
 *
 * @param locale locale string. typical format of this string is
 * ll_CC.charset. e.g. for Japanese locale, ja_JP.EUC-JP ll is language code
 * defined in ISO 639-1, CC is country code defined in ISO 3166.
 *
 * @return untranslated language name string, or else "-" if no language is
 * matched. apply gettext() in caller side if needed.
 *
 * @see uim_create_context
 */
const char *
uim_get_language_name_from_locale(const char *locale);

/**
 * Returns ISO 639-1 language code from a human-readable language name.
 *
 * @param language_name a human-readable language name in English such as
 * "Japanese".
 *
 * @return ISO 639-1 language code such as "ja", or else "-" if no language
 * is matched.
 */
const char *
uim_get_language_code_from_language_name(const char *language_name);


/* command execution in pipe-connected subprocess (like popen(3))*/
pid_t uim_ipc_open_command(pid_t old_pid,
			   FILE **read_handler, FILE **write_handler,
			   const char *command);
pid_t uim_ipc_open_command_with_option(pid_t old_pid,
				       FILE **read_handler,
				       FILE **write_handler,
				       const char *command,
				       const char *option);
char *uim_ipc_send_command(pid_t *pid,
			   FILE **read_handler, FILE **write_handler,
			   const char *command, const char *str);

/* an uim_code_converter implementation using iconv */
extern struct uim_code_converter *uim_iconv;


#ifdef __cplusplus
}
#endif
#endif
