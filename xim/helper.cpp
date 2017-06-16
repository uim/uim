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

// uim-helper connection

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <unistd.h>
#include <sys/types.h>

#include "xim.h"
#include "ximserver.h"
#include "canddisp.h"
#include "util.h"

#include "uim/uim-util.h"
#include "uim/uim-helper.h"
#include "uim/uim-im-switcher.h"

int lib_uim_fd = -1;

static void
parse_helper_str_im_change(const char *level, const char *engine) {
    InputContext *focusedContext = InputContext::focusedContext();

    if (!strcmp(level, "im_change_whole_desktop")) {
	std::map<Window, XimServer *>::iterator it;
	for (it = XimServer::gServerMap.begin(); it != XimServer::gServerMap.end(); ++it) {
	    (*it).second->changeContext(engine);
	}
    }

    if (focusedContext) {
	if (!strcmp(level, "im_change_this_text_area_only"))
	    focusedContext->changeContext(engine);
	else if (!strcmp(level, "im_change_this_application_only"))
	    get_im_by_id(focusedContext->get_ic()->get_imid())->changeContext(engine);
    }
}

void
send_im_list(void)
{
    char *buf = NULL, *tmp = NULL;
    int len;
    InputContext *focusedContext = InputContext::focusedContext();
    const char *current_im_name =
	    uim_get_current_im_name(focusedContext->getUC());
    const char *encoding = focusedContext->get_ic()->get_encoding();
    const char *client_locale = NULL;
    
    if (strcmp(encoding, "UTF-8"))
	client_locale = focusedContext->get_ic()->get_lang_region();

    if (asprintf(&buf, "im_list\ncharset=UTF-8\n") == -1) {
        free(buf);
        return;
    }

    std::list<UIMInfo>::iterator it;
    for (it = uim_info.begin(); it != uim_info.end(); ++it) {
	if (client_locale) { // context with legacy encodings
	    const char *engine_locales =
		    compose_localenames_from_im_lang(it->lang);
	    if (!is_locale_included(engine_locales, client_locale))
		continue;
	}

	const char *language;
	language = uim_get_language_name_from_locale(it->lang);
	if (asprintf(&tmp, "%s\t%s\t%s\t", it->name,
				       language ? language : "",
				       it->desc ? it->desc : "") == -1) {
            free(buf);
            return;
        }
	len = static_cast<int>(strlen(buf) + strlen(tmp));
	buf = (char *)realloc(buf, sizeof(char) * len + 1);
	if (!buf) {
	    free(tmp);
	    return;
        }
	strcat(buf, tmp);
	free(tmp);

	if (!strcmp(it->name, current_im_name)) {
	    if (asprintf(&tmp, "selected\n") == -1) {
                free(tmp);
                return;
            }
	    len = static_cast<int>(strlen(buf) + strlen(tmp));
	    buf = (char *)realloc(buf, sizeof(char) * len + 1);
	    if (!buf) {
		free(tmp);
		return;
            }
	    strcat(buf, tmp);
	    free(tmp);
	} else {
	    if (asprintf(&tmp, "\n") == -1) {
                free(tmp);
                return;
            }
	    len = static_cast<int>(strlen(buf) + strlen(tmp));
	    buf = (char *)realloc(buf, sizeof(char) * len + 1);
	    if (!buf) {
		free(tmp);
		return;
            }
	    strcat(buf, tmp);
	    free(tmp);
	}
    }
    uim_helper_send_message(lib_uim_fd, buf);
    free(buf);
}

static void
helper_str_parse(char *str)
{
    InputContext *focusedContext = InputContext::focusedContext();

    char *line = str;
    char *eol = strchr(line, '\n');
    if (eol != NULL)
	*eol = '\0';
    else
	return;

    if (focusedContext) {
	if (strcmp("prop_list_get", line) == 0) {
	    uim_prop_list_update(focusedContext->getUC());
	    return;
	} else if (strcmp("prop_label_get", line) == 0) {
	    uim_prop_label_update(focusedContext->getUC());
	    return;
	} else if (strcmp("prop_activate", line) == 0) {
	    line = eol + 1;
	    eol = strchr(line, '\n');
	    if (eol != NULL)
		*eol = '\0';
	    else
		return;

	    uim_prop_activate(focusedContext->getUC(), line);
	    return;
	} else if (strncmp("focus_in", line, 8) == 0) {
	    InputContext::deletefocusedContext();
	    Canddisp *disp = canddisp_singleton();
	    disp->hide();
	    disp->hide_caret_state();
	    return;
 	} else if (strcmp("im_list_get", line) == 0) {
	    send_im_list();
	    return;
	} else if (strcmp("commit_string", line) == 0) {
	    line = eol + 1;
	    eol = strchr(line, '\n');
	    if (eol != NULL)
		*eol = '\0';
	    else
		return;
	    if (!strncmp(line, "charset=", 8)) {
		const char *charset = line + 8;
		line = eol + 1;
		eol = strchr(line, '\n');
		if (eol != NULL)
		    *eol = '\0';
		else
		    return;
		if (!strcmp(charset, "UTF-8"))
		    focusedContext->extra_input(line);
		else {
		    int len = static_cast<int>(strlen(line));
		    char *utf8_str = (char *)malloc(len * 6 + 1);
		    if (!utf8_str)
			return;
		    mb_string_to_utf8(utf8_str, line, len * 6, charset);
		    focusedContext->extra_input(utf8_str);
		    free(utf8_str);
		}
	    } else
		focusedContext->extra_input(line);
	    return;
	}
    }

    if (strncmp("im_change_", line, 10) == 0) {
	char *engine;
	engine = eol + 1;
	eol = strchr(engine, '\n');
	if (eol != NULL)
	    *eol = '\0';
	else
	    return;

	parse_helper_str_im_change(line, engine);
	return;
    } else if (strcmp("prop_update_custom", line) == 0) {
	line = eol + 1;
	eol = strchr(line, '\n');
	if (eol != NULL)
	    *eol = '\0';
	else
	    return;

	char *custom = line;
	char *val = eol + 1;
	eol = strchr(val, '\n');
	if (eol != NULL)
	    *eol = '\0';
	else
	    return;

	std::map<Window, XimServer *>::iterator it;
	for (it = XimServer::gServerMap.begin(); it != XimServer::gServerMap.end(); ++it) {
	    (*it).second->customContext(custom, val);
	}
	return;
    } else if (strcmp("custom_reload_notify", line) == 0) {
	std::map<Window, XimServer *>::iterator it;
	for (it = XimServer::gServerMap.begin(); it != XimServer::gServerMap.end(); ++it) {
	    (*it).second->reloadConfigs();
	}
	return;
    }
}

static void
helper_read_cb(int fd, int /* ev */)
{
    uim_helper_read_proc(fd);
    char *tmp;
    while ((tmp = uim_helper_get_message())) {
	helper_str_parse(tmp);
	free(tmp);
    }
}

void
helper_disconnect_cb(void)
{
    remove_current_fd_watch(lib_uim_fd);
    close(lib_uim_fd);
    lib_uim_fd = -1;
}

void
check_helper_connection(void)
{
    if (lib_uim_fd < 0) {
	lib_uim_fd = uim_helper_init_client_fd(helper_disconnect_cb);
	if (lib_uim_fd >= 0)
	    add_fd_watch(lib_uim_fd, READ_OK, helper_read_cb);
    }
}

/*
 * Local variables:
 *  c-indent-level: 4
 *  c-basic-offset: 4
 * End:
 */
