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
# include <config.h>
#endif

#include <cerrno>
#include <cstdio>
#include <cstdlib>
#include <fcntl.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/param.h>

#include "uim/uim.h"
#include "uim/uim-util.h"
#include "uim/uim-scm.h"

#include "ximserver.h"
#include "xim.h"
#include "convdisp.h"
#include "canddisp.h"
#include "util.h"

#define CANDWIN_PROG_PREFIX	(UIM_LIBEXECDIR "/uim-candwin")
#if defined(USE_QT_CANDWIN)
  #define CANDWIN_PROG_SUFFIX	"-qt"
#elif defined(USE_QT4_CANDWIN)
  #define CANDWIN_PROG_SUFFIX	"-qt4"
#elif defined(USE_GTK_CANDWIN) && defined(USE_GTK2)
  #define CANDWIN_PROG_SUFFIX	"-gtk"
#elif defined(USE_GTK3_CANDWIN)
  #define CANDWIN_PROG_SUFFIX	"-gtk3"
#else
  #define NO_TOOLKIT
#endif

static FILE *candwin_r, *candwin_w;
static int candwin_pid;
static Canddisp *disp;
static const char *command;
static bool candwin_initted = false;

static void candwin_read_cb(int fd, int ev);

static const char *candwin_command(void)
{
    static char candwin_prog[MAXPATHLEN];
    const char *user_config;
    char *str, *style;

#ifdef NO_TOOLKIT
    return NULL;
#else
    /*
      Search order of candwin_command be summarized as follows
	 1. UIM_CANDWIN_PROG -- mainly for debugging purpose
	 2. value in 'uim-candwin-prog' symbol (deprecated)
	 3. default toolkit's candwin program determined by ./configure
	    and the style is selected from 'candidate-window-style' symbol
     */

    user_config = getenv("UIM_CANDWIN_PROG");
    str = user_config ?  strdup(user_config) :
	    uim_scm_symbol_value_str("uim-candwin-prog");

    if (str && *str) {
	snprintf(candwin_prog, MAXPATHLEN, UIM_LIBEXECDIR "/%s", str);
	free(str);

	return candwin_prog;
    }
    free(str);

#define TYPELEN	20
    style = uim_scm_symbol_value_str("candidate-window-style");
    char type[TYPELEN] = "";
    if (style) {
	if (!strcmp(style, "table"))
	    strlcpy(type, "-tbl", TYPELEN);
	else if (!strcmp(style, "horizontal"))
	    strlcpy(type, "-horizontal", TYPELEN);
    }
    snprintf(candwin_prog, MAXPATHLEN, "%s%s%s", CANDWIN_PROG_PREFIX, type, CANDWIN_PROG_SUFFIX);

    return candwin_prog;
#endif
}

Canddisp *canddisp_singleton()
{
    if (XimServer::gCandWinStyleUpdated) {
	terminate_canddisp_connection();
	command = NULL;
	XimServer::gCandWinStyleUpdated = false;
    }

    if (!command)
	command = candwin_command();

    if (!candwin_initted && command) {
	candwin_pid = uim_ipc_open_command(candwin_pid, &candwin_r, &candwin_w, command);
	if (disp)
	    delete disp;
	disp = new Canddisp();
	int fd = fileno(candwin_r);
	if (fd != -1) {
	    int flag = fcntl(fd, F_GETFL);
	    if (flag != -1) {
		flag |= O_NONBLOCK;
		if (fcntl(fd, F_SETFL, flag) != -1)
		    add_fd_watch(fd, READ_OK, candwin_read_cb);
	    }
	}
	candwin_initted = true;
    }
    return disp;
}

Canddisp::Canddisp()
{
}

Canddisp::~Canddisp() {
}

void Canddisp::activate(std::vector<const char *> candidates, int display_limit)
{
    std::vector<const char *>::iterator i;

    if (!candwin_w)
	return;

    fprintf(candwin_w, "activate\fcharset=UTF-8\fdisplay_limit=%d\f",
		    display_limit);
    for (i = candidates.begin(); i != candidates.end(); ++i)
	fprintf(candwin_w, "%s\f", *i);
    fprintf(candwin_w, "\f");
    fflush(candwin_w);
    check_connection();
}

#if UIM_XIM_USE_NEW_PAGE_HANDLING
void Canddisp::set_nr_candidates(int nr, int display_limit)
{
    if (!candwin_w)
	return;

    fprintf(candwin_w, "set_nr_candidates\f");
    fprintf(candwin_w, "%d\f", nr);
    fprintf(candwin_w, "%d\f", display_limit);
    fprintf(candwin_w, "\f");
    fflush(candwin_w);
    check_connection();
}

void Canddisp::set_page_candidates(int page, CandList candidates)
{
    std::vector<const char *>::iterator i;

    if (!candwin_w)
	return;

    fprintf(candwin_w, "set_page_candidates\fcharset=UTF-8\fpage=%d\f", page);
    for (i = candidates.begin(); i != candidates.end(); ++i)
	fprintf(candwin_w, "%s\f", *i);
    fprintf(candwin_w, "\f");
    fflush(candwin_w);
    check_connection();
}

void Canddisp::show_page(int page)
{
    if (!candwin_w)
	return;

    fprintf(candwin_w, "show_page\f");
    fprintf(candwin_w, "%d\f", page);
    fprintf(candwin_w, "\f");
    fflush(candwin_w);
    check_connection();
}
#endif /* UIM_XIM_USE_NEW_PAGE_HANDLING */

void Canddisp::select(int index, bool need_hilite)
{
    if (!candwin_w)
	return;
    fprintf(candwin_w, "select\f");
    fprintf(candwin_w, "%d\f", index);
    fprintf(candwin_w, "%d\f\f", need_hilite? 1 : 0);
    fflush(candwin_w);
    check_connection();
}

void Canddisp::deactivate()
{
    if (!candwin_w)
	return;
    fprintf(candwin_w, "deactivate\f\f");
    fflush(candwin_w);
    check_connection();
}

void Canddisp::show()
{
    if (!candwin_w)
	return;
    fprintf(candwin_w, "show\f\f");
    fflush(candwin_w);
    check_connection();
}

void Canddisp::hide()
{
    if (!candwin_w)
	return;
    fprintf(candwin_w, "hide\f\f");
    fflush(candwin_w);
    check_connection();
}

void Canddisp::move(int x, int y)
{
    if (!candwin_w)
	return;
    fprintf(candwin_w, "move\f");
    fprintf(candwin_w, "%d\f", x);
    fprintf(candwin_w, "%d\f", y);
    fprintf(candwin_w, "\f");
    fflush(candwin_w);
    check_connection();
}

void Canddisp::show_caret_state(const char *str, int timeout)
{
    if (!candwin_w)
	return;
    fprintf(candwin_w, "show_caret_state\f");
    fprintf(candwin_w, "%d\f", timeout);
    fprintf(candwin_w, "%s\f", str);
    fprintf(candwin_w, "\f");
    fflush(candwin_w);
    check_connection();
}

void Canddisp::update_caret_state()
{
    if (!candwin_w)
	return;
    fprintf(candwin_w, "update_caret_state\f\f");
    fflush(candwin_w);
    check_connection();
}

void Canddisp::hide_caret_state()
{
    if (!candwin_w)
	return;
    fprintf(candwin_w, "hide_caret_state\f\f");
    fflush(candwin_w);
    check_connection();
}

void Canddisp::check_connection()
{
    if (errno == EBADF || errno == EPIPE)
	terminate_canddisp_connection();
}

static void candwin_read_cb(int fd, int /* ev */)
{
    char buf[1024];
    int n;

    n = static_cast<int>(read(fd, buf, 1024 - 1));
    if (n == 0) {
	terminate_canddisp_connection();
	return;
    }
    if (n == -1)
	return;
    buf[n] = '\0';

    if (!strcmp(buf, "err")) {
	terminate_canddisp_connection();
	return;
    }

    InputContext *focusedContext = InputContext::focusedContext();
    if (focusedContext) {
	char *line = buf;
	char *eol = strchr(line, '\n');
	if (eol != NULL)
	    *eol = '\0';

	if (strcmp("index", line) == 0) {
	    line = eol + 1;
	    eol = strchr(line, '\n');
	    if (eol != NULL)
		*eol = '\0';

	    int index;
	    sscanf(line, "%d", &index);
	    focusedContext->candidate_select(index);
	    uim_set_candidate_index(focusedContext->getUC(), index);
	    // send packet queue for drawing on-the-spot preedit strings
	    focusedContext->get_ic()->force_send_packet();
	}
    }
    return;
}

void terminate_canddisp_connection()
{
    int fd_r, fd_w;

    if (candwin_r) {
	fd_r = fileno(candwin_r);
	close(fd_r);
	remove_current_fd_watch(fd_r);
    }
    if (candwin_w) {
	fd_w = fileno(candwin_w);
	close(fd_w);
    }

    candwin_w = candwin_r = NULL;
    candwin_initted = false;
    return;
}
