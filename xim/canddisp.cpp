/*

  Copyright (c) 2003-2005 uim Project http://uim.freedesktop.org/

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

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>

#include "uim/uim.h"

#include "ximserver.h"
#include "xim.h"
#include "convdisp.h"
#include "canddisp.h"
#include "util.h"


static FILE *candwin_r = NULL, *candwin_w = NULL;
static int candwin_pid = 0;
static Canddisp *disp;
static char *command;

static void candwin_read_cb(int fd, int ev);

static char *candwin_command(void)
{
    char *candwin_prog = NULL;

    candwin_prog = getenv("UIM_CANDWIN_PROG");
    if (candwin_prog == NULL) {
#ifdef USE_GTK2
	return "uim-candwin-gtk";
#else
	return NULL;
#endif
    } else
	return candwin_prog;
}

Canddisp *canddisp_singleton()
{
    if (command == NULL)
	  command = candwin_command();

    if (disp == NULL && command) {
	candwin_pid = uim_ipc_open_command(candwin_pid, &candwin_r, &candwin_w, command);
	disp = new Canddisp();
	int fd = fileno(candwin_r);
	if (fd != -1)
	    add_fd_watch(fd, READ_OK, candwin_read_cb);
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

    if (candwin_w == NULL)
	return;

    fprintf(candwin_w, "activate\ncharset=UTF-8\ndisplay_limit=%d\n",
		    display_limit);
    for (i = candidates.begin(); i != candidates.end(); i++) {
	fprintf(candwin_w, "%s\n", *i);
    }
    fprintf(candwin_w, "\n");
    fflush(candwin_w);
}

void Canddisp::select(int index)
{
    if (candwin_w == NULL)
	return;
    fprintf(candwin_w, "select\n");
    fprintf(candwin_w, "%d\n\n", index);
    fflush(candwin_w);
}

void Canddisp::deactivate()
{
    if (candwin_w == NULL)
	return;
    fprintf(candwin_w, "deactivate\n\n");
    fflush(candwin_w);
}

void Canddisp::show()
{
    if (candwin_w == NULL)
	return;
    fprintf(candwin_w, "show\n\n");
    fflush(candwin_w);
}

void Canddisp::hide()
{
    if (candwin_w == NULL)
	return;
    fprintf(candwin_w, "hide\n\n");
    fflush(candwin_w);
}

void Canddisp::move(int x, int y)
{
    if (candwin_w == NULL)
	return;
    fprintf(candwin_w, "move\n");
    fprintf(candwin_w, "%d\n", x);
    fprintf(candwin_w, "%d\n", y);
    fprintf(candwin_w, "\n");
    fflush(candwin_w);
}

static void candwin_read_cb(int fd, int ev)
{
    char buf[1024];
    int n;

    n = read(fd, buf, 1024 - 1);
    if (n == 0) {
	int fd_w = fileno(candwin_w);
	if (fd != -1)
	    close(fd_w);
	close(fd);
	remove_current_fd_watch(fd);
	return;
    }
    if (n == -1)
	return;
    buf[n] = '\0';

    if (!strcmp(buf, "err")) {
	int fd_w = fileno(candwin_w);
	if (fd != -1)
	    close(fd_w);
	close(fd);
	remove_current_fd_watch(fd);
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
