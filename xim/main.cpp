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

// XIM Server supporting CJK languages
// initialize many modules

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <clocale>
#include <csignal>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <sys/select.h>
#include <time.h>

#include "xim.h"
#include "xdispatch.h"
#include "ximserver.h"
#include "canddisp.h"
#include "connection.h"
#include "util.h"
#include "helper.h"

#include "uim/uim-util.h"
#include "uim/uim-im-switcher.h"
#include "uim/uim-scm.h"

Display *XimServer::gDpy;
std::map<Window, XimServer *> XimServer::gServerMap;

// Configuration
int g_option_mask;
int scr_width, scr_height;
int host_byte_order;

#define VERSION_NAME "uim-xim under the way! Version "PACKAGE_VERSION"\n"
const char *version_name=VERSION_NAME;
const char *usage=
"--help , --version :Show usage or version\n"
"--list             :Show available backend conversion engines\n"
"--engine=ENGINE    :Use ENGINE as a backend conversion engine at startup\n"
"--async            :Use on-demand-synchronous method of XIM event flow\n"
"                    (using this option is not safe for Tcl/Tk GUI toolkit)\n"
"--trace            :trace-connection\n"
"--trace-xim        :trace-xim-message\n";
const char *default_engine;


static Atom atom_locales;
static Atom atom_transport;
Atom xim_servers;

struct fd_watch_struct {
    int mask;
    void (*fn)(int, int);
};
static std::map<int, fd_watch_struct> fd_watch_stat;
static std::map<unsigned int, WindowIf *> window_watch_stat;

static char *supported_locales;
std::list<UIMInfo> uim_info;
static void check_pending_xevent(void);

#if UIM_XIM_USE_DELAY
static void timer_check(void);
static void *timer_ptr;
static void (*timer_cb)(void *ptr);
static time_t timer_time;
#endif

bool
pretrans_register()
{
    xim_servers = XInternAtom(XimServer::gDpy, "XIM_SERVERS", 0);
    atom_locales = XInternAtom(XimServer::gDpy, "LOCALES", 0);
    atom_transport = XInternAtom(XimServer::gDpy, "TRANSPORT", 0);
    XFlush(XimServer::gDpy);
    scr_width = DisplayWidth(XimServer::gDpy, 0);
    scr_height = DisplayHeight(XimServer::gDpy, 0);
    return true;
}

WindowIf::~WindowIf()
{
}

void
WindowIf::resize(Window, int, int)
{
    // do nothing
}

void remove_current_fd_watch(int fd)
{
    std::map<int, fd_watch_struct>::iterator i;
    i = fd_watch_stat.find(fd);
    if (i == fd_watch_stat.end())
	return;

    fd_watch_stat.erase(i);
}

void add_fd_watch(int fd, int mask, void (*fn)(int, int))
{
    remove_current_fd_watch(fd);

    fd_watch_struct s;
    s.mask = mask;
    s.fn = fn;
    std::pair<int, fd_watch_struct> p(fd, s);
    fd_watch_stat.insert(p);
}

static void main_loop()
{
    fd_set rfds, wfds;
    struct timeval tv;
    
    while (1) {
	FD_ZERO(&rfds);
	FD_ZERO(&wfds);
#if UIM_XIM_USE_DELAY
	tv.tv_sec = 1;
#else
	tv.tv_sec = 2;
#endif
	tv.tv_usec = 0;

	std::map<int, fd_watch_struct>::iterator it;
	int  fd_max = 0;
	for (it = fd_watch_stat.begin(); it != fd_watch_stat.end(); ++it) {
	    int fd = it->first;
	    if (it->second.mask & READ_OK)
		FD_SET(fd, &rfds);
	    if (it->second.mask & WRITE_OK)
		FD_SET(fd, &wfds);
	    if (fd_max < fd)
		fd_max = fd;
	}
	if ((select(fd_max + 1, &rfds, &wfds, NULL, &tv)) == 0) {
	    check_pending_xevent();
#if UIM_XIM_USE_DELAY
	    timer_check();
#endif
	    continue;
	}

	it = fd_watch_stat.begin();
	while (it != fd_watch_stat.end()) {
	    int fd = it->first;
	    if (FD_ISSET(fd, &rfds))
		it->second.fn(fd, READ_OK);
	    if (FD_ISSET(fd, &wfds))
		it->second.fn(fd, WRITE_OK);
	    // fd_watch_stat may be modified by above functions at
	    // this point.  Since the behavior with incrementing
	    // invalidated iterator is compiler dependent, use safer
	    // way.
	    it = fd_watch_stat.find(fd);
	    if (it == fd_watch_stat.end())	// shouldn't happen
		break;
	    ++it;
	}
#if UIM_XIM_USE_DELAY
	timer_check();
#endif
    }
}

void
add_window_watch(Window id, WindowIf *w, int mask)
{
    std::pair<unsigned int, WindowIf *> p(static_cast<unsigned int>(id), w);
    window_watch_stat.insert(p);

    // Event mask is the same value defined in X,
    // but do not depend on.
    int emask = 0;
    if (mask & EXPOSE_MASK)
	emask |= ExposureMask;
    if (mask & STRUCTURE_NOTIFY_MASK)
	emask |= StructureNotifyMask;

    XSelectInput(XimServer::gDpy, id, emask);
}

void
remove_window_watch(Window id)
{
    std::map<unsigned int, WindowIf *>::iterator i;
    i = window_watch_stat.find(static_cast<unsigned int>(id));
    if (i != window_watch_stat.end())
	window_watch_stat.erase(i);
}

WindowIf *
findWindowIf(Window w)
{
    std::map<unsigned int, WindowIf *>::iterator i;
    i = window_watch_stat.find(static_cast<unsigned int>(w));
    if (i == window_watch_stat.end())
	return NULL;

    return (*i).second;
}

static int
X_ErrorHandler(Display *d, XErrorEvent *e)
{
    if (g_option_mask & OPT_TRACE) {
	if (e->error_code) {
	    char buf[64];
	    XGetErrorText(d, e->error_code, buf, 63);
	    printf("X error occurred. %s\n", buf);
	}
    }

    return 0;
}

static int
X_IOErrorHandler(Display *d)
{
    fprintf(stderr, "%s: X IO error.\n", DisplayString(d));
    return 0;
}


static void
sendSelectionNotify(XEvent *ev, const char *buf, int len)
{
    XEvent e;
    e.type = SelectionNotify;
    e.xselection.requestor = ev->xselectionrequest.requestor;
    e.xselection.selection = ev->xselectionrequest.selection;
    e.xselection.target = ev->xselectionrequest.target;
    e.xselection.time = ev->xselectionrequest.time;
    e.xselection.property = ev->xselectionrequest.property;
    XChangeProperty(XimServer::gDpy, e.xselection.requestor,
		    e.xselection.property,
		    e.xselection.target,
		    8, PropModeReplace,
		    (unsigned char *)buf, len);
    XSendEvent(XimServer::gDpy, e.xselection.requestor, 0, 0, &e);
    XFlush(XimServer::gDpy);
}

void
notifyLocale(XEvent *ev)
{
    sendSelectionNotify(ev, supported_locales,
                        static_cast<int>(strlen(supported_locales)) + 1);
    if (g_option_mask & OPT_TRACE)
	printf("selection notify request for locale.\n");
}

void
notifyTransport(XEvent *ev)
{
    sendSelectionNotify(ev, "@transport=X/", 13 + 1);
    if (g_option_mask & OPT_TRACE)
	printf("selection notify request for transport.\n");
}

void
ProcXEvent(XEvent *e)
{
    Atom p;
    WindowIf *i;
    switch (e->type) {
    case SelectionRequest:
	p = e->xselectionrequest.property;
	if (p == atom_locales)
	    notifyLocale(e);
	else if (p == atom_transport)
	    notifyTransport(e);
	else
	    printf("property %s?\n",
		   XGetAtomName(XimServer::gDpy, e->xselection.property));
	break;
    case Expose:
	if (e->xexpose.count == 0) {
	    i = findWindowIf(e->xexpose.window);
	    if (i)
		i->expose(e->xexpose.window);
	}
	break;
    case ConfigureNotify:
	i = findWindowIf(e->xconfigure.window);
	if (i)
	    i->resize(e->xconfigure.window, e->xconfigure.x, e->xconfigure.y);
	break;
    case DestroyNotify:
	i = findWindowIf(e->xdestroywindow.window);
	if (i)
	    i->destroy(e->xdestroywindow.window);
	remove_window_watch(e->xdestroywindow.window);
	break;
    case ClientMessage:
	procXClientMessage(&e->xclient);
	break;
    case MappingNotify:
	XRefreshKeyboardMapping((XMappingEvent *)e);
	init_modifier_keys();
	break;
    default:;
	//printf("unknown type of X event. %d\n", e->type);
    }
}

static void
check_pending_xevent(void)
{
    XEvent e;
    while (XPending(XimServer::gDpy)) {
	XNextEvent(XimServer::gDpy, &e);
	ProcXEvent(&e);
    }
}

static void
xEventRead(int /* fd */, int /* ev */)
{
    check_pending_xevent();
}

#if UIM_XIM_USE_DELAY
static void
timer_check(void)
{
    if (timer_time > 0 && time(NULL) >= timer_time) {
	timer_time = 0;
	timer_cb(timer_ptr);
    }
}

void
timer_set(int seconds, void (*timeout_cb)(void *ptr), void *ptr)
{
    timer_time = time(NULL) + seconds;
    timer_cb = timeout_cb;
    timer_ptr = ptr;
}

void
timer_cancel()
{
    timer_time = 0;
}
#endif

static void
error_handler_setup()
{
    XSetErrorHandler(X_ErrorHandler);
    XSetIOErrorHandler(X_IOErrorHandler);
}

static int
pretrans_setup()
{
    int fd = XConnectionNumber(XimServer::gDpy);

    add_fd_watch(fd, READ_OK, xEventRead);
    return fd;
}

static void
print_version()
{
    printf("%s", version_name);
}

static void
print_usage()
{
    print_version();
    printf("%s", usage);
    exit(0);
}

static void
get_uim_info()
{
    int res;

    res = uim_init();
    if (res) {
	printf("Failed to init uim\n");
	exit(1);
    }
    uim_context uc = uim_create_context(NULL, "UTF-8", NULL,
					NULL, uim_iconv, NULL);

    struct UIMInfo ui;

    int nr = uim_get_nr_im(uc);
    for (int i = 0; i < nr; i++) {
	ui.name = strdup(uim_get_im_name(uc, i));
	ui.lang = strdup(uim_get_im_language(uc, i));
	ui.desc = strdup(uim_get_im_short_desc(uc, i));
	uim_info.push_back(ui);
    }
    uim_release_context(uc);
}

static void
print_uim_info()
{
    std::list<UIMInfo>::iterator it;

    printf("Supported conversion engines:\n");
    if (uim_info.empty())
	printf("  None.\n");
    else
	for (it = uim_info.begin(); it != uim_info.end(); ++it)
	    printf("  %s (%s)\n", it->name, it->lang);
    
}

static void
clear_uim_info()
{
    std::list<UIMInfo>::iterator it;
    for (it = uim_info.begin(); it != uim_info.end(); ++it) {
	free(it->name);
	free(it->lang);
	free(it->desc);
    }
    uim_info.clear();
}

static void
init_supported_locales()
{
    std::list<char *> locale_list;
    char *locales;
    const char *s;
    int len;

    if (asprintf(&supported_locales, "@locale=") == -1) {
        free(supported_locales);
        return;
    }
    len = static_cast<int>(strlen(supported_locales));

    // get all locales
    s = compose_localenames_from_im_lang("*");
    if (s)
	locales = strdup(s);
    else
	locales = strdup("");
    // replace ':' with ','
    char *sep;
    char *tmp = locales;
    while ((sep = strchr(tmp, ':')) != NULL) {
	*sep = ',';
	tmp = sep;
    }

    len += static_cast<int>(strlen(locales));
    supported_locales = (char *)realloc(supported_locales,
		    sizeof(char) * len + 1);
    if (!supported_locales) {
	fprintf(stderr, "Error: failed to register supported_locales. Aborting....");
	exit(1);
    }

    strcat(supported_locales, locales);
    free(locales);
}

static void
parse_args(int argc, char **argv)
{
    int i;
    for (i = 1; i < argc; i++) {
	if (!strncmp(argv[i], "--", 2)) {
	    char *opt;
	    opt = &argv[i][2];
	    if (!strcmp(opt, "version")) {
		print_version();
		exit(0);
	    } else if (!strcmp(opt, "help")) {
		print_usage();
	    } else if (!strcmp(opt, "list")) {
		get_uim_info();
		print_uim_info();
		exit(0);
	    } else if (!strcmp(opt, "trace")) {
		g_option_mask |= OPT_TRACE;
	    } else if (!strcmp(opt, "trace-xim")) {
		g_option_mask |= OPT_TRACE_XIM;
	    } else if (!strncmp(opt, "engine=", 7)) {
		default_engine = strdup(&argv[i][9]);
	    } else if (!strcmp(opt, "async")) {
		g_option_mask |= OPT_ON_DEMAND_SYNC;
	    }
	}
    }
}

static void check_default_engine(const char *locale)
{
    bool found = false;
    if (default_engine) {
	std::list<UIMInfo>::iterator it;
	for (it = uim_info.begin(); it != uim_info.end(); ++it) {
	    if (!strcmp(it->name, default_engine)) {
		found = true;
		break;
	    }
	}
    }

    if (found == false)
	default_engine = uim_get_default_im_name(locale);
}

static void
get_runtime_env()
{
    int i = 1;
    char *v = (char *)&i;
    if (*v == 1)
	host_byte_order = LSB_FIRST;
    else
	host_byte_order = MSB_FIRST;
}

static void
terminate_x_connection()
{
    int fd = XConnectionNumber(XimServer::gDpy);

    remove_current_fd_watch(fd);
}

void
reload_uim(int reload_libuim)
{
    if (reload_libuim) {
	fprintf(stderr, "\nReloading uim...\n\n");

	terminate_canddisp_connection();
	helper_disconnect_cb();
	terminate_x_connection();

	std::map<Window, XimServer *>::iterator it;
	std::list<InputContext *>::iterator it_c;

	for (it = XimServer::gServerMap.begin(); it != XimServer::gServerMap.end(); ++it) {
	    XimServer *xs = it->second;
	    for (it_c = xs->ic_list.begin(); it_c != xs->ic_list.end(); ++it_c)
		(*it_c)->clear();
	}
	uim_quit();
    }

    clear_uim_info();
    get_uim_info();
    //print_uim_info();

    if (reload_libuim) {
	std::map<Window, XimServer *>::iterator it;
	std::list<InputContext *>::iterator it_c;

	for (it = XimServer::gServerMap.begin(); it != XimServer::gServerMap.end(); ++it) {
	    XimServer *xs = it->second;
	    for (it_c = xs->ic_list.begin(); it_c != xs->ic_list.end(); ++it_c) {
		const char *engine = (*it_c)->get_engine_name();
		(*it_c)->createUimContext(engine);
	    }
	}

	// make sure to use appropriate locale for the focused context
	InputContext *focusedContext = InputContext::focusedContext();
	if (focusedContext)
	    focusedContext->focusIn();

	pretrans_setup();
    }
}

int
main(int argc, char **argv)
{
    const char *locale;

    printf("uim <-> XIM bridge. Supporting multiple locales.\n");

    get_runtime_env();

    parse_args(argc, argv);

    if (g_option_mask & OPT_ON_DEMAND_SYNC)
	printf("Using on-demand-synchronous XIM event flow (not safe for Tcl/TK)\n");
    else
	printf("Using full-synchronous XIM event flow\n");

    signal(SIGPIPE, SIG_IGN);
    signal(SIGUSR1, reload_uim);

    check_helper_connection();

    XimServer::gDpy = XOpenDisplay(NULL);
    if (!XimServer::gDpy) {
	printf("failed to open display!\n");
	return 1;
    }
    if (!pretrans_register()) {
	printf("pretrans_register failed\n");
	return 1;
    }

    get_uim_info();
    print_uim_info();

    locale = setlocale(LC_CTYPE, "");
    if (!locale)
	locale = setlocale(LC_CTYPE, "C");

    check_default_engine(locale);
    init_supported_locales();
    init_modifier_keys();

    std::list<UIMInfo>::iterator it;
    bool res = false;

    // First, setup conversion engine selected by cmdline option or
    // "default-im-name" on ~/.uim.
    for (it = uim_info.begin(); it != uim_info.end(); ++it) {
	if (strcmp(it->name, default_engine) == 0) {
	    XimServer *xs = new XimServer(it->name, it->lang);
	    res = xs->setupConnection(true);
	    if (res)
		printf("XMODIFIERS=@im=uim registered, selecting %s (%s) as default conversion engine\n", it->name, it->lang);
	    else
		delete xs;
	    break;
	}
    }

    if (!res) {
	printf("aborting...\n");
	return 1;
    }

    connection_setup();
    error_handler_setup();
    if (pretrans_setup() == -1)
	return 0;

#if HAVE_XFT_UTF8_STRING
    if (uim_scm_symbol_value_bool("uim-xim-use-xft-font?"))
	init_default_xftfont(); // setup Xft fonts for Ov/Rw preedit
#endif
    check_candwin_style();
    check_candwin_pos_type();

    // Handle pending events to prevent hang just after startup
    check_pending_xevent();

    main_loop();
    return 0;
}

/*
 * Local variables:
 *  c-indent-level: 4
 *  c-basic-offset: 4
 * End:
 */
