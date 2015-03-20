/*

  Copyright (c) 2003-2013 uim Project http://code.google.com/p/uim/

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

#define UIM_XIM_USE_JAPANESE_KANA_KEYBOARD_HACK 1

#include <cctype>
#include <clocale>
#include <cstdio>
#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <X11/Xutil.h>
#include <X11/XKBlib.h>

/* workaround for pre X11R6.7 */
#ifndef XK_KOREAN
#define XK_KOREAN
#endif
#ifndef XK_KATAKANA
#define XK_KATAKANA
#endif
#include <X11/keysymdef.h>

#include "xim.h"
#include "convdisp.h"
#include "canddisp.h"
#include "ximserver.h"
#include "util.h"
#include "helper.h"

#include "uim/uim-helper.h"
#include "uim/uim-im-switcher.h"
#include "uim/uim-scm.h"
#include "uim/uim-x-util.h"

#ifndef XK_dead_horn
#define XK_dead_horn	0xfe62
#endif

#ifndef __GNUC__
# ifdef HAVE_ALLOCA_H
#  include <alloca.h>
# endif
#endif

extern int lib_uim_fd;
extern Atom xim_servers;
InputContext *InputContext::mFocusedContext = NULL;

static int check_modifier(std::list<KeySym> list);
static int gMod1Mask, gMod2Mask, gMod3Mask, gMod4Mask, gMod5Mask;
static int gXNumLockMask;

CandWinPosType XimServer::gCandWinPosType;
CandWinStyle XimServer::gCandWinStyle;
bool XimServer::gCandWinStyleUpdated;

void print_ustring(uString *s)
{
    uString::iterator i;
    printf("length=%d : ", (int)s->size());
    uchar ch;
    char utf8[6];
    int nbyte;
    for (i = s->begin(); i != s->end(); ++i) {
	ch = *i;
	nbyte = utf8_wctomb((unsigned char *)utf8, ch);
	utf8[nbyte] = '\0';
	printf("%s", utf8);
    }
    printf("\n");
}

void erase_ustring(uString *s)
{
    s->erase(s->begin(), s->end());
}

void append_ustring(uString *d, uString *s)
{
    uString::iterator i;
    for (i = s->begin(); i !=s->end(); ++i) {
	d->push_back(*i);
    }
}

XimServer::XimServer(const char *name, const char *lang)
{
    mIMName = strdup(name);
    mIMLang = lang;
}

InputContext *XimServer::createContext(XimIC *xic, const char *engine)
{
    InputContext *ic = new InputContext(this, xic, engine);
    ic_list.push_back(ic);
    return ic;
}

void XimServer::deleteContext(InputContext *ic)
{
    std::list<InputContext *>::iterator it;
    for (it = ic_list.begin(); it != ic_list.end(); ++it) {
	if (*it == ic) {
	    ic_list.erase(it);
	    break;
	}
    }
}

void XimServer::changeContext(const char *engine) {
    set_im(engine);
    std::list<InputContext *>::iterator it;
    for (it = ic_list.begin(); it != ic_list.end(); ++it) {
	(*it)->changeContext(engine);
    }
    // make sure to use appropriate locale for the focused context
    InputContext *focusedContext = InputContext::focusedContext();
    if (focusedContext)
	focusedContext->focusIn();
}

void XimServer::customContext(const char *custom, const char *val) {
    std::list<InputContext *>::iterator it;
    for (it = ic_list.begin(); it != ic_list.end(); ++it) {
	(*it)->customContext(custom, val);
	break;
    }

    // Updated global IM of XimServer
    if (!strcmp(custom, "custom-preserved-default-im-name") &&
	uim_scm_symbol_value_bool("custom-activate-default-im-name?"))
	set_im(++val);

#if HAVE_XFT_UTF8_STRING
    if (!strcmp(custom, "uim-xim-xft-font-name"))
	update_default_xftfont();
#endif

    if (!strcmp(custom, "bridge-show-input-state?") &&
	!uim_scm_symbol_value_bool("bridge-show-input-state?")) {
	Canddisp *disp = canddisp_singleton();
	disp->hide_caret_state();
    }

    if (!strcmp(custom, "candidate-window-position"))
	check_candwin_pos_type();

    if (!strcmp(custom, "candidate-window-style"))
	check_candwin_style();
}

void XimServer::reloadConfigs() {
#if 1
    uim_prop_reload_configs();
    reload_uim(0);
#else
    reload_uim(1);
#endif

    // Updated global IM of XimServer
    char *im = uim_scm_symbol_value_str("default-im-name");
    if (im)
	set_im(im);
    free(im);

#if HAVE_XFT_UTF8_STRING
    update_default_xftfont();
#endif

    if (!uim_scm_symbol_value_bool("bridge-show-input-state?")) {
	Canddisp *disp = canddisp_singleton();
	disp->hide_caret_state();
    }

    check_candwin_style();
    check_candwin_pos_type();
}

bool
XimServer::setupConnection(bool useDefaultIM)
{
    const char *buf;
    if (!useDefaultIM) {
	return false;
    } else {
	buf = "@server=uim";
    }
    mServerAtom = XInternAtom(XimServer::gDpy, buf, 0);
    Window owner = XGetSelectionOwner(XimServer::gDpy, mServerAtom);
    if (owner != None) {
	if (!useDefaultIM)
	    printf("Another instance exists (uim-%s).\n", mIMName);
	else
	    printf("Another instance exists (uim).\n");
	return false;
    }
    mSelectionWin = XCreateSimpleWindow(XimServer::gDpy,
					DefaultRootWindow(XimServer::gDpy),
					0, 0, 1, 1,
					1, 0, 0);
    XSetSelectionOwner(XimServer::gDpy, mServerAtom, mSelectionWin, CurrentTime);
    XSelectInput(XimServer::gDpy, DefaultRootWindow(XimServer::gDpy), 0);
    XSync(XimServer::gDpy, False);

    Atom type;
    int format;
    unsigned long nr_prop, nr_bytes;
    Atom *prop;
    int mode = PropModePrepend;
    int valuechange = 1;

    XGetWindowProperty(XimServer::gDpy, DefaultRootWindow(XimServer::gDpy),
		       xim_servers, 0, 8192 ,False,
		       XA_ATOM, &type, &format,
		       &nr_prop, &nr_bytes, (unsigned char **)(uintptr_t)&prop);
    int i;
    if (type != XA_ATOM || format != 32)
	mode = PropModeReplace;
    else {
	for (i = 0; i < (int)nr_prop; i++) {
	    if (prop[i] == mServerAtom) {
		mode = PropModeAppend;
		valuechange = 0;
		break;
	    }
	}
    }
    if (nr_prop)
	XFree(prop);

    XChangeProperty(XimServer::gDpy, DefaultRootWindow(XimServer::gDpy),
		    xim_servers,
		    XA_ATOM, 32,
		    mode, (unsigned char *)&mServerAtom,
		    valuechange ? 1 : 0);
    std::pair<Window, XimServer *> p(mSelectionWin, this);
    gServerMap.insert(p);
    return true;
}

void
XimServer::strToUstring(uString *d, const char *s)
{
    int len;
    int l = 0, nbyte = 0;
    uchar ch;

    len = static_cast<int>(strlen(s));
    while (l < len && *s != 0 &&
	   (nbyte = utf8_mbtowc(&ch, (const unsigned char *)s, len - l)) > 0) {
	    d->push_back(ch);
	    s += nbyte;
	    l += nbyte;
    }
}

XimServer *XimServer::findServer(Window w)
{
    std::map<Window, XimServer *>::iterator it;
    it = gServerMap.find(w);
    if (it == gServerMap.end())
	return NULL;

    return it->second;
}

const char *XimServer::getIMName()
{
    return mIMName;
}

const char *XimServer::getIMLang()
{
    return mIMLang;
}

void XimServer::set_im(const char *engine)
{
    free(mIMName);

    mIMName = strdup(engine);
    mIMLang = get_im_lang_from_engine(engine);
}

const char *get_im_lang_from_engine(const char *engine)
{
    std::list<UIMInfo>::iterator it;
    for (it = uim_info.begin(); it != uim_info.end(); ++it) {
	if (!strcmp(it->name, engine))
	    return it->lang;
    }
    return "en"; // For safety...
}

//
// Methods for InputContext
InputContext::InputContext(XimServer *svr, XimIC *ic, const char *engine)
{
    mXic = ic;
    m_pe = new pe_stat(this);
    mConvdisp = NULL;
    mServer = svr;
    mEngineName = NULL;
    mLocaleName = NULL;
    mFocusedContext = this;
    createUimContext(engine);
    mCandwinActive = false;
#if UIM_XIM_USE_NEW_PAGE_HANDLING
    mNumCandidates = 0;
#endif
    mNumPage = 1;
    mDisplayLimit = 0;
    mCaretStateShown = false;
}

InputContext::~InputContext()
{
#if UIM_XIM_USE_DELAY
    timer_cancel();
#endif
    if (mFocusedContext == this)
	mFocusedContext = NULL;

    if (mConvdisp)
	mConvdisp->set_pe(NULL);

    delete m_pe;
    uim_release_context(mUc);
    mServer->deleteContext(this);
    free(mEngineName);
    free(mLocaleName);
}

void
InputContext::createUimContext(const char *engine)
{
    char *locale;
    const char *client_locale, *engine_locales;
    const char *encoding;
    const char *real_im;

    encoding = mXic->get_encoding();
    client_locale = mXic->get_lang_region();
    engine_locales = compose_localenames_from_im_lang(get_im_lang_from_engine(engine));

    if (!strcmp(encoding, "UTF-8")) {
	real_im = engine;
	if (is_locale_included(engine_locales, client_locale))
	    locale = strdup(client_locale);
	else {
	    locale = get_prefered_locale(engine_locales);
	}
    } else {
	// Use default engine for corresponding encoding of the client
	// unless encoding matches with selected engine.
	if (!is_locale_included(engine_locales, client_locale)) {
	    const char *test_im = uim_get_default_im_name(client_locale);
	    const char *test_im_lang = get_im_lang_from_engine(test_im);
	    const char *test_im_locales = compose_localenames_from_im_lang(test_im_lang);
	    if (is_locale_included(test_im_locales, client_locale))
		real_im = test_im;
	    else
		real_im = uim_get_im_name_for_locale(client_locale);

	} else
	    real_im = engine;

	locale = strdup(client_locale);
    }

    locale = (char *)realloc(locale, strlen(locale) + strlen(encoding) + 2);
    strcat(locale, ".");
    strcat(locale, encoding);

    setlocale(LC_CTYPE, locale);

    free(mLocaleName);
    mLocaleName = locale;

    if (mEngineName != real_im) {
      free(mEngineName);
      mEngineName = strdup(real_im);
    }

    uim_context uc = uim_create_context((void *) this, "UTF-8",
					NULL, real_im, NULL,
					InputContext::commit_cb);

    if (uc) {
	uim_set_preedit_cb(uc,
			InputContext::clear_cb,
			InputContext::pushback_cb,
			InputContext::update_cb);
	uim_set_candidate_selector_cb(uc,
			InputContext::candidate_activate_cb,
			InputContext::candidate_select_cb,
			InputContext::candidate_shift_page_cb,
			InputContext::candidate_deactivate_cb);
	uim_set_prop_list_update_cb(uc,
			InputContext::update_prop_list_cb);
#if 0
	uim_set_prop_label_update_cb(uc,
			InputContext::update_prop_label_cb);
#endif
	uim_set_configuration_changed_cb(uc,
			InputContext::configuration_changed_cb);
	uim_set_im_switch_request_cb(uc,
			InputContext::switch_app_global_im_cb,
			InputContext::switch_system_global_im_cb);
#if UIM_XIM_USE_DELAY
	uim_set_delay_candidate_selector_cb(uc,
			InputContext::candidate_activate_with_delay_cb);
#endif

	if (mFocusedContext == this)
	    uim_prop_list_update(uc);
    }
    mUc = uc;
}

void
InputContext::changeContext(const char *engine)
{
    const char *encoding, *im_lang;

    if (!strcmp(mEngineName, engine))
	return;

    encoding = mXic->get_encoding();
    im_lang = get_im_lang_from_engine(engine);

    // Don't change im unless encoding matches for clients with legacy locales.
    if (strcmp(encoding, "UTF-8")) {
	const char *client_locale = mXic->get_lang_region();
	const char *engine_locales = compose_localenames_from_im_lang(im_lang);

	if (!is_locale_included(engine_locales, client_locale))
	    return;
    }

    clear();
    uim_release_context(mUc);
    createUimContext(engine); // mUc, mEngineName, and locale will be set here.
    if (mConvdisp) {
	mConvdisp->set_im_lang(get_im_lang_from_engine(mEngineName));
	mConvdisp->set_locale_name(mLocaleName);
    }
}

void InputContext::configuration_changed()
{
    const char *engine = uim_get_current_im_name(mUc);

    review_im(engine);

    InputContext *focusedContext = InputContext::focusedContext();
    if (this == focusedContext)
	send_im_list();
}

void InputContext::switch_app_global_im(const char *name)
{
    get_im_by_id(this->get_ic()->get_imid())->changeContext(name);
}

void InputContext::switch_system_global_im(const char *name)
{
    char *msg;
    std::map<Window, XimServer *>::iterator it;

    for (it = XimServer::gServerMap.begin(); it != XimServer::gServerMap.end(); ++it)
	(*it).second->changeContext(name);

    if (asprintf(&msg, "im_change_whole_desktop\n%s\n", name) == -1) {
        free(msg);
        return;
    }
    uim_helper_send_message(lib_uim_fd, msg);
    free(msg);
}

void InputContext::review_im(const char *engine)
{
    char *locale, *prev_engine;
    const char *client_locale, *engine_locales;
    const char *encoding;

    prev_engine = mEngineName;
    mEngineName = strdup(engine);
    encoding = mXic->get_encoding();
    client_locale = mXic->get_lang_region();
    engine_locales = compose_localenames_from_im_lang(get_im_lang_from_engine(engine));

    if (!strcmp(encoding, "UTF-8")) {
	if (is_locale_included(engine_locales, client_locale))
	    locale = strdup(client_locale);
	else
	    locale = get_prefered_locale(engine_locales);
	locale = (char *)realloc(locale, strlen(locale) + strlen(".UTF-8") + 1);
	strcat(locale, ".UTF-8");
	setlocale(LC_CTYPE, locale);
	free(mLocaleName);
	mLocaleName = locale;
    } else {
	if (!is_locale_included(engine_locales, client_locale)) {
	    clear();
	    uim_switch_im(mUc, prev_engine);
	    free(mEngineName);
	    mEngineName = strdup(prev_engine);
	}
    }
    free(prev_engine);
}

void
InputContext::customContext(const char *custom, const char *val)
{
    uim_prop_update_custom(mUc, custom, val);
}

InputContext *
InputContext::focusedContext()
{
    return mFocusedContext;
}

void
InputContext::deletefocusedContext()
{
    mFocusedContext = NULL;
}

void
InputContext::focusIn()
{
    setlocale(LC_CTYPE, mLocaleName);

    check_helper_connection();
    uim_helper_client_focus_in(mUc);
    mFocusedContext = this;
    if (mConvdisp) {
	mConvdisp->unset_focus();
	mConvdisp->move_candwin();
	mConvdisp->update_caret_state();
    }
    uim_prop_list_update(mUc);	
    uim_prop_label_update(mUc);	
    if (hasActiveCandwin())
	candidate_update();
    uim_focus_in_context(mUc);
}

void
InputContext::focusOut()
{
    uim_focus_out_context(mUc);
    uim_helper_client_focus_out(mUc);
    if (mFocusedContext == this) {
	Canddisp *disp = canddisp_singleton();
	if (isCaretStateShown())
	    disp->hide_caret_state();
	if (hasActiveCandwin())
	    disp->hide();
    }
}

XimServer *
InputContext::getServer()
{
    return mServer;
}

uim_context
InputContext::getUC()
{
    return mUc;
}

void
InputContext::commit_cb(void *ptr, const char *str)
{
    InputContext *ic = (InputContext *)ptr;
    XimIC *xic = ic->get_ic();

    ic->clear_pe_stat();
    ic->update_preedit();
    xic->commit_string(str);
}

void InputContext::clear_cb(void *ptr)
{
    InputContext *ic = (InputContext *)ptr;
    ic->clear_pe_stat();
}

void InputContext::pushback_cb(void *ptr, int attr, const char *str)
{
    InputContext *ic = (InputContext *)ptr;
    ic->pushback_preedit_string(attr, str);
}

void InputContext::update_cb(void *ptr)
{
    InputContext *ic = (InputContext *)ptr;
    ic->update_preedit();
}

void InputContext::candidate_activate_cb(void *ptr, int nr, int display_limit)
{
    InputContext *ic = (InputContext *)ptr;
    ic->candidate_activate(nr, display_limit);
}

#if UIM_XIM_USE_DELAY
void InputContext::candidate_activate_with_delay_cb(void *ptr, int delay)
{
    InputContext *ic = (InputContext *)ptr;
    ic->candidate_activate_with_delay(delay);
}

void InputContext::candidate_activate_timeout_cb(void *ptr)
{
    InputContext *ic = (InputContext *)ptr;
    ic->candidate_activate_timeout();
}
#endif

void InputContext::candidate_select_cb(void *ptr, int index)
{
    InputContext *ic = (InputContext *)ptr;
    ic->set_need_hilite_selected_cand(true);
    ic->candidate_select(index);
}

void InputContext::candidate_shift_page_cb(void *ptr, int direction)
{
    InputContext *ic = (InputContext *)ptr;
    ic->candidate_shift_page(direction);
}

void InputContext::candidate_deactivate_cb(void *ptr)
{
    InputContext *ic = (InputContext *)ptr;
    ic->candidate_deactivate();
}

void InputContext::update_prop_list_cb(void *ptr, const char *str)
{
    InputContext *ic = (InputContext *)ptr;
    InputContext *focusedContext = InputContext::focusedContext();
    if (ic == focusedContext)
      ic->update_prop_list(str);
}

void InputContext::update_prop_label_cb(void *ptr, const char *str)
{
    InputContext *ic = (InputContext *)ptr;
    InputContext *focusedContext = InputContext::focusedContext();
    if (ic == focusedContext)
      ic->update_prop_label(str);
}

void InputContext::configuration_changed_cb(void *ptr)
{
    InputContext *ic = (InputContext *)ptr;

    ic->configuration_changed();
}

void InputContext::switch_app_global_im_cb(void *ptr, const char *name)
{
    InputContext *ic = (InputContext *)ptr;

    ic->switch_app_global_im(name);
}

void InputContext::switch_system_global_im_cb(void *ptr, const char *name)
{
    InputContext *ic = (InputContext *)ptr;

    ic->switch_system_global_im(name);
}

void InputContext::clear_pe_stat()
{
    m_pe->clear();
}

void InputContext::clear_preedit()
{
    clear_pe_stat();
    if (mConvdisp)
	mConvdisp->clear_preedit();
}

uString InputContext::get_preedit_string()
{
    uString str;

    if (mConvdisp)
	str = mConvdisp->get_pe();
    return str;
}

void InputContext::pushback_preedit_string(int attr, const char *str)
{
    if (str == NULL) {
	fprintf(stderr, "Warning: str is NULL in pushback_cb\n");
	return;
    }
    // Need to check caret pos at first.
    if (attr & UPreeditAttr_Cursor)
	m_pe->caret_pos = m_pe->get_char_count();
    if (!strlen(str))
	return;

    int p = 0;
    if (attr & UPreeditAttr_UnderLine)
	p |= PE_UNDERLINE;
    if (attr & UPreeditAttr_Reverse)
	p |= PE_REVERSE;
    m_pe->new_segment(p);
    uString js;
    mServer->strToUstring(&js, str);

    uString::iterator it;
    for (it = js.begin(); it != js.end(); ++it) {
	m_pe->push_uchar(*it);
    }
}

void InputContext::update_preedit()
{
    if (mConvdisp)
	mConvdisp->update_preedit();
}

int InputContext::pushKey(keyState *k)
{
    int key = k->key();
    int rv = 1;

    if (key != UKey_Other) {
	if (k->is_push()) {
	    rv = uim_press_key(mUc, key, k->modifier());
	    if (!(g_option_mask & OPT_ON_DEMAND_SYNC)) {
		// Call uim_release_key here since we don't filter key
		// release event with full-synchronous-method for now.
		uim_release_key(mUc, key, k->modifier());
	    }
	}
	else
	    rv = uim_release_key(mUc, key, k->modifier());
    }

    if (rv) {
	if (k->check_compose())
	    return UPDATE_MODE;
	else
	    return COMMIT_RAW;
    } else {
	return UPDATE_MODE;
    }
}

bool InputContext::hasActiveCandwin()
{
    return mCandwinActive;
}

// reset
void InputContext::clear()
{
    clear_preedit();
    candidate_deactivate();
    uim_reset_context(mUc);
}

void InputContext::setConvdisp(Convdisp *c)
{
    mConvdisp = c;
    if (mConvdisp)
	mConvdisp->set_pe(m_pe);
}

void InputContext::commit_string(char *s)
{
    mXic->commit_string(s);
}

void InputContext::extra_input(char *s)
{
    mXic->extra_input(s);
}

XimIC *InputContext::get_ic()
{
    return mXic;
}

void InputContext::candidate_activate(int nr, int display_limit)
{
    int i;
#if !UIM_XIM_USE_NEW_PAGE_HANDLING
    const char *cand_str;
    const char *heading_label;
    const char *annotation_str;
    char *str;
#else
    std::vector<CandList>::iterator slot_it;
#endif
    std::vector<const char *> candidates;
    std::vector<const char *>::iterator it;

#if UIM_XIM_USE_DELAY
    timer_cancel();
#endif
    Canddisp *disp = canddisp_singleton();

    mDisplayLimit = display_limit;
    if (display_limit)
	mNumPage = (nr - 1) / display_limit + 1;
#if !UIM_XIM_USE_NEW_PAGE_HANDLING
    /* remove old data */
    if (!active_candidates.empty()) {
	for (it = active_candidates.begin();
	     it != active_candidates.end();
	     ++it)
	    free((char *)*it);
    }
    active_candidates.clear();
    for (i = 0; i < nr; i++) {
	uim_candidate cand;
	cand = uim_get_candidate(mUc, i,
			display_limit ? i % display_limit : i);
	cand_str = uim_candidate_get_cand_str(cand);
	heading_label = uim_candidate_get_heading_label(cand);
	annotation_str = uim_candidate_get_annotation(cand);
	if (cand_str && heading_label && annotation_str) {
	    str = (char *)malloc(strlen(cand_str) + strlen(heading_label) + strlen(annotation_str) + 3);
	    sprintf(str, "%s\a%s\a%s", heading_label, cand_str, annotation_str);
	    candidates.push_back((const char *)str);
	}
	else {
	    fprintf(stderr, "Warning: cand_str at %d is NULL\n", i);
	    candidates.push_back((const char *)strdup("\a\a"));
	}
	uim_candidate_free(cand);
    }
    disp->activate(candidates, display_limit);
    active_candidates = candidates;
#else /* !UIM_XIM_USE_NEW_PAGE_HANDLING */
    mNumCandidates = nr;
    /* remove old data */
    for (slot_it = mCandidateSlot.begin();
	 slot_it != mCandidateSlot.end();
	 ++slot_it) {
	if (*slot_it != (CandList)0) {
	    for (it = (*slot_it).begin(); it != (*slot_it).end(); ++it)
		free((char *)*it);
	}
    }
    mCandidateSlot.clear();

    /* setup dummy data */
    for (i = 0; i < mNumPage; i++)
    	mCandidateSlot.push_back((CandList)0);

    prepare_page_candidates(0);
    disp->set_nr_candidates(nr, display_limit);
    disp->set_page_candidates(0, mCandidateSlot[0]);
    disp->show_page(0);
#endif /* !UIM_XIM_USE_NEW_PAGE_HANDLING */
    mCandwinActive = true;

    current_cand_selection = 0;
    current_page = 0;
    need_hilite_selected_cand = false;
}

#if UIM_XIM_USE_DELAY
void InputContext::candidate_activate_with_delay(int delay)
{
    timer_cancel();
    if (delay > 0) {
	timer_set(delay, InputContext::candidate_activate_timeout_cb, this);
    } else {
	candidate_activate_timeout();
    }
}

void InputContext::candidate_activate_timeout()
{
    int nr = -1, display_limit = -1, selected_index = -1;
    uim_delay_activating(mUc, &nr, &display_limit, &selected_index);
    if (nr > 0) {
	candidate_activate(nr, display_limit);
	if (selected_index >= 0) {
	    candidate_select(selected_index);
	}
    }
}
#endif

void InputContext::candidate_update()
{
    Canddisp *disp = canddisp_singleton();

#if !UIM_XIM_USE_NEW_PAGE_HANDLING
    disp->activate(active_candidates, mDisplayLimit);
#else
    prepare_page_candidates(current_page);
    disp->set_nr_candidates(mNumCandidates, mDisplayLimit);
    disp->set_page_candidates(current_page, mCandidateSlot[current_page]);
    disp->show_page(current_page);
#endif
    disp->select(current_cand_selection, need_hilite_selected_cand);
    disp->show();
}

#if UIM_XIM_USE_NEW_PAGE_HANDLING
void InputContext::prepare_page_candidates(int page)
{
    int i;
    int page_nr, start;
    const char *cand_str;
    const char *heading_label;
    const char *annotation_str;
    char *str;
    CandList candidates;

    if (page < 0)
	return;

    if (mCandidateSlot[page] != (CandList)0)
	return;

    start = page * mDisplayLimit;
    if (mDisplayLimit && (mNumCandidates - start) > mDisplayLimit)
	page_nr = mDisplayLimit;
    else
	page_nr = mNumCandidates - start;

    for (i = 0; i < page_nr; i++) {
	uim_candidate cand;
	cand = uim_get_candidate(mUc, (i + start),
			mDisplayLimit ? (i + start) % mDisplayLimit :
					(i + start));
	cand_str = uim_candidate_get_cand_str(cand);
	heading_label = uim_candidate_get_heading_label(cand);
	annotation_str = uim_candidate_get_annotation_str(cand);
	if (cand_str && heading_label && annotation_str) {
	    str = (char *)malloc(strlen(cand_str) + strlen(heading_label) + strlen(annotation_str) + 3);
	    sprintf(str, "%s\a%s\a%s", heading_label, cand_str, annotation_str);
	    candidates.push_back((const char *)str);
	}
	else {
	    fprintf(stderr, "Warning: cand_str at %d is NULL\n", i);
	    candidates.push_back((const char *)strdup("\a\a"));
	}
	uim_candidate_free(cand);
    }

    mCandidateSlot[page] = candidates;
}

int InputContext::prepare_page_candidates_by_index(int index)
{
    int page;

    page = mDisplayLimit ? index / mDisplayLimit : 0;
    prepare_page_candidates(page);

    return page;
}
#endif

void InputContext::candidate_select(int index)
{
    Canddisp *disp = canddisp_singleton();

#if UIM_XIM_USE_NEW_PAGE_HANDLING
    int new_page = prepare_page_candidates_by_index(index);

    if (new_page < 0)
	return;	// shouldn't happen

    if (current_page != new_page)
	disp->set_page_candidates(new_page, mCandidateSlot[new_page]);
#endif
    disp->select(index, need_hilite_selected_cand);
    current_cand_selection = index;
    if (mDisplayLimit)
	current_page = current_cand_selection / mDisplayLimit;
}

void InputContext::candidate_shift_page(int direction)
{
    int new_page;
    int new_index;

    if (mDisplayLimit) {
	if (direction)
	    new_page = current_page + 1;
	else
	    new_page = current_page - 1;

	if (new_page < 0)
	    current_page = mNumPage - 1;
	else if (new_page >= mNumPage)
	    current_page = 0;
	else
	    current_page = new_page;

	new_index = (current_page * mDisplayLimit) + (current_cand_selection % mDisplayLimit);

#if !UIM_XIM_USE_NEW_PAGE_HANDLING
	if (new_index >= active_candidates.size())
	    current_cand_selection = active_candidates.size() - 1;
#else
	if (new_index >= mNumCandidates)
	    current_cand_selection = mNumCandidates - 1;
#endif
	else
	    current_cand_selection = new_index;
#if UIM_XIM_USE_NEW_PAGE_HANDLING
    	Canddisp *disp = canddisp_singleton();
	prepare_page_candidates(current_page);
	disp->set_page_candidates(current_page, mCandidateSlot[current_page]);
#endif
    }
    candidate_select(current_cand_selection);
    if (need_hilite_selected_cand)
      uim_set_candidate_index(mUc, current_cand_selection);
}

void InputContext::candidate_deactivate()
{
#if UIM_XIM_USE_DELAY
    timer_cancel();
#endif
    if (mCandwinActive) {
	std::vector<const char *>::iterator i;
	Canddisp *disp = canddisp_singleton();

	disp->deactivate();
#if !UIM_XIM_USE_NEW_PAGE_HANDLING
	for (i = active_candidates.begin(); i != active_candidates.end(); ++i) {
	    free((char *)*i);
	}
	active_candidates.clear();
#else
	int j;
	for (j = 0; j < mNumPage; j++) {
	    if ((CandList)mCandidateSlot[j] != (CandList)0) {
		for (i = mCandidateSlot[j].begin();
		     i != mCandidateSlot[j].end();
		     ++i) {
		    free((char *)*i);
		}
	    }
	}
	mCandidateSlot.clear();
#endif
	mCandwinActive = false;
	current_cand_selection = 0;
    }
}

void InputContext::set_need_hilite_selected_cand(bool set)
{
    need_hilite_selected_cand = set;
}

char *InputContext::get_caret_state_label_from_prop_list(const char *str)
{
    const char *p, *q;
    char *state_label = NULL;
    char label[10];
    int len, state_label_len = 0;

    p = str;
    while ((p = strstr(p, "branch\t"))) {
	p = strchr(p + 7, '\t');
	if (p) {
	    p++;
	    q = strchr(p, '\t');
	    len = static_cast<int>(q - p);
	    if (q && len < 10) {
		strlcpy(label, p, len + 1);
		if (!state_label) {
		    state_label_len = len;
		    state_label = strdup(label);
		} else {
		    state_label_len += (len + 1);
		    state_label = (char *)realloc(state_label,
						      state_label_len + 1);
		    if (state_label) {
			strcat(state_label, "\t");
			strcat(state_label, label);
			state_label[state_label_len] = '\0';
		    }
		}
	    }
	}
    }

    return state_label;
}

void InputContext::update_prop_list(const char *str)
{
    char *buf;

    if (asprintf(&buf, "prop_list_update\ncharset=UTF-8\n%s", str) == -1) {
        free(buf);
        return;
    }
    uim_helper_send_message(lib_uim_fd, buf);
    free(buf);

#if 1
    // Show caret state indicator with this function instead of
    // InputContext::update_prop_label() to workaround the label
    // mismatch during IM switch caused from context-update-widgets.
    uim_bool show_caret_state =
	uim_scm_symbol_value_bool("bridge-show-input-state?");
    char *show_caret_with =
	uim_scm_c_symbol(uim_scm_symbol_value("bridge-show-with?"));
    uim_bool show_caret_mode = (strcmp(show_caret_with, "mode") == 0);
    uim_bool show_caret_mode_on = uim_scm_symbol_value_bool("bridge-show-input-state-mode-on?");

    if (show_caret_state == UIM_TRUE && !(show_caret_mode && !show_caret_mode_on)) {
	char *label;
	int timeout;
	Canddisp *disp = canddisp_singleton();

	if (strcmp(show_caret_with, "time") == 0)
	    timeout = static_cast<int>(uim_scm_symbol_value_int(
				    "bridge-show-input-state-time-length"));
	else
	    timeout = 0;

	label = get_caret_state_label_from_prop_list(str);
	disp->show_caret_state(label, timeout);
	free(label);
	mCaretStateShown = true;
    } else if (show_caret_mode && !show_caret_mode_on) {
	Canddisp *disp = canddisp_singleton();
	disp->hide_caret_state();
    }
    free(show_caret_with);
#endif
}

void InputContext::update_prop_label(const char *str)
{
    char *buf;

    if (asprintf(&buf, "prop_label_update\ncharset=UTF-8\n%s", str) == -1) {
        free(buf);
        return;
    }
    uim_helper_send_message(lib_uim_fd, buf);
    free(buf);
#if 0    
    uim_bool show_caret_state = uim_scm_symbol_value_bool("bridge-show-input-state?");
    if (show_caret_state == UIM_TRUE) {
	int timeout = uim_scm_symbol_value_int("bridge-show-input-state-time-length");
	Canddisp *disp = canddisp_singleton();
	disp->show_caret_state(str, timeout);
	mCaretStateShown = true;
    }
#endif
}

const char *InputContext::get_engine_name()
{
    return mEngineName;
}

const char *InputContext::get_locale_name()
{
    return mLocaleName;
}

bool InputContext::isCaretStateShown()
{
    return mCaretStateShown;
}

keyState::keyState(XimIC *ic)
{
    XimIM *im;
    DefTree *top;

    mModState = 0;
    mIc = ic;

    im = get_im_by_id(mIc->get_imid());
    top = im->get_compose_tree();

    mCompose = new Compose(top, mIc);
}

keyState::~keyState()
{
    delete mCompose;
}

void keyState::check_key(keyEventX *x)
{
    mModifier = 0;
    mXKeySym = x->key_sym;
    mXKeyState = x->state;

    mPreModState = mModState;

    if (x->press) {
	m_bPush = true;

	if (!(x->state) || x->state == LockMask || x->state == gXNumLockMask)
	    mModState = mPreModState = 0;

	mPreModState = mModState;
	switch (x->key_sym) {
	case XK_Alt_L:
	case XK_Alt_R:
	    mModState |= UMod_Alt;
	    break;
	case XK_Meta_L:
	case XK_Meta_R:
	    mModState |= UMod_Meta;
	    break;
	case XK_Super_L:
	case XK_Super_R:
	    mModState |= UMod_Super;
	    break;
	case XK_Hyper_L:
	case XK_Hyper_R:
	    mModState |= UMod_Hyper;
	    break;
	default:
	    break;
	}
    } else {
	m_bPush = false;

	switch (x->key_sym) {
	case XK_Alt_L:
	case XK_Alt_R:
	    mModState &= ~UMod_Alt;
	    break;
	case XK_Meta_L:
	case XK_Meta_R:
	    mModState &= ~UMod_Meta;
	    break;
	case XK_Super_L:
	case XK_Super_R:
	    mModState &= ~UMod_Super;
	    break;
	case XK_Hyper_L:
	case XK_Hyper_R:
	    mModState &= ~UMod_Hyper;
	    break;
	default:
	    break;
	}
    }

    if (x->state & ShiftMask)
	mModifier |= UMod_Shift;
    if (x->state & ControlMask)
	mModifier |= UMod_Control;
    if (x->state & Mod1Mask)
	mModifier |= (gMod1Mask & mPreModState);
    if (x->state & Mod2Mask)
	mModifier |= (gMod2Mask & mPreModState);
    if (x->state & Mod3Mask)
	mModifier |= (gMod3Mask & mPreModState);
    if (x->state & Mod4Mask)
	mModifier |= (gMod4Mask & mPreModState);
    if (x->state & Mod5Mask)
	mModifier |= (gMod5Mask & mPreModState);

    mKey = uim_x_keysym2ukey(x->key_sym);

#if UIM_XIM_USE_JAPANESE_KANA_KEYBOARD_HACK
    mKey = uim_x_kana_input_hack_translate_key(mKey,
					       (KeyCode)x->ev.xkey.keycode);
#endif
}

bool keyState::check_compose()
{
    return mCompose->handleKey(mXKeySym, mXKeyState, m_bPush);
}

int keyState::key()
{
    return mKey;
}

int keyState::modifier()
{
    return mModifier;
}

bool keyState::is_push()
{
    return m_bPush;
}

KeySym keyState::xkeysym()
{
    return mXKeySym;
}

int keyState::xkeystate()
{
    return mXKeyState;
}

void keyState::reset()
{
    mModState = 0;
    mCompose->reset();
}

void keyState::print()
{
    printf("key code=%x,modifier=%d.\n",
	   mKey, mModifier);
}

static int check_modifier(std::list<KeySym> keysym_list)
{
    int ret = 0;
    std::list<KeySym>::iterator i;
    for (i = keysym_list.begin(); i != keysym_list.end(); ++i) {
	switch (*i) {
	case XK_Alt_L:
	case XK_Alt_R:
	    ret |= UMod_Alt;
	    break;
	case XK_Meta_L:
	case XK_Meta_R:
	    ret |= UMod_Meta;
	    break;
	case XK_Super_L:
	case XK_Super_R:
	    ret |= UMod_Super;
	    break;
	case XK_Hyper_L:
	case XK_Hyper_R:
	    ret |= UMod_Hyper;
	    break;
	default:
	    break;
	}
    }
    return ret;
}

void init_modifier_keys() {
    int i, k = 0;
    int min_keycode, max_keycode, keysyms_per_keycode = 0;

    std::list<KeySym> Mod1MaskSyms, Mod2MaskSyms, Mod3MaskSyms,
		      Mod4MaskSyms, Mod5MaskSyms;

    gXNumLockMask = 0;
    XModifierKeymap *map = XGetModifierMapping(XimServer::gDpy);
    XDisplayKeycodes(XimServer::gDpy, &min_keycode, &max_keycode);
    KeySym *sym = XGetKeyboardMapping(XimServer::gDpy,
                    static_cast<KeyCode>(min_keycode),
                    (max_keycode - min_keycode + 1), &keysyms_per_keycode);
    for (i = 0; i < 8; i++) {
	int j;
	for (j = 0; j < map->max_keypermod; j++) {
	    if (map->modifiermap[k]) {
		KeySym ks;
		int index = 0;
		do {
		    ks = XkbKeycodeToKeysym(XimServer::gDpy,
				    map->modifiermap[k], 0, index);
		    index++;
		} while (!ks && index < keysyms_per_keycode);

		switch (i) {
		case ShiftMapIndex: break;
		case LockMapIndex: break;
		case ControlMapIndex: break;
		case Mod1MapIndex: Mod1MaskSyms.push_back(ks); break;
		case Mod2MapIndex: Mod2MaskSyms.push_back(ks); break;
		case Mod3MapIndex: Mod3MaskSyms.push_back(ks); break;
		case Mod4MapIndex: Mod4MaskSyms.push_back(ks); break;
		case Mod5MapIndex: Mod5MaskSyms.push_back(ks); break;
		default: break;
		}
		// Check NumLock key
		if (ks == XK_Num_Lock)
		    gXNumLockMask |= (1 << i);
	    }
	    k++;
	}
    }
    XFreeModifiermap(map);
    XFree(sym);

    gMod1Mask = check_modifier(Mod1MaskSyms);
    gMod2Mask = check_modifier(Mod2MaskSyms);
    gMod3Mask = check_modifier(Mod3MaskSyms);
    gMod4Mask = check_modifier(Mod4MaskSyms);
    gMod5Mask = check_modifier(Mod5MaskSyms);

    if (uim_scm_c_bool(uim_scm_callf("require-dynlib", "s", "xkb")))
	uim_scm_callf("%xkb-set-display", "p", XimServer::gDpy);

#if UIM_XIM_USE_JAPANESE_KANA_KEYBOARD_HACK
    // Init at here to sync with proper update timing although not a modifier.
    uim_x_kana_input_hack_init(XimServer::gDpy);
#endif
}

void
check_candwin_style()
{
    char *style = uim_scm_symbol_value_str("candidate-window-style");
    CandWinStyle PrevStyle = XimServer::gCandWinStyle;

    if (style && !strcmp(style, "table"))
	XimServer::gCandWinStyle = Table;
    else if (style && !strcmp(style, "horizontal"))
	XimServer::gCandWinStyle = Horizontal;
    else
	XimServer::gCandWinStyle = Vertical;

    XimServer::gCandWinStyleUpdated =
	    (PrevStyle != XimServer::gCandWinStyle) ? true : false;
    free(style);
}

void
check_candwin_pos_type()
{
    char *candwin_pos_type = uim_scm_symbol_value_str("candidate-window-position");

    if (candwin_pos_type && !strcmp(candwin_pos_type, "left"))
	XimServer::gCandWinPosType = Left;
    else if (candwin_pos_type && !strcmp(candwin_pos_type, "right"))
	XimServer::gCandWinPosType = Right;
    else
	XimServer::gCandWinPosType = Caret;

    free(candwin_pos_type);
}

/*
 * Local variables:
 *  c-indent-level: 4
 *  c-basic-offset: 4
 * End:
 */
