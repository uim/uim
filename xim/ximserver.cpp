/*

  Copyright (c) 2003,2004 uim Project http://uim.freedesktop.org/

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

#include <stdio.h>
#include <ctype.h>
#include <locale.h>
#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysymdef.h>

#include "xim.h"
#include "convdisp.h"
#include "canddisp.h"
#include "ximserver.h"
#include "util.h"
#include "helper.h"

#include "uim/uim-helper.h"

#ifndef __GNUC__
# ifdef HAVE_ALLOCA_H
#  include <alloca.h>
# endif
#endif

extern int lib_uim_fd;
extern Atom xim_servers;
InputContext *InputContext::mFocusedContext = NULL;

static int check_modifier(std::list<KeySym> list);
static int gShiftMask, gLockMask, gControlMask, gMod1Mask,
	   gMod2Mask, gMod3Mask, gMod4Mask, gMod5Mask;
static int gXNumLockMask;


// tables
static input_style input_style_tab_with_over_the_spot[] = {
    {XIMPreeditNothing|XIMStatusNothing, IS_ROOT_WINDOW},
    //{XIMPreeditPosition|XIMStatusArea, IS_OVER_THE_SPOT},// emacs
    {XIMPreeditPosition|XIMStatusNothing, IS_OVER_THE_SPOT},
    //{XIMPreeditCallbacks|XIMStatusCallbacks, IS_ON_THE_SPOT},// OOo
    //{XIMPreeditArea|XIMStatusArea, IS_ROOT_WINDOW},
    {XIMPreeditCallbacks|XIMStatusNothing, IS_ON_THE_SPOT},
    {0, 0},
};
static input_style input_style_tab_without_over_the_spot[] = {
    {XIMPreeditNothing|XIMStatusNothing, IS_ROOT_WINDOW},
    //{XIMPreeditPosition|XIMStatusArea, IS_OVER_THE_SPOT},// emacs
    //{XIMPreeditPosition|XIMStatusNothing, IS_OVER_THE_SPOT},
    //{XIMPreeditCallbacks|XIMStatusCallbacks, IS_ON_THE_SPOT},// OOo
    //{XIMPreeditArea|XIMStatusArea, IS_ROOT_WINDOW},
    {XIMPreeditCallbacks|XIMStatusNothing, IS_ON_THE_SPOT},
    {0, 0},
};
// XIMPreeditArea,XIMPreeditCallbacks,XIMPreeditPosition
// XIMPreeditNothing,XIMPreeditNone
// XIMStatusArea,XIMStatusCallbacks
// XIMStatusNothing,XIMStatusNone

void print_ustring(uString *s)
{
    uString::iterator i;
    printf("length=%d : ", s->size());
    uchar ch;
    char utf8[6];
    int nbyte;
    for (i = s->begin(); i != s->end(); i++) {
	ch = *i;
	nbyte = utf8_wctomb((unsigned char *)utf8, ch);
	utf8[nbyte] = '\0';
	printf(utf8);
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
    for (i = s->begin(); i !=s->end(); i++) {
	d->push_back(*i);
    }
}

XimServer::XimServer(Locale *lc, const char *name, const char *lang)
{
    mIMName = strdup(name);
    mIMLang = lang;
    mLocale = lc;
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
    for (it = ic_list.begin(); it != ic_list.end(); it++) {
	if (*it == ic) {
	    ic_list.erase(it);
	    break;
	}
    }
}

void XimServer::changeContext(const char *engine) {
    set_im(engine);
    std::list<InputContext *>::iterator it;
    for (it = ic_list.begin(); it != ic_list.end(); it++) {
	(*it)->changeContext(engine);
    }
    // make sure to update locale of focused context
    InputContext *focusedContext = InputContext::focusedContext();
    if (focusedContext)
	focusedContext->focusIn();
}

struct input_style *
XimServer::getInputStyles()
{
    if (mLocale->supportOverTheSpot())
	return input_style_tab_with_over_the_spot;

    return input_style_tab_without_over_the_spot;
}

bool
XimServer::setupConnection(bool useDefaultIM)
{
    char *buf;
    if (!useDefaultIM) {
	buf = (char *)alloca(15 + strlen(mIMName));
	sprintf(buf, "@server=uim-%s", mIMName);
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
		       &nr_prop, &nr_bytes, (unsigned char **)&prop);
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

char *
XimServer::uStringToCtext(uString *us, const char *encoding)
{
    return mLocale->uStringToCtext(us, encoding);
}

void
XimServer::strToUstring(uString *d, const char *s)
{
    int len;
    int l = 0, nbyte = 0;
    uchar ch;

    len = strlen(s);
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

Locale *XimServer::getLocale()
{
    return mLocale;
}

void XimServer::set_im(const char *engine)
{
    if (mIMName)
	free(mIMName);

    mIMName = strdup(engine);
    mIMLang = get_im_lang_from_engine(engine);
    mLocale->set_localename_from_im_lang(mIMLang);
}

const char *get_im_lang_from_engine(const char *engine)
{
    std::list<UIMInfo>::iterator it;
    for (it = uim_info.begin(); it != uim_info.end(); it++) {
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
    mConvdisp = 0;
    mServer = svr;
    mEngineName = NULL;
    mLocaleName = NULL;
    mFocusedContext = this;
    mUc = createUimContext(engine);
    mCandwinActive = false;
    mNumPage = 1;
    mDisplayLimit = 0;
}

InputContext::~InputContext()
{
    if (mFocusedContext == this)
	mFocusedContext = NULL;

    if (mConvdisp)
	mConvdisp->set_pe(0);

    delete m_pe;
    uim_release_context(mUc);
    mServer->deleteContext(this);
    free(mEngineName);
    free(mLocaleName);
}

uim_context
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

    if (mLocaleName)
	free(mLocaleName);
    mLocaleName = locale;

    if (mEngineName)
	free(mEngineName);
    mEngineName = strdup(real_im);

    uim_context uc = uim_create_context((void *) this, "UTF-8",
					NULL, real_im, uim_iconv,
					InputContext::commit_cb);

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
    uim_set_prop_label_update_cb(uc,
				 InputContext::update_prop_label_cb);

    if (mFocusedContext == this)
	uim_prop_list_update(uc);

    return uc;
}

void
InputContext::changeContext(const char *engine)
{
    const char *encoding = mXic->get_encoding();
    const char *im_lang = get_im_lang_from_engine(engine);

    // Don't change im unless encoding matches for clients with legacy locales.
    if (strcmp(encoding, "UTF-8")) {
	const char *client_locale = mXic->get_lang_region();
	const char *engine_locales = compose_localenames_from_im_lang(im_lang);

	if (!is_locale_included(engine_locales, client_locale))
	    return;
    }

    clear();
    uim_release_context(mUc);
    mUc = createUimContext(engine); // mEngineName and locale will be set here.
    if (mConvdisp) {
	mConvdisp->set_im_lang(get_im_lang_from_engine(mEngineName));
	mConvdisp->set_locale_name(mLocaleName);
    }
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
    uim_prop_list_update(mUc);	
    uim_prop_label_update(mUc);	
    uim_helper_client_focus_in(mUc);
    mFocusedContext = this;
}

void
InputContext::focusOut()
{
    uim_helper_client_focus_out(mUc);
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

    clear_cb(ptr);
    ic->update_preedit();
    xic->commit_string(str);
}

void InputContext::clear_cb(void *ptr)
{
    InputContext *ic = (InputContext *)ptr;
    ic->clear_preedit();
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

void InputContext::candidate_select_cb(void *ptr, int index)
{
    InputContext *ic = (InputContext *)ptr;
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
    ic->update_prop_list(str);
}

void InputContext::update_prop_label_cb(void *ptr, const char *str)
{
    InputContext *ic = (InputContext *)ptr;
    ic->update_prop_label(str);
}

void InputContext::clear_preedit()
{
    m_pe->clear();
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
    for (it = js.begin(); it != js.end(); it++) {
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
	if (k->is_push())
	    rv = uim_press_key(mUc, key, k->modifier());
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

void InputContext::clear()
{
    clear_preedit();
    if (mConvdisp)
	mConvdisp->clear_preedit();
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

bool InputContext::extra_input(char *s)
{
    return false;
}

XimIC *InputContext::get_ic()
{
    return mXic;
}

void InputContext::candidate_activate(int nr, int display_limit)
{
    int i;
    const char *cand_str;
    uim_candidate cand[nr];
    std::vector<const char *> candidates;

    Canddisp *disp = canddisp_singleton();

    for (i = 0; i < nr; i++) {
	cand[i] = uim_get_candidate(mUc, i, i % display_limit);
	cand_str = uim_candidate_get_cand_str(cand[i]);
	if (cand_str)
	    candidates.push_back((const char *)strdup(cand_str));
	else {
	    fprintf(stderr, "Warning: cand_str at %d is NULL\n", i);
	    candidates.push_back((const char *)strdup(""));
	}
    }
    disp->activate(candidates, display_limit);
    for (i = 0; i < nr; i++) {
	uim_candidate_free(cand[i]);
    }
    mCandwinActive = true;
    active_candidates = candidates;
    mDisplayLimit = display_limit;
    if (display_limit)
	mNumPage = nr / display_limit + 1;
}

void InputContext::candidate_update()
{
    Canddisp *disp = canddisp_singleton();

    disp->activate(active_candidates, mDisplayLimit);
    disp->select(current_cand_selection);
}

void InputContext::candidate_select(int index)
{
    Canddisp *disp = canddisp_singleton();
    disp->select(index);
    current_cand_selection = index;
    if (mDisplayLimit)
	current_page = current_cand_selection / mDisplayLimit;
}

void InputContext::candidate_shift_page(int direction)
{
    int new_page;
    uint new_index;

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

	if (new_index >= active_candidates.size())
	    current_cand_selection = active_candidates.size() - 1;
	else
	    current_cand_selection = new_index;
    }
    candidate_select(current_cand_selection);
    uim_set_candidate_index(mUc, current_cand_selection);
}

void InputContext::candidate_deactivate()
{
    if (mCandwinActive) {
	std::vector<const char *>::iterator i;
	Canddisp *disp = canddisp_singleton();

	disp->deactivate();
	for (i = active_candidates.begin(); i != active_candidates.end(); i++) {
	    free((char *)*i);
	}
	mCandwinActive = false;
	current_cand_selection = 0;
    }
}

void InputContext::update_prop_list(const char *str)
{
    char *buf;

    asprintf(&buf, "prop_list_update\ncharset=UTF-8\n%s", str);
    if (!buf)
	return;
    uim_helper_send_message(lib_uim_fd, buf);
    free(buf);
}

void InputContext::update_prop_label(const char *str)
{
    char *buf;

    asprintf(&buf, "prop_label_update\ncharset=UTF-8\n%s", str);
    if (!buf)
	return;
    uim_helper_send_message(lib_uim_fd, buf);
    free(buf);
}

const char *InputContext::get_engine_name()
{
    return mEngineName;
}

const char *InputContext::get_locale_name()
{
    return mLocaleName;
}

keyState::keyState(XimIC *ic)
{
    XimIM *im;
    DefTree *top;

    mAltOn = false;
    mMetaOn = false;
    mSuperOn = false;
    mHyperOn = false;
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

    if (x->press) {
	m_bPush = true;

	if (!(g_option_mask & OPT_ON_DEMAND_SYNC)) {
	    // Only KeyPress is forwarded with full-synchronous
	    // method.  So reset modifiers here.
	    if (!(x->state) || x->state == LockMask || x->state == gXNumLockMask)
		mAltOn = mMetaOn = mSuperOn = mHyperOn = false;
	}

	switch (x->key_sym) {
	case XK_Alt_L:
	case XK_Alt_R:
	    mAltOn = true;
	    break;
	case XK_Meta_L:
	case XK_Meta_R:
	    mMetaOn = true;
	    break;
	case XK_Super_L:
	case XK_Super_R:
	    mSuperOn = true;
	    break;
	case XK_Hyper_L:
	case XK_Hyper_R:
	    mHyperOn = true;
	    break;
	default:
	    break;
	}
    } else {
	m_bPush = false;

	switch (x->key_sym) {
	case XK_Alt_L:
	case XK_Alt_R:
	    mAltOn = false;
	    break;
	case XK_Meta_L:
	case XK_Meta_R:
	    mMetaOn = false;
	    break;
	case XK_Super_L:
	case XK_Super_R:
	    mSuperOn = false;
	    break;
	case XK_Hyper_L:
	case XK_Hyper_R:
	    mHyperOn = false;
	    break;
	default:
	    break;
	}
    }

    if (x->state & ShiftMask)
	mModifier |= gShiftMask;
    if (x->state & LockMask)
	mModifier |= gLockMask;
    if (x->state & ControlMask)
	mModifier |= gControlMask;
    if (x->state & Mod1Mask)
	mModifier |= revise_mod(gMod1Mask);
    if (x->state & Mod2Mask)
	mModifier |= revise_mod(gMod2Mask);
    if (x->state & Mod3Mask)
	mModifier |= revise_mod(gMod3Mask);
    if (x->state & Mod4Mask)
	mModifier |= revise_mod(gMod4Mask);
    if (x->state & Mod5Mask)
	mModifier |= revise_mod(gMod5Mask);

    if (x->key_sym < 128 && x->key_sym >= 32)
	mKey = x->key_sym;
    else if (x->key_sym >= XK_F1 && x->key_sym <= XK_F35)
	mKey = x->key_sym - XK_F1 + UKey_F1;
    else if (x->key_sym >= 0xfe50 && x->key_sym <= 0xfe62)
	mKey = UKey_Other; // Don't forward deadkeys to libuim.
    else {
	switch (x->key_sym) {
	case XK_BackSpace: mKey = UKey_Backspace; break;
	case XK_Delete: mKey = UKey_Delete; break;
	case XK_Escape: mKey = UKey_Escape; break;
	case XK_Tab: mKey = UKey_Tab; break;
	case XK_Return: mKey = UKey_Return; break;
	case XK_Left: mKey = UKey_Left; break;
	case XK_Up: mKey = UKey_Up; break;
	case XK_Right: mKey = UKey_Right; break;
	case XK_Down: mKey = UKey_Down; break;
	case XK_Prior: mKey = UKey_Prior; break;
	case XK_Next: mKey = UKey_Next; break;
	case XK_Home: mKey = UKey_Home; break;
	case XK_End: mKey = UKey_End; break;
	case XK_Kanji:
	case XK_Zenkaku_Hankaku: mKey = UKey_Zenkaku_Hankaku; break;
	// Don't forward Multi_key to libuim.
	//case XK_Multi_key: mKey = UKey_Multi_key; break;
	case XK_Multi_key: mKey = UKey_Other; break;
	case XK_Mode_switch: mKey = UKey_Mode_switch; break;
	case XK_Henkan_Mode: mKey = UKey_Henkan_Mode; break;
	case XK_Muhenkan: mKey = UKey_Muhenkan; break;
	case XK_Shift_L: mKey = UKey_Shift_key; break;
	case XK_Shift_R: mKey = UKey_Shift_key; break;
	case XK_Control_L: mKey = UKey_Control_key; break;
	case XK_Control_R: mKey = UKey_Control_key; break;
	case XK_Alt_L: mKey = UKey_Alt_key; break;
	case XK_Alt_R: mKey = UKey_Alt_key; break;
	case XK_Meta_L: mKey = UKey_Meta_key; break;
	case XK_Meta_R: mKey = UKey_Meta_key; break;
	case XK_Super_L: mKey = UKey_Super_key; break;
	case XK_Super_R: mKey = UKey_Super_key; break;
	case XK_Hyper_L: mKey = UKey_Hyper_key; break;
	case XK_Hyper_R: mKey = UKey_Hyper_key; break;
	default:
	    mKey = UKey_Other;
	}
    }
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

int keyState::revise_mod(int uim_mod)
{
    if ((uim_mod & UMod_Alt) && (mAltOn == false))
	uim_mod &= ~UMod_Alt;
    if ((uim_mod & UMod_Meta) && (mMetaOn == false))
	uim_mod &= ~UMod_Meta;
    if ((uim_mod & UMod_Super) && (mSuperOn == false))
	uim_mod &= ~UMod_Super;
    if ((uim_mod & UMod_Hyper) && (mHyperOn == false))
	uim_mod &= ~UMod_Hyper;

    return uim_mod;
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
    mAltOn = mMetaOn = mHyperOn = mSuperOn = false;
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
    for (i = keysym_list.begin(); i != keysym_list.end(); i++) {
	switch (*i) {
	case XK_Shift_L:
	case XK_Shift_R:
	    ret |= UMod_Shift;
	    break;
	case XK_Control_L:
	case XK_Control_R:
	    ret |= UMod_Control;
	    break;
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

    std::list<KeySym> ShiftMaskSyms, LockMaskSyms, ControlMaskSyms,
		      Mod1MaskSyms, Mod2MaskSyms, Mod3MaskSyms,
		      Mod4MaskSyms, Mod5MaskSyms;

    XModifierKeymap *map = XGetModifierMapping(XimServer::gDpy);
    XDisplayKeycodes(XimServer::gDpy, &min_keycode, &max_keycode);
    KeySym *sym = XGetKeyboardMapping(XimServer::gDpy, min_keycode,
		    (max_keycode - min_keycode + 1), &keysyms_per_keycode);
    for (i = 0; i < 8; i++) {
	int j;
	for (j = 0; j < map->max_keypermod; j++) {
	    if (map->modifiermap[k]) {
		KeySym ks;
		int index = 0;
		do {
		    ks = XKeycodeToKeysym(XimServer::gDpy,
				    map->modifiermap[k], index);
		    index++;
		} while (!ks && index < keysyms_per_keycode);

		switch (i) {
		case ShiftMapIndex: ShiftMaskSyms.push_back(ks); break;
		case LockMapIndex: LockMaskSyms.push_back(ks); break;
		case ControlMapIndex: ControlMaskSyms.push_back(ks); break;
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

    gShiftMask = check_modifier(ShiftMaskSyms);
    gLockMask = check_modifier(LockMaskSyms);
    gControlMask = check_modifier(ControlMaskSyms);
    gMod1Mask = check_modifier(Mod1MaskSyms);
    gMod2Mask = check_modifier(Mod2MaskSyms);
    gMod3Mask = check_modifier(Mod3MaskSyms);
    gMod4Mask = check_modifier(Mod4MaskSyms);
    gMod5Mask = check_modifier(Mod5MaskSyms);
}

/*
 * Local variables:
 *  c-indent-level: 4
 *  c-basic-offset: 4
 * End:
 */
