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

// Handle IM procedure defined in XIM protocol

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <map>

#include "xim.h"
#include <X11/Xutil.h>
#define NEED_EVENTS	// for declaration of xEvent
#include <X11/Xproto.h>

#ifdef HAVE_ALLOCA_H
# include <alloca.h>
#endif

static std::map<C16, XimIM *> g_ims;

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


class XimIM_impl : public XimIM {
public:
    XimIM_impl(Connection *c, C16 id);
    virtual ~XimIM_impl();
    virtual void create_ic(RxPacket *);
    virtual void destroy_ic(C16);
    virtual void set_ic_focus(C16 icid);
    virtual void set_ic_values(RxPacket *);
    virtual void get_ic_values(RxPacket *);
    virtual void unset_ic_focus(C16 icid);
    virtual void forward_event(RxPacket *);
    virtual void send_sync_reply(C16 icid);
    virtual void send_sync(C16 icid);
    virtual XimIC *get_ic_by_id(C16 id);
    virtual void onSendPacket();
    virtual void changeContext(const char *);

private:
    C16 unused_ic_id();
    void free_all_ic();
    void delete_ic(XimIC *);
    char *mEngineName;
    std::map<C16, XimIC *> m_ics;
};

XimIM_impl::XimIM_impl(Connection *c, C16 id) : XimIM(c, id)
{
    mEngineName = strdup(mConn->getXimServer()->getIMName());
}

XimIM_impl::~XimIM_impl()
{
    free_all_ic();
    free(mEngineName);
}

void XimIM_impl::create_ic(RxPacket *p)
{
    XimIC *ic;
    C16 icid= unused_ic_id();

    // create compose table with the first ic
    if (icid == 1)
	create_compose_tree();

    ic = ::create_ic(mConn, p, mID, icid, mEngineName);
    if (!ic) {
	mConn->push_error_packet(mID, icid,
				 ERR_Style, "invalid im style");
	return;
    }
    std::pair<C16, XimIC *> n(ic->get_icid(), ic);
    m_ics.insert(n);

    TxPacket *t;
    t = createTxPacket(XIM_CREATE_IC_REPLY, 0);
    t->pushC16(mID);
    t->pushC16(ic->get_icid());
    mConn->push_packet(t);
}

void XimIM_impl::destroy_ic(C16 icid)
{
    TxPacket *t;
    t = createTxPacket(XIM_DESTROY_IC_REPLY, 0);
    t->pushC16(mID);
    t->pushC16(icid);
    mConn->push_packet(t);
    // destruct IC
    XimIC *ic;
    ic = get_ic_by_id(icid);
    delete_ic(ic);
}

void XimIM_impl::changeContext(const char *engine)
{
    std::map<C16, XimIC *>::iterator i;
    for (i = m_ics.begin(); i != m_ics.end(); ++i) {
	(*i).second->changeContext(engine);
    }
    free(mEngineName);
    mEngineName = strdup(engine);
}

void XimIM_impl::set_ic_values(RxPacket *p)
{
    C16 imid, icid;
    XimIC *ic;
    p->rewind();
    imid = p->getC16();
    icid = p->getC16();
    ic = get_ic_by_id(icid);

    int atr_len;
    atr_len = p->getC16();
    p->getC16();

    unsigned char *v;
    v = (unsigned char *)alloca(atr_len);

    int i;
    for (i = 0; i < atr_len; i++) {
	v[i] = p->getC8();
    }

    ic->setICAttrs((void *)v, atr_len);

    TxPacket *t = createTxPacket(XIM_SET_IC_VALUES_REPLY, 0);
    t->pushC16(imid);
    t->pushC16(icid);
    mConn->push_packet(t);
}

void XimIM_impl::get_ic_values(RxPacket *p)
{
    C16 icid;
    XimIC *ic;
    p->rewind();
    p->getC16();
    icid = p->getC16();
    ic = get_ic_by_id(icid);

    int len;
    len = p->getC16();

    TxPacket *t = createTxPacket(XIM_GET_IC_VALUES_REPLY, 0);
    t->pushC16(mID);
    t->pushC16(icid);
    int i, l;
    C16 id;
    l = 0;
    for (i = 0; i < len / 2; i++) {
	id = p->getC16();
	l += ic->get_ic_atr(id, 0);
	l += 4;
    }
    t->pushC16((C16)l);
    t->pushC16(0);

    p->rewind();
    p->getC16(); // imid
    p->getC16(); // icid
    p->getC16(); // n

    for (i = 0; i < len / 2; i++) {
	id = p->getC16();
	t->pushC16(id);
	t->pushC16(ic->get_ic_atr(id, 0));
	l += ic->get_ic_atr(id, t);
    }
    mConn->push_packet(t);
}

C16 XimIM_impl::unused_ic_id()
{
    std::map<C16, XimIC *>::iterator i;
    C16 max_id = 1; // Does ID of input-context start with 1?
    for (i = m_ics.begin(); i != m_ics.end(); ++i) {
	if (max_id <= (*i).first)
	    max_id = (C16)((*i).first + 1);
    }
    return max_id;
}

void XimIM_impl::set_ic_focus(C16 icid)
{
    XimIC *ic = get_ic_by_id(icid);
    if (ic)
	ic->setFocus();
}

void XimIM_impl::unset_ic_focus(C16 icid)
{
    XimIC *ic = get_ic_by_id(icid);
    if (ic)
	ic->unsetFocus();
}

XimIC *XimIM_impl::get_ic_by_id(C16 icid)
{
    std::map<C16, XimIC *>::iterator it;
    it = m_ics.find(icid);
    if (it == m_ics.end())
	return 0;

    return it->second;
}

void XimIM_impl::forward_event(RxPacket *p)
{
    unsigned char *c;
    int i;
    keyEventX k;
    xEvent ev_raw;

    C16 imid, icid;
    int flag;

    XimIC *ic;
    imid = p->getC16();
    icid = p->getC16();
    flag = p->getC16();
    ic = get_ic_by_id(icid);
    k.serial = p->getC16();

    // keep copy of the xEvent
    c = (unsigned char *)&ev_raw;
    for (i = 0; i < (int)sizeof(ev_raw); i++) {
	*c = p->getC8();
	c++;
    }
    
    k.ev.type = ev_raw.u.u.type & 0x7f;
    k.ev.xany.serial = (k.serial << 16) | mConn->to_hs(ev_raw.u.u.sequenceNumber);
    k.ev.xany.display = XimServer::gDpy;
    k.ev.xany.send_event = ev_raw.u.u.type > 127;

    switch (k.ev.type) {
    case KeyPress:
    case KeyRelease:
	k.ev.xkey.window =
	    mConn->to_hl(ev_raw.u.keyButtonPointer.event);
	k.ev.xkey.root =
	    mConn->to_hl(ev_raw.u.keyButtonPointer.root);
	k.ev.xkey.subwindow =
	    mConn->to_hl(ev_raw.u.keyButtonPointer.child);
	k.ev.xkey.time =
	    mConn->to_hl(ev_raw.u.keyButtonPointer.time);
	k.ev.xkey.x =
	    mConn->to_hs(ev_raw.u.keyButtonPointer.eventX);
	k.ev.xkey.y =
	    mConn->to_hs(ev_raw.u.keyButtonPointer.eventY);
	k.ev.xkey.x_root =
	    mConn->to_hs(ev_raw.u.keyButtonPointer.rootX);
	k.ev.xkey.y_root =
	    mConn->to_hs(ev_raw.u.keyButtonPointer.rootY);
	k.ev.xkey.state = mConn->to_hs(ev_raw.u.keyButtonPointer.state);
	k.ev.xkey.keycode = ev_raw.u.u.detail;
	k.ev.xkey.same_screen = ev_raw.u.keyButtonPointer.sameScreen;
	char  buf[10];
	KeySym ks;
	XLookupString(&k.ev.xkey, buf, 10, &ks, 0);
	k.state = mConn->to_hs(ev_raw.u.keyButtonPointer.state);
	k.press = (k.ev.type == KeyPress);
	k.key_sym = ks;

	if (ic) {
	    InputContext *focusedContext = InputContext::focusedContext();
	    if (!focusedContext)
		ic->setFocus(); // workaround for some buggy applications
	    ic->OnKeyEvent(k);
	}
	if (!(g_option_mask & OPT_ON_DEMAND_SYNC))
	    send_sync_reply(icid);
	break;
    default:
	printf("unknown type of forwarded event.(%d)\n", k.ev.type);
	if (!(g_option_mask & OPT_ON_DEMAND_SYNC))
	    send_sync_reply(icid);
	break;
    }
}

void XimIM_impl::free_all_ic()
{
    std::map<C16, XimIC *>::iterator i;
    for (i = m_ics.begin(); i != m_ics.end(); ++i) {
	(*i).second->unsetFocus();
	delete (*i).second;
    }
    m_ics.erase(m_ics.begin(), m_ics.end());
}

void XimIM_impl::delete_ic(XimIC *ic)
{
    std::map<C16, XimIC *>::iterator it;
    for (it = m_ics.begin(); it != m_ics.end(); ++it) {
	if (it->second == ic) {
	    it->second->unsetFocus();
	    delete it->second;
	    m_ics.erase(it);
	    return;
	}
    }
}

void XimIM_impl::send_sync_reply(C16 icid)
{
    TxPacket *t = createTxPacket(XIM_SYNC_REPLY, 0);
    t->pushC16(mID);
    t->pushC16(icid);
    mConn->push_packet(t);
}

void XimIM_impl::send_sync(C16 icid)
{
    TxPacket *t = createTxPacket(XIM_SYNC, 0);
    t->pushC16(mID);
    t->pushC16(icid);
    mConn->push_packet(t);
}

void XimIM_impl::onSendPacket()
{
    std::map<C16, XimIC *>::iterator i;
    for (i = m_ics.begin(); i != m_ics.end(); ++i) {
	(*i).second->onSendPacket();
    }
}

XimIM::XimIM(Connection *c, C16 id)
{
    mConn = c;
    mID = id;
    mEncoding = NULL;
    mLangRegion = NULL;
    mTreeTop = NULL;
    mLocale = NULL;
}

XimIM::~XimIM()
{
    free(mEncoding);
    free(mLangRegion);
    FreeComposeTree(mTreeTop);
    delete mLocale;
}

void XimIM::FreeComposeTree(DefTree *top)
{
   if (!top)
	return;

   if (top->succession)
	FreeComposeTree(top->succession);
   if (top->next)
	FreeComposeTree(top->next);
   free(top->mb);
   free(top->utf8);
   free(top);
}

void XimIM::set_encoding(const char *encoding)
{
    free(mEncoding);
    mEncoding = strdup(encoding);

    // set iconv environment
    if (mLocale)
	delete mLocale;
    // workaround for Solaris 10 (bug #7558)
    char *p;
    if (!strcasecmp(encoding, "EUC") && mLangRegion &&
	(p = strchr(mLangRegion, '_'))) {
	char *iconv_encoding = (char *)malloc(3 + strlen(p + 1) + 1);
	if (iconv_encoding) {
	    sprintf(iconv_encoding, "euc%s", p + 1);
	    mLocale = createLocale(iconv_encoding);
	    free(iconv_encoding);
	} else {
	    mLocale = createLocale(mEncoding);
	}
    } else {
	mLocale = createLocale(mEncoding);
    }
}

const char *XimIM::get_encoding()
{
    return mEncoding;
}

void XimIM::set_lang_region(const char *lang_and_region)
{
    free(mLangRegion);
    mLangRegion = strdup(lang_and_region);
}

const char *XimIM::get_lang_region()
{
    return mLangRegion;
}

struct input_style *XimIM::getInputStyles()
{
    if (mLocale && mLocale->supportOverTheSpot())
	return input_style_tab_with_over_the_spot;

    return input_style_tab_without_over_the_spot;
}

char *XimIM::uStringToCtext(uString *us)
{
    char *ret = NULL;
    if (mLocale)
	ret = mLocale->uStringToCtext(us);

    return ret;
}

char *XimIM::utf8_to_native_str(char *str)
{
    char *ret = NULL;
    if (mLocale)
	ret = mLocale->utf8_to_native_str(str);

    return ret;
}

C16 unused_im_id()
{
    C16 max_id;
    std::map<C16, XimIM *>::iterator i;
    max_id = 1;
    for (i = g_ims.begin(); i != g_ims.end(); ++i) {
	if ((*i).first == max_id)
	    max_id = (C16)((*i).first + 1);
    }
    return max_id;
}

XimIM *create_im(Connection *c, C16 id)
{
    XimIM *im;
    im = new XimIM_impl(c, id);
    std::pair<C16, XimIM *> p(id, im);
    g_ims.insert(p);
    return im;
}

XimIM *get_im_by_id(C16 id)
{
    std::map<C16, XimIM *>::iterator it;
    it = g_ims.find(id);
    if (it == g_ims.end())
	return NULL;

    return it->second;
}

void close_im(C16 id)
{
    XimIM *im;

    im = get_im_by_id(id);
    if (im)
	delete im;

    std::map<C16, XimIM *>::iterator it;
    it = g_ims.find(id);
    if (it != g_ims.end())
	g_ims.erase(it);
}
/*
 * Local variables:
 *  c-indent-level: 4
 *  c-basic-offset: 4
 * End:
 */
