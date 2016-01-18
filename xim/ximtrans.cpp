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

// receive XIM packet and dispatch it.
 
#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <cstdio>
#include <cstring>
#include <X11/Xos.h>
#include "xim.h"
#include "util.h"

#ifdef HAVE_ALLOCA_H
# include <alloca.h>
#endif

// XIM Error Code
#define XIM_BadSomething	999

const char *xim_packet_name[] = {
    // 0
    0, "XIM_CONNECT", "XIM_CONNECT_REPLY",
    "XIM_DISCONNECT", "XIM_DISCONNECT_REPLY",
    0, 0, 0, 0, 0,
    // 10
    "XIM_AUTH_REQUIRED", "XIM_AUTH_REPLY", "XIM_AUTH_NEXT",
    "XIM_AUTH_SETUP", "XIM_AUTH_NG", 0, 0, 0, 0, 0,
    // 20
    "XIM_ERROR", 0, 0, 0, 0, 0, 0, 0, 0, 0,
    // 30
    "XIM_OPEN", "XIM_OPEN_REPLY", "XIM_CLOSE",
    "XIM_CLOSE_REPLY", "XIM_REGISTER_TRIGGERKEYS",
    "XIM_TRIGGER_NOTIFY", "XIM_TRIGGER_NOTIFY_REPLY",
    "XIM_SET_EVENT_MASK", "XIM_ENCODING_NEGOTIATION",
    "XIM_ENCODING_NEGOTIATION_REPLY",
    // 40
    "XIM_QUERY_EXTENSION", "XIM_QUERY_EXTENSION_REPLY",
    "XIM_SET_IM_VALUES", "XIM_SET_IM_VALUES_REPLY",
    "XIM_GET_IM_VALUES", "XIM_GET_IM_VALUES_REPLY",
    0, 0, 0, 0,
    // 50
    "XIM_CREATE_IC", "XIM_CREATE_IC_REPLY",
    "XIM_DESTROY_IC", "XIM_DESTROY_IC_REPLY",
    "XIM_SET_IC_VALUES", "XIM_SET_IC_VALUES_REPLY",
    "XIM_GET_IC_VALUES", "XIM_GET_IC_VALUES_REPLY",
    "XIM_SET_IC_FOCUS", "XIM_UNSET_FOCUS",
    // 60
    "XIM_FORWARD_EVENT", "XIM_SYNC", "XIM_SYNC_REPLY",
    "XIM_COMMIT", "XIM_RESET_IC", "XIM_RESET_IC_REPLY",
    0, 0, 0, 0,
    // 70
    "XIM_GEOMETRY", "XIM_STR_CONVERSION",
    "XIM_STR_CONVERSION_REPLY", "XIM_PREEDIT_START",
    "XIM_PREEDIT_START_REPLY", "XIM_PREEDIT_DRAW",
    "XIM_PREEDIT_CARET", "XIM_PREEDIT_CARET_REPLY",
    "XIM_PREEDIT_DONE", "XIM_STATUS_START",
    // 80
    "XIM_STATUS_DRAW", "XIM_STATUS_DONE", "XIM_PREEDITSTATE"
};

static struct XIMATTRIBUTE {
    XIMATTRIBUTE(const char *n, C16 t);
    static void write_imattr_to_packet(TxPacket *p);

    const char *name;
    C16 type;
} xim_attributes[] = {
    XIMATTRIBUTE(XNQueryInputStyle, TYPE_XIMSTYLE),
};

XIMATTRIBUTE::XIMATTRIBUTE(const char *n, C16 t)
{
    name = n;
    type = t;
}

void XIMATTRIBUTE::write_imattr_to_packet(TxPacket *p)
{
    int i, l;
    char tmp[4];
    for (i = 0; i < 4; i++) {
	tmp[i] = 0;
    }
    for (i = 0, l = 0; i < (int)(sizeof(xim_attributes) / sizeof(XIMATTRIBUTE));
	 i++) {
	l += 6;
	l += static_cast<int>(strlen(xim_attributes[i].name));
	l += pad4(static_cast<int>(strlen(xim_attributes[i].name)) + 2);
    }
    p->pushC16((C16)l);
    for (i = 0; i < (int)(sizeof(xim_attributes) / sizeof(XIMATTRIBUTE)); i++) {
	p->pushC16((C16)i);
	p->pushC16(xim_attributes[i].type);
	p->pushC16((C16)strlen(xim_attributes[i].name));
	p->pushBytes(xim_attributes[i].name,
		     static_cast<int>(strlen(xim_attributes[i].name)));
	p->pushBytes(tmp, pad4(static_cast<int>(
					strlen(xim_attributes[i].name))) + 2);
    }
}

static struct XICATTRIBUTE {
    XICATTRIBUTE(const char *n, C16 t);
    static void write_icattr_to_packet(TxPacket *p);
    const char *name;
    C16 type;
} xic_attributes[] = {
    // the sequence is required to be same as the order of
    // ICATTTRIBUTE defined in ximpn.h
    XICATTRIBUTE(XNInputStyle, TYPE_LONG),
    XICATTRIBUTE(XNClientWindow, TYPE_WINDOW),
    XICATTRIBUTE(XNFocusWindow, TYPE_WINDOW),
    XICATTRIBUTE(XNPreeditAttributes, TYPE_NESTEDLIST),
    XICATTRIBUTE(XNForeground, TYPE_LONG),
    XICATTRIBUTE(XNBackground, TYPE_LONG),
    XICATTRIBUTE(XNSpotLocation, TYPE_POINT),
    XICATTRIBUTE(XNFontSet, TYPE_XFONTSET),
    XICATTRIBUTE(XNArea, TYPE_XRECTANGLE),
    XICATTRIBUTE(XNLineSpace, TYPE_WORD),
    //
    XICATTRIBUTE(XNStatusAttributes, TYPE_NESTEDLIST),
    XICATTRIBUTE(XNAreaNeeded, TYPE_XRECTANGLE),
    XICATTRIBUTE(XNColormap, TYPE_WORD),
    XICATTRIBUTE(XNStdColormap, TYPE_WORD),
    XICATTRIBUTE(XNBackgroundPixmap, TYPE_LONG),
    XICATTRIBUTE(XNCursor, TYPE_WORD),
    XICATTRIBUTE(XNFilterEvents, TYPE_WORD),
    XICATTRIBUTE(XNSeparatorofNestedList, TYPE_SEPARATOR),
};

XICATTRIBUTE::XICATTRIBUTE(const char *n, C16 t)
{
    name = n;
    type = t;
}

void XICATTRIBUTE::write_icattr_to_packet(TxPacket *p)
{
    int i, l;
    char tmp[4];
    for (i = 0; i < 4; i++) {
	tmp[i] = 0;
    }
    for (i = 0, l = 0; i < (int)(sizeof(xic_attributes) / sizeof(XICATTRIBUTE)); i++) {
	l += 6;
	l += static_cast<int>(strlen(xic_attributes[i].name));
	l += pad4(static_cast<int>(strlen(xic_attributes[i].name)) + 2);
    }
    p->pushC16((C16)l);
    p->pushC16(0);
    for (i = 0; i < (int)(sizeof(xic_attributes) / sizeof(XICATTRIBUTE)); i++) {
	p->pushC16((C16)i);
	p->pushC16(xic_attributes[i].type);
	p->pushC16((C16)strlen(xic_attributes[i].name));
	p->pushBytes(xic_attributes[i].name,
		     static_cast<int>(strlen(xic_attributes[i].name)));
	p->pushBytes(tmp, pad4(static_cast<int>(
					strlen(xic_attributes[i].name)) + 2));
    }
}

Connection::Connection(XimServer *svr)
{
    mIsCloseWait = false;
    mByteorder = BYTEORDER_UNKNOWN;
    mServer = svr;
    mSyncFlag = false;
    mPreeditStartSyncFlag = false;
    mPreeditCaretSyncFlag = false;
}

Connection::~Connection()
{
    // destruct all the IM created by this Connection
    std::list<C16>::iterator i;
    for (i = mCreatedIm.begin(); i != mCreatedIm.end(); ++i) {
	close_im(*i);
    }
    //
    std::list<RxPacket *>::iterator ir;
    for (ir = mRxQ.begin(); ir != mRxQ.end(); ++ir) {
	delete *ir;
    }
    std::list<TxPacket *>::iterator it;
    for (it = mTxQ.begin(); it != mTxQ.end(); ++it) {
	delete *it;
    }
    for (it = mPTxQ.begin(); it != mPTxQ.end(); ++it) {
	delete *it;
    }
    for (it = mPendingTxQ.begin(); it != mPendingTxQ.end(); ++it) {
	delete *it;
    }
}

void Connection::terminate()
{
    mIsCloseWait = true;
}

void Connection::OnRecv()
{
    std::list<RxPacket *>::iterator i;
    while (!mRxQ.empty()) {
	i = mRxQ.begin();
	int major = (*i)->getMajor();
	if (g_option_mask & OPT_TRACE_XIM)
	    printf("<-: %s.\n", xim_packet_name[major]);

	// Need to sort the handling sequence using protocol number
	// once implementing all the protocol.
	RxPacket *p;
	p = *i;
	switch (major) {
	case XIM_CONNECT:
	    xim_connect(p);
	    break;
	case XIM_OPEN:
	    xim_open(p);
	    break;
	case XIM_QUERY_EXTENSION:
	    xim_query_extension(p);
	    break;
	case XIM_ENCODING_NEGOTIATION:
	    xim_encoding_negotiation(p);
	    break;
	case XIM_CLOSE:
	    xim_close(p);
	    break;
	case XIM_DISCONNECT:
	    xim_disconnect();
	    break;
	case XIM_GET_IM_VALUES:
	    xim_get_im_values(p);
	    break;
	case XIM_SET_IC_VALUES:
	    xim_set_ic_values(p);
	    break;
	case XIM_GET_IC_VALUES:
	    xim_get_ic_values(p);
	    break;
	case XIM_CREATE_IC:
	    xim_create_ic(p);
	    break;
	case XIM_DESTROY_IC:
	    xim_destroy_ic(p);
	    break;
	case XIM_SET_IC_FOCUS:
	    xim_set_ic_focus(p);
	    break;
	case XIM_UNSET_IC_FOCUS:
	    xim_unset_ic_focus(p);
	    break;
	case XIM_TRIGGER_NOTIFY:
	    // xim_trigger_notify(*i); We don't use trigger!
	    break;
	case XIM_FORWARD_EVENT:
	    xim_forward_event(p);
	    break;
	case XIM_SYNC_REPLY:
	    xim_sync_reply();
	    break;
	case XIM_RESET_IC:
	    xim_reset_ic(p);
	    break;
	case XIM_PREEDIT_START_REPLY:
	    xim_preedit_start_reply();
	    break;
	case XIM_PREEDIT_CARET_REPLY:
	    xim_preedit_caret_reply();
	    break;
	case XIM_ERROR:
	    xim_error(p);
	    break;
	default:
	    printf("Unknown (or not implemented) packet from xim connection.\n");
	    (*i)->dump();
	    break;
	}
	mRxQ.pop_front();
	delete p;
    }
}

XimServer *Connection::getXimServer()
{
    return mServer;
}


void Connection::OnSend()
{
    std::list<C16>::iterator i;
    for (i = mCreatedIm.begin(); i != mCreatedIm.end(); ++i) {
	XimIM *im;
	im = get_im_by_id(*i);
	if (im)
	    im->onSendPacket();
    }
}

void Connection::OnClose()
{
    std::list<C16>::iterator i;
    for (i = mCreatedIm.begin(); i != mCreatedIm.end(); ++i) {
	close_im(*i);
    }
    mCreatedIm.erase(mCreatedIm.begin(), mCreatedIm.end());
}

void Connection::push_packet(TxPacket *p)
{
    mTxQ.push_back(p);
}

void Connection::push_passive_packet(TxPacket *p)
{
    mPTxQ.push_back(p);
}

void Connection::push_error_packet(C16 imid, C16 icid, C16 er, const char *str)
{
    TxPacket *t;
    t = createTxPacket(XIM_ERROR, 0);
    t->pushC16(imid);
    t->pushC16(icid);
    int m = 0;
    if (imid)
	m += 1;
    if (icid)
	m += 2;

    t->pushC16((C16)m);
    t->pushC16(er);
    int l = static_cast<int>(strlen(str));
    char tmp[4];
    t->pushC16((C16)l);
    t->pushC16(0);
    t->pushBytes(str, l);
    t->pushBytes(tmp, pad4(l));
    push_packet(t);
}

unsigned short Connection::to_hs(unsigned short s)
{
    if (host_byte_order == mByteorder)
	return s;

    unsigned char *v;
    v = (unsigned char *)&s;
    if (host_byte_order == LSB_FIRST)
	s = (unsigned short)((v[0] << 8) + v[1]);
    else
	s = (unsigned short)((v[1] << 8) + v[0]);
    return s;
}

unsigned int Connection::to_hl(unsigned int l)
{
    if (host_byte_order == mByteorder)
	return l;

    unsigned char *v;
    v = (unsigned char *)&l;
    if (host_byte_order != LSB_FIRST)
	l = (v[3] << 24) + (v[2] << 16) + (v[1] << 8)+ v[0];
    else
	l = (v[0] << 24) + (v[1] << 16) + (v[2] << 8)+ v[3];
    return l;
}

void Connection::setSyncFlag()
{
    mSyncFlag = true;
    X_GETTIMEOFDAY(&mSyncStartTime);
}

void Connection::unsetSyncFlag()
{
    mSyncFlag = false;
}

bool Connection::hasSyncFlag()
{
    return mSyncFlag;
}

void Connection::setPreeditStartSyncFlag()
{
    mPreeditStartSyncFlag = true;
}

void Connection::unsetPreeditStartSyncFlag()
{
    mPreeditStartSyncFlag = false;
}

bool Connection::hasPreeditStartSyncFlag()
{
    return mPreeditStartSyncFlag;
}

void Connection::setPreeditCaretSyncFlag()
{
    mPreeditCaretSyncFlag = true;
}

void Connection::unsetPreeditCaretSyncFlag()
{
    mPreeditCaretSyncFlag = false;
}

bool Connection::hasPreeditCaretSyncFlag()
{
    return mPreeditCaretSyncFlag;
}

//
// Packet handlers
//
void Connection::xim_connect(RxPacket *p)
{
    TxPacket *t;
    
    p->rewind();
    p->getC8();
    p->getC8(); // discard
    if (p->isOverRun()) {
	push_error_packet(0, 0, 0, "Invalid Packet");
	terminate();
	return;
    }

    t = createTxPacket(XIM_CONNECT_REPLY, 0);
    t->pushC16(1);
    t->pushC16(0);
    push_packet(t);
    if (g_option_mask & OPT_TRACE)
	printf("accept xim connection.\n");
}

void Connection::xim_disconnect()
{
    TxPacket *t;
    t = createTxPacket(XIM_DISCONNECT_REPLY, 0);
    push_packet(t);

    terminate();
    if (g_option_mask & OPT_TRACE)
	printf("disconnect xim connection.\n");
}

void Connection::xim_open(RxPacket *p)
{
    char buf[16];
    int l;
    for (l = 0; l < 16; l++) {
	buf[l] = 0;
    }
    l = p->getStr8Len();
    if (l > 15) {
	printf("too long locale name.\n");
	return;
    }
    p->getStr8(buf);

    TxPacket *t;
    C16 imid;
    XimIM *im;
    imid = unused_im_id();
    mCreatedIm.push_back(imid); // had to be deleted by the creator Connection
    im = create_im(this, imid);
    im->set_lang_region(buf);
    t = createTxPacket(XIM_OPEN_REPLY, 0);
    t->pushC16(imid);
    XIMATTRIBUTE::write_imattr_to_packet(t);
    XICATTRIBUTE::write_icattr_to_packet(t);
  
    push_packet(t);

    // EventMask selection

    t = createTxPacket(XIM_SET_EVENT_MASK, 0);
    t->pushC16(imid);
    t->pushC16(0);
    if (g_option_mask & OPT_ON_DEMAND_SYNC) {
	t->pushC32(KeyPressMask|KeyReleaseMask);
	t->pushC32((unsigned int)~(KeyPressMask|KeyReleaseMask)); // no need to send
						    // XIM_SYNC_REPLY from
						    // XIM server
    } else {
	t->pushC32(KeyPressMask);
	t->pushC32(KeyPressMask); // need to send XIM_SYNC_REPLY from XIM server
    }

    push_packet(t);
}

void Connection::xim_close(RxPacket *p)
{
    C16 imid;
    imid = p->getC16();
    TxPacket *t;
    t = createTxPacket(XIM_CLOSE_REPLY, 0);
    t->pushC16(imid);
    t->pushC16(0);
    push_packet(t);
    close_im(imid);
    std::list<C16>::iterator i;
    for (i = mCreatedIm.begin(); i != mCreatedIm.end(); ++i) {
	if (*i == imid) {
	    mCreatedIm.erase(i);
	    return;
	}
    }
}

void Connection::xim_query_extension(RxPacket *p)
{
    C16 imid;
    imid = p->getC16();

    TxPacket *t;
    t = createTxPacket(XIM_QUERY_EXTENSION_REPLY, 0);
    t->pushC16(imid);
    t->pushC16(0);
  
    push_packet(t);
}

void Connection::xim_encoding_negotiation(RxPacket *p)
{
    TxPacket *t;
    t = createTxPacket(XIM_ENCODING_NEGOTIATION_REPLY, 0);
    C16 l, index;
    int i, m, s;
    C16 imid, idx;
    char buf[32];
    XimIM *im;

    imid = p->getC16(); // m_imid
    l = p->getC16();
    im = get_im_by_id(imid);
  
    index = 0;
    for (m = 0, s = 0, idx = 0; m < l; m += (s + 1), idx++) {
	s = p->getStr8Len();

	for (i= 0; i < 32; i++) {
	    buf[i] = 0;
	}
	p->getStr8(buf);

	// use COMPOUND_TEXT
	if (!strcmp("COMPOUND_TEXT", buf))
	    index = idx;
	else {
	    if (im)
		im->set_encoding(buf);
	}
    }

    t->pushC16(imid);
    t->pushC16(0);
    t->pushC16(index);
    t->pushC16(0);
    push_packet(t);
}

void Connection::xim_get_im_values(RxPacket *p)
{
    int l, i;
    C16 imid;
    TxPacket *t;
    t = createTxPacket(XIM_GET_IM_VALUES_REPLY, 0);
    imid = p->getC16(); // input-method id
    l = p->getC16() / 2; // number

    int *ra = (int *)alloca(sizeof(int) * l);
    int rlen = 0;
    for (i = 0; i < l; i++) {
	ra[i] = p->getC16();
	rlen += 4;
    }

    // XIMATTRIBUTE
    C16 nr_style;
    struct input_style *is = get_im_by_id(imid)->getInputStyles();
    for (nr_style = 0; is[nr_style].style; nr_style++) {
	;
    }

    // since only one IMAttribute...
    t->pushC16(imid);
    t->pushC16((C16)(8 + nr_style * 4));

    t->pushC16(0); // attribute id
    t->pushC16((C16)(4 + nr_style * 4)); // length

    t->pushC16(nr_style); // number
    t->pushC16(0);
    for (i = 0; i < nr_style; i++) {
	t->pushC32(is[i].x_style);
    }

    push_packet(t);
}

void Connection::xim_set_ic_values(RxPacket *p)
{
    C16 imid;
    imid = p->getC16();
    p->rewind();
    XimIM *im;
    im = get_im_by_id(imid);
    if (im)
	im->set_ic_values(p);
}

void Connection::xim_get_ic_values(RxPacket *p)
{
    C16 imid;
    imid = p->getC16();
    p->rewind();
    XimIM *im;
    im = get_im_by_id(imid);
    im->get_ic_values(p);
}

void Connection::xim_create_ic(RxPacket *p)
{
    XimIM *im;
    C16 imid;
    imid = p->getC16();
    p->rewind();
    im = get_im_by_id(imid);
    im->create_ic(p);
}

void Connection::xim_destroy_ic(RxPacket *p)
{
    C16 imid;
    C16 icid;
    XimIM *im;
    imid = p->getC16();
    icid = p->getC16();
    im = get_im_by_id(imid);
    im->destroy_ic(icid);
}

void Connection::xim_set_ic_focus(RxPacket *p)
{
    C16 imid;
    XimIM *im;
    imid = p->getC16();
    im = get_im_by_id(imid);
    im->set_ic_focus(p->getC16());
}

void Connection::xim_unset_ic_focus(RxPacket *p)
{
    C16 imid;
    XimIM *im;
    imid = p->getC16();
    im = get_im_by_id(imid);
    im->unset_ic_focus(p->getC16());
}

void Connection::xim_forward_event(RxPacket *p)
{
    C16 imid, icid;
    XimIM *im;
    
    imid = p->getC16();
    icid = p->getC16();
    p->rewind();
    im = get_im_by_id(imid);
    if (hasSyncFlag()) {
	if (is_xim_sync_reply_timeout()) {
	    // XIM protocol error?
	    push_error_packet(imid, icid, ERR_BadProtocol, "Bad Protocol");
	    clear_pending_queue();
	    unsetSyncFlag();
	}
    }
    im->forward_event(p);
}

void Connection::xim_sync_reply()
{
    unsetSyncFlag();
}

void Connection::xim_reset_ic(RxPacket *p)
{
    XimIC *ic = get_ic(p);
    if (ic)
	ic->reset_ic();
}

void Connection::xim_preedit_start_reply()
{
    unsetPreeditStartSyncFlag();
}

void Connection::xim_preedit_caret_reply()
{
    unsetPreeditCaretSyncFlag();
}

void Connection::xim_error(RxPacket *p)
{
    C16 imid, icid, mask, ecode;
    int len;
    char *buf;

    imid = p->getC16();
    icid = p->getC16();
    mask = p->getC16();
    ecode = p->getC16();
    len = p->getStrLen();
    buf = (char *)alloca(len + 1);
    buf[len] = 0;
    p->getStr(buf);
  
    switch (ecode) {
    case XIM_BadSomething:
	unsetPreeditStartSyncFlag();
	unsetPreeditCaretSyncFlag();
	break;
    default:
	fprintf(stderr, "XIM_ERROR received.\n");
	if (mask & 1)
	    fprintf(stderr, " imid = %d\n", imid);
	if (mask & 2)
	    fprintf(stderr, " icid = %d\n", icid);
	fprintf(stderr, " error_code = %d\n", ecode);
	fprintf(stderr, " message = (%s)\n", buf);
	break;
    }
}

XimIC *Connection::get_ic(RxPacket *p)
{
    C16 imid, icid;
    XimIM *im;

    imid = p->getC16();
    icid = p->getC16();
    p->rewind();
    im = get_im_by_id(imid);
    return im->get_ic_by_id(icid);
}

#define XIM_SYNC_REPLY_TIMEOUT_SEC	5
#define TIMEDELTA(dest, src1, src2) { \
	if (((dest).tv_usec = (src1).tv_usec - (src2).tv_usec) < 0) {\
		(dest).tv_usec += 1000000;\
		(dest).tv_sec = (src1).tv_sec - (src2).tv_sec - 1;\
	} else (dest).tv_sec = (src1).tv_sec - (src2).tv_sec;  }

bool Connection::is_xim_sync_reply_timeout(void)
{
    struct timeval cur_time, time_spent;
    X_GETTIMEOFDAY(&cur_time);
    TIMEDELTA(time_spent, cur_time, mSyncStartTime);
    if (time_spent.tv_sec >= XIM_SYNC_REPLY_TIMEOUT_SEC) {
	fprintf(stderr, "Warning: XIM_SYNC_REPLY timeout\n");
	return true;
    } else
	return false;
}

void Connection::clear_pending_queue() {
    std::list<TxPacket *>::iterator i;
    while (!mPendingTxQ.empty()) {
	i = mPendingTxQ.begin();
	mPendingTxQ.pop_front();
	delete *i;
    }
}

/*
 * Local variables:
 *  c-indent-level: 4
 *  c-basic-offset: 4
 * End:
 */
