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

// connection management

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include "connection.h"

#include <csignal>
#include <cstdio>
#include <cstdlib>
#include <list>
#include <map>
#include <unistd.h>
#include <X11/Xatom.h>
#ifdef HAVE_ALLOCA_H
# include <alloca.h>
#endif

#define INIT_BUF_SIZE 1024
#define TRANSPORT_UNIT 20
#define TRANSPORT_MAX 20	// Emacs's XIM won't work correctly when the value is 100

#ifndef HAVE_SIG_T
typedef void (*sig_t)(int);
#endif

extern char *xim_packet_name[];

static std::map<Window, XConnection *> gXConnections;
static Atom xim_xconnect;
static Atom xim_protocol;
static Atom xim_moredata;
static Atom uim_comm;

void
XimServer::setupNewConnection(XClientMessageEvent *ev)
{
    XClientMessageEvent r;
    Window comWin, clientWin;

    comWin = XCreateSimpleWindow(XimServer::gDpy, ev->window,
				 0, 0, 1, 1, 0, 0, 0);
    clientWin = ev->data.l[0];

    r.type = ClientMessage;
    r.window = clientWin;
    r.message_type = xim_xconnect;
    r.format = 32;
    r.data.l[0] = comWin;
    r.data.l[1] = 0;
    r.data.l[2] = 2;
    r.data.l[3] = TRANSPORT_MAX;
    XSendEvent(XimServer::gDpy, clientWin, False, NoEventMask, (XEvent *)&r);

    XConnection *xc = new XConnection(this, clientWin, comWin);
    std::pair<Window, XConnection *> p(comWin, xc);
    gXConnections.insert(p);
}

static void
reapOldConnection()
{
    std::map<Window, XConnection *>::iterator it;
    for (it = gXConnections.begin(); it != gXConnections.end(); ++it) {
	XConnection *xc = (*it).second;
	if (!xc->isValid()) {
	    delete xc;
	    gXConnections.erase(it);
	    return;
	}
    }
}

void
procXClientMessage(XClientMessageEvent *ev)
{
    // check connection request
    XimServer *xs = XimServer::findServer(ev->window);
    if (xs && ev->message_type == xim_xconnect) {
	xs->setupNewConnection(ev);
	reapOldConnection();
	return;
    }

    // process xim packet
    if (ev->message_type != xim_moredata &&
	ev->message_type != xim_protocol) {
	printf("non xim message\n");
	return;
    }
    std::map<Window, XConnection *>::iterator i;
    i = gXConnections.find(ev->window);
    if (i == gXConnections.end()) {
	printf("packet for unknown window\n");
	return;
    }

    XConnection *xc = (*i).second;
    xc->readProc(ev);
}

XConnection::XConnection(XimServer *svr, Window clientWin, Window commWin) :
    Connection(svr)
{
    mClientWin = clientWin;
    mCommWin = commWin;
    mBuf.buf = (char *)malloc(INIT_BUF_SIZE);
    mBuf.len = 0;
    mBuf.size = INIT_BUF_SIZE;
    add_window_watch(mClientWin, this, STRUCTURE_NOTIFY_MASK);
    mIsValid = true;
}

XConnection::~XConnection()
{
    if (mIsValid)
	remove_window_watch(mClientWin);
    XDestroyWindow(XimServer::gDpy, mCommWin);
    free(mBuf.buf);
}

void XConnection::destroy(Window /* w */)
{
    if (mIsValid)
	OnClose();
    mIsValid = false;
}

void XConnection::readProc(XClientMessageEvent *ev)
{
    if (!readToBuf(ev))
	return;

    checkByteorder();

    bool pushed = true;
    do {
	int len = -1;
	if (mBuf.len >= 4)
	    len = RxPacket::getPacketLength((unsigned char *)mBuf.buf,
					    mByteorder);

	if ((len > 4 && len <= mBuf.len) ||
	    (len == 4 && mBuf.buf[0] == XIM_DISCONNECT)) {
	    RxPacket *p = 
		createRxPacket((unsigned char *)mBuf.buf, mByteorder);
	    shiftBuffer(len);
	    mRxQ.push_back(p);
	} else if (len == 4) {
	    // do nothing
	    shiftBuffer(4);
	} else {
	    pushed = false;
	}
    } while (pushed);
    OnRecv();

    writeProc();
}

void XConnection::shiftBuffer(int len)
{
    memmove(mBuf.buf, &mBuf.buf[len], mBuf.len - len);
    mBuf.len -= len;
}

bool XConnection::checkByteorder()
{
    if (mByteorder == BYTEORDER_UNKNOWN) {
	if (mBuf.buf[0] != XIM_CONNECT) {
	    printf("not XIM_CONNECT\n");
	    return false;
	}
	if (mBuf.buf[4] == 0x42)
	    mByteorder = MSB_FIRST;
	else if (mBuf.buf[4] == 0x6c)
	    mByteorder = LSB_FIRST;
	else
	    return false;
    }
    return true;
}

bool XConnection::readToBuf(XClientMessageEvent *ev)
{
    if (ev->format == 32 && ev->message_type == xim_protocol) {
	// indirect
	int offset = 0;
	Atom type;
	int format;
	unsigned long nrItems;
	unsigned long remain;
	char *data;

	while (ev->data.l[0] >= mBuf.size) {
	    mBuf.size += 1024;
	    mBuf.buf = (char *)realloc(mBuf.buf, mBuf.size);
	}
	do {
	    XGetWindowProperty(XimServer::gDpy, ev->window, ev->data.l[1],
			       offset, mBuf.size - mBuf.len, True,
			       AnyPropertyType,
			       &type, &format, &nrItems, &remain,
			       (unsigned char **)(uintptr_t)&data);
	    if (!data)
		return false;

	    if (format == 8) {
		memcpy(&mBuf.buf[mBuf.len], data, nrItems);
		mBuf.len += static_cast<int>(nrItems);
	    } else
		return false;
	    XFree(data);
	} while (remain > 0);
    } else if (ev->format == 8) {
	// direct
	if ((mBuf.len + TRANSPORT_UNIT) >= mBuf.size) {
	    mBuf.size += TRANSPORT_UNIT;
	    mBuf.buf = (char *)realloc(mBuf.buf, mBuf.size);
	}
	memcpy(&mBuf.buf[mBuf.len], ev->data.b, TRANSPORT_UNIT);
	mBuf.len += TRANSPORT_UNIT;
    } else
	return false;

    return true;
}

void XConnection::writePendingPacket()
{
    std::list<TxPacket *>::iterator i, j;
    bool sent_preedit_done = false;
    C8 major;

    while (!mPendingTxQ.empty()) {
	if (hasSyncFlag() || hasPreeditStartSyncFlag() ||
			hasPreeditCaretSyncFlag())
	    break;

	i = mPendingTxQ.begin();
	major = (*i)->get_major();

	switch (major) {
	case XIM_COMMIT:
	case XIM_FORWARD_EVENT:
	    setSyncFlag();
	    break;
	case XIM_PREEDIT_START:
	    setPreeditStartSyncFlag();
	    break;
	case XIM_PREEDIT_CARET:
	    setPreeditCaretSyncFlag();
	    break;
	case XIM_PREEDIT_DONE:
	    sent_preedit_done = true;
	default:
	    break;
	}

	doSend(*i, true);
	delete *i;
	mPendingTxQ.pop_front();

 	// XIM_PREEDIT_START/DRAW just after XIM_PREEDIT_DONE maybe
	// need to wait XIM_COMMIT and its XIM_SYNC_REPLY
	if (sent_preedit_done == true) {
	    // push first XIM_COMMIT into the head of the queue
	    std::list<TxPacket *> tmp;
	    bool first = true;
	    while (!mPendingTxQ.empty()) {
		j = mPendingTxQ.begin();
		major = (*j)->get_major();
		if (major == XIM_COMMIT && first == true) {
		    tmp.push_front(*j);
		    first = false;
		} else
		    tmp.push_back(*j);

		mPendingTxQ.pop_front();
	    }
	    mPendingTxQ = tmp;
	    sent_preedit_done = false;
	}
    }
}

void XConnection::writePassivePacket()
{
    std::list<TxPacket *>::iterator i, j;
    bool sent_preedit_done = false;
    C8 major;

    while (!mPTxQ.empty()) {
	if (!mPendingTxQ.empty())
	    break;

	i = mPTxQ.begin();

	if (hasSyncFlag() || hasPreeditStartSyncFlag() ||
			hasPreeditCaretSyncFlag()) {
	    mPendingTxQ.push_back(*i);
	    mPTxQ.pop_front();
	    break;
	}

	major = (*i)->get_major();
	switch (major) {
	case XIM_COMMIT:
	    setSyncFlag();
	    break;
	case XIM_PREEDIT_START:
	    setPreeditStartSyncFlag();
	    break;
	case XIM_PREEDIT_CARET:
	    setPreeditCaretSyncFlag();
	    break;
	case XIM_PREEDIT_DONE:
	    sent_preedit_done = true;
	    break;
	default:
	    break;
	}

	doSend(*i, true);
	delete *i;
	mPTxQ.pop_front();

	// XIM_PREEDIT_START/DRAW just after XIM_PREEDIT_DONE maybe
	// need to wait XIM_COMMIT and its XIM_SYNC_REPLY
	if (sent_preedit_done == true) {
	    // push first XIM_COMMIT into the head of passive queue
	    std::list<TxPacket *> tmp;
	    bool first = true;
	    while (!mPTxQ.empty()) {
		j = mPTxQ.begin();
		major = (*j)->get_major();
		if (major == XIM_COMMIT && first == true) {
		    tmp.push_front(*j);
		    first = false;
		} else
		    tmp.push_back(*j);

		mPTxQ.pop_front();
	    }
	    mPTxQ = tmp;
	    sent_preedit_done = false;
	}
    }
}

void XConnection::writeNormalPacket()
{
    std::list<TxPacket *>::iterator i;
    C8 major;

    while (!mTxQ.empty()) {
	i = mTxQ.begin();
	major = (*i)->get_major();
	if (major == XIM_FORWARD_EVENT) {
	    if (hasSyncFlag()) {
		// move this packet to pending queue
		mPendingTxQ.push_back(*i);
		mTxQ.pop_front();
		continue;
	    }
	    setSyncFlag();
	}
	doSend(*i, false);
	delete *i;
	mTxQ.pop_front();
    }
}

void XConnection::writeProc()
{
    OnSend(); // add XIM_COMMIT packet to passive queue

    writePendingPacket();
    writePassivePacket();
    writeNormalPacket();

    // interrupt while _XFlushInt() here will cause lock up of the display.
    sig_t old_sigusr1 = signal(SIGUSR1, SIG_IGN);
    XFlush(XimServer::gDpy);
    signal(SIGUSR1, old_sigusr1);

    if (mIsCloseWait) {
	remove_window_watch(mClientWin);
	mClientWin = None;
	mIsValid = false;
	OnClose();
    }
}

void XConnection::doSend(TxPacket *t, bool passive)
{
    if (g_option_mask & OPT_TRACE_XIM) {
	if (passive)
	    printf("(->): %s.\n", xim_packet_name[t->get_major()]);
	else
	    printf("->: %s.\n", xim_packet_name[t->get_major()]);
    }

    XClientMessageEvent r;
    int buflen;
    char *buf;

    buflen = t->get_length();
    buf = (char *)alloca(buflen);
    t->write_to_buf((unsigned char *)buf, buflen, mByteorder);
    if (buflen < TRANSPORT_MAX) {
	// via event
	int offset = 0;
	int length = buflen;
	while (length > TRANSPORT_UNIT) {
	    r.type = ClientMessage;
	    r.window = mClientWin;
	    r.format = 8;
	    r.message_type = xim_moredata;
	    memcpy(r.data.b, &buf[offset], TRANSPORT_UNIT);
	    XSendEvent(XimServer::gDpy, mClientWin, False, NoEventMask, (XEvent *)&r);
	    offset += TRANSPORT_UNIT;
	    length -= TRANSPORT_UNIT;
	}
	r.type = ClientMessage;
	r.window = mClientWin;
	r.format = 8;
	r.message_type = xim_protocol;
	memset(r.data.b, 0, TRANSPORT_UNIT);
	memcpy(r.data.b, &buf[offset], length);
	XSendEvent(XimServer::gDpy, mClientWin, False, NoEventMask, (XEvent *)&r);
    } else {
	// via property
	r.type = ClientMessage;
	r.window = mClientWin;
	r.format = 32;
	r.message_type = xim_protocol;
	r.data.l[0] = buflen;
	r.data.l[1] = uim_comm;
	XChangeProperty(XimServer::gDpy, mClientWin, uim_comm, XA_STRING,
			8, PropModeAppend, (unsigned char *)buf, buflen);
	XSendEvent(XimServer::gDpy, mClientWin, False, NoEventMask, (XEvent *)&r);
    }
}

int connection_setup()
{
    xim_xconnect = XInternAtom(XimServer::gDpy, "_XIM_XCONNECT", False);
    xim_protocol = XInternAtom(XimServer::gDpy, "_XIM_PROTOCOL", False);
    xim_moredata = XInternAtom(XimServer::gDpy, "_XIM_MOREDATA", False);
    uim_comm = XInternAtom(XimServer::gDpy, "UIM_COMM", False);
    return 0;
}
/*
 * Local variables:
 *  c-indent-level: 4
 *  c-basic-offset: 4
 * End:
 */
