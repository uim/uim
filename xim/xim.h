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

// -*- C++ -*-
#ifndef _xim_h_included_
#define _xim_h_included_

#include <X11/X.h>
#include <X11/Xlib.h>
#include <X11/keysym.h>
#include <X11/Xos.h>
#include <list>
#include "ximserver.h"
#include "ximpn.h"
#include "compose.h"

// The header file contains about connection of XIM protocol, and the
// definition of IM and IC.

typedef unsigned char C8;
typedef unsigned short C16;
typedef unsigned int C32;

class XimIC;

// translate into host order
C16 readC16(unsigned char *val, int byte_order);
C32 readC32(unsigned char *val, int byte_order);

class TxPacket {
public:
    virtual ~TxPacket() {};

    virtual int get_length() = 0;
    virtual int write_to_buf(unsigned char *buf, int buflen, int byte_order) = 0;

    virtual void dump(int byte_order) = 0;
    virtual int get_major() = 0;

    virtual int pushC8(unsigned int) = 0;
    virtual int pushC16(unsigned int) = 0;
    virtual int pushC32(unsigned int) = 0;
    virtual int pushSTRING(char *) = 0;
    virtual int pushBytes(char *, int) = 0;

    virtual int pop_back() = 0;
};

class RxPacket {
public:
    virtual ~RxPacket() {};

    virtual void rewind() = 0;
    virtual C8 getC8() = 0;
    virtual C16 getC16() = 0;
    virtual C32 getC32() = 0;
    // this Str means STRING in xim.PS
    virtual int getStrLen() = 0;
    virtual void getStr(char *buf) = 0;
    // STR
    virtual int getStr8Len() = 0;
    virtual void getStr8(char *buf) = 0;

    virtual int getMajor() = 0;

    virtual bool isOverRun() = 0;
    virtual void dump() = 0;
    static int getPacketLength(unsigned char *, int byte_order);
};

TxPacket *createTxPacket(int major, int minor);
RxPacket *createRxPacket(unsigned char *buf, int byte_order);
RxPacket *copyRxPacket(RxPacket *packet);

class Connection {
public:
    Connection(XimServer *);
    virtual ~Connection();
    void OnRecv();
    void OnSend();
    void OnClose();
    virtual void OnPushPacket() {}; // Called when packet is pushed into sending queue.  But doesn't do anything for now.
    void push_packet(TxPacket *); // for normal packet for reply
    void push_passive_packet(TxPacket *); // for preceding packet for reply
    int byte_order() {return mByteorder;};
    void push_error_packet(int imid, int icid, int er, char *str);

    unsigned short to_hs(unsigned short s);
    unsigned int to_hl(unsigned int l);
    void terminate();
    XimServer *getXimServer();
protected:
    virtual void setSyncFlag();
    virtual void unsetSyncFlag();
    virtual bool hasSyncFlag();
    virtual void setPreeditStartSyncFlag();
    virtual void unsetPreeditStartSyncFlag();
    virtual bool hasPreeditStartSyncFlag();

    std::list<RxPacket *> mRxQ;
    std::list<RxPacket *> mPendingRxQ; // pending queue for XIM_FORWARD Rx event
    std::list<TxPacket *> mTxQ;
    std::list<TxPacket *> mPTxQ;
    std::list<TxPacket *> mPendingTxQ; // pending queue for mTxQ
    int mByteorder;
    bool mIsCloseWait; // true when the last packet has handled
private:
    void xim_connect(RxPacket *);
    void xim_disconnect(RxPacket *);
    void xim_open(RxPacket *);
    void xim_query_extension(RxPacket *);
    void xim_encoding_negotiation(RxPacket *);
    void xim_close(RxPacket *);
    void xim_get_im_values(RxPacket *);
    void xim_set_ic_values(RxPacket *);
    void xim_get_ic_values(RxPacket *);

    void xim_create_ic(RxPacket *);
    void xim_destroy_ic(RxPacket *);

    void xim_set_ic_focus(RxPacket *);
    void xim_unset_ic_focus(RxPacket *);
    void xim_reset_ic(RxPacket *);

    void xim_forward_event(RxPacket *);
    void xim_sync_reply(RxPacket *);
    void xim_preedit_start_reply(RxPacket *);
    void xim_error(RxPacket *);

    bool is_xim_sync_reply_timeout();
    void clear_pending_rx();
private:
    XimIC *get_ic(RxPacket *);
    std::list<int> mCreatedIm;
    XimServer *mServer;
    bool mSyncFlag;
    bool mPreeditStartSyncFlag;
    struct timeval mSyncStartTime;
};

// definition of IM
class XimIM {
public:
    XimIM(Connection *, int id);
    virtual ~XimIM();
    
    virtual void create_ic(RxPacket *) = 0;
    virtual void destroy_ic(int) = 0;
    virtual void set_ic_focus(int icid) = 0;
    virtual void set_ic_values(RxPacket *) = 0;
    virtual void get_ic_values(RxPacket *) = 0;
    virtual void unset_ic_focus(int icid) = 0;
    virtual void forward_event(RxPacket *) = 0;
    virtual void send_sync_reply(int icid) = 0;
    virtual XimIC *get_ic_by_id(int icid) = 0;
    virtual void onSendPacket() = 0;
    virtual void changeContext(const char *engine) = 0;
    void set_encoding(const char *encoding);
    const char *get_encoding();
    void set_lang_region(const char *name);
    const char *get_lang_region();
    // for Compose
    void create_compose_tree();
    DefTree *get_compose_tree();

protected:
    Connection *mConn;
    int mID;
    char *mEncoding;
    char *mLangRegion;

    // for Compose
    char *get_compose_filename();
    char *TransFileName(char *name);
    void ParseComposeStringFile(FILE *fp);
    void FreeComposeTree(DefTree *top);
    int parse_compose_line(FILE *fp, char *tokenbuf);
    int get_mb_string(char *buf, KeySym ks);
    DefTree *mTreeTop;
};

int unused_im_id();
XimIM *create_im(Connection *, int id);
XimIM *get_im_by_id(int id);
void close_im(int id);


struct keyEventX {
    KeySym key_sym; // keysym of X
    int state;
    bool press;
    int serial;
    XEvent ev;
};

class InputContext;
class Convdisp;

// icxatr is put in XimIC
class icxatr {
public:
    icxatr();
    ~icxatr();
    void set_atr(int id, C8 *v, int len, int byte_order);
    bool has_atr(int id);
    bool is_changed(int id);
    void unset_change_mask(int id);
    void print();
    int getSize(int id);
    void set_locale_name(const char *locale);

    unsigned long input_style;
    Window client_window;
    Window focus_window;

    C32 foreground_pixel; // Actually Pixel type
    C32 background_pixel;

    XPoint spot_location;
    XRectangle area;

    XFontSet font_set;
    C16 line_space;

private:
    char *font_set_name;
    char *m_locale;
    int atr_mask;
    int change_mask;
};

// definition of IC
class XimIC {
public:
    XimIC(Connection *, int imid, int icid, const char *engine);
    ~XimIC();
    void setFocus();
    void unsetFocus();
    int get_icid();
    int get_imid();

    void OnKeyEvent(keyEventX );
    void setICAttrs(void *, int);
    int get_ic_atr(int, TxPacket *);
    void commit_string(const char *s);
    void extra_input(char *t);
    void reset_ic();
    Convdisp *get_convdisp();
    void onSendPacket();
    bool isActive();
    void force_send_packet();
    void changeContext(const char *engine);
    const char *get_encoding();
    const char *get_lang_region();

public:
    static XimIC *get_current_ic();
    static bool isAnyActive();
  
private:
    // m_kkContext is created when XimIC is constructed, and deleted
    // when the XimIC is destructed.
    InputContext *m_kkContext;
    icxatr m_xatr;
    void send_key_event(XKeyEvent *k);
    int lookup_style(unsigned long);
    void set_ic_attr(int, C8 *, int );
    
    Connection *mConn;
    // mConvdisp is 0 until getting enough icxatr.  Need to delete
    // this after deletion of m_kkContext since it is also refered by
    // m_kkContext.
    Convdisp *mConvdisp;
    int mICid;
    int mIMid;
    uString mPending;
    bool mIsActive;
    keyState *m_keyState;
private:
    static XimIC *current_ic;
    static int nrActiveIC;
};

struct input_style {
    int x_style;
    int style;
};

XimIC *create_ic(Connection *, RxPacket *, int imid, int id, const char *engine);
void force_event(Window w);

void procXClientMessage(XClientMessageEvent *m);

#endif
/*
 * Local variables:
 *  c-indent-level: 4
 *  c-basic-offset: 4
 * End:
 */
