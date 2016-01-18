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

// -*- C++ -*-
#ifndef UIM_XIM_XIM_H
#define UIM_XIM_XIM_H

#include <X11/X.h>
#include <X11/Xlib.h>
#include <X11/keysym.h>
#include <X11/Xos.h>
#include <cstdio>
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
    virtual C8 get_major() = 0;

    virtual int pushC8(C8) = 0;
    virtual int pushC16(C16) = 0;
    virtual int pushC32(C32) = 0;
    virtual int pushSTRING(char *) = 0;
    virtual int pushBytes(const char *, int) = 0;

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

TxPacket *createTxPacket(C8 major, C8 minor);
RxPacket *createRxPacket(unsigned char *buf, int byte_order);
RxPacket *copyRxPacket(RxPacket *packet);

class Connection {
public:
    Connection(XimServer *);
    virtual ~Connection();
    void OnRecv();
    void OnSend();
    void OnClose();
    void push_packet(TxPacket *); // for normal packet for reply
    void push_passive_packet(TxPacket *); // for preceding packet for reply
    int byte_order() {return mByteorder;};
    void push_error_packet(C16 imid, C16 icid, C16 er, const char *str);

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
    virtual void setPreeditCaretSyncFlag();
    virtual void unsetPreeditCaretSyncFlag();
    virtual bool hasPreeditCaretSyncFlag();

    std::list<RxPacket *> mRxQ;
    std::list<TxPacket *> mTxQ;
    std::list<TxPacket *> mPTxQ;
    std::list<TxPacket *> mPendingTxQ; // pending queue for mTxQ and mPTxQ
    int mByteorder;
    bool mIsCloseWait; // true when the last packet has handled
private:
    void xim_connect(RxPacket *);
    void xim_disconnect();
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
    void xim_sync_reply();
    void xim_preedit_start_reply();
    void xim_preedit_caret_reply();
    void xim_error(RxPacket *);

    bool is_xim_sync_reply_timeout();
    void clear_pending_queue();
private:
    XimIC *get_ic(RxPacket *);
    std::list<C16> mCreatedIm;
    XimServer *mServer;
    bool mSyncFlag;
    bool mPreeditStartSyncFlag;
    bool mPreeditCaretSyncFlag;
    struct timeval mSyncStartTime;
};

// definition of IM
class XimIM {
public:
    XimIM(Connection *, C16 id);
    virtual ~XimIM();
    
    virtual void create_ic(RxPacket *) = 0;
    virtual void destroy_ic(C16) = 0;
    virtual void set_ic_focus(C16 icid) = 0;
    virtual void set_ic_values(RxPacket *) = 0;
    virtual void get_ic_values(RxPacket *) = 0;
    virtual void unset_ic_focus(C16 icid) = 0;
    virtual void forward_event(RxPacket *) = 0;
    virtual void send_sync_reply(C16 icid) = 0;
    virtual void send_sync(C16 icid) = 0;
    virtual XimIC *get_ic_by_id(C16 icid) = 0;
    virtual void onSendPacket() = 0;
    virtual void changeContext(const char *engine) = 0;
    void set_encoding(const char *encoding);
    const char *get_encoding();
    void set_lang_region(const char *name);
    const char *get_lang_region();
    char *uStringToCtext(uString *us);
    char *utf8_to_native_str(char *str);
    struct input_style *getInputStyles();
    // for Compose
    void create_compose_tree();
    DefTree *get_compose_tree();

protected:
    Connection *mConn;
    Locale *mLocale;
    C16 mID;
    char *mEncoding;
    char *mLangRegion;

    // for Compose
    int get_compose_filename(char *filename, size_t len);
    int TransFileName(char *transname, const char *name, size_t len);
    void ParseComposeStringFile(FILE *fp);
    void FreeComposeTree(DefTree *top);
    int parse_compose_line(FILE *fp, char **tokenbuf, size_t *buflen);
    int get_mb_string(char *buf, KeySym ks);
    DefTree *mTreeTop;
};

C16 unused_im_id();
XimIM *create_im(Connection *, C16 id);
XimIM *get_im_by_id(C16 id);
void close_im(C16 id);


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
    void set_atr(C16 id, C8 *v, int byte_order);
    bool has_atr(C16 id);
    bool is_changed(C16 id);
    void unset_change_mask(C16 id);
    void print();
    C16 getSize(C16 id);
    void set_locale_name(const char *locale);
    bool use_xft();

    unsigned long input_style;
    Window client_window;
    Window focus_window;

    C32 foreground_pixel; // Actually Pixel type
    C32 background_pixel;

    XPoint spot_location;
    XRectangle area;

    XFontSet font_set;
    char *font_set_name;
    C16 line_space;

private:
    char *m_locale;
    int atr_mask;
    int change_mask;
    bool m_use_xft;
};

// definition of IC
class XimIC {
public:
    XimIC(Connection *, C16 imid, C16 icid, const char *engine);
    ~XimIC();
    void setFocus();
    void unsetFocus();
    C16 get_icid();
    C16 get_imid();

    void OnKeyEvent(keyEventX );
    void setICAttrs(void *, int);
    C16 get_ic_atr(C16, TxPacket *);
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
    void set_ic_attr(C16, C8 *, int);
    void send_sync();
    
    Connection *mConn;
    // mConvdisp is NULL until getting enough icxatr.  Need to delete
    // this after deletion of m_kkContext since it is also refered by
    // m_kkContext.
    Convdisp *mConvdisp;
    C16 mICid;
    C16 mIMid;
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

XimIC *create_ic(Connection *, RxPacket *, C16 imid, C16 id, const char *engine);
void procXClientMessage(XClientMessageEvent *m);

#endif
/*
 * Local variables:
 *  c-indent-level: 4
 *  c-basic-offset: 4
 * End:
 */
