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
#ifndef _ximserver_h_included_
#define _ximserver_h_included_

#include <X11/X.h>
#include <X11/Xlib.h>

#include <list>
#include <vector>
#include <map>

#include "uim/uim.h"
#include "compose.h"


// preedit ornament
#define PE_NORMAL 0
#define PE_REVERSE 1
#define PE_UNDERLINE 2
#define PE_HILIGHT 4

typedef wchar_t uchar;
typedef std::list<uchar> uString;
struct pe_ustring {
    uString s;
    int stat;
};
typedef enum {
    Caret,
    Left,
    Right
} CandWinPosType;

// state of preedit.
// created in the constructor of InputContext, and deleted in the
// destructor of it.
class pe_stat {
public:
    pe_stat(class InputContext *);
    void clear();
    void new_segment(int s);
    void push_uchar(uchar);
    int get_char_count();
    int caret_pos;
    std::list<pe_ustring> ustrings; // separated with segments
    class InputContext *cont;
};

void print_ustring(uString *s);
void erase_ustring(uString *s);
void append_ustring(uString *d, uString *s);

// user interfaces
void init_convdisp();
void init_modifier_keys();
#if HAVE_XFT_UTF8_STRING
void init_default_xftfont();
void update_default_xftfont();
#endif
void reload_uim(int x);
void check_candwin_pos_type();


// for command line option
// trace comunication between client
#define OPT_TRACE 1
// trace XIM connection
#define OPT_TRACE_XIM 2
// use on-demand-synchronous XIM event flow (not safe for Tcl/Tk 8.{3,4})
#define OPT_ON_DEMAND_SYNC 4


// byte order
#define BYTEORDER_UNKNOWN 0
#define LSB_FIRST 1
#define MSB_FIRST 2


// 
extern int host_byte_order;
extern int g_option_mask;
extern int scr_width, scr_height;

#define DO_NOTHING 0
#define COMMIT_RAW 1
#define UPDATE_MODE 2

// do convert from original event into uim event
class keyState {
public:
    keyState(class XimIC *);
    ~keyState();
    void check_key(class keyEventX *);
    bool check_compose();
    int key();
    int modifier();
    KeySym xkeysym();
    int xkeystate();
    void reset();

    bool is_push(); // for distinguish from release
    void print();
private:
    int revise_mod(int uim_mod);
    int mKey;
    int mModifier;
    KeySym mXKeySym;
    int mXKeyState;
    bool m_bPush;
    bool mAltOn;
    bool mMetaOn;
    bool mHyperOn;
    bool mSuperOn;
    Compose *mCompose;
    XimIC *mIc;
};

class XimIC;
class Convdisp;
class XimServer;
class InputContext {
public:
    InputContext(XimServer *, XimIC *, const char *);
    ~InputContext();
    uim_context getUC();
    int pushKey(keyState *e);
    void clear();
    void setConvdisp(Convdisp *);
    void focusIn();
    void focusOut();
    XimIC *get_ic();
    XimServer *getServer();
    void extra_input(char *s);
    void clear_preedit();
    uString get_preedit_string();
    void pushback_preedit_string(int attr, const char *str);
    void update_preedit();
    void candidate_activate(int nr, int display_limit);
    void candidate_select(int index);
    void candidate_shift_page(int direction);
    void candidate_deactivate();
    void candidate_update();
    void update_prop_list(const char *str);
    void update_prop_label(const char *str);
    bool hasActiveCandwin();
    bool isCaretStateShown();
    const char *get_engine_name();
    const char *get_locale_name();
    void changeContext(const char *engine);
    void customContext(const char *custom, const char *val);
    void createUimContext(const char *engine);
public:
    static void commit_cb(void *, const char *);
    static void clear_cb(void *);
    static void pushback_cb(void *, int attr, const char *str);
    static void update_cb(void *);
    static void candidate_activate_cb(void *ptr, int nr, int index);
    static void candidate_select_cb(void *ptr, int index);
    static void candidate_shift_page_cb(void *ptr, int direction);
    static void candidate_deactivate_cb(void *ptr);
    static void update_prop_list_cb(void *ptr, const char *str);
    static void update_prop_label_cb(void *ptr, const char *str);
    static InputContext *focusedContext();
    static void deletefocusedContext();
private:
    void commit_string(char *);
    void clear_pe_stat();

    XimIC *mXic;
    XimServer *mServer;
    pe_stat *m_pe;
    Convdisp *mConvdisp;
    uim_context mUc;
    bool mCandwinActive;
    int mDisplayLimit;
    int mNumPage;
    int current_cand_selection;
    int current_page;
    std::vector<const char *> active_candidates;
    char *mEngineName;
    char *mLocaleName;
    bool mCaretStateShown;
private:
    static InputContext *mFocusedContext;
};

class Locale {
public:
    virtual ~Locale();
    virtual char *uStringToCtext(uString *us) = 0;
    virtual char *utf8_to_native_str(char *str) = 0;
    virtual bool supportOverTheSpot();
};

Locale *createLocale(const char *encoding);
const char *compose_localenames_from_im_lang(const char *im_lang);
bool is_locale_included(const char *locales, const char *locale);
// Sring returned by get_first_locale() is allocated with strdup().
char *get_prefered_locale(const char *locales);
const char *find_localename_from_encoding(const char *encoding);
int utf8_mbtowc(uchar *wc, const unsigned char *src, int src_len);
int utf8_wctomb(unsigned char *dest, uchar wc); // dest must have size 6

class XimServer {
public:
    XimServer(const char *name, const char *lang);
    ~XimServer() {};
    InputContext *createContext(XimIC *, const char *engine);
    void deleteContext(InputContext *);

    bool setupConnection(bool useDefaultIM);
    void setupNewConnection(XClientMessageEvent *ev);
    char *uStringToCtext(uString *js, const char *encoding);
    void strToUstring(uString *d, const char *s);
    const char *getIMName();
    const char *getIMLang();
    void set_im(const char *name);
    void changeContext(const char *engine);
    void customContext(const char *custom, const char *val);
    void reloadConfigs();
    std::list<InputContext *> ic_list;
public:
    static XimServer *findServer(Window w);
    static Display *gDpy;
    static std::map<Window, XimServer *> gServerMap;
    static CandWinPosType gCandWinPosType;
private:
    Window mSelectionWin;
    Atom mServerAtom;
    char *mIMName;
    const char *mIMLang;
};

struct UIMInfo {
    char *lang;
    char *name;
    char *desc;
};
extern std::list<UIMInfo> uim_info;

const char *get_im_lang_from_engine(const char *engine);

#endif
/*
 * Local variables:
 *  c-indent-level: 4
 *  c-basic-offset: 4
 * End:
 */
