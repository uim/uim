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

// classes for preedit draw

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/extensions/shape.h>
#if HAVE_XFT_UTF8_STRING
#include <X11/Xft/Xft.h>
#endif
#include <clocale>
#include <cstdlib>
#include "xim.h"
#include "ximserver.h"
#include "convdisp.h"
#include "canddisp.h"
#include "xdispatch.h"
#include "util.h"

#include "uim/uim-scm.h"

#define UNDERLINE_HEIGHT	2
#define DEFAULT_FONT_SIZE	16
// Temporal hack for flashplayer plugin's broken over-the-spot XIM style
#define FLASHPLAYER7_WORKAROUND
#define FLASHPLAYER9_WORKAROUND

//
// PeWin:     Base class for preedit window
// PeLineWin: Child class for root window style preedit window
// PeOvWin:   Child class for over the spot style preedit window

#ifdef FLASHPLAYER7_WORKAROUND
static Window getTopWindow(Display *, Window);
#endif

const char *fontset_zhCN = "-sony-fixed-medium-r-normal--16-*-*-*-c-80-iso8859-1, -isas-fangsong ti-medium-r-normal--16-160-72-72-c-160-gb2312.1980-0";
const char *fontset_zhTW = "-sony-fixed-medium-r-normal--16-*-*-*-c-80-iso8859-1, -taipei-fixed-medium-r-normal--16-150-75-75-c-160-big5-0";
const char *fontset_ja = "-sony-fixed-medium-r-normal--16-*-*-*-c-80-iso8859-1, -jis-fixed-medium-r-normal--16-*-75-75-c-160-jisx0208.1983-0, -sony-fixed-medium-r-normal--16-*-*-*-c-80-jisx0201.1976-0";
const char *fontset_ko = "-sony-fixed-medium-r-normal--16-*-*-*-c-80-iso8859-1, -daewoo-gothic-medium-r-normal--16-120-100-100-c-160-ksc5601.1987-0";


#if HAVE_XFT_UTF8_STRING
XftFont *gXftFont;
char *gXftFontName;
char *gXftFontLocale;

void
init_default_xftfont() {
    char *fontname = uim_scm_symbol_value_str("uim-xim-xft-font-name");
    gXftFontName = fontname;

    gXftFont = XftFontOpen(XimServer::gDpy, DefaultScreen(XimServer::gDpy),
		    XFT_FAMILY, XftTypeString, fontname,
		    XFT_PIXEL_SIZE, XftTypeDouble, (double)DEFAULT_FONT_SIZE,
		    NULL);
    gXftFontLocale = strdup(setlocale(LC_CTYPE, NULL));
    // maybe not needed, but in case it return NULL...
    if (!gXftFont) {
	gXftFont = XftFontOpen(XimServer::gDpy, DefaultScreen(XimServer::gDpy),
			XFT_FAMILY, XftTypeString, "Sans",
			XFT_PIXEL_SIZE, XftTypeDouble, (double)DEFAULT_FONT_SIZE,
			NULL);
    }
}

void
update_default_xftfont() {
    char *fontname;
    
    if (!uim_scm_symbol_value_bool("uim-xim-use-xft-font?"))
      return;

    fontname = uim_scm_symbol_value_str("uim-xim-xft-font-name");
    
    if (fontname) {
	XftFont *xftfont = XftFontOpen(XimServer::gDpy,
			DefaultScreen(XimServer::gDpy),
			XFT_FAMILY, XftTypeString, fontname,
			XFT_PIXEL_SIZE, XftTypeDouble, (double)DEFAULT_FONT_SIZE,
			NULL);
	if (xftfont) {
	    if (gXftFont)
		XftFontClose(XimServer::gDpy, gXftFont);
	    free(gXftFontName);
	    free(gXftFontLocale);
	    gXftFont = xftfont;
	    gXftFontName = fontname;
	    gXftFontLocale = strdup(setlocale(LC_CTYPE, NULL));
	} else {
	    free(fontname);
	}
    }
}
#endif

static XFontSet
create_default_fontset(const char *im_lang, const char *locale) {
    char *orig_locale;
    const char *name;
    XFontSet ret;

    orig_locale = strdup(setlocale(LC_CTYPE, NULL));

    if (strcmp(locale, orig_locale))
	setlocale(LC_CTYPE, locale);

    if (!strcmp(im_lang, "ja"))
	name = fontset_ja;
    else if (!strcmp(im_lang, "ko"))
	name = fontset_ko;
    else if (!strcmp(im_lang, "zh_CN"))
	name = fontset_zhCN;
    else if (!strcmp(im_lang, "zh_TW:zh_HK"))
	name = fontset_zhTW;
    else
	name = fontset_ja; // XXX fallback fontset

    ret = get_font_set(name, locale);

    if (strcmp(locale, orig_locale))
	setlocale(LC_CTYPE, orig_locale);
    free(orig_locale);

    return ret;
}

static XFontSet
choose_default_fontset(const char *im_lang, const char *locale) {
    return create_default_fontset(im_lang, locale);
}

struct char_ent {
    uchar c;
    int stat;
    int width;
    int height;
    int x, y;
};

// Preedit Window
class PeWin : public WindowIf {
public:
    PeWin(Window pw, const char *im_lang, const char *encoding, const char *locale, Convdisp *cd);
    virtual ~PeWin();

    virtual void release();
    
    virtual void destroy(Window w);
    virtual void expose(Window w);

    void draw_char(int x, int y, uchar ch, int stat);
    void set_back(unsigned long p);
    void set_fore(unsigned long p);
#if HAVE_XFT_UTF8_STRING
    void set_xftfont(const char *xfld);
#endif
    void set_fontset(XFontSet f);
    
    virtual void set_size(int w, int h);
    void set_pos(int x, int y);

    void do_map();

    void clear();
    void draw();
    void unmap();

#if HAVE_XFT_UTF8_STRING
    XftFont *mXftFont;
    int mXftFontSize;
#endif
protected:
#if HAVE_XFT_UTF8_STRING
    int get_fontsize(const char *xfld);
#endif
    Window mParentWin;
    Window mWin;
    Pixmap mPixmap;
    GC mGC, mClearGC;
    unsigned int mFore, mBack;
#if HAVE_XFT_UTF8_STRING
    XftDraw *mXftDraw;
    XftColor mXftColorFg;
    XftColor mXftColorFgRev;
#endif
    XFontSet mFontset;
    const char *mEncoding;
    int mWidth, mHeight;
    bool mIsMapped;
    Convdisp *mConvdisp;
};

// one line preedit window for RootWindowStyle
class PeLineWin : public PeWin {
public:
    PeLineWin(Window w, const char *im_lang, const char *encoding, const char *locale, Convdisp *cd);
    virtual ~PeLineWin();

    void draw_pe(pe_stat *p);
    int mCandWinXOff;

private:
    void calc_extent(pe_stat *p);
    int calc_segment_extent(pe_ustring *s);
    void draw_segment(pe_ustring *s);
    void draw_cursor();
    int get_char_width(uchar ch);

    int m_x;
    int mCharPos;
    int mCursorX;
};

// window for over the spot style
class PeOvWin : public PeWin {
public:
    PeOvWin(Window w, const char *im_lang, const char *encoding, const char *locale, Convdisp *cd);
    
    void draw_ce(char_ent *ce, int len);
    virtual void set_size(int w, int h);
    virtual ~PeOvWin();
private:
    void draw_a_ce(char_ent *ce);
    void draw_cursor(char_ent *ce);
    Pixmap m_mask_pix;
    GC m_mask_pix_gc;
};

class ConvdispOv : public Convdisp {
public:
    ConvdispOv(InputContext *, icxatr *);
    virtual ~ConvdispOv();
    virtual void update_preedit();
    virtual void clear_preedit();
    virtual void update_icxatr();
    virtual void move_candwin();
    virtual bool use_xft();
private:
    bool check_win();
    bool check_atr();
    void layoutCharEnt();
    void make_ce_array();
    void draw_preedit();
    void do_draw_preedit();
#ifdef FLASHPLAYER7_WORKAROUND
    int get_ce_font_height(char_ent *ce, int len);
    int revised_spot_y;
#endif

    void validate_area();

    char_ent *m_ce;
    int m_ce_len;
    int m_candwin_x_off;
    int m_candwin_y_off;
    PeOvWin *m_ov_win;
};

// Preedit window for RootWindowStyle
class ConvdispRw : public Convdisp {
public:
    ConvdispRw(InputContext *, icxatr *);
    virtual ~ConvdispRw();

    virtual void update_preedit();
    virtual void clear_preedit();
    virtual void update_icxatr();
    virtual void move_candwin();
    virtual bool use_xft();
private:
    PeLineWin *mPeWin;
};

class ConvdispOs : public Convdisp {
public:
    ConvdispOs(InputContext *, icxatr *, Connection *);
    virtual ~ConvdispOs();
    virtual void update_preedit();
    virtual void clear_preedit();
    virtual void update_icxatr();
    virtual void move_candwin();
    virtual bool use_xft();

private:
    void compose_preedit_array(TxPacket *);
    void compose_feedback_array(TxPacket *);
  
    Connection *mConn;
    C16 mImid, mIcid;
    int mPrevLen;
};

Convdisp *create_convdisp(int style, InputContext *k,
			  icxatr *a, Connection *c)
{
    switch (style) {
    case IS_ROOT_WINDOW:
	return new ConvdispRw(k, a);
    case IS_OVER_THE_SPOT:
	return new ConvdispOv(k, a);
    case IS_ON_THE_SPOT:
	return new ConvdispOs(k, a, c);
    default:
	break;
    }
    return 0;
}

//
// PeWin(PreEdit Window)
//
PeWin::PeWin(Window pw, const char *im_lang, const char *encoding, const char *locale, Convdisp *cd)
{
    mParentWin = pw;
    mConvdisp = cd;
    int scr_num = DefaultScreen(XimServer::gDpy);
    // tentative
    mWidth = 1;
    mHeight = 1;
    mWin = XCreateSimpleWindow(XimServer::gDpy, mParentWin,
			       0, 0, mWidth, mHeight, 0,
			       BlackPixel(XimServer::gDpy, scr_num),
			       WhitePixel(XimServer::gDpy, scr_num));
    XSetWindowAttributes attr;
    attr.override_redirect = True;
    XChangeWindowAttributes(XimServer::gDpy, mWin, CWOverrideRedirect,
			    &attr);
    mPixmap = XCreatePixmap(XimServer::gDpy, DefaultRootWindow(XimServer::gDpy),
			    mWidth, mHeight,
			    DefaultDepth(XimServer::gDpy, scr_num));
    
    mGC = XCreateGC(XimServer::gDpy, mPixmap, 0, 0);
    mClearGC = XCreateGC(XimServer::gDpy, mPixmap, 0, 0);
    
    XSetBackground(XimServer::gDpy, mGC, WhitePixel(XimServer::gDpy, scr_num));
    XSetForeground(XimServer::gDpy, mGC, BlackPixel(XimServer::gDpy, scr_num));
    XSetForeground(XimServer::gDpy, mClearGC, WhitePixel(XimServer::gDpy, scr_num));
    XSetBackground(XimServer::gDpy, mClearGC, BlackPixel(XimServer::gDpy, scr_num));

    add_window_watch(mWin, this, EXPOSE_MASK|STRUCTURE_NOTIFY_MASK);
    mIsMapped = false; //not mapped now
    
    if (mConvdisp->use_xft() == true) {
#if HAVE_XFT_UTF8_STRING
	mXftFontSize = DEFAULT_FONT_SIZE;
	if (!gXftFont)
	    init_default_xftfont();
	if (!strcmp(gXftFontLocale, locale)) {
	    mXftFont = gXftFont;
	} else {
	    mXftFont = XftFontOpen(XimServer::gDpy,
			    DefaultScreen(XimServer::gDpy),
			    XFT_FAMILY, XftTypeString, gXftFontName,
			    XFT_PIXEL_SIZE, XftTypeDouble, (double)mXftFontSize,
			    NULL);
	}
	mXftDraw = XftDrawCreate(XimServer::gDpy, mPixmap,
			DefaultVisual(XimServer::gDpy, scr_num),
			DefaultColormap(XimServer::gDpy, scr_num));
	XColor dummyc, fg;
	XAllocNamedColor(XimServer::gDpy, DefaultColormap(XimServer::gDpy, scr_num),"black", &fg, &dummyc);
	mXftColorFg.color.red = dummyc.red;
	mXftColorFg.color.green = dummyc.green;
	mXftColorFg.color.blue = dummyc.blue;
	mXftColorFg.color.alpha = 0xffff;
	mXftColorFg.pixel = fg.pixel;

	XAllocNamedColor(XimServer::gDpy, DefaultColormap(XimServer::gDpy, scr_num),"white", &fg, &dummyc);
	mXftColorFgRev.color.red = dummyc.red;
	mXftColorFgRev.color.green = dummyc.green;
	mXftColorFgRev.color.blue = dummyc.blue;
	mXftColorFgRev.color.alpha = 0xffff;
	mXftColorFgRev.pixel = fg.pixel;
#endif
    } else {
	mFontset = choose_default_fontset(im_lang, locale);
    }

    mEncoding = encoding;
    
    XFlush(XimServer::gDpy);
}

PeWin::~PeWin()
{
    if (mWin)
	release();

    XFreePixmap(XimServer::gDpy, mPixmap);
    
    XFreeGC(XimServer::gDpy, mGC);
    XFreeGC(XimServer::gDpy, mClearGC);
#if HAVE_XFT_UTF8_STRING 
    if (mConvdisp->use_xft() == true) {
	XftDrawDestroy(mXftDraw);
	if (mXftFont != gXftFont)
	    XftFontClose(XimServer::gDpy, mXftFont);
    }
#endif

    XFlush(XimServer::gDpy);
}

void PeWin::release()
{
    remove_window_watch(mWin);
    XDestroyWindow(XimServer::gDpy, mWin);
    mWin = 0;
}

void PeWin::destroy(Window w)
{
    if (mWin && mWin == w)
	mWin = 0;
}

void PeWin::expose(Window w)
{
    XCopyArea(XimServer::gDpy, mPixmap, w, mGC,
	      0, 0, mWidth, mHeight, 0, 0);

    XFlush(XimServer::gDpy);
}

void PeWin::draw_char(int x, int y, uchar ch, int stat)
{
    GC gc = mGC;
    if (stat & PE_REVERSE)
	gc = mClearGC;

    char utf8[6];
    int len = utf8_wctomb((unsigned char *)utf8, ch);
    utf8[len] = '\0';

    if (mConvdisp->use_xft() == true) {
#ifdef HAVE_XFT_UTF8_STRING
	XGlyphInfo ginfo;
	XftTextExtentsUtf8(XimServer::gDpy, mXftFont, (unsigned char *)utf8, len, &ginfo);
	if (stat & PE_REVERSE) {
	    XftDrawRect(mXftDraw, &mXftColorFg, x, y - (mXftFontSize - 2), ginfo.xOff, mXftFontSize);
	    XftDrawStringUtf8(mXftDraw, &mXftColorFgRev, mXftFont, x, y, (unsigned char *)utf8, len);
	} else {
	    XftDrawStringUtf8(mXftDraw, &mXftColorFg, mXftFont, x, y, (unsigned char *)utf8, len);
	}
#endif
    } else {
	if (!strcmp(mEncoding, "UTF-8")) {
    	    XwcDrawImageString(XimServer::gDpy, mPixmap, mFontset,
			gc, x, y, &ch, 1);
	} else {
	    char *native_str;
	    XimIM *im = get_im_by_id(mConvdisp->get_context()->get_ic()->get_imid());
	
	    native_str = im->utf8_to_native_str(utf8);
	    if (!native_str)
		return;
	    len = static_cast<int>(strlen(native_str));
	    XmbDrawImageString(XimServer::gDpy, mPixmap, mFontset,
			   gc, x, y, native_str, len);
	    free(native_str);
	}
    }
}

void PeWin::set_back(unsigned long p)
{
    mBack = static_cast<unsigned int>(p);
    XSetBackground(XimServer::gDpy, mGC, p);
    XSetForeground(XimServer::gDpy, mClearGC, p);
#if HAVE_XFT_UTF8_STRING
    if (mConvdisp->use_xft() == true) {
	XColor xcolor;
	xcolor.pixel = p;
	XQueryColor(XimServer::gDpy, DefaultColormap(XimServer::gDpy, DefaultScreen(XimServer::gDpy)), &xcolor);
	mXftColorFgRev.pixel = p;
	mXftColorFgRev.color.red = xcolor.red;
	mXftColorFgRev.color.green = xcolor.green;
	mXftColorFgRev.color.blue = xcolor.blue;
	mXftColorFgRev.color.alpha = 0xffff;
    }
#endif
}

void PeWin::set_fore(unsigned long p)
{
    mFore = static_cast<unsigned int>(p);
    XSetForeground(XimServer::gDpy, mGC, p);
    XSetBackground(XimServer::gDpy, mClearGC, p);
#if HAVE_XFT_UTF8_STRING
    if (mConvdisp->use_xft() == true) {
	XColor xcolor;
	xcolor.pixel = p;
	XQueryColor(XimServer::gDpy, DefaultColormap(XimServer::gDpy, DefaultScreen(XimServer::gDpy)), &xcolor);
	mXftColorFg.pixel = p;
	mXftColorFg.color.red = xcolor.red;
	mXftColorFg.color.green = xcolor.green;
	mXftColorFg.color.blue = xcolor.blue;
	mXftColorFg.color.alpha = 0xffff;
    }
#endif
}

void PeWin::set_fontset(XFontSet f)
{
    if (f)
	mFontset = f;
}

#if HAVE_XFT_UTF8_STRING
void PeWin::set_xftfont(const char *xfld)
{
	int size = get_fontsize(xfld);
	const char *locale = mConvdisp->get_locale_name();

	if (!gXftFont)
	    init_default_xftfont();
	if (size != -1 && (mXftFontSize != size || strcmp(locale, gXftFontLocale))) {
	    if (mXftFont != gXftFont)
		XftFontClose(XimServer::gDpy, mXftFont);

	    mXftFont = XftFontOpen(XimServer::gDpy,
			    DefaultScreen(XimServer::gDpy),
			    XFT_FAMILY, XftTypeString, gXftFontName,
			    XFT_PIXEL_SIZE, XftTypeDouble, (double)size,
			    NULL);
	    mXftFontSize = size;
	}
}

int PeWin::get_fontsize(const char *xfld)
{
#define MAX_DIGIT_OF_PIXEL_SIZE	3
    int size;
    char str[MAX_DIGIT_OF_PIXEL_SIZE + 1];
    const char *p = xfld;
    int count = 0;
    int i, j = 0;

    for (i = 0; i < (int)strlen(xfld); i++) {
	if (p[i] == '-')
	    count++;
	if (count == 7) {
	    i++;
	    while (p[i] != '-' && p[i] != '\0') {
		str[j] = p[i];
		i++;
		j++;
		if (j > MAX_DIGIT_OF_PIXEL_SIZE)
		    return -1;
	    }
	    str[j] = '\0';
	    break;
	}
    }
    if (!sscanf(str, "%d", &size))
	return -1;
    return size;
}
#endif

void PeWin::set_size(int w, int h)
{
    if (w == mWidth && h == mHeight)
	return;

    XResizeWindow(XimServer::gDpy, mWin, w, h);
    XFreePixmap(XimServer::gDpy, mPixmap);
    mPixmap = XCreatePixmap(XimServer::gDpy, DefaultRootWindow(XimServer::gDpy), w, h,
			    DefaultDepth(XimServer::gDpy, DefaultScreen(XimServer::gDpy)));
#if HAVE_XFT_UTF8_STRING 
    if (mConvdisp->use_xft() == true)
	XftDrawChange(mXftDraw, mPixmap);
#endif
    mWidth = w;
    mHeight = h;
    clear();
}

void PeWin::set_pos(int x, int y)
{
    XWindowChanges ch;
    ch.x = x;
    ch.y = y;
    XConfigureWindow(XimServer::gDpy, mWin, CWX|CWY, &ch);
}

void PeWin::do_map()
{
    if (!mIsMapped) {
	XFlush(XimServer::gDpy);
	XMapRaised(XimServer::gDpy, mWin);
	mIsMapped = true;
    }
}

void PeWin::clear()
{
    XFillRectangle(XimServer::gDpy, mPixmap, mClearGC,
		   0, 0, mWidth, mHeight);
}

void PeWin::draw()
{
    if (!mIsMapped)
	do_map();
    else
	expose(mWin);
}

void PeWin::unmap()
{
    if (mIsMapped) {
	XUnmapWindow(XimServer::gDpy, mWin);
	XFlush(XimServer::gDpy);
	mIsMapped = false;
    }
}

//
// PeLineWin
//
#define PE_LINE_WIN_WIDTH	400
#define PE_LINE_WIN_HEIGHT	28
#define PE_LINE_WIN_FONT_POS_Y	20
#define PE_LINE_WIN_MARGIN_X	2

PeLineWin::PeLineWin(Window w, const char *im_lang, const char *encoding, const char *locale, Convdisp *cd) : PeWin(w, im_lang, encoding, locale, cd)
{
    set_size(PE_LINE_WIN_WIDTH, PE_LINE_WIN_HEIGHT);
    clear();
}

PeLineWin::~PeLineWin()
{
}

void PeLineWin::draw_pe(pe_stat *p)
{
    clear();
    calc_extent(p);
    m_x = PE_LINE_WIN_MARGIN_X;
    mCursorX = m_x;
    mCharPos = 0;
    std::list<pe_ustring>::iterator i;
    for (i = p->ustrings.begin(); i != p->ustrings.end(); ++i) {
	draw_segment(&(*i));
    }
    draw_cursor();
}

void PeLineWin::draw_cursor()
{
    XDrawLine(XimServer::gDpy, mPixmap, mGC,
		    mCursorX,
		    (PE_LINE_WIN_HEIGHT - PE_LINE_WIN_FONT_POS_Y) / 2 + 1,
		    mCursorX,
		    PE_LINE_WIN_FONT_POS_Y + 1);
}

int PeLineWin::get_char_width(uchar ch)
{
    int width = 0;
    char utf8[6];

    int len = utf8_wctomb((unsigned char *)utf8, ch);
    utf8[len] = '\0';

    if (mConvdisp->use_xft() == true) {
#ifdef HAVE_XFT_UTF8_STRING
	XGlyphInfo ginfo;
	XftTextExtentsUtf8(XimServer::gDpy, mXftFont, (unsigned char *)utf8,
			len, &ginfo);
	width = ginfo.xOff;
#endif
    } else {
	XRectangle ink, logical;

	if (!strcmp(mEncoding, "UTF-8")) {
	    XwcTextExtents(mFontset, &ch, 1, &ink, &logical);
	} else {
	    char *native_str;
	    XimIM *im = get_im_by_id(mConvdisp->get_context()->get_ic()->get_imid());

	    native_str = im->utf8_to_native_str(utf8);
	    if (!native_str)
		return 0;
	    len = static_cast<int>(strlen(native_str));
	    XmbTextExtents(mFontset, native_str, len, &ink, &logical);
	    free(native_str);
	}
	width = logical.width;
    }

    return width;
}

void PeLineWin::draw_segment(pe_ustring *s)
{
    uString::iterator i;
    int caret_pos = mConvdisp->get_caret_pos();

    for (i = s->s.begin(); i != s->s.end(); ++i) {
	uchar ch = *i;
	int width = get_char_width(ch);
	draw_char(m_x, PE_LINE_WIN_FONT_POS_Y, ch, s->stat);
	mCharPos++;

	if (s->stat & PE_UNDERLINE) {
	    XDrawLine(XimServer::gDpy, mPixmap, mGC,
			    m_x, PE_LINE_WIN_FONT_POS_Y + UNDERLINE_HEIGHT,
			    m_x + width, PE_LINE_WIN_FONT_POS_Y + UNDERLINE_HEIGHT);
	}
	m_x += width;
	if (mCharPos == caret_pos)
	    mCursorX= m_x;
    }

    switch (XimServer::gCandWinPosType) {
    case Caret:
	mCandWinXOff = mCursorX;
	break;
    case Right:
	mCandWinXOff = m_x;
	break;
    case Left:
    default:
	mCandWinXOff = 0;
	break;
    }
}

int PeLineWin::calc_segment_extent(pe_ustring *s)
{
    int width = 0;
    uString::iterator i;

    for (i = s->s.begin(); i != s->s.end(); ++i) {
	uchar ch = *i;
	width += get_char_width(ch);
    }
    return width;
}

void PeLineWin::calc_extent(pe_stat *p)
{
    int width = 0;
    std::list<pe_ustring>::iterator i;

    for (i = p->ustrings.begin(); i != p->ustrings.end(); ++i)
	width += calc_segment_extent(&(*i));	

    if (width < PE_LINE_WIN_WIDTH)
	set_size(PE_LINE_WIN_WIDTH, PE_LINE_WIN_HEIGHT);
    else
	set_size(width + PE_LINE_WIN_MARGIN_X * 2, PE_LINE_WIN_HEIGHT);
}


//
// PeOvWin
//
PeOvWin::PeOvWin(Window w, const char *im_lang, const char *encoding, const char *locale, Convdisp *cd) : PeWin(w, im_lang, encoding, locale, cd)
{
    m_mask_pix = 0;
    m_mask_pix_gc = 0;
}

PeOvWin::~PeOvWin()
{
    if (m_mask_pix) {
	XFreePixmap(XimServer::gDpy, m_mask_pix);
	XFreeGC(XimServer::gDpy, m_mask_pix_gc);
    }
}

void PeOvWin::set_size(int w, int h)
{
    if (w == mWidth && h == mHeight)
	return;

    PeWin::set_size(w, h);
    if (m_mask_pix) {
	XFreePixmap(XimServer::gDpy, m_mask_pix);
	m_mask_pix = XCreatePixmap(XimServer::gDpy, mWin,
				   mWidth, mHeight, 1);
    }
}

void PeOvWin::draw_ce(char_ent *ce, int len)
{
    if (m_mask_pix == 0) {
	m_mask_pix = XCreatePixmap(XimServer::gDpy, mWin, mWidth, mHeight, 1);
	m_mask_pix_gc = XCreateGC(XimServer::gDpy, m_mask_pix, 0, 0);
    }


    clear();
    XSetForeground(XimServer::gDpy, m_mask_pix_gc,
		   BlackPixel(XimServer::gDpy,
			      DefaultScreen(XimServer::gDpy)));
    XFillRectangle(XimServer::gDpy, m_mask_pix,
		   m_mask_pix_gc, 0, 0, mWidth, mHeight);
    XSetForeground(XimServer::gDpy, m_mask_pix_gc,
		   WhitePixel(XimServer::gDpy,
			      DefaultScreen(XimServer::gDpy)));
    int i;
    for (i = 0; i < len; i++) {
	draw_a_ce(&ce[i]);
    }
    draw_cursor(ce);
    XShapeCombineMask(XimServer::gDpy, mWin, ShapeBounding,
		      0, 0, m_mask_pix, ShapeSet);
    do_map();
}

#define CURSOR_WIDTH	1
void PeOvWin::draw_a_ce(char_ent *ce)
{
    draw_char(ce->x, ce->y, ce->c, ce->stat);

    XFillRectangle(XimServer::gDpy, m_mask_pix, m_mask_pix_gc,
		   ce->x, ce->y - ce->height + 2,
		   ce->width + CURSOR_WIDTH, ce->height + UNDERLINE_HEIGHT - 1);
    if (ce->stat & PE_UNDERLINE) {
	XDrawLine(XimServer::gDpy, mPixmap, mGC,
		  ce->x, ce->y + UNDERLINE_HEIGHT,
		  ce->x + ce->width, ce->y + UNDERLINE_HEIGHT);
    }
}

void PeOvWin::draw_cursor(char_ent *ce)
{
    int x;
    int caret_pos = mConvdisp->get_caret_pos();
    char_ent *caret_ce;

    if (caret_pos == 0) {
	caret_ce = &ce[caret_pos];
	x = caret_ce->x;
    } else {
	caret_ce = &ce[caret_pos - 1];
	x = caret_ce->x + caret_ce->width;
    }

    XDrawLine(XimServer::gDpy, mPixmap, mGC,
		    x, caret_ce->y - caret_ce->height,
		    x, caret_ce->y);
}

//
//
//
Convdisp::Convdisp(InputContext *k, icxatr *a)
{
    mKkContext = k;
    m_atr = a;
    mIMLang = get_im_lang_from_engine(k->get_engine_name());
    mEncoding = k->get_ic()->get_encoding();
    mLocaleName = k->get_locale_name();
}

Convdisp::~Convdisp()
{
}

void Convdisp::set_im_lang(const char *im_lang)
{
    mIMLang = im_lang;
}

void Convdisp::set_locale_name(const char *locale)
{
    mLocaleName = locale;
}

const char *Convdisp::get_locale_name()
{
    return mLocaleName;
}

void Convdisp::set_pe(pe_stat *p)
{
    m_pe = p;
}

uString Convdisp::get_pe()
{
    uString s;
    std::list<pe_ustring>::iterator it;
    for (it = m_pe->ustrings.begin(); it != m_pe->ustrings.end(); ++it) {
	append_ustring(&s, &(*it).s);
    }
    return s;
}

void Convdisp::set_focus()
{
    Canddisp *disp = canddisp_singleton();
    disp->show();
}

void Convdisp::unset_focus()
{
    Canddisp *disp = canddisp_singleton();
    disp->hide();
}

InputContext *Convdisp::get_context()
{
    return mKkContext;
}

int Convdisp::get_caret_pos()
{
    if (!m_pe)
	return 0;
    return m_pe->caret_pos;
}

void Convdisp::update_caret_state()
{
    if (!uim_scm_symbol_value_bool("bridge-show-input-state?"))
	return;

    Canddisp *disp = canddisp_singleton();
    InputContext *focusedContext = InputContext::focusedContext();

    if (focusedContext && focusedContext == mKkContext) {
	if (mKkContext->isCaretStateShown())
	    disp->update_caret_state();
	else
	    disp->hide_caret_state();
    }
}

// Root window style
ConvdispRw::ConvdispRw(InputContext *k, icxatr *a) : Convdisp(k, a)
{
    mPeWin = NULL;
}

ConvdispRw::~ConvdispRw()
{
    if (mPeWin)
	delete mPeWin;
}

void ConvdispRw::update_preedit()
{
    if (!m_pe)
	return; 

    if (!m_pe->get_char_count()) {
	clear_preedit();
	move_candwin(); // reset candwin position
	update_caret_state();
	return;
    }

    // preedit string exists
    if (!mPeWin)
	mPeWin = new PeLineWin(DefaultRootWindow(XimServer::gDpy), mIMLang, mEncoding, mLocaleName, this);
    
    if (m_atr->has_atr(ICA_ClientWindow)) {
    	int x, y;
	Window win;
	XWindowAttributes xattr;

	XGetWindowAttributes(XimServer::gDpy, m_atr->client_window, &xattr);
	XTranslateCoordinates(XimServer::gDpy, m_atr->client_window, DefaultRootWindow(XimServer::gDpy), 0, 0, &x, &y, &win);
	mPeWin->set_pos(x, y + xattr.height);
    }

    mPeWin->do_map();
    mPeWin->draw_pe(m_pe);
    mPeWin->draw();

    move_candwin();
    update_caret_state();
}

void ConvdispRw::clear_preedit()
{
    delete mPeWin;
    mPeWin = NULL;
}

void ConvdispRw::update_icxatr()
{
}

void ConvdispRw::move_candwin()
{
    InputContext *focusedContext = InputContext::focusedContext();
    if (!focusedContext || focusedContext != mKkContext)
	return;

    if (m_atr->has_atr(ICA_ClientWindow)) {
	int x, y;
	Window win;
	XWindowAttributes xattr;

	XTranslateCoordinates(XimServer::gDpy, m_atr->client_window,
			      DefaultRootWindow(XimServer::gDpy),
			      0, 0, &x, &y, &win);

	Canddisp *disp = canddisp_singleton();

	XGetWindowAttributes(XimServer::gDpy, m_atr->client_window, &xattr);

	if (mPeWin)
	    x += mPeWin->mCandWinXOff;
	disp->move(x, y + xattr.height + 28); // lower-left side under the preedit window
    }
}

bool ConvdispRw::use_xft()
{
    return m_atr->use_xft();
}

// Over the spot style
ConvdispOv::ConvdispOv(InputContext *k, icxatr *a) : Convdisp(k, a)
{
    m_ov_win = 0;
    m_candwin_x_off = 0;
    m_candwin_y_off = 0;
#ifdef FLASHPLAYER7_WORKAROUND
    revised_spot_y = -1;
#endif
}

ConvdispOv::~ConvdispOv()
{
    if (m_ov_win)
	delete m_ov_win;
}

void ConvdispOv::update_preedit()
{
    draw_preedit();
    move_candwin();
    update_caret_state();
}

void ConvdispOv::move_candwin()
{
    InputContext *focusedContext = InputContext::focusedContext();
    if (!focusedContext || focusedContext != mKkContext)
	return;

    if (m_atr->has_atr(ICA_SpotLocation) ) {
	int x = -1, y = -1;
	Window win;

	if (m_atr->has_atr(ICA_ClientWindow)) {

	    XTranslateCoordinates(XimServer::gDpy, m_atr->client_window,
				  DefaultRootWindow(XimServer::gDpy),
				  m_atr->spot_location.x,
				  m_atr->spot_location.y,
				  &x, &y, &win);
	}

	if (m_atr->has_atr(ICA_FocusWindow)) {

	    XTranslateCoordinates(XimServer::gDpy, m_atr->focus_window,
				  DefaultRootWindow(XimServer::gDpy),
				  m_atr->spot_location.x,
				  m_atr->spot_location.y,
				  &x, &y, &win);
	}
#ifdef FLASHPLAYER7_WORKAROUND
	if (revised_spot_y != -1) {
	    XTranslateCoordinates(XimServer::gDpy, m_atr->client_window,
				  DefaultRootWindow(XimServer::gDpy),
				  m_atr->spot_location.x,
				  revised_spot_y,
				  &x, &y, &win);
	}
#endif
	if (x > -1 && y > -1) {
	    x += m_candwin_x_off;
	    y += m_candwin_y_off;

	    Canddisp *disp = canddisp_singleton();
	    disp->move(x, y + UNDERLINE_HEIGHT + 1);
	    m_atr->unset_change_mask(ICA_SpotLocation);
	}
#if 0
    } else if (m_atr->has_atr(ICA_SpotLocation)) {
	int x = -1, y = -1;
	Window win;
	if (revised_spot_y != -1) {
	    XTranslateCoordinates(XimServer::gDpy, m_atr->client_window,
				  DefaultRootWindow(XimServer::gDpy),
				  m_atr->spot_location.x,
				  revised_spot_y,
				  &x, &y, &win);
	    Canddisp *disp = canddisp_singleton();
	    disp->move(x, y + UNDERLINE_HEIGHT + 1);
	}
#endif
    }
}

void ConvdispOv::clear_preedit()
{
    delete m_ov_win;
    m_ov_win = NULL;
    m_candwin_x_off = 0;
    m_candwin_y_off = 0;
}

void ConvdispOv::validate_area()
{
    Window r, win;
    int x;
    unsigned int w, h, tmp;
    if (m_atr->has_atr(ICA_FocusWindow))
	win = m_atr->focus_window;
    else
	win = m_atr->client_window;

    XGetGeometry(XimServer::gDpy, win,
		 &r, &x, &x, &w, &h, &tmp, &tmp);
    // Absurd... (cope with Qt from RedHat7.3, and maybe some other)
    m_atr->area.width = (unsigned short)w;
    m_atr->area.height = (unsigned short)h;
}

void ConvdispOv::update_icxatr()
{

    if (m_atr->is_changed(ICA_SpotLocation)) {
	move_candwin();
	update_caret_state();
    }

    if (!m_ov_win)
	return;
    
    if (m_atr->is_changed(ICA_FocusWindow)) {
	// Some clients send FocusWindow later
	if (!check_win())
	    return;
    }
    
    if (m_atr->is_changed(ICA_Area)) {
	if (m_atr->area.width == 0)
	    validate_area();
#ifdef FLASHPLAYER9_WORKAROUND
	if (m_atr->area.width == 500 && m_atr->area.height == 40) {
	    validate_area();
	    m_atr->area.x = 0;
	    m_atr->area.y = 0;
	}
#endif
	m_ov_win->set_size(m_atr->area.width, m_atr->area.height);
	m_atr->unset_change_mask(ICA_Area);
    }
  
    if (m_atr->is_changed(ICA_Foreground)) {
	m_ov_win->set_fore(m_atr->foreground_pixel);
	m_atr->unset_change_mask(ICA_Foreground);
    }
    if (m_atr->is_changed(ICA_Background)) {
	m_ov_win->set_back(m_atr->background_pixel);
	m_atr->unset_change_mask(ICA_Background);
    }
    if (m_atr->is_changed(ICA_FontSet)) {
	if (use_xft() == true) {
#if HAVE_XFT_UTF8_STRING
	    m_ov_win->set_xftfont(m_atr->font_set_name);
#endif
	} else {
	    m_ov_win->set_fontset(m_atr->font_set);
	}
	m_atr->unset_change_mask(ICA_FontSet);
    }
  
    if (m_atr->is_changed(ICA_SpotLocation))
	move_candwin();

    draw_preedit();
}

void ConvdispOv::draw_preedit()
{
    if (!m_pe)
	return;

    m_ce_len = m_pe->get_char_count();
    if (!m_ce_len) {
	clear_preedit();
	return;
    }

    if (!check_win())
	return;

    m_ce = (char_ent *)malloc(sizeof(char_ent) * m_ce_len);
    make_ce_array();
    layoutCharEnt();
    do_draw_preedit();
    free(m_ce);
    m_ov_win->draw();
    XFlush(XimServer::gDpy);
}

void ConvdispOv::do_draw_preedit()
{
#ifdef FLASHPLAYER7_WORKAROUND
    revised_spot_y = -1;
#endif

    if (m_atr->has_atr(ICA_Area)) {
#ifdef FLASHPLAYER7_WORKAROUND
	// Workaround for brain damaged flash player plugin (at least
	// version 7.0 r25). --ekato
	//
	// Background: flashplayer plugin set same values for
	// m_atr->area.y and m_atr->spot_location.y.
	//
	// 1. If preedit area goes beyond the parent window (browser),
	// set the area at the upper side of client window.
	//
	// 2. If height for preedit is too narrow, make sure to set
	// preedit within the client window.

	if (m_atr->has_atr(ICA_SpotLocation) && (m_atr->area.y == m_atr->spot_location.y)) {
	    XWindowAttributes clientattr, topattr;
	    Window clientwin, topwin, win;
	    int x, y;
	    int font_height;
	
	    topattr.height = 0;
	    if (m_atr->has_atr(ICA_FocusWindow))
		clientwin = m_atr->focus_window;
	    else
		clientwin = m_atr->client_window;

	    XGetWindowAttributes(XimServer::gDpy, clientwin, &clientattr);
	    topwin = getTopWindow(XimServer::gDpy, clientwin);
	    if (topwin)
		XGetWindowAttributes(XimServer::gDpy, topwin, &topattr);
	    XTranslateCoordinates(XimServer::gDpy, clientwin,
			    DefaultRootWindow(XimServer::gDpy),
			    m_atr->area.x,
			    m_atr->area.y,
			    &x, &y, &win);
	    font_height = get_ce_font_height(m_ce, m_ce_len);

	    if (topattr.height) {
		if (y > (topattr.height + topattr.y)) {
		    // preedit area goes beyond the top window's geometry
		    if ((y - m_atr->area.y) < topattr.y) {
			// Set preedit at upper side of the top
			// window.  But it may not work because it
			// lacks height for window manager's title bar
			// and some toolbars of browser...
			m_ov_win->set_pos(m_atr->area.x, topattr.y - (y - m_atr->area.y));
			revised_spot_y = font_height + topattr.y - (y - m_atr->area.y);
		    } else {
			// Set preedit at upper side of the flash
			// player's window.
			m_ov_win->set_pos(m_atr->area.x, 0);
			revised_spot_y = font_height;
		    }
		} else {
		    // preedit area is visible within the browser
		    if ((clientattr.height - m_atr->area.y) < font_height + UNDERLINE_HEIGHT) {
			// Make sure preedit+underline to be fit
			// within the client window.
			m_ov_win->set_pos(m_atr->area.x, clientattr.height - font_height - (UNDERLINE_HEIGHT + 1));
			revised_spot_y = clientattr.height - (UNDERLINE_HEIGHT + 1);
		    } else {
			m_ov_win->set_pos(m_atr->area.x, m_atr->area.y);
			revised_spot_y = font_height;
		    }
		}
	    }
	} else
#endif // FLASHPLAYER7_WORKAROUND
	    m_ov_win->set_pos(m_atr->area.x, m_atr->area.y);
    } else {
	m_ov_win->set_pos(0, 0);
    }
    m_ov_win->draw_ce(m_ce, m_ce_len);
}

bool ConvdispOv::check_win()
{
    if (!check_atr())
	return false; // not enough information to map preedit
  
    if (m_ov_win && !m_atr->is_changed(ICA_FocusWindow))
	return true; // no need to update window

    if (m_ov_win)
	delete m_ov_win;
  
    m_atr->unset_change_mask(ICA_FocusWindow);
    m_atr->unset_change_mask(ICA_Foreground);
    m_atr->unset_change_mask(ICA_Background);
    m_atr->unset_change_mask(ICA_FontSet);

    Window w;
    if (m_atr->has_atr(ICA_FocusWindow))
	w = m_atr->focus_window;
    else
	w = m_atr->client_window;

    m_ov_win = new PeOvWin(w, mIMLang, mEncoding, mLocaleName, this);
    m_ov_win->set_size(m_atr->area.width, m_atr->area.height);
    m_ov_win->set_fore(m_atr->foreground_pixel);
    m_ov_win->set_back(m_atr->background_pixel);
    if (use_xft() == true) {
#if HAVE_XFT_UTF8_STRING
	m_ov_win->set_xftfont(m_atr->font_set_name);
#endif
    } else {
	m_ov_win->set_fontset(m_atr->font_set);
    }
  
    return true;
}

bool ConvdispOv::check_atr()
{
    if (!m_atr->has_atr(ICA_FocusWindow) &&
	!m_atr->has_atr(ICA_ClientWindow))
	return false;

    if (!m_atr->has_atr(ICA_SpotLocation)) {
	// set at top-left corner unless SpotLocation is available
	m_atr->spot_location.x = 0;
	m_atr->spot_location.y = 0;
    }
    if (!m_atr->has_atr(ICA_Area) ||
	m_atr->area.width == 0) {
	validate_area();
	m_atr->area.x = 0;
	m_atr->area.y = 0;
    }
#ifdef FLASHPLAYER9_WORKAROUND
    if (m_atr->has_atr(ICA_Area) &&
	m_atr->area.width == 500 && m_atr->area.height == 40) {
	validate_area();
	m_atr->area.x = 0;
	m_atr->area.y = 0;
    }
#endif
    if (!m_atr->has_atr(ICA_FontSet)) {
	if (use_xft() == false)
	    m_atr->font_set = choose_default_fontset(mIMLang, mLocaleName);
    }
    if (!m_atr->has_atr(ICA_LineSpace)) {
	m_atr->line_space = DEFAULT_FONT_SIZE;
    }

    if (!m_atr->has_atr(ICA_Foreground))
	m_atr->foreground_pixel
		= static_cast<C32>(BlackPixel(XimServer::gDpy,
					      DefaultScreen(XimServer::gDpy)));

    if (!m_atr->has_atr(ICA_Background))
	m_atr->background_pixel
		= static_cast<C32>(WhitePixel(XimServer::gDpy,
					      DefaultScreen(XimServer::gDpy)));

    return true;
}

void ConvdispOv::make_ce_array()
{
    std::list<pe_ustring>::iterator i;
    uString::iterator j;
    int s;
    int c = 0;
    for (i = m_pe->ustrings.begin(); i != m_pe->ustrings.end(); ++i) {
	s = (*i).stat;
	for (j = (*i).s.begin(); j != (*i).s.end(); ++j) {
	    m_ce[c].c = *j;
	    m_ce[c].stat = s;
	    c++;
	}
    }
}

// Dirty,,, 
void ConvdispOv::layoutCharEnt()
{
    int i;
    int x, y;
    int caret_pos = get_caret_pos();
    int right_limit = m_atr->area.width + m_atr->area.x;

    x = m_atr->spot_location.x;
    y = m_atr->spot_location.y;

    for (i = 0; i < m_ce_len; i++) {
	uchar ch = m_ce[i].c;

	char utf8[6];
	int len;
	if (use_xft() == true) {
#if HAVE_XFT_UTF8_STRING
	    len = utf8_wctomb((unsigned char *)utf8, ch);
	    utf8[len] = '\0';

	    XGlyphInfo ginfo;
	    XftTextExtentsUtf8(XimServer::gDpy, m_ov_win->mXftFont, (unsigned char *)utf8, len, &ginfo);
	    m_ce[i].width = ginfo.xOff;
	    m_ce[i].height = m_ov_win->mXftFontSize;
#endif
	} else {
	    XRectangle ink, logical;

	    if (!strcmp(mEncoding, "UTF-8")) {
		XwcTextExtents(m_atr->font_set, &ch, 1, &ink, &logical);
		m_ce[i].width = logical.width;
		m_ce[i].height = logical.height;
	    } else {
		len = utf8_wctomb((unsigned char *)utf8, ch);
		utf8[len] = '\0';
		XimIM *im = get_im_by_id(mKkContext->get_ic()->get_imid());
		char *str = im->utf8_to_native_str(utf8);
		if (!str) {
		    logical.width = 0;
		    logical.height = (unsigned short)((i > 0) ? m_ce[i - 1].height : 0);
		} else {
		    len = static_cast<int>(strlen(str));
		    XmbTextExtents(m_atr->font_set, str, len, &ink, &logical);
		    free(str);
		}
		m_ce[i].width = logical.width;
		m_ce[i].height = logical.height;
	    }
	}

	if (m_ce[i].width + x > right_limit) {
	    // goto next line
	    x = m_atr->area.x;
	    y += (m_atr->line_space + UNDERLINE_HEIGHT);
	}
	m_ce[i].x = x - m_atr->area.x;
	m_ce[i].y = y - m_atr->area.y;
#ifdef FLASHPLAYER7_WORKAROUND
	// workaround for brain damaged flash player plugin
	if (m_ce[i].y == 0)
	    m_ce[i].y += m_ce[i].height;
#endif
	x += m_ce[i].width;
    }

    switch (XimServer::gCandWinPosType) {
    case Caret:
	if (caret_pos == 0) {
	    m_candwin_x_off = m_ce[caret_pos].x - m_atr->spot_location.x;
	    m_candwin_y_off = m_ce[caret_pos].y - m_atr->spot_location.y;
	} else {
	    m_candwin_x_off = m_ce[caret_pos - 1].x + m_ce[caret_pos - 1].width - m_atr->spot_location.x;
	    m_candwin_y_off = m_ce[caret_pos - 1].y - m_atr->spot_location.y;
	}
	break;
    case Right:
	m_candwin_x_off = m_ce[m_ce_len - 1].x + m_ce[m_ce_len - 1].width - m_atr->spot_location.x;
	m_candwin_y_off = m_ce[m_ce_len - 1].y - m_atr->spot_location.y;
	break;
    case Left:
    default:
	break;
    }
}

#ifdef FLASHPLAYER7_WORKAROUND
int ConvdispOv::get_ce_font_height(char_ent *ce, int len)
{
    int i, h = 0;
    for (i = 0; i < len; i++) {
	if (ce[i].height > h)
	    h = ce[i].height;
    }
    return h;
}
#endif

bool ConvdispOv::use_xft()
{
    return m_atr->use_xft();
}

// On the spot style
ConvdispOs::ConvdispOs(InputContext *k, icxatr *a, Connection *c)
    : Convdisp(k, a)
{
    XimIC *ic = k->get_ic();
    mConn = c;
    mImid = ic->get_imid();
    mIcid = ic->get_icid();
    mPrevLen = 0;
}

ConvdispOs::~ConvdispOs()
{
}

void ConvdispOs::update_preedit()
{
    move_candwin();
    if (!m_pe)
	return;

    TxPacket *t;

    int len, caret_pos;
    len = m_pe->get_char_count();
    caret_pos = m_pe->caret_pos;

    if (mPrevLen == 0 && len == 0)
	return;

    if (mPrevLen == 0 && len) {
	t = createTxPacket(XIM_PREEDIT_START, 0);
	t->pushC16(mImid);
	t->pushC16(mIcid);
	mConn->push_passive_packet(t);
    }

    t = createTxPacket(XIM_PREEDIT_DRAW, 0);
    t->pushC16(mImid);
    t->pushC16(mIcid);
    t->pushC32(caret_pos);// caret
    t->pushC32(0); // chg_first
    t->pushC32(mPrevLen); // chg_length

    if (!m_pe->ustrings.empty())
	t->pushC32(0);
    else
	t->pushC32(3);
  
    compose_preedit_array(t);
    compose_feedback_array(t);
    mConn->push_passive_packet(t);

    if (mPrevLen && len == 0) {
	t = createTxPacket(XIM_PREEDIT_DONE, 0);
	t->pushC16(mImid);
	t->pushC16(mIcid);
	mConn->push_passive_packet(t);
    }
    mPrevLen = len;

    if (len) {
	t = createTxPacket(XIM_PREEDIT_CARET, 0);
	t->pushC16(mImid);
	t->pushC16(mIcid);
	t->pushC32(caret_pos);
	t->pushC32(XIMAbsolutePosition);
	t->pushC32(XIMIsPrimary);
	mConn->push_passive_packet(t);
    }
}

void ConvdispOs::move_candwin()
{
    InputContext *focusedContext = InputContext::focusedContext();
    if (!focusedContext || focusedContext != mKkContext)
	return;

    if (m_atr->has_atr(ICA_ClientWindow)) {
	int x, y;
	Window win;
	XWindowAttributes xattr;

	if (!m_atr->has_atr(ICA_SpotLocation)) {
	    m_atr->spot_location.x = 0;
	    m_atr->spot_location.y = 0;
	}

	XTranslateCoordinates(XimServer::gDpy, m_atr->client_window,
			      DefaultRootWindow(XimServer::gDpy),
			      m_atr->spot_location.x,
			      m_atr->spot_location.y,
			      &x, &y, &win);

	Canddisp *disp = canddisp_singleton();

	if (m_atr->has_atr(ICA_SpotLocation))
	    disp->move(x, y + 36); // 36 pixel seems too long.
				   // Is there any way to get current preedit
				   // height?
	else {
	    XGetWindowAttributes(XimServer::gDpy, m_atr->client_window,
			      &xattr);
	    //disp->move(x + xattr.width + 2, y + 2); //upper-right side
	    disp->move(x, y + xattr.height + 2); //lower-left side
	}
    }    
    
}

void ConvdispOs::clear_preedit()
{
    mPrevLen = 0;
}

void ConvdispOs::update_icxatr()
{
}

void ConvdispOs::compose_preedit_array(TxPacket *t)
{
    uString s;
    std::list<pe_ustring>::iterator it;
    for (it = m_pe->ustrings.begin(); it != m_pe->ustrings.end(); ++it) {
	append_ustring(&s, &(*it).s);
    }

    XimIM *im = get_im_by_id(mKkContext->get_ic()->get_imid());
    char *c = im->uStringToCtext(&s);
    int i, len = 0;
    if (c)
	len = static_cast<int>(strlen(c));
    t->pushC16((C16)len); // LENGTH
    for (i = 0; i < len; i++) {
	t->pushC8(c[i]); // CTEXT
    }
    len = pad4(len + 2);
    for (i = 0; i < len; i++) {
	t->pushC8(0); // PADDING
    }
    free(c);
}

void ConvdispOs::compose_feedback_array(TxPacket *t)
{
    int i, len, stat, xstat;
    len = m_pe->get_char_count();
    t->pushC16((C16)(len * 4));
    t->pushC16(0);
    std::list<pe_ustring>::iterator it;
    for (it = m_pe->ustrings.begin(); it != m_pe->ustrings.end(); ++it) {
	len = static_cast<int>((*it).s.size());
	stat = (*it).stat;
	xstat = FB_None;
	if (stat & PE_REVERSE)
	    xstat |= FB_Reverse;

	if (stat & PE_UNDERLINE)
	    xstat |= FB_Underline;

	if (stat & PE_HILIGHT)
	    xstat |= FB_Highlight;

	for (i = 0; i < len; i++) {
	    t->pushC32(xstat);
	}
    }
}

bool ConvdispOs::use_xft()
{
    return true;
}

#ifdef FLASHPLAYER7_WORKAROUND
static Window getTopWindow(Display *d, Window w)
{
    Window root, parent, *children;
    unsigned int nchild;
    Status retval;

    for (;;) {
	retval = XQueryTree(d, w, &root, &parent, &children, &nchild);
	if (retval == 0)
	    return 0;
	if (children)
	    XFree(children);
	if (parent == root)
	    break;
	w = parent;
    }
    return w;
};
#endif

/*
 * Local variables:
 *  c-indent-level: 4
 *  c-basic-offset: 4
 * End:
 */
