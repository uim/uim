/*

  Copyright (c) 2004-2005 Kazuki Ohta <mover@hct.zaq.ne.jp>
  Copyright (c) 2005-2013 uim Project https://github.com/uim/uim

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

#ifndef UIM_QT4_IMMODULE_QUIMINPUTCONTEXT_H
#define UIM_QT4_IMMODULE_QUIMINPUTCONTEXT_H

#include <QtCore/QtGlobal>
#include <QtGui/QInputContext>

#ifdef Q_WS_X11
#define UIM_QT_USE_JAPANESE_KANA_KEYBOARD_HACK 1
#endif

#include <uim/uim.h>
#include <uim/uim-helper.h>

#include "util.h" // for WORKAROUND_BROKEN_RESET_IN_QT4

class QEvent;

class CandidateWindowProxy;
class CaretStateIndicator;
class QUimHelperManager;
class QUimTextUtil;
#ifdef Q_WS_X11
typedef struct _DefTree DefTree;
class Compose;
#endif

struct PreeditSegment
{
    PreeditSegment( int attr, const QString &str )
    {
        this->attr = attr;
        this->str = str;
    }

    int attr;
    QString str;
};

class QUimInputContext : public QInputContext
{
    Q_OBJECT
public:
    explicit QUimInputContext( const char *imname = 0 );
    ~QUimInputContext();

    virtual QString identifierName();
    virtual QString language();

#ifdef Q_WS_X11
    virtual bool x11FilterEvent( QWidget *keywidget, XEvent *event );
#endif
    virtual bool filterEvent( const QEvent *event );
    virtual void reset();
    virtual void update();
    virtual void mouseHandler( int x, QMouseEvent *event );
    virtual bool isComposing() const { return m_isComposing; }
    virtual void setFocusWidget( QWidget *w );

    uim_context uimContext() { return m_uc; }

    static QUimInputContext *focusedIC();

    void commitString( const QString& str );

    void updatePosition();
    void updateStyle();

    QUimTextUtil *textUtil() { return mTextUtil; }

    QString getPreeditString();
    int getPreeditCursorPosition();

    void saveContext();
    void restoreContext();

    void updateIndicator( const QString &str );

    void setCandwinActive() { candwinIsActive = true; }

protected:
    uim_context createUimContext( const char *imname );
    void createCandidateWindow();
    bool isPreeditPreservationEnabled();  // not a QInputContext func
    virtual void setFocus();    // not a QInputContext func
    virtual void unsetFocus();  // not a QInputContext func

private:
    int getPreeditSelectionLength();
    QList<QInputMethodEvent::Attribute> getPreeditAttrs();
#ifdef WORKAROUND_BROKEN_RESET_IN_QT4
    void savePreedit();
    void restorePreedit();
#endif

    /* callbacks for uim */
    static void commit_cb( void *ptr, const char *str );
    //preedit
    static void clear_cb( void *ptr );
    static void pushback_cb( void *ptr, int attr, const char *str );
    static void update_cb( void *ptr );
    //candidate
    static void cand_activate_cb( void *ptr, int nr, int displayLimit );
    static void cand_select_cb( void *ptr, int index );
    static void cand_shift_page_cb( void* ptr, int index );
    static void cand_deactivate_cb( void *ptr );
    //imsw
    static void switch_app_global_im_cb( void *ptr, const char *str );
    static void switch_system_global_im_cb( void *ptr, const char *str );
    //delay
    static void cand_activate_with_delay_cb( void *ptr, int delay );
    /* real functions for callbacks (correspond order) */
    //preedit
    void clearPreedit();
    void pushbackPreeditString( int attr, const QString& str );
    void updatePreedit();
    //imsw
    void switch_app_global_im( const char *str );
    void switch_system_global_im( const char *str );

#ifdef Q_WS_X11
    // for X11 Compose
    static DefTree *mTreeTop;
    static void create_compose_tree( void );
    static int get_compose_filename( char *filename, size_t len );
    static int TransFileName( char *transname, const char *name, size_t len );
    static void ParseComposeStringFile( FILE *fp );
    static void FreeComposeTree( DefTree *top );
    static int parse_compose_line( FILE *fp, char **tokenbuf, size_t *buflen );
    static int get_mb_string( char *buf, unsigned int ks );
    static const char *get_encoding( void );
    static int get_lang_region( char *lang_region, size_t len );

    Compose *mCompose;
#endif
    QUimTextUtil *mTextUtil;

    CaretStateIndicator *m_indicator;

protected:
    bool candwinIsActive;
    bool m_isComposing;

    uim_context m_uc;
    QList<PreeditSegment> psegs;
    CandidateWindowProxy *proxy;

#ifdef WORKAROUND_BROKEN_RESET_IN_QT4
    QHash<QWidget*, uim_context> m_ucHash;
    QHash<QWidget*, QList<PreeditSegment> > psegsHash;
    QHash<QWidget*, CandidateWindowProxy*> proxyHash;
    QHash<QWidget*, bool> visibleHash;

    QWidget *focusedWidget;
#endif

    static QUimHelperManager *m_HelperManager;
};

#endif /* Not def: UIM_QT4_IMMODULE_QUIMINPUTCONTEXT_H */
