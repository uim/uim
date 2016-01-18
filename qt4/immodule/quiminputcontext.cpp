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
#include "quiminputcontext.h"

#include <cctype>
#include <cstring>

#include <QtCore/QPoint>
#include <QtGui/QApplication>
#include <QtGui/QInputMethodEvent>
#include <QtGui/QLabel>
#include <QtGui/QTextCharFormat>

#include <uim/uim-helper.h>
#include <uim/uim-im-switcher.h>
#include <uim/uim-scm.h>

#include "candidatewindowproxy.h"
#include "caretstateindicator.h"
#include "plugin.h"
#include "qhelpermanager.h"
#include "qtextutil.h"
#include "quiminfomanager.h"
#include "quiminputcontext_compose.h"

#if UIM_QT_USE_JAPANESE_KANA_KEYBOARD_HACK
#include <X11/Xlib.h>

#include "uim/uim-x-util.h"
#endif

static const char DEFAULT_SEPARATOR_STR[] = "|";

QUimInputContext *focusedInputContext = 0;
bool disableFocusedContext = false;

QList<QUimInputContext*> contextList;

QUimHelperManager * QUimInputContext::m_HelperManager = 0;
#ifdef Q_WS_X11
DefTree *QUimInputContext::mTreeTop = 0;
#endif

static int unicodeToUKey(ushort c);

// I think that current index-based query API of uim for language and
// input method name is useless and should be redesigned. I will
// suggest the change in future. -- YamaKen 2004-07-28

QUimInputContext::QUimInputContext( const char *imname )
        : candwinIsActive( false ), m_isComposing( false ), m_uc( 0 )
#ifdef WORKAROUND_BROKEN_RESET_IN_QT4
        , focusedWidget( 0 )
#endif
{
#ifdef ENABLE_DEBUG
    qDebug( "QUimInputContext()" );
#endif

    contextList.append( this );

    // must be initialized before createUimContext() call
    if ( !m_HelperManager )
        m_HelperManager = new QUimHelperManager;

    if ( imname )
        m_uc = createUimContext( imname );

    createCandidateWindow();

#ifdef Q_WS_X11
    if ( !mTreeTop )
        create_compose_tree();
    mCompose = new Compose( mTreeTop, this );
#endif
    mTextUtil = new QUimTextUtil( this );

    // read configuration
    updatePosition();

    m_indicator = new CaretStateIndicator;
}

QUimInputContext::~QUimInputContext()
{
#ifdef ENABLE_DEBUG
    qDebug( "~QUimInputContext()" );
#endif

    contextList.removeAll( this );

    if ( m_uc )
        uim_release_context( m_uc );
    delete proxy;
#ifdef WORKAROUND_BROKEN_RESET_IN_QT4
    foreach ( const uim_context uc, m_ucHash )
        if ( uc )
            uim_release_context( uc );
    foreach ( const CandidateWindowProxy* window, proxyHash )
        delete window;
#endif

    if ( this == focusedInputContext )
    {
        focusedInputContext = 0;
        disableFocusedContext = true;
    }

#ifdef Q_WS_X11
    delete mCompose;
#endif
}

uim_context QUimInputContext::createUimContext( const char *imname )
{
    uim_context uc = uim_create_context( this, "UTF-8",
                                         0, imname,
                                         0,
                                         QUimInputContext::commit_cb );

    m_HelperManager->checkHelperConnection();

    /**/

    uim_set_preedit_cb( uc, QUimInputContext::clear_cb,
                        QUimInputContext::pushback_cb,
                        QUimInputContext::update_cb );

    uim_set_candidate_selector_cb( uc,
                                   QUimInputContext::cand_activate_cb,
                                   QUimInputContext::cand_select_cb,
                                   QUimInputContext::cand_shift_page_cb,
                                   QUimInputContext::cand_deactivate_cb );


    uim_set_prop_list_update_cb( uc, QUimHelperManager::update_prop_list_cb );
    uim_set_prop_label_update_cb( uc, QUimHelperManager::update_prop_label_cb );

    uim_set_im_switch_request_cb( uc,
                                  QUimInputContext::switch_app_global_im_cb,
                                  QUimInputContext::switch_system_global_im_cb);

    uim_set_text_acquisition_cb( uc,
                                 QUimTextUtil::acquire_text_cb,
                                 QUimTextUtil::delete_text_cb);

#if UIM_QT_USE_DELAY
    uim_set_delay_candidate_selector_cb( uc,
        QUimInputContext::cand_activate_with_delay_cb );
#endif /* !UIM_QT_USE_DELAY */

    uim_prop_list_update( uc );

    return uc;
}

void QUimInputContext::createCandidateWindow()
{
    proxy = new CandidateWindowProxy;
    proxy->setQUimInputContext( this );
    proxy->hide();
}

#ifdef Q_WS_X11
bool QUimInputContext::x11FilterEvent( QWidget *keywidget, XEvent *event )
{
    Q_UNUSED( keywidget )

#if UIM_QT_USE_JAPANESE_KANA_KEYBOARD_HACK
    return uim_x_kana_input_hack_filter_event( m_uc, event );
#else
    Q_UNUSED( event )
    return false;
#endif
}
#endif // Q_WS_X11

bool QUimInputContext::filterEvent( const QEvent *event )
{
#ifdef ENABLE_DEBUG
    qDebug( "filterEvent" );
#endif

    int type = event->type();

    if ( type != QEvent::KeyPress &&
            type != QEvent::KeyRelease )
        return false;

    const QKeyEvent *keyevent = static_cast<const QKeyEvent *>( event );
    int qkey = keyevent->key();

    int modifier = 0;
    if ( keyevent->modifiers() & Qt::ShiftModifier )
        modifier |= UMod_Shift;
    if ( keyevent->modifiers() & Qt::ControlModifier )
        modifier |= UMod_Control;
    if ( keyevent->modifiers() & Qt::AltModifier )
        modifier |= UMod_Alt;
#if defined(Q_WS_X11)
    if ( keyevent->modifiers() & Qt::MetaModifier )
        modifier |= UMod_Meta;
#endif

    int key = 0;
    if ( isascii( qkey ) && isprint( qkey ) )
    {
        int ascii = keyevent->text()[ 0 ].toAscii();
        if ( isalpha( ascii ) )
        {
            key = ascii;  // uim needs lower/upper encoded key
        }
        else
        {
            if ( keyevent->modifiers() & Qt::ControlModifier &&
                 ( ascii >= 0x01 && ascii <= 0x1a ) )
                if ( keyevent->modifiers() & Qt::ShiftModifier )
                    key = ascii + 0x40;
                else
                    key = ascii + 0x60;
            else
                key = qkey;
        }
    }
    else if ( qkey >= Qt::Key_nobreakspace && qkey <= Qt::Key_ydiaeresis )
	key = qkey;
    else if ( qkey == Qt::Key_unknown )
    {
        QString text = keyevent->text();
        if ( !text.isNull() )
        {
            QChar s = text.at(0);
            key = unicodeToUKey ( s.unicode() );
        }
        else
        {
            key = UKey_Other;
        }
    }
    else
    {
        if ( qkey >= Qt::Key_F1 && qkey <= Qt::Key_F35 )
        {
            key = qkey - Qt::Key_F1 + UKey_F1;
        }
        else if ( qkey >= Qt::Key_Dead_Grave && qkey <= Qt::Key_Dead_Horn )
        {
            key = qkey - Qt::Key_Dead_Grave + UKey_Dead_Grave;
        }
        else if ( qkey >= Qt::Key_Kanji && qkey <= Qt::Key_Eisu_toggle )
        {
            key = qkey - Qt::Key_Kanji + UKey_Kanji;
        }
        else if ( qkey >= Qt::Key_Hangul && qkey <= Qt::Key_Hangul_Special )
        {
            key = qkey - Qt::Key_Hangul + UKey_Hangul;
        }
        else
        {
            switch ( qkey )
            {
            case Qt::Key_Tab: key = UKey_Tab; break;
            case Qt::Key_Backspace: key = UKey_Backspace; break;
            case Qt::Key_Escape: key = UKey_Escape; break;
            case Qt::Key_Delete: key = UKey_Delete; break;
            case Qt::Key_Return: key = UKey_Return; break;
            case Qt::Key_Left: key = UKey_Left; break;
            case Qt::Key_Up: key = UKey_Up; break;
            case Qt::Key_Right: key = UKey_Right; break;
            case Qt::Key_Down: key = UKey_Down; break;
            case Qt::Key_PageUp: key = UKey_Prior; break;
            case Qt::Key_PageDown: key = UKey_Next; break;
            case Qt::Key_Home: key = UKey_Home; break;
            case Qt::Key_End: key = UKey_End; break;
            case Qt::Key_Multi_key: key = UKey_Multi_key; break;
            case Qt::Key_Mode_switch: key = UKey_Mode_switch; break;
            case Qt::Key_Codeinput: key = UKey_Codeinput; break;
            case Qt::Key_SingleCandidate: key = UKey_SingleCandidate; break;
            case Qt::Key_MultipleCandidate: key = UKey_MultipleCandidate; break;
            case Qt::Key_PreviousCandidate: key = UKey_PreviousCandidate; break;
            // Qt4 seems to add its own modifier even the event is
            // KeyPress, which differs from ordinary model.  So remove
            // them for uim.
            case Qt::Key_Shift: key = UKey_Shift_key; 
                if ( type == QEvent::KeyPress )
                    modifier &= ~UMod_Shift;
                break;
            case Qt::Key_Control: key = UKey_Control_key;
                if ( type == QEvent::KeyPress )
                    modifier &= ~UMod_Control;
                break;
            case Qt::Key_Alt: key = UKey_Alt_key;
                if ( type == QEvent::KeyPress )
                    modifier &= ~UMod_Alt;
                break;
            case Qt::Key_Meta: key = UKey_Meta_key;
#ifdef Q_WS_X11
                if ( type == QEvent::KeyPress )
                    modifier &= ~UMod_Meta;
#endif
                break;
            case Qt::Key_CapsLock: key = UKey_Caps_Lock; break;
            case Qt::Key_NumLock: key = UKey_Num_Lock; break;
            case Qt::Key_ScrollLock: key = UKey_Scroll_Lock; break;
            default: key = UKey_Other;
            }
        }
    }

    int notFiltered;
    if ( type == QEvent::KeyPress )
    {
        notFiltered = uim_press_key( m_uc, key, modifier );
#ifdef Q_WS_X11
        if ( notFiltered )
            return mCompose->handle_qkey( keyevent );
#else
        if ( notFiltered )
            return false;
#endif
    }
    else if ( type == QEvent::KeyRelease )
    {
        notFiltered = uim_release_key( m_uc, key, modifier );
#ifdef Q_WS_X11
        if ( notFiltered )
            return mCompose->handle_qkey( keyevent );
#else
        if ( notFiltered )
            return false;
#endif
    }

    return true;
}

void QUimInputContext::setFocusWidget( QWidget *w )
{
#ifdef ENABLE_DEBUG
    qDebug( "QUimInputContext::setFocusWidget() w = %p", w );
#endif

    QInputContext::setFocusWidget( w );

    if ( w )
        setFocus();
    else
        unsetFocus();
}

// Qt4 does not have QInputContext::setFocus()
void QUimInputContext::setFocus()
{
#ifdef ENABLE_DEBUG
    qDebug( "QUimInputContext: %p->setFocus(), focusWidget()=%p",
            this, QApplication::focusWidget() );
#endif

    focusedInputContext = this;
    disableFocusedContext = false;

#ifdef WORKAROUND_BROKEN_RESET_IN_QT4
    focusedWidget = QApplication::focusWidget();
    if ( isPreeditPreservationEnabled()
            && m_ucHash.contains( focusedWidget ) )
        restorePreedit();
    else
#endif
    if ( candwinIsActive )
        proxy->popup();

    m_HelperManager->checkHelperConnection();

    uim_helper_client_focus_in( m_uc );
    uim_prop_list_update( m_uc );

    uim_focus_in_context( m_uc );
}

// Qt4 does not have QInputContext::unsetFocus()
void QUimInputContext::unsetFocus()
{
#ifdef ENABLE_DEBUG
    qDebug( "QUimInputContext: %p->unsetFocus(), focusWidget()=%p",
            this, QApplication::focusWidget() );
#endif

    uim_focus_out_context( m_uc );

    proxy->hide();
    m_indicator->hide();

    m_HelperManager->checkHelperConnection();

    uim_helper_client_focus_out( m_uc );
}

QUimInputContext * QUimInputContext::focusedIC()
{
    return focusedInputContext;
}

void QUimInputContext::mouseHandler( int x, QMouseEvent *e )
{
    switch ( e->type() )
    {
    case QEvent::MouseButtonPress:
    case QEvent::MouseButtonRelease:
    case QEvent::MouseButtonDblClick:
    case QEvent::MouseMove:
#ifdef ENABLE_DEBUG
        qDebug( "QUimInputContext::mouseHandler: "
                "x=%d, type=%d, button=%d ", x, e->type(), e->button() );
#else
        Q_UNUSED( x )
#endif
        break;
    default:
        break;
    }
}

void QUimInputContext::reset()
{
#ifdef ENABLE_DEBUG
    qDebug( "QUimInputContext::reset()" );
#endif

    candwinIsActive = false;

#ifdef WORKAROUND_BROKEN_RESET_IN_QT4
    // Japanese input context sometimes contains a whole paragraph
    // and has minutes of lifetime different to ephemeral one
    // in other languages. The input context should be survived
    // until focused again.
    if ( isPreeditPreservationEnabled()
            && !m_ucHash.contains( focusedWidget ) ) {
        psegs.isEmpty() ? proxy->hide() : savePreedit();
        return;
    }
#endif
    proxy->hide();
    uim_reset_context( m_uc );
#ifdef Q_WS_X11
    mCompose->reset();
#endif
    clearPreedit();
    updatePreedit();
}

void QUimInputContext::update()
{
    QWidget *w = QApplication::focusWidget();

#ifdef ENABLE_DEBUG
    qDebug( "QUimInputContext::update() w = %p", w );
#endif

    if ( w ) {
        QRect mf = w->inputMethodQuery( Qt::ImMicroFocus ).toRect();
        QPoint p = w->mapToGlobal( mf.topLeft() );
        proxy->layoutWindow( p.x(), p.y(), mf.height() );
        m_indicator->move( w->mapToGlobal( mf.bottomLeft() )
            + QPoint( 0, CaretStateIndicator::SPACING ) );
    }
}

QString QUimInputContext::identifierName()
{
    return QString( "uim" );
}

QString QUimInputContext::language()
{
    const QUimInfoManager *manager
        = UimInputContextPlugin::getQUimInfoManager();
    const QString name = QString::fromUtf8( uim_get_current_im_name( m_uc ) );
    return manager->imLang( name );
}

// callbacks for uim
void QUimInputContext::commit_cb( void *ptr, const char *str )
{
    QString qs = QString::fromUtf8( str );
#ifdef ENABLE_DEBUG
    qDebug( "commit_cb : str = |%s|", qs.toLocal8Bit().data() );
#endif

    QUimInputContext *ic = static_cast<QUimInputContext *>( ptr );
    ic->commitString( qs );
}

void QUimInputContext::clear_cb( void *ptr )
{
#ifdef ENABLE_DEBUG
    qDebug( "clear_cb" );
#endif

    QUimInputContext* ic = static_cast<QUimInputContext*>( ptr );
    ic->clearPreedit();
}

void QUimInputContext::pushback_cb( void *ptr, int attr, const char *str )
{
    QString qs = QString::fromUtf8( str );
#ifdef ENABLE_DEBUG
    qDebug( "pushback_cb :  str = |%s|", qs.toLocal8Bit().data() );
#endif

    if ( !str )
        return ;
    // Reject invalid empty string. UPreeditAttr_Cursor or
    // UPreeditAttr_Separator with empty string is *valid* and
    // required to work properly.
    if ( !strcmp( str, "" ) && !( attr & ( UPreeditAttr_Cursor | UPreeditAttr_Separator ) ) )
        return ;

    QUimInputContext* ic = static_cast<QUimInputContext*>( ptr );
    ic->pushbackPreeditString( attr, qs );
}

void QUimInputContext::update_cb( void *ptr )
{
#ifdef ENABLE_DEBUG
    qDebug( "update_cb" );
#endif

    QUimInputContext *ic = static_cast<QUimInputContext*>( ptr );
    ic->updatePreedit();
}

void QUimInputContext::cand_activate_cb( void *ptr, int nr, int displayLimit )
{
#ifdef ENABLE_DEBUG
    qDebug( "cand_activate_cb" );
#endif

    QUimInputContext *ic = static_cast<QUimInputContext*>( ptr );
    ic->proxy->candidateActivate( nr, displayLimit );
}

void QUimInputContext::cand_select_cb( void *ptr, int index )
{
#ifdef ENABLE_DEBUG
    qDebug( "cand_select_cb" );
#endif

    QUimInputContext *ic = static_cast<QUimInputContext*>( ptr );
    ic->proxy->candidateSelect( index );
}

void QUimInputContext::cand_shift_page_cb( void *ptr, int forward )
{
#ifdef ENABLE_DEBUG
    qDebug( "cand_shift_page_cb" );
#endif

    QUimInputContext *ic = static_cast<QUimInputContext*>( ptr );
    ic->proxy->candidateShiftPage( forward );
}

void QUimInputContext::cand_deactivate_cb( void *ptr )
{
#ifdef ENABLE_DEBUG
    qDebug( "cand_deactivate_cb" );
#endif

    QUimInputContext *ic = static_cast<QUimInputContext*>( ptr );
    ic->proxy->deactivateCandwin();
    ic->candwinIsActive = false;
}

void QUimInputContext::switch_app_global_im_cb( void *ptr, const char *name )
{
    QUimInputContext *ic = static_cast<QUimInputContext*>( ptr );
    ic->switch_app_global_im( name );
}

void QUimInputContext::switch_system_global_im_cb( void *ptr, const char *name )
{
    QUimInputContext *ic = static_cast<QUimInputContext*>( ptr );
    ic->switch_system_global_im( name );
}

#if UIM_QT_USE_DELAY
void QUimInputContext::cand_activate_with_delay_cb( void *ptr, int delay )
{
    QUimInputContext *ic = static_cast<QUimInputContext*>( ptr );
    ic->proxy->candidateActivateWithDelay( delay );
}
#endif /* !UIM_QT_USE_DELAY */

void QUimInputContext::commitString( const QString& str )
{
    QInputMethodEvent e;
    e.setCommitString( str );
    sendEvent( e );

    m_isComposing = false;
}

void QUimInputContext::clearPreedit()
{
    psegs.clear();
}

void QUimInputContext::pushbackPreeditString( int attr, const QString& str )
{
    PreeditSegment ps( attr, str );
    psegs.append( ps );
}

void QUimInputContext::updatePreedit()
{
    QString newString = getPreeditString();

    if ( !isComposing() ) {
        if ( newString.isEmpty() )
            return;

        // Start conversion
        m_isComposing = true;
    }

    if ( !newString.isEmpty() ) {
        QInputMethodEvent e( newString, getPreeditAttrs() );
        sendEvent( e );
        // Qt4.3.1 does not call back update() here
        update();
    } else {
        // Complete conversion implicitly since the preedit is empty
        commitString( "" );
    }
}

#ifdef WORKAROUND_BROKEN_RESET_IN_QT4
void QUimInputContext::savePreedit()
{
    m_ucHash.insert( focusedWidget, m_uc );
    psegsHash.insert( focusedWidget, psegs );
    proxyHash.insert( focusedWidget, proxy );
    visibleHash.insert( focusedWidget, proxy->isVisible() );
    proxy->hide();

    const char *imname = uim_get_current_im_name( m_uc );
    if ( imname )
        m_uc = createUimContext( imname );
    psegs.clear();
    createCandidateWindow();
}

void QUimInputContext::restorePreedit()
{
    CandidateWindowProxy *window = proxyHash.take( focusedWidget );
    // if window is 0, updateStyle() was called.
    if ( !window ) {
        psegs = psegsHash.take( focusedWidget );
        QString preedit;
        while ( !psegs.isEmpty() ) {
            preedit += psegs.takeFirst().str;
        }
        commitString( preedit );

        uim_context uc = m_ucHash.take( focusedWidget );
        if ( uc )
            uim_release_context( uc );
        visibleHash.remove( focusedWidget );
        return;
    }
    if ( m_uc )
        uim_release_context( m_uc );
    delete proxy;
    m_uc = m_ucHash.take( focusedWidget );
    psegs = psegsHash.take( focusedWidget );
    proxy = window;
    if ( visibleHash.take( focusedWidget ) )
        proxy->popup();
}
#endif

void QUimInputContext::saveContext()
{
    // just send QInputMethodEvent and keep preedit string
    if ( isComposing() )
        commitString( "" );
}

void QUimInputContext::restoreContext()
{
    updatePreedit();
}

bool QUimInputContext::isPreeditPreservationEnabled()
{
    return ( language() == "ja" );
}

QString QUimInputContext::getPreeditString()
{
    QString pstr;

    QList<PreeditSegment>::ConstIterator seg = psegs.begin();
    const QList<PreeditSegment>::ConstIterator end = psegs.end();
    for ( ; seg != end; ++seg )
    {
        if ( ( *seg ).attr & UPreeditAttr_Separator && ( *seg ).str.isEmpty() )
        {
            pstr += DEFAULT_SEPARATOR_STR;
        }
        else
        {
            pstr += ( *seg ).str;
        }
    }

    return pstr;
}

int QUimInputContext::getPreeditCursorPosition()
{
    if ( proxy->isAlwaysLeftPosition() )
        return 0;

    int cursorPos = 0;
    QList<PreeditSegment>::ConstIterator seg = psegs.begin();
    const QList<PreeditSegment>::ConstIterator end = psegs.end();
    for ( ; seg != end; ++seg )
    {
        if ( ( *seg ).attr & UPreeditAttr_Cursor )
        {
            return cursorPos;
        }
        else if ( ( *seg ).attr & UPreeditAttr_Separator
                  && ( *seg ).str.isEmpty() )
        {
            cursorPos += QString( DEFAULT_SEPARATOR_STR ).length();
        }
        else
        {
            cursorPos += ( *seg ).str.length();
        }
    }

    return cursorPos;
}

int QUimInputContext::getPreeditSelectionLength()
{
    int selectionLength = 0;

    QList<PreeditSegment>::ConstIterator seg = psegs.begin();
    const QList<PreeditSegment>::ConstIterator end = psegs.end();
    for ( ; seg != end; ++seg )
    {
        // In converting state, uim encodes UPreeditAttr_Cursor into
        // selected segment rather than separated empty cursor
        // segment. So we can get selection length by length of this
        // 'selected segment'. Don't use visual attributes such as
        // UPreeditAttr_Underline or UPreeditAttr_Reverse to detect
        // logical selection length. They are sometimes disabled by
        // user preference.
        if ( ( *seg ).attr & UPreeditAttr_Cursor )
        {
            selectionLength = ( *seg ).str.length();
            return selectionLength;
        }
    }

    return 0;
}

static QColor getUserDefinedColor( const char *symbol )
{
    char *literal = uim_scm_symbol_value_str( symbol );
    QColor color( QString::fromAscii( literal ) );
    free( literal );
    return color;
}

QList<QInputMethodEvent::Attribute> QUimInputContext::getPreeditAttrs()
{
    const int HIDE_CARET = 0;
    const int SHOW_CARET = 1;
    const int DUMMY = 0;
    QList<QInputMethodEvent::Attribute> attrs;

    QList<PreeditSegment>::ConstIterator seg = psegs.begin();
    const QList<PreeditSegment>::ConstIterator end = psegs.end();
    int segPos = 0;
    for ( ; seg != end; ++seg ) {
        int uimAttr = ( *seg ).attr;
        int segStrLen = ( *seg ).str.length();
        QTextCharFormat segFmt;

        if ( uimAttr & UPreeditAttr_Cursor ) {
            // Transparent cursor is required to set microfocus even
            // if the caret is invisible to user.
            //
            // FIXME: Segment string may be outframed if the cursor is
            // located near the right frame.
            int visibility = ( segStrLen ) ? HIDE_CARET : SHOW_CARET;
            QInputMethodEvent::Attribute cursor( QInputMethodEvent::Cursor,
                                                 segPos, visibility, DUMMY );
            attrs << cursor;
        } else if ( uimAttr & UPreeditAttr_Separator ) {
            if ( !segStrLen )
                segStrLen = QString( DEFAULT_SEPARATOR_STR ).length();
            if ( !( uimAttr & UPreeditAttr_Reverse ) ) {
                QColor color = getUserDefinedColor( "separator-foreground" );
                if ( color.isValid() )
                    segFmt.setForeground( color );
                color = getUserDefinedColor( "separator-background" );
                if ( color.isValid() )
                    segFmt.setBackground( color );
    
            }
        }

        if ( segStrLen ) {
            if ( uimAttr & UPreeditAttr_Reverse ) {
#if 0
                // FIXME: fmt.foreground() is white (expecting black)
                QTextFormat fmt = standardFormat( PreeditFormat );
                segFmt.setForeground( fmt.background() );
                segFmt.setBackground( fmt.foreground() );
#else
                // FIXME: Retrieve customized colors from the text widget
                // foreground symbol
                const char *fgSymbol;
                // background symbol
                const char *bgSymbol;
                if ( uimAttr & UPreeditAttr_Separator ) {
                    fgSymbol = "reversed-separator-foreground";
                    bgSymbol = "reversed-separator-background";
                } else {
                    fgSymbol = "reversed-preedit-foreground";
                    bgSymbol = "reversed-preedit-background";
                }
                QColor color = getUserDefinedColor( fgSymbol );
                segFmt.setForeground( color.isValid() ? color : Qt::white );
                color = getUserDefinedColor( bgSymbol );
                segFmt.setBackground( color.isValid() ? color : Qt::black );
#endif
            }
            if ( uimAttr & UPreeditAttr_UnderLine ) {
                segFmt.setFontUnderline( true );
            }
            QInputMethodEvent::Attribute segAttr( QInputMethodEvent::TextFormat,
                                                  segPos, segStrLen, segFmt );
            attrs << segAttr;
            segPos += segStrLen;
        }
    }

    return attrs;
}

void QUimInputContext::switch_app_global_im( const char *name )
{
    QList<QUimInputContext*>::iterator it;
    QString im_name_sym( "'" );

    im_name_sym += name;

    for ( it = contextList.begin(); it != contextList.end(); ++it )
    {
        if ( ( *it ) != this) {
            uim_switch_im( ( *it )->uimContext(), name );
            ( *it )->updatePosition();
        }
    }
    uim_prop_update_custom(this->uimContext(), "custom-preserved-default-im-name", im_name_sym.toUtf8().data() );
}

void QUimInputContext::switch_system_global_im( const char *name )
{
    switch_app_global_im( name );
    QUimHelperManager::send_im_change_whole_desktop( name );
}

void QUimInputContext::updatePosition()
{
    char * leftp = uim_scm_symbol_value_str( "candidate-window-position" );
    proxy->setAlwaysLeftPosition( leftp && !strcmp( leftp, "left" ) );
    free( leftp );
}

void QUimInputContext::updateStyle()
{
    // don't update window style if deprecated uim-candwin-prog is set
    char *candwinprog = uim_scm_symbol_value_str( "uim-candwin-prog" );
    if ( candwinprog ) {
        free( candwinprog );
        return;
    }
    delete proxy;
    createCandidateWindow();
#ifdef WORKAROUND_BROKEN_RESET_IN_QT4
    // invalidate all the candidate windows stored in proxyHash
    QHashIterator<QWidget*, CandidateWindowProxy*> i( proxyHash );
    while ( i.hasNext() ) {
        i.next();
        QWidget *widget = i.key();
        delete proxyHash[ widget ];
        proxyHash[ widget ] = 0;
    }
#endif
}

void QUimInputContext::updateIndicator( const QString &str )
{
    m_indicator->update( str );
}

static int unicodeToUKey (ushort c) {
    int sym;

    switch (c) {
    case 0x00A5: sym = UKey_Yen; break;
    case 0x3002: sym = UKey_Kana_Fullstop; break;
    case 0x300C: sym = UKey_Kana_OpeningBracket; break;
    case 0x300D: sym = UKey_Kana_ClosingBracket; break;
    case 0x3001: sym = UKey_Kana_Comma; break;
    case 0x30FB: sym = UKey_Kana_Conjunctive; break;
    case 0x30F2: sym = UKey_Kana_WO; break;
    case 0x30A1: sym = UKey_Kana_a; break;
    case 0x30A3: sym = UKey_Kana_i; break;
    case 0x30A5: sym = UKey_Kana_u; break;
    case 0x30A7: sym = UKey_Kana_e; break;
    case 0x30A9: sym = UKey_Kana_o; break;
    case 0x30E3: sym = UKey_Kana_ya; break;
    case 0x30E5: sym = UKey_Kana_yu; break;
    case 0x30E7: sym = UKey_Kana_yo; break;
    case 0x30C3: sym = UKey_Kana_tsu; break;
    case 0x30FC: sym = UKey_Kana_ProlongedSound; break;
    case 0x30A2: sym = UKey_Kana_A; break;
    case 0x30A4: sym = UKey_Kana_I; break;
    case 0x30A6: sym = UKey_Kana_U; break;
    case 0x30A8: sym = UKey_Kana_E; break;
    case 0x30AA: sym = UKey_Kana_O; break;
    case 0x30AB: sym = UKey_Kana_KA; break;
    case 0x30AD: sym = UKey_Kana_KI; break;
    case 0x30AF: sym = UKey_Kana_KU; break;
    case 0x30B1: sym = UKey_Kana_KE; break;
    case 0x30B3: sym = UKey_Kana_KO; break;
    case 0x30B5: sym = UKey_Kana_SA; break;
    case 0x30B7: sym = UKey_Kana_SHI; break;
    case 0x30B9: sym = UKey_Kana_SU; break;
    case 0x30BB: sym = UKey_Kana_SE; break;
    case 0x30BD: sym = UKey_Kana_SO; break;
    case 0x30BF: sym = UKey_Kana_TA; break;
    case 0x30C1: sym = UKey_Kana_CHI; break;
    case 0x30C4: sym = UKey_Kana_TSU; break;
    case 0x30C6: sym = UKey_Kana_TE; break;
    case 0x30C8: sym = UKey_Kana_TO; break;
    case 0x30CA: sym = UKey_Kana_NA; break;
    case 0x30CB: sym = UKey_Kana_NI; break;
    case 0x30CC: sym = UKey_Kana_NU; break;
    case 0x30CD: sym = UKey_Kana_NE; break;
    case 0x30CE: sym = UKey_Kana_NO; break;
    case 0x30CF: sym = UKey_Kana_HA; break;
    case 0x30D2: sym = UKey_Kana_HI; break;
    case 0x30D5: sym = UKey_Kana_FU; break;
    case 0x30D8: sym = UKey_Kana_HE; break;
    case 0x30DB: sym = UKey_Kana_HO; break;
    case 0x30DE: sym = UKey_Kana_MA; break;
    case 0x30DF: sym = UKey_Kana_MI; break;
    case 0x30E0: sym = UKey_Kana_MU; break;
    case 0x30E1: sym = UKey_Kana_ME; break;
    case 0x30E2: sym = UKey_Kana_MO; break;
    case 0x30E4: sym = UKey_Kana_YA; break;
    case 0x30E6: sym = UKey_Kana_YU; break;
    case 0x30E8: sym = UKey_Kana_YO; break;
    case 0x30E9: sym = UKey_Kana_RA; break;
    case 0x30EA: sym = UKey_Kana_RI; break;
    case 0x30EB: sym = UKey_Kana_RU; break;
    case 0x30EC: sym = UKey_Kana_RE; break;
    case 0x30ED: sym = UKey_Kana_RO; break;
    case 0x30EF: sym = UKey_Kana_WA; break;
    case 0x30F3: sym = UKey_Kana_N; break;
    case 0x309B: sym = UKey_Kana_VoicedSound; break;
    case 0x309C: sym = UKey_Kana_SemivoicedSound; break;
    default:
        sym = UKey_Other;
        break;
    }

    return sym;
}
