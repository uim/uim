/*

Copyright (c) 2003,2004,2005 uim Project http://uim.freedesktop.org/

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
#include "quiminputcontext.h"

#include <qnamespace.h>
#include <qevent.h>
#include <qglobal.h>
#include <qapplication.h>
#include <qpoint.h>
#include <qlabel.h>
#include <qvaluelist.h>

#include <ctype.h>
#include <string.h>

#include "candidatewindow.h"
#include "qhelpermanager.h"

#define DEFAULT_SEPARATOR_STR "|"

QUimInputContext *focusedInputContext = NULL;
bool disableFocusedContext = false;

QPtrList<QUimInputContext> contextList;
QValueList<UIMInfo> uimInfo;

QUimHelperManager * QUimInputContext::m_HelperManager = 0L;

// I think that current index-based query API of uim for language and
// input method name is useless and should be redesigned. I will
// suggest the change in future. -- YamaKen 2004-07-28

QUimInputContext::QUimInputContext( const char *imname, const char *lang )
        : QInputContext(), m_imname( imname ), m_lang( lang ), m_uc( 0 ),
        candwinIsActive( false )
{
    contextList.append( this );

    if ( imname )
        m_uc = createUimContext( imname );

    psegs.setAutoDelete( true );
    psegs.clear();

    cwin = new CandidateWindow( 0 );
    cwin->setQUimInputContext( this );
    cwin->hide();

    if ( !m_HelperManager )
        m_HelperManager = new QUimHelperManager();

    createUimInfo();

    // read configuration
    readIMConf();

    qDebug( "QUimInputContext()" );
}

QUimInputContext::~QUimInputContext()
{
    qDebug( "~QUimInputContext()" );

    contextList.remove( this );

    if ( m_uc )
        uim_release_context( m_uc );

    if ( this == focusedInputContext )
    {
        focusedInputContext = NULL;
        disableFocusedContext = true;
    }
}

uim_context QUimInputContext::createUimContext( const char *imname )
{
    m_imname = imname;

    uim_context uc = uim_create_context( this, "UTF-8",
                                         NULL, ( char * ) imname,
                                         uim_iconv,
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
    uim_prop_list_update( uc );

    return uc;
}

bool QUimInputContext::filterEvent( const QEvent *event )
{
    //qDebug("filterEvent");

    int type = event->type();

    if ( type != QEvent::KeyPress &&
            type != QEvent::KeyRelease )
        return FALSE;

    QKeyEvent *keyevent = ( QKeyEvent * ) event;
    int qkey = keyevent->key();

    int modifier = 0;
    if ( keyevent->state() & Qt::ShiftButton )
        modifier |= UMod_Shift;
    if ( keyevent->state() & Qt::ControlButton )
        modifier |= UMod_Control;
    if ( keyevent->state() & Qt::AltButton )
        modifier |= UMod_Alt;
#if defined(_WS_X11_)
    if ( keyevent->state() & Qt::MetaButton )
        modifier |= UMod_Meta;
#endif

    int key = 0;
    if ( isascii( qkey ) && isprint( qkey ) )
    {
        int ascii = keyevent->ascii();
        if ( isalpha( ascii ) )
        {
            key = ascii;  // uim needs lower/upper encoded key
        }
        else
        {
            key = qkey;
        }
    }
    else
    {
        switch ( qkey )
        {
        case Qt::Key_Tab: key = UKey_Tab; break;
        case Qt::Key_BackSpace: key = UKey_Backspace; break;
        case Qt::Key_Escape: key = UKey_Escape; break;
        case Qt::Key_Delete: key = UKey_Delete; break;
        case Qt::Key_Return: key = UKey_Return; break;
        case Qt::Key_Left: key = UKey_Left; break;
        case Qt::Key_Up: key = UKey_Up; break;
        case Qt::Key_Right: key = UKey_Right; break;
        case Qt::Key_Down: key = UKey_Down; break;
        case Qt::Key_Prior: key = UKey_Prior; break;
        case Qt::Key_Next: key = UKey_Next; break;
        case Qt::Key_Home: key = UKey_Home; break;
        case Qt::Key_End: key = UKey_End; break;
        case Qt::Key_Zenkaku_Hankaku: key = UKey_Zenkaku_Hankaku; break;
        case Qt::Key_Multi_key: key = UKey_Multi_key; break;
#if defined(_WS_X11_)
        case Qt::Key_Mode_switch: key = UKey_Mode_switch; break;
#endif
        case Qt::Key_Henkan: key = UKey_Henkan_Mode; break;
        case Qt::Key_Muhenkan: key = UKey_Muhenkan; break;
        case Qt::Key_F1: key = UKey_F1; break;
        case Qt::Key_F2: key = UKey_F2; break;
        case Qt::Key_F3: key = UKey_F3; break;
        case Qt::Key_F4: key = UKey_F4; break;
        case Qt::Key_F5: key = UKey_F5; break;
        case Qt::Key_F6: key = UKey_F6; break;
        case Qt::Key_F7: key = UKey_F7; break;
        case Qt::Key_F8: key = UKey_F8; break;
        case Qt::Key_F9: key = UKey_F9; break;
        case Qt::Key_F10: key = UKey_F10; break;
        case Qt::Key_F11: key = UKey_F11; break;
        case Qt::Key_F12: key = UKey_F12; break;
        case Qt::Key_F13: key = UKey_F13; break;
        case Qt::Key_F14: key = UKey_F14; break;
        case Qt::Key_F15: key = UKey_F15; break;
        case Qt::Key_F16: key = UKey_F16; break;
        case Qt::Key_F17: key = UKey_F17; break;
        case Qt::Key_F18: key = UKey_F18; break;
        case Qt::Key_F19: key = UKey_F19; break;
        case Qt::Key_F20: key = UKey_F20; break;
        case Qt::Key_F21: key = UKey_F21; break;
        case Qt::Key_F22: key = UKey_F22; break;
        case Qt::Key_F23: key = UKey_F23; break;
        case Qt::Key_F24: key = UKey_F24; break;
        case Qt::Key_F25: key = UKey_F25; break;
        case Qt::Key_F26: key = UKey_F26; break;
        case Qt::Key_F27: key = UKey_F27; break;
        case Qt::Key_F28: key = UKey_F28; break;
        case Qt::Key_F29: key = UKey_F29; break;
        case Qt::Key_F30: key = UKey_F30; break;
        case Qt::Key_F31: key = UKey_F31; break;
        case Qt::Key_F32: key = UKey_F32; break;
        case Qt::Key_F33: key = UKey_F33; break;
        case Qt::Key_F34: key = UKey_F34; break;
        case Qt::Key_F35: key = UKey_F35; break;
        default: key = UKey_Other;
        }
    }

    int notFiltered;
    if ( type == QEvent::KeyPress )
    {
        notFiltered = uim_press_key( m_uc, key, modifier );
        if ( notFiltered )
            return FALSE;
    }
    else if ( type == QEvent::KeyRelease )
    {
        notFiltered = uim_release_key( m_uc, key, modifier );
        if ( notFiltered )
            return FALSE;
    }

    return TRUE;
}

void QUimInputContext::setFocus()
{
    qDebug( "QUimInputContext: %p->setFocus(), focusWidget()=%p",
            this, focusWidget() );

    focusedInputContext = this;
    disableFocusedContext = false;

    if ( candwinIsActive )
        cwin->popup();

    m_HelperManager->checkHelperConnection();

    uim_helper_client_focus_in( m_uc );
    uim_prop_list_update( m_uc );
    uim_prop_label_update( m_uc );
}

void QUimInputContext::unsetFocus()
{
    qDebug( "QUimInputContext: %p->unsetFocus(), focusWidget()=%p",
            this, focusWidget() );

    // Don't reset Japanese input context here. Japanese input context
    // sometimes contains a whole paragraph and has minutes of
    // lifetime different to ephemeral one in other languages. The
    // input context should be survived until focused again.
    if ( ! isPreeditPreservationEnabled() )
        reset();

    cwin->hide();

    uim_helper_client_focus_out( m_uc );
}

QUimInputContext * QUimInputContext::focusedIC()
{
    return focusedInputContext;
}


void QUimInputContext::setMicroFocus( int x, int y, int w, int h, QFont *f )
{
    // qDebug("IC setMicroFocus (%d, %d), (%d, %d)", x, y, w, h);
    cwin->layoutWindow( x, y, w, h );
}

void QUimInputContext::mouseHandler( int x, QEvent::Type type,
                                     Qt::ButtonState button,
                                     Qt::ButtonState state )
{
    switch ( type )
    {
    case QEvent::MouseButtonPress:
    case QEvent::MouseButtonRelease:
    case QEvent::MouseButtonDblClick:
    case QEvent::MouseMove:
        qDebug( "QUimInputContext::mouseHandler: "
                "x=%d, type=%d, button=%d, state=%d", x, type, button, state );
        break;
    default:
        break;
    }
}

void QUimInputContext::reset()
{
    qDebug( "QUimInputContext::reset()" );

    QInputContext::reset();
    preeditString = QString::null;
    candwinIsActive = FALSE;
    cwin->hide();
    uim_reset_context( m_uc );
}

QString QUimInputContext::identifierName()
{
    return ( QString( "uim-" ) + m_imname );
}

QString QUimInputContext::language()
{
    return m_lang;
}

// callbacks for uim
void QUimInputContext::commit_cb( void *ptr, const char *str )
{
    QString qs = QString::fromUtf8( str );
    qDebug( "commit_cb : str = |%s|", ( const char* ) qs.local8Bit() );

    QUimInputContext *ic = ( QUimInputContext * ) ptr;
    ic->commitString( qs );
}

void QUimInputContext::clear_cb( void *ptr )
{
    qDebug( "clear_cb" );

    QUimInputContext* ic = ( QUimInputContext* ) ptr;
    ic->clearPreedit();
}

void QUimInputContext::pushback_cb( void *ptr, int attr, const char *str )
{
    QString qs = QString::fromUtf8( str );
    qDebug( "pushback_cb :  str = |%s|", ( const char* ) qs.local8Bit() );

    if ( !str )
        return ;
    // Reject invalid empty string. UPreeditAttr_Cursor or
    // UPreeditAttr_Separator with empty string is *valid* and
    // required to work properly.
    if ( !strcmp( str, "" ) && !( attr & ( UPreeditAttr_Cursor | UPreeditAttr_Separator ) ) )
        return ;

    QUimInputContext* ic = ( QUimInputContext* ) ptr;
    ic->pushbackPreeditString( attr, qs );
}

void QUimInputContext::update_cb( void *ptr )
{
    qDebug( "update_cb" );

    QUimInputContext *ic = ( QUimInputContext* ) ptr;
    ic->updatePreedit();
}

void QUimInputContext::cand_activate_cb( void *ptr, int nr, int displayLimit )
{
    qDebug( "cand_activate_cb" );

    QUimInputContext *ic = ( QUimInputContext* ) ptr;
    ic->candidateActivate( nr, displayLimit );
}

void QUimInputContext::cand_select_cb( void *ptr, int index )
{
    qDebug( "cand_select_cb" );

    QUimInputContext *ic = ( QUimInputContext* ) ptr;
    ic->candidateSelect( index );
}

void QUimInputContext::cand_shift_page_cb( void *ptr, int direction )
{
    qDebug( "cand_shift_page_cb" );

    QUimInputContext *ic = ( QUimInputContext* ) ptr;
    CandidateWindow *cwin = ic->cwin;

    cwin->shiftPage( direction );
}

void QUimInputContext::cand_deactivate_cb( void *ptr )
{
    qDebug( "cand_deactivate_cb" );

    QUimInputContext *ic = ( QUimInputContext* ) ptr;
    ic->candidateDeactivate();
}

void QUimInputContext::commitString( const QString& str )
{
    if ( !isComposing() )
    {
        sendIMEvent( QEvent::IMStart );
    }

    preeditString = QString::null;
    sendIMEvent( QEvent::IMEnd, str );
}
void QUimInputContext::clearPreedit()
{
    if( !psegs.isEmpty() )
        psegs.clear();
}

void QUimInputContext::pushbackPreeditString( int attr, const QString& str )
{
    PreeditSegment * ps = new PreeditSegment( attr, str );
    psegs.append( ps );
}

void QUimInputContext::updatePreedit()
{
    QString newString = getPreeditString();
    int cursor = getPreeditCursorPosition();
    int selLength = getPreeditSelectionLength();

    if ( newString.isEmpty() && preeditString.isEmpty() && ! isComposing() )
        return ;

    // Activating the IM
    if ( ! newString.isEmpty() && ! isComposing() )
        sendIMEvent( QEvent::IMStart );

    if ( ! newString.isEmpty() )
    {
        qDebug( "cursor = %d, length = %d", cursor, newString.length() );
        sendIMEvent( QEvent::IMCompose, newString, cursor, selLength );
    }

    // Preedit's length is Zero, we should deactivate IM and
    // cancel the inputting, that is, sending IMEnd event with
    // empty string.
    if ( newString.isEmpty() && isComposing() )
        sendIMEvent( QEvent::IMEnd );

    preeditString = newString;
}

bool QUimInputContext::isPreeditRelocationEnabled()
{
    return ( language() == "ja" );
}

bool QUimInputContext::isPreeditPreservationEnabled()
{
    return ( language() == "ja" );
}

QString QUimInputContext::getPreeditString()
{
    QString pstr;

    QPtrList<PreeditSegment>::ConstIterator seg = psegs.begin();
    const QPtrList<PreeditSegment>::ConstIterator end = psegs.end();
    for ( ; seg != end; ++seg )
    {
        if ( ( *seg ) ->attr & UPreeditAttr_Separator && ( *seg ) ->str.isEmpty() )
        {
            pstr += DEFAULT_SEPARATOR_STR;
        }
        else
        {
            pstr += ( *seg ) ->str;
        }
    }

    return pstr;
}

int QUimInputContext::getPreeditCursorPosition()
{
    if ( cwin->isAlwaysLeftPosition() )
        return 0;

    int cursorPos = 0;
    QPtrList<PreeditSegment>::ConstIterator seg = psegs.begin();
    const QPtrList<PreeditSegment>::ConstIterator end = psegs.end();
    for ( ; seg != end; ++seg )
    {
        if ( ( *seg ) ->attr & UPreeditAttr_Cursor )
        {
            return cursorPos;
        }
        else if ( ( *seg ) ->attr & UPreeditAttr_Separator
                  && ( *seg ) ->str.isEmpty() )
        {
            cursorPos += QString( DEFAULT_SEPARATOR_STR ).length();
        }
        else
        {
            cursorPos += ( *seg ) ->str.length();
        }
    }

    return cursorPos;
}

int QUimInputContext::getPreeditSelectionLength()
{
    int selectionLength = 0;

    QPtrList<PreeditSegment>::ConstIterator seg = psegs.begin();
    const QPtrList<PreeditSegment>::ConstIterator end = psegs.end();
    for ( ; seg != end; ++seg )
    {
        // In converting state, uim encodes UPreeditAttr_Cursor into
        // selected segment rather than separated empty cursor
        // segment. So we can get selection length by length of this
        // 'selected segment'. Don't use visual attributes such as
        // UPreeditAttr_Underline or UPreeditAttr_Reverse to detect
        // logical selection length. They are sometimes disabled by
        // user preference.
        if ( ( *seg ) ->attr & UPreeditAttr_Cursor )
        {
            selectionLength = ( *seg ) ->str.length();
            return selectionLength;
        }
    }

    return 0;
}


void QUimInputContext::candidateActivate( int nr, int displayLimit )
{
    QValueList<uim_candidate> list;
    list.clear();

    cwin->activateCandwin( displayLimit );

    /* set candidates */
    uim_candidate cand;
    for ( int i = 0; i < nr; i++ )
    {
        cand = uim_get_candidate( m_uc, i, displayLimit ? i % displayLimit : i );
        list.append( cand );
    }
    cwin->setCandidates( displayLimit, list );

    cwin->popup();
    candwinIsActive = true;
}

void QUimInputContext::candidateSelect( int index )
{
    cwin->setIndex( index );
}

void QUimInputContext::candidateDeactivate()
{
    cwin->deactivateCandwin();

    candwinIsActive = false;
}

void QUimInputContext::createUimInfo()
{
    if ( !uimInfo.isEmpty() )
        return ;

    uim_context tmp_uc = uim_create_context( NULL, "UTF-8", NULL, NULL, uim_iconv, NULL );
    struct UIMInfo ui;
    int nr = uim_get_nr_im( tmp_uc );
    for ( int i = 0; i < nr; i++ )
    {
        ui.name = uim_get_im_name( tmp_uc, i );
        /* return value of uim_get_im_language() is an ISO 639-1
           compatible language code such as "ja". Since it is unfriendly
           for human reading, we convert it into friendly one by
           uim_get_language_name_from_locale() here */
        const char *langcode = uim_get_im_language( tmp_uc, i );
        ui.lang = uim_get_language_name_from_locale( langcode );
        ui.short_desc = uim_get_im_short_desc( tmp_uc, i );

        uimInfo.append( ui );
    }
    uim_release_context( tmp_uc );
}

void QUimInputContext::readIMConf()
{
    char * leftp = uim_symbol_value_str( "candidate-window-position" );
    if ( leftp && !strcmp( leftp, "left" ) )
        cwin->setAlwaysLeftPosition( true );
    else
        cwin->setAlwaysLeftPosition( false );
    free( leftp );
}
