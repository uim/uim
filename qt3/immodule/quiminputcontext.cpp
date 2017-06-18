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
#include <config.h>

#include <qnamespace.h>
#include <qevent.h>
#include <qglobal.h>
#include <qapplication.h>
#include <qpoint.h>
#include <qlabel.h>
#include <qvaluelist.h>

#include <ctype.h>
#include <string.h>
#include <stdlib.h>

#include "uim/uim.h"
#include "uim/uim-helper.h"
#include "uim/uim-im-switcher.h"
#include "uim/uim-scm.h"

#include "quiminputcontext.h"
#include "candidatewindow.h"
#include "qhelpermanager.h"
#include "quiminfomanager.h"
#include "plugin.h"
#include "qtextutil.h"
#ifdef Q_WS_X11
#include "quiminputcontext_compose.h"
#endif

#if UIM_QT_USE_JAPANESE_KANA_KEYBOARD_HACK
#include <X11/Xlib.h>

#include "uim/uim-x-util.h"
#endif

#define DEFAULT_SEPARATOR_STR "|"

QUimInputContext *focusedInputContext = NULL;
bool disableFocusedContext = false;

QPtrList<QUimInputContext> contextList;

QUimHelperManager * QUimInputContext::m_HelperManager = 0L;
#ifdef Q_WS_X11
DefTree *QUimInputContext::mTreeTop = NULL;
#endif
static int unicodeToUKey(ushort c);

// I think that current index-based query API of uim for language and
// input method name is useless and should be redesigned. I will
// suggest the change in future. -- YamaKen 2004-07-28

QUimInputContext::QUimInputContext( const char *imname, const char *lang )
        : QInputContext(), m_imname( imname ), m_lang( lang ), m_uc( 0 ),
        candwinIsActive( false )
{
#ifdef ENABLE_DEBUG
    qDebug( "QUimInputContext()" );
#endif

    contextList.append( this );

    // must be initialized before createUimContext() call
    if ( !m_HelperManager )
        m_HelperManager = new QUimHelperManager();

    if ( imname )
        m_uc = createUimContext( imname );

    psegs.setAutoDelete( true );
    psegs.clear();

    cwin = new CandidateWindow( 0 );
    cwin->setQUimInputContext( this );
    cwin->hide();

#ifdef Q_WS_X11
    if ( !mTreeTop )
        create_compose_tree();
    mCompose = new Compose( mTreeTop, this );
#endif
    mTextUtil = new QUimTextUtil( this );

    // read configuration
    readIMConf();
}

QUimInputContext::~QUimInputContext()
{
#ifdef ENABLE_DEBUG
    qDebug( "~QUimInputContext()" );
#endif

    contextList.remove( this );

    if ( m_uc )
        uim_release_context( m_uc );

    if ( this == focusedInputContext )
    {
        focusedInputContext = NULL;
        disableFocusedContext = true;
    }

#ifdef Q_WS_X11
    delete mCompose;
#endif
}

uim_context QUimInputContext::createUimContext( const char *imname )
{
    m_imname = imname;

    uim_context uc = uim_create_context( this, "UTF-8",
                                         NULL, ( char * ) imname,
                                         NULL,
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

    uim_prop_list_update( uc );

    return uc;
}

#ifdef Q_WS_X11
bool QUimInputContext::x11FilterEvent( QWidget *keywidget, XEvent *event )
{
    // to suppress warning
    keywidget = keywidget;
    event = event;

#if UIM_QT_USE_JAPANESE_KANA_KEYBOARD_HACK
    return uim_x_kana_input_hack_filter_event( m_uc, event );
#else
    return FALSE;
#endif
}
#endif // Q_WS_X11

bool QUimInputContext::filterEvent( const QEvent *event )
{
#ifdef ENABLE_DEBUG
    // qDebug("filterEvent");
#endif

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
#if defined(Q_WS_X11)
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
            if ( keyevent->state() & Qt::ControlButton &&
                 ( ascii >= 0x01 && ascii <= 0x1a ) )
                if ( keyevent->state() & Qt::ShiftButton )
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
            case Qt::Key_Multi_key: key = UKey_Multi_key; break;
            case Qt::Key_Mode_switch: key = UKey_Mode_switch; break;
            case Qt::Key_Codeinput: key = UKey_Codeinput; break;
            case Qt::Key_SingleCandidate: key = UKey_SingleCandidate; break;
            case Qt::Key_MultipleCandidate: key = UKey_MultipleCandidate; break;
            case Qt::Key_PreviousCandidate: key = UKey_PreviousCandidate; break;
            case Qt::Key_Shift: key = UKey_Shift_key; break;
            case Qt::Key_Control: key = UKey_Control_key; break;
            case Qt::Key_Alt: key = UKey_Alt_key; break;
            case Qt::Key_Meta: key = UKey_Meta_key; break;
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
            return FALSE;
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
            return FALSE;
#endif
    }

    return TRUE;
}

void QUimInputContext::setFocus()
{
#ifdef ENABLE_DEBUG
    qDebug( "QUimInputContext: %p->setFocus(), focusWidget()=%p",
            this, focusWidget() );
#endif

    focusedInputContext = this;
    disableFocusedContext = false;

    if ( candwinIsActive )
        cwin->popup();

    m_HelperManager->checkHelperConnection();

    uim_helper_client_focus_in( m_uc );
    uim_prop_list_update( m_uc );

    uim_focus_in_context( m_uc );
}

void QUimInputContext::unsetFocus()
{
#ifdef ENABLE_DEBUG
    qDebug( "QUimInputContext: %p->unsetFocus(), focusWidget()=%p",
            this, focusWidget() );
#endif
    uim_focus_out_context( m_uc );

    cwin->hide();

    m_HelperManager->checkHelperConnection();

    uim_helper_client_focus_out( m_uc );
}

QUimInputContext * QUimInputContext::focusedIC()
{
    return focusedInputContext;
}

void QUimInputContext::setMicroFocus( int x, int y, int w, int h, QFont * /* f */)
{
#ifdef ENABLE_DEBUG
    // qDebug("IC setMicroFocus (%d, %d), (%d, %d)", x, y, w, h);
#endif
    cwin->layoutWindow( x, y, w, h );
}

void QUimInputContext::mouseHandler( int /* x */, QEvent::Type type,
                                     Qt::ButtonState /* button */,
                                     Qt::ButtonState /* state */ )
{
    switch ( type )
    {
    case QEvent::MouseButtonPress:
    case QEvent::MouseButtonRelease:
    case QEvent::MouseButtonDblClick:
    case QEvent::MouseMove:
#ifdef ENABLE_DEBUG
        qDebug( "QUimInputContext::mouseHandler: "
                "x=%d, type=%d, button=%d, state=%d", x, type, button, state );
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

    QInputContext::reset();
    candwinIsActive = FALSE;
    cwin->hide();
    uim_reset_context( m_uc );
#ifdef Q_WS_X11
    mCompose->reset();
#endif
    clearPreedit();
    updatePreedit();
}

QString QUimInputContext::identifierName()
{
    return QString( "uim" );
}

QString QUimInputContext::language()
{
    return m_lang;
}

// callbacks for uim
void QUimInputContext::commit_cb( void *ptr, const char *str )
{
    QString qs = QString::fromUtf8( str );
#ifdef ENABLE_DEBUG
    qDebug( "commit_cb : str = |%s|", ( const char* ) qs.local8Bit() );
#endif
    QUimInputContext *ic = ( QUimInputContext * ) ptr;
    ic->commitString( qs );
}

void QUimInputContext::clear_cb( void *ptr )
{
#ifdef ENABLE_DEBUG
    qDebug( "clear_cb" );
#endif

    QUimInputContext* ic = ( QUimInputContext* ) ptr;
    ic->clearPreedit();
}

void QUimInputContext::pushback_cb( void *ptr, int attr, const char *str )
{
    QString qs = QString::fromUtf8( str );
#ifdef ENABLE_DEBUG
    qDebug( "pushback_cb :  str = |%s|", ( const char* ) qs.local8Bit() );
#endif
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
#ifdef ENABLE_DEBUG
    qDebug( "update_cb" );
#endif

    QUimInputContext *ic = ( QUimInputContext* ) ptr;
    ic->updatePreedit();
}

void QUimInputContext::cand_activate_cb( void *ptr, int nr, int displayLimit )
{
#ifdef ENABLE_DEBUG
    qDebug( "cand_activate_cb" );
#endif

    QUimInputContext *ic = ( QUimInputContext* ) ptr;
    ic->candidateActivate( nr, displayLimit );
}

void QUimInputContext::cand_select_cb( void *ptr, int index )
{
#ifdef ENABLE_DEBUG
    qDebug( "cand_select_cb" );
#endif

    QUimInputContext *ic = ( QUimInputContext* ) ptr;
    ic->candidateSelect( index );
}

void QUimInputContext::cand_shift_page_cb( void *ptr, int forward )
{
#ifdef ENABLE_DEBUG
    qDebug( "cand_shift_page_cb" );
#endif

    QUimInputContext *ic = ( QUimInputContext* ) ptr;
    ic->candidateShiftPage( (bool)forward );
}

void QUimInputContext::cand_deactivate_cb( void *ptr )
{
#ifdef ENABLE_DEBUG
    qDebug( "cand_deactivate_cb" );
#endif

    QUimInputContext *ic = ( QUimInputContext* ) ptr;
    ic->candidateDeactivate();
}

void QUimInputContext::switch_app_global_im_cb( void *ptr, const char *name )
{
    QUimInputContext *ic = ( QUimInputContext* ) ptr;
    ic->switch_app_global_im( name );
}

void QUimInputContext::switch_system_global_im_cb( void *ptr, const char *name )
{
    QUimInputContext *ic = ( QUimInputContext* ) ptr;
    ic->switch_system_global_im( name );
}

void QUimInputContext::commitString( const QString& str )
{
    if ( !isComposing() )
    {
        sendIMEvent( QEvent::IMStart );
    }

    sendIMEvent( QEvent::IMEnd, str );
}
void QUimInputContext::clearPreedit()
{
    if ( !psegs.isEmpty() )
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

    if ( newString.isEmpty() && ! isComposing() )
        return ;

    // Activating the IM
    if ( ! newString.isEmpty() && ! isComposing() )
        sendIMEvent( QEvent::IMStart );

    if ( ! newString.isEmpty() )
    {
#ifdef ENABLE_DEBUG
        qDebug( "cursor = %d, length = %d", cursor, newString.length() );
#endif
        sendIMEvent( QEvent::IMCompose, newString, cursor, selLength );
    }

    // Preedit's length is Zero, we should deactivate IM and
    // cancel the inputting, that is, sending IMEnd event with
    // empty string.
    if ( newString.isEmpty() && isComposing() )
        sendIMEvent( QEvent::IMEnd );
}

void QUimInputContext::saveContext()
{
    // just send IMEnd and keep preedit string
    if ( isComposing() )
        sendIMEvent( QEvent::IMEnd );
}

void QUimInputContext::restoreContext()
{
    updatePreedit();
}

bool QUimInputContext::isPreeditRelocationEnabled()
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
#if 0
    if ( cwin->isAlwaysLeftPosition() )
        return 0;
#endif

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


#if UIM_QT_USE_NEW_PAGE_HANDLING
void QUimInputContext::prepare_page_candidates( int page )
{
    QValueList<uim_candidate> list;
    list.clear();

    if ( page < 0 )
	return;

    if (pageFilled[ page ] )
	return;

    /* set page candidates */
    uim_candidate cand;
    int pageNr, start, nrCandidates, displayLimit;

    nrCandidates = cwin->nrCandidates;
    displayLimit = cwin->displayLimit;
    start = page * displayLimit;

    if ( displayLimit && ( nrCandidates - start ) > displayLimit )
	pageNr = displayLimit;
    else
	pageNr = nrCandidates - start;

    for ( int i = start; i < ( pageNr + start ); i++ )
    {
        cand = uim_get_candidate( m_uc, i, displayLimit ? i % displayLimit : i );
        list.append( cand );
    }
    pageFilled[ page ] = true;
    cwin->setPageCandidates( page, list );
}
#endif

void QUimInputContext::candidateActivate( int nr, int displayLimit )
{
    QValueList<uim_candidate> list;
    list.clear();

#if !UIM_QT_USE_NEW_PAGE_HANDLING
    cwin->activateCandwin( displayLimit );

    /* set candidates */
    uim_candidate cand;
    for ( int i = 0; i < nr; i++ )
    {
        cand = uim_get_candidate( m_uc, i, displayLimit ? i % displayLimit : i );
        list.append( cand );
    }
    cwin->setCandidates( displayLimit, list );

#else /* !UIM_QT_USE_NEW_PAGE_HANDLING */
    nrPages = displayLimit ? ( nr - 1 ) / displayLimit + 1 : 1;
    pageFilled.clear();
    for ( int i = 0; i < nrPages; i++ )
	pageFilled.append( false );

    cwin->setNrCandidates( nr, displayLimit );

    // set page candidates
    prepare_page_candidates( 0 );
    cwin->setPage( 0 );
#endif /* !UIM_QT_USE_NEW_PAGE_HANDLING */
    cwin->popup();
    candwinIsActive = true;
}

void QUimInputContext::candidateSelect( int index )
{
#if UIM_QT_USE_NEW_PAGE_HANDLING
    int new_page;

    if ( index >= cwin->nrCandidates )
	index = 0;

    if ( index >= 0 && cwin->displayLimit )
	new_page = index / cwin->displayLimit;
    else
	new_page = cwin->pageIndex;

    prepare_page_candidates( new_page );
#endif
    cwin->setIndex( index );
}

void QUimInputContext::candidateShiftPage( bool forward )
{
#if UIM_QT_USE_NEW_PAGE_HANDLING
    int new_page, index;

    index = forward ? cwin->pageIndex + 1 : cwin->pageIndex - 1;
    if ( index < 0 )
	new_page = nrPages - 1;
    else if ( index >= nrPages )
	new_page = 0;
    else
	new_page = index;

    prepare_page_candidates( new_page );
#endif
    cwin->shiftPage( forward );
}

void QUimInputContext::candidateDeactivate()
{
    cwin->deactivateCandwin();

    candwinIsActive = false;
}

void QUimInputContext::switch_app_global_im( const char *name )
{
    QUimInputContext *cc;
    QString im_name_sym;

    im_name_sym.sprintf( "'%s", name);

    for ( cc = contextList.first(); cc; cc = contextList.next() )
    {
        if (cc != this) {
            uim_switch_im( cc->uimContext(), name );
            cc->readIMConf();
        }
    }
    uim_prop_update_custom(this->uimContext(), "custom-preserved-default-im-name", ( const char* ) im_name_sym );
}

void QUimInputContext::switch_system_global_im( const char *name )
{
    switch_app_global_im( name );
    QUimHelperManager::send_im_change_whole_desktop( name );
}

void QUimInputContext::readIMConf()
{
    char * leftp = uim_scm_symbol_value_str( "candidate-window-position" );
    if ( leftp && !strcmp( leftp, "left" ) )
        cwin->setAlwaysLeftPosition( true );
    else
        cwin->setAlwaysLeftPosition( false );
    free( leftp );
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

#include "quiminputcontext.moc"
