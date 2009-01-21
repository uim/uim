/*

Copyright (c) 2003-2009 uim Project http://code.google.com/p/uim/

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

//#include <config.h>

#include <qapplication.h>
#include <QDesktopWidget>
#include <qlabel.h>
#include <Q3Header>
#include <qfontmetrics.h>
#include <qevent.h>

#include "uim/uim.h"

#include "debug.h"
#include "candidatewindow.h"
#include "subwindow.h"
#include "quiminputcontext.h"

static const int MIN_CAND_WIDTH = 80;

static const int HEADING_COLUMN = 0;
static const int CANDIDATE_COLUMN = 1;
static const int ANNOTATION_COLUMN = 2;

const Qt::WFlags candidateFlag = ( Qt::Window
                                   | Qt::WindowStaysOnTopHint
                                   | Qt::FramelessWindowHint
                                   | Qt::Tool
#if defined(Q_WS_X11)
                                   | Qt::X11BypassWindowManagerHint
#endif
                                 );

CandidateWindow::CandidateWindow( QWidget *parent, const char * name )
        : Q3VBox( parent, name, candidateFlag )
{
    setFrameStyle( Raised | NoFrame );

    ic = NULL;

    //setup CandidateList
    cList = new CandidateListView( this, "candidateListView" );
    cList->setSorting( -1 );
    cList->setSelectionMode( Q3ListView::Single );
    cList->addColumn( "0" );
    cList->setColumnWidthMode( 0, Q3ListView::Maximum );
    cList->addColumn( "1" );
    cList->setColumnWidthMode( 1, Q3ListView::Maximum );
    cList->header() ->hide();
    cList->setVScrollBarMode( Q3ScrollView::AlwaysOff );
    cList->setHScrollBarMode( Q3ScrollView::AlwaysOff );
    cList->setAllColumnsShowFocus( true );
    QSizePolicy sp( QSizePolicy::Preferred, QSizePolicy::Preferred, false );
    cList->setSizePolicy( sp );
    QObject::connect( cList, SIGNAL( clicked( Q3ListViewItem * ) ),
                      this , SLOT( slotCandidateSelected( Q3ListViewItem * ) ) );
    QObject::connect( cList, SIGNAL( selectionChanged( Q3ListViewItem * ) ),
                      this , SLOT( slotHookSubwindow( Q3ListViewItem * ) ) );

    //setup NumberLabel
    numLabel = new QLabel( this, "candidateLabel" );
    numLabel->adjustSize();

    stores.clear();

    nrCandidates = 0;
    candidateIndex = -1;
    displayLimit = 0;
    pageIndex = -1;

    isAlwaysLeft = false;

    subWin = NULL;
}

CandidateWindow::~CandidateWindow()
{
    if ( !stores.isEmpty() )
    {
        // clear stored candidate datas
        for ( int i = 0; i < stores.size(); i++ )
            uim_candidate_free( stores[ i ] );
        stores.clear();
    }
}

void CandidateWindow::popup()
{
    raise();
    show();
}

void CandidateWindow::activateCandwin( int dLimit )
{
    candidateIndex = -1;
    displayLimit = dLimit;
    pageIndex = 0;

    if ( !subWin )
        subWin = new SubWindow( this );
}

void CandidateWindow::deactivateCandwin()
{
    if ( subWin )
        subWin->cancelHook();

    hide();
    clearCandidates();
}

void CandidateWindow::clearCandidates()
{
#ifdef ENABLE_DEBUG
    qDebug( "clear Candidates" );
#endif

    candidateIndex = -1;
    displayLimit = 0;
    nrCandidates = 0;

    // clear stored candidate datas
    for ( int i = 0; i < stores.size(); i++ )
    {
	if ( stores[ i ] )
	    uim_candidate_free( stores[ i ] );
    }
    stores.clear();
}


void CandidateWindow::setCandidates( int dl, const Q3ValueList<uim_candidate> &candidates )
{
#ifdef ENABLE_DEBUG
    qDebug( "setCandidates" );
#endif

    // remove old data
    if ( !stores.isEmpty() )
        clearCandidates();

    // set defalt value
    candidateIndex = -1;
    nrCandidates = candidates.count();
    displayLimit = dl;

    if ( candidates.isEmpty() )
        return ;

    // set candidates
    stores = candidates;

    // shift to default page
    setPage( 0 );
}

#if UIM_QT_USE_NEW_PAGE_HANDLING
void CandidateWindow::setNrCandidates( int nrCands, int dLimit )
{
#ifdef ENABLE_DEBUG
    qDebug( "setNrCandidates" );
#endif

    // remove old data
    if ( !stores.isEmpty() )
        clearCandidates();

    candidateIndex = -1;
    displayLimit = dLimit;
    nrCandidates = nrCands;
    pageIndex = 0;

    // setup dummy candidate
    for ( int i = 0; i < nrCandidates; i++ )
    {
	uim_candidate d = NULL;
	stores.append( d );
    }

    if ( !subWin )
        subWin = new SubWindow( this );
}

void CandidateWindow::setPageCandidates( int page, const Q3ValueList<uim_candidate> &candidates )
{
#ifdef ENABLE_DEBUG
    qDebug( "setPageCandidates" );
#endif

    if ( candidates.isEmpty() )
        return;

    // set candidates
    int i, start, pageNr;
    start = page * displayLimit;

    if ( displayLimit && ( nrCandidates - start ) > displayLimit )
	pageNr = displayLimit;
    else
	pageNr = nrCandidates - start;

    for ( i = 0; i < pageNr; i++ )
	stores[ start + i ] = candidates[ i ];
}
#endif /* UIM_QT_USE_NEW_PAGE_HANDLING */

void CandidateWindow::setPage( int page )
{
#ifdef ENABLE_DEBUG
    qDebug( "setPage : page = %d", page );
#endif

    // clear items
    cList->clear();

    // calculate page
    int newpage, lastpage;
    if ( displayLimit )
        lastpage = nrCandidates / displayLimit;
    else
        lastpage = 0;

    if ( page < 0 )
    {
        newpage = lastpage;
    }
    else if ( page > lastpage )
    {
        newpage = 0;
    }
    else
    {
        newpage = page;
    }

    pageIndex = newpage;

    // calculate index
    int newindex;
    if ( displayLimit )
    {
        if ( candidateIndex >= 0 )
            newindex = ( newpage * displayLimit ) + ( candidateIndex % displayLimit );
        else
            newindex = -1;
    }
    else
    {
        newindex = candidateIndex;
    }

    if ( newindex >= nrCandidates )
        newindex = nrCandidates - 1;

    // set cand items
    //
    // If we switch to last page, the number of items to be added
    // is lower than displayLimit.
    //
    // ex. if nrCandidate==14 and displayLimit==10, the number of
    //     last page's item==4
    int ncandidates = displayLimit;
    if ( newpage == lastpage )
        ncandidates = nrCandidates - displayLimit * lastpage;
    for ( int i = ncandidates - 1; i >= 0; i-- )
    {
        uim_candidate cand = stores[ displayLimit * newpage + i ];
        QString headString = QString::fromUtf8( ( const char * ) uim_candidate_get_heading_label( cand ) );
        QString candString = QString::fromUtf8( ( const char * ) uim_candidate_get_cand_str( cand ) );

        // 2004-12-13 Kazuki Ohta <mover@hct.zaq.ne.jp>
        // Commented out for the next release.
//        QString annotationString = QString::fromUtf8( ( const char * ) uim_candidate_get_annotation_str( cand ) );
        QString annotationString = "";

        // insert new item to the candidate list
        new Q3ListViewItem( cList, headString, candString, annotationString );
    }

    // set index
    if ( newindex != candidateIndex )
        setIndex( newindex );
    else
        updateLabel();

    // size adjustment
    cList->updateGeometry();
    adjustSize();
}

void CandidateWindow::setIndex( int totalindex )
{
#ifdef ENABLE_DEBUG
    qDebug( "setIndex : totalindex = %d", totalindex );
#endif

    // validity check
    if ( totalindex < 0 )
        candidateIndex = nrCandidates - 1;
    else if ( totalindex >= nrCandidates )
        candidateIndex = 0;
    else
        candidateIndex = totalindex;

    // set page
    int newpage = 0;
    if ( displayLimit )
        newpage = ( int ) candidateIndex / displayLimit;
    if ( pageIndex != newpage )
        setPage( newpage );

    // select item
    if ( candidateIndex >= 0 )
    {
        int pos = totalindex;
        if ( displayLimit )
            pos = candidateIndex % displayLimit;

        if ( cList->itemAtIndex( pos ) && ! ( cList->itemAtIndex( pos ) ->isSelected() ) )
            cList->setSelected( cList->itemAtIndex( pos ), true );
    }
    else
    {
        cList->clearSelection();
    }

    updateLabel();
}

void CandidateWindow::setIndexInPage( int index )
{
    Q3ListViewItem * selectedItem = cList->itemAtIndex( index );
    cList->setSelected( selectedItem, true );

    slotCandidateSelected( selectedItem );
}


void CandidateWindow::slotCandidateSelected( Q3ListViewItem * item )
{
    candidateIndex = ( pageIndex * displayLimit ) + cList->itemIndex( item );
    if ( ic && ic->uimContext() )
        uim_set_candidate_index( ic->uimContext(), candidateIndex );
    updateLabel();
}

void CandidateWindow::shiftPage( bool forward )
{
#ifdef ENABLE_DEBUG
    qDebug( "candidateIndex = %d", candidateIndex );
#endif
    
    if ( forward )
    {
        if ( candidateIndex != -1 )
            candidateIndex += displayLimit;
        setPage( pageIndex + 1 );
    }
    else
    {
        if (candidateIndex != -1 ) {
            if ( candidateIndex < displayLimit )
                candidateIndex = displayLimit * ( nrCandidates / displayLimit ) + candidateIndex;
            else
                candidateIndex -= displayLimit;
        }

        setPage( pageIndex - 1 );
    }

    if ( candidateIndex != -1 ) {
        if ( displayLimit )
            cList->setSelected( cList->itemAtIndex( candidateIndex % displayLimit ), true );
        else
            cList->setSelected( cList->itemAtIndex( candidateIndex ), true );
    }
    if ( ic && ic->uimContext() && candidateIndex != -1 )
        uim_set_candidate_index( ic->uimContext(), candidateIndex );
}

void CandidateWindow::layoutWindow( int x, int y, int /* w */, int h )
{
    int destX = x;
    int destY = y + h;

    int screenW = QApplication::desktop() ->screenGeometry().width();
    int screenH = QApplication::desktop() ->screenGeometry().height();

    if ( destX + width() > screenW )
        destX = screenW - width();

    if ( destY + height() > screenH )
        destY = y - height();

    move( destX, destY );
}

void CandidateWindow::updateLabel()
{
    QString indexString = QString::null;
    if ( candidateIndex >= 0 )
        indexString = QString::number( candidateIndex + 1 ) + " / " + QString::number( nrCandidates );
    else
        indexString = "- / " + QString::number( nrCandidates );

    numLabel->setText( indexString );
}

void CandidateWindow::slotHookSubwindow( Q3ListViewItem * item )
{
    if ( subWin ) {
        // cancel previous hook
        subWin->cancelHook();

        // hook annotation
        QString annotationString = item->text( 2 );
        if ( !annotationString.isEmpty() )
        {
            subWin->hookPopup( "Annotation", annotationString );
        }
    }
}

// Moving and Resizing affects the position of Subwindow
void CandidateWindow::moveEvent( QMoveEvent *e )
{
    // move subwindow
    if ( subWin )
        subWin->layoutWindow( e->pos().x() + width(), e->pos().y() );
}

void CandidateWindow::resizeEvent( QResizeEvent *e )
{
    // move subwindow
    if ( subWin )
        subWin->layoutWindow( pos().x() + e->size().width(), pos().y() );
}


QSize CandidateWindow::sizeHint( void ) const
{
    QSize cListSizeHint = cList->sizeHint();

    int width = cListSizeHint.width();
    int height = cListSizeHint.height() + numLabel->height();

    return QSize( width, height );
}

QSize CandidateListView::sizeHint( void ) const
{
    if(childCount() == 0)
        return QSize( MIN_CAND_WIDTH, 0 );
    
    int width = 0;
    int height = 0;
    Q3ListViewItem *item = firstChild();
    if ( item )
        height = item->height() * childCount() + 3;
    
    // 2004-08-02 Kazuki Ohta <mover@hct.zaq.ne.jp>
    // FIXME!:
    //    There may be more proper way. Now width is adjusted by indeterminal 5 'J'.
    int maxCharIndex = 0, maxCharCount = 0;
    for ( int i = 0; i < childCount(); i++ )
    {
        if ( maxCharCount < itemAtIndex( i )->text( 1 ).length() )
        {
            maxCharIndex = i;
            maxCharCount = itemAtIndex( i )->text( 1 ).length();
        }
    }
    QFontMetrics fm( font() );
    width = fm.width( itemAtIndex( maxCharIndex )->text( 0 ) + "JJJJJ" + itemAtIndex( maxCharIndex )->text( 1 ) ) + itemMargin() * 4 + frameWidth() * 2;

    if ( width < MIN_CAND_WIDTH )
        width = MIN_CAND_WIDTH;
    
    return QSize( width, height );
}
