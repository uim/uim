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

#include <tqapplication.h>
#include <tqlabel.h>
#include <tqheader.h>
#include <tqfontmetrics.h>
#include <tqevent.h>

#include "uim/uim.h"

#include "candidatewindow.h"
#include "quiminputcontext.h"
#include "subwindow.h"

static const int MIN_CAND_WIDTH = 80;

static const int HEADING_COLUMN = 0;
static const int CANDIDATE_COLUMN = 1;
static const int ANNOTATION_COLUMN = 2;

const TQt::WFlags candidateFlag = ( TQt::WType_TopLevel
                                   | TQt::WStyle_Customize
                                   | TQt::WStyle_StaysOnTop
                                   | TQt::WStyle_NoBorder
                                   | TQt::WStyle_Tool
#if defined(TQ_WS_X11)
                                   | TQt::WX11BypassWM
#endif
                                 );

CandidateWindow::CandidateWindow( TQWidget *parent, const char * name )
        : TQVBox( parent, name, candidateFlag )
{
    setFrameStyle( Raised | NoFrame );

    ic = NULL;

    //setup CandidateList
    cList = new CandidateListView( this, "candidateListView" );
    cList->setSorting( -1 );
    cList->setSelectionMode( TQListView::Single );
    cList->setMinimumWidth( MIN_CAND_WIDTH );
    cList->addColumn( "0" );
    cList->setColumnWidthMode( 0, TQListView::Maximum );
    cList->addColumn( "1" );
    cList->setColumnWidthMode( 1, TQListView::Maximum );
    cList->header() ->hide();
    cList->setVScrollBarMode( TQScrollView::AlwaysOff );
    cList->setHScrollBarMode( TQScrollView::AlwaysOff );
    cList->setAllColumnsShowFocus( true );
    TQObject::connect( cList, TQ_SIGNAL( clicked( TQListViewItem * ) ),
                      this , TQ_SLOT( slotCandidateSelected( TQListViewItem * ) ) );
    TQObject::connect( cList, TQ_SIGNAL( selectionChanged( TQListViewItem * ) ),
                      this , TQ_SLOT( slotHookSubwindow( TQListViewItem * ) ) );

    //setup NumberLabel
    numLabel = new TQLabel( this, "candidateLabel" );

    stores.clear();

    nrCandidates = 0;
    candidateIndex = -1;
    displayLimit = 0;
    pageIndex = -1;

    isAlwaysLeft = false;

    subWin = new SubWindow( 0 );
}

CandidateWindow::~CandidateWindow()
{
    if ( !stores.isEmpty() )
    {
        // clear stored candidate datas
        for ( unsigned int i = 0; i < stores.size(); i++ )
	{
	    if ( stores[ i ] )
		uim_candidate_free( stores[ i ] );
	}
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
}

void CandidateWindow::deactivateCandwin()
{
    subWin->cancelHook();

    hide();
    clearCandidates();
}

void CandidateWindow::clearCandidates()
{
#ifdef ENABLE_DEBUG
    tqDebug( "clear Candidates" );
#endif

    candidateIndex = -1;
    displayLimit = 0;
    nrCandidates = 0;

    // clear stored candidate datas
    for ( unsigned int i = 0; i < stores.size(); i++ )
    {
	if ( stores[ i ] )
	    uim_candidate_free( stores[ i ] );
    }
    stores.clear();
}


void CandidateWindow::setCandidates( int dl, const TQValueList<uim_candidate> &candidates )
{
#ifdef ENABLE_DEBUG
    tqDebug( "setCandidates" );
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

#if UIM_TQT_USE_NEW_PAGE_HANDLING
void CandidateWindow::setNrCandidates( int nrCands, int dLimit )
{
#ifdef ENABLE_DEBUG
    tqDebug( "setNrCandidates" );
#endif
    // remove old data
    if ( !stores.isEmpty() )
	clearCandidates();

    candidateIndex = -1;
    displayLimit = dLimit;
    nrCandidates = nrCands;
    pageIndex = 0;

    // setup dummy candidates
    for ( int i = 0; i < nrCandidates; i++ )
    {
	uim_candidate d = NULL;
	stores.append( d );
    }

    if ( !subWin )
	subWin = new SubWindow( this );
}

void CandidateWindow::setPageCandidates( int page, const TQValueList<uim_candidate> &candidates )
{
#ifdef ENABLE_DEBUG
    tqDebug( "setPageCandidates" );
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
#endif /* UIM_TQT_USE_NEW_PAGE_HANDLING */

void CandidateWindow::setPage( int page )
{
#ifdef ENABLE_DEBUG
    tqDebug( "setPage : page = %d", page );
#endif

    // clear items
    cList->clear();
    cList->setColumnWidth(0, 1);
    cList->setColumnWidth(1, 1);

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
        TQString headString = TQString::fromUtf8( ( const char * ) uim_candidate_get_heading_label( cand ) );
        TQString candString = TQString::fromUtf8( ( const char * ) uim_candidate_get_cand_str( cand ) );

        TQString annotationString = TQString::fromUtf8( ( const char * ) uim_candidate_get_annotation_str( cand ) );

        // insert new item to the candidate list
        new TQListViewItem( cList, headString, candString, annotationString );
    }

    // set index
    if ( newindex != candidateIndex )
        setIndex( newindex );
    else
        updateLabel();

    // size adjustment
    adjustSize();
}

void CandidateWindow::setIndex( int totalindex )
{
#ifdef ENABLE_DEBUG
    tqDebug( "setIndex : totalindex = %d", totalindex );
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
    TQListViewItem * selectedItem = cList->itemAtIndex( index );
    cList->setSelected( selectedItem, true );

    slotCandidateSelected( selectedItem );
}


void CandidateWindow::slotCandidateSelected( TQListViewItem * item )
{
    candidateIndex = ( pageIndex * displayLimit ) + cList->itemIndex( item );
    if ( ic && ic->uimContext() )
        uim_set_candidate_index( ic->uimContext(), candidateIndex );
    updateLabel();
}

void CandidateWindow::shiftPage( bool forward )
{
#ifdef ENABLE_DEBUG
    tqDebug( "candidateIndex = %d", candidateIndex );
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

    int screenW = TQApplication::desktop() ->screenGeometry().width();
    int screenH = TQApplication::desktop() ->screenGeometry().height();

    if ( destX + width() > screenW )
        destX = screenW - width();

    if ( destY + height() > screenH )
        destY = y - height();

    move( destX, destY );
}

void CandidateWindow::updateLabel()
{
    TQString indexString = TQString::null;
    if ( candidateIndex >= 0 )
        indexString = TQString::number( candidateIndex + 1 ) + " / " + TQString::number( nrCandidates );
    else
        indexString = "- / " + TQString::number( nrCandidates );

    numLabel->setText( indexString );
}

void CandidateWindow::slotHookSubwindow( TQListViewItem * item )
{
    // cancel previous hook
    subWin->cancelHook();

    // hook annotation
    TQString annotationString = item->text( 2 );
    if ( !annotationString.isEmpty() )
    {
        subWin->hookPopup( "Annotation", annotationString );
    }
}

// Moving and Resizing affects the position of Subwindow
void CandidateWindow::moveEvent( TQMoveEvent *e )
{
    // move subwindow
    subWin->layoutWindow( e->pos().x() + width(), e->pos().y() );
}

void CandidateWindow::resizeEvent( TQResizeEvent *e )
{
    // move subwindow
    subWin->layoutWindow( pos().x() + e->size().width(), pos().y() );
}


TQSize CandidateWindow::sizeHint( void ) const
{
    TQSize cListSizeHint = cList->sizeHint();

    int width = cListSizeHint.width();
    int height = cListSizeHint.height() + numLabel->height();

    return TQSize( width, height );
}

#include "candidatewindow.moc"
