/*

Copyright (c) 2003,2004 uim Project http://uim.freedesktop.org/

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
#include "candidatewindow.h"

#include <qapplication.h>
#include <qlabel.h>
#include <qheader.h>
#include <qfontmetrics.h>
#include <qevent.h>

#include "quiminputcontext.h"
#include "subwindow.h"

static const int MIN_CAND_WIDTH = 80;

static const int HEADING_COLUMN = 0;
static const int CANDIDATE_COLUMN = 1;
static const int ANNOTATION_COLUMN = 2;

const Qt::WFlags candidateFlag = ( Qt::WType_TopLevel
                                   | Qt::WStyle_Customize
                                   | Qt::WStyle_StaysOnTop
                                   | Qt::WStyle_NoBorder
                                   | Qt::WStyle_Tool
#if defined(Q_WS_X11)
                                   | Qt::WX11BypassWM
#endif
                                 );

CandidateWindow::CandidateWindow( QWidget *parent, const char * name )
        : QVBox( parent, name, candidateFlag )
{
    setFrameStyle( Raised | NoFrame );

    ic = NULL;

    //setup CandidateList
    cList = new CandidateListView( this, "candidateListView" );
    cList->setSorting( 0 );
    cList->setSelectionMode( QListView::Single );
    cList->addColumn( "0" );
    cList->setColumnWidthMode( 0, QListView::Maximum );
    cList->addColumn( "1" );
    cList->setColumnWidthMode( 1, QListView::Maximum );
    cList->header() ->hide();
    cList->setVScrollBarMode( QScrollView::AlwaysOff );
    cList->setHScrollBarMode( QScrollView::AlwaysOff );
    cList->setAllColumnsShowFocus( true );
    QObject::connect( cList, SIGNAL( clicked( QListViewItem * ) ),
                      this , SLOT( slotCandidateSelected( QListViewItem * ) ) );
    QObject::connect( cList, SIGNAL( selectionChanged( QListViewItem * ) ),
                      this , SLOT( slotHookSubwindow( QListViewItem * ) ) );

    //setup NumberLabel
    numLabel = new QLabel( this, "candidateLabel" );

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
}

void CandidateWindow::deactivateCandwin()
{
    subWin->cancelHook();

    hide();
    clearCandidates();
}

void CandidateWindow::clearCandidates()
{
    qDebug( "clear Candidates" );

    candidateIndex = -1;
    displayLimit = 0;
    nrCandidates = 0;

    // clear stored candidate datas
    for ( unsigned int i = 0; i < stores.size(); i++ )
        uim_candidate_free( stores[ i ] );
    stores.clear();
}


void CandidateWindow::setCandidates( int dl, const QValueList<uim_candidate> &candidates )
{
    qDebug( "setCandidates" );

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

void CandidateWindow::setPage( int page )
{
    qDebug( "setPage : page = %d", page );

    // clear items
    cList->clear();

    // calculate page
    int newpage, lastpage;
    lastpage = nrCandidates / displayLimit;
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
            newindex = newpage * displayLimit;
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
    for ( int i = 0; i < ncandidates; i++ )
    {
        uim_candidate cand = stores[ displayLimit * newpage + i ];
        QString headString = QString::fromUtf8( ( const char * ) uim_candidate_get_heading_label( cand ) );
        if ( ( headString.toInt() < 10 && headString.toInt() + displayLimit > 10 )
                || ( headString.toInt() < 100 && headString.toInt() + displayLimit > 100 ) )
            headString.prepend( "0" );
        QString candString = QString::fromUtf8( ( const char * ) uim_candidate_get_cand_str( cand ) );

        // 2004-12-13 Kazuki Ohta <mover@hct.zaq.ne.jp>
        // Commented out for the next release.
//        QString annotationString = QString::fromUtf8( ( const char * ) uim_candidate_get_annotation_str( cand ) );
        QString annotationString = "";

        // insert new item to the candidate list
        new QListViewItem( cList, headString, candString, annotationString );
    }

    // set index
    if ( newindex != candidateIndex )
        setIndex( newindex );

    // set candwin size
    adjustCandidateWindowSize();
}

void CandidateWindow::setIndex( int totalindex )
{
    qDebug( "setIndex : totalindex = %d", totalindex );

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
    QListViewItem * selectedItem = cList->itemAtIndex( index );
    cList->setSelected( selectedItem, true );

    slotCandidateSelected( selectedItem );
}


void CandidateWindow::slotCandidateSelected( QListViewItem * item )
{
    candidateIndex = ( pageIndex * displayLimit ) + cList->itemIndex( item );
    if ( ic && ic->uimContext() )
        uim_set_candidate_index( ic->uimContext(), candidateIndex );
    updateLabel();
}

void CandidateWindow::shiftPage( bool forward )
{
    qDebug( "candidateIndex = %d", candidateIndex );
    if ( forward )
    {
        candidateIndex += displayLimit;
        setPage( pageIndex + 1 );
    }
    else
    {
        if ( candidateIndex < displayLimit )
            candidateIndex = displayLimit * ( nrCandidates / displayLimit ) + candidateIndex;
        else
            candidateIndex -= displayLimit;

        setPage( pageIndex - 1 );
    }

    cList->setSelected( cList->itemAtIndex( candidateIndex % displayLimit ), true );
    if ( ic && ic->uimContext() )
        uim_set_candidate_index( ic->uimContext(), candidateIndex );
}

void CandidateWindow::layoutWindow( int x, int y, int w, int h )
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

void CandidateWindow::adjustCandidateWindowSize()
{
    qDebug( "adjustCandidateWindowSize()" );

    int width = 0;
    int height = 0;
    QListViewItem *item = cList->firstChild();
    if ( item )
        height = item->height() * ( cList->childCount() + 1 );

    // 2004-08-02 Kazuki Ohta <mover@hct.zaq.ne.jp>
    // FIXME!:
    //    There may be more proper way. Now width is adjusted by indeterminal 3 spaces.
    //    Using QWidget::adjustSize() seems not to work properly...
    unsigned int maxCharIndex = 0, maxCharCount = 0;
    for ( int i = 0; i < cList->childCount(); i++ )
    {
        if ( maxCharCount < cList->itemAtIndex( i ) ->text( 1 ).length() )
        {
            maxCharIndex = i;
            maxCharCount = cList->itemAtIndex( i ) ->text( 1 ).length();
        }
    }
    QFontMetrics fm( cList->font() );
    width = fm.width( cList->itemAtIndex( maxCharIndex ) ->text( 0 ) + "   " + cList->itemAtIndex( maxCharIndex ) ->text( 1 ) );
    if ( width < MIN_CAND_WIDTH )
        width = MIN_CAND_WIDTH;

    resize( width, height );
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

void CandidateWindow::slotHookSubwindow( QListViewItem * item )
{
    // cancel previous hook
    subWin->cancelHook();

    // hook annotation
    QString annotationString = item->text( 2 );
    if ( !annotationString.isEmpty() )
    {
        subWin->hookPopup( "Annotation", annotationString );
    }
}

// Moving and Resizing affects the position of Subwindow
void CandidateWindow::moveEvent( QMoveEvent *e )
{
    // move subwindow
    subWin->layoutWindow( e->pos().x() + width(), e->pos().y() );
}

void CandidateWindow::resizeEvent( QResizeEvent *e )
{
    // move subwindow
    subWin->layoutWindow( pos().x() + e->size().width(), pos().y() );
}
