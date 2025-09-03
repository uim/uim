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
#include <tqwidget.h>
#include <tqheader.h>
#include <tqsocketnotifier.h>
#include <tqstringlist.h>
#include <tqtextcodec.h>
#include <tqrect.h>

#include <clocale>
#include <cstdio>
#include <cstring>
#include <unistd.h>
#include <cstdlib>


#include "qtgettext.h"
#include "qt.h"

static const int NR_CANDIDATES = 10;
static const int MIN_CAND_WIDTH = 80;

const TQt::WFlags candidateFlag = ( TQt::WType_TopLevel
                                   | TQt::WStyle_Customize
                                   | TQt::WStyle_StaysOnTop
                                   | TQt::WStyle_NoBorder
                                   | TQt::WStyle_Tool
#if defined(TQ_WS_X11)
                                   | TQt::WX11BypassWM
#endif
                                 );
static TQSocketNotifier *notifier = NULL;

CandidateWindow::CandidateWindow( TQWidget *parent, const char * name )
        : TQVBox( parent, name, candidateFlag )
{
    setFrameStyle( Raised | NoFrame );
    setFocusPolicy( TQWidget::NoFocus );

    //setup CandidateList
    cList = new CandidateListView( this, "candidateListView" );
    cList->setSorting( -1 );
    cList->setSelectionMode( TQListView::Single );
    cList->setMinimumWidth( MIN_CAND_WIDTH );
    cList->addColumn( "1" );
    cList->setColumnWidthMode( 0, TQListView::Maximum );
    cList->addColumn( "2" );
    cList->setColumnWidthMode( 1, TQListView::Maximum );
    cList->header() ->hide();
    cList->setVScrollBarMode( TQScrollView::AlwaysOff );
    cList->setHScrollBarMode( TQScrollView::AlwaysOff );
    cList->setAllColumnsShowFocus( true );
    TQObject::connect( cList, TQ_SIGNAL( clicked( TQListViewItem * ) ),
                      this , TQ_SLOT( slotCandidateSelected( TQListViewItem * ) ) );

    //setup NumberLabel
    numLabel = new TQLabel( this, "candidateLabel" );
    numLabel->setFocusPolicy( TQWidget::NoFocus );

    nrCandidates = 0;
    candidateIndex = 0;
    displayLimit = NR_CANDIDATES;
    pageIndex = -1;

    isActive = false;

    notifier = new TQSocketNotifier( 0, TQSocketNotifier::Read );
    TQObject::connect( notifier, TQ_SIGNAL( activated( int ) ),
                      this, TQ_SLOT( slotStdinActivated( int ) ) );
    hide();
}

CandidateWindow::~CandidateWindow()
{
    if ( !stores.isEmpty() )
        stores.clear();
}

void CandidateWindow::activateCand( const TQStringList &list )
{
#if defined(ENABLE_DEBUG)
    tqDebug( "uim-candwin-tqt: activateCand()" );
#endif
    /**
     * format: activate\fcharset=$charset\fdisplay_limit=$value\fhead1\acand1\aannot1\fhead2\acand2\aannot2\fhead3\acand3\aannot3\f
     */

    // remove old data
    cList->clear();
    stores.clear();

    // get charset and create codec
    TQTextCodec *codec = NULL;
    if ( !list[ 1 ].isEmpty() && list[ 1 ].startsWith( "charset" ) )
    {
        const TQStringList l = TQStringList::split( "=", list[ 1 ] );
        codec = TQTextCodec::codecForName( l[ 1 ].local8Bit() );
    }

    // get display_limit
    if ( !list[ 2 ].isEmpty() && list[ 2 ].startsWith( "display_limit" ) )
    {
        const TQStringList l = TQStringList::split( "=", list[ 2 ] );
        displayLimit = l[ 1 ].toInt();
    }

    for ( int i = 3; !list[ i ].isNull(); i++ )
    {
        // case list[i] = ""
        if ( list[ i ].isEmpty() )
            break;

        // split heading_label and cand_str
        TQStringList l = TQStringList::split( "\a", list [ i ], true );

        // store data
        CandData d;
        TQString headString;
        if ( codec )
            headString = codec->toUnicode( l [ 0 ].local8Bit() );
        else
            headString = l [ 0 ];

        d.label = headString;

	l.pop_front();
	TQString candString = l [ 0 ];

        if ( codec )
            d.str = codec->toUnicode( candString.local8Bit() );
        else
            d.str = candString;

	l.pop_front();
	TQString annotString = l [ 0 ];

        stores.append( d );
    }

    // set default value
    candidateIndex = -1;
    nrCandidates = stores.count();

    // shift to default page
    setPage( 0 );

    show();

    isActive = true;
}
void CandidateWindow::selectCand( const TQStringList &list )
{
#if defined(ENABLE_DEBUG)
    tqDebug( "uim-candwin-tqt: selectCand()" );
#endif
    const int index = list[ 1 ].toInt();
    needHilite = (list[ 2 ].toInt() == 1) ? true : false;
    setIndex( index );

    updateLabel();
}

void CandidateWindow::moveCand( const TQStringList &list )
{
#if defined(ENABLE_DEBUG)
    tqDebug( "uim-candwin-tqt: moveCand()" );
#endif
    if ( list[ 1 ].isEmpty() || list[ 2 ].isEmpty() )
        return ;

    const int topwin_x = list[ 1 ].toInt();
    const int topwin_y = list[ 2 ].toInt();
    const int cw_wi = width();
    const int cw_he = height();
    const int sc_wi = TQApplication::desktop() ->screenGeometry().width();
    const int sc_he = TQApplication::desktop() ->screenGeometry().height();

    int x, y;
    if ( sc_wi < topwin_x + cw_wi )
    {
        x = topwin_x - cw_wi;
    }
    else
    {
        x = topwin_x;
    }

    if ( sc_he < topwin_y + cw_he )
    {
        /* FIXME : How can I determine the preedit height? */
        y = topwin_y - cw_he - 20;
    }
    else
    {
        y = topwin_y;
    }

    move( x, y );
}

void CandidateWindow::showCand()
{
#if defined(ENABLE_DEBUG)
    tqDebug( "uim-candwin-tqt: showCand()" );
#endif
    if ( isActive )
        show();
}
void CandidateWindow::deactivateCand()
{
#if defined(ENABLE_DEBUG)
    tqDebug( "uim-candwin-tqt: deactivateCand()" );
#endif
    hide();
    isActive = false;
}
void CandidateWindow::setNrCandidates( const TQStringList &list )
{
#if defined(ENABLE_DEBUG)
    tqDebug( "uim-candwin-tqt: setNrCandidates()" );
#endif
    if ( list[ 1 ].isEmpty() || list[ 2 ].isEmpty() )
        return ;

    // remove old data
    cList->clear();
    stores.clear();

    // set default value
    candidateIndex = -1;
    nrCandidates = list[ 1 ].toInt();
    displayLimit = list[ 2 ].toInt();
    needHilite = false;
    isActive = true;

    // setup dummy stores
    for ( int i = 0; i < nrCandidates; i++ ) {
	CandData d;
	stores.append( d );
    }
}
void CandidateWindow::setPageCandidates( const TQStringList &list )
{
#if defined(ENABLE_DEBUG)
    tqDebug( "uim-candwin-tqt: setPageCandidates()" );
#endif
    /**
     * format: set_page_candidates\fcharset=$charset\fpage=$value\fhead1\acand1\aannot1\fhead2\acand2\aannot2\fhead3\acand3\aannot3\f
     */

    int page = 0;

    // get charset and create codec
    TQTextCodec *codec = NULL;
    if ( !list[ 1 ].isEmpty() && list[ 1 ].startsWith( "charset" ) )
    {
        const TQStringList l = TQStringList::split( "=", list[ 1 ] );
        codec = TQTextCodec::codecForName( l[ 1 ].local8Bit() );
    }

    // get page
    if ( !list[ 2 ].isEmpty() && list[ 2 ].startsWith( "page" ) )
    {
        const TQStringList l = TQStringList::split( "=", list[ 2 ] );
        page = l[ 1 ].toInt();
    }

    for ( int i = 3; !list[ i ].isNull(); i++ )
    {
        // case list[i] = ""
        if ( list[ i ].isEmpty() )
            break;

        // split heading_label and cand_str
        TQStringList l = TQStringList::split( "\a", list [ i ], true );

        // store data
        CandData &d = stores[page * displayLimit + i - 3];
        TQString headString;
        if ( codec )
            headString = codec->toUnicode( l [ 0 ].local8Bit() );
        else
            headString = l [ 0 ];

        d.label = headString;

	l.pop_front();
	TQString candString = l [ 0 ];

        if ( codec )
            d.str = codec->toUnicode( candString.local8Bit() );
        else
            d.str = candString;

	l.pop_front();
	TQString annotString = l [ 0 ];
    }
}
void CandidateWindow::showPage( const TQStringList &list )
{
#if defined(ENABLE_DEBUG)
    tqDebug( "uim-candwin-tqt: showPage()" );
#endif
    const int page = list[ 1 ].toInt();

    setPage( page );
    show();
}
void CandidateWindow::slotStdinActivated( int fd )
{
    char buf[ 4096 ];
    TQString message;

    while (uim_helper_fd_readable( fd ) > 0) {
        int n = read( fd, buf, 4096 - 1 );
        if ( n == 0 )
        {
            ::close( fd );
            ::exit( 1 );
        }
        if ( n == -1 )
            return ;
        buf[ n ] = '\0';
	message += TQString::fromUtf8( buf );
    }

    TQStringList msgList = TQStringList::split( "\f\f", message );

    TQStringList::Iterator it = msgList.begin();
    const TQStringList::Iterator end = msgList.end();
    for ( ; it != end; ++it )
        strParse( ( *it ) );
}

void CandidateWindow::strParse( const TQString& str )
{
#if defined(ENABLE_DEBUG)
    tqDebug( "str = %s", ( const char* ) str.local8Bit() );
#endif
    TQStringList list = TQStringList::split( "\f", str );

    TQStringList::Iterator it = list.begin();
    const TQStringList::Iterator end = list.end();
    for ( ; it != end; ++it )
    {
        if ( TQString::compare( "activate", ( *it ) ) == 0 )
            activateCand( list );
        else if ( TQString::compare( "select", ( *it ) ) == 0 )
            selectCand( list );
        else if ( TQString::compare( "show", ( *it ) ) == 0 )
            showCand();
        else if ( TQString::compare( "hide", ( *it ) ) == 0 )
            hide();
        else if ( TQString::compare( "move", ( *it ) ) == 0 )
            moveCand( list );
        else if ( TQString::compare( "deactivate", ( *it ) ) == 0 )
            deactivateCand();
        else if ( TQString::compare( "set_nr_candidates", ( *it ) ) == 0 )
            setNrCandidates( list );
        else if ( TQString::compare( "set_page_candidates", ( *it ) ) == 0 )
            setPageCandidates( list );
        else if ( TQString::compare( "show_page", ( *it ) ) == 0 )
            showPage( list );
    }
}

void CandidateWindow::slotCandidateSelected( TQListViewItem * item )
{
    candidateIndex = ( pageIndex * displayLimit ) + cList->itemIndex( item );

    // write message
    fprintf( stdout, "index\n" );
    fprintf( stdout, "%d\n\n", candidateIndex );
    fflush( stdout );

    updateLabel();
}

TQSize CandidateWindow::sizeHint() const
{
    constPolish();

    TQSize cListSizeHint = cList->sizeHint();

    int width = cListSizeHint.width();
    int height = cListSizeHint.height() + numLabel->height();
    return TQSize(width, height);
}

void CandidateWindow::setPage( int page )
{
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
    for ( int i = ncandidates - 1; i >=0 ; i-- )
    {
        TQString headString = stores[ displayLimit * newpage + i ].label;
        TQString candString = stores[ displayLimit * newpage + i ].str;

        // insert new item to the candidate list
        new TQListViewItem( cList, headString, candString );
    }

    // set index
    if ( newindex != candidateIndex )
        setIndex( newindex );
    else
        updateLabel();

    // set candwin size
    adjustSize();
}

void CandidateWindow::setIndex( int index )
{
#if defined(ENABLE_DEBUG)
    tqDebug( "setIndex : index = %d", index );
#endif
    // validity check
    if ( index < 0 )
        candidateIndex = nrCandidates - 1;
    else if ( index >= nrCandidates )
        candidateIndex = 0;
    else
        candidateIndex = index;

    // set page
    int newpage = 0;
    if ( displayLimit )
        newpage = ( int ) candidateIndex / displayLimit;
    if ( pageIndex != newpage )
        setPage( newpage );

    // select item
    if ( candidateIndex >= 0 && needHilite )
    {
        int pos = index;
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

void CandidateWindow::updateLabel()
{
    TQString indexString = TQString::null;
    if ( candidateIndex >= 0 && needHilite )
        indexString = TQString::number( candidateIndex + 1 ) + " / " + TQString::number( nrCandidates );
    else
        indexString = "- / " + TQString::number( nrCandidates );

    numLabel->setText( indexString );
}

int main( int argc, char *argv[] )
{
    setlocale(LC_ALL, "");
    bindtextdomain(PACKAGE, LOCALEDIR);
    textdomain(PACKAGE);
    bind_textdomain_codeset(PACKAGE, "UTF-8"); // ensure code encoding is UTF8-

    TQApplication a( argc, argv );

    CandidateWindow b;
    a.setMainWidget( &b );

    return a.exec();
}

#include "qt.moc"
