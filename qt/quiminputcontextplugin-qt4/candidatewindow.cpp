#include "candidatewindow.h"

#include <qapplication.h>
#include <qdesktopwidget.h>
#include <qlabel.h>
#include <qevent.h>
#include <qlistwidget.h>

#include "quiminputcontext.h"

const Qt::WFlags candidateFlag = ( Qt::WType_TopLevel
                                   | Qt::WStyle_Customize
                                   | Qt::WStyle_StaysOnTop
                                   | Qt::WStyle_NoBorder
                                   | Qt::WStyle_Tool
#if defined(Q_WS_X11)
                                   | Qt::WX11BypassWM
#endif
                                 );

CandidateWindow::CandidateWindow( QWidget * parent )
        : QVBoxWidget( parent, candidateFlag ),
        ic( NULL ), nrCandidates( 0 ), candidateIndex( -1 ),
        displayLimit( 0 ), pageIndex( -1 ), isAlwaysLeft( false )
{
    // setup CandidateList
    cList = new QListWidget( this );
    cList->setSelectionMode( QAbstractItemView::SingleSelection );
    QObject::connect( cList, SIGNAL( clicked( QListWidgetItem * item, Qt::MouseButton button, Qt::KeyboardModifiers modifiers ) ),
                      this, SLOT( slotCandidateSelected( QListWidgetItem * item ) ) );

    // setup NumberLabel
    numLabel = new QLabel( this );

    stores.clear();
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
}

void CandidateWindow::deactivateCandwin()
{
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
    for ( int i = 0; i < stores.size(); i++ )
        uim_candidate_free( stores[ i ] );
    stores.clear();
}

void CandidateWindow::setCandidates( int dl, const QList<uim_candidate> &candidates )
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
        if ( headString.toInt() < 10 )
            headString.prepend( "0" );
        QString candString = QString::fromUtf8( ( const char * ) uim_candidate_get_cand_str( cand ) );

        // insert new item to the candidate list
        new QListWidgetItem( candString, cList );
    }

    // set index
    setIndex( newindex );

    // set candwin size
    //    adjustCandidateWindowSize();
}

void CandidateWindow::setIndex( int totalindex )
{
    qDebug( "setTotalIndex : totalindex = %d", totalindex );

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

        if ( cList->item( pos ) && ! ( cList->isSelected( cList->item( pos ) ) ) )
            cList->setSelected( cList->item( pos ), true );
    }
    else
    {
        //        cList->clearSelection();
    }
}

void CandidateWindow::setIndexInPage( int index )
{
    QListWidgetItem * selectedItem = cList->item( index );
    cList->setSelected( selectedItem, true );

    slotCandidateSelected( selectedItem );
}

void CandidateWindow::shiftPage( bool forward )
{
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
        setPage( pageIndex - 1 );
    }

    //    cList->setSelected(cList->itemAtIndex(candidateIndex%displayLimit), true);
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

void CandidateWindow::updateLabel()
{
    QString indexString = QString::null;
    if ( candidateIndex >= 0 )
        indexString = QString::number( candidateIndex + 1 ) + " / " + QString::number( nrCandidates );
    else
        indexString = "- / " + QString::number( nrCandidates );

    numLabel->setText( indexString );
}

void CandidateWindow::slotCandidateSelected( QListWidgetItem * item )
{
    candidateIndex = ( pageIndex * displayLimit ) + cList->row( item );
    if ( ic && ic->uimContext() )
        uim_set_candidate_index( ic->uimContext(), candidateIndex );
    updateLabel();
}
