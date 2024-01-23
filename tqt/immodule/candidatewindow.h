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
#ifndef UIM_TQT_IMMODULE_CANDIDATE_WINDOW_H
#define UIM_TQT_IMMODULE_CANDIDATE_WINDOW_H

#include <uim/uim.h>

#include <tqvbox.h>
#include <tqlistview.h>
#include <tqvaluelist.h>
#include <tqevent.h>
#include <tqfontmetrics.h>

class TQLabel;

class QUimInputContext;
class CandidateListView;
class SubWindow;

class CandidateWindow : public TQVBox
{
    TQ_OBJECT

public:
    CandidateWindow( TQWidget *parent, const char * name = 0 );
    ~CandidateWindow();

    void activateCandwin( int dLimit );
    void deactivateCandwin();
    void clearCandidates();
    void popup();

    void setAlwaysLeftPosition( bool left ) { isAlwaysLeft = left; }
    bool isAlwaysLeftPosition() const { return isAlwaysLeft; }

    void setCandidates( int displayLimit, const TQValueList<uim_candidate> &candidates );
    void setPage( int page );
    void shiftPage( bool forward );
    void layoutWindow( int x, int y, int w, int h );
    void setIndex( int totalindex );
    void setIndexInPage( int index );

    void setNrCandidates( int nrCands, int dLimit );
    void setPageCandidates( int page, const TQValueList<uim_candidate> &candidates );

    void setQUimInputContext( QUimInputContext* m_ic ) { ic = m_ic; }

    TQSize sizeHint(void) const;

    int nrCandidates;
    int candidateIndex;
    int displayLimit;
    int pageIndex;
protected slots:
    void slotCandidateSelected( TQListViewItem* );
    void slotHookSubwindow( TQListViewItem* );

protected:
    void updateLabel();

    // Moving and Resizing affects the positon of Subwindow
    virtual void moveEvent( TQMoveEvent * );
    virtual void resizeEvent( TQResizeEvent * );

    QUimInputContext *ic;

    CandidateListView *cList;
    TQLabel *numLabel;

    TQValueList<uim_candidate> stores;

    bool isAlwaysLeft;

    SubWindow *subWin;
};


class CandidateListView : public TQListView
{
    TQ_OBJECT

public:
    CandidateListView( TQWidget *parent, const char *name = 0, TQt::WFlags f = 0 ) : TQListView( parent, name, f ) {}
    ~CandidateListView() {}

    int itemIndex( const TQListViewItem *item ) const
    {
        if ( !item )
            return -1;
        if ( item == firstChild() )
            return 0;
        else
        {
            TQListViewItemIterator it( firstChild() );
            uint j = 0;
            for ( ; it.current() && it.current() != item; ++it, ++j )
                ;
            if ( !it.current() )
                return -1;
            return j;
        }
    }

    TQListViewItem* itemAtIndex( int index ) const
    {
        if ( index < 0 )
            return 0;
        int j = 0;
        for ( TQListViewItemIterator it = firstChild(); it.current(); ++it )
        {
            if ( j == index )
                return it.current();
            j++;
        }

        return 0;
    }

};
#endif /* Not def: UIM_TQT_IMMODULE_CANDIDATE_WINDOW_H */
