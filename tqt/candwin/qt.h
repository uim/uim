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
#ifndef UIM_TQT_CANDWIN_QT_H
#define UIM_TQT_CANDWIN_QT_H

#include <uim/uim.h>
#include <uim/uim-helper.h>

#include <tqvbox.h>
#include <tqlistview.h>
#include <tqvaluelist.h>

class TQLabel;
class CandidateListView;
class TQStringList;
class TQPoint;

struct CandData
{
    TQString label;
    TQString str;
};

class CandidateWindow : public TQVBox
{
    TQ_OBJECT
public:
    CandidateWindow( TQWidget *parent = 0, const char * name = 0 );
    ~CandidateWindow();

    virtual TQSize sizeHint() const;

    void activateCand( const TQStringList &list );
    void selectCand( const TQStringList &list );
    void moveCand( const TQStringList &list );
    void showCand();
    void deactivateCand();

    void setNrCandidates( const TQStringList &list );
    void setPageCandidates( const TQStringList &list );
    void showPage( const TQStringList &list );

public slots:
    void slotStdinActivated( int );
    void slotCandidateSelected( TQListViewItem* );

protected:
    void strParse( const TQString& str );
    void adjustCandidateWindowSize();

    void setPage( int page );
    void setIndex( int index );

    void updateLabel();

protected:
    CandidateListView *cList;
    TQLabel *numLabel;

    TQValueList<CandData> stores;

    int nrCandidates;
    int candidateIndex;
    int displayLimit;
    int pageIndex;

    bool isActive;
    bool needHilite;
};

class CandidateListView : public TQListView
{
    TQ_OBJECT

public:
    CandidateListView( TQWidget *parent, const char *name = 0, WFlags f = 0 ) : TQListView( parent, name, f ) {}
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
            for ( ; it.current() && it.current() != item; ++it, ++j ) ;

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
        };
        return 0;
    }
};

#endif  /* UIM_TQT_CANDWIN_QT_H */
