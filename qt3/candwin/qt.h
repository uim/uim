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
#ifndef UIM_QT_CANDWIN_QT_H
#define UIM_QT_CANDWIN_QT_H

#include <uim/uim.h>
#include <uim/uim-helper.h>

#include <qvbox.h>
#include <qlistview.h>
#include <qvaluelist.h>

class QLabel;
class CandidateListView;
class QStringList;
class QPoint;

struct CandData
{
    QString label;
    QString str;
};

class CandidateWindow : public QVBox
{
    Q_OBJECT
public:
    CandidateWindow( QWidget *parent = 0, const char * name = 0 );
    ~CandidateWindow();

    void activateCand( const QStringList &list );
    void selectCand( const QStringList &list );
    void moveCand( const QStringList &list );
    void showCand();
    void deactivateCand();

    void setNrCandidates( const QStringList &list );
    void setPageCandidates( const QStringList &list );
    void showPage( const QStringList &list );

public slots:
    void slotStdinActivated( int );
    void slotCandidateSelected( QListViewItem* );

protected:
    void strParse( const QString& str );
    void adjustCandidateWindowSize();

    void setPage( int page );
    void setIndex( int index );

    void updateLabel();

protected:
    CandidateListView *cList;
    QLabel *numLabel;

    QValueList<CandData> stores;

    int nrCandidates;
    int candidateIndex;
    int displayLimit;
    int pageIndex;

    bool isActive;
    bool needHilite;
};

class CandidateListView : public QListView
{
    Q_OBJECT

public:
    CandidateListView( QWidget *parent, const char *name = 0, WFlags f = 0 ) : QListView( parent, name, f ) {}
    ~CandidateListView() {}


    int itemIndex( const QListViewItem *item ) const
    {
        if ( !item )
            return -1;

        if ( item == firstChild() )
            return 0;
        else
        {
            QListViewItemIterator it( firstChild() );
            uint j = 0;
            for ( ; it.current() && it.current() != item; ++it, ++j ) ;

            if ( !it.current() )
                return -1;
            return j;
        }
    }

    QListViewItem* itemAtIndex( int index )
    {
        if ( index < 0 )
            return 0;

        int j = 0;
        for ( QListViewItemIterator it = firstChild(); it.current(); ++it )
        {
            if ( j == index )
                return it.current();
            j++;
        };
        return 0;
    }
};

#endif  /* UIM_QT_CANDWIN_QT_H */
