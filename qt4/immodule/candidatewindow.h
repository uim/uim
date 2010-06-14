/*

Copyright (c) 2003-2010 uim Project http://code.google.com/p/uim/

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
#ifndef UIM_QT4_IMMODULE_CANDIDATE_WINDOW_H
#define UIM_QT4_IMMODULE_CANDIDATE_WINDOW_H

#include <QtGui/QTableWidget>

#include "abstractcandidatewindow.h"

class CandidateListView;
class SubWindow;

class CandidateWindow : public AbstractCandidateWindow
{
    Q_OBJECT

public:
    explicit CandidateWindow( QWidget *parent );

    QSize sizeHint() const;

private slots:
    void slotCandidateSelected( int row );
    void slotHookSubwindow();

private:
    void activateCandwin( int dLimit );

    void setPage( int page );
    void shiftPage( bool forward );
    void setIndex( int totalindex );

    void setNrCandidates( int nrCands, int dLimit );

    // Moving and Resizing affects the position of Subwindow
    virtual void moveEvent( QMoveEvent * );
    virtual void resizeEvent( QResizeEvent * );
    virtual void hideEvent( QHideEvent *event );

    // widgets
    CandidateListView *cList;
    SubWindow *subWin;


    // candidate data
    QList<QString> annotations;

    // config
    const bool hasAnnotation;
};


class CandidateListView : public QTableWidget
{
    Q_OBJECT

public:
    explicit CandidateListView( QWidget *parent = 0 )
        : QTableWidget( parent ) {}
    ~CandidateListView() {}

    QSize sizeHint() const;
};
#endif /* Not def: UIM_QT4_IMMODULE_CANDIDATE_WINDOW_H */
