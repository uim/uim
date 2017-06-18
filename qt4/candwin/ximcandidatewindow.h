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
#ifndef UIM_QT4_XIM_CANDWIN_QT_H
#define UIM_QT4_XIM_CANDWIN_QT_H

#include <QtCore/QList>
#if QT_VERSION < 0x050000
# include <QtGui/QFrame>
#else
# include <QtWidgets/QFrame>
#endif

#include "util.h"

class QLabel;
class QStringList;
class QTableWidget;

class XimCandidateWindow : public QFrame
{
    Q_OBJECT

public:
    explicit XimCandidateWindow(QWidget *parent = 0);
    ~XimCandidateWindow();

    void activateCand(const QStringList &list);
    void selectCand(const QStringList &list);
    void moveCand(const QStringList &list);
    void showCand();
    void deactivateCand();

    void setNrCandidates(const QStringList &list);
    void setPageCandidates(const QStringList &list);
    void showPage(const QStringList &list);

public slots:
    void slotStdinActivated(int);
    void slotCandidateSelected(int row);

protected:
    void adjustCandidateWindowSize();

    void setPage(int page);
    void setIndex(int index);

    void updateLabel();

protected:
    QTableWidget *cList;
    QLabel *numLabel;

    QList<CandData> stores;

    int nrCandidates;
    int candidateIndex;
    int displayLimit;
    int pageIndex;

    bool isActive;
    bool needHighlight;
};

#endif  /* UIM_QT4_XIM_CANDWIN_QT_H */
