/*

  copyright (c) 2010-2012 uim Project http://code.google.com/p/uim/

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
#ifndef UIM_QT4_IMMODULE_ABSTRACT_CANDIDATE_WINDOW_H
#define UIM_QT4_IMMODULE_ABSTRACT_CANDIDATE_WINDOW_H

#include <QtCore/QList>
#include <QtGui/QFrame>

#include <uim/uim.h>

#include "util.h"

class QLabel;
class QSocketNotifier;

class AbstractCandidateWindow : public QFrame
{
    Q_OBJECT

    public:
        explicit AbstractCandidateWindow(QWidget *parent);
        virtual ~AbstractCandidateWindow();

        void deactivateCandwin();
        void clearCandidates();
        void popup();

        void layoutWindow(int x, int y, int height);

        void candidateActivate(int nr, int displayLimit);
        void candidateSelect(int index);
        void candidateShiftPage(bool forward);

    protected:
        virtual void activateCandwin(int dLimit);

        virtual void shiftPage(bool forward);
        virtual void setIndex(int totalindex);
#ifdef UIM_QT_USE_NEW_PAGE_HANDLING
        virtual void setNrCandidates(int nrCands, int dLimit);
#endif
        virtual void updateView(int newpage, int ncandidates) = 0;
        virtual void updateSize() = 0;
        void updateLabel();

        // widget
        QLabel *numLabel;

        // candidate data
        QList<CandData> stores;
        int nrCandidates;
        int displayLimit;
        int candidateIndex;
        int pageIndex;
#ifdef UIM_QT_USE_NEW_PAGE_HANDLING
        QList<bool> pageFilled;
#endif

    private slots:
        void slotStdinActivated(int fd);

    private:
#ifdef UIM_QT_USE_DELAY
        void timerDone();
#endif /* !UIM_QT_USE_DELAY */
        void setCandidates(int displayLimit,
                const QList<CandData> &candidates);
        void setPage(int page);
#ifdef UIM_QT_USE_NEW_PAGE_HANDLING
        void setPageCandidates(int page,
                const QList<CandData> &candidates);
        void preparePageCandidates(int page);
#endif
        void setCandidateData(const QStringList &message);

        void moveCandwin(int x, int y);

        // candidate data
#ifdef UIM_QT_USE_NEW_PAGE_HANDLING
        int nrPages;
#endif

        QSocketNotifier *notifier;
};

#endif /* Not def: UIM_QT4_IMMODULE_ABSTRACT_CANDIDATE_WINDOW_H */
