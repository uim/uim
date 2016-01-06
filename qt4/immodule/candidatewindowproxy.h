/*

  copyright (c) 2012-2013 uim Project https://github.com/uim/uim

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
#ifndef UIM_QT4_IMMODULE_CANDIDATE_WINDOW_PROXY_H
#define UIM_QT4_IMMODULE_CANDIDATE_WINDOW_PROXY_H

#include <QtCore/QObject>

#include <uim.h>

#include "util.h"

class QPoint;
class QProcess;
class QRect;
class QTimer;

#if QT_VERSION < 0x050000
class QUimInputContext;
#else
class QUimPlatformInputContext;
#endif

class CandidateWindowProxy : public QObject
{
    Q_OBJECT

    public:
        explicit CandidateWindowProxy();
        virtual ~CandidateWindowProxy();

        void deactivateCandwin();
        void clearCandidates();

        void popup();
        void hide();
#ifdef WORKAROUND_BROKEN_RESET_IN_QT4
        bool isVisible();
#endif

        void setAlwaysLeftPosition(bool left) { isAlwaysLeft = left; }
        bool isAlwaysLeftPosition() const { return isAlwaysLeft; }

        void layoutWindow(int x, int y, int height);

#if QT_VERSION < 0x050000
        void setQUimInputContext(QUimInputContext *m_ic) { ic = m_ic; }
#else
        void setQUimPlatformInputContext(QUimPlatformInputContext *m_ic)
        { ic = m_ic; }
#endif

        void candidateActivate(int nr, int displayLimit);
#ifdef UIM_QT_USE_DELAY
        void candidateActivateWithDelay(int delay);
#endif /* !UIM_QT_USE_DELAY */
        void candidateSelect(int index);
        void candidateShiftPage(bool forward);

        QString candidateWindowStyle();

    private slots:
        void slotReadyStandardOutput();
#ifdef UIM_QT_USE_DELAY
        void timerDone();
#endif /* !UIM_QT_USE_DELAY */

    private:
        void initializeProcess();
        void execute(const QString &command);

        void activateCandwin(int dLimit);

        void shiftPage(bool forward);
        void setIndex(int totalindex);
#ifdef UIM_QT_USE_NEW_PAGE_HANDLING
        void setNrCandidates(int nrCands, int dLimit);
#endif
        void updateLabel();
        void setCandidates(int displayLimit,
                const QList<uim_candidate> &candidates);
        void setPage(int page);
#ifdef UIM_QT_USE_NEW_PAGE_HANDLING
        void setPageCandidates(int page,
                const QList<uim_candidate> &candidates);
        void preparePageCandidates(int page);
#endif

        void setFocusWidget();
        bool eventFilter(QObject *obj, QEvent *event);

        QProcess *process;
#if QT_VERSION < 0x050000
        QUimInputContext *ic;
#else
        QUimPlatformInputContext *ic;
#endif

        // candidate data
        QList<uim_candidate> stores;
        int nrCandidates;
        int displayLimit;
        int candidateIndex;
        int pageIndex;
#ifdef UIM_QT_USE_NEW_PAGE_HANDLING
        QList<bool> pageFilled;
#endif

        // widget to follow movement
        QWidget *window;

        // candidate data
#ifdef UIM_QT_USE_NEW_PAGE_HANDLING
        int nrPages;
#endif

        // config
        bool isAlwaysLeft;

#ifdef WORKAROUND_BROKEN_RESET_IN_QT4
        bool m_isVisible;
#endif

#ifdef UIM_QT_USE_DELAY
        // timer for delay API
        QTimer *m_delayTimer;
#endif /* !UIM_QT_USE_DELAY */
};

#endif /* Not def: UIM_QT4_IMMODULE_CANDIDATE_WINDOW_PROXY_H */
