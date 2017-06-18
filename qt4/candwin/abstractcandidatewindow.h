/*

  copyright (c) 2010-2013 uim Project https://github.com/uim/uim

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
#if QT_VERSION < 0x050000
# include <QtGui/QFrame>
#else
# include <QtWidgets/QFrame>
#endif

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

    protected:
        virtual void setupSubWindow();

        virtual void shiftPage(int idx);
        virtual void setIndex(int totalindex, int displayLimit,
            int candidateIndex) = 0;
        virtual void updateView(int ncandidates,
            const QList<CandData> &stores) = 0;
        virtual void updateSize() = 0;

#ifdef WORKAROUND_BROKEN_RESET_IN_QT4
        virtual void showEvent(QShowEvent *event);
        virtual void hideEvent(QHideEvent *event);
#endif

        // widget
        QLabel *numLabel;

    private slots:
        void slotStdinActivated(int fd);

    private:
        QList<CandData> candidateData(const QStringList &message);
        void popup();
        void layoutWindow(int x, int y, int height);
        void moveCandwin(int x, int y);
        void candidateActivate();
        void updateLabel(const QString &indexString);

        QSocketNotifier *notifier;
};

#endif /* Not def: UIM_QT4_IMMODULE_ABSTRACT_CANDIDATE_WINDOW_H */
