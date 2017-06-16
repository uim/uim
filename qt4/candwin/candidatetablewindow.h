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
#ifndef UIM_QT4_IMMODULE_CANDIDATE_TABLE_WINDOW_H
#define UIM_QT4_IMMODULE_CANDIDATE_TABLE_WINDOW_H

#include <QtCore/QtGlobal>
#if QT_VERSION < 0x050000
# include <QtGui/QPushButton>
#else
# include <QtWidgets/QPushButton>
#endif

#include "abstractcandidatewindow.h"

class QGridLayout;

class KeyButton;

const int TABLE_NR_ROWS = 8;
const int TABLE_NR_COLUMNS = 13;

class CandidateTableWindow : public AbstractCandidateWindow
{
    Q_OBJECT

    public:
        explicit CandidateTableWindow(QWidget *parent);
        ~CandidateTableWindow();

        QSize sizeHint() const;

    private slots:
        void slotCandidateClicked(int index);

    private:
        void initTable();
        QGridLayout *createLayout(int row, int column,
            int rowOffset, int columnOffset);
        void setBlockVisible(QLayout *layout, bool visible);
        void updateView(int ncandidates, const QList<CandData> &stores);
        void updateSize();
        void setIndex(int totalIndex, int displayLimit, int candidateIndex);
        void getButtonPosition(int &row, int &column,
            const QString &headString);

        QGridLayout *lLayout;
        QGridLayout *rLayout;
        QGridLayout *lsLayout;
        QGridLayout *rsLayout;
        QGridLayout *aLayout;
        QGridLayout *asLayout;
        KeyButton *buttonArray[TABLE_NR_ROWS][TABLE_NR_COLUMNS];
        char *table;
};

class KeyButton : public QPushButton
{
    Q_OBJECT

    public:
        KeyButton();

        QSize sizeHint() const;
        void setIndex(int index);
        int index() const;

    signals:
        void candidateClicked(int index);

    private slots:
        void slotClicked();

    private:
        int m_index;
};

#endif /* Not def: UIM_QT4_IMMODULE_CANDIDATE_TABLE_WINDOW_H */
