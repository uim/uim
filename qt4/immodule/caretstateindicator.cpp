/*

  Copyright (c) 2010-2013 uim Project https://github.com/uim/uim

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
#include "caretstateindicator.h"

#include <QtCore/QRect>
#include <QtCore/QStringList>
#include <QtCore/QTimer>
#include <QtCore/QVariant>
#include <QtGui/QMoveEvent>
#if QT_VERSION < 0x050000
# include <QtGui/QApplication>
# include <QtGui/QHBoxLayout>
# include <QtGui/QLabel>
#else
# include <QtWidgets/QApplication>
# include <QtWidgets/QHBoxLayout>
# include <QtWidgets/QLabel>
#endif

#include <uim/uim-scm.h>

const int CaretStateIndicator::SPACING = 3;
static const int DEFAULT_WINDOW_WIDTH = 20;
static const int DEFAULT_WINDOW_HEIGHT = 20;

// caret state indicator is a state indicator nearby the caret.
CaretStateIndicator::CaretStateIndicator(QWidget *parent):
    QWidget(parent, Qt::ToolTip), m_window(0)
{
    QHBoxLayout *layout = new QHBoxLayout;
    layout->setMargin(0);
    layout->setSpacing(0);
    setLayout(layout);

    m_timer = new QTimer(this);
    connect(m_timer, SIGNAL(timeout()), this, SLOT(hide()));
}

CaretStateIndicator::~CaretStateIndicator()
{
    while (!m_labelList.isEmpty())
        delete m_labelList.takeFirst();
}

void CaretStateIndicator::update(const QString &str)
{
    bool isEnabled = uim_scm_symbol_value_bool("bridge-show-input-state?");
    char *type = uim_scm_c_symbol(uim_scm_symbol_value("bridge-show-with?"));
    bool isMode = (qstrcmp(type, "mode") == 0);
    free(type);
    bool isModeOn
        = uim_scm_symbol_value_bool("bridge-show-input-state-mode-on?");
    if (isEnabled && !(isMode && !isModeOn)) {
        updateLabels(str);
        if (!isMode) {
            int time = uim_scm_symbol_value_int(
                "bridge-show-input-state-time-length");
            if (time != 0)
                setTimeout(time);
        }
        setVisible(true);
    } else if (isMode && !isModeOn) {
        setVisible(false);
    }
}

void CaretStateIndicator::updateLabels(const QString &str)
{
    if (!str.isEmpty()) {
        QStringList lines = str.split('\n', QString::SkipEmptyParts);
        QStringList cols;
        for (int i = 0; i < lines.count(); i++) {
            if (lines.at(i).startsWith(QLatin1String("branch\t"))) {
                QStringList branchLines = lines.at(i).split('\t');
                if (branchLines.count() > 2)
                   cols.append(branchLines.at(2));
            }
        }
        int colsCount = cols.count();
        int labelCount = m_labelList.count();
        for (int i = labelCount; i < colsCount; i++) {
            QLabel *label = new QLabel;
            label->setFrameStyle(QFrame::Box | QFrame::Plain);
            label->setMinimumSize(DEFAULT_WINDOW_WIDTH, DEFAULT_WINDOW_HEIGHT);
            label->setAlignment(Qt::AlignCenter);
            m_labelList.append(label);
            layout()->addWidget(label);
        }
        for (int i = colsCount; i < labelCount; i++) {
            QLabel *label = m_labelList.takeAt(colsCount);
            layout()->removeWidget(label);
            delete label;
        }
        for (int i = 0; i < colsCount; i++)
            m_labelList[i]->setText(cols[i]);
    }
    QWidget *widget = QApplication::focusWidget();
    if (widget) {
        QRect rect = widget->inputMethodQuery(Qt::ImMicroFocus).toRect();
        move(widget->mapToGlobal(rect.bottomLeft())
            + QPoint(0, CaretStateIndicator::SPACING));
        m_window = widget->window();
        m_window->installEventFilter(this);
    }
}

void CaretStateIndicator::setTimeout(int second)
{
    if (m_timer->isActive())
        m_timer->stop();
    m_timer->start(1000 * second);
}

bool CaretStateIndicator::eventFilter(QObject *obj, QEvent *event)
{
    if (obj == m_window) {
        if (event->type() == QEvent::Move) {
            QMoveEvent *moveEvent = static_cast<QMoveEvent *>(event);
            move(pos() + moveEvent->pos() - moveEvent->oldPos());
        }
        return false;
    }
    return QWidget::eventFilter(obj, event);
}
