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
#include "abstractcandidatewindow.h"

#include <QtCore/QSocketNotifier>
#include <QtGui/QApplication>
#include <QtGui/QDesktopWidget>
#include <QtGui/QLabel>
#include <QtGui/QMoveEvent>

#include "util.h"

const Qt::WindowFlags candidateFlag = (Qt::Window
                                        | Qt::WindowStaysOnTopHint
                                        | Qt::FramelessWindowHint
                                        | Qt::Tool
#if defined(Q_WS_X11)
                                        | Qt::X11BypassWindowManagerHint
#endif
                                 );

AbstractCandidateWindow::AbstractCandidateWindow(QWidget *parent)
: QFrame(parent, candidateFlag)
{
    setFrameStyle(Raised|NoFrame);

    // setup NumberLabel
    numLabel = new QLabel;
    numLabel->adjustSize();

    notifier = new QSocketNotifier(0, QSocketNotifier::Read);
    connect(notifier, SIGNAL(activated(int)),
        this, SLOT(slotStdinActivated(int)));
}

AbstractCandidateWindow::~AbstractCandidateWindow()
{
}

void AbstractCandidateWindow::activateCandwin()
{
}

void AbstractCandidateWindow::shiftPage(int idx)
{
    Q_UNUSED(idx)
}

#ifdef UIM_QT_USE_NEW_PAGE_HANDLING
void AbstractCandidateWindow::setNrCandidates()
{
}
#endif

#ifdef WORKAROUND_BROKEN_RESET_IN_QT4
void AbstractCandidateWindow::showEvent(QShowEvent *event)
{
    QFrame::showEvent(event);

    fprintf(stdout, "shown\f\f");
    fflush(stdout);
}

void AbstractCandidateWindow::hideEvent(QHideEvent *event)
{
    QFrame::hideEvent(event);
    fprintf(stdout, "hidden\f\f");
    fflush(stdout);
}
#endif

void AbstractCandidateWindow::slotStdinActivated(int fd)
{
    QList<QStringList> messageList = parse_messages(get_messages(fd));
    for (int i = 0, j = messageList.count(); i < j; i++) {
        QStringList message = messageList[i];
        QString command = message[0];
        if (command == "activate_candwin")
            activateCandwin();
        else if (command == "candidate_activate")
            candidateActivate();
        else if (command == "hide")
            hide();
        else if (command == "layout_window")
            layoutWindow(message[1].toInt(), message[2].toInt(),
                message[3].toInt());
        else if (command == "move_candwin")
            moveCandwin(message[1].toInt(), message[2].toInt());
        else if (command == "popup")
            popup();
        else if (command == "set_nr_candidates")
            setNrCandidates();
        else if (command == "set_index")
            setIndex(message[1].toInt(), message[2].toInt(),
                message[3].toInt());
        else if (command == "shift_page")
            shiftPage(message[1].toInt());
        else if (command == "update_label")
            updateLabel(message[1]);
        else if (command == "update_size")
            updateSize();
        else if (command == "update_view")
            updateView(message[1].toInt(), candidateData(message));
    }
}

QList<CandData> AbstractCandidateWindow::candidateData(
    const QStringList &message)
{
    QList<CandData> stores;
    for (int i = 2, j = message.count(); i < j; i++) {
        QStringList candidate = message[i].split('\a');

        int count = candidate.count();
        if (count < 2)
            continue;

        CandData cand;
        cand.headingLabel = candidate[0];
        cand.str = candidate[1];

        if (count >= 3)
            cand.annotation = candidate[2];

        stores.append(cand);
    }
    return stores;
}

void AbstractCandidateWindow::popup()
{
    fprintf(stdout, "set_focus_widget\f\f");
    fflush(stdout);
    raise();
    show();
}

void AbstractCandidateWindow::layoutWindow(int x, int y, int h)
{
    int destX = x;
    int destY = y + h;

    int screenW = QApplication::desktop()->screenGeometry().width();
    int screenH = QApplication::desktop()->screenGeometry().height();

    if (destX + width() > screenW)
        destX = screenW - width();

    if (destY + height() > screenH)
        destY = y - height();

    move(destX, destY);
}

void AbstractCandidateWindow::moveCandwin(int x, int y)
{
    QPoint p = pos();
    move(p.x() + x, p.y() + y);
}

void AbstractCandidateWindow::candidateActivate()
{
    popup();
    fprintf(stdout, "set_candwin_active\f\f");
    fflush(stdout);
}

void AbstractCandidateWindow::updateLabel(const QString &indexString)
{
    numLabel->setText(indexString);
}
