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
#include <QtCore/QTimer>
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
: QFrame(parent, candidateFlag), ic(0), nrCandidates(0), displayLimit(0),
    candidateIndex(-1), pageIndex(-1), window(0), isAlwaysLeft(false)
{
    setFrameStyle(Raised|NoFrame);

    // setup NumberLabel
    numLabel = new QLabel;
    numLabel->adjustSize();

#ifdef UIM_QT_USE_DELAY
    m_delayTimer = new QTimer(this);
    m_delayTimer->setSingleShot(true);
    connect(m_delayTimer, SIGNAL(timeout()), this, SLOT(timerDone()));
#endif /* !UIM_QT_USE_DELAY */

    notifier = new QSocketNotifier(0, QSocketNotifier::Read);
    connect(notifier, SIGNAL(activated(int)),
        this, SLOT(slotStdinActivated(int)));
}

AbstractCandidateWindow::~AbstractCandidateWindow()
{
    stores.clear();
}

void AbstractCandidateWindow::deactivateCandwin()
{
#ifdef UIM_QT_USE_DELAY
    m_delayTimer->stop();
#endif /* !UIM_QT_USE_DELAY */

    hide();
    clearCandidates();
}

void AbstractCandidateWindow::clearCandidates()
{
#ifdef ENABLE_DEBUG
    qDebug("clear Candidates");
#endif

    candidateIndex = -1;
    displayLimit = 0;
    nrCandidates = 0;

    stores.clear();
}

void AbstractCandidateWindow::popup()
{
    window = QApplication::focusWidget()->window();
    window->installEventFilter(this);
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

void AbstractCandidateWindow::candidateActivate(int nr, int displayLimit)
{
#ifdef UIM_QT_USE_DELAY
    m_delayTimer->stop();
#endif /* !UIM_QT_USE_DELAY */

    QList<CandData> list;

#if !UIM_QT_USE_NEW_PAGE_HANDLING
    activateCandwin(displayLimit);

    // set candidates
    for (int i = 0; i < nr; i++)
    {
        //cand = uim_get_candidate(ic->uimContext(), i,
        //        displayLimit ? i % displayLimit : i);
        //list.append(cand);
    }
    setCandidates(displayLimit, list);

#else /* !UIM_QT_USE_NEW_PAGE_HANDLING */
    nrPages = displayLimit ? (nr - 1) / displayLimit + 1 : 1;
    pageFilled.clear();
    for (int i = 0; i < nrPages; i++)
        pageFilled.append(false);
    
    setNrCandidates(nr, displayLimit);

    // set page candidates
    preparePageCandidates(0);
    setPage(0);
#endif /* !UIM_QT_USE_NEW_PAGE_HANDLING */
    popup();
    fprintf(stdout, "set_candwin_active");
    fflush(stdout);
}

#ifdef UIM_QT_USE_DELAY
void AbstractCandidateWindow::candidateActivateWithDelay(int delay)
{
    m_delayTimer->stop();
    (delay > 0) ?  m_delayTimer->start(delay * 1000) : timerDone();
}
#endif /* !UIM_QT_USE_DELAY */

void AbstractCandidateWindow::candidateSelect(int index)
{
#if UIM_QT_USE_NEW_PAGE_HANDLING
    int new_page;
    
    if (index >= nrCandidates)
        index = 0;

    if (index >= 0 && displayLimit)
        new_page = index / displayLimit;
    else
        new_page = pageIndex;

    preparePageCandidates(new_page);
#endif /* UIM_QT_USE_NEW_PAGE_HANDLING */
    setIndex(index);
}

void AbstractCandidateWindow::candidateShiftPage(bool forward)
{
#if UIM_QT_USE_NEW_PAGE_HANDLING
    int new_page, index;

    index = forward ? pageIndex + 1 : pageIndex - 1;
    if (index < 0)
        new_page = nrPages - 1;
    else if (index >= nrPages)
        new_page = 0;
    else
        new_page = index;

    preparePageCandidates(new_page);
#endif /* UIM_QT_USE_NEW_PAGE_HANDLING */
    shiftPage(forward);
}

void AbstractCandidateWindow::activateCandwin(int dLimit)
{
    candidateIndex = -1;
    displayLimit = dLimit;
    pageIndex = 0;
}

void AbstractCandidateWindow::shiftPage(bool forward)
{
#ifdef ENABLE_DEBUG
    qDebug("candidateIndex = %d", candidateIndex);
#endif
    
    if (forward)
    {
        if (candidateIndex != -1)
            candidateIndex += displayLimit;
        setPage(pageIndex + 1);
    }
    else
    {
        if (candidateIndex != -1) {
            if (candidateIndex < displayLimit)
                candidateIndex = displayLimit * (nrCandidates / displayLimit) + candidateIndex;
            else
                candidateIndex -= displayLimit;
        }

        setPage(pageIndex - 1);
    }
    fprintf(stdout, "set_candidate_index\f%d", candidateIndex);
    fflush(stdout);
}

void AbstractCandidateWindow::setIndex(int totalindex)
{
#ifdef ENABLE_DEBUG
    qDebug("setIndex : totalindex = %d", totalindex);
#endif

    // validity check
    if (totalindex < 0)
        candidateIndex = nrCandidates - 1;
    else if (totalindex >= nrCandidates)
        candidateIndex = 0;
    else
        candidateIndex = totalindex;

    // set page
    int newpage = 0;
    if (displayLimit)
        newpage = candidateIndex / displayLimit;
    if (pageIndex != newpage)
        setPage(newpage);
}

#if UIM_QT_USE_NEW_PAGE_HANDLING
void AbstractCandidateWindow::setNrCandidates(int nrCands, int dLimit)
{
#ifdef ENABLE_DEBUG
    qDebug("setNrCandidates");
#endif

    // remove old data
    if (!stores.isEmpty())
        clearCandidates();

    candidateIndex = -1;
    displayLimit = dLimit;
    nrCandidates = nrCands;
    pageIndex = 0;

    // setup dummy candidate
    for (int i = 0; i < nrCandidates; i++)
    {
        stores.append(CandData());
    }
}
#endif /* UIM_QT_USE_NEW_PAGE_HANDLING */

#ifdef UIM_QT_USE_DELAY 
void AbstractCandidateWindow::timerDone()
{
    int nr = -1;
    int display_limit = -1;
    int selected_index = -1;
    fprintf(stdout, "uim_delay_activating\f%d\f%d\f%d\f\f", nr, display_limit,
        selected_index);
    fflush(stdout);
    if (nr <= 0) {
        return;
    }
    candidateActivate(nr, display_limit);
    if (selected_index >= 0) {
        candidateSelect(selected_index);
    }
}
#endif /* !UIM_QT_USE_DELAY */

void AbstractCandidateWindow::setCandidates(int dl,
        const QList<CandData> &candidates)
{
#ifdef ENABLE_DEBUG
    qDebug("setCandidates");
#endif

    // remove old data
    if (!stores.isEmpty())
        clearCandidates();

    // set defalt value
    candidateIndex = -1;
    nrCandidates = candidates.count();
    displayLimit = dl;

    if (candidates.isEmpty())
        return;

    // set candidates
    stores = candidates;

    // shift to default page
    setPage(0);
}

void AbstractCandidateWindow::setPage(int page)
{
#ifdef ENABLE_DEBUG
    qDebug("setPage : page = %d", page);
#endif

    // calculate page
    int lastpage = displayLimit ? nrCandidates / displayLimit : 0;

    int newpage;
    if (page < 0)
        newpage = lastpage;
    else if (page > lastpage)
        newpage = 0;
    else
        newpage = page;

    pageIndex = newpage;

    // calculate index
    int newindex;
    if (displayLimit) {
        newindex = (candidateIndex >= 0)
            ? (newpage * displayLimit) + (candidateIndex % displayLimit) : -1;
    } else {
        newindex = candidateIndex;
    }

    if (newindex >= nrCandidates)
        newindex = nrCandidates - 1;

    // set cand items
    //
    // If we switch to last page, the number of items to be added
    // is lower than displayLimit.
    //
    // ex. if nrCandidate == 14 and displayLimit == 10, the number of
    //     last page's item == 4
    int ncandidates = displayLimit;
    if (newpage == lastpage)
        ncandidates = nrCandidates - displayLimit * lastpage;

    updateView(newpage, ncandidates);

    // set index
    if (newindex != candidateIndex)
        setIndex(newindex);
    else
        updateLabel();

    updateSize();
}

#if UIM_QT_USE_NEW_PAGE_HANDLING
void AbstractCandidateWindow::setPageCandidates(int page,
        const QList<CandData> &candidates)
{
#ifdef ENABLE_DEBUG
    qDebug("setPageCandidates");
#endif

    if (candidates.isEmpty())
        return;

    // set candidates
    int start, pageNr;
    start = page * displayLimit;

    if (displayLimit && (nrCandidates - start) > displayLimit)
        pageNr = displayLimit;
    else
        pageNr = nrCandidates - start;

    for (int i = 0; i < pageNr; i++)
        stores[start + i] = candidates[i];
}

void AbstractCandidateWindow::preparePageCandidates(int page)
{
    QList<CandData> list;

    if (page < 0)
        return;

    if (pageFilled[page])
        return;

    // set page candidates
    int start = page * displayLimit;

    int pageNr;
    if (displayLimit && (nrCandidates - start) > displayLimit)
        pageNr = displayLimit;
    else
        pageNr = nrCandidates - start;

    for (int i = start; i < pageNr + start; i++) {
        //cand = uim_get_candidate(ic->uimContext(), i,
        //        displayLimit ? i % displayLimit : i);
        //list.append(cand);
    }
    pageFilled[page] = true;
    setPageCandidates(page, list);
}
#endif /* UIM_QT_USE_NEW_PAGE_HANDLING */

void AbstractCandidateWindow::updateLabel()
{
    QString indexString;
    if (candidateIndex >= 0)
        indexString = QString::number(candidateIndex + 1) + " / "
            + QString::number(nrCandidates);
    else
        indexString = "- / " + QString::number(nrCandidates);

    numLabel->setText(indexString);
}

bool AbstractCandidateWindow::eventFilter(QObject *obj, QEvent *event)
{
    if (obj == window) {
        if (event->type() == QEvent::Move) {
            QWidget *widget = QApplication::focusWidget();
            if (widget) {
                QRect rect
                    = widget->inputMethodQuery(Qt::ImMicroFocus).toRect();
                QPoint p = widget->mapToGlobal(rect.topLeft());
                layoutWindow(p.x(), p.y(), rect.height());
            } else {
                QMoveEvent *moveEvent = static_cast<QMoveEvent *>(event);
                move(pos() + moveEvent->pos() - moveEvent->oldPos());
            }
        }
        return false;
    }
    return QFrame::eventFilter(obj, event);
}

void AbstractCandidateWindow::setCandidateData(const QStringList &message)
{
    for (int i = 3, j = message.count(); i < j; i++) {
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
}

void AbstractCandidateWindow::slotStdinActivated(int fd)
{
    QList<QStringList> messageList = parse_messages(get_messages(fd));
    for (int i = 0, j = messageList.count(); i < j; i++) {
        QStringList message = messageList[i];
        QString command = message[0];
        if (command == "deactivate_candwin")
            deactivateCandwin();
        else if (command == "clear_candidates")
            clearCandidates();
        else if (command == "popup")
            popup();
        else if (command == "hide")
            hide();
        else if (command == "layout_window")
            layoutWindow(message[1].toInt(), message[2].toInt(),
                message[3].toInt());
        else if (command == "candidate_activate") {
            setCandidateData(message);
            candidateActivate(message[1].toInt(), message[2].toInt());
        }
#ifdef UIM_QT_USE_DELAY
        else if (command == "candidate_activate_with_delay")
            candidateActivateWithDelay(message[1].toInt());
#endif /* !UIM_QT_USE_DELAY */
        else if (command == "candidate_select")
            candidateSelect(message[1].toInt());
        else if (command == "candidate_shift_page")
            candidateShiftPage(message[1].toInt());
    }
}
