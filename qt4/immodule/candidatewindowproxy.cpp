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
#include <config.h>

#include "candidatewindowproxy.h"

#include <QtCore/QPoint>
#include <QtCore/QProcess>
#include <QtCore/QTimer>
#include <QtGui/QMoveEvent>
#if QT_VERSION < 0x050000
# include <QtGui/QApplication>
# include <QtGui/QDesktopWidget>
# include <QtGui/QLabel>
#else
# include <QtWidgets/QApplication>
# include <QtWidgets/QDesktopWidget>
# include <QtWidgets/QLabel>
#endif

#include <uim-scm.h>

#if QT_VERSION < 0x050000
# include "quiminputcontext.h"
#else
# include "quimplatforminputcontext.h"
#endif

CandidateWindowProxy::CandidateWindowProxy()
: ic(0), nrCandidates(0), displayLimit(0), candidateIndex(-1), pageIndex(-1),
  window(0), isAlwaysLeft(false)
#ifdef WORKAROUND_BROKEN_RESET_IN_QT4
, m_isVisible(false)
#endif
{
#ifdef UIM_QT_USE_DELAY
    m_delayTimer = new QTimer(this);
    m_delayTimer->setSingleShot(true);
    connect(m_delayTimer, SIGNAL(timeout()), this, SLOT(timerDone()));
#endif /* !UIM_QT_USE_DELAY */

    process = new QProcess;
    initializeProcess();
    connect(process, SIGNAL(readyReadStandardOutput()),
        this, SLOT(slotReadyStandardOutput()));
}

CandidateWindowProxy::~CandidateWindowProxy()
{
    // clear stored candidate data
    while (!stores.isEmpty()) {
        uim_candidate cand = stores.takeFirst();
        if (cand)
            uim_candidate_free(cand);
    }
    process->close();
}

void CandidateWindowProxy::deactivateCandwin()
{
#ifdef UIM_QT_USE_DELAY
    m_delayTimer->stop();
#endif /* !UIM_QT_USE_DELAY */

    execute("hide");
    clearCandidates();
}

void CandidateWindowProxy::clearCandidates()
{
#ifdef ENABLE_DEBUG
    qDebug("clear Candidates");
#endif

    candidateIndex = -1;
    displayLimit = 0;
    nrCandidates = 0;

    // clear stored candidate data
    while (!stores.isEmpty()) {
        uim_candidate cand = stores.takeFirst();
        if (cand)
            uim_candidate_free(cand);
    }
}

void CandidateWindowProxy::popup()
{
    execute("popup");
}

void CandidateWindowProxy::hide()
{
    execute("hide");
}

#ifdef WORKAROUND_BROKEN_RESET_IN_QT4
bool CandidateWindowProxy::isVisible()
{
    return m_isVisible;
}
#endif

void CandidateWindowProxy::layoutWindow(int x, int y, int height)
{
    execute("layout_window\f" + QString::number(x) + '\f'
        + QString::number(y) + '\f' + QString::number(height));
}

void CandidateWindowProxy::candidateActivate(int nr, int displayLimit)
{
#ifdef UIM_QT_USE_DELAY
    m_delayTimer->stop();
#endif /* !UIM_QT_USE_DELAY */

   QList<uim_candidate> list;

#if !UIM_QT_USE_NEW_PAGE_HANDLING
    activateCandwin(displayLimit);

    // set candidates
    for (int i = 0; i < nr; i++) {
        cand = uim_get_candidate(ic->uimContext(), i,
                displayLimit ? i % displayLimit : i);
        list.append(cand);
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

    execute("candidate_activate");
}

#ifdef UIM_QT_USE_DELAY
void CandidateWindowProxy::candidateActivateWithDelay(int delay)
{
    m_delayTimer->stop();
    (delay > 0) ?  m_delayTimer->start(delay * 1000) : timerDone();
}
#endif /* !UIM_QT_USE_DELAY */

void CandidateWindowProxy::candidateSelect(int index)
{
#if UIM_QT_USE_NEW_PAGE_HANDLING
    if (index >= nrCandidates)
        index = 0;

    int new_page;
    if (index >= 0 && displayLimit)
        new_page = index / displayLimit;
    else
        new_page = pageIndex;

    preparePageCandidates(new_page);
#endif /* UIM_QT_USE_NEW_PAGE_HANDLING */
    setIndex(index);
}

void CandidateWindowProxy::candidateShiftPage(bool forward)
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

// -v -> vertical
// -h -> horizontal
// -t -> table
QString CandidateWindowProxy::candidateWindowStyle()
{
    QString windowStyle;
    // uim-candwin-prog is deprecated
    char *candwinprog = uim_scm_symbol_value_str("uim-candwin-prog");
    if (candwinprog) {
        if (!strncmp(candwinprog, "uim-candwin-tbl", 15))
            windowStyle = "-t";
        else if (!strncmp(candwinprog, "uim-candwin-horizontal", 22))
            windowStyle = "-h";
    } else {
        char *style = uim_scm_symbol_value_str("candidate-window-style");
        if (style) {
            if (!strcmp(style, "table"))
                windowStyle = "-t";
            else if (!strcmp(style, "horizontal"))
                windowStyle = "-h";
        }
        free(style);
    }
    free(candwinprog);
    
    if (windowStyle.isEmpty())
        return "-v";
    return windowStyle;
}

void CandidateWindowProxy::slotReadyStandardOutput()
{
    QByteArray output = process->readAllStandardOutput();
    QList<QStringList> messageList = parse_messages(QString(output));
    for (int i = 0, j = messageList.count(); i < j; i++) {
        QStringList message = messageList[i];
        QString command = message[0];
        if (command == "set_candidate_index") {
            uim_set_candidate_index(ic->uimContext(), message[1].toInt());
        } else if (command == "set_candidate_index_2") {
            candidateIndex = pageIndex * displayLimit + message[1].toInt();
            uim_set_candidate_index(ic->uimContext(), candidateIndex);
        } else if (command == "set_candwin_active") {
            ic->setCandwinActive();
        } else if (command == "set_focus_widget") {
            setFocusWidget();
        } else if (command == "update_label") {
            updateLabel();
        }
#ifdef WORKAROUND_BROKEN_RESET_IN_QT4
        else if (command == "shown") {
            m_isVisible = true;
        } else if (command == "hidden") {
            m_isVisible = false;
        }
#endif
    }
}

#ifdef UIM_QT_USE_DELAY 
void CandidateWindowProxy::timerDone()
{
    int nr = -1;
    int display_limit = -1;
    int selected_index = -1;
    uim_delay_activating(ic->uimContext(), &nr, &display_limit,
        &selected_index);
    if (nr <= 0) {
        return;
    }
    candidateActivate(nr, display_limit);
    if (selected_index >= 0) {
        candidateSelect(selected_index);
    }
}
#endif /* !UIM_QT_USE_DELAY */

void CandidateWindowProxy::initializeProcess()
{
    if (process->state() != QProcess::NotRunning) {
        return;
    }
    process->close();
    QString style = candidateWindowStyle();
#if QT_VERSION < 0x050000
    process->start(UIM_LIBEXECDIR "/uim-candwin-qt4", QStringList() << style);
#else
    process->start(UIM_LIBEXECDIR "/uim-candwin-qt5", QStringList() << style);
#endif
    process->waitForStarted();
}

void CandidateWindowProxy::execute(const QString &command)
{
    initializeProcess();
    process->write((command + "\f\f").toUtf8());
}

void CandidateWindowProxy::activateCandwin(int dLimit)
{
    candidateIndex = -1;
    displayLimit = dLimit;
    pageIndex = 0;
    execute("setup_sub_window");
}

void CandidateWindowProxy::shiftPage(bool forward)
{
#ifdef ENABLE_DEBUG
    qDebug("candidateIndex = %d", candidateIndex);
#endif
    
    if (forward) {
        if (candidateIndex != -1)
            candidateIndex += displayLimit;
        setPage(pageIndex + 1);
    } else {
        if (candidateIndex != -1) {
            if (candidateIndex < displayLimit)
                candidateIndex = displayLimit * (nrCandidates / displayLimit)
                    + candidateIndex;
            else
                candidateIndex -= displayLimit;
        }
        setPage(pageIndex - 1);
    }
    if (ic && ic->uimContext() && candidateIndex != -1)
        uim_set_candidate_index(ic->uimContext(), candidateIndex);
    // for CandidateWindow
    if (candidateIndex != -1) {
        int idx = displayLimit ? candidateIndex % displayLimit : candidateIndex;
        execute("shift_page\f" + QString::number(idx));
    }
}

void CandidateWindowProxy::setIndex(int totalindex)
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
    execute("set_index\f" + QString::number(totalindex)
        + '\f' + QString::number(displayLimit)
        + '\f' + QString::number(candidateIndex));
}

#if UIM_QT_USE_NEW_PAGE_HANDLING
void CandidateWindowProxy::setNrCandidates(int nrCands, int dLimit)
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
        stores.append(0);

    execute("setup_sub_window");
}
#endif /* UIM_QT_USE_NEW_PAGE_HANDLING */

void CandidateWindowProxy::updateLabel()
{
    QString indexString;
    if (candidateIndex >= 0)
        indexString = QString::number(candidateIndex + 1) + " / "
            + QString::number(nrCandidates);
    else
        indexString = "- / " + QString::number(nrCandidates);

    execute("update_label\f" + indexString);
}

void CandidateWindowProxy::setCandidates(int dl,
        const QList<uim_candidate> &candidates)
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

void CandidateWindowProxy::setPage(int page)
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

    QString candidateMessage;
    for (int i = 0; i < ncandidates; i++) {
        uim_candidate cand = stores.at(displayLimit * newpage + i);
        candidateMessage +=
            QString::fromUtf8(uim_candidate_get_heading_label(cand)) + '\a'
            + QString::fromUtf8(uim_candidate_get_cand_str(cand)) + '\a'
            + QString::fromUtf8(uim_candidate_get_annotation_str(cand)) + '\f';
    }

    execute("update_view\f" + QString::number(ncandidates) + "\f"
        + candidateMessage);

    // set index
    if (newindex != candidateIndex)
        setIndex(newindex);
    else
        updateLabel();

    execute("update_size");
}

#if UIM_QT_USE_NEW_PAGE_HANDLING
void CandidateWindowProxy::setPageCandidates(int page,
        const QList<uim_candidate> &candidates)
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

void CandidateWindowProxy::preparePageCandidates(int page)
{
    QList<uim_candidate> list;

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
        // set page candidates
        uim_candidate cand = uim_get_candidate(ic->uimContext(), i,
                displayLimit ? i % displayLimit : i);
        list.append(cand);
    }
    pageFilled[page] = true;
    setPageCandidates(page, list);
}
#endif /* UIM_QT_USE_NEW_PAGE_HANDLING */

void CandidateWindowProxy::setFocusWidget()
{
    window = QApplication::focusWidget()->window();
    window->installEventFilter(this);
}

bool CandidateWindowProxy::eventFilter(QObject *obj, QEvent *event)
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
                QPoint p = moveEvent->pos() - moveEvent->oldPos();
                execute("move_candwin\f" + QString::number(p.x()) + '\f'
                    + QString::number(p.y()));
            }
        }
        return false;
    }
    return QObject::eventFilter(obj, event);
}
