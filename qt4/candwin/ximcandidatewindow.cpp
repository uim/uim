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
#include <config.h>

#include "ximcandidatewindow.h"

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <unistd.h>

#include <QtCore/QSocketNotifier>
#include <QtCore/QStringList>
#include <QtCore/QTextCodec>
#if QT_VERSION < 0x050000
# include <QtGui/QApplication>
# include <QtGui/QDesktopWidget>
# include <QtGui/QHeaderView>
# include <QtGui/QLabel>
# include <QtGui/QTableWidget>
# include <QtGui/QVBoxLayout>
#else
# include <QtWidgets/QApplication>
# include <QtWidgets/QDesktopWidget>
# include <QtWidgets/QHeaderView>
# include <QtWidgets/QLabel>
# include <QtWidgets/QTableWidget>
# include <QtWidgets/QVBoxLayout>
#endif

#include <uim/uim.h>
#include <uim/uim-helper.h>

#include "util.h"

static const int NR_CANDIDATES = 10;
static const int MIN_CAND_WIDTH = 80;

static const int HEADING_COLUMN = 0;
static const int CANDIDATE_COLUMN = 1;

const Qt::WindowFlags candidateFlag = (Qt::Window
                                        | Qt::WindowStaysOnTopHint
                                        | Qt::FramelessWindowHint
                                        | Qt::Tool
#if defined(Q_WS_X11)
                                        | Qt::X11BypassWindowManagerHint
#endif
                                );
static QSocketNotifier *notifier = 0;

XimCandidateWindow::XimCandidateWindow(QWidget *parent)
: QFrame(parent, candidateFlag), nrCandidates(0), candidateIndex(0),
    displayLimit(NR_CANDIDATES), pageIndex(-1), isActive(false)
{
    setFrameStyle(Raised | NoFrame);
    setFocusPolicy(Qt::NoFocus);

    //setup CandidateList
    cList = new QTableWidget;
    cList->setSelectionMode(QAbstractItemView::SingleSelection);
    cList->setSelectionBehavior(QAbstractItemView::SelectRows);
    // the last column is dummy for adjusting size.
    cList->setColumnCount(3);
#if QT_VERSION < 0x050000
    cList->horizontalHeader()->setResizeMode(QHeaderView::ResizeToContents);
#else
    cList->horizontalHeader()->setSectionResizeMode(
        QHeaderView::ResizeToContents);
#endif
    cList->horizontalHeader()->setStretchLastSection(true);
    cList->horizontalHeader()->hide();
    cList->verticalHeader()->hide();
    cList->setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
    cList->setVerticalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
    cList->setAutoScroll(false);
    cList->setShowGrid(false);
    cList->setMinimumWidth(MIN_CAND_WIDTH);
    connect(cList, SIGNAL(cellClicked(int, int)),
          this , SLOT(slotCandidateSelected(int)));

    //setup NumberLabel
    numLabel = new QLabel;
    numLabel->setFocusPolicy(Qt::NoFocus);
    numLabel->adjustSize();

    notifier = new QSocketNotifier(0, QSocketNotifier::Read);
    connect(notifier, SIGNAL(activated(int)),
                      this, SLOT(slotStdinActivated(int)));
    QVBoxLayout *layout = new QVBoxLayout;
    layout->setMargin(0);
    layout->setSpacing(0);
    layout->addWidget(cList);
    layout->addWidget(numLabel);
    setLayout(layout);

    hide();
}

XimCandidateWindow::~XimCandidateWindow()
{
    if (!stores.isEmpty())
        stores.clear();
}

void XimCandidateWindow::activateCand(const QStringList &list)
{
#if defined(ENABLE_DEBUG)
    qDebug("uim-candwin-qt4: activateCand()");
#endif
    /**
     * format: activate\fcharset=$charset\fdisplay_limit=$value\fhead1\acand1\aannot1\fhead2\acand2\aannot2\fhead3\acand3\aannot3\f
     */

    // remove old data
    cList->clearContents();
    cList->setRowCount(0);
    stores.clear();

    // get charset and create codec
    QTextCodec *codec = 0;
    if (!list[1].isEmpty()
        && list[1].startsWith(QLatin1String("charset")))
    {
        const QStringList l = list[1].split('=', QString::SkipEmptyParts);
        codec = QTextCodec::codecForName(l[1].toLatin1());
    }

    // get display_limit
    if (!list[2].isEmpty()
        && list[2].startsWith(QLatin1String("display_limit")))
    {
        const QStringList l = list[2].split('=', QString::SkipEmptyParts);
        displayLimit = l[1].toInt();
    }

    for (int i = 3; i < list.count(); i++)
    {
        // case list[i] = ""
        if (list[i].isEmpty())
            break;

        // split heading_label and cand_str
        QStringList l = list[i].split('\a');
        int count = l.count();
        if (count < 2)
            continue;

        // store data
        CandData d;
        QString headString;
        if (codec)
            headString = codec->toUnicode(l[0].toLatin1());
        else
            headString = l [0];

        d.headingLabel = headString;

        l.pop_front();
        QString candString = l [0];

        if (codec)
            d.str = codec->toUnicode(candString.toLatin1());
        else
            d.str = candString;

        if (count >= 3) {
            l.pop_front();
            QString annotString = l [0];
        }

        stores.append(d);
    }

    // set default value
    candidateIndex = -1;
    nrCandidates = stores.count();

    // shift to default page
    setPage(0);

    adjustCandidateWindowSize();
    show();

    isActive = true;
}
void XimCandidateWindow::selectCand(const QStringList &list)
{
#if defined(ENABLE_DEBUG)
    qDebug("uim-candwin-qt4: selectCand()");
#endif
    const int index = list[1].toInt();
    needHighlight = (list[2].toInt() == 1);
    setIndex(index);

    updateLabel();
}

void XimCandidateWindow::moveCand(const QStringList &list)
{
#if defined(ENABLE_DEBUG)
    qDebug("uim-candwin-qt4: moveCand()");
#endif
    if (list[1].isEmpty() || list[2].isEmpty())
        return ;

    const int topwin_x = list[1].toInt();
    const int topwin_y = list[2].toInt();
    const int cw_wi = width();
    const int cw_he = height();
    const int sc_wi = QApplication::desktop()->screenGeometry().width();
    const int sc_he = QApplication::desktop()->screenGeometry().height();

    int x, y;
    if (sc_wi < topwin_x + cw_wi)
    {
        x = topwin_x - cw_wi;
    }
    else
    {
        x = topwin_x;
    }

    if (sc_he < topwin_y + cw_he)
    {
        /* FIXME : How can I determine the preedit height? */
        y = topwin_y - cw_he - 20;
    }
    else
    {
        y = topwin_y;
    }

    move(x, y);
}

void XimCandidateWindow::showCand()
{
#if defined(ENABLE_DEBUG)
    qDebug("uim-candwin-qt4: showCand()");
#endif
    if (isActive)
        show();
}
void XimCandidateWindow::deactivateCand()
{
#if defined(ENABLE_DEBUG)
    qDebug("uim-candwin-qt4: deactivateCand()");
#endif
    hide();
    isActive = false;
}
void XimCandidateWindow::setNrCandidates(const QStringList &list)
{
#if defined(ENABLE_DEBUG)
    qDebug("uim-candwin-qt: setNrCandidates()");
#endif
    if (list[1].isEmpty() || list[2].isEmpty())
        return ;

    // remove old data
    cList->clearContents();
    cList->setRowCount(0);
    stores.clear();

    // set default value
    candidateIndex = -1;
    nrCandidates = list[1].toInt();
    displayLimit = list[2].toInt();
    needHighlight = false;
    isActive = true;

    // setup dummy stores
    for (int i = 0; i < nrCandidates; i++) {
        CandData d;
        stores.append(d);
    }
}
void XimCandidateWindow::setPageCandidates(const QStringList &list)
{
#if defined(ENABLE_DEBUG)
    qDebug("uim-candwin-qt: setPageCandidates()");
#endif
    /**
     * format: set_page_candidates\fcharset=$charset\fpage=$value\fhead1\acand1\aannot1\fhead2\acand2\aannot2\fhead3\acand3\aannot3\f
     */

    int page = 0;

    // get charset and create codec
    QTextCodec *codec = 0;
    if (!list[1].isEmpty()
        && list[1].startsWith(QLatin1String("charset")))
    {
        const QStringList l = list[1].split('=', QString::SkipEmptyParts);
        codec = QTextCodec::codecForName(l[1].toLatin1());
    }

    // get page
    if (!list[2].isEmpty()
        && list[2].startsWith(QLatin1String("page")))
    {
        const QStringList l = list[2].split('=', QString::SkipEmptyParts);
        page = l[1].toInt();
    }

    int len = list.count();
    for (int i = 3; i < len; i++)
    {
        // case list[i] = ""
        if (list[i].isEmpty())
            break;

        // split heading_label and cand_str
        QStringList l = list [i].split('\a');

        // store data
        CandData &d = stores[page * displayLimit + i - 3];
        QString headString;
        if (codec)
            headString = codec->toUnicode(l [0].toLatin1());
        else
            headString = l [0];

        d.headingLabel = headString;

        l.pop_front();
        QString candString = l [0];

        if (codec)
            d.str = codec->toUnicode(candString.toLatin1());
        else
            d.str = candString;

        l.pop_front();
        QString annotString = l [0];
    }
}
void XimCandidateWindow::showPage(const QStringList &list)
{
#if defined(ENABLE_DEBUG)
    qDebug("uim-candwin-qt: showPage()");
#endif
    const int page = list[1].toInt();

    setPage(page);
    adjustCandidateWindowSize();
    show();
}

void XimCandidateWindow::slotStdinActivated(int fd)
{
    QList<QStringList> messageList = parse_messages(get_messages(fd));
    for (int i = 0, j = messageList.count(); i < j; i++) {
        QStringList message = messageList[i];
        QString command = message[0];
        if (command == "activate")
            activateCand(message);
        else if (command == "select")
            selectCand(message);
        else if (command == "show")
            showCand();
        else if (command == "hide")
            hide();
        else if (command == "move")
            moveCand(message);
        else if (command == "deactivate")
            deactivateCand();
        else if (command == "set_nr_candidates")
            setNrCandidates(message);
        else if (command == "set_page_candidates")
            setPageCandidates(message);
        else if (command == "show_page")
            showPage(message);
    }
}

void XimCandidateWindow::slotCandidateSelected(int row)
{
    candidateIndex = (pageIndex * displayLimit) + row;

    // write message
    fprintf(stdout, "index\n");
    fprintf(stdout, "%d\n\n", candidateIndex);
    fflush(stdout);

    updateLabel();
}

void XimCandidateWindow::adjustCandidateWindowSize()
{
#if defined(ENABLE_DEBUG)
    qDebug("adjustCandidateWindowSize()");
#endif
    // frame width
    // According to the Qt4 documentation on the QFrame class,
    // the frame width is 1 pixel.
    int frame = 1 * 2
        + cList->style()->pixelMetric(QStyle::PM_DefaultFrameWidth) * 2;

    const int rowNum = cList->rowCount();
    if (rowNum == 0) {
        resize(MIN_CAND_WIDTH, numLabel->height() + frame);
        return;
    }
    int width = frame;
    // the size of the dummy column should be 0.
    for (int i = 0; i < cList->columnCount() - 1; i++)
        width += cList->columnWidth(i);

    resize(width, cList->rowHeight(0) * rowNum + numLabel->height()
        + frame);
}

void XimCandidateWindow::setPage(int page)
{
    // clear items
    cList->clearContents();

    // calculate page
    int newpage, lastpage;
    if (displayLimit)
        lastpage = nrCandidates / displayLimit;
    else
        lastpage = 0;
    
    if (page < 0)
    {
        newpage = lastpage;
    }
    else if (page > lastpage)
    {
        newpage = 0;
    }
    else
    {
        newpage = page;
    }

    pageIndex = newpage;

    // calculate index
    int newindex;
    if (displayLimit)
    {
        if (candidateIndex >= 0)
            newindex = (newpage * displayLimit) + (candidateIndex % displayLimit);
        else
            newindex = -1;
    }
    else
    {
        newindex = candidateIndex;
    }

    if (newindex >= nrCandidates)
        newindex = nrCandidates - 1;

    // set cand items
    //
    // If we switch to last page, the number of items to be added
    // is lower than displayLimit.
    //
    // ex. if nrCandidate==14 and displayLimit==10, the number of
    //     last page's item==4
    int ncandidates = displayLimit;
    if (newpage == lastpage)
        ncandidates = nrCandidates - displayLimit * lastpage;
    cList->setRowCount(ncandidates);
    for (int i = 0; i < ncandidates ; i++)
    {
        QString headString = stores[displayLimit * newpage + i].headingLabel;
        QString candString = stores[displayLimit * newpage + i].str;

        // insert new item to the candidate list
        QTableWidgetItem *headItem = new QTableWidgetItem;
        headItem->setText(headString);
        headItem->setFlags(Qt::ItemIsSelectable | Qt::ItemIsEnabled);
        QTableWidgetItem *candItem = new QTableWidgetItem;
        candItem->setText(candString);
        candItem->setFlags(Qt::ItemIsSelectable | Qt::ItemIsEnabled);
        cList->setItem(i, HEADING_COLUMN, headItem);
        cList->setItem(i, CANDIDATE_COLUMN, candItem);
        cList->setRowHeight(i, QFontMetrics(cList->font()).height() + 2);
    }

    // set index
    if (newindex != candidateIndex)
        setIndex(newindex);
    else
        updateLabel();

    // set candwin size
    adjustCandidateWindowSize();
}

void XimCandidateWindow::setIndex(int index)
{
#if defined(ENABLE_DEBUG)
    qDebug("setIndex : index = %d", index);
#endif
    // validity check
    if (index < 0)
        candidateIndex = nrCandidates - 1;
    else if (index >= nrCandidates)
        candidateIndex = 0;
    else
        candidateIndex = index;

    // set page
    int newpage = 0;
    if (displayLimit)
        newpage = candidateIndex / displayLimit;
    if (pageIndex != newpage)
        setPage(newpage);

    // select item
    if (candidateIndex >= 0 && needHighlight)
    {
        int pos = index;
        if (displayLimit)
            pos = candidateIndex % displayLimit;

        if (cList->item(pos, 0) && !cList->item(pos, 0)->isSelected())
        {
            cList->clearSelection();
            cList->selectRow(pos);
        }
    }
    else
    {
        cList->clearSelection();
    }

    updateLabel();
}

void XimCandidateWindow::updateLabel()
{
    QString indexString;
    if (candidateIndex >= 0 && needHighlight)
        indexString = QString::number(candidateIndex + 1) + " / " + QString::number(nrCandidates);
    else
        indexString = "- / " + QString::number(nrCandidates);

    numLabel->setText(indexString);
}
