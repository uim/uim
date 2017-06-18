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
#include "candidatewindow.h"

#include <cstdio>

#include <QtGui/QFontMetrics>
#include <QtGui/QMoveEvent>
#if QT_VERSION < 0x050000
# include <QtGui/QHeaderView>
# include <QtGui/QLabel>
# include <QtGui/QVBoxLayout>
#else
# include <QtWidgets/QHeaderView>
# include <QtWidgets/QLabel>
# include <QtWidgets/QVBoxLayout>
#endif

#include <uim/uim-scm.h>

#include "subwindow.h"

static const int MIN_CAND_WIDTH = 80;

static const int HEADING_COLUMN = 0;
static const int CANDIDATE_COLUMN = 1;
static const int ANNOTATION_COLUMN = 2;

CandidateWindow::CandidateWindow(QWidget *parent, bool vertical)
: AbstractCandidateWindow(parent), subWin(0),
    hasAnnotation(uim_scm_symbol_value_bool("enable-annotation?")),
    isVertical(vertical)
{
    //setup CandidateList
    cList = new CandidateListView(0, isVertical);
    cList->setSelectionMode(QAbstractItemView::SingleSelection);
    cList->setSelectionBehavior(isVertical
        ? QAbstractItemView::SelectRows : QAbstractItemView::SelectColumns);
    cList->setMinimumWidth(MIN_CAND_WIDTH);
    if (isVertical)
        // the last column is dummy for adjusting size.
        cList->setColumnCount(hasAnnotation ? 4 : 3);
    else
        cList->setRowCount(2);
#if QT_VERSION < 0x050000
    cList->horizontalHeader()->setResizeMode(QHeaderView::ResizeToContents);
#else
    cList->horizontalHeader()->setSectionResizeMode(
        QHeaderView::ResizeToContents);
#endif
    cList->horizontalHeader()->setStretchLastSection(true);
    if (!isVertical) {
#if QT_VERSION < 0x050000
        cList->verticalHeader()
            ->setResizeMode(QHeaderView::ResizeToContents);
#else
        cList->verticalHeader()
            ->setSectionResizeMode(QHeaderView::ResizeToContents);
#endif
        cList->verticalHeader()->setStretchLastSection(true);
    }
    cList->horizontalHeader()->hide();
    cList->verticalHeader()->hide();
    cList->setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
    cList->setVerticalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
    cList->setAutoScroll(false);
    cList->setShowGrid(false);
    connect(cList, SIGNAL(cellClicked(int, int)),
          this , SLOT(slotCandidateSelected(int, int)));
    connect(cList, SIGNAL(itemSelectionChanged()),
          this , SLOT(slotHookSubwindow()));

    QVBoxLayout *layout = new QVBoxLayout;
    layout->setMargin(0);
    layout->setSpacing(0);
    layout->addWidget(cList);
    layout->addWidget(numLabel);
    setLayout(layout);
}

void CandidateWindow::setupSubWindow()
{
    if (!subWin)
        subWin = new SubWindow(this);
}

void CandidateWindow::updateView(int ncandidates,
    const QList<CandData> &stores)
{
    cList->clearContents();
    annotations.clear();

    if (isVertical)
        cList->setRowCount(ncandidates);
    else
        // the last column is dummy for adjusting size.
        cList->setColumnCount(ncandidates + 1);
    for (int i = 0; i < ncandidates ; i++) {
        CandData cand = stores[i];
        QString headString = cand.headingLabel;
        QString candString = cand.str;
        QString annotationString;
        if (hasAnnotation) {
            annotationString = cand.annotation;
            annotations.append(annotationString);
        }

        // insert new item to the candidate list
        if (isVertical) {
            QTableWidgetItem *headItem = new QTableWidgetItem;
            headItem->setText(headString);
            headItem->setFlags(Qt::ItemIsSelectable | Qt::ItemIsEnabled);

            QTableWidgetItem *candItem = new QTableWidgetItem;
            candItem->setText(candString);
            candItem->setFlags(Qt::ItemIsSelectable | Qt::ItemIsEnabled);

            cList->setItem(i, HEADING_COLUMN, headItem);
            cList->setItem(i, CANDIDATE_COLUMN, candItem);

            if (hasAnnotation) {
                QTableWidgetItem *annotationItem = new QTableWidgetItem;
                annotationItem->setFlags(
                    Qt::ItemIsSelectable | Qt::ItemIsEnabled);
                if (!annotationString.isEmpty())
                    annotationItem->setText("...");

                cList->setItem(i, ANNOTATION_COLUMN, annotationItem);
            }
            cList->setRowHeight(i,
                QFontMetrics(cList->font()).height() + 2);
        } else {
            QTableWidgetItem *candItem = new QTableWidgetItem;
            candItem->setFlags(Qt::ItemIsSelectable | Qt::ItemIsEnabled);

            QString candText = headString + ": " + candString;
            if (hasAnnotation && !annotationString.isEmpty())
                candText += "...";
            candItem->setText(candText);

            cList->setItem(0, i, candItem);
        }
    }
    if (!isVertical)
        cList->setRowHeight(0, QFontMetrics(cList->font()).height() + 2);
}

void CandidateWindow::updateSize()
{
    // size adjustment
    cList->updateGeometry();
    setFixedSize(sizeHint());
}

void CandidateWindow::setIndex(int totalindex, int displayLimit,
    int candidateIndex)
{
    // select item
    if (candidateIndex >= 0) {
        int pos = totalindex;
        if (displayLimit)
            pos = candidateIndex % displayLimit;

        int row;
        int column;
        if (isVertical) {
            row = pos;
            column = 0;
        } else {
            row = 0;
            column = pos;
        }
        if (cList->item(row, column)
            && !cList->item(row, column)->isSelected()) {
            cList->clearSelection();
            if (isVertical)
                cList->selectRow(pos);
            else
                cList->selectColumn(pos);
        }
    } else {
        cList->clearSelection();
    }

    fprintf(stdout, "update_label\f\f");
    fflush(stdout);
}

void CandidateWindow::slotCandidateSelected(int row, int column)
{
    fprintf(stdout, "set_candidate_index_2\f%d\f\f", isVertical ? row : column);
    fflush(stdout);
    fprintf(stdout, "update_label\f\f");
    fflush(stdout);
}

void CandidateWindow::shiftPage(int idx)
{
    cList->clearSelection();
    if (isVertical)
        cList->selectRow(idx);
    else
        cList->selectColumn(idx);
}

void CandidateWindow::slotHookSubwindow()
{
    if (!hasAnnotation || !subWin)
        return;

    QList<QTableWidgetItem *> list = cList->selectedItems();
    if (list.isEmpty())
        return;
    QTableWidgetItem *item = list[0];

    // cancel previous hook
    subWin->cancelHook();

    // hook annotation
    QString annotationString
        = annotations.at(isVertical ? item->row() : item->column());
    if (!annotationString.isEmpty()) {
        subWin->layoutWindow(subWindowRect(frameGeometry(), item),
            isVertical);
        subWin->hookPopup(annotationString);
    }
}

// Moving and Resizing affects the position of Subwindow
void CandidateWindow::moveEvent(QMoveEvent *e)
{
    // move subwindow
    if (subWin)
        subWin->layoutWindow(subWindowRect(QRect(e->pos(), size())),
            isVertical);
}

void CandidateWindow::resizeEvent(QResizeEvent *e)
{
    // move subwindow
    if (subWin)
        subWin->layoutWindow(subWindowRect(QRect(pos(), e->size())),
            isVertical);
}

void CandidateWindow::hideEvent(QHideEvent *event)
{
    AbstractCandidateWindow::hideEvent(event);
    if (subWin)
        subWin->cancelHook();
}

QRect CandidateWindow::subWindowRect(const QRect &rect,
    const QTableWidgetItem *item)
{
    if (!item) {
        QList<QTableWidgetItem *> list = cList->selectedItems();
        if (list.isEmpty())
            return rect;
        item = list[0];
    }
    QRect r = rect;
    if (isVertical) {
        r.setY(rect.y() + cList->rowHeight(0) * item->row());
    } else {
        int xdiff = 0;
        for (int i = 0, j = item->column(); i < j; i++) {
            xdiff += cList->columnWidth(i);
        }
        r.setX(rect.x() + xdiff);
    }
    return r;
}

QSize CandidateWindow::sizeHint() const
{
    QSize cListSizeHint = cList->sizeHint();

    // According to the Qt4 documentation on the QFrame class,
    // the frame width is 1 pixel.
    int frame = 1 * 2;
    int width = cListSizeHint.width() + frame;
    int height = cListSizeHint.height() + numLabel->height() + frame;

    return QSize(width, height);
}

QSize CandidateListView::sizeHint() const
{
    // frame width
    int frame = style()->pixelMetric(QStyle::PM_DefaultFrameWidth) * 2;

    // the size of the dummy row should be 0.
    const int rowNum = isVertical ? rowCount() : rowCount() - 1;
    if (rowNum == 0) {
        return QSize(MIN_CAND_WIDTH, frame);
    }
    int width = frame;
    // the size of the dummy column should be 0.
    for (int i = 0; i < columnCount() - 1; i++)
        width += columnWidth(i);

    return QSize(width, rowHeight(0) * rowNum + frame);
}
