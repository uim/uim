/*

  copyright (c) 2010 uim Project http://code.google.com/p/uim/

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
#include "candidatetablewindow.h"

#include <QtGui/QLabel>
#include <QtGui/QFontMetrics>
#include <QtGui/QGridLayout>
#include <QtGui/QStyle>
#include <QtGui/QVBoxLayout>

#include <uim/uim-scm.h>

#include "quiminputcontext.h"

static const int TABLE_NR_CELLS = TABLE_NR_COLUMNS * TABLE_NR_ROWS;

static const int BLOCK_SPACING = 20;
static const int HOMEPOSITION_SPACING = 2;

// labelchar_table consists of four blocks
//   blockLR  blockA
//   blockLRS blockAS
static const int L_WIDTH = 5;
static const int R_WIDTH = 5;
static const int A_WIDTH = 3;

static const int A_HEIGHT = 4;
static const int AS_HEIGHT = 4;

// 106 keyboard
static char DEFAULT_TABLE[TABLE_NR_CELLS] = {
  '1','2','3','4','5', '6','7','8','9','0',   '-','^','\\',
  'q','w','e','r','t', 'y','u','i','o','p',   '@','[','\0',
  'a','s','d','f','g', 'h','j','k','l',';',   ':',']','\0',
  'z','x','c','v','b', 'n','m',',','.','/',   '\0','\0',' ',
  '!','"','#','$','%', '&','\'','(',')','\0', '=','~','|',
  'Q','W','E','R','T', 'Y','U','I','O','P',   '`','{','\0',
  'A','S','D','F','G', 'H','J','K','L','+',   '*','}','\0',
  'Z','X','C','V','B', 'N','M','<','>','?',   '_','\0','\0',
};

CandidateTableWindow::CandidateTableWindow(QWidget *parent)
: AbstractCandidateWindow(parent)
{
    QGridLayout *lLayout = createLayout(A_HEIGHT, L_WIDTH, 0, 0);
    QGridLayout *rLayout = createLayout(A_HEIGHT, R_WIDTH, 0, L_WIDTH);
    aLayout = createLayout(A_HEIGHT, A_WIDTH, 0, L_WIDTH + R_WIDTH);
    lsLayout = createLayout(AS_HEIGHT, L_WIDTH, A_HEIGHT, 0);
    rsLayout = createLayout(AS_HEIGHT, R_WIDTH, A_HEIGHT, L_WIDTH);
    asLayout = createLayout(AS_HEIGHT, A_WIDTH, A_HEIGHT, L_WIDTH + R_WIDTH);

    QGridLayout *buttonLayout = new QGridLayout;
    buttonLayout->setSpacing(BLOCK_SPACING - 2 * HOMEPOSITION_SPACING);
    buttonLayout->setMargin(0);
    buttonLayout->addLayout(lLayout, 0, 0);
    buttonLayout->addLayout(rLayout, 0, 1);
    buttonLayout->addLayout(aLayout, 0, 2);
    buttonLayout->addLayout(lsLayout, 1, 0);
    buttonLayout->addLayout(rsLayout, 1, 1);
    buttonLayout->addLayout(asLayout, 1, 2);

    QVBoxLayout *layout = new QVBoxLayout;
    layout->setMargin(0);
    layout->setSpacing(0);
    layout->addLayout(buttonLayout);
    layout->addWidget(numLabel);

    setLayout(layout);

    initTable();
}

CandidateTableWindow::~CandidateTableWindow()
{
    if (table != DEFAULT_TABLE)
        free(table);
}

QGridLayout *CandidateTableWindow::createLayout(int row, int column,
        int rowOffset, int columnOffset)
{
    QGridLayout *layout = new QGridLayout;
    layout->setSpacing(HOMEPOSITION_SPACING);
    layout->setMargin(0);
    for (int i = 0; i < row; i++) {
        for (int j = 0; j < column; j++) {
            KeyButton *button = new KeyButton;
            connect(button, SIGNAL(candidateClicked(int)),
                this, SLOT(slotCandidateClicked(int)));
            buttonArray[i + rowOffset][j + columnOffset] = button;
            layout->addWidget(button, i, j);
        }
    }
    return layout;
}

static char *initTableInternal()
{
    uim_lisp list = uim_scm_symbol_value("uim-candwin-prog-layout");
    if (!list || !uim_scm_listp(list))
        return DEFAULT_TABLE;
    size_t len = 0;
    void **array = uim_scm_list2array(list, &len,
        reinterpret_cast<void *(*)(uim_lisp)>(uim_scm_c_str));
    if (!array || len <= 0) {
        free(array);
        return DEFAULT_TABLE;
    }
    char *table = static_cast<char *>(malloc(TABLE_NR_CELLS * sizeof(char)));
    if (!table) {
        free(array);
        return DEFAULT_TABLE;
    }
    for (int i = 0; i < TABLE_NR_CELLS; i++) {
        if (i >= static_cast<int>(len)) {
            table[i] = '\0';
            continue;
        }
        char *str = static_cast<char *>(array[i]);
        // XXX: only use first char
        table[i] = str[0];
    }
    free(array);
    return table;
}

void CandidateTableWindow::initTable()
{
    uim_gc_gate_func_ptr func_body
        = reinterpret_cast<uim_gc_gate_func_ptr>(initTableInternal);
    void *ret = uim_scm_call_with_gc_ready_stack(func_body, 0);
    table = static_cast<char *>(ret);
}

void CandidateTableWindow::slotCandidateClicked(int index)
{
    if (ic && ic->uimContext())
        uim_set_candidate_index(ic->uimContext(), index);
    updateLabel();
}

void CandidateTableWindow::setPage(int page)
{
#ifdef ENABLE_DEBUG
    qDebug("setPage : page = %d", page);
#endif

    // calculate page
    int lastpage = displayLimit ? nrCandidates / displayLimit : 0;

    int newpage;
    if (page < 0)
        newpage = lastpage;
    else if ( page > lastpage)
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
    // ex. if nrCandidate==14 and displayLimit==10, the number of
    //     last page's item==4
    int ncandidates = displayLimit;
    if (newpage == lastpage)
        ncandidates = nrCandidates - displayLimit * lastpage;

    int index = 0;
    int delta = 0;
    for (int i = 0; i < TABLE_NR_ROWS; i++) {
        for (int j = 0; j < TABLE_NR_COLUMNS; j++) {
            if (table[index] == '\0') {
                buttonArray[i][j]->hide();
                delta++;
                index++;
                continue;
            }
            if (index - delta >= ncandidates) {
                buttonArray[i][j]->setEnabled(false);
                buttonArray[i][j]->setText("");
                buttonArray[i][j]->setIndex(-1);
                continue;
            }
            int candidateIndex = displayLimit * newpage + index - delta;
            uim_candidate cand = stores[candidateIndex];
            QString candString
                = QString::fromUtf8(uim_candidate_get_cand_str(cand));
            if (!candString.isEmpty()) {
                buttonArray[i][j]->setEnabled(true);
                // '&' shouldn't be used as the shortcut key
                buttonArray[i][j]->setText(candString.replace('&', "&&"));
                buttonArray[i][j]->setIndex(candidateIndex);
            } else {
                buttonArray[i][j]->setEnabled(false);
                buttonArray[i][j]->setText("");
                buttonArray[i][j]->setIndex(-1);
            }
            index++;
        }
    }
    setTable();

    // set index
    if (newindex != candidateIndex)
        setIndex(newindex);
    else
        updateLabel();
}

static bool isEmptyBlock(QGridLayout *layout)
{
    for (int i = 0; i < layout->count(); i++) {
        QWidget *widget = layout->itemAt(i)->widget();
        if (widget && widget->isEnabled())
            return false;
    }
    return true;
}

void CandidateTableWindow::setBlockVisible(QLayout *layout, bool visible)
{
    if (visible == layout->isEnabled())
        return;
    layout->setEnabled(visible);
    for (int i = 0; i < layout->count(); i++) {
        QWidget *widget = layout->itemAt(i)->widget();
        if (widget)
            widget->setVisible(visible);
    }
}

void CandidateTableWindow::setTable()
{
    // hide empty blocks.
    // pattern0 (full table)
    //   blockLR  blockA
    //   blockLRS blockAS (for shift key)
    // pattern1 (minimal blocks)
    //   blockLR
    // pattern2 (without shift blocks)
    //   blockLR  blockA
    // pattern3 (without symbol blocks)
    //   blockLR
    //   blockLRS
    bool hasBlockA = !isEmptyBlock(aLayout);
    bool hasBlockAs = !isEmptyBlock(asLayout);
    bool hasBlockLrs = !(isEmptyBlock(lsLayout) && isEmptyBlock(rsLayout));

    setBlockVisible(aLayout, hasBlockA || hasBlockAs);
    setBlockVisible(asLayout, hasBlockAs);
    setBlockVisible(lsLayout, hasBlockLrs || hasBlockAs);
    setBlockVisible(rsLayout, hasBlockLrs || hasBlockAs);
}

KeyButton::KeyButton() : m_index(-1)
{
    connect(this, SIGNAL(clicked()), this, SLOT(slotClicked()));
}

QSize KeyButton::sizeHint() const
{
    QSize size = QPushButton::sizeHint();
    int margin = style()->pixelMetric(QStyle::PM_ButtonMargin);
    int width = qMax(size.height(),
        QFontMetrics(QFont()).boundingRect(text()).width() + margin * 2);
    return QSize(width, size.height());
}

void KeyButton::setIndex(int index)
{
    m_index = index;
}

int KeyButton::index() const
{
    return m_index;
}

void KeyButton::slotClicked()
{
    if (m_index >= 0)
        emit candidateClicked(m_index);
}
