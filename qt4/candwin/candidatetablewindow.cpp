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
#include "candidatetablewindow.h"

#include <cstdio>

#include <QtGui/QFontMetrics>
#if QT_VERSION < 0x050000
# include <QtGui/QLabel>
# include <QtGui/QGridLayout>
# include <QtGui/QStyle>
# include <QtGui/QVBoxLayout>
#else
# include <QtWidgets/QLabel>
# include <QtWidgets/QGridLayout>
# include <QtWidgets/QStyle>
# include <QtWidgets/QVBoxLayout>
#endif

#include <uim/uim-scm.h>

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
    initTable();

    lLayout = createLayout(A_HEIGHT, L_WIDTH, 0, 0);
    rLayout = createLayout(A_HEIGHT, R_WIDTH, 0, L_WIDTH);
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
}

CandidateTableWindow::~CandidateTableWindow()
{
    if (table != DEFAULT_TABLE)
        free(table);
}

QSize CandidateTableWindow::sizeHint() const
{
    QRect lRect = lLayout->geometry();

    // height
    // numLabel + lLayout
    int height = numLabel->height() + lRect.height();
    if (lsLayout->isEnabled()) {
        // lsLayout
        height += lsLayout->geometry().height()
            + BLOCK_SPACING - 2 * HOMEPOSITION_SPACING;
    }

    // width
    // lLayout + rLayout
    int width = lRect.width() + rLayout->geometry().width()
        + BLOCK_SPACING - 2 * HOMEPOSITION_SPACING;
    if (aLayout->isEnabled()) {
        // aLayout
        width += aLayout->geometry().width()
            + BLOCK_SPACING - 2 * HOMEPOSITION_SPACING;
    }

    return QSize(width, height);
}

void CandidateTableWindow::slotCandidateClicked(int index)
{
    fprintf(stdout, "set_candidate_index\f%d\f\f", index);
    fflush(stdout);
    fprintf(stdout, "update_label\f\f");
    fflush(stdout);
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
            int r = i + rowOffset;
            int c = j + columnOffset;
            buttonArray[r][c] = button;
            if (table[r * TABLE_NR_COLUMNS + c] == '\0') {
                // Hide this button because some styles such as Oxygen
                // ignore the flat property.
                button->hide();
                button->setFlat(true);
            }
            layout->addWidget(button, i, j);
        }
    }
    layout->addItem(new QSpacerItem(0, 0,
        QSizePolicy::Expanding, QSizePolicy::Expanding), row, column);
    return layout;
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
        QPushButton *widget
            = qobject_cast<QPushButton*>(layout->itemAt(i)->widget());
        // Flat buttons shouldn't be shown.
        if (widget && !(visible && widget->isFlat()))
            widget->setVisible(visible);
    }
}

void CandidateTableWindow::updateView(int ncandidates,
    const QList<CandData> &stores)
{
    for (int i = 0; i < TABLE_NR_ROWS; i++) {
        for (int j = 0; j < TABLE_NR_COLUMNS; j++) {
            KeyButton *button = buttonArray[i][j];
            button->setIndex(-1);
            button->setEnabled(false);
            button->setText("");
        }
    }
    int index = 0;
    int delta = 0;
    for (int i = 0; i < TABLE_NR_ROWS; i++) {
        for (int j = 0; j < TABLE_NR_COLUMNS; j++) {
            if (table[index] == '\0') {
                delta++;
                index++;
                continue;
            }
            if (index - delta >= ncandidates)
                continue;
            int candidateIndex = index - delta;
            CandData cand = stores[candidateIndex];
            QString candString = cand.str;
            if (!candString.isEmpty()) {
                int row = i;
                int column = j;
                QString headString = cand.headingLabel;
                getButtonPosition(row, column, headString);
                KeyButton *b = buttonArray[row][column];
                // '&' shouldn't be used as the shortcut key
                b->setText(candString.replace('&', "&&"));
                b->setIndex(candidateIndex);
                b->setEnabled(true);
            }
            index++;
        }
    }
}

void CandidateTableWindow::updateSize()
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
    setBlockVisible(asLayout, hasBlockAs || (hasBlockA && hasBlockLrs));
    setBlockVisible(lsLayout, hasBlockLrs || hasBlockAs);
    setBlockVisible(rsLayout, hasBlockLrs || hasBlockAs);

    setMaximumSize(sizeHint());
    setMinimumSize(QSize(0, 0));
}

void CandidateTableWindow::setIndex(int totalindex, int displayLimit,
    int candidateIndex)
{
    Q_UNUSED(totalindex)
    Q_UNUSED(displayLimit)
    Q_UNUSED(candidateIndex)
    fprintf(stdout, "update_label\f\f");
    fflush(stdout);
}

void CandidateTableWindow::getButtonPosition(int &row, int &column,
    const QString &headString)
{
    char *ch = table;
    for (int i = 0; i < TABLE_NR_ROWS; i++) {
        for (int j = 0; j < TABLE_NR_COLUMNS; j++) {
            if (*ch == '\0') {
                ch++;
                continue;
            }
            const char str[] = {*ch, '\0'};
            if (headString == QLatin1String(str)) {
                row = i;
                column = j;
                return;
            }
            ch++;
        }
    }
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
