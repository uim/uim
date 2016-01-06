/*

 Copyright (c) 2011-2013 uim Project https://github.com/uim/uim

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
#include "keyeditformbase.h"

#if QT_VERSION < 0x050000
# include <QtGui/QGridLayout>
# include <QtGui/QHBoxLayout>
# include <QtGui/QHeaderView>
# include <QtGui/QPushButton>
# include <QtGui/QSpacerItem>
# include <QtGui/QTreeWidget>
# include <QtGui/QVBoxLayout>
#else
# include <QtWidgets/QGridLayout>
# include <QtWidgets/QHBoxLayout>
# include <QtWidgets/QPushButton>
# include <QtWidgets/QSpacerItem>
# include <QtWidgets/QTreeWidget>
# include <QtWidgets/QVBoxLayout>
#endif

#include "qtgettext.h"

KeyEditFormBase::KeyEditFormBase(QWidget *widget) : QDialog(widget)
{
    m_listView = new QTreeWidget;
    QTreeWidgetItem *item = m_listView->headerItem();
    item->setText(0, _("Key Combination"));

    m_addButton = new QPushButton;
    m_addButton->setText(_("Add..."));

    m_removeButton = new QPushButton;
    m_removeButton->setText(_("Remove"));

    m_editButton = new QPushButton;
    m_editButton->setText(_("Edit..."));

    QSpacerItem *spacer = new QSpacerItem(0, 0,
            QSizePolicy::Minimum, QSizePolicy::Expanding);

    QPushButton *m_okButton = new QPushButton;
    m_okButton->setText(_("OK"));
    connect(m_okButton, SIGNAL(clicked()), this, SLOT(accept()));

    QPushButton *m_cancelButton = new QPushButton;
    m_cancelButton->setText(_("Cancel"));
    connect(m_cancelButton, SIGNAL(clicked()), this, SLOT(reject()));

    QVBoxLayout *vboxLayout = new QVBoxLayout;
    vboxLayout->addWidget(m_addButton);
    vboxLayout->addWidget(m_removeButton);
    vboxLayout->addWidget(m_editButton);
    vboxLayout->addItem(spacer);
    vboxLayout->addWidget(m_okButton);
    vboxLayout->addWidget(m_cancelButton);

    QHBoxLayout *hboxLayout = new QHBoxLayout;
    hboxLayout->addWidget(m_listView);
    hboxLayout->addLayout(vboxLayout);

    QGridLayout *gridLayout = new QGridLayout;
    gridLayout->addLayout(hboxLayout, 0, 0, 1, 1);

    setLayout(gridLayout);
}
