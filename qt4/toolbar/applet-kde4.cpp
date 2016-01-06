/* 
 Copyright (c) 2006-2013 uim Project https://github.com/uim/uim

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

#include "applet-kde4.h"

#include <QtGui/QGraphicsProxyWidget>
#include <QtGui/QGraphicsLinearLayout>

#include <Plasma/ToolButton>

#include "qtgettext.h"
#include "uim.h"

#include "common-quimhelpertoolbar.h"
#include "common-uimstateindicator.h"

K_EXPORT_PLASMA_APPLET(uim, UimApplet)

UimApplet::UimApplet(QObject *parent, const QVariantList &args)
: Plasma::PopupApplet(parent, args)
{
    bindtextdomain(PACKAGE, LOCALEDIR);
    bind_textdomain_codeset(PACKAGE, "UTF-8");
    setSizePolicy(QSizePolicy(QSizePolicy::Fixed, QSizePolicy::Preferred));
}

void UimApplet::init()
{
    uim_init();
    m_toolbar = new QUimHelperToolbar(0, true);
    m_toolbar->setMargin(0);
    m_toolbar->setAttribute(Qt::WA_NoSystemBackground);
    connect(m_toolbar, SIGNAL(toolbarResized()),
            this, SLOT(slotToolbarResized()));
    connect(m_toolbar, SIGNAL(menuRequested(QMenu*)),
            this, SLOT(slotMenuRequested(QMenu*)));

    m_proxy = new QGraphicsProxyWidget;
    m_proxy->setWidget(m_toolbar);

    m_layout = new QGraphicsLinearLayout;
    m_layout->addItem(m_proxy);

    setLayout(m_layout);

    initPopup();
    slotToolbarResized();
}

void UimApplet::initPopup()
{
    QList<QAction *> list = m_toolbar->contextMenu()->actions();

    QGraphicsLinearLayout *layout = new QGraphicsLinearLayout(Qt::Vertical);
    QAction *act;
    foreach (act, list) {
        Plasma::ToolButton *button = new Plasma::ToolButton;
        button->setText(act->text());
        connect(button, SIGNAL(clicked()), act, SLOT(trigger()));

        QToolButton *nativeWidget = button->nativeWidget();
        nativeWidget->setToolButtonStyle(Qt::ToolButtonTextBesideIcon);
        nativeWidget->setIcon(act->icon());

        layout->addItem(button);
    }

    m_widget = new QGraphicsWidget(this);
    m_widget->setLayout(layout);
}

void UimApplet::slotToolbarResized()
{
    m_toolbar->adjustSize();
    qreal lr = 0, tb = 0;
    qreal left, top, right, bottom;
    m_proxy->getContentsMargins(&left, &top, &right, &bottom);
    lr += (left + right);
    tb += (top + bottom);

    m_layout->getContentsMargins(&left, &top, &right, &bottom);
    lr += (left + right);
    tb += (top + bottom);

    getContentsMargins(&left, &top, &right, &bottom);
    lr += (left + right);
    tb += (top + bottom);
    resize(m_toolbar->width() + lr, m_toolbar->height() + tb);
}

void UimApplet::slotMenuRequested(QMenu *menu)
{
    menu->adjustSize();
    menu->exec(popupPosition(menu->size()));
}

QGraphicsWidget *UimApplet::graphicsWidget()
{
    return m_widget;
}
