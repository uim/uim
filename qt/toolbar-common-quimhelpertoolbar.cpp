/*

 Copyright (c) 2003,2004,2005 uim Project http://uim.freedesktop.org/

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

 THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 SUCH DAMAGE.

*/
#include "toolbar-common-quimhelpertoolbar.h"
#include "toolbar-common-uimstateindicator.h"

#include <qtooltip.h>
#include <qtoolbutton.h>

#include <stdlib.h>

#define i18n(string) (string)

QUimHelperToolbar::QUimHelperToolbar( QWidget *parent, const char *name, WFlags f )
        : QHBox( parent, name, f )
{
    new UimStateIndicator( this );

    // switcher exec button
    addExecImSwitcherButton();

    // kasumi exec button (configure option)
    addExecKasumiButton();
}

QUimHelperToolbar::~QUimHelperToolbar()
{}

void QUimHelperToolbar::addExecImSwitcherButton()
{
    QToolButton * swbutton = new QToolButton( this );
    swbutton->setText( i18n( "sw" ) );
    QObject::connect( swbutton, SIGNAL( clicked() ),
                      this, SLOT( slotExecSwitcher() ) );
    QToolTip::add( swbutton, i18n( "exec im-switcher" ) );
}


void QUimHelperToolbar::slotExecSwitcher()
{
    /* exec uim-im-switcher */
    system( "uim-im-switcher-qt &" );
}

void QUimHelperToolbar::addExecKasumiButton()
{
#ifdef USE_KASUMI
    QToolButton * kasumiButton = new QToolButton( this );
    kasumiButton->setText( i18n( "Kasumi" ) );
    QObject::connect( kasumiButton, SIGNAL( clicked() ),
                      this, SLOT( slotExecKasumi() ) );
    QToolTip::add( kasumiButton, i18n( "exec Kasumi" ) );
#endif
}

void QUimHelperToolbar::slotExecKasumi()
{
#ifdef USE_KASUMI
    /* exec kasumi */
    system( "kasumi &" );
#endif
}

#include "toolbar-common-quimhelpertoolbar.moc"
