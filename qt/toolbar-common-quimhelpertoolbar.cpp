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

#include "uim/uim-compat-scm.h"
#include "qtgettext.h"

static const QString ICONDIR = UIM_PIXMAPSDIR;

QUimHelperToolbar::QUimHelperToolbar( QWidget *parent, const char *name, WFlags f )
    : QHBox( parent, name, f )
{
    new UimStateIndicator( this );

    m_swicon = QPixmap( ICONDIR + "/switcher-icon.png" );
    m_preficon = QPixmap( ICONDIR + "/configure-qt.png");

    m_contextMenu = new QPopupMenu( this );
    m_contextMenu->insertItem( m_swicon,   _("Execute uim's input method switcher"), this, SLOT(slotExecImSwitcher()) );
    m_contextMenu->insertItem( m_preficon, _("Execute uim's preference tool"), this, SLOT(slotExecPref()) );
    m_contextMenu->insertItem( _("Execute uim's japanese dictionary tool"), this, SLOT(slotExecDict()) );
    m_contextMenu->insertItem( _("Execute uim's input pad tool"), this, SLOT(slotExecInputPad()) );
    m_contextMenu->insertItem( _("Execute uim's handwriting input pad tool"), this, SLOT(slotExecHandwritingInputPad()) );
    m_contextMenu->insertItem( _("Execute uim's help tool"), this, SLOT(slotExecHelp()) );
    m_contextMenu->insertItem( _("Quit this toolbar"), this, SIGNAL(quitToolbar()) );

    // toolbar buttons    
    addExecImSwitcherButton();
    addExecPrefButton();
    addExecDictButton();
    addExecInputPadButton();
    addExecHandwritingInputPadButton();
    addExecHelpButton();
}

QUimHelperToolbar::~QUimHelperToolbar()
{
}

void QUimHelperToolbar::contextMenuEvent( QContextMenuEvent * e )
{
    if( !m_contextMenu->isShown() )
    {
        m_contextMenu->move( e->globalPos() );
        m_contextMenu->exec();
    }
}

void QUimHelperToolbar::addExecImSwitcherButton()
{
    uim_bool isShowSwitcher = uim_scm_symbol_value_bool("toolbar-show-switcher-button?");
    if( isShowSwitcher == UIM_FALSE )
        return;

    QToolButton * swButton = new QHelperToolbarButton( this );
    if( !m_swicon.isNull() )
        swButton->setPixmap( m_swicon );
    else
        swButton->setText( "Sw" );

    QObject::connect( swButton, SIGNAL( clicked() ),
                      this, SLOT( slotExecImSwitcher() ) );
    QToolTip::add( swButton, _( "exec im-switcher" ) );
}


void QUimHelperToolbar::slotExecImSwitcher()
{
    /* exec uim-im-switcher */
    system( "uim-im-switcher-qt &" );
}

void QUimHelperToolbar::addExecPrefButton()
{
    uim_bool isShowPref = uim_scm_symbol_value_bool("toolbar-show-pref-button?");
    if( isShowPref == UIM_FALSE )
        return;
    
    QToolButton * prefButton = new QHelperToolbarButton( this );
    if( !m_preficon.isNull() )
        prefButton->setPixmap( m_preficon );
    else
        prefButton->setText( "Pref" );

    QObject::connect( prefButton, SIGNAL( clicked() ),
                      this, SLOT( slotExecPref() ) );
    QToolTip::add( prefButton, _( "exec Preference Application" ) );
}

void QUimHelperToolbar::slotExecPref()
{
    /* exec uim-pref-qt */
    system( "uim-pref-qt &" );
}

void QUimHelperToolbar::addExecDictButton()
{
    uim_bool isShowDict = uim_scm_symbol_value_bool("toolbar-show-dict-button?");
    if( isShowDict == UIM_FALSE )
        return;

    QToolButton *dictButton = new QHelperToolbarButton( this );
    dictButton->setText( "Dic" );

    QObject::connect( dictButton, SIGNAL( clicked() ),
                      this, SLOT( slotExecDict() ) );
    QToolTip::add( dictButton, _( "exec Japanese dictionary Tool Application" ) );
}

void QUimHelperToolbar::slotExecDict()
{
    /* exec uim-dict */
    system( "uim-dict-gtk&" );
}

void QUimHelperToolbar::addExecInputPadButton()
{
    uim_bool isShowInputPad = uim_scm_symbol_value_bool("toolbar-show-input-pad-button?");
    if( isShowInputPad == UIM_FALSE )
        return;

    QToolButton *inputpadButton = new QHelperToolbarButton( this );
    inputpadButton->setText( "Pad" );

    QObject::connect( inputpadButton, SIGNAL( clicked() ),
                      this, SLOT( slotExecInputPad() ) );
    QToolTip::add( inputpadButton, _( "exec Input Pad Tool Application" ) );
}

void QUimHelperToolbar::slotExecInputPad()
{
    /* exec input pad */
    system( "uim-chardict-qt &");
}

void QUimHelperToolbar::addExecHandwritingInputPadButton()
{
    uim_bool isShowHandwritingInputPad = uim_scm_symbol_value_bool("toolbar-show-handwriting-input-pad-button?");
    if( isShowHandwritingInputPad == UIM_FALSE )
        return;

    QToolButton *inputpadButton = new QHelperToolbarButton( this );
    inputpadButton->setText( "Hand" );

    QObject::connect( inputpadButton, SIGNAL( clicked() ),
                      this, SLOT( slotExecHandwritingInputPad() ) );
    QToolTip::add( inputpadButton, _( "exec Handwriting Input Pad Tool Application" ) );
}

void QUimHelperToolbar::slotExecHandwritingInputPad()
{
    system( "uim-tomoe-gtk &" );
}

void QUimHelperToolbar::addExecHelpButton()
{
    uim_bool isShowHelp = uim_scm_symbol_value_bool("toolbar-show-help-button?");
    if( isShowHelp == UIM_FALSE )
        return;

    QToolButton *helpButton = new QHelperToolbarButton( this );
    helpButton->setText( "Help" );

    QObject::connect( helpButton, SIGNAL( clicked() ),
                      this, SLOT( slotExecHelp() ) );
    QToolTip::add( helpButton, _( "exec Help Application" ) );
}

void QUimHelperToolbar::slotExecHelp()
{
    system( "uim-help &" );
}

#include "toolbar-common-quimhelpertoolbar.moc"
