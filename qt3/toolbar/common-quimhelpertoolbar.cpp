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
#include "common-quimhelpertoolbar.h"
#include "common-uimstateindicator.h"

#include <qtooltip.h>
#include <qtoolbutton.h>
#include <qimage.h>

#include <stdlib.h>

#include "uim/uim-scm.h"
#include "qtgettext.h"

static const QString ICONDIR = UIM_PIXMAPSDIR;
static const QString ACTION_ICONDIR = KDE_ICONDIR "/crystalsvg/16x16/actions";

QUimHelperToolbar::QUimHelperToolbar( QWidget *parent, const char *name, WFlags f, bool isApplet )
    : QHBox( parent, name, f )
{
    m_indicator = new UimStateIndicator( this );

    QObject::connect( m_indicator, SIGNAL( indicatorResized() ), this, SLOT( slotIndicatorResized() ) );

    QPixmap swicon = QPixmap( ICONDIR + "/im_switcher.png" );
    QPixmap preficon = QPixmap( ACTION_ICONDIR + "/configure.png");
    QPixmap dicticon = QPixmap( ICONDIR + "/uim-dict.png");
    QPixmap padicon = QPixmap( ACTION_ICONDIR + "/text_bold.png");
    QPixmap handicon = QPixmap( ACTION_ICONDIR + "/edit.png");
    QPixmap helpicon = QPixmap( ACTION_ICONDIR + "/help.png");
    QPixmap exiticon = QPixmap( ACTION_ICONDIR + "/exit.png");
    QImage swimage = swicon.convertToImage();
    QImage prefimage = preficon.convertToImage();
    QImage dictimage = dicticon.convertToImage();
    QImage padimage = padicon.convertToImage();
    QImage handimage = handicon.convertToImage();
    QImage helpimage = helpicon.convertToImage();
    QImage exitimage = exiticon.convertToImage();
    m_swicon = swimage.smoothScale(ICON_SIZE, ICON_SIZE);
    m_preficon = prefimage.smoothScale(ICON_SIZE, ICON_SIZE);
    m_dicticon = dictimage.smoothScale(ICON_SIZE, ICON_SIZE);
    m_padicon = padimage.smoothScale(ICON_SIZE, ICON_SIZE);
    m_handicon = handimage.smoothScale(ICON_SIZE, ICON_SIZE);
    m_helpicon = helpimage.smoothScale(ICON_SIZE, ICON_SIZE);
    exiticon = exitimage.smoothScale(ICON_SIZE, ICON_SIZE);

    m_contextMenu = new QPopupMenu( this );
    m_contextMenu->insertItem( m_swicon,   _("Switch input method"), this, SLOT(slotExecImSwitcher()) );
    m_contextMenu->insertItem( m_preficon, _("Preference"), this, SLOT(slotExecPref()) );
    m_contextMenu->insertItem( m_dicticon, _("Japanese dictionary editor"), this, SLOT(slotExecDict()) );
    m_contextMenu->insertItem( m_padicon, _("Input pad"), this, SLOT(slotExecInputPad()) );
    m_contextMenu->insertItem( m_handicon, _("Handwriting input pad"), this, SLOT(slotExecHandwritingInputPad()) );
    m_contextMenu->insertItem( m_helpicon, _("Help"), this, SLOT(slotExecHelp()) );
    if ( !isApplet )
        m_contextMenu->insertItem( exiticon, _("Quit this toolbar"), this, SIGNAL(quitToolbar()) );
    m_nr_exec_buttons = 0;

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

int QUimHelperToolbar::getNumButtons()
{
    return m_indicator->getNumButtons() + m_nr_exec_buttons;
}

void QUimHelperToolbar::contextMenuEvent( QContextMenuEvent * e )
{
    if( !m_contextMenu->isShown() )
    {
        m_contextMenu->move( e->globalPos() );
        m_contextMenu->exec();
    }
}

QPopupMenu *
QUimHelperToolbar::contextMenu()
{
    return m_contextMenu;
}

void QUimHelperToolbar::slotIndicatorResized()
{
    emit toolbarResized();
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
    QToolTip::add( swButton, _( "Switch input method" ) );
    ++m_nr_exec_buttons;
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
    QToolTip::add( prefButton, _( "Preference" ) );
    ++m_nr_exec_buttons;
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
    if( !m_dicticon.isNull() )
        dictButton->setPixmap( m_dicticon );
    else
        dictButton->setText( "Dic" );

    QObject::connect( dictButton, SIGNAL( clicked() ),
                      this, SLOT( slotExecDict() ) );
    QToolTip::add( dictButton, _( "Japanese dictionary editor" ) );
    ++m_nr_exec_buttons;
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
    if( !m_padicon.isNull() )
        inputpadButton->setPixmap( m_padicon );
    else
        inputpadButton->setText( "Pad" );

    QObject::connect( inputpadButton, SIGNAL( clicked() ),
                      this, SLOT( slotExecInputPad() ) );
    QToolTip::add( inputpadButton, _( "Input pad" ) );
    ++m_nr_exec_buttons;
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

    QToolButton *handwritingButton = new QHelperToolbarButton( this );
    if( !m_handicon.isNull() )
        handwritingButton->setPixmap( m_handicon );
    else
        handwritingButton->setText( "Hand" );

    QObject::connect( handwritingButton, SIGNAL( clicked() ),
                      this, SLOT( slotExecHandwritingInputPad() ) );
    QToolTip::add( handwritingButton, _( "Handwriting input pad" ) );
    ++m_nr_exec_buttons;
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
    if( !m_helpicon.isNull() )
        helpButton->setPixmap( m_helpicon );
    else
        helpButton->setText( "Help" );

    QObject::connect( helpButton, SIGNAL( clicked() ),
                      this, SLOT( slotExecHelp() ) );
    QToolTip::add( helpButton, _( "Help" ) );
    ++m_nr_exec_buttons;
}

void QUimHelperToolbar::slotExecHelp()
{
    system( "uim-help &" );
}

#include "common-quimhelpertoolbar.moc"
