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

#include <tqtooltip.h>
#include <tqtoolbutton.h>
#include <tqimage.h>
#include <tqprocess.h>
#include <tqmessagebox.h>

#include <cstdlib>

#include "uim/uim-scm.h"
#include "qtgettext.h"

static const TQString ICONDIR = UIM_PIXMAPSDIR;
static const TQString ACTION_ICONDIR = TDE_ICONDIR "/crystalsvg/16x16/actions";

static void launchHelperApplication( const TQString &command )
{
    if ( !command.isEmpty() ) {
        TQProcess proc( command );
        proc.setCommunication( 0 );
        if ( ! proc.start() ) {
            TQMessageBox::warning( 0, "uim",
                _( "Cannot launch '%1'." ).arg( command ) );
        }
    }
}

QUimHelperToolbar::QUimHelperToolbar( TQWidget *parent, const char *name, WFlags f, bool isApplet )
    : TQHBox( parent, name, f )
{
    m_indicator = new UimStateIndicator( this );

    TQObject::connect( m_indicator, TQ_SIGNAL( indicatorResized() ), this, TQ_SLOT( slotIndicatorResized() ) );

    TQPixmap swicon = TQPixmap( ICONDIR + "/im_switcher.png" );
    TQPixmap preficon = TQPixmap( ACTION_ICONDIR + "/configure.png");
    TQPixmap dicticon = TQPixmap( ICONDIR + "/uim-dict.png");
    TQPixmap padicon = TQPixmap( ACTION_ICONDIR + "/format-text-bold.png");
    TQPixmap handicon = TQPixmap( ACTION_ICONDIR + "/edit.png");
    TQPixmap helpicon = TQPixmap( ACTION_ICONDIR + "/help-contents.png");
    TQPixmap exiticon = TQPixmap( ACTION_ICONDIR + "/window-close.png");
    TQImage swimage = swicon.convertToImage();
    TQImage prefimage = preficon.convertToImage();
    TQImage dictimage = dicticon.convertToImage();
    TQImage padimage = padicon.convertToImage();
    TQImage handimage = handicon.convertToImage();
    TQImage helpimage = helpicon.convertToImage();
    TQImage exitimage = exiticon.convertToImage();
    m_swicon = swimage.smoothScale(ICON_SIZE, ICON_SIZE);
    m_preficon = prefimage.smoothScale(ICON_SIZE, ICON_SIZE);
    m_dicticon = dictimage.smoothScale(ICON_SIZE, ICON_SIZE);
    m_padicon = padimage.smoothScale(ICON_SIZE, ICON_SIZE);
    m_handicon = handimage.smoothScale(ICON_SIZE, ICON_SIZE);
    m_helpicon = helpimage.smoothScale(ICON_SIZE, ICON_SIZE);
    exiticon = exitimage.smoothScale(ICON_SIZE, ICON_SIZE);

    m_contextMenu = new TQPopupMenu( this );
    m_contextMenu->insertItem( m_swicon,   _("Switch input method"), this, TQ_SLOT(slotExecImSwitcher()) );
    m_contextMenu->insertItem( m_preficon, _("Preference"), this, TQ_SLOT(slotExecPref()) );
    m_contextMenu->insertItem( m_dicticon, _("Japanese dictionary editor"), this, TQ_SLOT(slotExecDict()) );
    m_contextMenu->insertItem( m_padicon, _("Input pad"), this, TQ_SLOT(slotExecInputPad()) );
    m_contextMenu->insertItem( m_handicon, _("Handwriting input pad"), this, TQ_SLOT(slotExecHandwritingInputPad()) );
    m_contextMenu->insertItem( m_helpicon, _("Help"), this, TQ_SLOT(slotExecHelp()) );
    if ( !isApplet )
        m_contextMenu->insertItem( exiticon, _("Quit this toolbar"), this, TQ_SIGNAL(quitToolbar()) );
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

void QUimHelperToolbar::contextMenuEvent( TQContextMenuEvent * e )
{
    if( !m_contextMenu->isShown() )
    {
        m_contextMenu->move( e->globalPos() );
        m_contextMenu->exec();
    }
}

TQPopupMenu *
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

    TQToolButton * swButton = new QHelperToolbarButton( this );
    if( !m_swicon.isNull() )
        swButton->setPixmap( m_swicon );
    else
        swButton->setText( "Sw" );

    TQObject::connect( swButton, TQ_SIGNAL( clicked() ),
                      this, TQ_SLOT( slotExecImSwitcher() ) );
    TQToolTip::add( swButton, _( "Switch input method" ) );
    ++m_nr_exec_buttons;
}


void QUimHelperToolbar::slotExecImSwitcher()
{
    /* exec uim-im-switcher */
    launchHelperApplication( "uim-im-switcher-tqt" );
}

void QUimHelperToolbar::addExecPrefButton()
{
    uim_bool isShowPref = uim_scm_symbol_value_bool("toolbar-show-pref-button?");
    if( isShowPref == UIM_FALSE )
        return;

    TQToolButton * prefButton = new QHelperToolbarButton( this );
    if( !m_preficon.isNull() )
        prefButton->setPixmap( m_preficon );
    else
        prefButton->setText( "Pref" );

    TQObject::connect( prefButton, TQ_SIGNAL( clicked() ),
                      this, TQ_SLOT( slotExecPref() ) );
    TQToolTip::add( prefButton, _( "Preference" ) );
    ++m_nr_exec_buttons;
}

void QUimHelperToolbar::slotExecPref()
{
    /* exec uim-pref */
    launchHelperApplication( "uim-pref-tqt" );
}

void QUimHelperToolbar::addExecDictButton()
{
    uim_bool isShowDict = uim_scm_symbol_value_bool("toolbar-show-dict-button?");
    if( isShowDict == UIM_FALSE )
        return;

    TQToolButton *dictButton = new QHelperToolbarButton( this );
    if( !m_dicticon.isNull() )
        dictButton->setPixmap( m_dicticon );
    else
        dictButton->setText( "Dic" );

    TQObject::connect( dictButton, TQ_SIGNAL( clicked() ),
                      this, TQ_SLOT( slotExecDict() ) );
    TQToolTip::add( dictButton, _( "Japanese dictionary editor" ) );
    ++m_nr_exec_buttons;
}

void QUimHelperToolbar::slotExecDict()
{
    /* exec uim-dict */
    launchHelperApplication( "uim-dict-gtk" );
}

void QUimHelperToolbar::addExecInputPadButton()
{
    uim_bool isShowInputPad = uim_scm_symbol_value_bool("toolbar-show-input-pad-button?");
    if( isShowInputPad == UIM_FALSE )
        return;

    TQToolButton *inputpadButton = new QHelperToolbarButton( this );
    if( !m_padicon.isNull() )
        inputpadButton->setPixmap( m_padicon );
    else
        inputpadButton->setText( "Pad" );

    TQObject::connect( inputpadButton, TQ_SIGNAL( clicked() ),
                      this, TQ_SLOT( slotExecInputPad() ) );
    TQToolTip::add( inputpadButton, _( "Input pad" ) );
    ++m_nr_exec_buttons;
}

void QUimHelperToolbar::slotExecInputPad()
{
    /* exec input pad */
    launchHelperApplication( "uim-chardict-tqt");
}

void QUimHelperToolbar::addExecHandwritingInputPadButton()
{
    uim_bool isShowHandwritingInputPad = uim_scm_symbol_value_bool("toolbar-show-handwriting-input-pad-button?");
    if( isShowHandwritingInputPad == UIM_FALSE )
        return;

    TQToolButton *handwritingButton = new QHelperToolbarButton( this );
    if( !m_handicon.isNull() )
        handwritingButton->setPixmap( m_handicon );
    else
        handwritingButton->setText( "Hand" );

    TQObject::connect( handwritingButton, TQ_SIGNAL( clicked() ),
                      this, TQ_SLOT( slotExecHandwritingInputPad() ) );
    TQToolTip::add( handwritingButton, _( "Handwriting input pad" ) );
    ++m_nr_exec_buttons;
}

void QUimHelperToolbar::slotExecHandwritingInputPad()
{
    launchHelperApplication( "uim-tomoe-gtk" );
}

void QUimHelperToolbar::addExecHelpButton()
{
    uim_bool isShowHelp = uim_scm_symbol_value_bool("toolbar-show-help-button?");
    if( isShowHelp == UIM_FALSE )
        return;

    TQToolButton *helpButton = new QHelperToolbarButton( this );
    if( !m_helpicon.isNull() )
        helpButton->setPixmap( m_helpicon );
    else
        helpButton->setText( "Help" );

    TQObject::connect( helpButton, TQ_SIGNAL( clicked() ),
                      this, TQ_SLOT( slotExecHelp() ) );
    TQToolTip::add( helpButton, _( "Help" ) );
    ++m_nr_exec_buttons;
}

void QUimHelperToolbar::slotExecHelp()
{
    launchHelperApplication( "uim-help" );
}

#include "common-quimhelpertoolbar.moc"
