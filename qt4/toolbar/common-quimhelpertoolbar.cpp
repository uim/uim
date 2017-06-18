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

#include <config.h>

#include <QtCore/QProcess>
#include <QtGui/QContextMenuEvent>
#if QT_VERSION < 0x050000
# include <QtGui/QHBoxLayout>
# include <QtGui/QMenu>
# include <QtGui/QMessageBox>
# include <QtGui/QToolButton>
#else
# include <QtWidgets/QHBoxLayout>
# include <QtWidgets/QMenu>
# include <QtWidgets/QMessageBox>
# include <QtWidgets/QToolButton>
#endif

#include "uim/uim-scm.h"
#include "qtgettext.h"

static void launchHelperApplication( const QString &command )
{
    if ( !command.isEmpty() && !QProcess::startDetached( command ) ) {
        QMessageBox::warning( 0, "uim",
            _( "Cannot launch '%1'." ).arg( command ) );
    }
}

QUimHelperToolbar::QUimHelperToolbar( QWidget *parent, bool isApplet )
    : QFrame( parent )
{
    m_layout = new QHBoxLayout;
    m_layout->setMargin( 0 );
    m_layout->setSpacing( 0 );

    m_indicator = new UimStateIndicator( this );
    m_layout->addWidget( m_indicator );

    connect( m_indicator, SIGNAL( indicatorResized() ),
        this, SLOT( slotIndicatorResized() ) );
    connect( m_indicator, SIGNAL( menuRequested( QMenu* ) ),
        this, SIGNAL( menuRequested( QMenu* ) ) );

    const QString ICONDIR = UIM_PIXMAPSDIR;
    const QString ACTION_ICONDIR = KDE4_ICONDIR "/oxygen/16x16/actions";
    const QSize size( ICON_SIZE, ICON_SIZE );
    m_swicon = QPixmap( ICONDIR + "/im_switcher.png" ).scaled(
        size, Qt::IgnoreAspectRatio, Qt::SmoothTransformation );
    m_preficon = QPixmap( ACTION_ICONDIR + "/configure.png" ).scaled(
        size, Qt::IgnoreAspectRatio, Qt::SmoothTransformation );
    m_dicticon = QPixmap( ICONDIR + "/uim-dict.png" ).scaled(
        size, Qt::IgnoreAspectRatio, Qt::SmoothTransformation );
    m_padicon = QPixmap( ACTION_ICONDIR + "/format-text-bold.png" ).scaled(
        size, Qt::IgnoreAspectRatio, Qt::SmoothTransformation );
    m_handicon = QPixmap( ACTION_ICONDIR + "/document-edit.png" ).scaled(
        size, Qt::IgnoreAspectRatio, Qt::SmoothTransformation );
    m_helpicon = QPixmap( ACTION_ICONDIR + "/help-contents.png" ).scaled(
        size, Qt::IgnoreAspectRatio, Qt::SmoothTransformation );
    QPixmap exiticon = QPixmap( ACTION_ICONDIR + "/window-close.png" ).scaled(
        size, Qt::IgnoreAspectRatio, Qt::SmoothTransformation );

    m_contextMenu = new QMenu( isApplet ? 0 : this );
    m_contextMenu->addAction( m_swicon, _("Switch input method"),
        this, SLOT(slotExecImSwitcher()) );
    m_contextMenu->addAction( m_preficon, _("Preference"),
         this, SLOT(slotExecPref()) );
    m_contextMenu->addAction( m_dicticon, _("Japanese dictionary editor"),
         this, SLOT(slotExecDict()) );
    m_contextMenu->addAction( m_padicon, _("Input pad"),
         this, SLOT(slotExecInputPad()) );
    m_contextMenu->addAction( m_handicon, _("Handwriting input pad"),
         this, SLOT(slotExecHandwritingInputPad()) );
    m_contextMenu->addAction( m_helpicon, _("Help"),
         this, SLOT(slotExecHelp()) );
    if ( !isApplet )
        m_contextMenu->addAction( exiticon, _("Quit this toolbar"),
            this, SIGNAL(quitToolbar()) );
    m_nr_exec_buttons = 0;

    // toolbar buttons    
    addExecImSwitcherButton();
    addExecPrefButton();
    addExecDictButton();
    addExecInputPadButton();
    addExecHandwritingInputPadButton();
    addExecHelpButton();

    setLayout(m_layout);
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
    if( m_contextMenu->isHidden() )
    {
#ifdef PLASMA_APPLET_UIM
        Q_UNUSED( e );
        emit menuRequested( m_contextMenu );
#else
        m_contextMenu->move( e->globalPos() );
        m_contextMenu->exec();
#endif
    }
}

QMenu *
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
    m_layout->addWidget( swButton );
    if( !m_swicon.isNull() )
        swButton->setIcon( QIcon( m_swicon ) );
    else
        swButton->setText( "Sw" );

    connect( swButton, SIGNAL( clicked() ),
                      this, SLOT( slotExecImSwitcher() ) );
    swButton->setToolTip( _( "Switch input method" ) );
    ++m_nr_exec_buttons;
}


void QUimHelperToolbar::slotExecImSwitcher()
{
    /* exec uim-im-switcher */
    launchHelperApplication( "uim-im-switcher-qt4" );
}

void QUimHelperToolbar::addExecPrefButton()
{
    uim_bool isShowPref = uim_scm_symbol_value_bool("toolbar-show-pref-button?");
    if( isShowPref == UIM_FALSE )
        return;
    
    QToolButton * prefButton = new QHelperToolbarButton( this );
    m_layout->addWidget( prefButton );
    if( !m_preficon.isNull() )
        prefButton->setIcon( QIcon( m_preficon ) );
    else
        prefButton->setText( "Pref" );

    connect( prefButton, SIGNAL( clicked() ),
                      this, SLOT( slotExecPref() ) );
    prefButton->setToolTip( _( "Preference" ) );
    ++m_nr_exec_buttons;
}

void QUimHelperToolbar::slotExecPref()
{
    /* exec uim-pref-qt4 */
    launchHelperApplication( "uim-pref-qt4" );
}

void QUimHelperToolbar::addExecDictButton()
{
    uim_bool isShowDict = uim_scm_symbol_value_bool("toolbar-show-dict-button?");
    if( isShowDict == UIM_FALSE )
        return;

    QToolButton *dictButton = new QHelperToolbarButton( this );
    m_layout->addWidget( dictButton );
    if( !m_dicticon.isNull() )
        dictButton->setIcon( QIcon( m_dicticon ) );
    else
        dictButton->setText( "Dic" );

    connect( dictButton, SIGNAL( clicked() ),
                      this, SLOT( slotExecDict() ) );
    dictButton->setToolTip( _( "Japanese dictionary editor" ) );
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

    QToolButton *inputpadButton = new QHelperToolbarButton( this );
    m_layout->addWidget( inputpadButton );
    if( !m_padicon.isNull() )
        inputpadButton->setIcon( QIcon( m_padicon ) );
    else
        inputpadButton->setText( "Pad" );

    connect( inputpadButton, SIGNAL( clicked() ),
                      this, SLOT( slotExecInputPad() ) );
    inputpadButton->setToolTip( _( "Input pad" ) );
    ++m_nr_exec_buttons;
}

void QUimHelperToolbar::slotExecInputPad()
{
    /* exec input pad */
    launchHelperApplication( "uim-chardict-qt4" );
}

void QUimHelperToolbar::addExecHandwritingInputPadButton()
{
    uim_bool isShowHandwritingInputPad = uim_scm_symbol_value_bool("toolbar-show-handwriting-input-pad-button?");
    if( isShowHandwritingInputPad == UIM_FALSE )
        return;

    QToolButton *handwritingButton = new QHelperToolbarButton( this );
    m_layout->addWidget( handwritingButton );
    if( !m_handicon.isNull() )
        handwritingButton->setIcon( QIcon( m_handicon ) );
    else
        handwritingButton->setText( "Hand" );

    connect( handwritingButton, SIGNAL( clicked() ),
                      this, SLOT( slotExecHandwritingInputPad() ) );
    handwritingButton->setToolTip( _( "Handwriting input pad" ) );
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

    QToolButton *helpButton = new QHelperToolbarButton( this );
    m_layout->addWidget( helpButton );
    if( !m_helpicon.isNull() )
        helpButton->setIcon( QIcon( m_helpicon ) );
    else
        helpButton->setText( "Help" );

    connect( helpButton, SIGNAL( clicked() ),
                      this, SLOT( slotExecHelp() ) );
    helpButton->setToolTip( _( "Help" ) );
    ++m_nr_exec_buttons;
}

void QUimHelperToolbar::slotExecHelp()
{
    launchHelperApplication( "uim-help" );
}

void QUimHelperToolbar::setMargin( int margin )
{
    m_layout->setMargin( margin );
}
