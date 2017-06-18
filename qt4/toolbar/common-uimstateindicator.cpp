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
#include <config.h>

#include "common-uimstateindicator.h"
#include <uim/uim-scm.h>

#include <QtCore/QFile>
#include <QtCore/QSocketNotifier>
#include <QtCore/QString>
#include <QtCore/QStringList>
#include <QtCore/QTextCodec>
#include <QtGui/QMouseEvent>
#include <QtGui/QPixmap>
#if QT_VERSION < 0x050000
# include <QtGui/QApplication>
# include <QtGui/QHBoxLayout>
#else
# include <QtWidgets/QApplication>
# include <QtWidgets/QHBoxLayout>
#endif

#include <cstring>
#include <cstdlib>

#include "qtgettext.h"

static const QString ICONDIR = UIM_PIXMAPSDIR;
static int uim_fd;
static QHelperToolbarButton *fallbackButton = 0;
static QSocketNotifier *notifier = 0;

static inline QString qstring_(const QString &string)
{
#if ENABLE_NLS
    return mygettext(string.toUtf8().data());
#else
    return string;
#endif
}

UimStateIndicator::UimStateIndicator( QWidget *parent )
        : QFrame( parent )
{
    m_layout = new QHBoxLayout;
    m_layout->setMargin( 0 );
    m_layout->setSpacing( 0 );

    if ( !fallbackButton )
    {
        fallbackButton = new QHelperToolbarButton( this );
        m_layout->addWidget( fallbackButton );
        QPixmap icon = QPixmap( ICONDIR + '/' + "uim-icon.png" );
        if ( !icon.isNull() ) {
            QPixmap scaledIcon = icon.scaled( ICON_SIZE, ICON_SIZE,
                    Qt::IgnoreAspectRatio, Qt::SmoothTransformation );
            fallbackButton->setIcon( QIcon( scaledIcon ) );
        } else {
            fallbackButton->setText( "?" );
        }
        fallbackButton->show();
    }

    clearButtons();

    uim_fd = -1;
    checkHelperConnection();
    uim_helper_client_get_prop_list();
    popupMenuShowing = false;

    setLayout( m_layout );
}


UimStateIndicator::~UimStateIndicator()
{
    delete notifier;
    notifier = 0;

    clearButtons();
}

int UimStateIndicator::getNumButtons()
{
    return buttons.count();
}

void UimStateIndicator::checkHelperConnection()
{
    if ( uim_fd < 0 )
    {
        uim_fd = uim_helper_init_client_fd( helper_disconnect_cb );
        if ( uim_fd > 0 )
        {
            if ( notifier )
                delete notifier;
            notifier = new QSocketNotifier( uim_fd, QSocketNotifier::Read );
            connect( notifier, SIGNAL( activated( int ) ),
                              this, SLOT( slotStdinActivated() ) );
        }
    }
}
void UimStateIndicator::parseHelperStr( const QString& str )
{
    const QStringList lines = str.split( '\n', QString::SkipEmptyParts );
    if ( !lines.isEmpty() && !lines[ 0 ].isEmpty() )
    {
        if ( lines[ 0 ] == "prop_list_update" )
            propListUpdate( lines );
        else if (lines[0] == "custom_reload_notify" )
            uim_prop_reload_configs();
    }
}

void UimStateIndicator::propListUpdate( const QStringList& lines )
{
    if (popupMenuShowing)
        return;

    QHelperPopupMenu *popupMenu = 0;
#ifdef PLASMA_APPLET_UIM
    int prevCount = m_layout->count();
#endif
    foreach ( QHelperToolbarButton *button, buttons )
    {
        if ( m_layout->indexOf( button ) >= 0 )
            m_layout->removeWidget( button );
        if ( buttons.contains( button ) )
        {
            buttons.removeAll( button );
            delete button;
        }
    }

#ifdef PLASMA_APPLET_UIM
    bool isHidden = true;
#else
    char *display_time
        = uim_scm_c_symbol( uim_scm_symbol_value( "toolbar-display-time" ) );
    bool isHidden = strcmp( display_time, "mode" );
#endif
    foreach ( const QString &line, lines )
    {
        const QStringList fields = line.split( '\t', QString::SkipEmptyParts );

        if ( !fields.isEmpty() && !fields[ 0 ].isEmpty() )
        {
            if ( fields[ 0 ].startsWith( QLatin1String( "branch" ) ) )
            {
                if ( fallbackButton )
                {
                    m_layout->removeWidget( fallbackButton );
                    delete fallbackButton;
                    fallbackButton = 0;
                }
                // create button
                QHelperToolbarButton *button = new QHelperToolbarButton;
                m_layout->addWidget( button );
                buttons.append( button );

                uim_bool isDarkBg =
                    uim_scm_symbol_value_bool("toolbar-icon-for-dark-background?");
                const QString append = isDarkBg ? "_dark_background" : "";
                QString fileName = ICONDIR + '/' + fields[1] + append + ".png";
                if ( isDarkBg && !QFile::exists( fileName ) ) {
                  fileName = ICONDIR + '/' + fields[1] + ".png";
                }
                if ( !isHidden && (fields[1] == "direct"
                        || fields[1].endsWith( "_direct" ) ) ) {
                    isHidden = true;
                }
                QPixmap icon = QPixmap( fileName );
                if (!icon.isNull()) {
                    QPixmap scaledIcon = icon.scaled( ICON_SIZE, ICON_SIZE,
                            Qt::IgnoreAspectRatio, Qt::SmoothTransformation );
                    button->setIcon( QIcon( scaledIcon ) );
                } else {
                    button->setText( fields[ 2 ] );
                }
                if ( fields.size() > 3 )
                    button->setToolTip( fields[ 3 ] );

                // create popup
#ifdef PLASMA_APPLET_UIM
                popupMenu = new QHelperPopupMenu( 0 );
#else
                popupMenu = new QHelperPopupMenu( button );
#endif
                connect( popupMenu, SIGNAL( aboutToShow() ),
                    this, SLOT( slotPopupMenuAboutToShow() ) );
                connect( popupMenu, SIGNAL( aboutToHide() ),
                    this, SLOT( slotPopupMenuAboutToHide() ) );
                connect( button, SIGNAL( menuRequested( QMenu* ) ),
                    this, SIGNAL( menuRequested( QMenu* ) ) );
                button->setMenu( popupMenu );
                button->setPopupMode( QToolButton::InstantPopup );

                button->show();
            }
            else if ( fields[ 0 ].startsWith( QLatin1String( "leaf" ) ) )
            {
                if ( popupMenu
                        && !fields[ 1 ].isEmpty()
                        && !fields[ 3 ].isEmpty()
                        && !fields[ 4 ].isEmpty()
                        && !fields[ 5 ].isEmpty() )
                {
                    QAction *action = popupMenu->insertHelperItem(
                        fields[1], qstring_( fields[ 3 ] ),
                        fields[ 4 ], fields[ 5 ] );
                    // check the item which is now used
                    if ( fields.count() > 6 && fields[ 6 ] == "*" )
                        action->setChecked( true );
                }
            }
        }
    }
#ifndef PLASMA_APPLET_UIM
    foreach ( QWidget *widget, QApplication::topLevelWidgets() ) {
        if ( widget->isAncestorOf( this ) ) {
           isHidden = ( isHidden && strcmp( display_time, "always" ) );
           if ( isHidden != widget->isHidden() )
               widget->setHidden( isHidden );
           break;
        }
    }
#endif

#ifdef PLASMA_APPLET_UIM
    if ( m_layout->count() != prevCount )
#endif
        emit indicatorResized();
}

void UimStateIndicator::helper_disconnect_cb()
{
    uim_fd = -1;
    disconnect( notifier, SIGNAL( activated( int ) ), 0, 0 );
}

void UimStateIndicator::slotStdinActivated()
{
    uim_helper_read_proc( uim_fd );

    QString tmp;
    char *s;
    while ( ( s = uim_helper_get_message() ) )
    {
        const QStringList lines = QString( s ).split( '\n',
            QString::SkipEmptyParts );
        if ( lines.count() > 1
            && lines[ 1 ].startsWith( QLatin1String( "charset" ) ) )
        {
            /* get charset */
            QString charset = lines[ 1 ].split( '=',
                QString::SkipEmptyParts ) [ 1 ];

            /* convert to unicode */
            QTextCodec *codec
                = QTextCodec::codecForName( QByteArray( charset.toLatin1() ) );
            tmp = codec->toUnicode( s );
        }
        else
        {
            /* no charset */
            tmp = s;
        }

        parseHelperStr( tmp );
        free( s );
    }
}

void UimStateIndicator::slotPopupMenuAboutToShow()
{
    popupMenuShowing = true;
}

void UimStateIndicator::slotPopupMenuAboutToHide()
{
    popupMenuShowing = false;
}

void UimStateIndicator::clearButtons()
{
    while ( !buttons.isEmpty() ) {
        QHelperToolbarButton *button = buttons.takeFirst();
        m_layout->removeWidget( button );
        delete button;
    }
}

/**/
QHelperToolbarButton::QHelperToolbarButton( QWidget *parent )
    : QToolButton( parent )
{
    setAutoRaise( true );
}

QSize QHelperToolbarButton::sizeHint() const
{
    return QSize( BUTTON_SIZE, BUTTON_SIZE );
}

void QHelperToolbarButton::mousePressEvent( QMouseEvent *event )
{
#ifdef PLASMA_APPLET_UIM
    QMenu *popupMenu = menu();
    if ( event->button() == Qt::LeftButton && popupMenu ) {
        emit menuRequested( popupMenu );
        return;
    }
#endif
    QToolButton::mousePressEvent( event );
}

/**/

QHelperPopupMenu::QHelperPopupMenu( QWidget *parent )
    : QMenu( parent )
{
    msgDict.clear(); 
    connect( this, SIGNAL( triggered( QAction * ) ),
        this, SLOT( slotMenuActivated( QAction * ) ) );
}

QHelperPopupMenu::~QHelperPopupMenu()
{
    msgDict.clear();
}

QAction *QHelperPopupMenu::insertHelperItem( const QString &indicationIdStr,
                                        const QString &menulabelStr,
                                        const QString &menutooltipStr,
                                        const QString &menucommandStr )
{
    QAction *action;
    uim_bool isDarkBg =
	    uim_scm_symbol_value_bool("toolbar-icon-for-dark-background?");
    const QString append = isDarkBg ? "_dark_background" : "";
    QString fileName = ICONDIR + '/' + indicationIdStr + append + ".png";
    if ( isDarkBg && !QFile::exists( fileName ) )
    {
        fileName = ICONDIR + '/' + indicationIdStr + ".png";
    }
    QPixmap icon = QPixmap ( fileName );

    if (!icon.isNull()) {
        QPixmap scaledIcon = icon.scaled( ICON_SIZE, ICON_SIZE,
                Qt::IgnoreAspectRatio, Qt::SmoothTransformation );
        action = addAction( scaledIcon, menulabelStr );
    } else {
        action = addAction( menulabelStr );
    }

    action->setCheckable( true );
    action->setWhatsThis( menutooltipStr );
    msgDict.insert( action, menucommandStr );

    return action;
}

void QHelperPopupMenu::slotMenuActivated( QAction *action )
{
    QString msg = msgDict.find( action ).value();
    msg.prepend( "prop_activate\n" );
    msg.append( "\n" );
    uim_helper_send_message( uim_fd, msg.toLatin1().constData() );
}
