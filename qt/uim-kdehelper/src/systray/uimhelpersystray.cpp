/*

 Copyright (c) 2003,2004 uim Project http://uim.freedesktop.org/

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
#include "uimhelpersystray.h"

#include <qapplication.h>
#include <kaboutdata.h>
#include <kcmdlineargs.h>
#include <kglobal.h>


UimHelperSysApp::UimHelperSysApp()
        : mSystray( new UimHelperSystray( 0L, "UimHelperSystray" ) )
{
    mSystray->show();
}

/**/

UimHelperSystray::UimHelperSystray( QWidget *parent, const char *name )
        : KSystemTray( parent, name )

{
    QUimHelperToolbar * qht = new QUimHelperToolbar( this );
    qht->resize( 22, 22 );
    qht->show();

    connect( this, SIGNAL( quitSelected() ), kapp, SLOT( quit() ) );
    show();
}

void UimHelperSystray::contextMenuAboutToShow( KPopupMenu* menu )
{
}

/**/
static const char* uimhelpersystrayVersion = "0.0.1";
static const KCmdLineOptions options[] =
    {
        { "login", I18N_NOOP( "Application is being auto-started at KDE session start" ), 0L },
        KCmdLineLastOption
    };


int main( int argc, char* argv[] )
{
    KAboutData aboutData( "uimhelper", I18N_NOOP( "Uim Helper" ), uimhelpersystrayVersion,
                          I18N_NOOP( "Uim Helper System Tray App" ), KAboutData::License_GPL,
                          "(c) 2004 Kazuki Ohta", 0L, "" );

    KCmdLineArgs::init( argc, argv, &aboutData );
    KCmdLineArgs::addCmdLineOptions( options );
    KApplication::addCmdLineOptions();

    UimHelperSysApp a;
    return a.exec();
}
