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
#include <qevent.h>

#include <klocale.h>
#include <kglobal.h>
#include <kdebug.h>

#include "uimhelperapplet.h"

extern "C"
{
    KPanelApplet* init( QWidget * parent, const QString & configFile )
    {
        KGlobal::locale() ->insertCatalogue( "uimhelperapplet" );
        return new UimHelperApplet( configFile, KPanelApplet::Normal,
                                    0, parent, "uimhelperdapplet" );
    }
}

UimHelperApplet::UimHelperApplet( const QString& configFile,
                                  Type type, int actions,
                                  QWidget *parent, const char *name )
        : KPanelApplet( configFile, type, actions, parent, name )
{
    uhb = new UimHelperButtons( this );
    uhb->show();
}

int UimHelperApplet::widthForHeight( int h ) const
{
    return uhb->width();
}

int UimHelperApplet::heightForWidth( int w ) const
{
    return uhb->height();
}

void UimHelperApplet::positionChange( Position p )
{}

UimHelperButtons::UimHelperButtons( QWidget *parent, const char *name )
        : QUimHelperToolbar( parent, name )
{
    if ( parent && ! parent->parent() )
    {
        setBackgroundMode( X11ParentRelative );
    }
    setBackgroundOrigin( AncestorOrigin );
}

UimHelperButtons::~UimHelperButtons()
{
}

/*
void UimHelperButtons::setPopupDirectionFromPosition(KPanelApplet::Position p)
{
    QUimHelperToolbar::PopupDirection d = QUimHelperToolbar::Up;
 
    switch (p) {
    case KPanelApplet::pLeft:
        d = QUimHelperToolbar::Right; break;
 
    case KPanelApplet::pRight:
        d = QUimHelperToolbar::Left; break;
 
    case KPanelApplet::pTop:
        d = QUimHelperToolbar::Down; break;
 
    case KPanelApplet::pBottom:
        d = QUimHelperToolbar::Up; break;
    }
 
    setPopupDirection(d);
}
*/
