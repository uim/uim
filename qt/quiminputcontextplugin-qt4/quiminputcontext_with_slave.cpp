/*
 Copyright (C) 2004 Kazuki Ohta <mover@hct.zaq.ne.jp>
*/
#include "quiminputcontext_with_slave.h"

#include <qinputcontextfactory.h>

QUimInputContextWithSlave::QUimInputContextWithSlave( const char *imname, const char *lang )
        : QUimInputContext( imname, lang )
{
    slave = QInputContextFactory::create( "simple", NULL );
    if ( slave )
    {
        slave->setParent( this );

        QObject::connect( slave, SIGNAL( imEventGenerated( QObject *, QIMEvent * ) ),
                          this, SIGNAL( imEventGenerated( QObject *, QIMEvent * ) ) );
        QObject::connect( slave, SIGNAL( deletionRequested() ),
                          this, SLOT( destroyInputContext() ) );
    }
}

QUimInputContextWithSlave::~QUimInputContextWithSlave()
{}

void QUimInputContextWithSlave::setFocus()
{
    QUimInputContext::setFocus();

    if ( slave )
    {
        slave->setFocus();
        slave->setFocusWidget( focusWidget() );
    }
}

void QUimInputContextWithSlave::unsetFocus()
{
    QUimInputContext::unsetFocus();

    if ( slave )
        slave->unsetFocus();
}

#if defined(Q_WS_X11)
void QUimInputContextWithSlave::setFocusWidget( QWidget *w )
{
    QUimInputContext::setFocusWidget( w );

    if ( slave )
        slave->setFocusWidget( w );
}

void QUimInputContextWithSlave::setHolderWidget( QWidget *w )
{
    QUimInputContext::setHolderWidget( w );

    if ( slave )
        slave->setHolderWidget( w );
}
#endif

bool QUimInputContextWithSlave::filterEvent( QEvent *event )
{
    // when isComposing==false, event is forwarded to slave ic
    if ( ! isComposing() && slave && slave->filterEvent( event ) )
        return true;

    // else, event is dealt with uim-qt
    return QUimInputContext::filterEvent( event );
}

void QUimInputContextWithSlave::destroyInputContext()
{
    if ( slave )
    {
        // slave->reset() may not properly work in the case, so we
        // manually resets the composing state of text widget
        if ( slave->focusWidget() )
        {
            emit imEventGenerated( slave->focusWidget(), new QIMEvent( QEvent::IMEnd, QString::null, -1 ) );
        }
        slave->deleteLater();
        slave = 0;
    }
}
