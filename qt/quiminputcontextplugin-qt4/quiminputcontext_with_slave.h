/*
 Copyright (C) 2004 Kazuki Ohta <mover@hct.zaq.ne.jp>
*/

#ifndef _QUIMINPUT_CONTEXT_WITH_SLAVE_H_
#define _QUIMINPUT_CONTEXT_WITH_SLAVE_H_

#include "quiminputcontext.h"

// This class is for dealing with Dead/Multi key composing.
// Have QSimpleInputContext as slave and forward event to the
// slave when isComposing==false.

class QUimInputContextWithSlave : public QUimInputContext
{
    Q_OBJECT
public:
    QUimInputContextWithSlave( const char *imname = 0, const char *lang = 0 );
    ~QUimInputContextWithSlave();

    virtual void setFocus();
    virtual void unsetFocus();

#if defined(Q_WS_X11)
    virtual void setFocusWidget( QWidget *w );
    virtual void setHolderWidget( QWidget *w );
#endif

    virtual bool filterEvent( QEvent *event );

signals:
    void imEventGenerated( QWidget *, QIMEvent * );

protected slots:
    virtual void destroyInputContext();

protected:
    QInputContext *slave;
};

#endif /* Not def: _QUIMINPUT_CONTEXT_WITH_SLAVE_H_ */
