/*
Copyright (C) 2004 Kazuki Ohta <mover@hct.zaq.ne.jp>
*/
#ifndef _UIM_HELPER_MANAGER_H_
#define _UIM_HELPER_MANAGER_H_

#include <qobject.h>

class QString;
class QSocketNotifier;

class QUimHelperManager : public QObject
{
    Q_OBJECT

public:
    QUimHelperManager( QObject * parent = 0 );
    ~QUimHelperManager();

    void checkHelperConnection();
    void parseHelperStr( const QString &str );
    void parseHelperStrImChange( const QString &str );

    void sendImList();

    static void helper_disconnect_cb();
    static void update_prop_list_cb( void *ptr, const char *str );
    static void update_prop_label_cb( void *ptr, const char *str );

public slots:
    void slotStdinActivated( int );
};

#endif /* Not def: _UIM_HELPER_MANAGER_H_ */
