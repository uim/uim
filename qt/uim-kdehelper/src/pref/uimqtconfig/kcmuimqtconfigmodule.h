#ifndef _KCM_UIM_QT_CONFIG_MODULE_H_
#define _KCM_UIM_QT_CONFIG_MODULE_H_

#include <qstringlist.h>
#include <qwidget.h>

#include <kcmodule.h>
#include <kgenericfactory.h>

class KCMUimQtConfigModule : public KCModule {
    Q_OBJECT

public:
    KCMUimQtConfigModule(QWidget *parent=0, const char *name=0, const QStringList &args=QStringList());
    ~KCMUimQtConfigModule();

    void load();
    void save();
    void defaults();
};

#endif /* Not def: _KCM_UIM_QT_CONFIG_MODULE_H_ */
