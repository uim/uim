#include "kcmuimqtconfigmodule.h"

typedef KGenericFactory<KCMUimQtConfigModule, QWidget> KCMUimQtFactory;
K_EXPORT_COMPONENT_FACTORY( kcm_uimqtconfig, KCMUimQtFactory("uimqtconfig") )

KCMUimQtConfigModule::KCMUimQtConfigModule(QWidget *parent, const char *name, const QStringList &args )
    : KCModule(parent, name, args)
{
    
}


KCMUimQtConfigModule::~KCMUimQtConfigModule()
{
    
}

void KCMUimQtConfigModule::load()
{
    
}

void KCMUimQtConfigModule::save()
{
    
}

void KCMUimQtConfigModule::defaults()
{
    
}
