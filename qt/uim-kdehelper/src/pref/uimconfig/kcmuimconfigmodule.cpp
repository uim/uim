#include "kcmuimconfigmodule.h"

typedef KGenericFactory<KCMUimConfigModule, QWidget> KCMUimFactory;
K_EXPORT_COMPONENT_FACTORY( kcm_uimconfig, KCMUimFactory("uimconfig") )

KCMUimConfigModule::KCMUimConfigModule(QWidget *parent, const char *name, const QStringList &args )
    : KCModule(parent, name, args)
{
    
}


KCMUimConfigModule::~KCMUimConfigModule()
{
    
}

void KCMUimConfigModule::load()
{
    
}

void KCMUimConfigModule::save()
{
    
}

void KCMUimConfigModule::defaults()
{
    
}
