#include "uim-pref-qt.h"

#include <kuniqueapplication.h>
#include <klocale.h>
#include <kcmdlineargs.h>
#include <kaboutdata.h>
#include <kcmoduleinfo.h>

UimPrefDialog::UimPrefDialog(QWidget *parent, const char *name )
//    : KCMultiDialog(KDialogBase::TreeList, i18n("Uim Preference Dialog"), parent)
    : KCMultiDialog(parent, name)
{
    /*
    KCModuleInfo moduleinfo("uimconfig");
    addModule(moduleinfo, "uim-pref-qt");

    KCModuleInfo moduleinfo("uimqtconfig");
    addModule(moduleinfo, "uim-pref-qt");
    */

    addModule("uimconfig");
    addModule("uimqtconfig");
}

UimPrefDialog::~UimPrefDialog()
{
    removeAllModules();    
}

int main( int argc, char **argv )
{
     KAboutData aboutData("uim-pref-qt",
                          I18N_NOOP("uim-pref-qt"),
                          "0.1.0",
                          "the dialog to customize Uim");     
     KCmdLineArgs::init(argc, argv, &aboutData);
     KUniqueApplication::addCmdLineOptions();

     KUniqueApplication a;
     
     UimPrefDialog *dlg = new UimPrefDialog();
     a.setMainWidget( dlg );
     KGlobal::setActiveInstance( &a );     
     dlg->show();

     return a.exec();
}

void UimPrefDialog::slotApply()
{
    KCMultiDialog::slotApply();    
}

void UimPrefDialog::slotOk()
{
    slotApply();
    KCMultiDialog::slotOk();
}
