#ifndef _UIM_PREF_QT_H_
#define _UIM_PREF_QT_H_

#include <kcmultidialog.h>

class UimPrefDialog : public KCMultiDialog
{
    Q_OBJECT

public:
    UimPrefDialog( QWidget *parent = 0, const char *name = 0 );
    ~UimPrefDialog();

public slots:
    void slotApply();
    void slotOk();
};

#endif /* Not def: _UIM_PREF_QT_H_ */
