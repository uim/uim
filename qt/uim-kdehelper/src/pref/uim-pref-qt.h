#ifndef _UIM_PREF_QT_H_
#define _UIM_PREF_QT_H_

#include <qdialog.h>
#include <qmap.h>
#include <qlistview.h>
#include <qwidgetstack.h>
#include <qvbox.h>

#include <uim/uim.h>
#include <uim/uim-custom.h>

class UimPrefDialog : public QDialog
{
    Q_OBJECT

public:
    UimPrefDialog( QWidget *parent = 0, const char *name = 0 );
    ~UimPrefDialog();

protected:
    void setupWidgets();
    void createMainWidgets();
    void createGroupWidgets();
    QWidget* createGroupWidget( const char *grpname );
    void addCustom( QVBox *vbox, const char *custom_sym );

protected slots:
    void accept();
    void reject();

private:
    QMap<QString, QWidget*>  pageWidgets;
    QString m_currentPageName;

    QListView *m_groupListView;
    QWidgetStack *m_groupWidgetStack;
};

#endif /* Not def: _UIM_PREF_QT_H_ */
