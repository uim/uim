#include <qapplication.h>
#include <qsettings.h>
#include <qlabel.h>

#include "testinputwidgets.h"


int main( int argc, char **argv )
{
    QApplication a( argc, argv );

    TestInputWidgets test;
    QSettings st;
    test.currentImmodule->setText(st.readEntry("/qt/DefaultInputMethod", "XIM"));
    a.setMainWidget( &test );
    test.move(350, 200);
    test.show();
    return a.exec();
}
