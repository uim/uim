#include <qapplication.h>
#include <qlineedit.h>
#include <qtextedit.h>
#include <qsplitter.h>
#include <qvbox.h>
#include <qlabel.h>

int main(int argc, char *argv[])
{
    QApplication app(argc, argv);

    QVBox page;

    QLabel *l = new QLabel("Edit Test", &page);
    l->setAlignment(Qt::AlignCenter);

    new QLabel(&page);

    QVBox *linev = new QVBox(&page);
    new QLabel("lineedit", linev);
    new QLineEdit(linev);

    QVBox *textv = new QVBox(&page);
    new QLabel("textedit", textv);
    new QTextEdit(textv);

    app.setMainWidget(&page);
    page.show();

    return app.exec();
}
