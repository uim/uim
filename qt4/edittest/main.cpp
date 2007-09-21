#include <qapplication.h>
#include <qlineedit.h>
#include <qtextedit.h>
#include <qsplitter.h>
#include <qvboxwidget.h>
#include <qlabel.h>

int main(int argc, char *argv[])
{
    QApplication app(argc, argv);
    QSplitter page;

    QVBoxWidget *linev = new QVBoxWidget(&page);
    new QLabel("lineedit", linev);
    new QLineEdit(linev);

    QVBoxWidget *textv = new QVBoxWidget(&page);
    new QLabel("textedit", textv);
    new QTextEdit(textv);

    app.setMainWidget(&page);
    page.show();

    return app.exec();
}
