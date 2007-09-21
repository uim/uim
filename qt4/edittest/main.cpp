#include <qapplication.h>
#include <qlineedit.h>
#include <qtextedit.h>
#include <qsplitter.h>
#include <Q3VBox>
#include <qlabel.h>

int main(int argc, char *argv[])
{
    QApplication app(argc, argv);
    QSplitter page;

    Q3VBox *linev = new Q3VBox(&page);
    new QLabel("lineedit", linev);
    new QLineEdit(linev);

    Q3VBox *textv = new Q3VBox(&page);
    new QLabel("textedit", textv);
    new QTextEdit(textv);

    app.setMainWidget(&page);
    page.show();

    return app.exec();
}
