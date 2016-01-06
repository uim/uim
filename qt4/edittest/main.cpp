/*

  Copyright (c) 2004-2005 Kazuki Ohta <mover@hct.zaq.ne.jp>
  Copyright (c) 2005-2013 uim Project https://github.com/uim/uim

  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions
  are met:

  1. Redistributions of source code must retain the above copyright
     notice, this list of conditions and the following disclaimer.
  2. Redistributions in binary form must reproduce the above copyright
     notice, this list of conditions and the following disclaimer in the
     documentation and/or other materials provided with the distribution.
  3. Neither the name of authors nor the names of its contributors
     may be used to endorse or promote products derived from this software
     without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS IS'' AND
  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE
  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
  OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
  SUCH DAMAGE.

*/

#include <QtCore/QtGlobal>
#if QT_VERSION < 0x050000
# include <QtGui/QApplication>
# include <QtGui/QLabel>
# include <QtGui/QLineEdit>
# include <QtGui/QSplitter>
# include <QtGui/QTextEdit>
# include <QtGui/QVBoxLayout>
#else
# include <QtWidgets/QApplication>
# include <QtWidgets/QLabel>
# include <QtWidgets/QLineEdit>
# include <QtWidgets/QSplitter>
# include <QtWidgets/QTextEdit>
# include <QtWidgets/QVBoxLayout>
#endif

int main(int argc, char *argv[])
{
    QApplication app(argc, argv);
    QSplitter page;

    QWidget *linev = new QWidget(&page);
    QLabel *lineLabel = new QLabel("lineedit");
    QLineEdit *lineEdit = new QLineEdit;

    QVBoxLayout *lineLayout = new QVBoxLayout;
    lineLayout->addWidget(lineLabel);
    lineLayout->addWidget(lineEdit);
    linev->setLayout(lineLayout);

    QWidget *textv = new QWidget(&page);
    QLabel *textLabel = new QLabel("textedit");
    QTextEdit *textEdit = new QTextEdit;

    QVBoxLayout *textLayout = new QVBoxLayout;
    textLayout->addWidget(textLabel);
    textLayout->addWidget(textEdit);
    textv->setLayout(textLayout);

    page.show();

    return app.exec();
}
