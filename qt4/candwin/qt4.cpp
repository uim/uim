/*

 Copyright (c) 2003-2013 uim Project https://github.com/uim/uim

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
#include <config.h>

#include <clocale>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <unistd.h>

#include <QtCore/QSocketNotifier>
#include <QtCore/QStringList>
#include <QtCore/QTextCodec>
#if QT_VERSION < 0x050000
# include <QtGui/QApplication>
# include <QtGui/QDesktopWidget>
# include <QtGui/QHeaderView>
# include <QtGui/QLabel>
# include <QtGui/QTableWidget>
# include <QtGui/QVBoxLayout>
#else
# include <QtWidgets/QApplication>
# include <QtWidgets/QDesktopWidget>
# include <QtWidgets/QHeaderView>
# include <QtWidgets/QLabel>
# include <QtWidgets/QTableWidget>
# include <QtWidgets/QVBoxLayout>
#endif

#include "candidatewindow.h"
#include "candidatetablewindow.h"
#include "ximcandidatewindow.h"

#include "qtgettext.h"

class Window
{
    public:
        Window(int argc, char *argv[]);
        ~Window();

    private:
        QWidget *widget;
};

Window::Window(int argc, char *argv[])
{
    uim_init();
    if (argc > 1) {
        // vertical
        if (!strcmp(argv[1], "-v")) {
            widget = new CandidateWindow(0, true);
        // horizontal
        } else if (!strcmp(argv[1], "-h")) {
            widget = new CandidateWindow(0, false);
        // table
        } else if (!strcmp(argv[1], "-t")) {
            widget = new CandidateTableWindow(0);
        } else {
            widget = new XimCandidateWindow;
        }
    } else {
        widget = new XimCandidateWindow;
    }
}

Window::~Window()
{
    delete widget;
    uim_quit();
}

int main(int argc, char *argv[])
{
    setlocale(LC_ALL, "");
    bindtextdomain(PACKAGE, LOCALEDIR);
    textdomain(PACKAGE);
    bind_textdomain_codeset(PACKAGE, "UTF-8"); // ensure code encoding is UTF8-
    
    QApplication app(argc, argv);

    Window window(argc, argv);

    return app.exec();
}
