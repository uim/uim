/*

  Copyright (c) 2026 uim Project https://github.com/uim/uim

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

// Loads the immodule given as the first argument as Qt does and checks
// that Qt accepts it as the input context plugin for "uim".

#include <QFileInfo>
#include <QGuiApplication>
#include <QJsonArray>
#include <QJsonObject>
#include <QPluginLoader>
#include <QTextStream>

int main(int argc, char **argv)
{
    QGuiApplication app(argc, argv);
    QTextStream err(stderr);

    if (argc != 2) {
        err << "Usage: " << argv[0] << " PLUGIN\n";
        return 2;
    }

    // QPluginLoader searches the library paths of Qt for a relative path.
    QPluginLoader loader(
        QFileInfo(QString::fromLocal8Bit(argv[1])).absoluteFilePath());
    const QJsonObject metaData = loader.metaData();
    const QString iid = metaData.value("IID").toString();
    if (!iid.startsWith(
            "org.qt-project.Qt.QPlatformInputContextFactoryInterface")) {
        err << "Not an input context plugin: IID: <" << iid << ">\n";
        return 1;
    }
    const QJsonArray keys =
        metaData.value("MetaData").toObject().value("Keys").toArray();
    if (!keys.contains("uim")) {
        err << "No \"uim\" key in the metadata\n";
        return 1;
    }
    // This also resolves all the symbols and initializes uim.
    if (!loader.instance()) {
        err << "Failed to load: " << loader.errorString() << "\n";
        return 1;
    }
    return 0;
}
