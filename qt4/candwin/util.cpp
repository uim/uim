/*

 Copyright (c) 2012-2013 uim Project https://github.com/uim/uim

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
#include "util.h"

#include <unistd.h>

#include <QtCore/QStringList>

#include <uim/uim.h>
#include <uim/uim-helper.h>

QString get_messages(int fd)
{
    char buf[4096];
    QString message;
    while (uim_helper_fd_readable(fd) > 0) {
        int n = read(fd, buf, 4096 - 1);
        if (n == 0) {
            ::close(fd);
            ::exit(0);
        }
        if (n == -1)
            return message;
        buf[n] = '\0';
        message += QString::fromUtf8(buf);
    }
    return message;
}

QList<QStringList> parse_messages(const QString &message)
{
    QStringList messageList = message.split("\f\f", QString::SkipEmptyParts);
    QList<QStringList> result;
    for (int i = 0, j = messageList.count(); i < j; i++)
        result.append(messageList[i].split('\f', QString::SkipEmptyParts));
    return result;
}
