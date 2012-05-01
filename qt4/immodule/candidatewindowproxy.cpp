/*

  copyright (c) 2012 uim Project http://code.google.com/p/uim/

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
#include "candidatewindowproxy.h"

#include <QtCore/QPoint>
#include <QtCore/QProcess>

CandidateWindowProxy::CandidateWindowProxy()
{
    process = new QProcess;
    initializeProcess();
    connect(process, SIGNAL(readyReadStandardOutput()),
        this, SLOT(slotReadyStandardOutput()));
}

CandidateWindowProxy::~CandidateWindowProxy()
{
    process->close();
}

void CandidateWindowProxy::deactivateCandwin()
{
    execute("deactivate");
}

void CandidateWindowProxy::clearCandidates()
{
}

void CandidateWindowProxy::popup()
{
    execute("show");
}

void CandidateWindowProxy::hide()
{
    execute("hide");
}

bool CandidateWindowProxy::isVisible()
{
    return true;
}

void CandidateWindowProxy::layoutWindow(const QPoint &point, const QRect &rect)
{
    Q_UNUSED(rect)
    execute("move\f" + QString::number(point.x()) + "\f"
        + QString::number(point.y()));
}

void CandidateWindowProxy::candidateActivate(int nr, int displayLimit)
{
    Q_UNUSED(nr)
    execute("activate\fcharset=UTF-8\fdisplay_limit="
        + QString::number(displayLimit)
        + "\fhead1\acand1\aannot1\fhead2\acand2\aannot2\fhead3\acand3\aannot3");
}

#ifdef UIM_QT_USE_DELAY
void candidateActivateWithDelay(int delay)
{
    Q_UNUSED(delay)
}
#endif /* !UIM_QT_USE_DELAY */

void CandidateWindowProxy::candidateSelect(int index)
{
    execute("set_nr_candidates\f" + QString::number(index) + "\f20");
}

void CandidateWindowProxy::candidateShiftPage(bool forward)
{
    Q_UNUSED(forward)
    execute("show_page\f0");
}

void CandidateWindowProxy::initializeProcess()
{
    if (process->state() != QProcess::NotRunning) {
        return;
    }
    process->close();
    process->start("/usr/libexec/uim-candwin-qt4", QStringList());
    process->waitForStarted();
}

void CandidateWindowProxy::execute(const QString &command)
{
    initializeProcess();
    process->write(command);
}

void CandidateWindowProxy::slotReadyStandardOutput()
{
    qDebug("%s", process->readAllStandardOutput().constData());
}
