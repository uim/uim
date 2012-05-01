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

#include <uim-scm.h>

#include "quiminputcontext.h"
#include "util.h"

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
    execute("deactivate_candwin");
}

void CandidateWindowProxy::clearCandidates()
{
    execute("clear_candidates");
}

void CandidateWindowProxy::popup()
{
    execute("popup");
}

void CandidateWindowProxy::hide()
{
    execute("hide");
}

bool CandidateWindowProxy::isVisible()
{
    return false;
}

void CandidateWindowProxy::layoutWindow(int x, int y, int height)
{
    execute("layout_window\f" + QString::number(x) + "\f"
        + QString::number(y) + "\f" + QString::number(height));
}

void CandidateWindowProxy::candidateActivate(int nr, int displayLimit)
{
    execute("candidate_activate\f" + QString::number(nr) + "\f"
        + QString::number(displayLimit));
}

#ifdef UIM_QT_USE_DELAY
void CandidateWindowProxy::candidateActivateWithDelay(int delay)
{
    execute("candidate_activate_with_delay\f" + QString::number(delay));
}
#endif /* !UIM_QT_USE_DELAY */

void CandidateWindowProxy::candidateSelect(int index)
{
    execute("candidate_select\f" + QString::number(index));
}

void CandidateWindowProxy::candidateShiftPage(bool forward)
{
    execute("candidate_shift_page\f" + QString::number(forward));
}

// -v -> vertical
// -h -> horizontal
// -t -> table
QString CandidateWindowProxy::candidateWindowStyle()
{
    QString windowStyle;
    // uim-candwin-prog is deprecated
    char *candwinprog = uim_scm_symbol_value_str("uim-candwin-prog");
    if (candwinprog) {
        if (!strncmp(candwinprog, "uim-candwin-tbl", 15))
            windowStyle = "-t";
        else if (!strncmp(candwinprog, "uim-candwin-horizontal", 22))
            windowStyle = "-h";
    } else {
        char *style = uim_scm_symbol_value_str("candidate-window-style");
        if (style) {
            if (!strcmp(style, "table"))
                windowStyle = "-t";
            else if (!strcmp(style, "horizontal"))
                windowStyle = "-h";
        }
        free(style);
    }
    free(candwinprog);
    
    if (windowStyle.isEmpty())
        return "-v";
    return windowStyle;
}

void CandidateWindowProxy::initializeProcess()
{
    if (process->state() != QProcess::NotRunning) {
        return;
    }
    process->close();
    QString style = candidateWindowStyle();
    process->start("/usr/libexec/uim-candwin-qt4", QStringList() << style);
    process->waitForStarted();
}

void CandidateWindowProxy::execute(const QString &command)
{
    initializeProcess();
    process->write(command + "\f\f");
}

void CandidateWindowProxy::slotReadyStandardOutput()
{
    QByteArray output = process->readAllStandardOutput();
    qDebug("%s", output.constData());
    QList<QStringList> messageList = parse_messages(QString(output));
    for (int i = 0, j = messageList.count(); i < j; i++) {
        QStringList message = messageList[i];
        QString command = message[0];
        if (command == "set_candwin_active") {
            ic->setCandwinActive();
        } else if (command == "set_candidate_index") {
            int candidateIndex = message[1].toInt();
            if (ic && ic->uimContext() && candidateIndex != -1)
                uim_set_candidate_index(ic->uimContext(), candidateIndex);
        } else if (command == "delay_activating") {
            int nr = message[1].toInt();
            int display_limit = message[2].toInt();
            int selected_index = message[3].toInt();
            uim_delay_activating(ic->uimContext(), &nr, &display_limit,
                &selected_index);
        }
    }
}
