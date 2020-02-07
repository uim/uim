#include "uim-socket.h"

#include <uim/uim.h>
#include <uim/uim-helper.h>
#include <uim/uim-scm.h>

#include <QRegularExpression>
#include <QSocketNotifier>
#include <QTextCodec>

UimSocket::UimSocket(QQuickItem* parent) :
    QQuickItem(parent),
    m_notifier(
        uim_helper_init_client_fd(onSocketDisconnected),
        QSocketNotifier::Read
    )
{
    connect(&m_notifier, &QSocketNotifier::activated,
            this, &UimSocket::onSocketActivated);
}

void UimSocket::onSocketDisconnected() {
    // the socket shouldn't activate if it's disconnected
}

const auto charsetRegex = QRegularExpression(
            "^charset=(.+)$",
            QRegularExpression::MultilineOption
    );

void UimSocket::onSocketActivated(int fd) {
    uim_helper_read_proc(fd);

    char *s;
    while ((s = uim_helper_get_message())) {
        auto msg = QString(s);

        auto match = charsetRegex.match(msg);
        // Check if this message has a special charset
        if (match.hasMatch()) {
            auto charset = match.captured(1);
            qDebug() << "Recived message from UIM, CHARSET:" << charset;

            // Convert before sending it up
            auto codec = QTextCodec::codecForName(charset.toLatin1());
            emit messageReceived(codec->toUnicode(s));

        } else {
            // Regular message, set it up as-is.
            emit messageReceived(msg);
        }

        free(s);
    }
}

UimSocket::~UimSocket() {
    uim_helper_close_client_fd(static_cast<int>(m_notifier.socket()));
}
