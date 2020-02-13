#pragma once

#include <QtQuick/QQuickPaintedItem>
#include <QColor>
#include <QSocketNotifier>
#include <QString>

/**
 * @brief UIM Socket.
 * Creates a socket to UIM and emits a signal on message.
 */
class UimSocket: public QQuickItem {
    Q_OBJECT
public:
    UimSocket(QQuickItem* parent = nullptr);
    ~UimSocket();

signals:
    /**
     * @brief Some data arrived on the socket from uim.
     * @param msg Contents of the message.
     */
    void messageReceived(const QString &msg);

public slots:
    /**
     * @brief Write data to uim socket.
     * @param msg Text to write.
     */
    void sendMessage(const QString &msg);

private slots:
    void onSocketActivated(int socket);

    // Not really a slot, but oh well.
    static void onSocketDisconnected();

private:
    QSocketNotifier m_notifier;
};
