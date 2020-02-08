#ifndef MYQUICKITEM_H
#define MYQUICKITEM_H

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
    void messageReceived(const QString &msg);

private:
    QSocketNotifier m_notifier;

private slots:
    void onSocketActivated(int socket);

    // Not really a slot, but oh well.
    static void onSocketDisconnected();
};

#endif // MYQUICKITEM_H
