#ifndef MYQUICKITEM_H
#define MYQUICKITEM_H

#include <QtQuick/QQuickPaintedItem>
#include <QColor>
#include <QSocketNotifier>
#include <QString>

/**
 * @brief The MyQuickItem class. Simple QQuickItem plugin example;
 */
class UimSocket: public QQuickItem {
    Q_OBJECT
    Q_PROPERTY(QString text READ text NOTIFY textChanged)
public:
    UimSocket(QQuickItem* parent = nullptr);

    QString text() const;

signals:

    void textChanged();
    void messageReceived(const QString &msg);

private:
    QSocketNotifier m_notifier;

    static void onSocketDisconnected();

private slots:
    void onSocketActivated(int socket);
};

#endif // MYQUICKITEM_H
