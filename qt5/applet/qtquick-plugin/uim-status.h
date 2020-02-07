#ifndef MYQUICKITEM_H
#define MYQUICKITEM_H

#include <QtQuick/QQuickPaintedItem>
#include <QColor>
#include <QString>

/**
 * @brief The MyQuickItem class. Simple QQuickItem plugin example;
 */
class UimStatus: public QQuickItem {
    Q_OBJECT
    Q_PROPERTY(QString text READ text NOTIFY textChanged)
public:
    UimStatus(QQuickItem* parent = nullptr);

    QString text() const;

signals:

    void textChanged();

private:
};

#endif // MYQUICKITEM_H
