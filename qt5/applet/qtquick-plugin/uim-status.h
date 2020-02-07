#ifndef MYQUICKITEM_H
#define MYQUICKITEM_H

#include <QtQuick/QQuickPaintedItem>
#include <QColor>

/**
 * @brief The MyQuickItem class. Simple QQuickItem plugin example;
 */
class MyQuickItem: public QQuickPaintedItem {
    Q_OBJECT
    Q_PROPERTY(QColor color READ getColor WRITE setColor NOTIFY colorChanged)
public:
    MyQuickItem(QQuickItem* parent = nullptr);

    /**
     * @brief getColor  getter for @property color
     * @return          current color
     */
    QColor getColor() const;

    /**
     * @brief setColor  setter for @property color
     * @param color     color to set
     */
    void setColor(const QColor &color);

    /**
     * @brief paint     overrided method that will paint our item on scene
     * @param painter   painter
     */
    void paint(QPainter *painter) override;

signals:
    /**
     * @brief colorChanged  signal that should be emitted when @property color changes
     */
    void colorChanged();

private:
    QColor color;
};

#endif // MYQUICKITEM_H
