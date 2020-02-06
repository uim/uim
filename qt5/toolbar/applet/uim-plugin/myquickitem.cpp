#include "myquickitem.h"

#include <QPen>
#include <QPainter>

MyQuickItem::MyQuickItem(QQuickItem* parent)
    : QQuickPaintedItem(parent) {

}

QColor MyQuickItem::getColor() const {
    return color;
}

void MyQuickItem::setColor(const QColor& color) {
    // Look at chapter 3 http://doc.qt.io/qt-5/qtqml-tutorials-extending-qml-example.html
    if (color != this->color) {
        this->color = color;
        update();
        emit colorChanged();
    }
}

void MyQuickItem::paint(QPainter* painter) {
    // Drawing simple filled rect
    QPen pen(color, 2);
    painter->setPen(pen);
    painter->fillRect(QRectF(0, 0, width(), height()), color);
}
