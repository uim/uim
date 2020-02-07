#include "uim-status.h"

#include <QPen>
#include <QPainter>

UimStatus::UimStatus(QQuickItem* parent)
    : QQuickItem(parent) {

}

QString UimStatus::text() const {
    return "my super secret text";
}
