import QtQuick 2.0
import QtQuick.Layouts 1.3
import org.kde.plasma.core 2.0 as PlasmaCore
import org.kde.plasma.components 3.0 as PlasmaComponents
import org.kde.private.uim 1.0

Row {
    Layout.minimumWidth: childrenRect.width

    Repeater {
        id: repeater
//        Layout.minimumWidth: children.width

        PlasmaComponents.Label {
            text: modelData
            fontSizeMode: "VerticalFit"
            font.pixelSize: parent.height

            height: parent.height
            width: Math.max(paintedWidth, parent.height)

            horizontalAlignment: "AlignHCenter"
        }

        resources:  UimSocket {
            onMessageReceived: {
                const [msgType, charset, ...contents] = msg.split('\n');

                // We only care about these, since this is just a status widget
                if (msgType !== 'prop_list_update') {
                    return;
                }

                const props = contents.map(propInfo => propInfo.split(/\s+/));
                const status = props
                    .filter(([propType]) => propType === 'branch')
                    .map(([
                              type,
                              name,
                              value,
                              longValue,
                              comment
                          ]) => value);

                repeater.model = status;
            }
        }
    }
}

