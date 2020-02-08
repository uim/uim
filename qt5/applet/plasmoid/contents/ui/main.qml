import QtQuick 2.0
import QtQuick.Layouts 1.3
import org.kde.plasma.core 2.0 as PlasmaCore
import org.kde.plasma.components 3.0 as PlasmaComponents
import org.kde.plasma.plasmoid 2.0
import org.kde.private.uim 1.0

Item {
    property var dataModel: [{
        value: '?',
        title: '???',
        comment: 'Unable to connect to UIM'
    }]

    id: root

    Plasmoid.fullRepresentation: Plasmoid.compactRepresentation
    Plasmoid.compactRepresentation: Row {

        Layout.minimumWidth: childrenRect.width

        Repeater {
            id: repeater
            model: root.dataModel

            PlasmaComponents.Label {
                text: modelData.value
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
                              title,
                              comment
                          ]) => ({
                                     value,
                                     title,
                                     comment
                                 })
                         );

                    root.dataModel = status;
                }
            }
        }
    }

    Plasmoid.toolTipItem: ColumnLayout {
        Repeater {
            model: root.dataModel
            PlasmaComponents.Label {
                text: `<b>${modelData.title}</b><br/>${modelData.comment}`
                wrapMode: "WordWrap"
            }
        }
    }
}
