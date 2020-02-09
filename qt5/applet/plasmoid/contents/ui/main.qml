import QtQuick 2.0
import QtQuick.Layouts 1.3
import org.kde.plasma.core 2.0 as PlasmaCore
import org.kde.plasma.components 3.0 as PlasmaComponents
import org.kde.plasma.plasmoid 2.0
import org.kde.plasma.extras 2.0 as PlasmaExtras
import org.kde.private.uim 1.0
import "messageProcessor.js" as MessageProcessor

Item {
    property var dataModel: [{
            "value": '?',
            "title": 'Unable to connect to UIM'
        }]

    id: root

    Plasmoid.associatedApplication: "uim-pref-qt5"
    Plasmoid.preferredRepresentation: Plasmoid.compactRepresentation
    Plasmoid.fullRepresentation: ColumnLayout {
        Repeater {
            model: root.dataModel

            Text {
                text: "a"
            }
            Text {
                text: "b"
            }

            //            ListView {
            //                model: modelData.options
            //            }
        }
    }

    Plasmoid.compactRepresentation: Item {
        id: compactRoot

        Layout.minimumWidth: contentRow.width
        Layout.maximumWidth: Layout.minimumWidth
        Layout.fillHeight: true
        Layout.fillWidth: false

        Row {
            id: contentRow

            Repeater {
                id: repeater
                model: root.dataModel

                PlasmaComponents.Label {
                    text: modelData.value
                    font.pixelSize: parent.height
                    fontSizeMode: "Fit"

                    height: compactRoot.height
                    width: Math.max(paintedWidth, parent.height)

                    horizontalAlignment: "AlignHCenter"
                }
            }
        }

        MouseArea {
            anchors.fill: parent
            onClicked: plasmoid.expanded = !plasmoid.expanded
        }
    }

    Plasmoid.toolTipItem: GridLayout {
        columns: 2

        Repeater {
            model: root.dataModel
            Repeater {
                model: [modelData.value, modelData.title]

                PlasmaComponents.Label {
                    text: modelData
                    font.bold: index === 0
                    Layout.alignment: index === 0 ? Qt.AlignHCenter : Qt.AlignLeft
                }
            }
        }
    }

    resources: UimSocket {
        onMessageReceived: root.dataModel = MessageProcessor.parseMessage(msg)
                           || root.dataModel
    }
}
