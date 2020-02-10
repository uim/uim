import QtQuick 2.0
import QtQuick.Layouts 1.3
import QtQuick.Controls 2.12
import org.kde.plasma.core 2.0 as PlasmaCore
import org.kde.plasma.components 3.0 as PlasmaComponents
import org.kde.plasma.plasmoid 2.0
import org.kde.plasma.extras 2.0 as PlasmaExtras
import com.github.uim 1.0
import "messageProcessor.js" as MessageProcessor

Item {
    property var dataModel: [{
            "value": '?',
            "title": 'Unable to connect to uim'
        }]

    id: root

    Plasmoid.associatedApplication: "uim-pref-qt5"
//    Plasmoid.preferredRepresentation: Plasmoid.compactRepresentation
    Plasmoid.fullRepresentation:  Plasmoid.compactRepresentation

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
    }

    Plasmoid.toolTipSubText:
        root.dataModel.map(e => `<b>${e.title}</b> (${e.value})<br/>${e.comment}<br/>`).join('<br/>')
    Plasmoid.toolTipTextFormat: Text.RichText
    Plasmoid.icon: '/usr/share/uim/pixmaps/uim-icon64.png' //TODO: resolve this using uim resources

    resources: UimSocket {
        id: socket
        onMessageReceived: {
            // If the method returns null, it means the new model is the same as old
            const newModel = MessageProcessor.getUpdatedModel(msg, root.dataModel);
            if (newModel) {
                // only set this if model is new, to avoid unnecessary re-paints
                root.dataModel = newModel;
            }
        }
    }
}
