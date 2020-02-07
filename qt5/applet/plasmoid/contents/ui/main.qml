import QtQuick 2.0
import org.kde.plasma.plasmoid 2.0
import org.kde.plasma.core 2.0 as PlasmaCore
import org.kde.plasma.components 3.0 as PlasmaComponents
import org.kde.private.uim 1.0

PlasmaComponents.Label {
    id: mainLabel

    width: parent.width
    height: parent.width
    UimSocket {
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

            mainLabel.text = status.join(' ');
//            mainLabel.text = msg;
        }
    }
}

