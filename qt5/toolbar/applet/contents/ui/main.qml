import QtQuick 2.0
import org.kde.plasma.plasmoid 2.0
import org.kde.plasma.core 2.0 as PlasmaCore

// Item - the most basic plasmoid component, an empty container.
Item {

        // IconItem - a simple item to display an icon
	PlasmaCore.IconItem {
		
                // source - the icon to be displayed
                source: "face-smile"

                // height & width set to equal the size of the parent item (the empty "Item" above)
		width: parent.width
    	        height: parent.width
	}
}
