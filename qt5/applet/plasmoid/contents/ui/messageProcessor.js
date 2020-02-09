.pragma library

function parseMessage(msg) {
    const [msgType, charset, ...contents] = msg.split('\n');

    // We only care about these, since this is just a status widget
    if (msgType !== 'prop_list_update') {
        return;
    }

    const newData = [];
    for (const content of contents) {
        const [ type, name, value, title, comment ] = content.split('\t');

        if (type === 'branch') {
            newData.push({ name, value, title, options: [] });
        } else if (type === 'leaf') {
            newData[newData.length - 1].options.push({ value, title, comment })
        }
    }

    return newData;
}
