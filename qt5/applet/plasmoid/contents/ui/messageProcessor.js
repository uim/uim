.pragma library

function getUpdatedModel(msg, oldData) {
    const [msgType, charset, ...contents] = msg.split('\n');

    // We only care about these, since this is just a status widget
    if (msgType !== 'prop_list_update') {
        return;
    }

    const data =  contents
    .map(content => content.split('\t'))
    .map(([
              type,
              name,
              value,
              title,
              comment,
              action,
              status
          ]) => ({
                     // Spice up the '-' for default input. The regular
                     // one kind of looks terrible on the ui.
                     value: value && value.replace(/\-/g, '—'),
                     title,
                     comment,
                     active: status === '*'
                 })
         )
    .filter(d => d.active);

    // Easy way of doing a deep object comparison
    if (JSON.stringify(data) === JSON.stringify(oldData)) {
        return;
    }

    return data;
}
