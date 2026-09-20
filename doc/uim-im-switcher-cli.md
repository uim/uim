# uim-im-switcher-cli

`uim-im-switcher-cli` switches the input method of all running uim contexts
through the uim helper message bus.


## Usage

```bash
uim-im-switcher-cli [-s|--scope SCOPE] <input-method-name>
uim-im-switcher-cli [-h|--help]
uim-im-switcher-cli [-l|--list]
```

### Options

| Option | Description |
| --- | --- |
| `-s SCOPE`, `--scope SCOPE` | Select the scope in which to change the input method. The default is `desktop`. |
| `-h`, `--help` | Display the usage information. This is also displayed when no argument is given. |
| `-l`, `--list` | Display the available input methods. |

### Scope values

The `-s`/`--scope` option accepts the following values:

| Scope | Description |
| --- | --- |
| `desktop` | Change all running uim contexts. This is the default. |
| `application` | Change input contexts in the focused application. |
| `text-area` | Change the focused text area. |

### Examples

```
$ uim-im-switcher-cli elatin
$ uim-im-switcher-cli -s application direct
$ uim-im-switcher-cli --scope application direct
$ uim-im-switcher-cli --scope text-area skk
```
