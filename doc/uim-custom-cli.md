# uim-custom-cli

`uim-custom-cli` sends a custom value to running uim processes through the
uim helper message bus.

## Usage

```bash
uim-custom-cli CUSTOM_NAME CUSTOM_VALUE
uim-custom-cli CUSTOM_NAME VALUE [VALUE ...]
uim-custom-cli [-l|--list]
uim-custom-cli [-h|--help]
```

## Options

- `-l`, `--list`: List custom variables and their current values.
- `-h`, `--help`: Display usage information.

## Custom values

`CUSTOM_NAME` must be a registered custom variable name. `CUSTOM_VALUE` is
parsed by the receiving uim process as a Scheme expression.
The custom name and value are checked before the message is sent.

### Boolean values

For boolean custom values, `t` and `true` are accepted as `#t`, and `f` and
`false` are accepted as `#f`. The Scheme literals `#t` and `#f` must be
quoted for the shell, while the following forms can be written directly:

```bash
$ uim-custom-cli 'custom-activate-default-im-name?' true
$ uim-custom-cli 'custom-activate-default-im-name?' t
$ uim-custom-cli 'custom-activate-default-im-name?' '#t'
$ uim-custom-cli 'custom-activate-default-im-name?' false
$ uim-custom-cli 'custom-activate-default-im-name?' f
$ uim-custom-cli 'custom-activate-default-im-name?' '#f'
```

### Integer values

Integer custom values must be valid decimal integers within the custom's
declared minimum and maximum range:

```bash
$ uim-custom-cli anthy-nr-candidate-max 10
```

### Choice values

For choice custom values, a bare symbol is automatically quoted. These two
commands are equivalent:

```bash
$ uim-custom-cli custom-preserved-default-im-name anthy
$ uim-custom-cli custom-preserved-default-im-name "'anthy"
```

The value must be one of the custom's declared candidates.

### String and pathname values

For string and pathname custom values, a bare value is automatically enclosed
in double quotes. Double quotes and backslashes inside a bare value are
escaped:

These two commands are equivalent:

```bash
$ uim-custom-cli anthy-segment-separator abc
$ uim-custom-cli anthy-segment-separator '"abc"'
```

If a value already starts with a double quote, it must be a valid Scheme
string literal. An unescaped double quote in the middle of such a value is
rejected.

### Ordered-list values

For ordered-list custom values, each item can also be passed as a separate
argument:

The following commands are equivalent for a single item:

```bash
$ uim-custom-cli enabled-im-list anthy
$ uim-custom-cli enabled-im-list "(anthy)"
```

Multiple items can also be passed as separate arguments. The following
commands are equivalent:

```bash
$ uim-custom-cli skk-kana-input-method-actions action_skk_roma action_skk_azik
$ uim-custom-cli skk-kana-input-method-actions "(action_skk_roma action_skk_azik)"
```

Each item must be one of the custom's declared items.

### Listing custom values

To list custom variables and their current values:

```bash
$ uim-custom-cli --list
$ uim-custom-cli -l
```

The `-l`/`--list` option includes each variable's type, active status, current
value, label, description, and type-specific ranges, candidates, items, or
columns, including pathname file types.

## Important notes

The values shown by `-l`/`--list` are obtained from the CLI's own uim context;
the option does not query the state of other running uim processes. Therefore,
changing a value with one invocation of `uim-custom-cli` is not reflected in a
later invocation of `uim-custom-cli -l`.

The value is sent to running uim processes connected to the helper server.
Changes are not saved for future processes. Use `~/.uim` or `uim-pref-gtk3` for
persistent configuration.
