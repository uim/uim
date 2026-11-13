# Karukan

[Karukan](https://github.com/togatoga/karukan) is a Japanese input
method with neural kana-kanji conversion. uim uses it through
`karukan-imserver`, the conversion server of Karukan that speaks
JSON-RPC over its standard input and output. uim needs no plugin
for it, so the `karukan` input method is always built.

## Setup

Install `karukan-imserver`. Karukan doesn't distribute it as a
package, so build it from the source of Karukan:

```console
$ git clone https://github.com/togatoga/karukan.git
$ cd karukan
$ cargo build --release -p karukan-im --bin karukan-imserver
$ install target/release/karukan-imserver ~/.local/bin/
```

Put `karukan-imserver` into `PATH`, or set the path to "karukan-imserver
command" (`karukan-server-command`) in uim-pref. Karukan can't be
selected without it: the default input method is used instead.

Install the system dictionary as the
[dictionary document of Karukan](https://github.com/togatoga/karukan/blob/main/docs/dictionary.md)
describes. Karukan works without it but its vocabulary is limited.

`karukan-imserver` downloads the conversion models from Hugging Face
when it starts for the first time. Conversion uses the dictionary
until they're loaded. To download them in advance:

```console
$ karukan-imserver --prefetch-models
```

Karukan reads its configuration from `~/.config/karukan-im/config.toml`.
See the
[configuration document of Karukan](https://github.com/togatoga/karukan/blob/main/docs/configuration.md).

## Usage

Select "Karukan" as the input method. The on key (`karukan-on-key`)
and the off key (`karukan-off-key`) turn it on and off. They're
"Zenkaku_Hankaku" and Shift+Space by default. The off key doesn't
turn it off while composing because Karukan uses Shift+Space then.

The other keys are the ones of Karukan. See the
[key bindings of Karukan](https://github.com/togatoga/karukan/blob/main/docs/key-bindings.md).
For example, Space converts and selects the next candidate, Ctrl+1
to Ctrl+9 select a candidate on the page, and Return commits the
selected one.

The Karukan key bindings of uim add keys to them, such as Ctrl+G for
cancel (`karukan-cancel-key`) and Ctrl+D for delete
(`karukan-delete-key`). They're the global ones by default. A key is
sent to Karukan as the key of Karukan that has the same meaning, such
as Escape and Delete. The commit key (`karukan-commit-key`) is the
global return key, not the global commit key, because Ctrl+J has
another meaning in Karukan. The next candidate key
(`karukan-next-candidate-key`) doesn't have Space of the global one
for the same reason.

The text before the cursor is passed to Karukan for conversion when
the application provides it. Turn off "Use the text before the cursor
for conversion" (`karukan-use-surrounding-text?`) not to pass it.

## Limitations

- A process of uim starts one `karukan-imserver` when Karukan is
  turned on first. It holds one input state, which the input fields
  of the process share. What is being composed is committed when the
  field loses the focus, or when another field of the process starts
  composing.
- `karukan-imserver` uses about 700 MB of memory with the default
  dictionary and models. uim-xim and uim-wayland run one for all
  applications. uim-fep runs one for each terminal, and the GTK and Qt
  input method modules run one for each application.
- The candidate window shows only the page that Karukan shows now.
  Page Up and Page Down turn the page.
- Karukan's auxiliary text, such as the reading and the conversion
  time, isn't shown.
- Karukan's katakana and alphanumeric modes aren't shown in the
  toolbar. The toolbar shows only whether Karukan is on. Turning
  Karukan on goes back to the hiragana mode: Right Alt and Right
  Super, which leave the katakana mode of Ctrl+K, don't reach
  Karukan.
