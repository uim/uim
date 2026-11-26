# Rime

`rime` is an input method that uses [Rime](https://rime.im/), an input
method engine for Chinese. Rime has schemas for many input methods
such as Pinyin, Pinyin with tones, Bopomofo, Cangjie and so on.

Rime (librime) processes keys and has the composition and the
candidates. uim only passes keys to it and shows what it has. So the
keys and the behavior are the ones of Rime, and they are set in the
settings of Rime, not in uim-pref.

## Build

`rime` is built when librime is found. Pass `-Drime=disabled` to
`meson setup`, or `--without-rime` to `configure`, to disable it.

The schemas aren't included in librime. Install the ones to use. For
example, on Debian:

```console
$ sudo apt install rime-data-luna-pinyin rime-data-terra-pinyin
```

They also need the common settings and the vocabulary of Rime,
`rime-prelude` and `rime-essay` on Debian. They are installed with
the schemas as recommended packages by default. The key bindings such
as F4 don't work without the former and the order of candidates is
wrong without the latter.

## Usage

Choose "Rime" as the input method and turn it on with the on key of
uim, Shift+Space or Zenkaku/Hankaku by default.

The default schema of Rime, Luna Pinyin (`luna_pinyin`), is used by
default:

1. Type `ni`. The candidates, 你, 擬, 尼 and so on, are shown.
2. Press Space to commit the first one, or a digit to commit the one
   of the number. Page Down shows the next candidates.

To use another schema, set its ID to "Schema" (`rime-schema`) of
uim-pref, or with `uim-custom-cli`:

```console
$ uim-custom-cli --save rime-schema terra_pinyin
```

The text fields that are used now also switch to it.

For example, Terra Pinyin (`terra_pinyin`) takes tones: `-` is the 1st
tone, `/` is the 2nd, `<` is the 3rd and `\` is the 4th. `pi\` shows
only the candidates of "pì" such as 屁.

Shift switches Rime between the Chinese mode and the ASCII mode by
default. `rime` is still on in the ASCII mode: the keys are processed
by Rime. Left Shift and Right Shift can have different actions as the
settings of Rime say. The Qt 3, TQt and Qt 4 immodules don't tell the
side, so both of them are Left Shift with them.

The input mode of the toolbar shows "A" in the ASCII mode. It can also
be used to switch the modes.

The toolbar also shows the schema that is used now with the first
character of its name such as "朙" of 朙月拼音, and it can be used to
choose another schema. The schemas are shown after `rime` is turned on
for the first time, because Rime is started then. The schema that is
chosen there is only for the text field that is used now. Use
`rime-schema` or the menu of Rime to choose the schema of the text
fields that are used later.

Rime also has its own menu to switch schemas and options such as the
ASCII mode, Ctrl+\` or F4 by default.

## Customs

| Custom | Description |
| --- | --- |
| `rime-schema` | The ID of the schema to use. The default one of Rime is used if this is empty. |
| `rime-shared-data-dir` | The directory that has the schemas. The default is the one of librime that was used on build such as `/usr/share/rime-data`. |
| `rime-user-data-dir` | The directory for the user settings and the learned data of Rime. The default is `~/.uim.d/rime`. It isn't shared with the other frontends of Rime such as ibus-rime. |
| `rime-on-key`, `rime-off-key` | The keys to turn `rime` on and off. |

Put the user settings of Rime such as `default.custom.yaml` into
`rime-user-data-dir`.

## Limitations

- Shift alone doesn't reach Rime with uim-fep and uim.el: a terminal
  and Emacs don't send a modifier key alone. Use the off key of uim or
  the menu of Rime instead.
- Rime is started when `rime` is turned on for the first time. It
  deploys the schemas then if they are changed. It takes time when the
  shared data directory doesn't have prebuilt schemas.
- The candidate window shows only the candidates of the page that Rime
  shows now.
- What is composed is committed as Rime converts it now when the focus
  is lost.
