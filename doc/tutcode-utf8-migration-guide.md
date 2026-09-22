# uim-tutcode UTF-8 Migration Guide

## Overview

The internal string processing of `uim-tutcode` changes from EUC-JP to UTF-8,
allowing dictionaries to be used with UTF-8. This guide describes the
required migration steps.

## Dictionary encoding settings

The following settings specify the encoding of mazegaki and Bushu dictionary files.
The default is EUC-JP, so no setting changes are required when using EUC-JP.
Change the settings when using UTF-8.

| Variable | Target | Default |
| --- | --- | --- |
| `tutcode-dic-file-encoding` | System mazegaki dictionary | `euc-jp` |
| `tutcode-personal-dic-encoding` | Personal mazegaki dictionary | `euc-jp` |
| `tutcode-bushu-index2-encoding` | `bushu.index2` | `euc-jp` |
| `tutcode-bushu-expand-encoding` | `bushu.expand` | `euc-jp` |
| `tutcode-bushu-help-encoding` | `bushu.help` | `euc-jp` |

### Converting the personal EUC-JP dictionary to UTF-8

When using a UTF-8 system mazegaki dictionary, change the personal mazegaki
dictionary used for learning to UTF-8 as well.
If the personal dictionary remains in EUC-JP, learning a candidate that
contains a character not available in EUC-JP causes the entire dictionary
save to fail, so the learned data cannot be saved.
The following describes how to convert the personal mazegaki dictionary from EUC-JP to UTF-8.

```sh
cp ~/.mazegaki.dic ~/.mazegaki.dic.bak && \
iconv -f EUC-JIS-2004 -t UTF-8 ~/.mazegaki.dic > ~/.mazegaki.dic.tmp && \
chmod 600 ~/.mazegaki.dic.tmp && mv ~/.mazegaki.dic.tmp ~/.mazegaki.dic
```

If conversion with `EUC-JIS-2004` fails, try `EUC-JISX0213` and then `EUC-JP`.
If conversion succeeds, compare the contents of `~/.mazegaki.dic.bak` and
`~/.mazegaki.dic` to verify that the conversion was performed correctly.

Change the corresponding dictionary encoding setting,
`tutcode-personal-dic-encoding`, to `utf-8`, and restart uim.

## Encoding of code table files

The file specified by `tutcode-rule-filename` is Scheme source loaded by
uim-tutcode. Save a custom code table as UTF-8.

```scheme
;; ~/.uim
(define tutcode-rule-filename "/path/to/custom-rule.scm")
```

## Encoding of user configuration files

If a user configuration file such as `~/.uim` contains `uim-tutcode` settings
with EUC-JP characters, convert it to UTF-8. For example, the following
settings may contain Japanese text:

- `tutcode-postfix-mazegaki-terminate-char-list`
- `tutcode-auto-help-cand-str-list`
- `tutcode-postfix-katakana-char-list`
- `tutcode-postfix-kanji2seq-delimiter-char-list`
- `tutcode-rule-set-sequences!`
- `tutcode-stroke-help-top-page-alist`

If the other settings do not contain EUC-JP characters, convert the entire
file to UTF-8. If EUC-JP settings must remain in the same file, leave the
original file in EUC-JP, create a new UTF-8 file, and load it with `load`.
For example, add the following to load `.uim-tutcode-utf8`:

```scheme
;; ~/.uim
(load (string-append (getenv "HOME") "/.uim-tutcode-utf8"))
```
