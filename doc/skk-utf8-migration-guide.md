# uim-skk UTF-8 Migration Guide

## Overview

The internal string processing of `uim-skk` changes from EUC-JP to UTF-8,
allowing dictionaries and `skkserv` to be used with UTF-8. This guide
describes the required migration steps.

## Dictionary and `skkserv` encoding settings

The following settings specify the encoding of external data such as
dictionaries and `skkserv`.
The default is EUC-JP, so no setting changes are required when using EUC-JP.
Change the settings when using UTF-8.

| Variable | Target | Default |
| --- | --- | --- |
| `skk-skkserv-encoding` | Communication with `skkserv` | `euc-jp` |
| `skk-dic-file-encoding` | System dictionary | `euc-jp` |
| `skk-personal-dic-encoding` | Personal dictionary | `euc-jp` |
| `skk-uim-personal-dic-encoding` | uim personal dictionary | `euc-jp` |

### Converting the uim personal EUC-JP dictionary to UTF-8

When using the system dictionary or `skkserv` with UTF-8, change the
uim personal dictionary used for learning to UTF-8 as well.
If the uim personal dictionary remains in EUC-JP, learning a candidate that
contains a character not available in EUC-JP causes the entire dictionary
save to fail, so the learned data cannot be saved.
The following describes how to convert the uim personal dictionary from EUC-JP to UTF-8.

```sh
cp ~/.skk-uim-jisyo ~/.skk-uim-jisyo.bak && \
iconv -f EUC-JIS-2004 -t UTF-8 ~/.skk-uim-jisyo > ~/.skk-uim-jisyo.tmp && \
chmod 600 ~/.skk-uim-jisyo.tmp && mv ~/.skk-uim-jisyo.tmp ~/.skk-uim-jisyo
```

If conversion with `EUC-JIS-2004` fails, try `EUC-JISX0213` and then `EUC-JP`.
If conversion succeeds, compare the contents of `~/.skk-uim-jisyo.bak` and
`~/.skk-uim-jisyo` to verify that the conversion was performed correctly.

Change the corresponding dictionary encoding setting,
`skk-uim-personal-dic-encoding`, to `utf-8`, and restart uim.
