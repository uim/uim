# uim-skk UTF-8対応 移行ガイド

## 概要

`uim-skk`の内部文字列処理がEUC-JPからUTF-8に変わり、辞書や`skkserv`をUTF-8で使用できるようになりました。
ここでは必要な移行手順を説明します。

## 辞書と`skkserv`の文字コード設定

以下の設定で、辞書や`skkserv`などの外部データの文字コードを指定できます。
デフォルトはEUC-JPなので、EUC-JPで使っている場合は設定変更は不要です。
UTF-8で使う場合は設定を変更してください。

| 変数 | 対象 | デフォルト |
| --- | --- | --- |
| `skk-skkserv-encoding` | `skkserv`との通信 | `euc-jp` |
| `skk-dic-file-encoding` | システム辞書 | `euc-jp` |
| `skk-personal-dic-encoding` | 通常の個人辞書 | `euc-jp` |
| `skk-uim-personal-dic-encoding` | uim専用個人辞書 | `euc-jp` |

### EUC-JPのuim専用個人辞書をUTF-8へ変換する

システム辞書や`skkserv`をUTF-8で使用する場合、学習先のuim専用個人辞書もUTF-8に変更してください。
uim専用個人辞書をEUC-JPのまま使用すると、EUC-JPにない文字を含む候補を学習した際に、辞書の保存に失敗し、学習内容を保存できません。
ここではEUC-JPのuim専用個人辞書をUTF-8へ変換する方法を説明します。

```sh
cp ~/.skk-uim-jisyo ~/.skk-uim-jisyo.bak && \
iconv -f EUC-JIS-2004 -t UTF-8 ~/.skk-uim-jisyo > ~/.skk-uim-jisyo.tmp && \
chmod 600 ~/.skk-uim-jisyo.tmp && mv ~/.skk-uim-jisyo.tmp ~/.skk-uim-jisyo
```

`EUC-JIS-2004`での変換がエラーになる場合は、`EUC-JISX0213`、`EUC-JP`の順に試してください。
変換できた場合は、~/.skk-uim-jisyo.bak と ~/.skk-uim-jisyo の内容を確認して変換が正しくできていることを確認してください。

変換後は、対応する設定`skk-uim-personal-dic-encoding`を`utf-8`に変更し、uimを再起動してください。
