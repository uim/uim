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

### EUC-JPの辞書をUTF-8へ変換する

システム辞書や `skkserv` をUTF-8で使用する場合、学習先の個人辞書もUTF-8に変更してください。
個人辞書をEUC-JPのまま使用すると、EUC-JPにない文字を含む候補を学習した際に、個人辞書全体の保存に失敗し、学習内容を保存できません。
ここではEUC-JPのユーザ辞書をUTF-8へ変換する方法を説明します。

```sh
cp ~/.skk-uim-jisyo ~/.skk-uim-jisyo.bak
iconv -f EUC-JIS-2004 -t UTF-8 ~/.skk-uim-jisyo > ~/.skk-uim-jisyo.tmp && mv ~/.skk-uim-jisyo.tmp ~/.skk-uim-jisyo
```

EUC-JIS-2004での変換がエラーになる場合は、EUC-JISX0213、EUC-JPの順に試してください。
変換できた場合は、~/.skk-uim-jisyo.bak と ~/.skk-uim-jisyo の内容を確認して変換が正しくできていることを確認してください。

対応する辞書エンコーディング設定
`skk-uim-personal-dic-encoding`を`utf-8`に変更して、uimを再起動してください。

## ユーザー設定ファイルの文字コード

`~/.uim`などのユーザー設定ファイルでSKKの設定をしていて、EUC-JPの文字が
含まれる場合は、UTF-8に変換する必要があります。具体的には、以下の変数を
設定している場合が対象です。

- `skk-ja-rk-rule`
- `skk-auto-start-henkan-keyword-list`
- `skk-style-uim`
- `skk-style-ddskk-like`

これら以外の設定にEUC-JPの文字を含まない場合は、ファイル全体をUTF-8に
変換してください。EUC-JPの設定を同じファイルに残す場合は、元のファイルを
EUC-JPのままにして、新しいUTF-8のファイルを作成し、`load`で読み込んでください。
例えば、`.uim-utf8`を読み込む場合は、以下のように記載します。

```scheme
;; ~/.uim
(load (string-append (getenv "HOME") "/.uim-utf8"))
```
