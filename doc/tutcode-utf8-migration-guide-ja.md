# uim-tutcode UTF-8対応 移行ガイド

## 概要

`uim-tutcode`の内部文字列処理がEUC-JPからUTF-8に変わり、辞書をUTF-8で使用できるようになりました。
ここでは必要な移行手順を説明します。

## 辞書の文字コード設定

以下の設定で、交ぜ書き辞書、部首辞書、補助予測入力で使用するSKK-JISYOの文字コードを指定できます。
デフォルトはEUC-JPなので、EUC-JPの辞書を使用する場合は変更不要です。
UTF-8で使用する場合は、各辞書に対応する設定を変更してください。

| 変数 | 対象 | デフォルト |
| --- | --- | --- |
| `tutcode-dic-file-encoding` | システム交ぜ書き辞書 | `euc-jp` |
| `tutcode-personal-dic-encoding` | 個人交ぜ書き辞書 | `euc-jp` |
| `tutcode-bushu-index2-encoding` | `bushu.index2` | `euc-jp` |
| `tutcode-bushu-expand-encoding` | `bushu.expand` | `euc-jp` |
| `tutcode-bushu-help-encoding` | `bushu.help` | `euc-jp` |
| `predict-custom-look-skk-jisyo-encoding` | Look-SKK予測用SKK-JISYO | `euc-jp` |

### EUC-JPの個人辞書をUTF-8へ変換する

UTF-8のシステム交ぜ書き辞書を使用する場合は、学習先の個人辞書もUTF-8に変更してください。
個人辞書をEUC-JPのまま使用すると、EUC-JPにない文字を含む候補を学習した際に、辞書の保存に失敗し、学習内容を保存できません。
ここではEUC-JPの個人辞書をUTF-8へ変換する方法を説明します。

```sh
cp ~/.mazegaki.dic ~/.mazegaki.dic.bak && \
iconv -f EUC-JIS-2004 -t UTF-8 ~/.mazegaki.dic > ~/.mazegaki.dic.tmp && \
chmod 600 ~/.mazegaki.dic.tmp && mv ~/.mazegaki.dic.tmp ~/.mazegaki.dic
```

`EUC-JIS-2004`での変換がエラーになる場合は、`EUC-JISX0213`、`EUC-JP`の順に試してください。
変換できた場合は、~/.mazegaki.dic.bak と ~/.mazegaki.dic の内容を確認して変換が正しくできていることを確認してください。

変換後は、対応する設定`tutcode-personal-dic-encoding`を`utf-8`に変更し、uimを再起動してください。

## コード表ファイルの文字コード

`tutcode-rule-filename`で指定するコード表ファイルは、uim-tutcodeが読み込む
Schemeソースです。カスタムコード表を使用する場合はUTF-8で保存してください。

```scheme
;; ~/.uim
(define tutcode-rule-filename "/path/to/custom-rule.scm")
```
