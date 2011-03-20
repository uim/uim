;;;
;;; Copyright (c) 2003-2011 uim Project http://code.google.com/p/uim/
;;;
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;; 3. Neither the name of authors nor the names of its contributors
;;;    may be used to endorse or promote products derived from this software
;;;    without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS IS'' AND
;;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE
;;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.
;;;;

;;; tutcode.scm: TUT-Code for Japanese input.
;;;
;;; TUT-Code<http://www.crew.sfc.keio.ac.jp/~chk/>入力スクリプト。
;;; TUT-Code配列で日本語の入力を行う。
;;; TUT-Code以外のT-CodeやTry-Codeの入力も、コード表の設定により可能。
;;;
;;; 【部首合成変換】(ala)
;;;   再帰的な部首合成変換も可能です。
;;;   部首合成のアルゴリズムはtc-2.1のものです。
;;;
;;; * 対話的な部首合成変換
;;;   tc-2.3.1のtc-bushu.elの移植です。(ただしsortは未対応です)
;;;   以下のような設定をすると使用可能になります。
;;;   (define tutcode-use-interactive-bushu-conversion? #t)
;;;   (define tutcode-bushu-index2-filename "/usr/local/share/tc/bushu.index2")
;;;   (define tutcode-bushu-expand-filename "/usr/local/share/tc/bushu.expand")
;;;   (define tutcode-interactive-bushu-start-sequence "als")
;;;   bushu.index2とbushu.expandファイルは、
;;;   tc-2.3.1のインストール時に生成・インストールされるファイルです。
;;;
;;; 【交ぜ書き変換】(alj)
;;;   交ぜ書き変換辞書はtc2と同じ形式(SKK辞書と同様の形式)です。
;;; 
;;; * 交ぜ書き変換辞書(例:/usr/local/share/tc/mazegaki.dic)へのアクセスは
;;;   libuim-skk.soの機能を使っています。
;;;   そのため、学習機能もSKKと同様の動作になります:
;;;     + 確定した候補は次回の変換から先頭に来ます。
;;;     + 確定した候補は個人辞書(~/.mazegaki.dic)に保存されます。
;;;   これらの学習機能をオフにするには、
;;;   tutcode-enable-mazegaki-learning?変数を#fに設定してください。
;;; ** 交ぜ書き変換辞書への登録・削除もSKKと同様の動作になります:
;;;     + ~/.mazegaki.dicへの登録・削除。
;;;     + 登録: 変換候補の最後まで行ったら再帰的登録モードに移行。
;;;             あるいは、読みを入力後、|を押す。
;;;     + 削除: 辞書からの削除は、削除したい候補を選んで!を押す。
;;; 
;;; * 活用する語の変換
;;;   tutcode-mazegaki-enable-inflection?変数を#tに設定すると、活用しない語
;;;   としての変換候補が見つからなかった時に、活用する語として変換を試みます。
;;;   (ただし、活用しない語の候補が1個の場合の自動確定はしなくなります。
;;;    次の文字の入力を開始すれば確定されます。)
;;;   また、最初から活用する語として変換したい場合は、読みに"―"を付けるか、
;;;   以下のキーで変換開始してください。
;;;     tutcode-postfix-mazegaki-inflection-start-sequence (後置型用キーの流用)
;;;
;;;   候補表示中は、<と>キーにより、語幹(活用語尾以外の部分)の伸縮が可能です。
;;;   (ただし、読みの入力時に"―"を付けて語幹を明示した場合を除く)
;;;
;;; * 読みをカタカナとして確定
;;;   tutcode-katakana-commit-keyにカタカナ確定キーを設定すると
;;;   使用可能になります。
;;;
;;; * 英字変換(SKK abbrev)モードを追加しています(al/)。
;;;   例えば、「file」を入力して「ファイル」に変換する機能です。
;;;
;;; 【後置型変換】
;;;   uimのsurrounding text関係のAPIを使って、
;;;   カーソル前の文字列の取得・削除を行います。
;;;   そのため、uimのsurrounding text APIをサポートしているブリッジ
;;;   (uim-gtk, uim-qt, uim-qt4(lineeditのみ))でのみ後置型変換が可能です。
;;;
;;;   これら以外のブリッジでも後置型変換を使いたい場合、
;;;   tutcode-enable-fallback-surrounding-text?を#tに設定すると、
;;;   surrounding text APIが使用できない場合に、
;;;   文字列の取得は内部の確定済文字列バッファから行い、
;;;   文字列の削除は"\b"(tutcode-fallback-backspace-string)を送出します。
;;;     - \b(BS,0x08)文字を受けた時に削除を行うアプリでのみ動作。
;;;     - 内部の確定済文字列バッファは補完と兼用で、
;;;       長さはtutcode-completion-chars-maxの値。
;;;
;;; * 後置型部首合成変換は、開始キーをtutcode-postfix-bushu-start-sequenceに
;;;   設定すると使用可能になります。
;;; * 後置型交ぜ書き変換は、以下の開始キーを設定すると使用可能になります。
;;;  活用しない語 tutcode-postfix-mazegaki-start-sequence
;;;  活用する語   tutcode-postfix-mazegaki-inflection-start-sequence
;;;  活用しない語(読み1文字) tutcode-postfix-mazegaki-1-start-sequence
;;;   ...
;;;  活用しない語(読み9文字) tutcode-postfix-mazegaki-9-start-sequence
;;;  活用する語(読み1文字) tutcode-postfix-mazegaki-inflection-1-start-sequence
;;;   ...
;;;  活用する語(読み9文字) tutcode-postfix-mazegaki-inflection-9-start-sequence
;;; * 後置型交ぜ書き変換における、読み/語幹の伸縮
;;;   候補表示中は、<と>キーにより、読み/語幹の伸縮が可能です。
;;;   活用する語に関しては、語幹が長いものを優先して変換します。
;;;     例:「あおい」>「おい」>「い」
;;;         (tutcode-mazegaki-enable-inflection?が#tの場合、
;;;          さらに縮めると活用する語として変換)
;;;        >「あおい―」>「あお―」>「おい―」>「あ―」>「お―」>「い―」
;;;        (実際にはtc2付属のmazegaki.dicに「あおい―」は無いのでスキップ)
;;; ** 読みの文字数を指定して変換開始した場合
;;;    活用する語に関しては、読みは指定された文字数で固定して語幹のみ伸縮。
;;;      例(「あおい」に対して3文字指定):「あおい―」>「あお―」>「あ―」
;;;
;;; 【ヘルプ機能】
;;; * 仮想鍵盤表示(表形式の候補ウィンドウを流用)
;;;   各位置のキーの打鍵により入力される文字を表示します。
;;;   uim-pref-gtkでの表示・非表示設定の他に、
;;;   <Control>/で一時的に表示・非表示の切り替えも可能です。
;;;   (打ち方があやふやな文字を入力するときだけ表示したい場合があるので)
;;;  - 「*」前付き文字:該当キーの打鍵により、
;;;    該当文字を含む仮想鍵盤が表示されることを表します。(中間打鍵)
;;;  - 「+」前付き文字:該当キーが、該当文字の中間打鍵であることを表します。
;;;    (熟語ガイドの中間打鍵)
;;;  - 「+」後付き文字:熟語ガイドの最終打鍵であることを表します。
;;; * 自動ヘルプ表示機能(表形式の候補ウィンドウを流用)
;;;   交ぜ書き変換や部首合成変換で入力した文字の打ち方を表示します。
;;;   部首合成方法も、簡単な合成に関しては表示可能です。
;;;   例:交ぜ書き変換で「憂鬱」を確定した場合
;;;    ┌─┬─┬─┬─┬─┬─┬──────┬─┬─┬───┬─┐
;;;    │  │  │  │  │  │  │            │  │  │      │  │
;;;    ├─┼─┼─┼─┼─┤  ├──────┼─┼─┼───┼─┤
;;;    │  │  │  │  │b │  │            │  │  │f     │  │
;;;    ├─┼─┼─┼─┼─┤  ├──────┼─┼─┼───┼─┤
;;;    │  │3 │  │  │  │  │            │  │  │1(憂) │  │
;;;    ├─┼─┼─┼─┼─┤  ├──────┼─┼─┼───┼─┤
;;;    │  │  │d │  │e │  │2a(鬱▲林缶)│  │  │      │  │
;;;    └─┴─┴─┴─┴─┴─┴──────┴─┴─┴───┴─┘
;;; ** 直近に表示した自動ヘルプの再表示
;;;    tutcode-auto-help-redisplay-sequenceに以下のようにキーシーケンスを
;;;    設定すると使用可能になります。
;;;      (define tutcode-auto-help-redisplay-sequence "al-")
;;;
;;; 【補完/予測入力・熟語ガイド】
;;; +「補完」:確定済文字列に対して、続く文字列の候補を表示します。
;;; +「予測入力」:交ぜ書き変換の読みに対して、変換後文字列の候補を表示します。
;;; +「熟語ガイド」:補完/予測入力候補文字列をもとに、
;;;   次に入力が予測される文字の打鍵ガイドを表示します('+'を付けて表示)。
;;; * 補完/予測入力・熟語ガイドとも候補ウィンドウに表示します。
;;; * 補完/予測入力機能を使うには、
;;;   uim-pref-gtk等の「補助予測入力」グループの設定が必要です。
;;;     (a)Look-SKKを有効にしてmazegaki.dic相当の辞書ファイルを指定する。
;;;        (主に予測入力用)
;;;     (b)Lookを有効にして単語ファイルを指定する。(補完用)
;;;        mazegaki.dicの読みに、漢字としては入っていない単語を補完したい場合。
;;;        (例:「狂」を入力した時点で「奔」を補完して欲しいが、
;;;         「狂奔」がmazegaki.dicには読みとしては入っていないので、
;;;         (a)だけでは補完されない。((a)は読みを検索するので))
;;;         mazegaki.dicから変換後の単語を抜き出して、
;;;         補完用単語ファイルを生成するには、以下のコマンドで可能。
;;;           awk -F'[ /]' '{for(i=3;i<=NF;i++)if(length($i)>2)print $i}' \
;;;           mazegaki.dic | sort -u > mazegaki.words
;;;     (c)Sqlite3を有効にする。
;;;        補完/予測入力で選択した候補を学習したい場合、一番上に配置。
;;;   二分探索するので、(a)(b)のファイルはソートしておく必要があります。
;;; * 補完/予測入力の開始は以下のいずれかのタイミング:
;;; ** 補完: tutcodeオンの状態でtutcode-completion-chars-minの文字数入力時
;;; ** 補完: tutcodeオンの状態で<Control>.打鍵時
;;; ** 予測入力: 交ぜ書き変換の読み入力状態で
;;;              tutcode-prediction-start-char-countの文字数入力時
;;; ** 予測入力: 交ぜ書き変換の読み入力状態で<Control>.打鍵時
;;; * 補完候補表示にさらに<Control>.を打鍵すると対象文字を1つ減らして再補完。
;;;   長すぎる文字列を対象に補完された場合に、補完し直しができるように。
;;; * 上記の補完開始文字数(tutcode-completion-chars-min)や
;;;   予測開始文字数(tutcode-prediction-start-char-count)を0に設定すると、
;;;   <Control>.打鍵時にのみ補完/予測入力候補を表示します。
;;; * 熟語ガイド(次に入力が予測される文字の打鍵ガイド)は
;;;   補完/予測入力候補から作っています。
;;; * 熟語ガイドで表示されている+付き文字に対応するキーを入力した場合、
;;;   2打鍵目以降も仮想鍵盤上に+付きで表示するので、
;;;   ガイドに従って漢字の入力が可能です。
;;;   (通常は仮想鍵盤非表示の場合でも、一時的に<Control>/で表示すれば確認可能)
;;;
;;; - (理想的には、次の打鍵がしばらく無い場合に補完/予測入力候補を表示したいの
;;;    ですが、現状のuimにはタイマが無いので、打鍵直後に表示されます。
;;;    1文字入力するごとに表示されると邪魔なことが多いので、
;;;    デフォルトでは2文字入力時に表示する設定にしています:
;;;      tutcode-completion-chars-min, tutcode-prediction-start-char-count。
;;;    この場合でも、1文字入力後に<Control>.キー(tutcode-begin-completion-key)
;;;    により補完/予測入力候補の表示が可能です。)
;;;
;;; 【部首合成変換時の予測入力】
;;;   部首合成変換辞書を検索して、入力された部首が含まれる項目を表示。
;;;
;;; 【記号入力モード】
;;;   <Control>_で記号入力モードのトグル。
;;;   全角英数入力モードとしても使えるようにしています。
;;;
;;; 【2ストローク記号入力モード】
;;;   百相鍵盤『き』と同様に、2打鍵で各種の記号・漢字を入力するモード。
;;;   漢字は基本的に文字コード順に並んでいます。
;;;   (通常の記号入力モードでは、目的の文字までたどりつくために
;;;   next-pageキーを何回も押す必要があって面倒なので)
;;;
;;; 【設定例】
;;; * コード表の一部を変更したい場合は、例えば~/.uimで以下のように記述する。
;;;   (require "tutcode.scm")
;;;   (tutcode-rule-set-sequences!
;;;     '(((("s" " "))("―"))                ; 記号の定義を変更
;;;       ((("a" "l" "i"))("捗"))            ; 追加
;;;       ((("d" "l" "u"))("づ" "ヅ"))       ; カタカナを含む場合
;;;       ((("d" "l" "d" "u"))("っ" "ッ"))))
;;;
;;; * T-Code/Try-Codeを使いたい場合
;;;   uim-pref-gtk等で設定するか、~/.uimで以下のように設定してください。
;;;    (define tutcode-rule-filename "/usr/local/share/uim/tcode.scm")
;;;    ;(define tutcode-rule-filename "/usr/local/share/uim/trycode.scm")
;;;    (define tutcode-mazegaki-start-sequence "fj")
;;;    (define tutcode-bushu-start-sequence "jf")
;;;    (define tutcode-latin-conv-start-sequence "47")
;;;    (define tutcode-kana-toggle-key? (make-key-predicate '()))
;;;
;;; 【ソースについて】
;;; generic.scmをベースにして以下の変更をしている。
;;;  * キーシーケンス中のスペースが有効になるように変更。
;;;  * ひらがな/カタカナモードの切り替えを追加。
;;;  * rk入力中の未確定(preedit)文字列の表示をしないようにした
;;;    (EmacsのT/TUT-Code入力環境tc2では表示しないのでそれに合わせて)。
;;;  * 交ぜ書き変換ではSKK形式の辞書を使うので、
;;;    skk.scmのかな漢字変換処理から必要な部分を取り込み。
;;;  * 部首合成変換機能を追加。
;;;  * 記号入力モードを追加。
;;;  * 仮想鍵盤表示機能を追加。
;;;  * 自動ヘルプ表示機能を追加。
;;;  * 補完/予測入力・熟語ガイド機能を追加。

(require-extension (srfi 1 2 8))
(require "generic.scm")
(require "generic-predict.scm")
(require-custom "tutcode-custom.scm")
(require-custom "generic-key-custom.scm")
(require-custom "tutcode-key-custom.scm")
(require-dynlib "skk") ;SKK形式の交ぜ書き辞書の検索のためlibuim-skk.soをロード
(require "tutcode-bushudic.scm") ;部首合成変換辞書
(require "tutcode-kigoudic.scm") ;記号入力モード用の記号表
(require "tutcode-dialog.scm"); 交ぜ書き変換辞書からの削除確認ダイアログ
(require "japanese.scm") ; for ja-wide or ja-make-kana-str{,-list}
(require "ustr.scm")

;;; user configs

;; widgets and actions

;; widgets
(define tutcode-widgets '(widget_tutcode_input_mode))

;; default activity for each widgets
(define default-widget_tutcode_input_mode 'action_tutcode_direct)

;; actions of widget_tutcode_input_mode
(define tutcode-input-mode-actions
  (if tutcode-use-kigou2-mode?
    '(action_tutcode_direct
      action_tutcode_hiragana
      action_tutcode_katakana
      action_tutcode_kigou
      action_tutcode_kigou2)
    '(action_tutcode_direct
      action_tutcode_hiragana
      action_tutcode_katakana
      action_tutcode_kigou)))

;;; 使用するコード表。
;;; tutcode-context-new時に(tutcode-custom-load-rule!)で設定
(define tutcode-rule ())
;;; 2ストローク記号入力モード用コード表
(define tutcode-kigou-rule ())
;;; tutcode-ruleから作成する、逆引き検索(漢字から打鍵リストを取得)用alist。
;;; (自動ヘルプ用の部首合成変換候補検索時の高速化のため)
(define tutcode-reverse-rule-alist ())
;;; tutcode-kigou-ruleから作成する、逆引き検索用alist。
(define tutcode-reverse-kigou-rule-alist ())
;;; tutcode-bushudicから作成する、
;;; 逆引き検索(合成後の文字から合成用の2文字を取得)用alist。
;;; (自動ヘルプ用の部首合成変換候補検索時の高速化のため)
(define tutcode-reverse-bushudic-alist ())
;;; stroke-helpで、何もキー入力が無い場合に表示する内容のalist。
;;; (毎回tutcode-ruleを全てなめて作成すると遅いし、
;;; 最初のページは固定内容なので、一度作成したものを使い回す)
(define tutcode-stroke-help-top-page-alist ())
;;; stroke-helpで、何もキー入力が無い場合に表示する内容のalist。
;;; カタカナモード用。
;;; (XXX:キー入力有の場合もキャッシュを使うようにする?
;;;  もしそうすれば、~/.uimで仮想鍵盤表示内容のカスタマイズも容易になる)
(define tutcode-stroke-help-top-page-katakana-alist ())

;;; コード表を上書き変更/追加するためのコード表。
;;; ~/.uimでtutcode-rule-set-sequences!で登録して、
;;; tutcode-context-new時に反映する。
(define tutcode-rule-userconfig ())

;;; 候補ウィンドウのプログラム名。
;;; uim-ximが、UIM_LIBEXECDIR/uim-candwin-progを候補ウィンドウとして使用。
;;; gtk-immodule等が、表形式候補ウィンドウを使用するか判断するため、
;;; "uim-candwin-tbl"で始まっているかどうかをチェックしている。
;;; 表形式候補ウィンドウをcustomで設定できるようにするため、
;;; あらかじめdefine。
;;; XXX:tutcode以外にも影響するので、他の場所の方がいいかも。
(define uim-candwin-prog "")
(if tutcode-use-table-style-candidate-window?
  (set! uim-candwin-prog "uim-candwin-tbl-gtk"))

;;; 表形式の候補ウィンドウ上の各ボタンとキーの対応表(13列8行)。
;;; 表形式候補ウィンドウが参照して使用する。
(define uim-candwin-prog-layout ())
;;; 表形式候補ウィンドウ上のキーレイアウト:QWERTY(JIS)配列。
(define uim-candwin-prog-layout-qwerty-jis
  '("1" "2" "3" "4" "5"  "6" "7" "8" "9" "0"  "-" "^" "\\"
    "q" "w" "e" "r" "t"  "y" "u" "i" "o" "p"  "@" "[" ""
    "a" "s" "d" "f" "g"  "h" "j" "k" "l" ";"  ":" "]" ""
    "z" "x" "c" "v" "b"  "n" "m" "," "." "/"  ""  ""  " "
    "!" "\"" "#" "$" "%" "&" "'" "(" ")" ""   "=" "~" "|"
    "Q" "W" "E" "R" "T"  "Y" "U" "I" "O" "P"  "`" "{" ""
    "A" "S" "D" "F" "G"  "H" "J" "K" "L" "+"  "*" "}" ""
    "Z" "X" "C" "V" "B"  "N" "M" "<" ">" "?"  "_" ""  ""))
;;; 表形式候補ウィンドウ上のキーレイアウト:QWERTY(US/ASCII)配列。
(define uim-candwin-prog-layout-qwerty-us
  '("1" "2" "3" "4" "5"  "6" "7" "8" "9" "0"  "-" "=" "\\"
    "q" "w" "e" "r" "t"  "y" "u" "i" "o" "p"  "[" "]" ""
    "a" "s" "d" "f" "g"  "h" "j" "k" "l" ";"  "'" "`" ""
    "z" "x" "c" "v" "b"  "n" "m" "," "." "/"  ""  ""  " "
    "!" "@" "#" "$" "%"  "^" "&" "*" "(" ")"  "_" "+" "|"
    "Q" "W" "E" "R" "T"  "Y" "U" "I" "O" "P"  "{" "}" ""
    "A" "S" "D" "F" "G"  "H" "J" "K" "L" ":"  "\"" "~" ""
    "Z" "X" "C" "V" "B"  "N" "M" "<" ">" "?"  ""  ""  ""))
;;; 表形式候補ウィンドウ上のキーレイアウト:DVORAK配列。
;;; (記号の配置は統一されたものは無いようなので一例)
(define uim-candwin-prog-layout-dvorak
  '("1" "2" "3" "4" "5"  "6" "7" "8" "9" "0"  "[" "]" "\\"
    "'" "," "." "p" "y"  "f" "g" "c" "r" "l"  "/" "=" ""
    "a" "o" "e" "u" "i"  "d" "h" "t" "n" "s"  "-" "`" ""
    ";" "q" "j" "k" "x"  "b" "m" "w" "v" "z"  ""  ""  " "
    "!" "@" "#" "$" "%"  "^" "&" "*" "(" ")"  "{" "}" "|"
    "\"" "<" ">" "P" "Y" "F" "G" "C" "R" "L"  "?" "+" ""
    "A" "O" "E" "U" "I"  "D" "H" "T" "N" "S"  "_" "~" ""
    ":" "Q" "J" "K" "X"  "B" "M" "W" "V" "Z"  ""  ""  ""))
;;; 表形式の候補ウィンドウ上の各ボタンとキーの対応表を設定。
;;; (~/.uimはこの後で実行されるので、
;;;  ~/.uimで変更するにはuim-candwin-prog-layoutを上書きする必要あり)
(set! uim-candwin-prog-layout
  (case tutcode-candidate-window-table-layout
    ((qwerty-jis) uim-candwin-prog-layout-qwerty-jis)
    ((qwerty-us) uim-candwin-prog-layout-qwerty-us)
    ((dvorak) uim-candwin-prog-layout-dvorak)
    (else ()))) ; default

;;; 交ぜ書き変換時の候補選択用ラベル文字のリスト(表形式候補ウィンドウ用)。
;;; QWERTY(JIS)配列用。
(define tutcode-table-heading-label-char-list-qwerty-jis
  '("a" "s" "d" "f" "g" "h" "j" "k" "l" ";"
    "q" "w" "e" "r" "t" "y" "u" "i" "o" "p"
    "z" "x" "c" "v" "b" "n" "m" "," "." "/"
    "1" "2" "3" "4" "5" "6" "7" "8" "9" "0"))
;;; 交ぜ書き変換時の候補選択用ラベル文字のリスト(表形式候補ウィンドウ用)。
;;; QWERTY(US)配列用。
(define tutcode-table-heading-label-char-list-qwerty-us
  tutcode-table-heading-label-char-list-qwerty-jis)
;;; 交ぜ書き変換時の候補選択用ラベル文字のリスト(表形式候補ウィンドウ用)。
;;; DVORAK配列用。
(define tutcode-table-heading-label-char-list-dvorak
  '("a" "o" "e" "u" "i"  "d" "h" "t" "n" "s"
    "'" "," "." "p" "y"  "f" "g" "c" "r" "l"
    ";" "q" "j" "k" "x"  "b" "m" "w" "v" "z"
    "1" "2" "3" "4" "5"  "6" "7" "8" "9" "0"))
;;; 交ぜ書き変換時の候補選択用ラベル文字のリスト(表形式候補ウィンドウ用)。
;;; (打ちやすい場所から先に候補を埋める)
(define tutcode-table-heading-label-char-list
  tutcode-table-heading-label-char-list-qwerty-jis)
;;; 交ぜ書き変換時の候補選択用ラベル文字のリスト(uimスタイル用)
(define tutcode-uim-heading-label-char-list
  '("1" "2" "3" "4" "5" "6" "7" "8" "9" "0"
    "a" "b" "c" "d" "e" "f" "g" "h" "i" "j"
    "k" "l" "m" "n" "o" "p" "q" "r" "s" "t"
    "u" "v" "w" "x" "y" "z"
    "A" "B" "C" "D" "E" "F" "G" "H" "I" "J"
    "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T"
    "U" "V" "W" "X" "Y" "Z"))
;;; 交ぜ書き変換時の候補選択用ラベル文字のリスト
(define tutcode-heading-label-char-list ())

;;; 記号入力モード時の候補選択用ラベル文字のリスト(表形式候補ウィンドウ用)。
;;; (キーボードレイアウトに従って、左上から右下へ順に候補を埋める)
(define tutcode-table-heading-label-char-list-for-kigou-mode
  (if (null? uim-candwin-prog-layout)
    (delete "" uim-candwin-prog-layout-qwerty-jis)
    (delete "" uim-candwin-prog-layout)))
;;; 記号入力モード時の候補選択用ラベル文字のリスト(uimスタイル用)
(define tutcode-uim-heading-label-char-list-for-kigou-mode
  '(" "
    "1" "2" "3" "4" "5" "6" "7" "8" "9" "0"
    "a" "b" "c" "d" "e" "f" "g" "h" "i" "j"
    "k" "l" "m" "n" "o" "p" "q" "r" "s" "t"
    "u" "v" "w" "x" "y" "z"
    "-" "^" "\\" "@" "[" ";" ":" "]" "," "." "/"
    "!" "\"" "#" "$" "%" "&" "'" "(" ")"
    "A" "B" "C" "D" "E" "F" "G" "H" "I" "J"
    "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T"
    "U" "V" "W" "X" "Y" "Z"
    "=" "~" "|" "`" "{" "+" "*" "}" "<" ">" "?" "_"))
;;; 記号入力モード時の候補選択用ラベル文字のリスト
;;; (全角英数モードとして使うには、tutcode-kigoudicと合わせる必要あり)
(define tutcode-heading-label-char-list-for-kigou-mode ())

;;; 補完/予測入力時の候補選択用ラベル文字のリスト。
;;; (通常の文字入力に影響しないように、1打鍵目とかぶらない文字を使用。
;;; 記号(や数字)は直接入力できるように、ここでは含めない)
;;; QWERTY(JIS)配列用。TUT-Code用。
(define tutcode-heading-label-char-list-for-prediction-qwerty
  '(                     "Y" "U" "I" "O" "P"
                         "H" "J" "K" "L"
    "Z" "X" "C" "V" "B"  "N" "M"))
;;; 補完/予測入力時の候補選択用ラベル文字のリスト。
;;; DVORAK配列用。TUT-Code用。
(define tutcode-heading-label-char-list-for-prediction-dvorak
  '(                     "F" "G" "C" "R" "L"
                         "D" "H" "T" "N" "S"
        "Q" "J" "K" "X"  "B" "M" "W" "V" "Z"))
;;; 補完/予測入力時の候補選択用ラベル文字のリスト。
(define tutcode-heading-label-char-list-for-prediction
  tutcode-heading-label-char-list-for-prediction-qwerty)

;;; 自動ヘルプでの文字の打ち方表示の際に候補文字列として使う文字のリスト
(define tutcode-auto-help-cand-str-list
  ;; 第1,2,3打鍵を示す文字(部首1用, 部首2用)
  '((("1" "2" "3") ("4" "5" "6")) ; 1文字目用
    (("a" "b" "c") ("d" "e" "f")) ; 2文字目用
    (("A" "B" "C") ("D" "E" "F"))
    (("一" "二" "三") ("四" "五" "六"))
    (("あ" "い" "う") ("か" "き" "く"))
    (("ア" "イ" "ウ") ("カ" "キ" "ク"))))

;;; 熟語ガイド用マーク
(define tutcode-guide-mark "+")
;;; 熟語ガイド用終了マーク
(define tutcode-guide-end-mark "+")
;;; 仮想鍵盤のストローク途中で、
;;; 続きに来る漢字のヒントとして表示する漢字に付けるマーク
(define tutcode-hint-mark "*")
;;; 2ストローク記号入力モード時に仮想鍵盤表示を行うかどうかの設定
(define tutcode-use-stroke-help-window-another? #t)

;;; 後置型交ぜ書き変換の読み取得時に、読みに含めない文字のリスト
(define tutcode-postfix-mazegaki-terminate-char-list
  '("\n" "\t" " " "、" "。" "，" "．" "・" "「" "」" "（" "）"))

;;; surrounding text APIが使えない時に、文字削除のためにcommitする文字列
(define tutcode-fallback-backspace-string "\b")

;;; implementations

;;; 交ぜ書き変換辞書の初期化が終わっているかどうか
(define tutcode-dic #f)

;;; list of context
(define tutcode-context-list '())

(define tutcode-prepare-activation
  (lambda (tc)
    (let ((rkc (tutcode-context-rk-context tc)))
      (rk-flush rkc))))

(register-action 'action_tutcode_direct
		 (lambda (tc)
		   '(ja_halfwidth_alnum
		     "a"
		     "直接入力"
		     "直接入力モード"))
		 (lambda (c)
		   (let ((tc (tutcode-find-descendant-context c)))
                     (not (tutcode-context-on? tc))))
		 (lambda (c)
		   (let ((tc (tutcode-find-descendant-context c)))
                     (tutcode-prepare-activation tc)
                     (tutcode-flush tc)
                     (tutcode-context-set-state! tc 'tutcode-state-off)
                     (tutcode-update-preedit tc))));flushでクリアした表示を反映

(register-action 'action_tutcode_hiragana
		 (lambda (tc)
		   '(ja_hiragana
		     "あ"
		     "ひらがな"
		     "ひらがなモード"))
		 (lambda (c)
		   (let ((tc (tutcode-find-descendant-context c)))
                     (and (tutcode-context-on? tc)
                          (not (eq? (tutcode-context-state tc)
                                    'tutcode-state-kigou))
                          (not (tutcode-context-katakana-mode? tc))
                          (not (tutcode-kigou2-mode? tc)))))
		 (lambda (c)
		   (let ((tc (tutcode-find-descendant-context c)))
                     (tutcode-prepare-activation tc)
                     (if
                       (or
                         (not (tutcode-context-on? tc)) ; 変換中状態は変更しない
                         (eq? (tutcode-context-state tc) 'tutcode-state-kigou))
                       (begin
                         (tutcode-reset-candidate-window tc)
                         (tutcode-context-set-state! tc 'tutcode-state-on)))
                     (if (tutcode-kigou2-mode? tc)
                       (begin
                         (tutcode-reset-candidate-window tc)
                         (tutcode-toggle-kigou2-mode tc)))
                     (tutcode-context-set-katakana-mode! tc #f)
                     (tutcode-update-preedit tc))))

(register-action 'action_tutcode_katakana
		 (lambda (tc)
		   '(ja_katakana
		     "ア"
		     "カタカナ"
		     "カタカナモード"))
		 (lambda (c)
		   (let ((tc (tutcode-find-descendant-context c)))
                     (and (tutcode-context-on? tc)
                          (not (eq? (tutcode-context-state tc)
                                    'tutcode-state-kigou))
                          (tutcode-context-katakana-mode? tc)
                          (not (tutcode-kigou2-mode? tc)))))
		 (lambda (c)
		   (let ((tc (tutcode-find-descendant-context c)))
                     (tutcode-prepare-activation tc)
                     (if
                       (or
                         (not (tutcode-context-on? tc)) ; 変換中状態は変更しない
                         (eq? (tutcode-context-state tc) 'tutcode-state-kigou))
                       (begin
                         (tutcode-reset-candidate-window tc)
                         (tutcode-context-set-state! tc 'tutcode-state-on)))
                     (if (tutcode-kigou2-mode? tc)
                       (begin
                         (tutcode-reset-candidate-window tc)
                         (tutcode-toggle-kigou2-mode tc)))
                     (tutcode-context-set-katakana-mode! tc #t)
                     (tutcode-update-preedit tc))))

(register-action 'action_tutcode_kigou
                 (lambda (tc)
                   '(ja_fullwidth_alnum
                     "Ａ"
                     "記号入力"
                     "記号入力モード"))
                 (lambda (c)
		   (let ((tc (tutcode-find-descendant-context c)))
                     (eq? (tutcode-context-state tc) 'tutcode-state-kigou)))
                 (lambda (c)
		   (let ((tc (tutcode-find-descendant-context c)))
                     (tutcode-prepare-activation tc)
                     (if
                       (not
                         (eq? (tutcode-context-state tc) 'tutcode-state-kigou))
                       (tutcode-flush tc))
                     (tutcode-begin-kigou-mode tc)
                     (tutcode-update-preedit tc))))

(register-action 'action_tutcode_kigou2
                 (lambda (tc)
                   '(ja_fullwidth_alnum
                     "き"
                     "記号入力2"
                     "記号入力モード2"))
                 (lambda (c)
                   (let ((tc (tutcode-find-descendant-context c)))
                     (and (tutcode-context-on? tc)
                          (not (eq? (tutcode-context-state tc)
                                    'tutcode-state-kigou))
                          (tutcode-kigou2-mode? tc))))
                 (lambda (c)
		   (let ((tc (tutcode-find-descendant-context c)))
                     (tutcode-prepare-activation tc)
                     (if
                       (or
                         (not (tutcode-context-on? tc)) ; 変換中状態は変更しない
                         (eq? (tutcode-context-state tc) 'tutcode-state-kigou))
                       (begin
                         (tutcode-reset-candidate-window tc)
                         (tutcode-context-set-state! tc 'tutcode-state-on)))
                     (if (not (tutcode-kigou2-mode? tc))
                       (begin
                         (tutcode-reset-candidate-window tc)
                         (tutcode-toggle-kigou2-mode tc)))
                     (tutcode-update-preedit tc))))

;; Update widget definitions based on action configurations. The
;; procedure is needed for on-the-fly reconfiguration involving the
;; custom API
(define tutcode-configure-widgets
  (lambda ()
    (register-widget 'widget_tutcode_input_mode
		     (activity-indicator-new tutcode-input-mode-actions)
		     (actions-new tutcode-input-mode-actions))))

(define tutcode-context-rec-spec
  (append
   context-rec-spec
   (list
     (list 'rk-context ()) ; キーストロークから文字への変換のためのコンテキスト
     (list 'rk-context-another ()) ;もう一つのrk-context(2stroke記号入力モード)
     ;;; TUT-Code入力状態
     ;;; 'tutcode-state-off TUT-Codeオフ
     ;;; 'tutcode-state-on TUT-Codeオン
     ;;; 'tutcode-state-yomi 交ぜ書き変換の読み入力中
     ;;; 'tutcode-state-converting 交ぜ書き変換の候補選択中
     ;;; 'tutcode-state-bushu 部首入力・変換中
     ;;; 'tutcode-state-interactive-bushu 対話的部首合成変換中
     ;;; 'tutcode-state-kigou 記号入力モード
     (list 'state 'tutcode-state-off)
     ;;; カタカナモードかどうか
     ;;; #t: カタカナモード。#f: ひらがなモード。
     (list 'katakana-mode #f)
     ;;; 交ぜ書き変換/部首合成変換の対象の文字列リスト(逆順)
     ;;; (例: 交ぜ書き変換で読み「かん字」を入力した場合、("字" "ん" "か"))
     (list 'head ())
     ;;; 交ぜ書き変換の選択中の候補の番号
     (list 'nth 0)
     ;;; 交ぜ書き変換の候補数
     (list 'nr-candidates 0)
     ;;; 後置型交ぜ書き変換時に、変換に使用する読みの長さ。
     ;;; (確定時にim-delete-textするために使用)
     ;;; 後置型か前置型かの判定にも使用。(前置型の場合は0)
     (list 'postfix-yomi-len 0)
     ;;; 交ぜ書き変換開始時に指定された読みの文字数。
     ;;; 前置型の場合は入力済みの読みの文字数。
     (list 'mazegaki-yomi-len-specified 0)
     ;;; 交ぜ書き変換用の読み全体。後置型の場合は取得済みの読み
     (list 'mazegaki-yomi-all ())
     ;;; 交ぜ書き変換時の活用語尾
     ;;; (活用する語は語尾を―に置換して検索するので、確定時に戻すために使用)
     (list 'mazegaki-suffix ())
     ;;; 候補ウィンドウの状態
     ;;; 'tutcode-candidate-window-off 非表示
     ;;; 'tutcode-candidate-window-converting 交ぜ書き変換候補表示中
     ;;; 'tutcode-candidate-window-kigou 記号表示中
     ;;; 'tutcode-candidate-window-stroke-help 仮想鍵盤表示中
     ;;; 'tutcode-candidate-window-auto-help 自動ヘルプ表示中
     ;;; 'tutcode-candidate-window-predicting 補完/予測入力候補表示中
     ;;; 'tutcode-candidate-window-interactive-bushu 対話的部首合成変換候補表示
     (list 'candidate-window 'tutcode-candidate-window-off)
     ;;; ストローク表
     ;;; 次に入力するキーと文字の対応の、get-candidate-handler用形式でのリスト
     (list 'stroke-help ())
     ;;; 自動ヘルプ
     (list 'auto-help ())
     ;;; 交ぜ書き変換辞書への再帰的登録のための子コンテキスト
     (list 'child-context ())
     ;;; 子コンテキストの種類
     ;;; 'tutcode-child-type-editor 登録用の変換後文字列編集エディタ
     ;;; 'tutcode-child-type-dialog 辞書からの削除確認ダイアログ
     (list 'child-type ())
     ;;; 親コンテキスト
     (list 'parent-context ())
     ;;; 登録用文字列編集エディタ
     (list 'editor ())
     ;;; 削除確認ダイアログ
     (list 'dialog ())
     ;;; 英字変換(SKK abbrev)モードかどうか
     (list 'latin-conv #f)
     ;;; commit済の文字列リスト(補完用)
     (list 'commit-strs ())
     ;;; commit-strsのうちで補完に使用している文字数
     (list 'commit-strs-used-len 0)
     ;;; 補完/予測入力の候補選択中かどうか
     ;;; 'tutcode-predicting-off 補完/予測入力の候補選択中でない
     ;;; 'tutcode-predicting-completion 補完候補選択中
     ;;; 'tutcode-predicting-prediction 交ぜ書き変換時の予測入力候補選択中
     ;;; 'tutcode-predicting-bushu 部首合成変換時の予測入力候補選択中
     ;;; 'tutcode-predicting-interactive-bushu 対話的部首合成変換中
     (list 'predicting 'tutcode-predicting-off)
     ;;; 補完/予測入力用コンテキスト
     (list 'prediction-ctx ())
     ;;; 補完/予測入力候補の読みのリスト
     (list 'prediction-word ())
     ;;; 補完/予測入力候補の候補のリスト
     (list 'prediction-candidates ())
     ;;; 補完/予測入力候補のappendixのリスト
     (list 'prediction-appendix ())
     ;;; 補完/予測入力候補数
     (list 'prediction-nr 0)
     ;;; 補完/予測入力候補の現在選択されているインデックス(熟語ガイド込み)
     (list 'prediction-index 0)
     ;;; 補完/予測入力候補数(熟語ガイド分含む)
     (list 'prediction-nr-all 0)
     ;;; ページごとの補完/予測入力の候補表示数(熟語ガイド分は除く)
     (list 'prediction-nr-in-page tutcode-nr-candidate-max-for-prediction)
     ;;; ページごとの補完/予測入力の候補表示数(熟語ガイド分も含む)
     (list 'prediction-page-limit
      (+ tutcode-nr-candidate-max-for-prediction
         tutcode-nr-candidate-max-for-guide))
     ;;; 部首合成変換の予測候補
     (list 'prediction-bushu ())
     ;;; 部首合成変換の予測候補の現在の表示ページの最初のインデックス番号
     (list 'prediction-bushu-page-start 0)
     ;;; 熟語ガイド。補完/予測入力時の表示用。
     ;;; 予測される次の入力漢字の第1打鍵と入力漢字の対応のリスト。
     ;;; 例: (("," "石") ("u" "屋" "池"))
     (list 'guide ())
     ;;; 熟語ガイド作成元データ。仮想鍵盤(stroke-help)へのガイド表示用。
     ;;; 文字とストロークのリスト(rk-lib-find-partial-seqs用形式)。
     ;;; 例: (((("," "r"))("石")) ((("u" "c"))("屋")) ((("u" "v"))("池")))
     (list 'guide-chars ())
     )))
(define-record 'tutcode-context tutcode-context-rec-spec)
(define tutcode-context-new-internal tutcode-context-new)
(define tutcode-context-katakana-mode? tutcode-context-katakana-mode)
(define (tutcode-context-on? pc)
  (not (eq? (tutcode-context-state pc) 'tutcode-state-off)))
(define (tutcode-kigou2-mode? pc)
  (and tutcode-use-kigou2-mode?
       (eq? (rk-context-rule (tutcode-context-rk-context pc))
            tutcode-kigou-rule)))

;;; TUT-Codeのコンテキストを新しく生成する。
;;; @return 生成したコンテキスト
(define (tutcode-context-new id im)
  (if (not tutcode-dic)
    (if (not (symbol-bound? 'skk-lib-dic-open))
      (begin
        (if (symbol-bound? 'uim-notify-info)
          (uim-notify-info
            (N_ "libuim-skk.so is not available. Mazegaki conversion is disabled")))
        (set! tutcode-use-recursive-learning? #f)
        (set! tutcode-enable-mazegaki-learning? #f))
      (begin
        (set! tutcode-dic (skk-lib-dic-open tutcode-dic-filename #f "localhost" 0 'unspecified))
        (if tutcode-use-recursive-learning?
          (require "tutcode-editor.scm"))
        (tutcode-read-personal-dictionary))))
  (let ((tc (tutcode-context-new-internal id im)))
    (tutcode-context-set-widgets! tc tutcode-widgets)
    (if (null? tutcode-rule)
      (begin
        (tutcode-custom-load-rule! tutcode-rule-filename)
        (if tutcode-use-dvorak?
          (begin
            (set! tutcode-rule (tutcode-rule-qwerty-to-dvorak tutcode-rule))
            (set! tutcode-heading-label-char-list-for-prediction
              tutcode-heading-label-char-list-for-prediction-dvorak)))
        ;; tutcode-mazegaki/bushu-start-sequenceは、
        ;; tutcode-use-dvorak?がオンのときはDvorakのシーケンスとみなして反映。
        ;; つまり、ruleのqwerty-to-dvorak変換後に反映する。
        (tutcode-custom-set-mazegaki/bushu-start-sequence!)
        (tutcode-rule-commit-sequences! tutcode-rule-userconfig)))
    ;; 表形式候補ウィンドウ用設定
    (if (null? tutcode-heading-label-char-list)
      (if tutcode-use-table-style-candidate-window?
        (set! tutcode-heading-label-char-list
          (case tutcode-candidate-window-table-layout
            ((qwerty-jis) tutcode-table-heading-label-char-list-qwerty-jis)
            ((qwerty-us) tutcode-table-heading-label-char-list-qwerty-us)
            ((dvorak) tutcode-table-heading-label-char-list-dvorak)
            (else tutcode-table-heading-label-char-list)))
        (set! tutcode-heading-label-char-list
          tutcode-uim-heading-label-char-list)))
    (if (null? tutcode-heading-label-char-list-for-kigou-mode)
      (if tutcode-use-table-style-candidate-window?
        (begin
          (set! tutcode-heading-label-char-list-for-kigou-mode
            tutcode-table-heading-label-char-list-for-kigou-mode)
          ;; 記号入力モードを全角英数モードとして使うため、
          ;; tutcode-heading-label-char-list-for-kigou-modeを全角にして
          ;; tutcode-kigoudicの先頭に入れる
          (set! tutcode-kigoudic
            (append
              (map (lambda (lst) (list (ja-wide lst)))
                tutcode-heading-label-char-list-for-kigou-mode)
              (list-tail tutcode-kigoudic
                (length tutcode-heading-label-char-list-for-kigou-mode)))))
        (set! tutcode-heading-label-char-list-for-kigou-mode
          tutcode-uim-heading-label-char-list-for-kigou-mode)))
    (tutcode-context-set-rk-context! tc (rk-context-new tutcode-rule #t #f))
    (if tutcode-use-kigou2-mode?
      (begin
        (if (null? tutcode-kigou-rule)
          (begin
            (require "tutcode-kigou-rule.scm") ;2stroke記号入力モード用コード表
            (tutcode-kigou-rule-translate
              tutcode-candidate-window-table-layout)))
        (tutcode-context-set-rk-context-another!
          tc (rk-context-new tutcode-kigou-rule #t #f))))
    (if tutcode-use-recursive-learning?
      (tutcode-context-set-editor! tc (tutcode-editor-new tc)))
    (tutcode-context-set-dialog! tc (tutcode-dialog-new tc))
    (if tutcode-use-interactive-bushu-conversion?
      (require "tutcode-bushu.scm"))
    (if (or tutcode-use-completion? tutcode-use-prediction?)
      (begin
        (tutcode-context-set-prediction-ctx! tc (predict-make-meta-search))
        (predict-meta-open (tutcode-context-prediction-ctx tc) "tutcode")
        (predict-meta-set-external-charset! (tutcode-context-prediction-ctx tc) "EUC-JP")))
    tc))

;;; ひらがな/カタカナモードの切り替えを行う。
;;; 現状の状態がひらがなモードの場合はカタカナモードに切り替える。
;;; 現状の状態がカタカナモードの場合はひらがなモードに切り替える。
;;; @param pc コンテキストリスト
(define (tutcode-context-kana-toggle pc)
  (let ((s (tutcode-context-katakana-mode? pc)))
    (tutcode-context-set-katakana-mode! pc (not s))))

;;; 根っこのコンテキストを取得する。
(define (tutcode-find-root-context pc)
  (let ((ppc (tutcode-context-parent-context pc)))
    (if (null? ppc)
      pc
      (tutcode-find-root-context ppc))))

;;; 枝先のコンテキスト(交ぜ書き変換の再帰的登録の一番深いところ
;;; =現在編集中のコンテキスト)を取得する。
(define (tutcode-find-descendant-context pc)
  (let ((cpc (tutcode-context-child-context pc)))
    (if (null? cpc)
      pc
      (tutcode-find-descendant-context cpc))))

(define (tutcode-predict pc str)
  (predict-meta-search
   (tutcode-context-prediction-ctx pc)
   str))
;;; 補完/予測入力候補を検索
;;; @param str 検索文字列
;;; @param completion? 補完の場合は#t
;;; @return 重複除去前の全ての読みのリスト(熟語ガイド用)
(define (tutcode-lib-set-prediction-src-string pc str completion?)
  (let* ((ret      (tutcode-predict pc str))
         (word     (predict-meta-word? ret))
         (cands    (predict-meta-candidates? ret))
         (appendix (predict-meta-appendix? ret))
         (word/cand/appendix (map list word cands appendix))
         (uniq-word/cand/appendix 
          ;; 重複候補を除く
          (delete-duplicates word/cand/appendix
            (lambda (x y)
              (let ((xcand (list-ref x 1))
                    (ycand (list-ref y 1)))
                (string=? xcand ycand)))))
         (strlen (string-length str))
         (filtered-word/cand/appendix
          (if completion?
            (filter
              ;; 補完時はstrで始まっていない候補を除く(strはcommit済なので)
              (lambda (elem)
                (let ((cand (list-ref elem 1)))
                  (and
                    (> (string-length cand) strlen)
                    (string=? str (substring cand 0 strlen)))))
              uniq-word/cand/appendix)
            uniq-word/cand/appendix))
         (filtered-word
          (map (lambda (x) (list-ref x 0)) filtered-word/cand/appendix))
         (filtered-cands
          (map (lambda (x) (list-ref x 1)) filtered-word/cand/appendix))
         (filtered-appendix
          (map (lambda (x) (list-ref x 2)) filtered-word/cand/appendix)))
    (tutcode-context-set-prediction-word! pc filtered-word)
    (tutcode-context-set-prediction-candidates! pc
      (if completion?
        (map
          (lambda (cand)
            ;; 補完時は先頭のstrを削除:
            ;; strは確定済文字列なので、文字の重複を避けるため。
            (if (string=? str (substring cand 0 strlen))
              (substring cand strlen (string-length cand))
              cand))
          filtered-cands)
        filtered-cands))
    (tutcode-context-set-prediction-appendix! pc filtered-appendix)
    (tutcode-context-set-prediction-nr! pc (length filtered-cands))
    word))

;;; 部首合成変換時の予測入力候補を設定
;;; @param start-index 開始番号
(define (tutcode-lib-set-bushu-prediction pc start-index)
  ;; 開始番号から始まる1ページぶんの候補だけを取り出して使用。
  ;; 全て使用すると、熟語ガイドのラベル文字列が長くなって横幅が広がりすぎ、
  ;; ウィンドウに収まらなくなる場合があるので(候補数200以上の場合など)。
  ;; (熟語ガイドを、表示中の候補だけから作る方法もあるが、
  ;; その場合、熟語ガイドの数が、ページごとに変わってしまうため、
  ;; 現状の候補ウィンドウ(ページごとの表示候補数が変わらないことを想定)
  ;; での表示に問題が発生)
  (let* ((ret (tutcode-context-prediction-bushu pc))
         (all-len (length ret))
         (start
          (cond
            ((>= start-index all-len)
              (tutcode-context-prediction-bushu-page-start pc))
            ((< start-index 0)
              0)
            (else
              start-index)))
         (end (+ start tutcode-nr-candidate-max-for-prediction))
         (cnt
          (if (< end all-len)
            tutcode-nr-candidate-max-for-prediction
            (- all-len start)))
         (page-word/cand (take (drop ret start) cnt))
         (page-word (map (lambda (elem) (car elem)) page-word/cand))
         (page-cands (map (lambda (elem) (cadr elem)) page-word/cand))
         (len (length page-cands))
         (appendix (make-list len "")))
    (tutcode-context-set-prediction-bushu-page-start! pc start)
    (tutcode-context-set-prediction-word! pc page-word)
    (tutcode-context-set-prediction-candidates! pc page-cands)
    (tutcode-context-set-prediction-appendix! pc appendix)
    (tutcode-context-set-prediction-nr! pc len)))

(define (tutcode-lib-get-nr-predictions pc)
  (tutcode-context-prediction-nr pc))
(define (tutcode-lib-get-nth-word pc nth)
  (let ((word (tutcode-context-prediction-word pc)))
    (list-ref word nth)))
(define (tutcode-lib-get-nth-prediction pc nth)
  (let ((cands (tutcode-context-prediction-candidates pc)))
    (list-ref cands nth)))
(define (tutcode-lib-get-nth-appendix pc nth)
  (let ((appendix (tutcode-context-prediction-appendix pc)))
    (list-ref appendix nth)))
(define (tutcode-lib-commit-nth-prediction pc nth completion?)
  (let ((cand (tutcode-lib-get-nth-prediction pc nth)))
    (predict-meta-commit
      (tutcode-context-prediction-ctx pc)
      (tutcode-lib-get-nth-word pc nth)
      (if completion?
        ;; 補完時は、candsからは元々付いていた
        ;; 先頭のcommit-strsを削除しているので、復元
        (string-append
          (tutcode-make-string
            (take (tutcode-context-commit-strs pc)
                  (tutcode-context-commit-strs-used-len pc)))
          cand)
        cand)
      (tutcode-lib-get-nth-appendix pc nth))))

;;; 熟語ガイド表示用候補リストを補完/予測入力候補から作成する
;;; @param str 補完/予測入力候補の検索時に使用した文字列=入力済文字列
;;; @param completion? 補完時は#t
;;; @param all-yomi 予測入力候補検索結果に含まれる全ての読み
(define (tutcode-guide-set-candidates pc str completion? all-yomi)
  (let* ((cands (tutcode-context-prediction-candidates pc))
         (rule (rk-context-rule (tutcode-context-rk-context pc)))
         (word all-yomi)
         (strlen (string-length str))
         (filtered-cands
          (if (not completion?)
            (filter
              ;; 予測入力時はstrで始まっていない候補も入ってるので除く
              (lambda (cand)
                (and
                  (> (string-length cand) strlen)
                  (string=? str (substring cand 0 strlen))))
              cands)
            cands))
         ;; 読み入力中は、読み(word)も見て次に来る可能性のある漢字をガイド
         ;; 例:"あお"を入力した時点で、look結果内の"あお虫"という読みをもとに、
         ;;    "虫"をガイド
         (filtered-words
          (if completion?
            ()
            (filter
              (lambda (cand)
                (let ((candlen (string-length cand)))
                  (and
                    (> candlen strlen)
                    ;; strの後に、活用する語を示す"―"しか残らない候補は除く
                    (not (string=?  "―" (substring cand strlen candlen))))))
              word)))
         (trim-str
          (lambda (lst)
            (if (not completion?)
              (map
                (lambda (cand)
                  ;; 予測入力時は入力済のstrも含まれているので、
                  ;; それより後の文字をガイド表示
                  (substring cand strlen (string-length cand)))
                lst)
              lst)))
         (trim-cands (trim-str filtered-cands))
         (trim-words (trim-str filtered-words))
         (candchars ; 予測した熟語の1文字目の漢字のリスト
          (delete-duplicates
            (map (lambda (cand) (last (string-to-list cand)))
              (append trim-cands trim-words))))
         (cand-stroke
          (map
            (lambda (elem)
              (list (list (tutcode-reverse-find-seq elem rule)) (list elem)))
            candchars))
         (filtered-cand-stroke
          (filter
            (lambda (elem)
              (pair? (caar elem))) ; コード表に無い外字は除く
            cand-stroke))
         (label-cands-alist
          (tutcode-guide-update-alist () filtered-cand-stroke)))
    (tutcode-context-set-guide! pc label-cands-alist)
    (tutcode-context-set-guide-chars! pc filtered-cand-stroke)))

;;; 熟語ガイド表示用候補リストを部首合成予測入力候補から作成する
;;; @param str 部首合成予測入力候補の検索時に使用した漢字=入力済漢字
(define (tutcode-guide-set-candidates-for-bushu pc)
  (let* ((word (tutcode-context-prediction-word pc))
         (rule (rk-context-rule (tutcode-context-rk-context pc)))
         (cand-stroke
          (map
            (lambda (elem)
              (list (list (tutcode-reverse-find-seq elem rule)) (list elem)))
            word))
         (filtered-cand-stroke
          (filter
            (lambda (elem)
              (pair? (caar elem))) ; コード表に無い外字は除く
            cand-stroke))
         (label-cands-alist
          (tutcode-guide-update-alist () filtered-cand-stroke)))
    (tutcode-context-set-guide! pc label-cands-alist)
    (tutcode-context-set-guide-chars! pc filtered-cand-stroke)))

;;; 熟語ガイドの表示に使うalistを更新する。
;;; alistは以下のようにラベル文字と漢字のリスト。
;;; 例: (("," "石") ("u" "屋" "池"))
;;; @param label-cands-alist 元のalist
;;; @param kanji-list 漢字とストロークのリスト
;;; 例: (((("," "r"))("石")) ((("u" "c"))("屋")) ((("u" "v"))("池")))
;;; @return 更新後の熟語ガイド用alist
(define (tutcode-guide-update-alist label-cands-alist kanji-list)
  (if (null? kanji-list)
    label-cands-alist
    (let*
      ((kanji-stroke (car kanji-list))
       (kanji (caadr kanji-stroke))
       (stroke (caar kanji-stroke)))
      (tutcode-guide-update-alist
        (tutcode-auto-help-update-stroke-alist-with-key label-cands-alist
          kanji (car stroke))
        (cdr kanji-list)))))

;;; 交ぜ書き変換用個人辞書を読み込む。
(define (tutcode-read-personal-dictionary)
  (if (not (setugid?))
      (skk-lib-read-personal-dictionary tutcode-dic tutcode-personal-dic-filename)))

;;; 交ぜ書き変換用個人辞書を書き込む。
;;; @param force? tutcode-enable-mazegaki-learning?が#fでも書き込むかどうか
(define (tutcode-save-personal-dictionary force?)
  (if (and
        (or force? tutcode-enable-mazegaki-learning?)
        (not (setugid?)))
      (skk-lib-save-personal-dictionary tutcode-dic tutcode-personal-dic-filename)))

;;; キーストロークから文字への変換のためのrk-push-key!を呼び出す。
;;; 戻り値が#fでなければ、戻り値(リスト)のcarを返す。
;;; ただし、カタカナモードの場合は戻り値リストのcadrを返す。
;;; (rk-push-key!はストローク途中の場合は#fを返す)
;;; @param pc コンテキストリスト
;;; @param key キーの文字列
(define (tutcode-push-key! pc key)
  (let ((res (rk-push-key! (tutcode-context-rk-context pc) key)))
    (and res
      (if
        (and
          (not (null? (cdr res)))
          (tutcode-context-katakana-mode? pc))
        (cadr res)
        (car res)))))

;;; 変換中状態をクリアする。
;;; @param pc コンテキストリスト
(define (tutcode-flush pc)
  (let ((cpc (tutcode-context-child-context pc)))
    (rk-flush (tutcode-context-rk-context pc))
    (if tutcode-use-recursive-learning?
      (tutcode-editor-flush (tutcode-context-editor pc)))
    (tutcode-dialog-flush (tutcode-context-dialog pc))
    (if (tutcode-context-on? pc) ; オフ時に呼ばれた場合はオンにしたら駄目
      (tutcode-context-set-state! pc 'tutcode-state-on)) ; 変換状態をクリアする
    (tutcode-context-set-head! pc ())
    (tutcode-context-set-nr-candidates! pc 0)
    (tutcode-context-set-postfix-yomi-len! pc 0)
    (tutcode-context-set-mazegaki-yomi-len-specified! pc 0)
    (tutcode-context-set-mazegaki-yomi-all! pc ())
    (tutcode-context-set-mazegaki-suffix! pc ())
    (tutcode-reset-candidate-window pc)
    (tutcode-context-set-latin-conv! pc #f)
    (tutcode-context-set-child-context! pc ())
    (tutcode-context-set-child-type! pc ())
    (if (not (null? cpc))
      (tutcode-flush cpc))))

;;; 変換対象の文字列リストから文字列を作る。
;;; @param sl 文字列リスト
(define (tutcode-make-string sl)
  (if (null? sl)
    ""
    (string-append (tutcode-make-string (cdr sl)) (car sl))))

;;; 交ぜ書き変換中のn番目の候補を返す。
;;; @param pc コンテキストリスト
;;; @param n 対象の候補番号
(define (tutcode-get-nth-candidate pc n)
  (let* ((head (tutcode-context-head pc))
         (cand (skk-lib-get-nth-candidate
                tutcode-dic
                n
                (cons (tutcode-make-string head) "")
                ""
                #f)))
    cand))

;;; 記号入力モード時のn番目の候補を返す。
;;; @param n 対象の候補番号
(define (tutcode-get-nth-candidate-for-kigou-mode pc n)
 (car (nth n tutcode-kigoudic)))

;;; 交ぜ書き変換中の現在選択中の候補を返す。
;;; @param pc コンテキストリスト
(define (tutcode-get-current-candidate pc)
  (tutcode-get-nth-candidate pc (tutcode-context-nth pc)))

;;; 記号入力モード時の現在選択中の候補を返す。
(define (tutcode-get-current-candidate-for-kigou-mode pc)
  (tutcode-get-nth-candidate-for-kigou-mode pc (tutcode-context-nth pc)))

;;; 交ぜ書き変換で確定した文字列を返す。
;;; @param pc コンテキストリスト
;;; @return 確定した文字列
(define (tutcode-prepare-commit-string pc)
  (let ((res (tutcode-get-current-candidate pc))
        (suffix (tutcode-context-mazegaki-suffix pc))
        (head (tutcode-context-head pc)))
    ;; いつも特定のラベルキーで特定の候補を確定する使い方ができるように、
    ;; tutcode-enable-mazegaki-learning?が#fの場合は候補の並び順を変えない。
    ;; (例:「かい」の変換において、常にdキーで「悔」、eキーで「恢」を確定)
    (if tutcode-enable-mazegaki-learning?
      (begin
        ;; skk-lib-commit-candidateを呼ぶと学習が行われ、候補順が変更される
        (skk-lib-commit-candidate
          tutcode-dic
          (cons (tutcode-make-string head) "")
          ""
          (tutcode-context-nth pc)
          #f)
        (if (> (tutcode-context-nth pc) 0)
          (tutcode-save-personal-dictionary #f))))
    (tutcode-flush pc)
    (if (null? suffix)
      res
      (string-append res (tutcode-make-string suffix)))))

;;; 記号入力モード時に確定した文字列を返す。
(define (tutcode-prepare-commit-string-for-kigou-mode pc)
  (tutcode-get-current-candidate-for-kigou-mode pc))

;;; im-commit-rawを呼び出す。
;;; ただし、子コンテキストの場合は、editorかdialogに入力キーを渡す。
(define (tutcode-commit-raw pc key key-state)
  (if (or tutcode-use-completion? tutcode-enable-fallback-surrounding-text?)
    (tutcode-append-commit-string pc (im-get-raw-key-str key key-state)))
  (let ((ppc (tutcode-context-parent-context pc)))
    (if (not (null? ppc))
      (if (eq? (tutcode-context-child-type ppc) 'tutcode-child-type-editor)
        (tutcode-editor-commit-raw (tutcode-context-editor ppc) key key-state)
        (tutcode-dialog-commit-raw (tutcode-context-dialog ppc) key key-state))
      (im-commit-raw pc))))

;;; im-commitを呼び出す。
;;; ただし、子コンテキストの場合は、editorかdialogに入力キーを渡す。
;;; @param str コミットする文字列
;;; @param opt-skip-append-commit-strs? commit-strsへの追加を
;;;  スキップするかどうか。未指定時は#f
(define (tutcode-commit pc str . opt-skip-append-commit-strs?)
  (if
    (and (or tutcode-use-completion? tutcode-enable-fallback-surrounding-text?)
         (not (:optional opt-skip-append-commit-strs? #f)))
    (tutcode-append-commit-string pc str))
  (let ((ppc (tutcode-context-parent-context pc)))
    (if (not (null? ppc))
      (if (eq? (tutcode-context-child-type ppc) 'tutcode-child-type-editor)
        (tutcode-editor-commit (tutcode-context-editor ppc) str)
        (tutcode-dialog-commit (tutcode-context-dialog ppc) str))
      (im-commit pc str))))

;;; im-commitを呼び出すとともに、自動ヘルプ表示のチェックを行う
(define (tutcode-commit-with-auto-help pc)
  (let* ((head (tutcode-context-head pc))
         (yomi-len (tutcode-context-postfix-yomi-len pc))
         (suffix (tutcode-context-mazegaki-suffix pc))
         (res (tutcode-prepare-commit-string pc))) ; flushによりhead等がクリア
    (if (> yomi-len 0)
      (tutcode-postfix-delete-text pc yomi-len))
    (tutcode-commit pc res)
    (tutcode-check-auto-help-window-begin pc
      (drop (string-to-list res) (length suffix))
      (append suffix head))))

;;; 交ぜ書き変換の候補選択時に、指定されたラベル文字に対応する候補を確定する
;;; @param ch 入力されたラベル文字
(define (tutcode-commit-by-label-key pc ch)
  ;; 現在候補ウィンドウに表示されていないラベル文字を入力した場合、
  ;; 現在以降の候補内において入力ラベル文字に対応する候補を確定する。
  ;; (学習機能をオフにして候補の並び順を固定にして使用する場合に、
  ;; next-page-keyを押す回数を減らし、
  ;; なるべく少ないキーで目的の候補を選べるようにするため)
  (let* ((nr (tutcode-context-nr-candidates pc))
         (nth (tutcode-context-nth pc))
         (cur-page (cond
                     ((= tutcode-nr-candidate-max 0) 0)
                     (else
                       (quotient nth tutcode-nr-candidate-max))))
         ;; 現在候補ウィンドウに表示中の候補リストの先頭の候補番号
         (cur-offset (* cur-page tutcode-nr-candidate-max))
         (cur-labels (list-tail
                       tutcode-heading-label-char-list
                       (remainder cur-offset
                                  (length tutcode-heading-label-char-list))))
         (target-labels (member ch cur-labels))
         (offset (if target-labels
                   (- (length cur-labels) (length target-labels))
                   (+ (length cur-labels)
                      (- (length tutcode-heading-label-char-list)
                         (length
                           (member ch tutcode-heading-label-char-list))))))
         (idx (+ cur-offset offset)))
    (if (and (>= idx 0)
             (< idx nr))
      (begin
        (tutcode-context-set-nth! pc idx)
        (tutcode-commit-with-auto-help pc)))))

;;; 記号入力モード時に、指定されたラベル文字に対応する候補を確定する
(define (tutcode-commit-by-label-key-for-kigou-mode pc ch)
  ;; 交ぜ書き変換時と異なり、現在より前の候補を確定する場合あり
  ;; (全角英数入力モードとして使えるようにするため)。
  ;; (記号入力モード時は、一度確定した候補を連続して入力できるように、
  ;; 確定後は直前の候補を選択しているが、
  ;; このとき交ぜ書き変換時と同様の候補選択を行うと、
  ;; ラベル文字リストの2周目で対応する候補を確定してしまう場合がある
  ;; (例:thと打った場合、全角英数入力としてはｔｈになって欲しいが、ｔーになる)
  ;; ため、交ぜ書き変換とは異なる候補確定処理を行う)
  (let* ((nr (tutcode-context-nr-candidates pc))
         (nth (tutcode-context-nth pc))
         (labellen (length tutcode-heading-label-char-list-for-kigou-mode))
         (cur-base (quotient nth labellen))
         (offset
           (- labellen
              (length
                (member ch tutcode-heading-label-char-list-for-kigou-mode))))
         (idx (+ (* cur-base labellen) offset)))
    (if (and (>= idx 0)
             (< idx nr))
      (begin
        (tutcode-context-set-nth! pc idx)
        (tutcode-commit pc
          (tutcode-prepare-commit-string-for-kigou-mode pc))))))

;;; 補完/予測入力候補表示時に、指定されたラベル文字に対応する候補を確定する
;;; @param ch 入力されたラベル文字
;;; @param mode tutcode-context-predictingの値
(define (tutcode-commit-by-label-key-for-prediction pc ch mode)
  (let*
    ((nth (tutcode-context-prediction-index pc))
     (page-limit (tutcode-context-prediction-page-limit pc))
     (cur-page (quotient nth page-limit))
     (nr-in-page (tutcode-context-prediction-nr-in-page pc))
     ;; 現在候補ウィンドウに表示中の候補リストの先頭の候補番号
     (cur-offset (* cur-page nr-in-page))
     (labellen (length tutcode-heading-label-char-list-for-prediction))
     (cur-labels
       (list-tail
         tutcode-heading-label-char-list-for-prediction
         (remainder cur-offset labellen)))
     (target-labels (member ch cur-labels))
     (offset (if target-labels
               (- (length cur-labels) (length target-labels))
               (+ (length cur-labels)
                  (- labellen
                     (length
                       (member ch tutcode-heading-label-char-list-for-prediction))))))
     (nr (tutcode-lib-get-nr-predictions pc))
     (idx (+ cur-offset offset))
     (i (remainder idx nr)))
    (if (>= i 0)
      (begin
        (tutcode-context-set-prediction-index! pc i)
        (case mode
          ((tutcode-predicting-bushu)
            (tutcode-do-commit-prediction-for-bushu pc))
          ((tutcode-predicting-interactive-bushu)
            (tutcode-do-commit-prediction-for-interactive-bushu pc))
          ((tutcode-predicting-completion)
            (tutcode-do-commit-prediction pc #t))
          (else
            (tutcode-do-commit-prediction pc #f)))))))

(define (tutcode-get-prediction-string pc)
  (tutcode-lib-get-nth-prediction
   pc
   (tutcode-context-prediction-index pc)))

(define (tutcode-learn-prediction-string pc completion?)
  (tutcode-lib-commit-nth-prediction
   pc
   (tutcode-context-prediction-index pc)
   completion?))

;;; 補完/予測入力候補を確定する
;;; @param completion? 補完かどうか
(define (tutcode-do-commit-prediction pc completion?)
  (let ((str (tutcode-get-prediction-string pc)))
    (tutcode-learn-prediction-string pc completion?)
    (tutcode-reset-candidate-window pc)
    (tutcode-commit pc str)
    (tutcode-flush pc)
    (tutcode-check-auto-help-window-begin pc (string-to-list str) ())))

;;; 部首合成変換時の予測入力候補を確定する
(define (tutcode-do-commit-prediction-for-bushu pc)
  (let ((str (tutcode-get-prediction-string pc)))
    (tutcode-reset-candidate-window pc)
    (tutcode-bushu-commit pc str)))

;;; 対話的部首合成変換時の候補を確定する
(define (tutcode-do-commit-prediction-for-interactive-bushu pc)
  (let ((str (tutcode-get-prediction-string pc)))
    (tutcode-reset-candidate-window pc)
    (tutcode-commit pc str)
    (tutcode-flush pc)
    (tutcode-check-auto-help-window-begin pc (string-to-list str) ())))

;;; 交ぜ書き変換辞書から、現在選択されている候補を削除する。
(define (tutcode-purge-candidate pc)
  (let ((res (skk-lib-purge-candidate
               tutcode-dic
               (cons (tutcode-make-string (tutcode-context-head pc)) "")
               ""
               (tutcode-context-nth pc)
               #f)))
    (if res
      (tutcode-save-personal-dictionary #t))
    (tutcode-reset-candidate-window pc)
    (tutcode-flush pc)
    res))

;;; 交ぜ書き変換の読み/部首合成変換の部首(文字列リストhead)に文字列を追加する。
;;; @param pc コンテキストリスト
;;; @param str 追加する文字列
(define (tutcode-append-string pc str)
  (if (and str (string? str))
    (tutcode-context-set-head! pc
      (cons str
        (tutcode-context-head pc)))))

;;; commit済の文字列リストcommit-strsに文字列を追加する。
;;; @param str 追加する文字列
(define (tutcode-append-commit-string pc str)
  (if (and str (string? str))
    (let* ((strlist (string-to-list str)) ; strは複数文字の場合あり
           (commit-strs (tutcode-context-commit-strs pc))
           (new-strs (append strlist commit-strs)))
      (tutcode-context-set-commit-strs! pc
        (if (> (length new-strs) tutcode-completion-chars-max)
          (take new-strs tutcode-completion-chars-max)
          new-strs)))))

;;; 交ぜ書き変換を開始する
;;; @param yomi 変換対象の読み(文字列の逆順リスト)
;;; @param suffix 活用する語の変換を行う場合の活用語尾(文字列の逆順リスト)
;;; @param autocommit? 候補が1個の場合に自動的に確定するかどうか
;;; @param recursive-learning? 候補が無い場合に再帰登録モードに入るかどうか
;;; @return #t:候補が有った場合。#f:候補が無かった場合。
(define (tutcode-begin-conversion pc yomi suffix autocommit?
          recursive-learning?)
  (let*
    ((yomi-str (tutcode-make-string yomi))
     (res (and (symbol-bound? 'skk-lib-get-entry)
               (skk-lib-get-entry tutcode-dic yomi-str "" "" #f)
               (skk-lib-get-nr-candidates tutcode-dic yomi-str "" "" #f))))
    (if res
      (begin
        (tutcode-context-set-head! pc yomi)
        (tutcode-context-set-mazegaki-suffix! pc suffix)
        (tutcode-context-set-nth! pc 0)
        (tutcode-context-set-nr-candidates! pc res)
        (tutcode-context-set-state! pc 'tutcode-state-converting)
        (if (and autocommit? (= res 1))
          ;; 候補が1個しかない場合は自動的に確定する。
          ;; (辞書登録はtutcode-register-candidate-keyを押して明示的に開始する)
          (tutcode-commit-with-auto-help pc)
          (begin
            (tutcode-check-candidate-window-begin pc)
            (if (eq? (tutcode-context-candidate-window pc)
                     'tutcode-candidate-window-converting)
              (im-select-candidate pc 0))))
        #t)
      ;; 候補無し
      (begin
        (if recursive-learning?
          (begin
            (tutcode-context-set-head! pc yomi)
            (tutcode-context-set-mazegaki-suffix! pc suffix)
            (tutcode-context-set-state! pc 'tutcode-state-converting)
            (tutcode-setup-child-context pc 'tutcode-child-type-editor)))
          ;(tutcode-flush pc) ; flushすると入力した文字列が消えてがっかり
        #f))))

;;; 前置型交ぜ書き変換を開始する(活用する語にも対応)
;;; @param inflection? 活用する語の検索を行うかどうか
;;; @return #t:候補が有った場合。#f:候補が無かった場合。
(define (tutcode-begin-conversion-with-inflection pc inflection?)
  (let*
    ((yomi (tutcode-context-head pc))
     (yomi-len (length yomi)))
    (tutcode-context-set-postfix-yomi-len! pc 0)
    (tutcode-context-set-mazegaki-yomi-len-specified! pc yomi-len)
    (tutcode-context-set-mazegaki-yomi-all! pc yomi)
    (if (or (not inflection?)
            (not tutcode-mazegaki-enable-inflection?)
            (tutcode-mazegaki-inflection? yomi)) ; 明示的に"―"付きで入力された
      (tutcode-begin-conversion pc yomi () #t tutcode-use-recursive-learning?)
      (or
        (tutcode-begin-conversion pc yomi () #f #f)
        ;; 活用する語として再検索
          (or
            (tutcode-mazegaki-inflection-relimit-right pc yomi-len yomi-len #f)
            ;; 活用しない語として再帰学習
            (and tutcode-use-recursive-learning?
              (begin
                (tutcode-begin-conversion pc yomi () #t
                  tutcode-use-recursive-learning?))))))))

;;; 活用する語の前置型交ぜ書き変換を開始する
;;; @return #t:候補が有った場合。#f:候補が無かった場合。
(define (tutcode-begin-mazegaki-inflection-conversion pc)
  (let*
    ((yomi (tutcode-context-head pc))
     (yomi-len (length yomi)))
    (tutcode-context-set-postfix-yomi-len! pc 0)
    (tutcode-context-set-mazegaki-yomi-len-specified! pc yomi-len)
    (tutcode-context-set-mazegaki-yomi-all! pc yomi)
    (if (tutcode-mazegaki-inflection? yomi) ; 明示的に"―"付きで入力された
      (tutcode-begin-conversion pc yomi () #t tutcode-use-recursive-learning?)
      (tutcode-mazegaki-inflection-relimit-right pc yomi-len yomi-len #f))))

;;; 子コンテキストを作成する。
;;; @param type 'tutcode-child-type-editorか'tutcode-child-type-dialog
(define (tutcode-setup-child-context pc type)
  (let ((cpc (tutcode-context-new (tutcode-context-uc pc)
              (tutcode-context-im pc))))
    (tutcode-context-set-child-context! pc cpc)
    (tutcode-context-set-child-type! pc type)
    (tutcode-context-set-parent-context! cpc pc)
    (if (eq? type 'tutcode-child-type-editor)
      (tutcode-context-set-state! cpc 'tutcode-state-on)
      (tutcode-context-set-state! cpc 'tutcode-state-off))))

;;; 記号入力モードを開始する。
;;; @param pc コンテキストリスト
(define (tutcode-begin-kigou-mode pc)
  (tutcode-context-set-nth! pc 0)
  (tutcode-context-set-nr-candidates! pc (length tutcode-kigoudic))
  (tutcode-context-set-state! pc 'tutcode-state-kigou)
  (tutcode-check-candidate-window-begin pc)
  (if (eq? (tutcode-context-candidate-window pc)
           'tutcode-candidate-window-kigou)
    (im-select-candidate pc 0)))

;;; 2ストローク記号入力モード(tutcode-kigou-rule)とtutcode-ruleの切り替えを行う
;;; @param pc コンテキストリスト
(define (tutcode-toggle-kigou2-mode pc)
  (if tutcode-use-kigou2-mode?
    (let ((tmp-rkc (tutcode-context-rk-context pc))
          (tmp-stroke-help? tutcode-use-stroke-help-window?))
      (tutcode-context-set-rk-context! pc
        (tutcode-context-rk-context-another pc))
      (tutcode-context-set-rk-context-another! pc tmp-rkc)
      (set! tutcode-use-stroke-help-window?
        tutcode-use-stroke-help-window-another?)
      (set! tutcode-use-stroke-help-window-another? tmp-stroke-help?)
      (tutcode-context-set-guide-chars! pc ()))))

;;; 候補ウィンドウの表示を開始する
(define (tutcode-check-candidate-window-begin pc)
  (if (and (eq? (tutcode-context-candidate-window pc)
                'tutcode-candidate-window-off)
           tutcode-use-candidate-window?
           (>= (tutcode-context-nth pc) (- tutcode-candidate-op-count 1)))
    (begin
      (tutcode-context-set-candidate-window! pc
        (if (eq? (tutcode-context-state pc) 'tutcode-state-kigou)
          'tutcode-candidate-window-kigou
          'tutcode-candidate-window-converting))
      (im-activate-candidate-selector
        pc
        (tutcode-context-nr-candidates pc)
        (if (eq? (tutcode-context-state pc) 'tutcode-state-kigou)
          tutcode-nr-candidate-max-for-kigou-mode
          tutcode-nr-candidate-max)))))

;;; 仮想鍵盤の表示を開始する
(define (tutcode-check-stroke-help-window-begin pc)
  (if (eq? (tutcode-context-candidate-window pc) 'tutcode-candidate-window-off)
    (let* ((rkc (tutcode-context-rk-context pc))
           (seq (rk-context-seq rkc))
           (seqlen (length seq))
           (rule (rk-context-rule rkc))
           (ret (rk-lib-find-partial-seqs (reverse seq) rule))
           (katakana? (tutcode-context-katakana-mode? pc))
           ;; 例:(("k" "あ") ("i" "い") ("g" "*贈"))
           (label-cand-alist
            (if (null? seq) ; tutcode-rule全部なめて作成→遅いのでキャッシュ
              (cond
                ((tutcode-kigou2-mode? pc)
                  tutcode-kigou-rule-stroke-help-top-page-alist)
                (katakana?
                  (if (null? tutcode-stroke-help-top-page-katakana-alist)
                    (set! tutcode-stroke-help-top-page-katakana-alist
                      (tutcode-stroke-help-update-alist
                        () seqlen katakana? ret)))
                  tutcode-stroke-help-top-page-katakana-alist)
                (else
                  (if (null? tutcode-stroke-help-top-page-alist)
                    (set! tutcode-stroke-help-top-page-alist
                      (tutcode-stroke-help-update-alist
                        () seqlen katakana? ret)))
                  tutcode-stroke-help-top-page-alist))
              (tutcode-stroke-help-update-alist () seqlen katakana? ret))))
      ;; 熟語ガイドや自動ヘルプからの続きで、入力候補文字にマークを付ける
      (if (and (pair? seq)
               (pair? (tutcode-context-guide-chars pc)))
        (let*
          ((guide-rule (tutcode-context-guide-chars pc))
           (ret (rk-lib-find-partial-seqs (reverse seq) guide-rule))
           (guide-alist (tutcode-stroke-help-guide-update-alist () seqlen ret))
           ;; 例:(("," "石") ("u" "+池屋"))
           (guide-candcombined
            (map
              (lambda (elem)
                (list (car elem) (tutcode-make-string (cdr elem))))
              guide-alist)))
          ;; 表示する候補文字列を、熟語ガイド(+)付き文字列に置き換える
          (for-each
            (lambda (elem)
              (let*
                ((label (car elem))
                 (label-cand (assoc label label-cand-alist)))
                (if label-cand
                  (set-cdr! label-cand (cdr elem)))))
            guide-candcombined)))
      (if (not (null? label-cand-alist))
        (let
          ((stroke-help
            (map
              (lambda (elem)
                (list (cadr elem) (car elem) ""))
              (reverse label-cand-alist))))
          (tutcode-context-set-stroke-help! pc stroke-help)
          (tutcode-context-set-candidate-window! pc
            'tutcode-candidate-window-stroke-help)
          (im-activate-candidate-selector pc
            (length stroke-help) tutcode-nr-candidate-max-for-kigou-mode))))))

;;; 仮想鍵盤の表示を行うかどうかの設定を一時的に切り替える(トグル)。
;;; (常に表示すると目ざわりなので。打ち方に迷ったときだけ表示したい。
;;;  XXX: tc2だと、一定時間以内に次の打鍵が無かったら仮想鍵盤を
;;;  表示するようになっているが、現状のuimで同じことをするのは難しそう。)
(define (tutcode-toggle-stroke-help pc)
  (if tutcode-use-stroke-help-window?
    (begin
      (set! tutcode-use-stroke-help-window? #f)
      (tutcode-reset-candidate-window pc))
    (set! tutcode-use-stroke-help-window? #t)))

;;; 仮想鍵盤表示用データ作成
;;; @param label-cand-alist 表示用データ。
;;;  例:(("k" "あ") ("i" "い") ("g" "*贈"))
;;; @param seqlen 何番目のストロークを対象とするか。
;;; @param katakana? カタカナモードかどうか。
;;; @param rule-list rk-rule。
;;; @return 更新したlabel-cand-alist
(define (tutcode-stroke-help-update-alist
         label-cand-alist seqlen katakana? rule-list)
  (if (null? rule-list)
    label-cand-alist
    (tutcode-stroke-help-update-alist
      (tutcode-stroke-help-update-alist-with-rule
        label-cand-alist seqlen katakana? (car rule-list))
      seqlen katakana? (cdr rule-list))))

;;; 仮想鍵盤表示用データ作成:一つのruleを反映。
;;; @param label-cand-alist 表示用データ。
;;; @param seqlen 何番目のストロークを対象とするか。
;;; @param katakana? カタカナモードかどうか。
;;; @param rule rk-rule内の一つのrule。
;;; @return 更新したlabel-cand-alist
(define (tutcode-stroke-help-update-alist-with-rule
         label-cand-alist seqlen katakana? rule)
  (let* ((label (list-ref (caar rule) seqlen))
         (label-cand (assoc label label-cand-alist)))
    ;; 既に割当てられてたら何もしない→rule中で最初に出現する文字を使用
    (if label-cand
      label-cand-alist
      (let*
        ((candlist (cadr rule))
         ;; シーケンス途中?
         (has-next? (> (length (caar rule)) (+ seqlen 1)))
         (cand
          (or
            (and (not (null? (cdr candlist)))
                 katakana?
                 (cadr candlist))
            (car candlist)))
         (candstr
          (case cand
            ((tutcode-mazegaki-start) "◇")
            ((tutcode-latin-conv-start) "/")
            ((tutcode-bushu-start) "◆")
            ((tutcode-interactive-bushu-start) "▼")
            ((tutcode-postfix-bushu-start) "▲")
            ((tutcode-postfix-mazegaki-start) "△")
            ((tutcode-postfix-mazegaki-1-start) "△1")
            ((tutcode-postfix-mazegaki-2-start) "△2")
            ((tutcode-postfix-mazegaki-3-start) "△3")
            ((tutcode-postfix-mazegaki-4-start) "△4")
            ((tutcode-postfix-mazegaki-5-start) "△5")
            ((tutcode-postfix-mazegaki-6-start) "△6")
            ((tutcode-postfix-mazegaki-7-start) "△7")
            ((tutcode-postfix-mazegaki-8-start) "△8")
            ((tutcode-postfix-mazegaki-9-start) "△9")
            ((tutcode-postfix-mazegaki-inflection-start) "―")
            ((tutcode-postfix-mazegaki-inflection-1-start) "―1")
            ((tutcode-postfix-mazegaki-inflection-2-start) "―2")
            ((tutcode-postfix-mazegaki-inflection-3-start) "―3")
            ((tutcode-postfix-mazegaki-inflection-4-start) "―4")
            ((tutcode-postfix-mazegaki-inflection-5-start) "―5")
            ((tutcode-postfix-mazegaki-inflection-6-start) "―6")
            ((tutcode-postfix-mazegaki-inflection-7-start) "―7")
            ((tutcode-postfix-mazegaki-inflection-8-start) "―8")
            ((tutcode-postfix-mazegaki-inflection-9-start) "―9")
            ((tutcode-auto-help-redisplay) "≪")
            (else cand)))
         (cand-hint
          (or
            ;; シーケンス途中の場合はhint-mark(*)付き
            (and has-next? (string-append tutcode-hint-mark candstr))
            candstr)))
        (cons (list label cand-hint) label-cand-alist)))))

;;; 仮想鍵盤上の熟語ガイドの表示に使うalistに漢字を追加する。
;;; @param kanji-stroke 追加する漢字とストロークのリスト。
;;; 例: ((("," "r"))("石"))
(define (tutcode-stroke-help-guide-add-kanji pc kanji-stroke)
  (let ((chars (tutcode-context-guide-chars pc)))
    (if (not (member kanji-stroke chars))
      (tutcode-context-set-guide-chars! pc (cons kanji-stroke chars)))))

;;; 仮想鍵盤上の熟語ガイドの表示に使うalistを更新する。
;;; alistは以下のようにラベル文字と表示用文字列のリスト。
;;; 例: (("," "石") ("u" "+池屋"))
;;; @param label-cands-alist 元のalist
;;; @param seqlen 何番目のストロークを対象とするか。
;;; @param rule-list rk-rule。
;;; @return 更新後の熟語ガイド用alist
(define (tutcode-stroke-help-guide-update-alist
         label-cands-alist seqlen rule-list)
  (if (null? rule-list)
    label-cands-alist
    (tutcode-stroke-help-guide-update-alist
      (tutcode-stroke-help-guide-update-alist-with-rule
        label-cands-alist seqlen (car rule-list))
      seqlen (cdr rule-list))))

;;; 仮想鍵盤上の熟語ガイド:対象の1文字を、熟語ガイド用alistに追加する。
;;; @param label-cands-alist 元のalist
;;; @param seqlen 何番目のストロークを対象とするか。
;;; @param rule rk-rule内の一つのrule。
;;; @return 更新後の熟語ガイドalist
(define (tutcode-stroke-help-guide-update-alist-with-rule
         label-cands-alist seqlen rule)
  (let* ((label (list-ref (caar rule) seqlen))
         (label-cand (assoc label label-cands-alist))
         (has-next? (> (length (caar rule)) (+ seqlen 1))) ; シーケンス途中?
         (cand (car (cadr rule))))
    (if label-cand
      (begin
        ;; 既に割当てられてたら結合
        (set-cdr! label-cand (cons cand (cdr label-cand)))
        label-cands-alist)
      (cons
        (if has-next?
          (list label cand tutcode-guide-mark)
          (list label tutcode-guide-end-mark cand))
        label-cands-alist))))

;;; 部首合成変換・交ぜ書き変換で確定した文字の打ち方を表示する。
;;; 表形式の候補ウィンドウの場合は、以下のように表示する。
;;; 1が第1打鍵、2が第2打鍵。「携」
;;;  ・・・・        ・・・・
;;;  ・・・・        ・・3 ・
;;;  ・・・・1(携)   ・・・・
;;;  ・・・2         ・・・・
;;; 交ぜ書き変換で複数の文字「携帯」を変換した場合は、以下のように表示する。
;;;  ・・・    ・        ・・・・
;;;  ・・a(帯) ・        ・・3 ・
;;;  ・・・    ・1(携)b  ・・c ・
;;;  ・・・    2         ・・・・
;;; 確定した文字が直接入力できない場合、単純な部首合成変換で入力できれば、
;;; 以下のように部首合成変換方法を表示する。「憂鬱」
;;;    ┌─┬─┬─┬─┬─┬─┬──────┬─┬─┬───┬─┐
;;;    │  │  │  │  │  │  │            │  │  │      │  │
;;;    ├─┼─┼─┼─┼─┤  ├──────┼─┼─┼───┼─┤
;;;    │  │  │  │  │b │  │            │  │  │f     │  │
;;;    ├─┼─┼─┼─┼─┤  ├──────┼─┼─┼───┼─┤
;;;    │  │3 │  │  │  │  │            │  │  │1(憂) │  │
;;;    ├─┼─┼─┼─┼─┤  ├──────┼─┼─┼───┼─┤
;;;    │  │  │d │  │e │  │2a(鬱▲林缶)│  │  │      │  │
;;;    └─┴─┴─┴─┴─┴─┴──────┴─┴─┴───┴─┘
;;;
;;; 通常の候補ウィンドウの場合は、以下のように表示する。
;;;   憂 lns
;;;   鬱 ▲林缶 nt cbo
;;;
;;; @param strlist 確定した文字列のリスト(逆順)
;;; @param yomilist 変換前の読みの文字列のリスト(逆順)
(define (tutcode-check-auto-help-window-begin pc strlist yomilist)
  (if (and (eq? (tutcode-context-candidate-window pc)
                'tutcode-candidate-window-off)
           tutcode-use-auto-help-window?)
    (begin
      (tutcode-context-set-guide-chars! pc ())
      (let*
        ((helpstrlist (lset-difference string=? (reverse strlist) yomilist))
         (label-cands-alist
          (if (not tutcode-auto-help-with-real-keys?)
            ;; 表形式の場合の例:(("y" "2" "1") ("t" "3"))
            (tutcode-auto-help-update-stroke-alist
              pc () tutcode-auto-help-cand-str-list helpstrlist)
            ;; 通常の場合の例:(("暗" "t" "y" "y"))
            (reverse
              (tutcode-auto-help-update-stroke-alist-normal pc () helpstrlist)))))
        (if (not (null? label-cands-alist))
          (let
            ((auto-help
              (map
                (lambda (elem)
                  (list (tutcode-make-string (cdr elem)) (car elem) ""))
                label-cands-alist)))
            (tutcode-context-set-auto-help! pc auto-help)
            (tutcode-context-set-candidate-window! pc
              'tutcode-candidate-window-auto-help)
            (im-activate-candidate-selector pc
              (length auto-help) tutcode-nr-candidate-max-for-kigou-mode)))))))

;;; 自動ヘルプの表形式表示に使うalistを更新する。
;;; alistは以下のように打鍵を示すラベル文字と、該当セルに表示する文字列のリスト
;;;  例:(("y" "2" "1") ("t" "3")) ; ("y" "y" "t")というストロークを表す。
;;;  ・・・・    ・・・・
;;;  ・・・・3 12・・・・
;;;  ・・・・    ・・・・
;;;  ・・・・    ・・・・
;;; @param label-cands-alist 元のalist
;;; @param kanji-list ヘルプ表示対象である、確定された漢字
;;; @param cand-list ヘルプ表示に使う、各打鍵を示す文字のリスト
;;; @return 更新後の自動ヘルプ用alist
(define (tutcode-auto-help-update-stroke-alist pc label-cands-alist
         cand-list kanji-list)
  (if (or (null? cand-list) (null? kanji-list))
    label-cands-alist
    (tutcode-auto-help-update-stroke-alist
      pc
      (tutcode-auto-help-update-stroke-alist-with-kanji
        pc label-cands-alist (car cand-list) (car kanji-list))
      (cdr cand-list) (cdr kanji-list))))

;;; 自動ヘルプの通常形式表示に使うalistを更新する。
;;; alistは以下のように文字と、文字を入力するためのキーのリスト(逆順)
;;;  例:(("暗" "t" "y" "y"))
;;; @param label-cands-alist 元のalist
;;; @param kanji-list ヘルプ表示対象である、確定された漢字
;;; @return 更新後の自動ヘルプ用alist
(define (tutcode-auto-help-update-stroke-alist-normal pc label-cands-alist
         kanji-list)
  (if (null? kanji-list)
    label-cands-alist
    (tutcode-auto-help-update-stroke-alist-normal
      pc
      (tutcode-auto-help-update-stroke-alist-normal-with-kanji
        pc label-cands-alist (car kanji-list))
      (cdr kanji-list))))

;;; 自動ヘルプ:対象の1文字を入力するストロークをヘルプ用alistに追加する。
;;; @param label-cands-alist 元のalist
;;; @param cand-list ヘルプ表示に使う、各打鍵を示す文字のリスト
;;; @param kanji ヘルプ表示対象文字
;;; @return 更新後の自動ヘルプ用alist
(define (tutcode-auto-help-update-stroke-alist-with-kanji pc label-cands-alist
         cand-list kanji)
  (let*
    ((rule (rk-context-rule (tutcode-context-rk-context pc)))
     (stroke (tutcode-reverse-find-seq kanji rule)))
    (if stroke
      (begin
        (tutcode-stroke-help-guide-add-kanji
          pc (list (list stroke) (list kanji)))
        (tutcode-auto-help-update-stroke-alist-with-stroke
          label-cands-alist
          (cons (string-append (caar cand-list) "(" kanji ")") (cdar cand-list))
          stroke))
      (let ((decomposed (tutcode-auto-help-bushu-decompose kanji rule)))
        ;; 例: "繋" => (((("," "o"))("撃")) ((("f" "q"))("糸")))
        (if (not decomposed)
          label-cands-alist
          (begin
            (tutcode-stroke-help-guide-add-kanji pc (car decomposed))
            (tutcode-stroke-help-guide-add-kanji pc (cadr decomposed))
            (tutcode-auto-help-update-stroke-alist-with-stroke
              (tutcode-auto-help-update-stroke-alist-with-stroke
                label-cands-alist
                (cons
                  (string-append (caar cand-list) "(" kanji "▲"
                    (caar (cdar decomposed)) (caar (cdadr decomposed)) ")")
                  (cdar cand-list))
                (caaar decomposed)) ; 部首1
              (cadr cand-list) (caaadr decomposed)))))))) ; 部首2

;;; 自動ヘルプ:対象の1文字を入力するストロークをヘルプ用alistに追加する。
;;; @param label-cands-alist 元のalist
;;; @param kanji ヘルプ表示対象文字
;;; @return 更新後の自動ヘルプ用alist
(define (tutcode-auto-help-update-stroke-alist-normal-with-kanji
          pc label-cands-alist kanji)
  (let*
    ((rule (rk-context-rule (tutcode-context-rk-context pc)))
     (stroke (tutcode-reverse-find-seq kanji rule)))
    (if stroke
      (begin
        (tutcode-stroke-help-guide-add-kanji
          pc (list (list stroke) (list kanji)))
        (tutcode-auto-help-update-stroke-alist-normal-with-stroke
          label-cands-alist
          (cons (string-append kanji " ") stroke)
          kanji))
      (let ((decomposed (tutcode-auto-help-bushu-decompose kanji rule)))
        ;; 例: "繋" => (((("," "o"))("撃")) ((("f" "q"))("糸")))
        (if (not decomposed)
          label-cands-alist
          (begin
            (tutcode-stroke-help-guide-add-kanji pc (car decomposed))
            (tutcode-stroke-help-guide-add-kanji pc (cadr decomposed))
            (tutcode-auto-help-update-stroke-alist-normal-with-stroke
              label-cands-alist
              (cons
                (string-append kanji "▲"
                  (caar (cdar decomposed)) (caar (cdadr decomposed)) " ")
                (append
                  (caaar decomposed)    ; 部首1
                  (list " ")
                  (caaadr decomposed))) ; 部首2
              kanji)))))))

;;; 自動ヘルプ:対象のストローク(キーのリスト)をヘルプ用alistに追加する。
;;; @param label-cands-alist 元のalist
;;; @param cand-list ヘルプ表示に使う、各打鍵を示す文字のリスト
;;; @param stroke 対象ストローク
;;; @return 更新後の自動ヘルプ用alist
(define (tutcode-auto-help-update-stroke-alist-with-stroke label-cands-alist
         cand-list stroke)
  (if (or (null? cand-list) (null? stroke))
    label-cands-alist
    (tutcode-auto-help-update-stroke-alist-with-stroke
      (tutcode-auto-help-update-stroke-alist-with-key
        label-cands-alist
        (if (pair? cand-list) (car cand-list) "")
        (car stroke))
      (cdr cand-list) (cdr stroke))))

;;; 自動ヘルプ:対象のストローク(キーのリスト)をヘルプ用alistに追加する。
;;; @param label-cands-alist 元のalist
;;; @param stroke 対象ストローク
;;; @param label 確定された漢字
;;; @return 更新後の自動ヘルプ用alist
(define (tutcode-auto-help-update-stroke-alist-normal-with-stroke
          label-cands-alist stroke label)
  (let ((label-cand (assoc label label-cands-alist)))
    (if (not label-cand)
      (cons (cons label (reverse stroke)) label-cands-alist))))

;;; 自動ヘルプ:対象のキーをヘルプ用alistに追加する。
;;; @param label-cands-alist 元のalist
;;; @param cand ヘルプ表示に使う、対象キーを示す文字
;;; @param key 対象キー
;;; @return 更新後の自動ヘルプ用alist
(define (tutcode-auto-help-update-stroke-alist-with-key label-cands-alist
         cand key)
  (let*
    ((label key)
     (label-cand (assoc label label-cands-alist)))
    (if label-cand
      (begin
        (set-cdr! label-cand (cons cand (cdr label-cand)))
        label-cands-alist)
      (cons (list label cand) label-cands-alist))))

;;; 自動ヘルプ:直近の自動ヘルプを再表示する
(define (tutcode-auto-help-redisplay pc)
  (let ((help (tutcode-context-auto-help pc)))
    (if (and help (> (length help) 0))
      (begin
        (tutcode-context-set-candidate-window! pc
          'tutcode-candidate-window-auto-help)
        (im-activate-candidate-selector pc
          (length help)
          tutcode-nr-candidate-max-for-kigou-mode)))))

;;; preedit表示を更新する。
(define (tutcode-do-update-preedit pc)
  (let ((stat (tutcode-context-state pc))
        (cpc (tutcode-context-child-context pc))
        (cursor-shown? #f))
    (case stat
      ((tutcode-state-yomi)
        (im-pushback-preedit pc preedit-none "△")
        (let ((h (tutcode-make-string (tutcode-context-head pc))))
          (if (string? h)
            (im-pushback-preedit pc preedit-none h))))
      ((tutcode-state-converting)
        (im-pushback-preedit pc preedit-none "△")
        (if (null? cpc)
          (begin
            (im-pushback-preedit pc preedit-none
              (tutcode-get-current-candidate pc))
            (let ((suffix (tutcode-context-mazegaki-suffix pc))) ; 活用語尾
              (if (pair? suffix)
                (begin
                  (im-pushback-preedit pc preedit-cursor "")
                  (set! cursor-shown? #t)
                  (im-pushback-preedit pc preedit-none
                    (tutcode-make-string suffix))))))
          ;; child context's preedit
          (let ((h (tutcode-make-string (tutcode-context-head pc)))
                (editor (tutcode-context-editor pc))
                (dialog (tutcode-context-dialog pc)))
            (if (string? h)
              (im-pushback-preedit pc preedit-none h))
            (im-pushback-preedit pc preedit-none "【")
            (im-pushback-preedit pc preedit-none
              (if (eq? (tutcode-context-child-type pc)
                    'tutcode-child-type-editor)
                (tutcode-editor-get-left-string editor)
                (tutcode-dialog-get-left-string dialog)))
            (tutcode-do-update-preedit cpc)
            (set! cursor-shown? #t)
            (im-pushback-preedit pc preedit-none
              (if (eq? (tutcode-context-child-type pc)
                    'tutcode-child-type-editor)
                (tutcode-editor-get-right-string editor)
                (tutcode-dialog-get-right-string dialog)))
            (im-pushback-preedit pc preedit-none "】"))))
      ;; 部首合成変換のマーカ▲は文字列としてhead内で管理(再帰的部首合成のため)
      ((tutcode-state-bushu)
        (let ((h (tutcode-make-string (tutcode-context-head pc))))
          (if (string? h)
            (im-pushback-preedit pc preedit-none h))))
      ((tutcode-state-interactive-bushu)
        (im-pushback-preedit pc preedit-none "▼")
        (let ((h (tutcode-make-string (tutcode-context-head pc))))
          (if (string? h)
            (im-pushback-preedit pc preedit-none h)))
        (im-pushback-preedit pc preedit-cursor "")
        (set! cursor-shown? #t)
        (if (> (tutcode-lib-get-nr-predictions pc) 0)
          (begin
            (im-pushback-preedit pc preedit-underline "=>")
            (im-pushback-preedit pc preedit-underline
              (tutcode-get-prediction-string pc)))))
      ((tutcode-state-kigou)
        ;; 候補ウィンドウ非表示時でも候補選択できるようにpreedit表示
        (im-pushback-preedit pc preedit-reverse
          (tutcode-get-current-candidate-for-kigou-mode pc))))
    (if (not cursor-shown?)
      (im-pushback-preedit pc preedit-cursor ""))))

;;; preedit表示を更新する。
(define (tutcode-update-preedit pc)
  (im-clear-preedit pc)
  (tutcode-do-update-preedit (tutcode-find-root-context pc))
  (im-update-preedit pc))

;; called from tutcode-editor
;;; tutcode-editor側での編集完了時に呼ばれる。
;;; @param str エディタ側で確定された文字列
(define (tutcode-commit-editor-context pc str)
  (let ((yomi-len (tutcode-context-postfix-yomi-len pc))
        (suffix (tutcode-context-mazegaki-suffix pc)))
    (if (> yomi-len 0)
      (tutcode-postfix-delete-text pc yomi-len))
    (tutcode-flush pc)
    (tutcode-context-set-child-context! pc ())
    (tutcode-context-set-child-type! pc ())
    (tutcode-commit pc
      (if (null? suffix)
        str
        (string-append str (tutcode-make-string suffix))))
    (tutcode-update-preedit pc)))

;;; 補完候補を検索して候補ウィンドウに表示する
;;; @param force-check? 必ず検索を行うかどうか。
;;;  #fの場合は文字数が設定値未満の場合は検索しない。
;;; @param num commit-strsから検索対象にする文字数。0の場合は全て。
(define (tutcode-check-completion pc force-check? num)
  (tutcode-context-set-commit-strs-used-len! pc 0)
  (if (eq? (tutcode-context-predicting pc) 'tutcode-predicting-off)
    (let* ((commit-strs-all (tutcode-context-commit-strs pc))
           (commit-strs
            (if (> num 0)
              (take commit-strs-all num)
              commit-strs-all))
           (len (length commit-strs)))
      (if
        (or (>= len tutcode-completion-chars-min)
            (and force-check?
                 (> len 0)))
        (let ((str (tutcode-make-string commit-strs)))
          (tutcode-lib-set-prediction-src-string pc str #t)
          (let ((nr (tutcode-lib-get-nr-predictions pc)))
            (if (and nr (> nr 0))
              (let*
                ((nr-guide
                  (if tutcode-use-kanji-combination-guide?
                    (begin
                      (tutcode-guide-set-candidates pc str #t ())
                      (length (tutcode-context-guide pc)))
                    0))
                 (res (tutcode-prediction-calc-window-param nr nr-guide))
                 (nr-all (list-ref res 0)) ; 全候補数(補完候補+熟語ガイド)
                 (page-limit (list-ref res 1)) ; ページ内候補数(補完+熟語)
                 (nr-in-page (list-ref res 2))) ; ページ内候補数(補完候補のみ)
                (if (> page-limit 0)
                  (begin
                    (tutcode-context-set-commit-strs-used-len! pc len)
                    (tutcode-context-set-prediction-nr-in-page! pc nr-in-page)
                    (tutcode-context-set-prediction-page-limit! pc page-limit)
                    (tutcode-context-set-prediction-nr-all! pc nr-all)
                    (tutcode-context-set-prediction-index! pc 0)
                    (tutcode-context-set-candidate-window! pc
                      'tutcode-candidate-window-predicting)
                    (tutcode-context-set-predicting! pc
                      'tutcode-predicting-completion)
                    (im-activate-candidate-selector pc nr-all page-limit))))
              ;; 補完候補が見つからない場合、1文字削った文字列を使って再検索
              ;; (直接tutcode-context-set-commit-strs!で文字を削ると、
              ;;  間違った文字を入力してBackspaceで消したときに、
              ;;  以前入力した文字列が削られているため、期待した補完にならない
              ;;  恐れあり。速度的には、直接削る方が速いけど)
              (if (> len 1)
                (tutcode-check-completion pc force-check? (- len 1))))))))))

;;; 交ぜ書き変換中に予測入力候補を検索して候補ウィンドウに表示する
;;; @param force-check? 必ず検索を行うかどうか。
;;;  #fの場合は文字数が設定値未満の場合は検索しない。
(define (tutcode-check-prediction pc force-check?)
  (if (eq? (tutcode-context-predicting pc) 'tutcode-predicting-off)
    (let* ((head (tutcode-context-head pc))
           (preedit-len (length head)))
      (if
        (or (>= preedit-len tutcode-prediction-start-char-count)
            force-check?)
        (let*
          ((preconv-str (tutcode-make-string head))
           (all-yomi (tutcode-lib-set-prediction-src-string pc preconv-str #f))
           (nr (tutcode-lib-get-nr-predictions pc)))
          (if (and nr (> nr 0))
            (let*
              ((nr-guide
                (if tutcode-use-kanji-combination-guide?
                  (begin
                    (tutcode-guide-set-candidates pc preconv-str #f all-yomi)
                    (length (tutcode-context-guide pc)))
                  0))
               (res (tutcode-prediction-calc-window-param nr nr-guide))
               (nr-all (list-ref res 0)) ; 全候補数(予測候補+熟語ガイド)
               (page-limit (list-ref res 1)) ; ページ内候補数(予測+熟語)
               (nr-in-page (list-ref res 2))) ; ページ内候補数(予測候補のみ)
              (if (> page-limit 0)
                (begin
                  (tutcode-context-set-prediction-nr-in-page! pc nr-in-page)
                  (tutcode-context-set-prediction-page-limit! pc page-limit)
                  (tutcode-context-set-prediction-nr-all! pc nr-all)
                  (tutcode-context-set-prediction-index! pc 0)
                  (tutcode-context-set-candidate-window! pc
                    'tutcode-candidate-window-predicting)
                  (tutcode-context-set-predicting! pc
                    'tutcode-predicting-prediction)
                  (im-activate-candidate-selector pc nr-all page-limit))))))
        (tutcode-reset-candidate-window pc)))))

;;; 部首合成変換中に予測入力候補を検索して候補ウィンドウに表示する
;;; @param char 入力された部首1
(define (tutcode-check-bushu-prediction pc char)
  (if (eq? (tutcode-context-predicting pc) 'tutcode-predicting-off)
    (let* ((res (tutcode-bushu-predict char tutcode-bushudic))
           (alt (assoc char tutcode-bushudic-altchar))
           (altres
            (if alt
              (tutcode-bushu-predict (cadr alt) tutcode-bushudic)
              ()))
           (resall (append res altres)))
      (tutcode-context-set-prediction-bushu! pc resall)
      (tutcode-bushu-prediction-show-page pc 0))))

;;; 部首合成変換の予測入力候補のうち、指定された番号から始まる候補を表示する。
;;; @param start-index 開始番号
(define (tutcode-bushu-prediction-show-page pc start-index)
  (tutcode-lib-set-bushu-prediction pc start-index)
  (let ((nr (tutcode-lib-get-nr-predictions pc)))
    (if (and nr (> nr 0))
      (let*
        ((nr-guide
          (if tutcode-use-kanji-combination-guide?
            (begin
              (tutcode-guide-set-candidates-for-bushu pc)
              (length (tutcode-context-guide pc)))
            0))
         (res (tutcode-prediction-calc-window-param nr nr-guide))
         (nr-all (list-ref res 0)) ; 全候補数(予測候補+熟語ガイド)
         (page-limit (list-ref res 1)) ; ページ内候補数(予測+熟語)
         (nr-in-page (list-ref res 2))) ; ページ内候補数(予測候補のみ)
        (if (> page-limit 0)
          (begin
            (tutcode-context-set-prediction-nr-in-page! pc nr-in-page)
            (tutcode-context-set-prediction-page-limit! pc page-limit)
            (tutcode-context-set-prediction-nr-all! pc nr-all)
            (tutcode-context-set-prediction-index! pc 0)
            (tutcode-context-set-candidate-window! pc
              'tutcode-candidate-window-predicting)
            (tutcode-context-set-predicting! pc 'tutcode-predicting-bushu)
            (im-activate-candidate-selector pc nr-all page-limit)))))))

;;; 補完候補と熟語ガイド表示のためのcandwin用パラメータを計算する
;;; @param nr 補完候補数
;;; @param nr-guide 熟語ガイド候補数
;;; @return (<全候補数> <ページごとの候補数上限> <ページごとの補完候補数上限>)
(define (tutcode-prediction-calc-window-param nr nr-guide)
  ;; XXX:表形式候補ウィンドウ用display_limitの調整前だと、計算に使う
  ;;     tutcode-nr-candidate-max-for-guide等が適切な値になっていない恐れあり。
  (cond
    ;; 1ページに収まる場合
    ((and (<= nr-guide tutcode-nr-candidate-max-for-guide)
          (<= nr tutcode-nr-candidate-max-for-prediction))
      (list (+ nr-guide nr) (+ nr-guide nr) nr))
    ;; 補完候補が1ページに収まらない場合
    ((and (<= nr-guide tutcode-nr-candidate-max-for-guide)
          (> nr tutcode-nr-candidate-max-for-prediction))
      (if (= 0 tutcode-nr-candidate-max-for-prediction)
        (list nr-guide nr-guide 0) ; 補完候補は表示しない
        (let*
          ((nr-page
            ;; ページ内候補数は一定にしないと面倒なので、
            ;; 各ページに同じ熟語ガイドを表示。
            ;; ただし、余りのページには表示しない。
            ;; (各ページ内でのindexがnr-candidate-max-for-prediction未満の
            ;;  候補を補完/予測入力候補、以上の候補を熟語ガイドとして扱うので
            ;;  それに満たない補完候補数しかない余りのページでの表示管理が面倒)
            (quotient nr tutcode-nr-candidate-max-for-prediction))
           (page-limit (+ nr-guide tutcode-nr-candidate-max-for-prediction))
           (nr-all (+ nr (* nr-guide nr-page))))
          (list nr-all page-limit tutcode-nr-candidate-max-for-prediction))))
    ;; 熟語ガイドが1ページに収まらない場合
    ((and (> nr-guide tutcode-nr-candidate-max-for-guide)
          (<= nr tutcode-nr-candidate-max-for-prediction))
      (if (= 0 tutcode-nr-candidate-max-for-guide)
        (list nr nr nr) ; 熟語ガイドは表示しない
        (let*
          ((nr-page
            (+ 
              (quotient nr-guide tutcode-nr-candidate-max-for-guide)
              (if (= 0 (remainder nr-guide tutcode-nr-candidate-max-for-guide))
                0
                1)))
           (page-limit (+ nr tutcode-nr-candidate-max-for-guide))
           (nr-all (+ nr-guide (* nr nr-page))))
          (list nr-all page-limit nr))))
    ;; 補完候補と熟語ガイド両方とも1ページに収まらない場合
    (else
      (cond
        ;; 熟語ガイドのみ表示
        ((= 0 tutcode-nr-candidate-max-for-prediction)
          (list nr-guide tutcode-nr-candidate-max-for-guide 0))
        ;; 補完候補のみ表示
        ((= 0 tutcode-nr-candidate-max-for-guide)
          (list nr tutcode-nr-candidate-max-for-prediction
            tutcode-nr-candidate-max-for-prediction))
        (else
          (let*
            ((nr-page-prediction
              (quotient nr tutcode-nr-candidate-max-for-prediction))
             (nr-page-guide
              (+
                (quotient nr-guide tutcode-nr-candidate-max-for-guide)
                (if (= 0 (remainder nr-guide tutcode-nr-candidate-max-for-guide))
                  0
                  1)))
             (nr-page (max nr-page-prediction nr-page-guide))
             (page-limit (+ tutcode-nr-candidate-max-for-guide
              tutcode-nr-candidate-max-for-prediction))
             (nr-all 
              (if (> nr-page-prediction nr-page-guide)
                (+ nr (* nr-page tutcode-nr-candidate-max-for-guide))
                (+ nr-guide (* nr-page tutcode-nr-candidate-max-for-prediction)))))
            (list nr-all page-limit tutcode-nr-candidate-max-for-prediction)))))))

;;; TUT-Code入力状態のときのキー入力を処理する。
;;; @param c コンテキストリスト
;;; @param key 入力されたキー
;;; @param key-state コントロールキー等の状態
(define (tutcode-proc-state-on c key key-state)
  (let*
    ((pc (tutcode-find-descendant-context c))
     (rkc (tutcode-context-rk-context pc))
     ;; reset-candidate-windowでリセットされるので保存しておく
     (completing?
      (eq? (tutcode-context-predicting pc) 'tutcode-predicting-completion))
     ;; 補完候補表示のページ移動時は、reset-candidate-windowしたら駄目
     (prediction-keys-handled?
      (if completing?
        (cond
          ((tutcode-next-page-key? key key-state)
            (tutcode-change-prediction-page pc #t)
            #t)
          ((tutcode-prev-page-key? key key-state)
            (tutcode-change-prediction-page pc #f)
            #t)
          (else
            #f))
        #f)))
    (if (not prediction-keys-handled?)
      (begin
        (tutcode-reset-candidate-window pc)
        (cond
          ((and
            (tutcode-vi-escape-key? key key-state)
            tutcode-use-with-vi?)
           (rk-flush rkc)
           (tutcode-context-set-commit-strs! pc ())
           (tutcode-context-set-state! pc 'tutcode-state-off)
           (tutcode-commit-raw pc key key-state)) ; ESCキーをアプリにも渡す
          ((tutcode-off-key? key key-state)
           (rk-flush rkc)
           (tutcode-context-set-commit-strs! pc ())
           (tutcode-context-set-state! pc 'tutcode-state-off))
          ((tutcode-kigou-toggle-key? key key-state)
           (rk-flush rkc)
           (tutcode-begin-kigou-mode pc))
          ((tutcode-kigou2-toggle-key? key key-state)
           (rk-flush rkc)
           (tutcode-toggle-kigou2-mode pc))
          ((and (tutcode-kana-toggle-key? key key-state)
                (not (tutcode-kigou2-mode? pc)))
           (rk-flush rkc)
           (tutcode-context-kana-toggle pc))
          ((tutcode-backspace-key? key key-state)
           (if (> (length (rk-context-seq rkc)) 0)
             (rk-flush rkc)
             (begin
               (tutcode-commit-raw pc key key-state)
               (if (and (or tutcode-use-completion?
                            tutcode-enable-fallback-surrounding-text?)
                        (pair? (tutcode-context-commit-strs pc)))
                 (tutcode-context-set-commit-strs! pc
                     (cdr (tutcode-context-commit-strs pc))))
               (if (and tutcode-use-completion?
                        completing?
                        (> tutcode-completion-chars-min 0))
                 (tutcode-check-completion pc #f 0)))))
          ((tutcode-stroke-help-toggle-key? key key-state)
           (tutcode-toggle-stroke-help pc))
          ((and tutcode-use-completion?
                (tutcode-begin-completion-key? key key-state))
           (rk-flush rkc)
           (if completing?
             ;; 補完中にbegin-completin-keyが押されたら対象文字を1減らして再補完
             ;; (意図しない文字列で補完された場合に、補完し直しができるように)
             ;; 対象が1文字未満になる場合は補完ウィンドウを閉じる(再表示しない)
             (let ((len (tutcode-context-commit-strs-used-len pc)))
               (if (> len 1)
                 (tutcode-check-completion pc #t (- len 1))))
             (tutcode-check-completion pc #t 0)))
          ((or
            (symbol? key)
            (and
              (modifier-key-mask key-state)
              (not (shift-key-mask key-state))))
           (rk-flush rkc)
           (tutcode-commit-raw pc key key-state))
          ;; 補完候補用ラベルキー?
          ((and completing? (tutcode-heading-label-char-for-prediction? key))
            (tutcode-commit-by-label-key-for-prediction pc
              (charcode->string key) 'tutcode-predicting-completion))
          ;; 正しくないキーシーケンスは全て捨てる(tc2に合わせた動作)。
          ;; (rk-push-key!すると、途中までのシーケンスは捨てられるが、
          ;; 間違ったキーは残ってしまうので、rk-push-key!は使えない)
          ((not (rk-expect-key? rkc (charcode->string key)))
           (if (> (length (rk-context-seq rkc)) 0)
             (rk-flush rkc) ; 正しくないシーケンスは捨てる
             ;; 単独のキー入力(TUT-Code入力でなくて)
             (tutcode-commit-raw pc key key-state)))
          (else
           (let ((res (tutcode-push-key! pc (charcode->string key))))
            (cond
              ((string? res)
                (tutcode-commit pc res)
                (if (and tutcode-use-completion?
                         (> tutcode-completion-chars-min 0))
                  (tutcode-check-completion pc #f 0)))
              ((eq? res 'tutcode-mazegaki-start)
                (tutcode-context-set-latin-conv! pc #f)
                (tutcode-context-set-postfix-yomi-len! pc 0)
                (tutcode-context-set-state! pc 'tutcode-state-yomi))
              ((eq? res 'tutcode-latin-conv-start)
                (tutcode-context-set-latin-conv! pc #t)
                (tutcode-context-set-postfix-yomi-len! pc 0)
                (tutcode-context-set-state! pc 'tutcode-state-yomi))
              ((eq? res 'tutcode-bushu-start)
                (tutcode-context-set-state! pc 'tutcode-state-bushu)
                (tutcode-append-string pc "▲"))
              ((eq? res 'tutcode-interactive-bushu-start)
                (tutcode-context-set-prediction-nr! pc 0)
                (tutcode-context-set-state! pc
                  'tutcode-state-interactive-bushu))
              ((eq? res 'tutcode-postfix-bushu-start)
                (tutcode-begin-postfix-bushu-conversion pc))
              ((eq? res 'tutcode-postfix-mazegaki-start)
                (tutcode-begin-postfix-mazegaki-conversion pc #f #f #f))
              ((eq? res 'tutcode-postfix-mazegaki-1-start)
                (tutcode-begin-postfix-mazegaki-conversion pc 1 #t
                  tutcode-use-recursive-learning?))
              ((eq? res 'tutcode-postfix-mazegaki-2-start)
                (tutcode-begin-postfix-mazegaki-conversion pc 2 #t
                  tutcode-use-recursive-learning?))
              ((eq? res 'tutcode-postfix-mazegaki-3-start)
                (tutcode-begin-postfix-mazegaki-conversion pc 3 #t
                  tutcode-use-recursive-learning?))
              ((eq? res 'tutcode-postfix-mazegaki-4-start)
                (tutcode-begin-postfix-mazegaki-conversion pc 4 #t
                  tutcode-use-recursive-learning?))
              ((eq? res 'tutcode-postfix-mazegaki-5-start)
                (tutcode-begin-postfix-mazegaki-conversion pc 5 #t
                  tutcode-use-recursive-learning?))
              ((eq? res 'tutcode-postfix-mazegaki-6-start)
                (tutcode-begin-postfix-mazegaki-conversion pc 6 #t
                  tutcode-use-recursive-learning?))
              ((eq? res 'tutcode-postfix-mazegaki-7-start)
                (tutcode-begin-postfix-mazegaki-conversion pc 7 #t
                  tutcode-use-recursive-learning?))
              ((eq? res 'tutcode-postfix-mazegaki-8-start)
                (tutcode-begin-postfix-mazegaki-conversion pc 8 #t
                  tutcode-use-recursive-learning?))
              ((eq? res 'tutcode-postfix-mazegaki-9-start)
                (tutcode-begin-postfix-mazegaki-conversion pc 9 #t
                  tutcode-use-recursive-learning?))
              ((eq? res 'tutcode-postfix-mazegaki-inflection-start)
                (tutcode-begin-postfix-mazegaki-inflection-conversion pc #f))
              ((eq? res 'tutcode-postfix-mazegaki-inflection-1-start)
                (tutcode-begin-postfix-mazegaki-inflection-conversion pc 1))
              ((eq? res 'tutcode-postfix-mazegaki-inflection-2-start)
                (tutcode-begin-postfix-mazegaki-inflection-conversion pc 2))
              ((eq? res 'tutcode-postfix-mazegaki-inflection-3-start)
                (tutcode-begin-postfix-mazegaki-inflection-conversion pc 3))
              ((eq? res 'tutcode-postfix-mazegaki-inflection-4-start)
                (tutcode-begin-postfix-mazegaki-inflection-conversion pc 4))
              ((eq? res 'tutcode-postfix-mazegaki-inflection-5-start)
                (tutcode-begin-postfix-mazegaki-inflection-conversion pc 5))
              ((eq? res 'tutcode-postfix-mazegaki-inflection-6-start)
                (tutcode-begin-postfix-mazegaki-inflection-conversion pc 6))
              ((eq? res 'tutcode-postfix-mazegaki-inflection-7-start)
                (tutcode-begin-postfix-mazegaki-inflection-conversion pc 7))
              ((eq? res 'tutcode-postfix-mazegaki-inflection-8-start)
                (tutcode-begin-postfix-mazegaki-inflection-conversion pc 8))
              ((eq? res 'tutcode-postfix-mazegaki-inflection-9-start)
                (tutcode-begin-postfix-mazegaki-inflection-conversion pc 9))
              ((eq? res 'tutcode-auto-help-redisplay)
                (tutcode-auto-help-redisplay pc))))))))))

;;; 後置型部首合成変換を行う
(define (tutcode-begin-postfix-bushu-conversion pc)
  (and-let*
    ((former-seq (tutcode-postfix-acquire-text pc 2))
     (res (and (>= (length former-seq) 2)
               (tutcode-bushu-convert (cadr former-seq) (car former-seq)))))
    (tutcode-postfix-delete-text pc 2)
    (tutcode-commit pc res)
    (tutcode-check-auto-help-window-begin pc (list res) ())))

;;; 後置型交ぜ書き変換を開始する
;;; @param yomi-len 指定された読みの文字数。指定されてない場合は#f。
;;; @param autocommit? 候補が1個の場合に自動的に確定するかどうか
;;;  (yomi-lenが#fでない場合に有効)
;;; @param recursive-learning? 候補が無い場合に再帰登録モードに入るかどうか
;;;  (yomi-lenが#fでない場合に有効)
;;; @return #t:候補が有った場合。#f:候補が無かった場合。
(define (tutcode-begin-postfix-mazegaki-conversion pc yomi-len autocommit?
          recursive-learning?)
  (tutcode-context-set-mazegaki-yomi-len-specified! pc (or yomi-len 0))
  (let*
    ((former-seq (tutcode-postfix-mazegaki-acquire-yomi pc yomi-len))
     (former-len (length former-seq)))
    (if yomi-len
      (and
        (>= former-len yomi-len)
        (let ((yomi (take former-seq yomi-len)))
          (tutcode-context-set-postfix-yomi-len! pc yomi-len)
          (if (> yomi-len (length (tutcode-context-mazegaki-yomi-all pc)))
            (tutcode-context-set-mazegaki-yomi-all! pc yomi))
          (tutcode-begin-conversion pc yomi () autocommit? recursive-learning?)))
      ;; 読みの文字数が指定されていない→読みを縮めながら変換
      (and
        (> former-len 0)
        (begin
          (tutcode-context-set-postfix-yomi-len! pc former-len)
          (tutcode-context-set-mazegaki-yomi-all! pc former-seq)
          (tutcode-mazegaki-relimit-right pc former-seq #f))))))

;;; 読みを縮めながら交ぜ書き変換を行う。
;;; 活用しない語として検索しても候補が見つからない場合は、
;;; 活用する語として検索を試みる。
;;; @param yomi 変換対象の読み(文字列の逆順リスト)
;;; @param relimit-first? (最初の辞書検索前に)先に読みを縮める
;;; @return #t:候補が有った場合。#f:候補が無かった場合。
(define (tutcode-mazegaki-relimit-right pc yomi relimit-first?)
  (or
    (and
      (not relimit-first?)
      (tutcode-begin-conversion pc yomi () #f #f))
    ;; 候補が見つからなかった
    (or
      (and
        (> (length yomi) 1)
        (> (tutcode-context-postfix-yomi-len pc) 0) ;前置型の場合は何もしない
        (begin ; 読みを1文字減らして再検索
          (tutcode-context-set-postfix-yomi-len! pc (- (length yomi) 1))
          (tutcode-mazegaki-relimit-right pc (drop-right yomi 1) #f)))
      (and tutcode-mazegaki-enable-inflection? ; 活用する語の検索に移行
        (not (tutcode-mazegaki-inflection? yomi)) ; 明示的―→―は重複させない
        (let*
          ((len-specified (tutcode-context-mazegaki-yomi-len-specified pc))
           (len
            (if (> len-specified 0)
              len-specified
              (length (tutcode-context-mazegaki-yomi-all pc)))))
          (tutcode-mazegaki-inflection-relimit-right pc len len #f))))))

;;; 読みを伸ばしながら後置型交ぜ書き変換を行う。
;;; @param yomi-len 検索する読みの長さ
;;; @return #t:候補が有った場合。#f:候補が無かった場合。
(define (tutcode-postfix-mazegaki-relimit-left pc yomi-len)
  (and
    (<= yomi-len tutcode-mazegaki-yomi-max)
    (let*
      ((yomi-all (tutcode-context-mazegaki-yomi-all pc))
       (yomi-all-len (length yomi-all)))
      (if (<= yomi-len yomi-all-len)
        (let ((yomi (take yomi-all yomi-len)))
          (tutcode-context-set-postfix-yomi-len! pc yomi-len)
          (or
            (tutcode-begin-conversion pc yomi () #f #f)
            (tutcode-postfix-mazegaki-relimit-left pc (+ yomi-len 1))))
        ;; 取得済の読みが足りなくなった場合、上限のyomi-max長の読みを取得
        (and
          (< yomi-all-len tutcode-mazegaki-yomi-max)
          (let ((former-seq (tutcode-postfix-mazegaki-acquire-yomi pc
                             tutcode-mazegaki-yomi-max)))
            (and
              (> (length former-seq) yomi-all-len)
              (begin
                (tutcode-context-set-mazegaki-yomi-all! pc former-seq)
                (tutcode-postfix-mazegaki-relimit-left pc yomi-len)))))))))

;;; 指定された読みが、活用する語かどうかを返す
;;; @param head 対象の読み
;;; @return #t:活用する語の場合。#f:それ以外の場合。
(define (tutcode-mazegaki-inflection? head)
  (and
    (pair? head)
    (string=? "―" (car head))))

;;; 活用する語として後置型交ぜ書き変換を開始する
;;; @param yomi-len 指定された読みの文字数。指定されてない場合は#f。
;;; @return #t:候補が有った場合。#f:候補が無かった場合。
(define (tutcode-begin-postfix-mazegaki-inflection-conversion pc yomi-len)
  (tutcode-context-set-mazegaki-yomi-len-specified! pc (or yomi-len 0))
  (let*
    ((former-seq (tutcode-postfix-mazegaki-acquire-yomi pc yomi-len))
     (former-len (length former-seq)))
    (if yomi-len
      (and
        (>= former-len yomi-len)
        (let ((yomi (take former-seq yomi-len)))
          (tutcode-context-set-postfix-yomi-len! pc yomi-len)
          (tutcode-context-set-mazegaki-yomi-all! pc yomi)
          (if (tutcode-mazegaki-inflection? yomi)
            ;; 明示的に"―"付きで入力された場合、活用しない語として検索する
            ;; (活用する語として取り扱う場合は、"―"の位置を調整しながら
            ;;  検索する形になるが、明示的に指定されている場合は位置調整不要)
            (tutcode-begin-conversion pc yomi () #t
              tutcode-use-recursive-learning?)
            (tutcode-mazegaki-inflection-relimit-right pc
              yomi-len yomi-len #f))))
      ;; 読みの文字数が指定されていない→読み/語幹を縮めながら変換
      (and
        (> former-len 0)
        ;; 語幹の長いものを優先して変換
        (begin
          (tutcode-context-set-postfix-yomi-len! pc former-len)
          (tutcode-context-set-mazegaki-yomi-all! pc former-seq)
          (if (tutcode-mazegaki-inflection? former-seq) ; 明示的"―"
            (tutcode-mazegaki-relimit-right pc former-seq #f)
            (tutcode-mazegaki-inflection-relimit-right pc
              former-len former-len #f)))))))

;;; 活用する語の交ぜ書き変換のため、
;;; 読み/語幹を縮めながら、語幹が最長となる読みを見つけて変換を行う。
;;; @param yomi-cur-len yomi-allのうちで現在変換対象となっている読みの長さ
;;; @param len 検索対象とする語幹の長さ
;;; @param relimit-first? (最初の辞書検索前に)先に読みを縮めるかどうか
;;; @return #t:候補が有った場合。#f:候補が無かった場合。
(define (tutcode-mazegaki-inflection-relimit-right pc yomi-cur-len len
          relimit-first?)
  (and
    (> len 0)
    (let*
      ((yomi-all (tutcode-context-mazegaki-yomi-all pc))
       (len-specified (tutcode-context-mazegaki-yomi-len-specified pc)))
      (or
        ;; 語幹の長さ(len=length head)は保持したまま、読みを縮めながら検索
        (let loop
          ((yomi-cur (take yomi-all yomi-cur-len))
           (skip-search? relimit-first?))
          (let* ((yomi-len (length yomi-cur))
                 (suffix-len (- yomi-len len)))
            (and
              (>= suffix-len 0)
              (or
                (and
                  (not skip-search?)
                  (<= suffix-len tutcode-mazegaki-suffix-max)
                  (receive (suffix head) (split-at yomi-cur suffix-len)
                    (if (> (tutcode-context-postfix-yomi-len pc) 0) ; 後置型?
                      (tutcode-context-set-postfix-yomi-len! pc yomi-len))
                    (tutcode-begin-conversion pc (cons "―" head) suffix #f #f)))
                (and
                  (= len-specified 0)
                  ;; 読みを1文字縮めて検索
                  (loop (drop-right yomi-cur 1) #f))))))
        ;; 語幹を1文字縮めて検索
        (tutcode-mazegaki-inflection-relimit-right pc
          (if (> len-specified 0)
            len-specified
            (length yomi-all))
          (- len 1) #f)))))

;;; 活用する語の交ぜ書き変換のため、
;;; 読み/語幹を伸ばしながら、語幹が最長となる読みを見つけて変換を行う。
;;; @param yomi-cur-len yomi-allのうちで現在変換対象となっている読みの長さ
;;; @param len 検索対象とする語幹の長さ
;;; @param relimit-first? (最初の辞書検索前に)先に読みを伸ばす
;;; @return #t:候補が有った場合。#f:候補が無かった場合。
(define (tutcode-mazegaki-inflection-relimit-left pc yomi-cur-len len
          relimit-first?)
  (let*
    ((yomi-all (tutcode-context-mazegaki-yomi-all pc))
     (yomi-all-len (length yomi-all))
     (len-specified (tutcode-context-mazegaki-yomi-len-specified pc)))
    (or
      (and
        (<= len yomi-all-len)
        (or
          (let loop
            ((yomi-len yomi-cur-len)
             (skip-search? relimit-first?))
            ;; 語幹の長さ(len=length head)は保持したまま読みを伸ばしながら検索
            (and
              (<= len yomi-len yomi-all-len)
              (or
                (and
                  (not skip-search?)
                  (<= (- yomi-len len) tutcode-mazegaki-suffix-max)
                  (receive (suffix head)
                           (split-at (take yomi-all yomi-len) (- yomi-len len))
                    (if (> (tutcode-context-postfix-yomi-len pc) 0) ; 後置型?
                      (tutcode-context-set-postfix-yomi-len! pc yomi-len))
                    (tutcode-begin-conversion pc
                      (cons "―" head) suffix #f #f)))
                ;; 読みを1文字伸ばして検索
                (and
                  (= len-specified 0)
                  (loop (+ yomi-len 1) #f)))))
          ;; 語幹を1文字伸ばして検索
          (tutcode-mazegaki-inflection-relimit-left pc
            (if (> len-specified 0)
              yomi-cur-len
              (+ len 1)) ; 伸長後の語幹を持てる読みの最短長
            (+ len 1) #f)))
      ;; さらに伸ばす場合は、活用しない語の検索に移行
      (if (> (tutcode-context-postfix-yomi-len pc) 0) ; 後置型?
        (let ((len-new (if (> len-specified 0) len-specified 1)))
          (tutcode-postfix-mazegaki-relimit-left pc len-new))
        ;; 前置型
        (tutcode-begin-conversion pc yomi-all () #f #f)))))

;;; 交ぜ書き変換中のrelimit-rightキー入力時の処理を行う:
;;; 読み/語幹を縮めて再検索
(define (tutcode-mazegaki-proc-relimit-right pc)
  (tutcode-reset-candidate-window pc)
  (let*
    ((head (tutcode-context-head pc))
     (head-len (length head))
     (postfix-yomi-len (tutcode-context-postfix-yomi-len pc))
     (yomi-all (tutcode-context-mazegaki-yomi-all pc))
     (inflection?
      (and (tutcode-mazegaki-inflection? head)
           (not (tutcode-mazegaki-inflection? yomi-all)))) ; 明示的"―"
     (found?
      (if (not inflection?)
        (tutcode-mazegaki-relimit-right pc head #t)
        (tutcode-mazegaki-inflection-relimit-right pc
          (+ (- head-len 1)
             (length (tutcode-context-mazegaki-suffix pc)))
          (- head-len 1) #t)))) ; (car head)は"―"
    (if (not found?) ; 候補無し→読み/語幹を縮めるのは中止
      (tutcode-context-set-postfix-yomi-len! pc postfix-yomi-len))))

;;; 交ぜ書き変換中のrelimit-leftキー入力時の処理を行う:
;;; 読み/語幹を伸ばして再検索
(define (tutcode-mazegaki-proc-relimit-left pc)
  (tutcode-reset-candidate-window pc)
  (let*
    ((head (tutcode-context-head pc))
     (head-len (length head))
     (postfix-yomi-len (tutcode-context-postfix-yomi-len pc))
     (yomi-all (tutcode-context-mazegaki-yomi-all pc))
     (inflection?
      (and (tutcode-mazegaki-inflection? head)
           (not (tutcode-mazegaki-inflection? yomi-all)))) ; 明示的"―"
     (found?
      (if (not inflection?)
        (and (> postfix-yomi-len 0) ; 後置型の場合は読みを伸ばす
             (tutcode-postfix-mazegaki-relimit-left pc (+ head-len 1)))
        (tutcode-mazegaki-inflection-relimit-left pc
          (+ (- head-len 1)
             (length (tutcode-context-mazegaki-suffix pc)))
          (- head-len 1) #t)))) ; (car head)は"―"
    (if (not found?) ; 候補無し→読み/語幹を伸ばすのは中止
      (tutcode-context-set-postfix-yomi-len! pc postfix-yomi-len))))

;;; 後置型交ぜ書き変換用の読みを取得する
;;; @param yomi-len 指定された読みの文字数。指定されてない場合は#f。
;;; @return 取得した読み(文字列の逆順リスト)
(define (tutcode-postfix-mazegaki-acquire-yomi pc yomi-len)
  (let ((former-seq (tutcode-postfix-acquire-text pc
                     (or yomi-len tutcode-mazegaki-yomi-max))))
    (if yomi-len
      ;; XXX:読みの文字数が指定されている場合は"。"等も含める。relimit-left
      ;;     経由の場合もユーザが明示的に指定したものとみなして同様に含める。
      former-seq
      ;; 読みの文字数が指定されていない→取得できた文字を使用(上限yomi-max)。
      (let*
        ;; 日本語文字とASCII文字の境目があれば、そこまでを取得する
        ((ascii?
          (lambda (str)
            (let ((ch (string->ichar str)))
              (and ch (<= ch 127)))))
         (last-ascii? (and (pair? former-seq) (ascii? (car former-seq)))))
        (take-while
          (lambda (elem)
            (and
              (eq? (ascii? elem) last-ascii?)
              ;; "、"や"。"以前の文字は読みに含めない。
              (not (member elem tutcode-postfix-mazegaki-terminate-char-list))))
          former-seq)))))

;;; 確定済文字列を取得する
;;; @param len 取得する文字数
;;; @return 取得した文字列のリスト(逆順)
(define (tutcode-postfix-acquire-text pc len)
  (let ((ppc (tutcode-context-parent-context pc)))
    (if (not (null? ppc))
      (if (eq? (tutcode-context-child-type ppc) 'tutcode-child-type-dialog)
        ()
        (let*
          ((ec (tutcode-context-editor ppc))
           (left-string (tutcode-editor-left-string ec)))
          (if (> (length left-string) len)
            (take left-string len)
            left-string)))
      (let*
        ((ustr (im-acquire-text pc 'primary 'cursor len 0))
         (former (and ustr (ustr-former-seq ustr)))
         (former-seq (and (pair? former) (string-to-list (car former)))))
        (if ustr
          (or former-seq ())
          ;; im-acquire-text未対応環境の場合、内部の確定済文字列バッファを使用
          (if tutcode-enable-fallback-surrounding-text?
            (let ((commit-strs (tutcode-context-commit-strs pc)))
              (if (> (length commit-strs) len)
                (take commit-strs len)
                commit-strs))
            ()))))))

;;; 確定済文字列を削除する
;;; @param len 削除する文字数
(define (tutcode-postfix-delete-text pc len)
  (let ((ppc (tutcode-context-parent-context pc)))
    (if (not (null? ppc))
      (if (eq? (tutcode-context-child-type ppc) 'tutcode-child-type-editor)
        (let*
          ((ec (tutcode-context-editor ppc))
           (left-string (tutcode-editor-left-string ec)))
          (tutcode-editor-set-left-string! ec
            (if (> (length left-string) len)
              (drop left-string len)
              ()))))
      (or
        (im-delete-text pc 'primary 'cursor len 0)
        ;; im-delete-text未対応環境の場合、"\b"を送る。
        ;; XXX:"\b"を認識して文字を削除するアプリでないと動作しない
        ;; (tutcode-commit-rawは入力済キーをそのままアプリに渡すことを指定する
        ;;  ものなので、以下のようにbackspaceキー打鍵の生成には使えない
        ;;  (tutcode-commit-raw pc 'backspace 0))
        (and tutcode-enable-fallback-surrounding-text?
          (begin
            (let ((commit-strs (tutcode-context-commit-strs pc)))
              (tutcode-context-set-commit-strs! pc
                (if (> (length commit-strs) len)
                  (drop commit-strs len)
                  ())))
            (if (> (string-length tutcode-fallback-backspace-string) 0)
              (tutcode-commit pc
                (tutcode-make-string
                  (make-list len tutcode-fallback-backspace-string))
                #t))))))))

;;; 直接入力状態のときのキー入力を処理する。
;;; @param c コンテキストリスト
;;; @param key 入力されたキー
;;; @param key-state コントロールキー等の状態
(define (tutcode-proc-state-off c key key-state)
  (let ((pc (tutcode-find-descendant-context c)))
    (if (tutcode-on-key? key key-state)
      (tutcode-context-set-state! pc 'tutcode-state-on)
      (tutcode-commit-raw pc key key-state))))

;;; 記号入力モード時のキー入力を処理する。
;;; @param c コンテキストリスト
;;; @param key 入力されたキー
;;; @param key-state コントロールキー等の状態
(define (tutcode-proc-state-kigou c key key-state)
  (let ((pc (tutcode-find-descendant-context c)))
    (cond
      ((and
        (tutcode-vi-escape-key? key key-state)
        tutcode-use-with-vi?)
       (tutcode-reset-candidate-window pc)
       (tutcode-context-set-state! pc 'tutcode-state-off)
       (tutcode-commit-raw pc key key-state)) ; ESCキーをアプリにも渡す
      ((tutcode-off-key? key key-state)
       (tutcode-reset-candidate-window pc)
       (tutcode-context-set-state! pc 'tutcode-state-off))
      ((tutcode-kigou-toggle-key? key key-state)
       (tutcode-reset-candidate-window pc)
       (tutcode-context-set-state! pc 'tutcode-state-on))
      ((tutcode-kigou2-toggle-key? key key-state)
       (tutcode-reset-candidate-window pc)
       (if (not (tutcode-kigou2-mode? pc))
         (tutcode-toggle-kigou2-mode pc))
       (tutcode-context-set-state! pc 'tutcode-state-on))
      ;; スペースキーで全角スペース入力可能とするため、
      ;; next-candidate-key?のチェックより前にheading-label-char?をチェック
      ((and (not (and (modifier-key-mask key-state)
                      (not (shift-key-mask key-state))))
            (tutcode-heading-label-char-for-kigou-mode? key))
        (tutcode-commit-by-label-key-for-kigou-mode pc (charcode->string key))
        (if (eq? (tutcode-context-candidate-window pc)
                 'tutcode-candidate-window-kigou)
          (im-select-candidate pc (tutcode-context-nth pc))))
      ((tutcode-next-candidate-key? key key-state)
        (tutcode-change-candidate-index pc 1))
      ((tutcode-prev-candidate-key? key key-state)
        (tutcode-change-candidate-index pc -1))
      ((tutcode-cancel-key? key key-state)
        (tutcode-reset-candidate-window pc)
        (tutcode-begin-kigou-mode pc))
      ((tutcode-next-page-key? key key-state)
        (tutcode-change-candidate-index pc
          tutcode-nr-candidate-max-for-kigou-mode))
      ((tutcode-prev-page-key? key key-state)
        (tutcode-change-candidate-index pc
          (- tutcode-nr-candidate-max-for-kigou-mode)))
      ((tutcode-commit-key? key key-state) ; return-keyはアプリに渡す
        (tutcode-commit pc (tutcode-prepare-commit-string-for-kigou-mode pc)))
      ((or
        (symbol? key)
        (and
          (modifier-key-mask key-state)
          (not (shift-key-mask key-state))))
        (tutcode-commit-raw pc key key-state))
      (else
        (tutcode-commit-raw pc key key-state)))))

;;; 交ぜ書き変換の読み入力状態のときのキー入力を処理する。
;;; @param c コンテキストリスト
;;; @param key 入力されたキー
;;; @param key-state コントロールキー等の状態
(define (tutcode-proc-state-yomi c key key-state)
  (let*
    ((pc (tutcode-find-descendant-context c))
     (rkc (tutcode-context-rk-context pc))
     (head (tutcode-context-head pc))
     (kigou2-mode? (tutcode-kigou2-mode? pc))
     (res #f)
     ;; reset-candidate-windowでリセットされるので保存しておく
     (predicting?
      (eq? (tutcode-context-predicting pc) 'tutcode-predicting-prediction))
     ;; 予測入力候補表示のページ移動時は、reset-candidate-windowしたら駄目
     (prediction-keys-handled?
      (if predicting?
        (cond
          ((tutcode-next-page-key? key key-state)
            (tutcode-change-prediction-page pc #t)
            #t)
          ((tutcode-prev-page-key? key key-state)
            (tutcode-change-prediction-page pc #f)
            #t)
          (else
            #f))
        #f)))
    (if (not prediction-keys-handled?)
      (begin
        (tutcode-reset-candidate-window pc)
        (cond
          ((tutcode-off-key? key key-state)
           (tutcode-flush pc)
           (tutcode-context-set-state! pc 'tutcode-state-off))
          ((and (tutcode-kana-toggle-key? key key-state)
                (not (tutcode-context-latin-conv pc))
                (not kigou2-mode?))
           (rk-flush rkc)
           (tutcode-context-kana-toggle pc))
          ((tutcode-kigou2-toggle-key? key key-state)
           (rk-flush rkc)
           (tutcode-toggle-kigou2-mode pc))
          ((tutcode-backspace-key? key key-state)
           (if (> (length (rk-context-seq rkc)) 0)
            (rk-flush rkc)
            (if (> (length head) 0)
              (begin
                (tutcode-context-set-head! pc (cdr head))
                (if (and predicting? (> tutcode-prediction-start-char-count 0))
                  (tutcode-check-prediction pc #f))))))
          ((or
            (tutcode-commit-key? key key-state)
            (tutcode-return-key? key key-state))
           (tutcode-commit pc (tutcode-make-string head))
           (tutcode-flush pc))
          ((tutcode-cancel-key? key key-state)
           (tutcode-flush pc))
          ((tutcode-stroke-help-toggle-key? key key-state)
           (tutcode-toggle-stroke-help pc))
          ((and tutcode-use-prediction?
                (tutcode-begin-completion-key? key key-state))
           (rk-flush rkc)
           (if (not predicting?)
             (tutcode-check-prediction pc #t)))
          ;; 候補数が1個の場合、変換後自動確定されてconvertingモードに入らない
          ;; ので、その場合でもpurgeできるように、ここでチェック
          ((and (tutcode-purge-candidate-key? key key-state)
                (not (null? head))
                (not kigou2-mode?))
           ;; convertingモードに移行してからpurge
           (tutcode-begin-conversion pc head () #f #f)
           (if (eq? (tutcode-context-state pc) 'tutcode-state-converting)
             (tutcode-proc-state-converting pc key key-state)))
          ((and (tutcode-register-candidate-key? key key-state)
                tutcode-use-recursive-learning?
                (not kigou2-mode?))
           (tutcode-context-set-state! pc 'tutcode-state-converting)
           (tutcode-setup-child-context pc 'tutcode-child-type-editor))
          ((tutcode-katakana-commit-key? key key-state)
            (tutcode-commit pc
              ;;XXX:かなカナ混在時の反転(→カナかな)や、「ゑゐ」は未対応
              (ja-make-kana-str (ja-make-kana-str-list head)
                (if (tutcode-context-katakana-mode? pc)
                  ja-type-hiragana
                  ja-type-katakana)))
            (tutcode-flush pc))
          ((symbol? key)
           (tutcode-flush pc)
           (tutcode-proc-state-on pc key key-state))
          ((and
            (modifier-key-mask key-state)
            (not (shift-key-mask key-state)))
           ;; <Control>n等での変換開始?
           (if (tutcode-begin-conv-key? key key-state)
             (if (not (null? head))
               (tutcode-begin-conversion-with-inflection pc #t)
               (tutcode-flush pc))
             (begin
               (tutcode-flush pc)
               (tutcode-proc-state-on pc key key-state))))
          ;; 予測入力候補用ラベルキー?
          ((and predicting? (tutcode-heading-label-char-for-prediction? key))
            (tutcode-commit-by-label-key-for-prediction pc
              (charcode->string key) 'tutcode-predicting-prediction))
          ((tutcode-context-latin-conv pc)
           (if (tutcode-begin-conv-key? key key-state) ; spaceキーでの変換開始?
             (if (not (null? head))
               (tutcode-begin-conversion-with-inflection pc #t)
               (tutcode-flush pc))
             (set! res (charcode->string key))))
          ((not (rk-expect-key? rkc (charcode->string key)))
           (if (> (length (rk-context-seq rkc)) 0)
             (rk-flush rkc)
             ;; spaceキーでの変換開始?
             ;; (spaceはキーシーケンスに含まれる場合があるので、
             ;;  rk-expectにspaceが無いことが条件)
             ;; (trycodeでspaceで始まるキーシーケンスを使っている場合、
             ;;  spaceで変換開始はできないので、<Control>n等を使う必要あり)
             (if (tutcode-begin-conv-key? key key-state)
               (if (not (null? head))
                 (tutcode-begin-conversion-with-inflection pc #t)
                 (tutcode-flush pc))
               (set! res (charcode->string key)))))
          (else
           (set! res (tutcode-push-key! pc (charcode->string key)))
           (cond
            ((eq? res 'tutcode-auto-help-redisplay)
              (tutcode-auto-help-redisplay pc)
              (set! res #f))
            ((eq? res 'tutcode-postfix-bushu-start)
              (set! res
                (and (>= (length head) 2)
                     (tutcode-bushu-convert (cadr head) (car head))))
              (if res
                (begin
                  (tutcode-context-set-head! pc (cddr head))
                  (tutcode-check-auto-help-window-begin pc (list res) ()))))
            ;; 活用しない語として変換開始。候補が1つの場合は自動確定
            ((eq? res 'tutcode-postfix-mazegaki-start)
              (set! res #f)
              (if (not (null? head))
                (tutcode-begin-conversion-with-inflection pc #f)
                (begin
                  (tutcode-flush pc)
                  (tutcode-begin-postfix-mazegaki-conversion pc #f #f #f))))
            ;; 活用する語として変換開始(postfix用キーシーケンスを流用)
            ((eq? res 'tutcode-postfix-mazegaki-inflection-start)
              (set! res #f)
              (if (not (null? head))
                (tutcode-begin-mazegaki-inflection-conversion pc)
                (begin
                  (tutcode-flush pc)
                  (tutcode-begin-postfix-mazegaki-inflection-conversion pc #f))))
            ((symbol? res)
              (set! res #f)))))
        (if res
          (begin
            (tutcode-append-string pc res)
            (if (and tutcode-use-prediction?
                     (> tutcode-prediction-start-char-count 0)
                     ;; 後置型部首合成変換によるauto-help表示済時は何もしない
                     (eq? (tutcode-context-candidate-window pc)
                          'tutcode-candidate-window-off))
              (tutcode-check-prediction pc #f))))))))

;;; 部首合成変換の部首入力状態のときのキー入力を処理する。
;;; @param c コンテキストリスト
;;; @param key 入力されたキー
;;; @param key-state コントロールキー等の状態
(define (tutcode-proc-state-bushu c key key-state)
  (let*
    ((pc (tutcode-find-descendant-context c))
     (rkc (tutcode-context-rk-context pc))
     (res #f)
     (predicting?
      (eq? (tutcode-context-predicting pc) 'tutcode-predicting-bushu)))
    (tutcode-reset-candidate-window pc)
    (cond
      ((tutcode-off-key? key key-state)
       (tutcode-flush pc)
       (tutcode-context-set-state! pc 'tutcode-state-off))
      ((and (tutcode-kana-toggle-key? key key-state)
            (not (tutcode-kigou2-mode? pc)))
       (rk-flush rkc)
       (tutcode-context-kana-toggle pc))
      ((tutcode-kigou2-toggle-key? key key-state)
       (rk-flush rkc)
       (tutcode-toggle-kigou2-mode pc))
      ((tutcode-backspace-key? key key-state)
       (if (> (length (rk-context-seq rkc)) 0)
        (rk-flush rkc)
        ;; headの1文字目は部首合成変換のマーク▲。backspaceでは消せないように
        ;; する。間違って確定済の文字を消さないようにするため。
        (if (> (length (tutcode-context-head pc)) 1)
          (tutcode-context-set-head! pc (cdr (tutcode-context-head pc))))))
      ((or
        (tutcode-commit-key? key key-state)
        (tutcode-return-key? key key-state))
        ;; 再帰的部首合成変換を(確定して)一段戻す
        (set! res (car (tutcode-context-head pc)))
        (tutcode-context-set-head! pc (cdr (tutcode-context-head pc)))
        (if (not (string=? res "▲"))
          ;; もう1文字(▲のはず)を消して、▲を消す
          (tutcode-context-set-head! pc (cdr (tutcode-context-head pc)))
          (set! res #f))
        (if (= (length (tutcode-context-head pc)) 0)
          (begin
            ;; 最上位の部首合成変換の場合、変換途中の部首があればcommit
            (if res
              (tutcode-commit pc res))
            (tutcode-flush pc)
            (if res (tutcode-check-auto-help-window-begin pc (list res) ()))
            (set! res #f))))
      ((tutcode-cancel-key? key key-state)
        ;; 再帰的部首合成変換を(キャンセルして)一段戻す
        (set! res (car (tutcode-context-head pc)))
        (tutcode-context-set-head! pc (cdr (tutcode-context-head pc)))
        (if (not (string=? res "▲"))
          ;; もう1文字(▲のはず)を消して、▲を消す
          (tutcode-context-set-head! pc (cdr (tutcode-context-head pc))))
        (set! res #f)
        (if (= (length (tutcode-context-head pc)) 0)
          (tutcode-flush pc)))
      ((tutcode-stroke-help-toggle-key? key key-state)
       (tutcode-toggle-stroke-help pc))
      ((and predicting? (tutcode-next-page-key? key key-state))
       (tutcode-change-bushu-prediction-page pc #t))
      ((and predicting? (tutcode-prev-page-key? key key-state))
       (tutcode-change-bushu-prediction-page pc #f))
      ((or
        (symbol? key)
        (and
          (modifier-key-mask key-state)
          (not (shift-key-mask key-state))))
       (tutcode-flush pc)
       (tutcode-proc-state-on pc key key-state))
      ;; 予測入力候補用ラベルキー?
      ((and predicting? (tutcode-heading-label-char-for-prediction? key))
        (tutcode-commit-by-label-key-for-prediction pc
          (charcode->string key) 'tutcode-predicting-bushu))
      ((not (rk-expect-key? rkc (charcode->string key)))
       (if (> (length (rk-context-seq rkc)) 0)
         (rk-flush rkc)
         (set! res (charcode->string key))))
      (else
       (set! res (tutcode-push-key! pc (charcode->string key)))
       (cond
        ((eq? res 'tutcode-bushu-start) ; 再帰的な部首合成変換
          (tutcode-append-string pc "▲")
          (set! res #f))
        ((eq? res 'tutcode-auto-help-redisplay)
          (tutcode-auto-help-redisplay pc)
          (set! res #f))
        ((symbol? res) ;XXX 部首合成変換中は交ぜ書き変換等は無効にする
          (set! res #f)))))
    (if res
      (tutcode-begin-bushu-conversion pc res))))

;;; 部首合成変換開始
;;; @param char 新たに入力された文字(2番目の部首)
(define (tutcode-begin-bushu-conversion pc char)
  (let ((prevchar (car (tutcode-context-head pc))))
    (if (string=? prevchar "▲")
      (begin
        (tutcode-append-string pc char)
        (if tutcode-use-bushu-prediction?
          (tutcode-check-bushu-prediction pc char)))
      ;; 直前の文字が部首合成マーカでない→2文字目が入力された→変換開始
      (let ((convchar (tutcode-bushu-convert prevchar char)))
        (if (string? convchar)
          ;; 合成成功
          (tutcode-bushu-commit pc convchar)
          ;; 合成失敗時は入力し直しを待つ
          )))))

;;; 部首合成変換で変換した文字を確定する
;;; @param convchar 変換後の文字
(define (tutcode-bushu-commit pc convchar)
  ;; 1番目の部首と▲を消す
  (tutcode-context-set-head! pc (cddr (tutcode-context-head pc)))
  (if (null? (tutcode-context-head pc))
    ;; 変換待ちの部首が残ってなければ、確定して終了
    (begin
      (tutcode-commit pc convchar)
      (tutcode-flush pc)
      (tutcode-check-auto-help-window-begin pc (list convchar) ()))
    ;; 部首がまだ残ってれば、再確認。
    ;; (合成した文字が2文字目ならば、連続して部首合成変換)
    (tutcode-begin-bushu-conversion pc convchar)))

;;; 対話的部首合成変換のときのキー入力を処理する。
;;; @param c コンテキストリスト
;;; @param key 入力されたキー
;;; @param key-state コントロールキー等の状態
(define (tutcode-proc-state-interactive-bushu c key key-state)
  (let*
    ((pc (tutcode-find-descendant-context c))
     (rkc (tutcode-context-rk-context pc))
     (head (tutcode-context-head pc))
     (res #f)
     (has-candidate? (> (tutcode-context-prediction-nr pc) 0))
     ;; 候補表示のページ移動時は、reset-candidate-windowしたら駄目
     (candidate-selection-keys-handled?
      (if has-candidate?
        (cond
          ((tutcode-next-page-key? key key-state)
            (tutcode-change-prediction-page pc #t)
            #t)
          ((tutcode-prev-page-key? key key-state)
            (tutcode-change-prediction-page pc #f)
            #t)
          ((and (tutcode-next-candidate-key? key key-state)
                ;; 2打鍵目のスペースキーの場合は候補選択ではない
                (= (length (rk-context-seq rkc)) 0))
            (tutcode-change-prediction-index pc 1)
            #t)
          ((tutcode-prev-candidate-key? key key-state)
            (tutcode-change-prediction-index pc -1)
            #t)
          (else
            #f))
        #f)))
    (if (not candidate-selection-keys-handled?)
      (begin
        (tutcode-reset-candidate-window pc)
        (cond
          ((tutcode-off-key? key key-state)
           (tutcode-flush pc)
           (tutcode-context-set-state! pc 'tutcode-state-off))
          ((and (tutcode-kana-toggle-key? key key-state)
                (not (tutcode-kigou2-mode? pc)))
           (rk-flush rkc)
           (tutcode-context-kana-toggle pc))
          ((tutcode-kigou2-toggle-key? key key-state)
           (rk-flush rkc)
           (tutcode-toggle-kigou2-mode pc))
          ((tutcode-backspace-key? key key-state)
           (if (> (length (rk-context-seq rkc)) 0)
            (rk-flush rkc)
            (if (> (length head) 0)
              (begin
                (tutcode-context-set-head! pc (cdr head))
                (if has-candidate?
                  (tutcode-begin-interactive-bushu-conversion pc))))))
          ((or
            (tutcode-commit-key? key key-state)
            (tutcode-return-key? key key-state))
           (let ((str
                  (cond
                    (has-candidate?
                      (tutcode-get-prediction-string pc))
                    ((> (length head) 0)
                      (tutcode-make-string (tutcode-context-head pc)))
                    (else
                      #f))))
             (if str (tutcode-commit pc str))
             (tutcode-flush pc)
             (if str (tutcode-check-auto-help-window-begin pc (list str) ()))))
          ((tutcode-cancel-key? key key-state)
           (tutcode-flush pc))
          ((tutcode-stroke-help-toggle-key? key key-state)
           (tutcode-toggle-stroke-help pc))
          ((or
            (symbol? key)
            (and
              (modifier-key-mask key-state)
              (not (shift-key-mask key-state))))
           (tutcode-flush pc)
           (tutcode-proc-state-on pc key key-state))
          ((and (tutcode-heading-label-char-for-prediction? key)
                (= (length (rk-context-seq rkc)) 0))
            (tutcode-commit-by-label-key-for-prediction pc
              (charcode->string key) 'tutcode-predicting-interactive-bushu))
          ((not (rk-expect-key? rkc (charcode->string key)))
           (if (> (length (rk-context-seq rkc)) 0)
             (rk-flush rkc)
             (set! res (charcode->string key))))
          (else
           (set! res (tutcode-push-key! pc (charcode->string key)))
           (cond
            ((eq? res 'tutcode-auto-help-redisplay)
              (tutcode-auto-help-redisplay pc)
              (set! res #f))
            ((symbol? res) ;XXX 部首合成変換中は交ぜ書き変換等は無効にする
              (set! res #f)))))
        (if res
          (begin
            (tutcode-append-string pc res)
            (tutcode-begin-interactive-bushu-conversion pc)))))))

;;; 対話的部首合成変換開始
(define (tutcode-begin-interactive-bushu-conversion pc)
  (let*
    ((head (tutcode-context-head pc))
     (res
      (if (null? head)
        ()
        (tutcode-bushu-compose-interactively (reverse head)))))
    (cond
      ;; BSで入力文字が全部消された場合、preeditの候補を消すためnrを0に
      ((null? head)
        (tutcode-context-set-prediction-nr! pc 0)
        (tutcode-context-set-prediction-candidates! pc ()))
      ;; 新たな入力文字を加えた合成不能→新たな入力文字を削除
      ((null? res)
        (tutcode-context-set-head! pc (cdr (tutcode-context-head pc)))
        (if (> (tutcode-context-prediction-nr pc) 0)
          (begin
            (tutcode-context-set-candidate-window! pc
              'tutcode-candidate-window-interactive-bushu)
            (im-activate-candidate-selector pc
              (tutcode-context-prediction-nr-all pc)
              (tutcode-context-prediction-page-limit pc)))))
      (else
        (let ((nr (length res)))
          (tutcode-context-set-prediction-word! pc ())
          (tutcode-context-set-prediction-candidates! pc res)
          (tutcode-context-set-prediction-appendix! pc ())
          (tutcode-context-set-prediction-nr! pc nr)
          (tutcode-context-set-prediction-index! pc 0)
          (let*
            ((params (tutcode-prediction-calc-window-param nr 0))
             (nr-all (list-ref params 0)) ; 全候補数
             (page-limit (list-ref params 1)) ; ページ内候補数
             (nr-in-page (list-ref params 2))) ; ページ内候補数
            (if (> page-limit 0)
              (begin
                ;; 予測入力候補用変数を流用
                (tutcode-context-set-prediction-nr-in-page! pc nr-in-page)
                (tutcode-context-set-prediction-page-limit! pc page-limit)
                (tutcode-context-set-prediction-nr-all! pc nr-all)
                (tutcode-context-set-candidate-window! pc
                  'tutcode-candidate-window-interactive-bushu)
                (im-activate-candidate-selector pc nr-all page-limit)))))))))

;;; 新しい候補を選択する
;;; @param pc コンテキストリスト
;;; @param num 現在の候補番号から新候補番号までのオフセット
(define (tutcode-change-candidate-index pc num)
  (let* ((nr (tutcode-context-nr-candidates pc))
         (nth (tutcode-context-nth pc))
         (new-nth (+ nth num)))
    (cond
      ((< new-nth 0)
       (set! new-nth 0))
      ((and tutcode-use-recursive-learning? (= nth (- nr 1)) (>= new-nth nr))
       (tutcode-reset-candidate-window pc)
       (tutcode-setup-child-context pc 'tutcode-child-type-editor))
      ((>= new-nth nr)
       (set! new-nth (- nr 1))))
    (tutcode-context-set-nth! pc new-nth))
  (if (null? (tutcode-context-child-context pc))
    (begin
      (tutcode-check-candidate-window-begin pc)
      (if (not (eq? (tutcode-context-candidate-window pc)
                    'tutcode-candidate-window-off))
        (im-select-candidate pc (tutcode-context-nth pc))))))

;;; 新しい補完/予測入力候補を選択する
;;; @param num 現在の候補番号から新候補番号までのオフセット
(define (tutcode-change-prediction-index pc num)
  (let* ((nr-all (tutcode-context-prediction-nr-all pc))
         (idx (tutcode-context-prediction-index pc))
         (n (+ idx num))
         (compensated-n
          (cond
           ((>= n nr-all) (- nr-all 1))
           ((< n 0) 0)
           (else n))))
    (tutcode-context-set-prediction-index! pc compensated-n)
    (im-select-candidate pc compensated-n)))

;;; 次/前ページの補完/予測入力候補を表示する
;;; @param next? #t:次ページ, #f:前ページ
(define (tutcode-change-prediction-page pc next?)
  (let ((page-limit (tutcode-context-prediction-page-limit pc)))
    (tutcode-change-prediction-index pc (if next? page-limit (- page-limit)))))

;;; 次/前ページの部首合成変換の予測入力候補を表示する
;;; @param next? #t:次ページ, #f:前ページ
(define (tutcode-change-bushu-prediction-page pc next?)
  (let* ((idx (tutcode-context-prediction-bushu-page-start pc))
         (n (+ idx
              (if next?
                tutcode-nr-candidate-max-for-prediction
                (- tutcode-nr-candidate-max-for-prediction)))))
    (tutcode-bushu-prediction-show-page pc n)))

;;; 候補ウィンドウを閉じる
(define (tutcode-reset-candidate-window pc)
  (if (not (eq? (tutcode-context-candidate-window pc)
                'tutcode-candidate-window-off))
    (begin
      (im-deactivate-candidate-selector pc)
      (tutcode-context-set-candidate-window! pc 'tutcode-candidate-window-off)
      (tutcode-context-set-predicting! pc 'tutcode-predicting-off))))

;;; 交ぜ書き変換の候補選択状態から、読み入力状態に戻す。
;;; @param pc コンテキストリスト
(define (tutcode-back-to-yomi-state pc)
  (if (> (tutcode-context-postfix-yomi-len pc) 0) ; 後置型?
    (tutcode-flush pc)
    (begin
      (tutcode-reset-candidate-window pc)
      (tutcode-context-set-state! pc 'tutcode-state-yomi)
      (tutcode-context-set-head! pc (tutcode-context-mazegaki-yomi-all pc))
      (tutcode-context-set-nr-candidates! pc 0))))

;;; 交ぜ書き変換の辞書登録状態から、候補選択状態に戻す。
;;; @param pc コンテキストリスト
(define (tutcode-back-to-converting-state pc)
  (tutcode-context-set-nth! pc (- (tutcode-context-nr-candidates pc) 1))
  (tutcode-check-candidate-window-begin pc)
  (if (eq? (tutcode-context-candidate-window pc)
           'tutcode-candidate-window-converting)
    (im-select-candidate pc (tutcode-context-nth pc)))
  (tutcode-context-set-state! pc 'tutcode-state-converting))

;;; 入力されたキーが候補ラベル文字かどうかを調べる
;;; @param key 入力されたキー
(define (tutcode-heading-label-char? key)
  (member (charcode->string key) tutcode-heading-label-char-list))

;;; 入力されたキーが記号入力モード時の候補ラベル文字かどうかを調べる
;;; @param key 入力されたキー
(define (tutcode-heading-label-char-for-kigou-mode? key)
  (member (charcode->string key) tutcode-heading-label-char-list-for-kigou-mode))

;;; 入力されたキーが補完/予測入力時の候補ラベル文字かどうかを調べる
;;; @param key 入力されたキー
(define (tutcode-heading-label-char-for-prediction? key)
  (member (charcode->string key) tutcode-heading-label-char-list-for-prediction))

;;; 交ぜ書き変換の候補選択状態のときのキー入力を処理する。
;;; @param c コンテキストリスト
;;; @param key 入力されたキー
;;; @param key-state コントロールキー等の状態
(define (tutcode-proc-state-converting c key key-state)
  (let ((pc (tutcode-find-descendant-context c)))
    (cond
      ((tutcode-next-candidate-key? key key-state)
        (tutcode-change-candidate-index pc 1))
      ((tutcode-prev-candidate-key? key key-state)
        (tutcode-change-candidate-index pc -1))
      ((tutcode-cancel-key? key key-state)
        (tutcode-back-to-yomi-state pc))
      ((tutcode-next-page-key? key key-state)
        (tutcode-change-candidate-index pc tutcode-nr-candidate-max))
      ((tutcode-prev-page-key? key key-state)
        (tutcode-change-candidate-index pc (- tutcode-nr-candidate-max)))
      ((or (tutcode-commit-key? key key-state)
           (tutcode-return-key? key key-state))
        (tutcode-commit-with-auto-help pc))
      ((tutcode-purge-candidate-key? key key-state)
        (tutcode-reset-candidate-window pc)
        (tutcode-setup-child-context pc 'tutcode-child-type-dialog))
      ((and (tutcode-register-candidate-key? key key-state)
            tutcode-use-recursive-learning?)
        (tutcode-reset-candidate-window pc)
        (tutcode-setup-child-context pc 'tutcode-child-type-editor))
      ((tutcode-mazegaki-relimit-right-key? key key-state)
        (tutcode-mazegaki-proc-relimit-right pc))
      ((tutcode-mazegaki-relimit-left-key? key key-state)
        (tutcode-mazegaki-proc-relimit-left pc))
      ((and tutcode-commit-candidate-by-label-key?
            (> (tutcode-context-nr-candidates pc) 1)
            (tutcode-heading-label-char? key))
        (tutcode-commit-by-label-key pc (charcode->string key)))
      (else
        (let ((postfix-yomi-len (tutcode-context-postfix-yomi-len pc)))
          (if (> postfix-yomi-len 0)
            (tutcode-postfix-delete-text pc postfix-yomi-len)))
        (tutcode-commit pc (tutcode-prepare-commit-string pc))
        (tutcode-proc-state-on pc key key-state)))))

;;; 部首合成変換を行う。
;;; @param c1 1番目の部首
;;; @param c2 2番目の部首
;;; @return 合成後の文字。合成できなかったときは#f
(define (tutcode-bushu-convert c1 c2)
  ;; tc-2.1+[tcode-ml:1925]の部首合成アルゴリズムを使用
  (and c1 c2
    (or
      (tutcode-bushu-compose-sub c1 c2)
      (let ((a1 (tutcode-bushu-alternative c1))
            (a2 (tutcode-bushu-alternative c2)))
        (and
          (or
            (not (string=? a1 c1))
            (not (string=? a2 c2)))
          (begin
            (set! c1 a1)
            (set! c2 a2)
            #t)
          (tutcode-bushu-compose-sub c1 c2)))
      (let* ((decomposed1 (tutcode-bushu-decompose c1))
             (decomposed2 (tutcode-bushu-decompose c2))
             (tc11 (and decomposed1 (car decomposed1)))
             (tc12 (and decomposed1 (cadr decomposed1)))
             (tc21 (and decomposed2 (car decomposed2)))
             (tc22 (and decomposed2 (cadr decomposed2)))
             ;; 合成後の文字が、合成前の2つの部首とは異なる
             ;; 新しい文字であることを確認する。
             ;; (string=?だと#fがあったときにエラーになるのでequal?を使用)
             (newchar
                (lambda (new)
                  (and
                    (not (equal? new c1))
                    (not (equal? new c2))
                    new))))
        (or
          ;; 引き算
          (and
            (equal? tc11 c2)
            (newchar tc12))
          (and
            (equal? tc12 c2)
            (newchar tc11))
          (and
            (equal? tc21 c1)
            (newchar tc22))
          (and
            (equal? tc22 c1)
            (newchar tc21))
          ;; 部品による足し算
          (let ((compose-newchar
                  (lambda (i1 i2)
                    (let ((res (tutcode-bushu-compose-sub i1 i2)))
                      (and res
                        (newchar res))))))
            (or
              (compose-newchar c1 tc22) (compose-newchar tc11 c2)
              (compose-newchar c1 tc21) (compose-newchar tc12 c2)
              (compose-newchar tc11 tc22) (compose-newchar tc11 tc21)
              (compose-newchar tc12 tc22) (compose-newchar tc12 tc21)))
          ;; 部品による引き算
          (and tc11
            (equal? tc11 tc21)
            (newchar tc12))
          (and tc11
            (equal? tc11 tc22)
            (newchar tc12))
          (and tc12
            (equal? tc12 tc21)
            (newchar tc11))
          (and tc12
            (equal? tc12 tc22)
            (newchar tc11)))))))

;;; 部首合成変換:c1とc2を合成してできる文字を探して返す。
;;; 指定された順番で見つからなかった場合は、順番を入れかえて探す。
;;; @param c1 1番目の部首
;;; @param c2 2番目の部首
;;; @return 合成後の文字。合成できなかったときは#f
(define (tutcode-bushu-compose-sub c1 c2)
  (and c1 c2
    (or
      (tutcode-bushu-compose c1 c2)
      (tutcode-bushu-compose c2 c1))))

;;; 部首合成変換:c1とc2を合成してできる文字を探して返す。
;;; @param c1 1番目の部首
;;; @param c2 2番目の部首
;;; @return 合成後の文字。合成できなかったときは#f
(define (tutcode-bushu-compose c1 c2)
  (let ((seq (rk-lib-find-seq (list c1 c2) tutcode-bushudic)))
    (and seq
      (car (cadr seq)))))

;;; 部首合成変換:等価文字を探して返す。
;;; @param c 検索対象の文字
;;; @return 等価文字。等価文字が見つからなかったときは元の文字自身
(define (tutcode-bushu-alternative c)
  (let ((alt (assoc c tutcode-bushudic-altchar)))
    (or
      (and alt (cadr alt))
      c)))

;;; 部首合成変換:文字を2つの部首に分解する。
;;; @param c 分解対象の文字
;;; @return 分解してできた2つの部首のリスト。分解できなかったときは#f
(define (tutcode-bushu-decompose c)
  (if (null? tutcode-reverse-bushudic-alist)
    (set! tutcode-reverse-bushudic-alist
      (map
        (lambda (elem)
          (cons (caadr elem) (caar elem)))
        tutcode-bushudic)))
  (let ((res (assoc c tutcode-reverse-bushudic-alist)))
    (and res
      (cdr res))))

;;; 自動ヘルプ:対象文字を部首合成するのに必要となる、
;;; 外字でない2つの文字のリストを返す
;;; 例: "繋" => (((("," "o"))("撃")) ((("f" "q"))("糸")))
;;; @param c 対象文字
;;; @param rule tutcode-rule
;;; @return 対象文字の部首合成に必要な2つの文字とストロークのリスト。
;;;  見つからなかった場合は#f
(define (tutcode-auto-help-bushu-decompose c rule)
  (let*
    ((bushu (tutcode-bushu-decompose c))
     (b1 (and bushu (car bushu)))
     (b2 (and bushu (cadr bushu)))
     (seq1 (and b1 (tutcode-auto-help-get-stroke b1 rule)))
     (seq2 (and b2 (tutcode-auto-help-get-stroke b2 rule))))
    (or
      ;; 足し算による合成
      (and seq1 seq2
        (list seq1 seq2))
      ;; 単純な引き算による合成
      (tutcode-auto-help-bushu-decompose-by-subtraction c rule)
      ;; 部品による合成
      (or
        ;; 部首1が直接入力可能
        ;; →(部首1)と(部首2を部品として持つ漢字)による合成が可能か?
        (and seq1 b2
          (tutcode-auto-help-bushu-decompose-looking-bushudic tutcode-bushudic
            () 99
            (lambda (elem)
              (tutcode-auto-help-get-stroke-list-with-right-part
                c b1 b2 seq1 rule elem))))
        ;; 部首2が直接入力可能
        ;; →(部首2)と(部首1を部品として持つ漢字)による合成が可能か?
        (and seq2 b1
          (tutcode-auto-help-bushu-decompose-looking-bushudic tutcode-bushudic
            () 99
            (lambda (elem)
              (tutcode-auto-help-get-stroke-list-with-left-part
                c b1 b2 seq2 rule elem))))
        ;; XXX: 部品どうしの合成や、3文字以上での合成は未対応
        ))))

;;; 自動ヘルプ:対象文字を入力する際の打鍵のリストを取得する。
;;; 例: "撃" => ((("," "o")) ("撃"))
;;; @param b 対象文字
;;; @param rule tutcode-rule
;;; @return 打鍵リスト。入力不可能な場合は#f
(define (tutcode-auto-help-get-stroke b rule)
  (let
    ((seq
      (or (tutcode-reverse-find-seq b rule)
          ;; 部首合成で使われる"3"のような直接入力可能な部首に対応するため、
          ;; ラベル文字に含まれていれば、直接入力可能とみなす
          (and
            (member b tutcode-heading-label-char-list-for-kigou-mode)
            (list b)))))
    (and seq
      (list (list seq) (list b)))))

;;; 自動ヘルプ:部首合成辞書の要素を順に見て
;;; 最初に見つかった2打鍵の部首の組み合わせを返す。
;;; (filterやmapを使って、最小のストロークのものを探すと時間がかかるので。)
;;; 両方とも2打鍵の部首の組み合わせが見つからなかったら、
;;; 3打鍵の部首との組み合わせがあれば返す。
;;; @param long-stroke-result 3打鍵以上の文字を含む結果
;;; @param min-stroke long-stroke-result内の現在の最少打鍵数
;;; @param get-stroke-list 部首合成用の2つの文字とストロークのリストを返す関数
;;; @return 部首合成用の2つの文字とストロークのリスト。
;;;  見つからなかった場合は#f
(define (tutcode-auto-help-bushu-decompose-looking-bushudic bushudic
          long-stroke-result min-stroke get-stroke-list)
  (if (null? bushudic)
    (and
      (not (null? long-stroke-result))
      long-stroke-result)
    (let*
      ((res
        (get-stroke-list (list min-stroke (car bushudic))))
       (len (if (not res) 99 (tutcode-auto-help-count-stroke-length res)))
       (min (if (< len min-stroke) len min-stroke)))
      (if (<= len 4) ; "5"等を使う部首合成で4打鍵未満もあるが、そこまでは見ない
        res
        (tutcode-auto-help-bushu-decompose-looking-bushudic (cdr bushudic)
          (if (< len min-stroke) res long-stroke-result)
          min get-stroke-list)))))

;;; 自動ヘルプ:対象文字を引き算により部首合成するのに必要となる、
;;; 外字でない文字のリストを返す。
;;; 例: "歹" => (((("g" "t" "h")) ("列")) ((("G" "I")) ("リ")))
;;;    (元となるtutcode-bushudic内の要素は((("歹" "リ")) ("列")))
;;; @param c 対象文字
;;; @param rule tutcode-rule
;;; @return 対象文字の部首合成に必要な2つの文字とストロークのリスト。
;;;  見つからなかった場合は#f
(define (tutcode-auto-help-bushu-decompose-by-subtraction c rule)
  (tutcode-auto-help-bushu-decompose-looking-bushudic tutcode-bushudic
    () 99
    (lambda (elem)
      (tutcode-auto-help-get-stroke-list-by-subtraction c rule elem))))

;;; 自動ヘルプ:部首合成に必要な打鍵数を数える
;;; @param bushu-compose-list 部首合成に使う2文字とストロークのリスト。
;;;  例: (((("g" "t" "h")) ("列")) ((("G" "I")) ("リ")))
;;; @return bushu-compose-listに含まれる打鍵数。(上の例の場合は5)
(define (tutcode-auto-help-count-stroke-length bushu-compose-list)
  (+ (length (caaar bushu-compose-list))
     (length (caaadr bushu-compose-list))))

;;; 自動ヘルプ:対象文字を引き算により部首合成できる場合は、
;;; 合成に使う各文字と、そのストロークのリストを返す。
;;; @param c 対象文字
;;; @param rule tutcode-rule
;;; @param min-stroke-bushu-list min-strokeとbushudic内の要素のリスト。
;;;  例: (6 ((("歹" "リ")) ("列")))
;;; @return 対象文字の部首合成に必要な2つの文字とストロークのリスト。
;;;  bushu-listを使って合成できない場合は#f。
;;;  例: (((("g" "t" "h")) ("列")) ((("G" "I")) ("リ")))
(define (tutcode-auto-help-get-stroke-list-by-subtraction
          c rule min-stroke-bushu-list)
  (and-let*
    ((min-stroke (car min-stroke-bushu-list))
     (bushu-list (cadr min-stroke-bushu-list))
     (mem (member c (caar bushu-list)))
     (b1 (caadr bushu-list))
     ;; 2つの部首のうち、c以外の部首を取得
     (b2 (if (= 2 (length mem)) (cadr mem) (car (caar bushu-list))))
     (seq1 (tutcode-auto-help-get-stroke b1 rule))
     (seq2 (tutcode-auto-help-get-stroke b2 rule))
     (ret (list seq1 seq2))
     ;; 部首合成は遅いので、先に打鍵数をチェック
     (small-stroke? (< (tutcode-auto-help-count-stroke-length ret) min-stroke))
     ;; 実際に部首合成して、対象文字が合成されないものは駄目
     (composed (tutcode-bushu-convert b1 b2))
     (c-composed? (string=? composed c)))
    ret))

;;; 自動ヘルプ:対象文字を「部首1」と「部首2を部品として持つ漢字」により
;;; 部首合成できる場合は、
;;; 合成に使う各文字と、そのストロークのリストを返す。
;;; @param c 対象文字
;;; @param b1 部首1(直接入力可能)
;;; @param b2 部首2(直接入力不可能)
;;; @param seq1 b1の入力キーシーケンスと部首のリスト
;;; @param rule tutcode-rule
;;; @param min-stroke-bushu-list min-strokeとbushudic内の要素のリスト。
;;; @return 対象文字の部首合成に必要な2つの文字とストロークのリスト。
;;;  bushu-listを使って合成できない場合は#f。
(define (tutcode-auto-help-get-stroke-list-with-right-part
         c b1 b2 seq1 rule min-stroke-bushu-list)
  (and-let*
    ((min-stroke (car min-stroke-bushu-list))
     (bushu-list (cadr min-stroke-bushu-list))
     (mem (member b2 (caar bushu-list)))
     (kanji (caadr bushu-list)) ; 部首2を部品として持つ漢字
     (seq (tutcode-auto-help-get-stroke kanji rule))
     (ret (list seq1 seq))
     ;; 部首合成は遅いので、先に打鍵数をチェック
     (small-stroke? (< (tutcode-auto-help-count-stroke-length ret) min-stroke))
     ;; 実際に部首合成して、対象文字が合成されないものは駄目
     (composed (tutcode-bushu-convert b1 kanji))
     (c-composed? (string=? composed c)))
    ret))

;;; 自動ヘルプ:対象文字を「部首1を部品として持つ漢字」と「部首2」により
;;; 部首合成できる場合は、
;;; 合成に使う各文字と、そのストロークのリストを返す。
;;; @param c 対象文字 (例: "讐")
;;; @param b1 部首1(直接入力不可能) (例: "隹")
;;; @param b2 部首2(直接入力可能) (例: "言")
;;; @param seq2 b2の入力キーシーケンスと部首のリスト。
;;;  例: ((("b" ",")) ("言"))
;;; @param rule tutcode-rule
;;; @param min-stroke-bushu-list min-strokeとbushudic内の要素のリスト。
;;;  例: (6 ((("性" "隹")) ("惟")))
;;; @return 対象文字の部首合成に必要な2つの文字とストロークのリスト。
;;;  bushu-listを使って合成できない場合は#f。
;;;  例: (((("e" "v" ".")) ("惟")) ((("b" ",")) ("言")))
(define (tutcode-auto-help-get-stroke-list-with-left-part
         c b1 b2 seq2 rule min-stroke-bushu-list)
  (and-let*
    ((min-stroke (car min-stroke-bushu-list))
     (bushu-list (cadr min-stroke-bushu-list))
     (mem (member b1 (caar bushu-list)))
     (kanji (caadr bushu-list)) ; 部首1を部品として持つ漢字
     (seq (tutcode-auto-help-get-stroke kanji rule))
     (ret (list seq seq2))
     ;; 部首合成は遅いので、先に打鍵数をチェック
     (small-stroke? (< (tutcode-auto-help-count-stroke-length ret) min-stroke))
     ;; 実際に部首合成して、対象文字が合成されないものは駄目
     (composed (tutcode-bushu-convert kanji b2))
     (c-composed? (string=? composed c)))
    ret))

;;; 部首合成変換時の予測入力候補を検索
;;; @param str 部首1
;;; @param bushudic 部首合成リスト
;;; @return (<部首2> <合成文字>)のリスト
(define (tutcode-bushu-predict str bushudic)
  (let*
    ((rules (rk-lib-find-partial-seqs (list str) bushudic))
     (words1 (map (lambda (elem) (cadaar elem)) rules))
     (more-cands
      (filter
        (lambda (elem)
          (let
            ;; (((部首1 部首2))(合成文字))
            ((bushu1 (caaar elem))
             (bushu2 (cadaar elem))
             (gosei (caadr elem)))
            (or
              ;; strが1文字目の場合はrk-lib-find-partial-seqsで検索済
              ;(string=? str bushu1) ; (((str 部首2))(合成文字))
              (and (string=? str bushu2) ; (((部首1 str))(合成文字))
                    ;; 既に上で出現済の場合は除外。
                    ;; 例: ((("門" "才"))("閉"))で"才"が出現済の場合、
                    ;;     ((("才" "門"))("捫"))の"才"は除外。
                   (not (member bushu1 words1)))
              (string=? str gosei)))) ; (((部首1 部首2))(str))
              ;; XXX:この場合、strとbushu1でbushu2が合成できることを
              ;;     確認すべきだが、tutcode-bushu-convertは遅いので省略。
          bushudic))
     (res (append rules more-cands))
     (word/cand
      (map
       (lambda (elem)
        (let
         ((bushu1 (caaar elem))
          (bushu2 (cadaar elem))
          (gosei (caadr elem)))
         (cond
          ((string=? str bushu1) ; (((str 部首2))(合成文字))
           (list bushu2 gosei))
          ((string=? str bushu2) ; (((部首1 str))(合成文字))
           (list bushu1 gosei))
          ((string=? str gosei) ; (((部首1 部首2))(str))
           (list bushu1 bushu2)))))
       res)))
    word/cand))

;;; tutcode-ruleを逆引きして、変換後の文字から、入力キー列を取得する。
;;; 例: (tutcode-reverse-find-seq "あ" tutcode-rule) => ("r" "k")
;;; @param c 変換後の文字
;;; @param rule tutcode-rule
;;; @return 入力キーのリスト。tutcode-rule中にcが見つからなかった場合は#f
(define (tutcode-reverse-find-seq c rule)
  (let*
    ((make-reverse-rule-alist
      (lambda (r)
        (map
          (lambda (elem)
            (cons (caadr elem) (caar elem)))
          r)))
     (alist
      (if (eq? rule tutcode-kigou-rule)
        (begin
          (if (null? tutcode-reverse-kigou-rule-alist)
            (set! tutcode-reverse-kigou-rule-alist
              (make-reverse-rule-alist rule)))
          tutcode-reverse-kigou-rule-alist)
        (begin
          (if (null? tutcode-reverse-rule-alist)
            (set! tutcode-reverse-rule-alist
              (make-reverse-rule-alist rule)))
          tutcode-reverse-rule-alist)))
     (res (assoc c alist)))
    (and res
      (cdr res))))

;;; 現在のstateがpreeditを持つかどうかを返す。
;;; @param pc コンテキストリスト
(define (tutcode-state-has-preedit? pc)
  (or
    (not (null? (tutcode-context-child-context pc)))
    (memq (tutcode-context-state pc)
      '(tutcode-state-yomi tutcode-state-bushu tutcode-state-converting
        tutcode-state-interactive-bushu tutcode-state-kigou))))

;;; キーが押されたときの処理の振り分けを行う。
;;; @param c コンテキストリスト
;;; @param key 入力されたキー
;;; @param key-state コントロールキー等の状態
(define (tutcode-key-press-handler c key key-state)
  (if (ichar-control? key)
      (im-commit-raw c)
      (let ((pc (tutcode-find-descendant-context c)))
        (case (tutcode-context-state pc)
          ((tutcode-state-on)
           (tutcode-proc-state-on pc key key-state)
           (if (or
                 ;; 交ぜ書き変換や部首合成変換開始。△や▲を表示する
                 (tutcode-state-has-preedit? c)
                 ;; 文字数指定後置型交ぜ書き変換の再帰学習キャンセル
                 (not (eq? (tutcode-find-descendant-context c) pc)))
             (tutcode-update-preedit pc)))
          ((tutcode-state-kigou)
           (tutcode-proc-state-kigou pc key key-state)
           (tutcode-update-preedit pc))
          ((tutcode-state-yomi)
           (tutcode-proc-state-yomi pc key key-state)
           (tutcode-update-preedit pc))
          ((tutcode-state-converting)
           (tutcode-proc-state-converting pc key key-state)
           (tutcode-update-preedit pc))
          ((tutcode-state-bushu)
           (tutcode-proc-state-bushu pc key key-state)
           (tutcode-update-preedit pc))
          ((tutcode-state-interactive-bushu)
           (tutcode-proc-state-interactive-bushu pc key key-state)
           (tutcode-update-preedit pc))
          (else
           (tutcode-proc-state-off pc key key-state)
           (if (tutcode-state-has-preedit? c) ; 再帰学習時
             (tutcode-update-preedit pc))))
        (if tutcode-use-stroke-help-window?
          ;; editorの作成・削除の可能性があるのでdescendant-context取得し直し
          (let ((newpc (tutcode-find-descendant-context c)))
            (if
              (and
                (memq (tutcode-context-state newpc)
                  '(tutcode-state-on tutcode-state-yomi tutcode-state-bushu
                    tutcode-state-interactive-bushu))
                (not (tutcode-context-latin-conv newpc)))
              (tutcode-check-stroke-help-window-begin newpc)))))))

;;; キーが離されたときの処理を行う。
;;; @param pc コンテキストリスト
;;; @param key 入力されたキー
;;; @param key-state コントロールキー等の状態
(define (tutcode-key-release-handler pc key key-state)
  (if (or (ichar-control? key)
	  (not (tutcode-context-on? pc)))
      ;; don't discard key release event for apps
      (im-commit-raw pc)))

;;; TUT-Code IMの初期化を行う。
(define (tutcode-init-handler id im arg)
  (let ((tc (tutcode-context-new id im)))
    (set! tutcode-context-list (cons tc tutcode-context-list))
    tc))

(define (tutcode-release-handler tc)
  (tutcode-save-personal-dictionary #f)
  (set! tutcode-context-list (delete! tc tutcode-context-list))
  (if (null? tutcode-context-list)
    (begin
      (skk-lib-free-dic tutcode-dic)
      (set! tutcode-dic #f))))

(define (tutcode-reset-handler tc)
  (tutcode-flush tc))

(define (tutcode-focus-in-handler tc) #f)

(define (tutcode-focus-out-handler c)
  (let* ((tc (tutcode-find-descendant-context c))
         (rkc (tutcode-context-rk-context tc)))
    (rk-flush rkc)))

(define tutcode-place-handler tutcode-focus-in-handler)
(define tutcode-displace-handler tutcode-focus-out-handler)

;;; 候補ウィンドウが候補文字列を取得するために呼ぶ関数
(define (tutcode-get-candidate-handler c idx accel-enum-hint)
  (let ((tc (tutcode-find-descendant-context c)))
    (cond
      ((= accel-enum-hint 9999) ;XXX 表形式候補ウィンドウからのdisplay_limit調整時
        (set! tutcode-nr-candidate-max (length tutcode-heading-label-char-list))
        (set! tutcode-nr-candidate-max-for-kigou-mode
          (length tutcode-heading-label-char-list-for-kigou-mode))
        (set! tutcode-nr-candidate-max-for-prediction
          (length tutcode-heading-label-char-list-for-prediction))
        (set! tutcode-nr-candidate-max-for-guide
          (- tutcode-nr-candidate-max-for-kigou-mode
             tutcode-nr-candidate-max-for-prediction))
        (list "" ""
          (string-append "display_limit="
            (number->string
              (cond
                ((eq? (tutcode-context-state tc) 'tutcode-state-kigou)
                  tutcode-nr-candidate-max-for-kigou-mode)
                ((eq? (tutcode-context-state tc)
                      'tutcode-state-interactive-bushu)
                  (tutcode-context-prediction-page-limit tc))
                ((not (eq? (tutcode-context-predicting tc)
                           'tutcode-predicting-off))
                  (tutcode-context-prediction-page-limit tc))
                (else
                  tutcode-nr-candidate-max))))))
      ;; 記号入力
      ((eq? (tutcode-context-state tc) 'tutcode-state-kigou)
        (let* ((cand (tutcode-get-nth-candidate-for-kigou-mode tc idx))
               (n (remainder idx
                    (length tutcode-heading-label-char-list-for-kigou-mode)))
               (label (nth n tutcode-heading-label-char-list-for-kigou-mode)))
          ;; XXX:annotation表示は現状無効化されているので、常に""を返しておく
          (list cand label "")))
      ;; 補完/予測入力候補
      ((not (eq? (tutcode-context-predicting tc) 'tutcode-predicting-off))
        (let*
          ((nr-in-page (tutcode-context-prediction-nr-in-page tc))
           (page-limit (tutcode-context-prediction-page-limit tc))
           (pages (quotient idx page-limit))
           (idx-in-page (remainder idx page-limit)))
          ;; 各ページには、nr-in-page個の補完/予測入力候補と、熟語ガイドを表示
          (if (< idx-in-page nr-in-page)
            ;; 補完/予測入力候補文字列
            (let*
              ((nr-predictions (tutcode-lib-get-nr-predictions tc))
               (p-idx (+ idx-in-page (* pages nr-in-page)))
               (i (remainder p-idx nr-predictions))
               (cand (tutcode-lib-get-nth-prediction tc i))
               (cand-guide
                (if (eq? (tutcode-context-predicting tc)
                          'tutcode-predicting-bushu)
                  (string-append
                    cand "(" (tutcode-lib-get-nth-word tc i) ")")
                  cand))
               (n (remainder p-idx
                    (length tutcode-heading-label-char-list-for-prediction)))
               (label (nth n tutcode-heading-label-char-list-for-prediction)))
              (list cand-guide label ""))
            ;; 熟語ガイド
            (let*
              ((guide (tutcode-context-guide tc))
               (guide-len (length guide)))
              (if (= guide-len 0)
                (list "" "" "")
                (let*
                  ((guide-idx-in-page (- idx-in-page nr-in-page))
                   (nr-guide-in-page (- page-limit nr-in-page))
                   (guide-idx (+ guide-idx-in-page (* pages nr-guide-in-page)))
                   (n (remainder guide-idx guide-len))
                   (label-cands-alist (nth n guide))
                   (label (car label-cands-alist))
                   (cands (cdr label-cands-alist))
                   (cand
                    (tutcode-make-string
                      (append cands (list tutcode-guide-mark)))))
                  (list cand label "")))))))
      ;; 仮想鍵盤
      ((eq? (tutcode-context-candidate-window tc)
            'tutcode-candidate-window-stroke-help)
        (nth idx (tutcode-context-stroke-help tc)))
      ;; 自動ヘルプ
      ((eq? (tutcode-context-candidate-window tc)
            'tutcode-candidate-window-auto-help)
        (nth idx (tutcode-context-auto-help tc)))
      ;; 対話的部首合成変換
      ((eq? (tutcode-context-state tc) 'tutcode-state-interactive-bushu)
        (let*
          ;; 予測入力候補用変数を流用
          ((nr-in-page (tutcode-context-prediction-nr-in-page tc))
           (page-limit (tutcode-context-prediction-page-limit tc))
           (pages (quotient idx page-limit))
           (idx-in-page (remainder idx page-limit))
           (nr-predictions (tutcode-lib-get-nr-predictions tc))
           (p-idx (+ idx-in-page (* pages nr-in-page)))
           (i (remainder p-idx nr-predictions))
           (cand (tutcode-lib-get-nth-prediction tc i))
           (n (remainder p-idx
                (length tutcode-heading-label-char-list-for-prediction)))
           (label (nth n tutcode-heading-label-char-list-for-prediction)))
          (list cand label "")))
      ;; 交ぜ書き変換
      (else
        (let* ((cand (tutcode-get-nth-candidate tc idx))
               (n (remainder idx (length tutcode-heading-label-char-list)))
               (label (nth n tutcode-heading-label-char-list)))
          (list cand label ""))))))

;;; 候補ウィンドウが候補を選択したときに呼ぶ関数。
;;; 選択された候補を確定する。
(define (tutcode-set-candidate-index-handler c idx)
  (let* ((pc (tutcode-find-descendant-context c))
         (candwin (tutcode-context-candidate-window pc)))
    (cond
      ((and (or (eq? candwin 'tutcode-candidate-window-converting)
                (eq? candwin 'tutcode-candidate-window-kigou))
          (>= idx 0)
          (< idx (tutcode-context-nr-candidates pc)))
        (tutcode-context-set-nth! pc idx)
        (if (eq? (tutcode-context-state pc) 'tutcode-state-kigou)
          (tutcode-commit pc (tutcode-prepare-commit-string-for-kigou-mode pc))
          (tutcode-commit-with-auto-help pc))
        (tutcode-update-preedit pc))
      ((and (or (eq? candwin 'tutcode-candidate-window-predicting)
                (eq? candwin 'tutcode-candidate-window-interactive-bushu))
            (>= idx 0))
        (let*
          ((nr-in-page (tutcode-context-prediction-nr-in-page pc))
           (page-limit (tutcode-context-prediction-page-limit pc))
           (idx-in-page (remainder idx page-limit)))
          (if (< idx-in-page nr-in-page)
            (let*
              ((nr-predictions (tutcode-lib-get-nr-predictions pc))
               (pages (quotient idx page-limit))
               (p-idx (+ idx-in-page (* pages nr-in-page)))
               (i (remainder p-idx nr-predictions))
               (mode (tutcode-context-predicting pc)))
              (tutcode-context-set-prediction-index! pc i)
              (if (eq? candwin 'tutcode-candidate-window-interactive-bushu)
                (tutcode-do-commit-prediction-for-interactive-bushu pc)
                (if (eq? mode 'tutcode-predicting-bushu)
                  (tutcode-do-commit-prediction-for-bushu pc)
                  (tutcode-do-commit-prediction pc
                    (eq? mode 'tutcode-predicting-completion))))
              (tutcode-update-preedit pc))))))))

(tutcode-configure-widgets)

;;; TUT-Code IMを登録する。
(register-im
 'tutcode
 "ja"
 "EUC-JP"
 tutcode-im-name-label
 tutcode-im-short-desc
 #f
 tutcode-init-handler
 tutcode-release-handler
 context-mode-handler
 tutcode-key-press-handler
 tutcode-key-release-handler
 tutcode-reset-handler
 tutcode-get-candidate-handler
 tutcode-set-candidate-index-handler
 context-prop-activate-handler
 #f
 tutcode-focus-in-handler
 tutcode-focus-out-handler
 tutcode-place-handler
 tutcode-displace-handler
 )

;;; コード表を変換する。
;;; @param from 変換対象コード表
;;; @param translate-alist 変換表
;;; @return 変換したコード表
(define (tutcode-rule-translate from translate-alist)
  (map
    (lambda (elem)
      (cons
        (list
          (map
            (lambda (key)
              (let ((res (assoc key translate-alist)))
                (if res
                  (cadr res)
                  key)))
            (caar elem)))
        (cdr elem)))
    from))

;;; コード表をQwertyからDvorak用に変換する。
;;; @param qwerty Qwertyのコード表
;;; @return Dvorakに変換したコード表
(define (tutcode-rule-qwerty-to-dvorak qwerty)
  (tutcode-rule-translate qwerty tutcode-rule-qwerty-to-dvorak-alist))

;;; コード表をQwerty-jisからQwerty-us用に変換する。
;;; @param jis Qwerty-jisのコード表
;;; @return Qwerty-usに変換したコード表
(define (tutcode-rule-qwerty-jis-to-qwerty-us jis)
  (tutcode-rule-translate jis tutcode-rule-qwerty-jis-to-qwerty-us-alist))

;;; kigou-ruleをキーボードレイアウトに合わせて変換する
;;; @param layout tutcode-candidate-window-table-layout
(define (tutcode-kigou-rule-translate layout)
  (let
    ((translate-stroke-help-alist
      (lambda (lis translate-alist)
        (map
          (lambda (elem)
            (cons
              (let ((res (assoc (car elem) translate-alist)))
                (if res
                  (cadr res)
                  (car elem)))
              (cdr elem)))
          lis))))
    (case layout
      ((qwerty-us)
        (set! tutcode-kigou-rule
          (tutcode-rule-qwerty-jis-to-qwerty-us
            (tutcode-kigou-rule-pre-translate
              tutcode-rule-qwerty-jis-to-qwerty-us-alist)))
        (set! tutcode-kigou-rule-stroke-help-top-page-alist
          (translate-stroke-help-alist 
            tutcode-kigou-rule-stroke-help-top-page-alist
            tutcode-rule-qwerty-jis-to-qwerty-us-alist)))
      ((dvorak)
        (set! tutcode-kigou-rule
          (tutcode-rule-qwerty-to-dvorak
            (tutcode-kigou-rule-pre-translate
              tutcode-rule-qwerty-to-dvorak-alist)))
        (set! tutcode-kigou-rule-stroke-help-top-page-alist
          (translate-stroke-help-alist 
            tutcode-kigou-rule-stroke-help-top-page-alist
            tutcode-rule-qwerty-to-dvorak-alist))))))

;;; Qwerty-jisからQwerty-usへの変換テーブル。
(define tutcode-rule-qwerty-jis-to-qwerty-us-alist
  '(
    ("^" "=")
    ("@" "[")
    ("[" "]")
    (":" "'")
    ("]" "`")
    ("\"" "@")
    ("'" "&")
    ("&" "^")
    ("(" "*")
    (")" "(")
    ("|" ")") ;tutcode-kigou-rule用。<Shift>0をqwerty-jisでは|で代用してるので
    ("=" "_")
    ("~" "+")
    ("_" "|") ;XXX
    ("`" "{")
    ("{" "}")
    ("+" ":")
    ("*" "\"")
    ("}" "~")))

;;; QwertyからDvorakへの変換テーブル。
(define tutcode-rule-qwerty-to-dvorak-alist
  '(
    ;("1" "1")
    ;("2" "2")
    ;("3" "3")
    ;("4" "4")
    ;("5" "5")
    ;("6" "6")
    ;("7" "7")
    ;("8" "8")
    ;("9" "9")
    ;("0" "0")
    ("-" "[")
    ("^" "]") ;106
    ("q" "'")
    ("w" ",")
    ("e" ".")
    ("r" "p")
    ("t" "y")
    ("y" "f")
    ("u" "g")
    ("i" "c")
    ("o" "r")
    ("p" "l")
    ("@" "/") ;106
    ("[" "=") ;106
    ;("a" "a")
    ("s" "o")
    ("d" "e")
    ("f" "u")
    ("g" "i")
    ("h" "d")
    ("j" "h")
    ("k" "t")
    ("l" "n")
    (";" "s")
    (":" "-") ;106
    ("]" "`")
    ("z" ";")
    ("x" "q")
    ("c" "j")
    ("v" "k")
    ("b" "x")
    ("n" "b")
    ;("m" "m")
    ("," "w")
    ("." "v")
    ("/" "z")
    ;(" " " ")
    ;; shift
    ;("!" "!")
    ("\"" "@") ;106
    ;("#" "#")
    ;("$" "$")
    ;("%" "%")
    ("&" "^") ;106
    ("'" "&") ;106
    ("(" "*") ;106
    (")" "(") ;106
    ("=" "{") ;106
    ("~" "}") ;106
    ("|" ")") ;tutcode-kigou-rule用。<Shift>0をqwerty-jisでは|で代用してるので
    ("_" "|") ;XXX
    ("Q" "\"")
    ("W" "<")
    ("E" ">")
    ("R" "P")
    ("T" "Y")
    ("Y" "F")
    ("U" "G")
    ("I" "C")
    ("O" "R")
    ("P" "L")
    ("`" "?") ;106
    ("{" "+") ;106
    ;("A" "A")
    ("S" "O")
    ("D" "E")
    ("F" "U")
    ("G" "I")
    ("H" "D")
    ("J" "H")
    ("K" "T")
    ("L" "N")
    ("+" "S") ;106
    ("*" "_") ;106
    ("}" "~")
    ("Z" ":")
    ("X" "Q")
    ("C" "J")
    ("V" "K")
    ("B" "X")
    ("N" "B")
    ;("M" "M")
    ("<" "W")
    (">" "V")
    ("?" "Z")
    ))

;;; tutcode-customで設定されたコード表のファイル名からコード表名を作って、
;;; 使用するコード表として設定する。
;;; 作成するコード表名は、ファイル名から".scm"をけずって、
;;; "-rule"がついてなかったら追加したもの。
;;; 例: "tutcode-rule.scm"→tutcode-rule
;;;     "tcode.scm"→tcode-rule
;;; @param filename tutcode-rule-filename
(define (tutcode-custom-load-rule! filename)
  (and
    (try-load filename)
    (let*
      ((basename (last (string-split filename "/")))
       ;; ファイル名から".scm"をけずる
       (bnlist (string-to-list basename))
       (codename
        (or
          (and
            (> (length bnlist) 4)
            (string=? (string-list-concat (list-head bnlist 4)) ".scm")
            (string-list-concat (list-tail bnlist 4)))
          basename))
       ;; "-rule"がついてなかったら追加
       (rulename
        (or
          (and
            (not (string=? (last (string-split codename "-")) "rule"))
            (string-append codename "-rule"))
          codename)))
      (and rulename
        (symbol-bound? (string->symbol rulename))
        (set! tutcode-rule
          (eval (string->symbol rulename) (interaction-environment)))))))

;;; tutcode-key-customで設定された交ぜ書き/部首合成変換開始のキーシーケンスを
;;; コード表に反映する
(define (tutcode-custom-set-mazegaki/bushu-start-sequence!)
  (let
    ((make-subrule
      (lambda (keyseq cmd)
        (and keyseq
             (> (string-length keyseq) 0))
          (let ((keys (reverse (string-to-list keyseq))))
            (list (list keys) cmd)))))
    (tutcode-rule-set-sequences!
      (filter
        pair?
        (list
          (make-subrule tutcode-mazegaki-start-sequence
            '(tutcode-mazegaki-start))
          (make-subrule tutcode-latin-conv-start-sequence
            '(tutcode-latin-conv-start))
          (make-subrule tutcode-bushu-start-sequence
            '(tutcode-bushu-start))
          (and
            tutcode-use-interactive-bushu-conversion?
            (make-subrule tutcode-interactive-bushu-start-sequence
              '(tutcode-interactive-bushu-start)))
          (make-subrule tutcode-postfix-bushu-start-sequence
            '(tutcode-postfix-bushu-start))
          (make-subrule tutcode-postfix-mazegaki-start-sequence
            '(tutcode-postfix-mazegaki-start))
          (make-subrule tutcode-postfix-mazegaki-1-start-sequence
            '(tutcode-postfix-mazegaki-1-start))
          (make-subrule tutcode-postfix-mazegaki-2-start-sequence
            '(tutcode-postfix-mazegaki-2-start))
          (make-subrule tutcode-postfix-mazegaki-3-start-sequence
            '(tutcode-postfix-mazegaki-3-start))
          (make-subrule tutcode-postfix-mazegaki-4-start-sequence
            '(tutcode-postfix-mazegaki-4-start))
          (make-subrule tutcode-postfix-mazegaki-5-start-sequence
            '(tutcode-postfix-mazegaki-5-start))
          (make-subrule tutcode-postfix-mazegaki-6-start-sequence
            '(tutcode-postfix-mazegaki-6-start))
          (make-subrule tutcode-postfix-mazegaki-7-start-sequence
            '(tutcode-postfix-mazegaki-7-start))
          (make-subrule tutcode-postfix-mazegaki-8-start-sequence
            '(tutcode-postfix-mazegaki-8-start))
          (make-subrule tutcode-postfix-mazegaki-9-start-sequence
            '(tutcode-postfix-mazegaki-9-start))
          (make-subrule tutcode-postfix-mazegaki-inflection-start-sequence
            '(tutcode-postfix-mazegaki-inflection-start))
          (make-subrule tutcode-postfix-mazegaki-inflection-1-start-sequence
            '(tutcode-postfix-mazegaki-inflection-1-start))
          (make-subrule tutcode-postfix-mazegaki-inflection-2-start-sequence
            '(tutcode-postfix-mazegaki-inflection-2-start))
          (make-subrule tutcode-postfix-mazegaki-inflection-3-start-sequence
            '(tutcode-postfix-mazegaki-inflection-3-start))
          (make-subrule tutcode-postfix-mazegaki-inflection-4-start-sequence
            '(tutcode-postfix-mazegaki-inflection-4-start))
          (make-subrule tutcode-postfix-mazegaki-inflection-5-start-sequence
            '(tutcode-postfix-mazegaki-inflection-5-start))
          (make-subrule tutcode-postfix-mazegaki-inflection-6-start-sequence
            '(tutcode-postfix-mazegaki-inflection-6-start))
          (make-subrule tutcode-postfix-mazegaki-inflection-7-start-sequence
            '(tutcode-postfix-mazegaki-inflection-7-start))
          (make-subrule tutcode-postfix-mazegaki-inflection-8-start-sequence
            '(tutcode-postfix-mazegaki-inflection-8-start))
          (make-subrule tutcode-postfix-mazegaki-inflection-9-start-sequence
            '(tutcode-postfix-mazegaki-inflection-9-start))
          (make-subrule tutcode-auto-help-redisplay-sequence
            '(tutcode-auto-help-redisplay)))))))

;;; コード表の一部の定義を上書き変更/追加する。~/.uimからの使用を想定。
;;; 呼び出し時にはtutcode-rule-userconfigに登録しておくだけで、
;;; 実際にコード表に反映するのは、tutcode-context-new時。
;;;
;;; (tutcode-rule-filenameの設定が、uim-prefと~/.uimのどちらで行われた場合でも
;;;  ~/.uimでのコード表の一部変更が同じ記述でできるようにするため。
;;;  コード表ロード後のhookを用意した方がいいかも)。
;;;
;;; 呼び出し例:
;;;   (tutcode-rule-set-sequences!
;;;     '(((("d" "l" "u")) ("づ" "ヅ"))
;;;       ((("d" "l" "d" "u")) ("っ" "ッ"))))
;;; @param rules キーシーケンスと入力される文字のリスト
(define (tutcode-rule-set-sequences! rules)
  (set! tutcode-rule-userconfig
    (append rules tutcode-rule-userconfig)))

;;; コード表の上書き変更/追加のためのtutcode-rule-userconfigを
;;; コード表に反映する。
(define (tutcode-rule-commit-sequences! rules)
  (let* ((newseqs ()) ;新規追加するキーシーケンス
         ;; コード表内の指定シーケンスで入力される文字を変更する。
         ;; seq キーシーケンス
         ;; kanji 入力される文字。carがひらがなモード用、cadrがカタカナモード用
         (setseq1!
          (lambda (elem)
            (let* ((seq (caar elem))
                   (kanji (cadr elem))
                   (curseq (rk-lib-find-seq seq tutcode-rule))
                   (pair (and curseq (cadr curseq))))
              (if (and pair (pair? pair))
                (begin
                  (set-car! pair (car kanji))
                  (if (not (null? (cdr kanji)))
                    (if (< (length pair) 2)
                      (set-cdr! pair (list (cadr kanji)))
                      (set-car! (cdr pair) (cadr kanji)))))
                (begin
                  ;; コード表内に指定されたキーシーケンスの定義が無い
                  (set! newseqs (append newseqs (list elem)))))))))
    (for-each setseq1! rules)
    ;; 新規追加シーケンス
    (if (not (null? newseqs))
      (set! tutcode-rule (append tutcode-rule newseqs)))))
