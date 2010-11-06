;;;
;;; Copyright (c) 2003-2010 uim Project http://code.google.com/p/uim/
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
;;;   前置型のみ実装しています。
;;;   再帰的な部首合成変換も可能です。
;;;   部首合成のアルゴリズムはtc-2.1のものです。
;;; 
;;; 【交ぜ書き変換】(alj)
;;;   単純な前置型交ぜ書き変換ができます。
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
;;; * 活用する語の変換は自動的には行いません。
;;;   読みに明示的に"―"を付加して変換してください。
;;;
;;; * 英字変換(SKK abbrev)モードを追加しています(al/)。
;;;   例えば、「file」を入力して「ファイル」に変換する機能です。
;;;
;;; 【ヘルプ機能】
;;; * 仮想鍵盤表示(表形式の候補ウィンドウを流用)
;;;   uim-pref-gtkでの表示・非表示設定の他に、
;;;   <Control>/で一時的に表示・非表示の切り替えも可能です。
;;;   (打ち方があやふやな文字を入力するときだけ表示したい場合があるので)
;;; * 自動ヘルプ表示機能(表形式の候補ウィンドウを流用)
;;;   交ぜ書き変換や部首合成変換で入力した文字の打ち方を表示します。
;;;   部首合成方法も、簡単な合成に関しては表示可能です。
;;;   例:交ぜ書き変換で「憂鬱」を確定した場合
;;;    ┌─┬─┬─┬─┬─┬─┬─────┬─┬─┬─┬─┐
;;;    │  │  │  │  │  │  │          │  │  │  │  │
;;;    ├─┼─┼─┼─┼─┤  ├─────┼─┼─┼─┼─┤
;;;    │  │  │  │  │b │  │          │  │  │f │  │
;;;    ├─┼─┼─┼─┼─┤  ├─────┼─┼─┼─┼─┤
;;;    │  │3 │  │  │  │  │          │  │  │1 │  │
;;;    ├─┼─┼─┼─┼─┤  ├─────┼─┼─┼─┼─┤
;;;    │  │  │d │  │e │  │2a(▲林缶)│  │  │  │  │
;;;    └─┴─┴─┴─┴─┴─┴─────┴─┴─┴─┴─┘
;;;
;;; 【補完/予測入力・熟語ガイド】
;;; * 補完/予測入力・熟語ガイドとも候補ウィンドウに表示します。
;;; * 補完/予測入力機能を使うには、
;;;   uim-pref-gtk等の「補助予測入力」グループの設定で、
;;;   Look-SKKを有効にしてmazegaki.dic相当の辞書を指定するか、
;;;   Lookを有効にして単語ファイルを指定してください。
;;; * 補完/予測入力の開始は以下のいずれかのタイミング:
;;; ** 補完: tutcodeオンの状態でtutcode-completion-chars-minの文字数入力時
;;; ** 補完: tutcodeオンの状態で<Control>.打鍵時
;;; ** 予測入力: 交ぜ書き変換の読み入力状態で
;;;              tutcode-prediction-start-char-countの文字数入力時
;;; ** 予測入力: 交ぜ書き変換の読み入力状態で<Control>.打鍵時
;;; * 熟語ガイド(次に入力が予測される文字の打鍵ガイド)は
;;;   補完/予測入力候補から作っています。
;;; * 補完候補表示にさらに<Control>.を打鍵すると対象文字を1つ減らして再補完。
;;;   長すぎる文字列を対象に補完された場合に、補完し直しができるように。
;;;
;;; 【記号入力モード】
;;;   <Control>_で記号入力モードのトグル。
;;;   全角英数入力モードとしても使えるようにしています。
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

(require-extension (srfi 1 2))
(require "generic.scm")
(require "generic-predict.scm")
(require-custom "tutcode-custom.scm")
(require-custom "generic-key-custom.scm")
(require-custom "tutcode-key-custom.scm")
;;(load-plugin "skk") ;SKK形式の交ぜ書き辞書の検索のため、libuim-skk.soをロード
(require-dynlib "skk")
(require "tutcode-bushudic.scm") ;部首合成変換辞書
(require "tutcode-kigoudic.scm") ;記号入力モード用の記号表
(require "tutcode-dialog.scm"); 交ぜ書き変換辞書からの削除確認ダイアログ

;;; user configs

;; widgets and actions

;; widgets
(define tutcode-widgets '(widget_tutcode_input_mode))

;; default activity for each widgets
(define default-widget_tutcode_input_mode 'action_tutcode_direct)

;; actions of widget_tutcode_input_mode
(define tutcode-input-mode-actions
  '(action_tutcode_direct
    action_tutcode_hiragana
    action_tutcode_katakana
    action_tutcode_kigou))

;;; 使用するコード表。
;;; tutcode-context-new時に(tutcode-custom-load-rule!)で設定
(define tutcode-rule ())
;;; tutcode-ruleから作成する、逆引き検索(漢字から打鍵リストを取得)用alist。
;;; (自動ヘルプ用の部首合成変換候補検索時の高速化のため)
(define tutcode-reverse-rule-alist ())
;;; tutcode-bushudicから作成する、
;;; 逆引き検索(合成後の文字から合成用の2文字を取得)用alist。
;;; (自動ヘルプ用の部首合成変換候補検索時の高速化のため)
(define tutcode-reverse-bushudic-alist ())

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

;;; implementations

;;; 交ぜ書き変換辞書の初期化が終わっているかどうか
(define tutcode-dic-init #f)

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
                          (not (tutcode-context-katakana-mode? tc)))))
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
                          (tutcode-context-katakana-mode? tc))))
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
   '((rk-context    ()) ; キーストロークから文字への変換のためのコンテキスト
     ;;; TUT-Code入力状態
     ;;; 'tutcode-state-off TUT-Codeオフ
     ;;; 'tutcode-state-on TUT-Codeオン
     ;;; 'tutcode-state-yomi 交ぜ書き変換の読み入力中
     ;;; 'tutcode-state-converting 交ぜ書き変換の候補選択中
     ;;; 'tutcode-state-bushu 部首入力・変換中
     ;;; 'tutcode-state-kigou 記号入力モード
     (state 'tutcode-state-off)
     ;;; カタカナモードかどうか
     ;;; #t: カタカナモード。#f: ひらがなモード。
     (katakana-mode #f)
     ;;; 交ぜ書き変換/部首合成変換の対象の文字列リスト(逆順)
     ;;; (例: 交ぜ書き変換で読み「かん字」を入力した場合、("字" "ん" "か"))
     (head ())
     ;;; 交ぜ書き変換の選択中の候補の番号
     (nth 0)
     ;;; 交ぜ書き変換の候補数
     (nr-candidates 0)
     ;;; 候補ウィンドウの状態
     ;;; 'tutcode-candidate-window-off 非表示
     ;;; 'tutcode-candidate-window-converting 交ぜ書き変換候補表示中
     ;;; 'tutcode-candidate-window-kigou 記号表示中
     ;;; 'tutcode-candidate-window-stroke-help 仮想鍵盤表示中
     ;;; 'tutcode-candidate-window-auto-help 自動ヘルプ表示中
     ;;; 'tutcode-candidate-window-predicting 補完/予測入力候補表示中
     (candidate-window 'tutcode-candidate-window-off)
     ;;; ストローク表
     ;;; 次に入力するキーと文字の対応の、get-candidate-handler用形式でのリスト
     (stroke-help ())
     ;;; 交ぜ書き変換辞書への再帰的登録のための子コンテキスト
     (child-context ())
     ;;; 子コンテキストの種類
     ;;; 'tutcode-child-type-editor 登録用の変換後文字列編集エディタ
     ;;; 'tutcode-child-type-dialog 辞書からの削除確認ダイアログ
     (child-type ())
     ;;; 親コンテキスト
     (parent-context ())
     ;;; 登録用文字列編集エディタ
     (editor ())
     ;;; 削除確認ダイアログ
     (dialog ())
     ;;; 英字変換(SKK abbrev)モードかどうか
     (latin-conv #f)
     ;;; commit済の文字列リスト(補完用)
     (commit-strs ())
     ;;; commit-strsのうちで補完に使用している文字数
     (commit-strs-used-len 0)
     ;;; 補完/予測入力の候補選択中かどうか
     ;;; 'tutcode-predicting-off 補完/予測入力の候補選択中でない
     ;;; 'tutcode-predicting-completion 補完候補選択中
     ;;; 'tutcode-predicting-prediction 交ぜ書き変換時の予測入力候補選択中
     (predicting 'tutcode-predicting-off)
     ;;; 補完/予測入力用コンテキスト
     (prediction-ctx ())
     ;;; 補完/予測入力候補の読みのリスト
     (prediction-word ())
     ;;; 補完/予測入力候補の候補のリスト
     (prediction-candidates ())
     ;;; 補完/予測入力候補のappendixのリスト
     (prediction-appendix ())
     ;;; 補完/予測入力候補数
     (prediction-nr 0)
     ;;; 補完/予測入力候補の現在選択されているインデックス(熟語ガイド込み)
     (prediction-index #f)
     ;;; 補完/予測入力候補数(熟語ガイド分含む)
     (prediction-nr-all 0)
     ;;; ページごとの補完/予測入力の候補表示数(熟語ガイド分は除く)
     (prediction-nr-in-page tutcode-nr-candidate-max-for-prediction)
     ;;; ページごとの補完/予測入力の候補表示数(熟語ガイド分も含む)
     (prediction-page-limit
      (+ tutcode-nr-candidate-max-for-prediction
         tutcode-nr-candidate-max-for-guide))
     ;;; 熟語ガイド。
     ;;; 予測される次の入力漢字の第1打鍵と入力漢字の対応のリスト
     ;;; ((<第1打鍵1> (<入力漢字11> (<入力漢字1のストロークリスト>)) ...) ...)
     ;;; 例: (("," ("石" ("," "r"))) ("u" ("屋" ("u" "c")) ("池" ("u" "v"))))
     (guide ())
     )))

(define (tutcode-predict pc str)
  (predict-meta-search
   (tutcode-context-prediction-ctx pc)
   str))
;;; 補完/予測入力候補を検索
;;; @param str 検索文字列
;;; @param completion? 補完の場合は#t
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
    (tutcode-context-set-prediction-nr! pc (length filtered-cands)))
  #f)
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
(define (tutcode-guide-set-candidates pc str completion?)
  (let* ((cands (tutcode-context-prediction-candidates pc))
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
         (trim-cands
          (if (not completion?)
            (map
              (lambda (cand)
                ;; 予測入力時は入力済のstrも含まれているので、
                ;; それより後の文字をガイド表示
                (if (string=? str (substring cand 0 strlen))
                  (substring cand strlen (string-length cand))
                  cand))
              filtered-cands)
            filtered-cands))
         (candchars ; 予測した熟語の1文字目の漢字のリスト
          (delete-duplicates
            (map (lambda (cand) (last (string-to-list cand))) trim-cands)))
         (cand-stroke
          (map
            (lambda (elem)
              (list elem (tutcode-reverse-find-seq elem)))
            candchars))
         (label-cands-alist
          (tutcode-guide-update-alist () cand-stroke)))
    (tutcode-context-set-guide! pc label-cands-alist)))

;;; 熟語ガイドの表示に使うalistを更新する。
;;; alistは以下のようにラベル文字と、漢字とストロークのリスト。
;;; 例: (("," ("石" ("," "r"))) ("u" ("屋" ("u" "c")) ("池" ("u" "v"))))
;;; @param label-cands-alist 元のalist
;;; @param kanji-list 漢字とストロークのリスト
;;; 例: (("石" ("," "r")) ("屋" ("u" "c")) ("池" ("u" "v")))
;;; @return 更新後の熟語ガイド用alist
(define (tutcode-guide-update-alist label-cands-alist kanji-list)
  (if (null? kanji-list)
    label-cands-alist
    (let*
      ((kanji-stroke (car kanji-list))
       (stroke (cadr kanji-stroke)))
      (tutcode-guide-update-alist
        (if (or (not stroke) (null? stroke))
          label-cands-alist
          (tutcode-guide-update-alist-with-stroke
            label-cands-alist kanji-stroke))
        (cdr kanji-list)))))

;;; 熟語ガイド:対象の1文字を、熟語ガイド用alistに追加する。
;;; @param label-cands-alist 元のalist
;;; @param cand-stroke 対象文字とストローク
;;; @return 更新後の熟語ガイドalist
(define (tutcode-guide-update-alist-with-stroke label-cands-alist cand-stroke)
  (let*
    ((label (car (cadr cand-stroke)))
     (label-cand (assoc label label-cands-alist)))
    (if label-cand
      (begin
        (set-cdr! label-cand (cons cand-stroke (cdr label-cand)))
        label-cands-alist)
      (cons (list label cand-stroke) label-cands-alist))))

(define-record 'tutcode-context tutcode-context-rec-spec)
(define tutcode-context-new-internal tutcode-context-new)
(define tutcode-context-katakana-mode? tutcode-context-katakana-mode)
(define (tutcode-context-on? pc)
  (not (eq? (tutcode-context-state pc) 'tutcode-state-off)))

;;; TUT-Codeのコンテキストを新しく生成する。
;;; @return 生成したコンテキスト
(define (tutcode-context-new id im)
  (if (not tutcode-dic-init)
    (if (not (symbol-bound? 'skk-lib-dic-open))
      (begin
        (if (symbol-bound? 'uim-notify-info)
          (uim-notify-info
            (N_ "libuim-skk.so is not available. Mazegaki conversion is disabled")))
        (set! tutcode-use-recursive-learning? #f)
        (set! tutcode-enable-mazegaki-learning? #f))
      (begin
        (skk-lib-dic-open tutcode-dic-filename #f "localhost" 0 'unspecified)
        (if tutcode-use-recursive-learning?
          (require "tutcode-editor.scm"))
        (set! tutcode-dic-init #t)
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
          (require "japanese.scm") ; for ja-wide
          (set! tutcode-kigoudic
            (append
              (map (lambda (lst) (list (ja-wide lst)))
                tutcode-heading-label-char-list-for-kigou-mode)
              (list-tail tutcode-kigoudic
                (length tutcode-heading-label-char-list-for-kigou-mode)))))
        (set! tutcode-heading-label-char-list-for-kigou-mode
          tutcode-uim-heading-label-char-list-for-kigou-mode)))
    (tutcode-context-set-rk-context! tc (rk-context-new tutcode-rule #t #f))
    (if tutcode-use-recursive-learning?
      (tutcode-context-set-editor! tc (tutcode-editor-new tc)))
    (tutcode-context-set-dialog! tc (tutcode-dialog-new tc))
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

;;; 交ぜ書き変換用個人辞書を読み込む。
(define (tutcode-read-personal-dictionary)
  (if (not (setugid?))
      (skk-lib-read-personal-dictionary tutcode-personal-dic-filename)))

;;; 交ぜ書き変換用個人辞書を書き込む。
;;; @param force? tutcode-enable-mazegaki-learning?が#fでも書き込むかどうか
(define (tutcode-save-personal-dictionary force?)
  (if (and
        (or force? tutcode-enable-mazegaki-learning?)
        (not (setugid?)))
      (skk-lib-save-personal-dictionary tutcode-personal-dic-filename)))

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
                n (tutcode-make-string head) "" "" #f)))
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
(define (tutcode-prepare-commit-string pc)
  (let* ((res (tutcode-get-current-candidate pc)))
    ;; いつも特定のラベルキーで特定の候補を確定する使い方ができるように、
    ;; tutcode-enable-mazegaki-learning?が#fの場合は候補の並び順を変えない。
    ;; (例:「かい」の変換において、常にdキーで「悔」、eキーで「恢」を確定)
    (if tutcode-enable-mazegaki-learning?
      (begin
        ;; skk-lib-commit-candidateを呼ぶと学習が行われ、候補順が変更される
        (skk-lib-commit-candidate
          (tutcode-make-string (tutcode-context-head pc)) "" ""
          (tutcode-context-nth pc) #f)
        (if (> (tutcode-context-nth pc) 0)
          (tutcode-save-personal-dictionary #f))))
    (tutcode-flush pc)
    res))

;;; 記号入力モード時に確定した文字列を返す。
(define (tutcode-prepare-commit-string-for-kigou-mode pc)
  (tutcode-get-current-candidate-for-kigou-mode pc))

;;; im-commit-rawを呼び出す。
;;; ただし、子コンテキストの場合は、editorかdialogに入力キーを渡す。
(define (tutcode-commit-raw pc key key-state)
  (if tutcode-use-completion?
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
(define (tutcode-commit pc str)
  (if tutcode-use-completion?
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
         (res (tutcode-prepare-commit-string pc)))
    (tutcode-commit pc res)
    (tutcode-check-auto-help-window-begin pc (string-to-list res) head)))

;;; 交ぜ書き変換の候補選択時に、指定されたラベル文字に対応する候補を確定する
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
(define (tutcode-commit-by-label-key-for-prediction pc ch completion?)
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
        (tutcode-do-commit-prediction pc completion?)))))

(define (tutcode-get-prediction-string pc)
  (tutcode-lib-get-nth-prediction
   pc
   (tutcode-context-prediction-index pc)))

(define (tutcode-learn-prediction-string pc completion?)
  (tutcode-lib-commit-nth-prediction
   pc
   (tutcode-context-prediction-index pc)
   completion?))

(define (tutcode-do-commit-prediction pc completion?)
  (let ((str (tutcode-get-prediction-string pc)))
    (tutcode-learn-prediction-string pc completion?)
    (tutcode-reset-candidate-window pc)
    (tutcode-commit pc str)
    (tutcode-flush pc)
    (tutcode-check-auto-help-window-begin pc (string-to-list str) ())))

;;; 交ぜ書き変換辞書から、現在選択されている候補を削除する。
(define (tutcode-purge-candidate pc)
  (let ((res (skk-lib-purge-candidate
               (tutcode-make-string (tutcode-context-head pc))
               ""
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

;;; 交ぜ書き辞書の検索を行う。
;;; @param pc コンテキストリスト
;;; @param autocommit? 候補が1個の場合に自動的に確定するかどうか
;;; @param recursive-learning? 候補が無い場合に再帰登録モードに入るかどうか
(define (tutcode-begin-conversion pc autocommit? recursive-learning?)
  (let* ((yomi (tutcode-make-string (tutcode-context-head pc)))
         (res (and (symbol-bound? 'skk-lib-get-entry)
                   (skk-lib-get-entry yomi "" "" #f))))
    (if res
      (begin
        (tutcode-context-set-nth! pc 0)
        (tutcode-context-set-nr-candidates! pc
         (skk-lib-get-nr-candidates yomi "" "" #f))
        (tutcode-context-set-state! pc 'tutcode-state-converting)
        (if (and autocommit? (= (tutcode-context-nr-candidates pc) 1))
          ;; 候補が1個しかない場合は自動的に確定する。
          ;; (辞書登録はtutcode-register-candidate-keyを押して明示的に開始する)
          (tutcode-commit-with-auto-help pc)
          (begin
            (tutcode-check-candidate-window-begin pc)
            (if (eq? (tutcode-context-candidate-window pc)
                     'tutcode-candidate-window-converting)
              (im-select-candidate pc 0)))))
      ;; 候補無し
      (if recursive-learning?
        (begin
          (tutcode-context-set-state! pc 'tutcode-state-converting)
          (tutcode-setup-child-context pc 'tutcode-child-type-editor)))
        ;(tutcode-flush pc) ; flushすると入力した文字列が消えてがっかり
        )))

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
  (if (and (eq? (tutcode-context-candidate-window pc)
                'tutcode-candidate-window-off)
           tutcode-use-stroke-help-window?)
    (let* ((rkc (tutcode-context-rk-context pc))
           (seq (rk-context-seq rkc))
           (seqlen (length seq))
           (ret (rk-lib-find-partial-seqs (reverse seq) tutcode-rule))
           (label-cand-alist ())) ; 例:(("k" "あ") ("i" "い") ("v" "□"))
      (for-each
        (lambda (elem) ; 例: ((("r" "v" "y")) ("猿"))
          (let* ((label (nth seqlen (caar elem)))
                 (label-cand (assoc label label-cand-alist)))
            (if (not label-cand)
              (let*
                ((candlist (cadr elem))
                 (cand
                  (or
                    ;; シーケンス途中の場合は□
                    (and (> (length (caar elem)) (+ seqlen 1)) "□")
                    (or
                      (and (not (null? (cdr candlist)))
                           (tutcode-context-katakana-mode? pc)
                           (cadr candlist))
                      (car candlist))))
                 (candstr
                   (case cand
                    ((tutcode-mazegaki-start) "◇")
                    ((tutcode-latin-conv-start) "/")
                    ((tutcode-bushu-start) "◆")
                    (else cand))))
                (set! label-cand-alist
                  (cons (list label candstr) label-cand-alist))))))
        ret)
      (if (and tutcode-use-kanji-combination-guide?
               (pair? seq)
               (pair? (tutcode-context-guide pc)))
        (let*
          ((prevkey (car seq))
           (guide (assoc prevkey (tutcode-context-guide pc)))
           (nextguide
            (if (not guide)
              ()
              (tutcode-guide-update-alist ()
                (map
                  (lambda (elem)
                    ;; elemのstrokeから最初のキーを削除
                    ;; 例: ("屋" ("u" "c")) -> ("屋" ("c"))
                    (list (car elem) (cdr (cadr elem))))
                  (cdr guide)))))
           (nextguide-candcombined
            ;; 例:(("u" ("屋" ("u" "c")) ("池" ("u" "v")))) -> (("u" ">池屋"))
            (map
              (lambda (elem)
                (let*
                  ((cands
                    (map
                      (lambda (e)
                        (car e))
                      (cdr elem)))
                   (last? (= 1 (length (cadr (cadr elem)))))
                   (candlist
                    (if last?
                      (cons tutcode-guide-end-mark cands)
                      (append cands (list tutcode-guide-mark))))
                   (combined (tutcode-make-string candlist)))
                  (list (car elem) combined)))
              nextguide)))
          (tutcode-context-set-guide! pc nextguide)
          ;; 表示する候補文字列を、熟語ガイド(>)付き文字列に置き換える
          (for-each
            (lambda (elem)
              (let*
                ((label (car elem))
                 (label-cand (assoc label label-cand-alist)))
                (if label-cand
                  (set-cdr! label-cand (cdr elem)))))
            nextguide-candcombined)))
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
    (begin
      (set! tutcode-use-stroke-help-window? #t)
      (tutcode-check-stroke-help-window-begin pc))))

;;; 部首合成変換・交ぜ書き変換で確定した文字の打ち方を表示する。
;;; 表形式の候補ウィンドウの場合は、以下のように表示する。
;;; 1が第1打鍵、2が第2打鍵。「携」
;;;  ・・・・    ・・・・
;;;  ・・・・    ・・3 ・
;;;  ・・・・1   ・・・・
;;;  ・・・2     ・・・・
;;; 交ぜ書き変換で複数の文字「携帯」を変換した場合は、以下のように表示する。
;;;  ・・・・    ・・・・
;;;  ・・a ・    ・・3 ・
;;;  ・・・・1b  ・・c ・
;;;  ・・・2     ・・・・
;;; 確定した文字が直接入力できない場合、単純な部首合成変換で入力できれば、
;;; 以下のように部首合成変換方法を表示する。「憂鬱」
;;; ┌─┬─┬─┬─┬─┬─┬─────┬─┬─┬─┬─┐
;;; │  │  │  │  │  │  │          │  │  │  │  │
;;; ├─┼─┼─┼─┼─┤  ├─────┼─┼─┼─┼─┤
;;; │  │  │  │  │b │  │          │  │  │f │  │
;;; ├─┼─┼─┼─┼─┤  ├─────┼─┼─┼─┼─┤
;;; │  │3 │  │  │  │  │          │  │  │1 │  │
;;; ├─┼─┼─┼─┼─┤  ├─────┼─┼─┼─┼─┤
;;; │  │  │d │  │e │  │2a(▲林缶)│  │  │  │  │
;;; └─┴─┴─┴─┴─┴─┴─────┴─┴─┴─┴─┘
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
    (let*
      ((helpstrlist (lset-difference string=? (reverse strlist) yomilist))
       (label-cands-alist
        (if (not tutcode-auto-help-with-real-keys?)
          ;; 表形式の場合の例:(("y" "2" "1") ("t" "3"))
          (tutcode-auto-help-update-stroke-alist
            () tutcode-auto-help-cand-str-list helpstrlist)
          ;; 通常の場合の例:(("暗" "t" "y" "y"))
          (reverse
            (tutcode-auto-help-update-stroke-alist-normal () helpstrlist)))))
      (if (not (null? label-cands-alist))
        (let
          ((stroke-help
            (map
              (lambda (elem)
                (list (tutcode-make-string (cdr elem)) (car elem) ""))
              label-cands-alist)))
          (tutcode-context-set-stroke-help! pc stroke-help)
          (tutcode-context-set-candidate-window! pc
            'tutcode-candidate-window-auto-help)
          (im-activate-candidate-selector pc
            (length stroke-help) tutcode-nr-candidate-max-for-kigou-mode))))))

;;; 自動ヘルプの表形式表示に使うalistを更新する。
;;; alistは以下のように打鍵を示すラベル文字と、該当セルに表示する文字列のリスト
;;;  例:(("y" "2" "1") ("t" "3")) ; ("y" "y" "t")というストロークを現す。
;;;  ・・・・    ・・・・
;;;  ・・・・3 12・・・・
;;;  ・・・・    ・・・・
;;;  ・・・・    ・・・・
;;; @param label-cands-alist 元のalist
;;; @param kanji-list ヘルプ表示対象である、確定された漢字
;;; @param cand-list ヘルプ表示に使う、各打鍵を示す文字のリスト
;;; @return 更新後の自動ヘルプ用alist
(define (tutcode-auto-help-update-stroke-alist label-cands-alist
         cand-list kanji-list)
  (if (or (null? cand-list) (null? kanji-list))
    label-cands-alist
    (tutcode-auto-help-update-stroke-alist
      (tutcode-auto-help-update-stroke-alist-with-kanji
        label-cands-alist (car cand-list) (car kanji-list))
      (cdr cand-list) (cdr kanji-list))))

;;; 自動ヘルプの通常形式表示に使うalistを更新する。
;;; alistは以下のように文字と、文字を入力するためのキーのリスト(逆順)
;;;  例:(("暗" "t" "y" "y"))
;;; @param label-cands-alist 元のalist
;;; @param kanji-list ヘルプ表示対象である、確定された漢字
;;; @return 更新後の自動ヘルプ用alist
(define (tutcode-auto-help-update-stroke-alist-normal label-cands-alist
         kanji-list)
  (if (null? kanji-list)
    label-cands-alist
    (tutcode-auto-help-update-stroke-alist-normal
      (tutcode-auto-help-update-stroke-alist-normal-with-kanji
        label-cands-alist (car kanji-list))
      (cdr kanji-list))))

;;; 自動ヘルプ:対象の1文字を入力するストロークをヘルプ用alistに追加する。
;;; @param label-cands-alist 元のalist
;;; @param cand-list ヘルプ表示に使う、各打鍵を示す文字のリスト
;;; @param kanji ヘルプ表示対象文字
;;; @return 更新後の自動ヘルプ用alist
(define (tutcode-auto-help-update-stroke-alist-with-kanji label-cands-alist
         cand-list kanji)
  (let ((stroke (tutcode-reverse-find-seq kanji)))
    (if stroke
      (tutcode-auto-help-update-stroke-alist-with-stroke
        label-cands-alist (car cand-list) stroke)
      (let ((decomposed (tutcode-auto-help-bushu-decompose kanji)))
        ;; 例: "繋" => (((("," "o"))("撃")) ((("f" "q"))("糸")))
        (if (not decomposed)
          label-cands-alist
          (tutcode-auto-help-update-stroke-alist-with-stroke
            (tutcode-auto-help-update-stroke-alist-with-stroke
              label-cands-alist
              (cons
                (string-append (caar cand-list) "(▲"
                  (caar (cdar decomposed)) (caar (cdadr decomposed)) ")")
                (cdar cand-list))
              (caaar decomposed)) ; 部首1
            (cadr cand-list) (caaadr decomposed))))))) ; 部首2

;;; 自動ヘルプ:対象の1文字を入力するストロークをヘルプ用alistに追加する。
;;; @param label-cands-alist 元のalist
;;; @param kanji ヘルプ表示対象文字
;;; @return 更新後の自動ヘルプ用alist
(define (tutcode-auto-help-update-stroke-alist-normal-with-kanji
          label-cands-alist kanji)
  (let ((stroke (tutcode-reverse-find-seq kanji)))
    (if stroke
      (tutcode-auto-help-update-stroke-alist-normal-with-stroke
        label-cands-alist stroke kanji)
      (let ((decomposed (tutcode-auto-help-bushu-decompose kanji)))
        ;; 例: "繋" => (((("," "o"))("撃")) ((("f" "q"))("糸")))
        (if (not decomposed)
          label-cands-alist
          (tutcode-auto-help-update-stroke-alist-normal-with-stroke
            label-cands-alist
            (cons
              (string-append "▲"
                (caar (cdar decomposed)) (caar (cdadr decomposed)) " ")
              (append
                (caaar decomposed)    ; 部首1
                (list " ")
                (caaadr decomposed))) ; 部首2
            kanji))))))

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

;;; preedit表示を更新する。
(define (tutcode-do-update-preedit pc)
  (let ((stat (tutcode-context-state pc))
        (cpc (tutcode-context-child-context pc)))
    (case stat
      ((tutcode-state-yomi)
        (im-pushback-preedit pc preedit-none "△")
        (let ((h (tutcode-make-string (tutcode-context-head pc))))
          (if (string? h)
            (im-pushback-preedit pc preedit-none h))))
      ((tutcode-state-converting)
        (im-pushback-preedit pc preedit-none "△")
        (if (null? cpc)
          (im-pushback-preedit pc preedit-none
            (tutcode-get-current-candidate pc))
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
      ((tutcode-state-kigou)
        ;; 候補ウィンドウ非表示時でも候補選択できるようにpreedit表示
        (im-pushback-preedit pc preedit-reverse
          (tutcode-get-current-candidate-for-kigou-mode pc))))
    (if (null? cpc)
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
  (let ((ppc (tutcode-context-parent-context pc)))
    (tutcode-flush pc)
    (tutcode-context-set-child-context! pc ())
    (tutcode-context-set-child-type! pc ())
    (tutcode-commit pc str)
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
           (str (tutcode-make-string commit-strs))
           (len (length commit-strs)))
      (if
        (or (>= len tutcode-completion-chars-min)
            (and force-check?
                 (> len 0)))
        (begin
          (tutcode-lib-set-prediction-src-string pc str #t)
          (let ((nr (tutcode-lib-get-nr-predictions pc)))
            (if (and nr (> nr 0))
              (let*
                ((nr-guide
                  (if tutcode-use-kanji-combination-guide?
                    (begin
                      (tutcode-guide-set-candidates pc str #t)
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
           (preconv-str (tutcode-make-string head))
           (preedit-len (length head)))
      (if
        (or (>= preedit-len tutcode-prediction-start-char-count)
            force-check?)
        (begin
          (tutcode-lib-set-prediction-src-string pc preconv-str #f)
          (let ((nr (tutcode-lib-get-nr-predictions pc)))
            (if (and nr (> nr 0))
              (let*
                ((nr-guide
                  (if tutcode-use-kanji-combination-guide?
                    (begin
                      (tutcode-guide-set-candidates pc preconv-str #f)
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
                    (im-activate-candidate-selector pc nr-all page-limit)))))))
        (tutcode-reset-candidate-window pc)))))

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
          ((tutcode-kana-toggle-key? key key-state)
           (rk-flush rkc)
           (tutcode-context-kana-toggle pc))
          ((tutcode-backspace-key? key key-state)
           (if (> (length (rk-context-seq rkc)) 0)
             (rk-flush rkc)
             (begin
               (tutcode-commit-raw pc key key-state)
               (if tutcode-use-completion?
                 (begin
                   (if (> (length (tutcode-context-commit-strs pc)) 0)
                     (tutcode-context-set-commit-strs! pc
                       (cdr (tutcode-context-commit-strs pc))))
                   (if completing?
                     (tutcode-check-completion pc #f 0)))))))
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
              (charcode->string key) #t))
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
             (if res
               (case res
                ((tutcode-mazegaki-start)
                  (tutcode-context-set-latin-conv! pc #f)
                  (tutcode-context-set-state! pc 'tutcode-state-yomi))
                ((tutcode-latin-conv-start)
                  (tutcode-context-set-latin-conv! pc #t)
                  (tutcode-context-set-state! pc 'tutcode-state-yomi))
                ((tutcode-bushu-start)
                  (tutcode-context-set-state! pc 'tutcode-state-bushu)
                  (tutcode-append-string pc "▲"))
                (else
                  (tutcode-commit pc res)
                  (if tutcode-use-completion?
                    (tutcode-check-completion pc #f 0))))
               (tutcode-check-stroke-help-window-begin pc)))))))))

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
      ;; スペースキーで全角スペース入力可能とするため、
      ;; next-candidate-key?のチェックより前にheading-label-char?をチェック
      ((and tutcode-commit-candidate-by-label-key?
            (not (and (modifier-key-mask key-state)
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
      ((or
        (tutcode-commit-key? key key-state)
        (tutcode-return-key? key key-state))
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
          ((tutcode-kana-toggle-key? key key-state)
           (rk-flush rkc)
           (tutcode-context-kana-toggle pc))
          ((tutcode-backspace-key? key key-state)
           (if (> (length (rk-context-seq rkc)) 0)
            (rk-flush rkc)
            (if (> (length (tutcode-context-head pc)) 0)
              (begin
                (tutcode-context-set-head! pc (cdr (tutcode-context-head pc)))
                (if predicting?
                  (tutcode-check-prediction pc #f))))))
          ((or
            (tutcode-commit-key? key key-state)
            (tutcode-return-key? key key-state))
           (tutcode-commit pc (tutcode-make-string (tutcode-context-head pc)))
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
          ;; 候補数が1個の場合、変換後自動確定されてconvertingモードに入らないので
          ;; その場合でもpurgeできるように、ここでチェック
          ((and (tutcode-purge-candidate-key? key key-state)
                (not (null? (tutcode-context-head pc))))
           ;; convertingモードに移行してからpurge
           (tutcode-begin-conversion pc #f #f)
           (if (eq? (tutcode-context-state pc) 'tutcode-state-converting)
             (tutcode-proc-state-converting pc key key-state)))
          ((and (tutcode-register-candidate-key? key key-state)
                tutcode-use-recursive-learning?)
           (tutcode-context-set-state! pc 'tutcode-state-converting)
           (tutcode-setup-child-context pc 'tutcode-child-type-editor))
          ((and predicting? (tutcode-next-page-key? key key-state))
            (tutcode-change-prediction-page pc #t))
          ((and predicting? (tutcode-prev-page-key? key key-state))
            (tutcode-change-prediction-page pc #f))
          ((symbol? key)
           (tutcode-flush pc)
           (tutcode-proc-state-on pc key key-state))
          ((and
            (modifier-key-mask key-state)
            (not (shift-key-mask key-state)))
           ;; <Control>n等での変換開始?
           (if (tutcode-begin-conv-key? key key-state)
             (if (not (null? (tutcode-context-head pc)))
               (tutcode-begin-conversion pc #t tutcode-use-recursive-learning?)
               (tutcode-flush pc))
             (begin
               (tutcode-flush pc)
               (tutcode-proc-state-on pc key key-state))))
          ;; 予測入力候補用ラベルキー?
          ((and predicting? (tutcode-heading-label-char-for-prediction? key))
            (tutcode-commit-by-label-key-for-prediction pc
              (charcode->string key) #f))
          ((not (rk-expect-key? rkc (charcode->string key)))
           (if (> (length (rk-context-seq rkc)) 0)
             (rk-flush rkc)
             ;; spaceキーでの変換開始?
             ;; (spaceはキーシーケンスに含まれる場合があるので、
             ;;  rk-expectにspaceが無いことが条件)
             ;; (trycodeでspaceで始まるキーシーケンスを使っている場合、
             ;;  spaceで変換開始はできないので、<Control>n等を使う必要あり)
             (if (tutcode-begin-conv-key? key key-state)
               (if (not (null? (tutcode-context-head pc)))
                 (tutcode-begin-conversion pc #t tutcode-use-recursive-learning?)
                 (tutcode-flush pc))
               (set! res (charcode->string key)))))
          ((tutcode-context-latin-conv pc)
           (set! res (charcode->string key)))
          (else
           (set! res (tutcode-push-key! pc (charcode->string key)))
           (if (not res)
            (tutcode-check-stroke-help-window-begin pc))))
        (if res
          (begin
            (tutcode-append-string pc res)
            (if tutcode-use-prediction?
              (tutcode-check-prediction pc #f))))))))

;;; 部首合成変換の部首入力状態のときのキー入力を処理する。
;;; @param c コンテキストリスト
;;; @param key 入力されたキー
;;; @param key-state コントロールキー等の状態
(define (tutcode-proc-state-bushu c key key-state)
  (let* ((pc (tutcode-find-descendant-context c))
         (rkc (tutcode-context-rk-context pc))
         (res #f))
    (tutcode-reset-candidate-window pc)
    (cond
      ((tutcode-off-key? key key-state)
       (tutcode-flush pc)
       (tutcode-context-set-state! pc 'tutcode-state-off))
      ((tutcode-kana-toggle-key? key key-state)
       (rk-flush rkc)
       (tutcode-context-kana-toggle pc))
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
      ((or
        (symbol? key)
        (and
          (modifier-key-mask key-state)
          (not (shift-key-mask key-state))))
       (tutcode-flush pc)
       (tutcode-proc-state-on pc key key-state))
      ((not (rk-expect-key? rkc (charcode->string key)))
       (if (> (length (rk-context-seq rkc)) 0)
         (rk-flush rkc)
         (set! res (charcode->string key))))
      (else
       (set! res (tutcode-push-key! pc (charcode->string key)))
       (case res
        ((tutcode-mazegaki-start) ;XXX 部首合成変換中は交ぜ書き変換は無効にする
          (set! res #f))
        ((tutcode-latin-conv-start)
          (set! res #f))
        ((tutcode-bushu-start) ; 再帰的な部首合成変換
          (tutcode-append-string pc "▲")
          (set! res #f))
        ((#f)
	  (tutcode-check-stroke-help-window-begin pc)))))
    (if res
      (let loop ((prevchar (car (tutcode-context-head pc)))
                  (char res))
        (if (string=? prevchar "▲")
          (tutcode-append-string pc char)
          ;; 直前の文字が部首合成マーカでない→2文字目が入力された→変換開始
          (begin
            (set! char
              (tutcode-bushu-convert prevchar char))
            (if (string? char)
              ;; 合成成功
              (begin
                ;; 1番目の部首と▲を消す
                (tutcode-context-set-head! pc (cddr (tutcode-context-head pc)))
                (if (null? (tutcode-context-head pc))
                  ;; 変換待ちの部首が残ってなければ、確定して終了
                  (begin
                    (tutcode-commit pc char)
                    (tutcode-flush pc)
                    (tutcode-check-auto-help-window-begin pc (list char) ()))
                  ;; 部首がまだ残ってれば、再確認。
                  ;; (合成した文字が2文字目ならば、連続して部首合成変換)
                  (loop
                    (car (tutcode-context-head pc))
                    char)))
              ;; 合成失敗時は入力し直しを待つ
              )))))))

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

;;; 次/前ページの補完/予測入力候補を表示する
;;; @param next? #t:次ページ, #f:前ページ
(define (tutcode-change-prediction-page pc next?)
  (let* ((nr-all (tutcode-context-prediction-nr-all pc))
         (idx (tutcode-context-prediction-index pc))
         (page-limit (tutcode-context-prediction-page-limit pc))
         (n (+ idx
              (if next?
                page-limit
                (- page-limit))))
         (compensated-n
          (cond
           ((>= n nr-all) (- nr-all 1))
           ((< n 0) 0)
           (else n))))
    (tutcode-context-set-prediction-index! pc compensated-n)
    (im-select-candidate pc compensated-n)))

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
  (tutcode-reset-candidate-window pc)
  (tutcode-context-set-state! pc 'tutcode-state-yomi)
  (tutcode-context-set-nr-candidates! pc 0))

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
      ((or
        (tutcode-commit-key? key key-state)
        (tutcode-return-key? key key-state))
        (tutcode-commit-with-auto-help pc))
      ((tutcode-purge-candidate-key? key key-state)
        (tutcode-reset-candidate-window pc)
        (tutcode-setup-child-context pc 'tutcode-child-type-dialog))
      ((and (tutcode-register-candidate-key? key key-state)
            tutcode-use-recursive-learning?)
        (tutcode-reset-candidate-window pc)
        (tutcode-setup-child-context pc 'tutcode-child-type-editor))
      ((and tutcode-commit-candidate-by-label-key?
            (tutcode-heading-label-char? key))
        (tutcode-commit-by-label-key pc (charcode->string key)))
      (else
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
;;; @return 対象文字の部首合成に必要な2つの文字とストロークのリスト。
;;;  見つからなかった場合は#f
(define (tutcode-auto-help-bushu-decompose c)
  (let*
    ((bushu (tutcode-bushu-decompose c))
     (b1 (and bushu (car bushu)))
     (b2 (and bushu (cadr bushu)))
     (seq1 (and b1 (tutcode-auto-help-get-stroke b1)))
     (seq2 (and b2 (tutcode-auto-help-get-stroke b2))))
    (or
      ;; 足し算による合成
      (and seq1 seq2
        (list seq1 seq2))
      ;; 単純な引き算による合成
      (tutcode-auto-help-bushu-decompose-by-subtraction c)
      ;; 部品による合成
      (or
        ;; 部首1が直接入力可能
        ;; →(部首1)と(部首2を部品として持つ漢字)による合成が可能か?
        (and seq1 b2
          (tutcode-auto-help-bushu-decompose-looking-bushudic tutcode-bushudic
            () 99
            (lambda (elem)
              (tutcode-auto-help-get-stroke-list-with-right-part
                c b1 b2 seq1 elem))))
        ;; 部首2が直接入力可能
        ;; →(部首2)と(部首1を部品として持つ漢字)による合成が可能か?
        (and seq2 b1
          (tutcode-auto-help-bushu-decompose-looking-bushudic tutcode-bushudic
            () 99
            (lambda (elem)
              (tutcode-auto-help-get-stroke-list-with-left-part
                c b1 b2 seq2 elem))))
        ;; XXX: 部品どうしの合成や、3文字以上での合成は未対応
        ))))

;;; 自動ヘルプ:対象文字を入力する際の打鍵のリストを取得する。
;;; 例: "撃" => ((("," "o")) ("撃"))
;;; @param b 対象文字
;;; @return 打鍵リスト。入力不可能な場合は#f
(define (tutcode-auto-help-get-stroke b)
  (let
    ((seq
      (or (tutcode-reverse-find-seq b)
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
;;; @return 対象文字の部首合成に必要な2つの文字とストロークのリスト。
;;;  見つからなかった場合は#f
(define (tutcode-auto-help-bushu-decompose-by-subtraction c)
  (tutcode-auto-help-bushu-decompose-looking-bushudic tutcode-bushudic
    () 99
    (lambda (elem)
      (tutcode-auto-help-get-stroke-list-by-subtraction c elem))))

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
;;; @param min-stroke-bushu-list min-strokeとbushudic内の要素のリスト。
;;;  例: (6 ((("歹" "リ")) ("列")))
;;; @return 対象文字の部首合成に必要な2つの文字とストロークのリスト。
;;;  bushu-listを使って合成できない場合は#f。
;;;  例: (((("g" "t" "h")) ("列")) ((("G" "I")) ("リ")))
(define (tutcode-auto-help-get-stroke-list-by-subtraction
          c min-stroke-bushu-list)
  (and-let*
    ((min-stroke (car min-stroke-bushu-list))
     (bushu-list (cadr min-stroke-bushu-list))
     (mem (member c (caar bushu-list)))
     (b1 (caadr bushu-list))
     ;; 2つの部首のうち、c以外の部首を取得
     (b2 (if (= 2 (length mem)) (cadr mem) (car (caar bushu-list))))
     (seq1 (tutcode-auto-help-get-stroke b1))
     (seq2 (tutcode-auto-help-get-stroke b2))
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
;;; @param min-stroke-bushu-list min-strokeとbushudic内の要素のリスト。
;;; @return 対象文字の部首合成に必要な2つの文字とストロークのリスト。
;;;  bushu-listを使って合成できない場合は#f。
(define (tutcode-auto-help-get-stroke-list-with-right-part
         c b1 b2 seq1 min-stroke-bushu-list)
  (and-let*
    ((min-stroke (car min-stroke-bushu-list))
     (bushu-list (cadr min-stroke-bushu-list))
     (mem (member b2 (caar bushu-list)))
     (kanji (caadr bushu-list)) ; 部首2を部品として持つ漢字
     (seq (tutcode-auto-help-get-stroke kanji))
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
;;; @param min-stroke-bushu-list min-strokeとbushudic内の要素のリスト。
;;;  例: (6 ((("性" "隹")) ("惟")))
;;; @return 対象文字の部首合成に必要な2つの文字とストロークのリスト。
;;;  bushu-listを使って合成できない場合は#f。
;;;  例: (((("e" "v" ".")) ("惟")) ((("b" ",")) ("言")))
(define (tutcode-auto-help-get-stroke-list-with-left-part
         c b1 b2 seq2 min-stroke-bushu-list)
  (and-let*
    ((min-stroke (car min-stroke-bushu-list))
     (bushu-list (cadr min-stroke-bushu-list))
     (mem (member b1 (caar bushu-list)))
     (kanji (caadr bushu-list)) ; 部首1を部品として持つ漢字
     (seq (tutcode-auto-help-get-stroke kanji))
     (ret (list seq seq2))
     ;; 部首合成は遅いので、先に打鍵数をチェック
     (small-stroke? (< (tutcode-auto-help-count-stroke-length ret) min-stroke))
     ;; 実際に部首合成して、対象文字が合成されないものは駄目
     (composed (tutcode-bushu-convert kanji b2))
     (c-composed? (string=? composed c)))
    ret))

;;; tutcode-ruleを逆引きして、変換後の文字から、入力キー列を取得する。
;;; 例: (tutcode-reverse-find-seq "あ") => ("r" "k")
;;; @param c 変換後の文字
;;; @return 入力キーのリスト。tutcode-rule中にcが見つからなかった場合は#f
(define (tutcode-reverse-find-seq c)
  (if (null? tutcode-reverse-rule-alist)
    (set! tutcode-reverse-rule-alist
      (map
        (lambda (elem)
          (cons (caadr elem) (caar elem)))
        tutcode-rule)))
  (let ((res (assoc c tutcode-reverse-rule-alist)))
    (and res
      (cdr res))))

;;; 現在のstateがpreeditを持つかどうかを返す。
;;; @param pc コンテキストリスト
(define (tutcode-state-has-preedit? pc)
  (or
    (not (null? (tutcode-context-child-context pc)))
    (memq (tutcode-context-state pc)
      '(tutcode-state-yomi tutcode-state-bushu tutcode-state-converting
                           tutcode-state-kigou))))

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
           (if (tutcode-state-has-preedit? c)
             ;; 交ぜ書き変換や部首合成変換開始。△や▲を表示する
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
          (else
           (tutcode-proc-state-off pc key key-state)
           (if (tutcode-state-has-preedit? c) ; 再帰学習時
             (tutcode-update-preedit pc)))))))

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
  (tutcode-context-new id im))

(define (tutcode-release-handler pc)
  (tutcode-save-personal-dictionary #f))

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
               (n (remainder p-idx
                    (length tutcode-heading-label-char-list-for-prediction)))
               (label (nth n tutcode-heading-label-char-list-for-prediction)))
              (list cand label ""))
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
                   (cands
                    (map
                      (lambda (elem)
                        (car elem))
                      (cdr label-cands-alist)))
                   (cand
                    (tutcode-make-string
                      (append cands (list tutcode-guide-mark)))))
                  (list cand label "")))))))
      ;; 仮想鍵盤/自動ヘルプ
      ((and (not (eq? (tutcode-context-state tc) 'tutcode-state-converting))
            (or tutcode-use-stroke-help-window? tutcode-use-auto-help-window?))
        (nth idx (tutcode-context-stroke-help tc)))
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
      ((and (eq? candwin 'tutcode-candidate-window-predicting)
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
               (i (remainder p-idx nr-predictions)))
              (tutcode-context-set-prediction-index! pc i)
              (tutcode-do-commit-prediction pc
                (eq? (tutcode-context-predicting pc)
                     'tutcode-predicting-completion))
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

;;; コード表をQwertyからDvorak用に変換する。
;;; @param qwerty Qwertyのコード表
;;; @return Dvorakに変換したコード表
(define (tutcode-rule-qwerty-to-dvorak qwerty)
  (map
    (lambda (elem)
      (cons
        (list
          (map
            (lambda (key)
              (cadr (assoc key tutcode-rule-qwerty-to-dvorak-alist)))
            (caar elem)))
        (cdr elem)))
    qwerty))

;;; QwertyからDvorakへの変換テーブル。
(define tutcode-rule-qwerty-to-dvorak-alist
  '(
    ;漢直で使うキー以外はコメントアウト
    ("1" "1")
    ("2" "2")
    ("3" "3")
    ("4" "4")
    ("5" "5")
    ("6" "6")
    ("7" "7")
    ("8" "8")
    ("9" "9")
    ("0" "0")
    ;("-" "[")
    ;("^" "]") ;106
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
    ;("@" "/") ;106
    ;("[" "=") ;106
    ("a" "a")
    ("s" "o")
    ("d" "e")
    ("f" "u")
    ("g" "i")
    ("h" "d")
    ("j" "h")
    ("k" "t")
    ("l" "n")
    (";" "s")
    ;(":" "-") ;106
    ("z" ";")
    ("x" "q")
    ("c" "j")
    ("v" "k")
    ("b" "x")
    ("n" "b")
    ("m" "m")
    ("," "w")
    ("." "v")
    ("/" "z")
    ;; shift
    ;("\"" "@") ;106
    ;("&" "^") ;106
    ;("'" "&") ;106
    ;("(" "*") ;106
    ;(")" "(") ;106
    ;("=" "{") ;106
    ;("~" "}") ;106
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
    ;("`" "?") ;106
    ;("{" "+") ;106
    ("A" "A")
    ("S" "O")
    ("D" "E")
    ("F" "U")
    ("G" "I")
    ("H" "D")
    ("J" "H")
    ("K" "T")
    ("L" "N")
    ("+" "S") ;106
    ;("*" "_") ;106
    ("Z" ":")
    ("X" "Q")
    ("C" "J")
    ("V" "K")
    ("B" "X")
    ("N" "B")
    ("M" "M")
    ("<" "W")
    (">" "V")
    ("?" "Z")
    (" " " ")
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
  (let*
    ((make-subrule
      (lambda (keyseq cmd)
        (if
          (and
            keyseq
            (> (string-length keyseq) 0))
          (let ((keys (reverse (string-to-list keyseq))))
            (list (list (list keys) cmd)))
          #f)))
     (mazegaki-rule
      (make-subrule tutcode-mazegaki-start-sequence
        '(tutcode-mazegaki-start)))
     (latin-conv-rule
      (make-subrule tutcode-latin-conv-start-sequence
        '(tutcode-latin-conv-start)))
     (bushu-rule
      (make-subrule tutcode-bushu-start-sequence
        '(tutcode-bushu-start))))
    (if mazegaki-rule
      (tutcode-rule-set-sequences! mazegaki-rule))
    (if latin-conv-rule
      (tutcode-rule-set-sequences! latin-conv-rule))
    (if bushu-rule
      (tutcode-rule-set-sequences! bushu-rule))))

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
      (set! tutcode-rule (append newseqs tutcode-rule)))))
