;;;
;;; Copyright (c) 2003-2013 uim Project https://github.com/uim/uim
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
;;;   部首合成のアルゴリズムは以下の3つから選択可能です。
;;;     - tc-2.1+[tcode-ml:1925]
;;;     - 漢直Win YAMANOBE
;;;     - tc-2.3.1-22.6 (対話的な部首合成変換と同じ処理を使用するので、
;;;                      bushu.index2とbushu.expandファイルの設定が必要)
;;;
;;; * 対話的な部首合成変換
;;;   tc-2.3.1のtc-bushu.elの移植です(ただしsortでの打ちやすさの考慮は未対応)。
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
;;;       tc2と同様に、先頭数個の候補順を変えたくない場合は、
;;;       tutcode-mazegaki-fixed-priority-countにその個数を設定してください。
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
;;;   uimのsurrounding text関係のAPI(text acquisition API)を使って、
;;;   カーソル前の文字列の取得・削除を行います。
;;;   そのため、uimのsurrounding text APIをサポートしているブリッジ
;;;   (uim-gtk, uim-qt3, uim-qt4)でのみ後置型変換が可能です。
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
;;; * 後置型カタカナ変換は、以下の開始キーを設定すると使用可能になります。
;;;  対象伸縮モード   tutcode-postfix-katakana-start-sequence
;;;    カタカナ変換対象の文字列を選択するモードを開始(後置型交ぜ書き変換同様)。
;;;  ひらがなが続く間 tutcode-postfix-katakana-0-start-sequence
;;;    ひらがなや「ー」が続く間対象文字列として、カタカナに置換。
;;;  対象1文字        tutcode-postfix-katakana-1-start-sequence
;;;   ...
;;;  対象9文字        tutcode-postfix-katakana-9-start-sequence
;;;    指定文字数をカタカナに置換。
;;;  1文字除いて置換  tutcode-postfix-katakana-exclude-1-sequence
;;;   ...
;;;  6文字除いて置換  tutcode-postfix-katakana-exclude-6-sequence
;;;    指定文字数をひらがなとして残してカタカナに置換。
;;;    (カタカナに変換する文字列が長くて文字数を数えるのが面倒な場合向け)
;;;    例:「例えばあぷりけーしょん」2文字除いて置換→「例えばアプリケーション」
;;;  1文字縮める      tutcode-postfix-katakana-shrink-1-sequence
;;;   ...
;;;  6文字縮める      tutcode-postfix-katakana-shrink-6-sequence
;;;    直前の後置型カタカナ変換を指定文字数縮めます。繰り返し実行可能。
;;;    例:「例えばあぷりけーしょん」ひらがなが続く間置換
;;;     →「例エバアプリケーション」1文字縮める
;;;     →「例えバアプリケーション」1文字縮める
;;;     →「例えばアプリケーション」
;;; * 後置型漢字→入力シーケンス変換
;;;   TUT-Codeオン・オフのモード切り替えなしで英単語を入力して、
;;;   後から英字化するための機能です。
;;;   tutcode-keep-illegal-sequence?を#tに設定した上で、
;;;   英単語入力後に、tutcode-verbose-stroke-key(デフォルトはスペースキー)を
;;;   打鍵することでシーケンスを終端した後に、以下の開始キーを入力して下さい。
;;;   (変換後に、最後のtutcode-verbose-stroke-keyは自動削除します)
;;;   例:"undo "と打鍵すると"趣・"と表示され、以下の開始キーで、"undo"に変換。
;;;               tutcode-postfix-kanji2seq-start-sequence
;;;     漢字1文字 tutcode-postfix-kanji2seq-1-start-sequence
;;;      ...
;;;     漢字9文字 tutcode-postfix-kanji2seq-9-start-sequence
;;;   文字数を指定しない場合、<と>キーにより、字数の伸縮が可能です。
;;;   文字数を指定しない場合、英単語入力前にスペースを入力しておくと、
;;;   スペースより後の文字を英字に変換します。
;;;   このとき、英単語の区切りのために入力したスペースを自動削除するには、
;;;   tutcode-delete-leading-delimiter-on-postfix-kanji2seq?を
;;;   #tに設定してください。
;;;   例:" code "と打鍵すると" 演各 "と表示され、開始キーで、"code"に変換。
;;;   なお、文字数指定なしの場合でも、確定操作なしで英字に置換したければ、
;;;   ~/.uimに以下を記述(開始キーを";0"にする例):
;;;   (tutcode-rule-set-sequences!
;;;     `((((";" "0"))
;;;         (,(lambda (state pc)
;;;           (tutcode-begin-postfix-kanji2seq-conversion pc 0))))))
;;;   注:英単語中に前置型交ぜ書き変換開始などの機能に対するシーケンスが
;;;      含まれていると、該当する機能が実行されてしまいます。
;;;      現状では、それらの機能に対するシーケンスを、
;;;      英単語中では出現しないシーケンスに変更することで回避してください。
;;;      例:"/local/"と打つと"授阪"の後に"al/"により前置型英字変換モードが開始
;;;         (なお、"local/"の場合は"薬児適"なので問題なし)
;;; * 後置型入力シーケンス→漢字変換
;;;   TUT-Codeオンにし忘れてTUT-Codeを入力した場合に後から漢字に変換するための
;;;   機能です。
;;;           tutcode-postfix-seq2kanji-start-sequence
;;;     1文字 tutcode-postfix-seq2kanji-1-start-sequence
;;;      ...
;;;     9文字 tutcode-postfix-seq2kanji-9-start-sequence
;;;   前置型交ぜ書き変換の読み入力など、確定されていない入力は消えます。
;;;   例:"aljekri"を変換→""。"ekri"だけ変換→"かい"。
;;;      "aljekri \n"のように確定されている場合→"下位"
;;;
;;; 【selectionに対する変換】
;;;   uimのsurrounding text関係のAPI(text acquisition API)を使って、
;;;   selection文字列の取得・削除を行います。
;;; * 交ぜ書き変換
;;;     活用しない語  tutcode-selection-mazegaki-start-sequence
;;;     活用する語    tutcode-selection-mazegaki-inflection-start-sequence
;;; * カタカナ変換    tutcode-selection-katakana-start-sequence
;;; * 漢字→入力シーケンス変換  tutcode-selection-kanji2seq-start-sequence
;;; * 入力シーケンス→漢字変換  tutcode-selection-seq2kanji-start-sequence
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
;;;   部首合成方法のヘルプは、bushu.helpファイルが設定されていれば
;;;   二分探索して表示します。bushu.help内に見つからない場合でも、
;;;   簡単な部首合成に関しては表示可能です。
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
;;; * 文字ヘルプ表示機能(tutcode-help-sequence)
;;;   カーソル位置直前の文字のヘルプ(打ち方)を表示します。
;;;   (uimのsurrounding text APIを使ってカーソル位置直前の文字を取得)
;;; * 直近に表示した(自動)ヘルプの再表示(tutcode-auto-help-redisplay-sequence)
;;; * 直近に表示した(自動)ヘルプのダンプ(tutcode-auto-help-dump-sequence)
;;;   候補ウィンドウに表示したヘルプ内容を以下のような文字列にしてcommitします。
;;;   (部首合成シーケンス(例:"林缶")をコピーして、後でクリップボードから
;;;    前置型部首合成変換のpreeditへペーストして変換したい場合向け)
;;;       |  |  |  |  ||            |  |  |     |  ||
;;;       |  |  |  | b||            |  |  |  f  |  ||
;;;       | 3|  |  |  ||            |  |  |1(憂)|  ||
;;;       |  | d|  | e||2a(鬱▲林缶)|  |  |     |  ||
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
;;; * 仮想鍵盤上での熟語ガイド表示
;;;   熟語ガイドで表示されている+付き文字に対応するキーを入力した場合、
;;;   2打鍵目以降も仮想鍵盤上に+付きで表示するので、
;;;   ガイドに従って漢字の入力が可能です。
;;;   通常は仮想鍵盤非表示の場合でも、+付き文字に対応するキーを入力した場合、
;;;   一時的に仮想鍵盤を表示するには、
;;;   tutcode-stroke-help-with-kanji-combination-guideを'full(+付き以外の
;;;   文字も表示)か'guide-only(+付きの文字のみ表示)に設定してください。
;;;     例:「火蓋」を入力しようとして「火」の入力後「蓋」の打ち方を
;;;        ど忘れした場合、<Control>.キーで補完。熟語ガイドで+付きの「蓋」の
;;;        表示に従って1,2,3打鍵を入力。
;;;
;;; * 次の打鍵がしばらく無い場合に補完/予測入力候補を表示するには、
;;;   候補ウィンドウが遅延表示に対応していれば、以下の設定で可能です。
;;;     tutcode-candidate-window-use-delay?を#tに設定し、
;;;     tutcode-candidate-window-activate-delay-for-{completion,prediction}
;;;     の値を1[秒]以上に設定。
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
;;; 【漢字コード入力モード】
;;;   漢字コードを指定して文字を入力するモード。漢字コード入力後スペースキー
;;;   (tutcode-begin-conv-key)を押すと、対応する文字が確定されます。
;;;   以下の3種類の形式での入力が可能(DDSKK 14.2と同様)。
;;; + Unicode(UCS): U+の後に16進数。U+のかわりにuでもOK。(例:U+4E85またはu4e85)
;;;                 (ただし、uim-tutcodeの内部コードはEUC-JP(EUC-JIS-2004)なの
;;;                  で、JIS X 0213に無い文字(例:はしご高U+9AD9)は入力不可)
;;; + 区点番号(JIS X 0213): -で区切った、面-区-点番号(面区点それぞれ10進数)。
;;;                         1面の場合、面-は省略可能。(例:1-48-13または48-13)
;;; + JISコード(ISO-2022-JP): 4桁の16進数。(例:502d)
;;;
;;; 【ヒストリ入力モード】
;;;   最近の部首合成変換や交ぜ書き変換、補完/予測入力、記号入力、
;;;   漢字コード入力で確定した文字列を再入力するモード。
;;;   tutcode-history-sizeを1以上に設定すると有効になります。
;;;
;;; 【確定取り消し】
;;;   直前の確定を取り消します(tutcode-undo-sequence)。
;;;   以下の変換では、変換後に確定される文字列を確認する機会無しに
;;;   確定を行うので、意図しない漢字に確定されることがあります。
;;;   この場合に、最初からの入力し直しを不要するための機能です。
;;;   (確定済文字列を削除するため、uimのsurrounding text APIを使います)
;;;   + 部首合成変換: 2文字目の部首入力により変換・確定開始(特に再帰的な場合)
;;;   + 漢字コード入力
;;;   + 交ぜ書き変換: 候補数が1個の時の自動確定
;;;   + 読み入力時のカタカナ確定、漢字→入力シーケンス確定
;;;
;;; 【クリップボード】
;;;  クリップボード内の文字列に対する以下の機能を追加しました。
;;;  (クリップボードからの文字列取得のため、uimのsurrounding text APIを使用)
;;;  * クリップボード内の文字列に対してヘルプ(打ち方)を表示
;;;    (tutcode-help-clipboard-sequence)
;;;  * クリップボード内の文字列を以下のpreeditに貼り付け(tutcode-paste-key)
;;;   + 辞書登録時
;;;   + 交ぜ書き変換の読み入力時
;;;   + 部首合成変換時("言▲▲西一早"のような部首合成シーケンスにも対応)
;;;   + 対話的な部首合成変換時
;;;   + 漢字コード入力時
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
;;;  * 交ぜ書き変換ではSKK形式の辞書を使うので、
;;;    skk.scmのかな漢字変換処理から必要な部分を取り込み。
;;;  * 部首合成変換機能を追加。
;;;  * 記号入力モードを追加。
;;;  * 仮想鍵盤表示機能を追加。
;;;  * 自動ヘルプ表示機能を追加。
;;;  * 補完/予測入力・熟語ガイド機能を追加。
;;; 【候補ウィンドウの遅延表示】
;;; 遅延表示の目的:ユーザが入力時にヘルプや補完の処理完了を待たずにすむように。
;;;  + 自動ヘルプの作成に少し時間がかかるため、自動ヘルプが表示されるまでの間に
;;;    以降の文字のキー入力をしても入力した文字が表示されない問題に対処
;;;  + 一定時間キー入力が無い場合のみヘルプ(仮想鍵盤)や補完/予測入力候補表示
;;;    (迷わず入力している間は余計なヘルプは表示しない)
;;; 遅延表示の流れ:
;;; candwin                        tutcode.scm
;;; [表示のみを遅延する場合]
;;;                                候補リストを作成しnr,display_limitを計算
;;;                            <-- im-delay-activate-candidate-selector
;;;  タイマ設定して待つ
;;;  タイマ満了
;;;                            --> delay-activating-handler
;;;                                nr,display_limit,indexを返す
;;;                            --> get-candidate-handler (候補を返す)
;;;  候補表示
;;;
;;; [候補リストの作成も遅延する場合]
;;;                            <-- im-delay-activate-candidate-selector
;;;  タイマ設定して待つ
;;;  タイマ満了
;;;                            --> delay-activating-handler
;;;                                候補リストを作成し、
;;;                                nr,display_limit,indexを返す
;;;                            --> get-candidate-handler (候補を返す)
;;;  候補表示
;;;
;;; (タイマ満了前にキー入力等によりim-{de,}activate-candidate-selector
;;;  が呼ばれたらタイマキャンセル)

(require-extension (srfi 1 2 8 69))
(require "generic.scm")
(require "generic-predict.scm")
(require-custom "tutcode-custom.scm")
(require-custom "generic-key-custom.scm")
(require-custom "tutcode-key-custom.scm")
(require-custom "tutcode-rule-custom.scm");uim-prefへ表示のため(tcode時は無用)
(require-dynlib "skk") ;SKK形式の交ぜ書き辞書の検索のためlibuim-skk.soをロード
(require "tutcode-bushudic.scm") ;部首合成変換辞書
(require "tutcode-kigoudic.scm") ;記号入力モード用の記号表
(require "tutcode-dialog.scm"); 交ぜ書き変換辞書からの削除確認ダイアログ
(require "tutcode-bushu.scm")
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
;;; tutcode-ruleから作成する、逆引き検索(漢字から打鍵リストを取得)用hash-table
;;; (自動ヘルプ用の部首合成変換候補検索時の高速化のため)
(define tutcode-reverse-rule-hash-table ())
;;; tutcode-kigou-ruleから作成する、逆引き検索用hash-table。
(define tutcode-reverse-kigou-rule-hash-table ())
;;; tutcode-bushudicから作成する、
;;; 逆引き検索(合成後の文字から合成用の2文字を取得)用hash-table。
;;; (自動ヘルプ用の部首合成変換候補検索時の高速化用。ただし初回作成時が遅い)
(define tutcode-reverse-bushudic-hash-table ())
;;; stroke-helpで、何もキー入力が無い場合に表示する内容のalist。
;;; 表示したくない場合は~/.uimで()に設定するか、
;;; tutcode-show-stroke-help-window-on-no-input?を#fに設定する。
;;; (毎回tutcode-ruleを全てなめて作成すると遅いし、
;;; 最初のページは固定内容なので、一度作成したものを使い回す)
(define tutcode-stroke-help-top-page-alist #f)
;;; stroke-helpで、何もキー入力が無い場合に表示する内容のalist。
;;; カタカナモード用。
;;; (XXX:キー入力有の場合もキャッシュを使うようにする?
;;;  もしそうすれば、~/.uimで仮想鍵盤表示内容のカスタマイズも容易になる)
(define tutcode-stroke-help-top-page-katakana-alist #f)

;;; コード表を上書き変更/追加するためのコード表。
;;; ~/.uimでtutcode-rule-set-sequences!で登録して、
;;; tutcode-context-new時に反映する。
(define tutcode-rule-userconfig ())

;;; 旧版の設定を反映
(if (and (symbol-bound? 'tutcode-use-table-style-candidate-window?)
         tutcode-use-table-style-candidate-window?)
  (set! candidate-window-style 'table))
(if (symbol-bound? 'tutcode-commit-candidate-by-label-key?)
  (set! tutcode-commit-candidate-by-label-key
    (if tutcode-commit-candidate-by-label-key?
      'always
      'never)))

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

;;; ヒストリ入力時の候補選択用ラベル文字のリスト
(define tutcode-heading-label-char-list-for-history ())

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
  '((("1" "2" "3") ("4" "5" "6") ("7" "8" "9")) ; 1文字目用
    (("a" "b" "c") ("d" "e" "f") ("g" "h" "i")) ; 2文字目用
    (("A" "B" "C") ("D" "E" "F") ("G" "H" "I"))
    (("一" "二" "三") ("四" "五" "六") ("七" "八" "九"))
    (("あ" "い" "う") ("か" "き" "く") ("さ" "し" "す"))
    (("ア" "イ" "ウ") ("カ" "キ" "ク") ("サ" "シ" "ス"))))

;;; 自動ヘルプ作成時間上限[s]
(define tutcode-auto-help-time-limit 3)

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

;;; 後置型カタカナ変換の対象取得時に、(ひらがなに加えて)対象にする文字のリスト
(define tutcode-postfix-katakana-char-list '("ー"))

;;; 後置型漢字→入力シーケンス変換の読み取得時に、読みに含めない文字のリスト。
;;; スペースを含む英単語の変換を楽にしたい場合、'(":")等にすることを想定。
;;; ("\n" "\t"は別扱い。tutcode-delete-leading-delimiter-on-postfix-kanji2seq?
;;;  が#tの場合でも削除しないようにするため)
(define tutcode-postfix-kanji2seq-delimiter-char-list '(" "))

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
     ;;; 'tutcode-state-code 漢字コード入力中
     ;;; 'tutcode-state-converting 交ぜ書き変換の候補選択中
     ;;; 'tutcode-state-bushu 部首入力・変換中
     ;;; 'tutcode-state-interactive-bushu 対話的部首合成変換中
     ;;; 'tutcode-state-kigou 記号入力モード
     ;;; 'tutcode-state-history ヒストリ入力モード
     ;;; 'tutcode-state-postfix-katakana 後置型カタカナ変換中
     ;;; 'tutcode-state-postfix-kanji2seq 後置型漢字→入力シーケンス変換中
     ;;; 'tutcode-state-postfix-seq2kanji 後置型入力シーケンス→漢字変換中
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
     ;;; 後置型(正)か前置型(0)かselection型(負)かの判定にも使用。
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
     ;;; 'tutcode-candidate-window-history ヒストリ入力候補表示中
     (list 'candidate-window 'tutcode-candidate-window-off)
     ;;; 候補ウィンドウの遅延表示待ち中かどうか
     (list 'candwin-delay-waiting #f)
     ;;; 候補ウィンドウの遅延表示待ち中に選択された候補のインデックス番号
     (list 'candwin-delay-selected-index -1)
     ;;; 擬似表形式候補表示用の候補vector(各要素が各ページの候補リスト)
     (list 'pseudo-table-cands #f)
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
     ;;; 'tutcode-child-type-seq2kanji シーケンス→漢字変換用
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
     ;;; commitした文字列の履歴(ヒストリ入力用)
     (list 'history ())
     ;;; 後置型変換の確定をundoするためのデータ
     (list 'undo ())
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
  (im-set-delay-activating-handler! im tutcode-delay-activating-handler)
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
      (if (or (eq? candidate-window-style 'table)
              tutcode-use-pseudo-table-style?)
        (set! tutcode-heading-label-char-list
          (case tutcode-candidate-window-table-layout
            ((qwerty-jis) tutcode-table-heading-label-char-list-qwerty-jis)
            ((qwerty-us) tutcode-table-heading-label-char-list-qwerty-us)
            ((dvorak) tutcode-table-heading-label-char-list-dvorak)
            (else tutcode-table-heading-label-char-list)))
        (set! tutcode-heading-label-char-list
          tutcode-uim-heading-label-char-list)))
    (if (null? tutcode-heading-label-char-list-for-history)
      (set! tutcode-heading-label-char-list-for-history
        tutcode-heading-label-char-list))
    (if (null? tutcode-heading-label-char-list-for-kigou-mode)
      (if (or (eq? candidate-window-style 'table)
              tutcode-use-pseudo-table-style?)
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
          (string-list-concat
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
      (begin
        (tutcode-context-set-guide-chars! pc ())
        (if
          (and
            (not (null? (cdr res)))
            (tutcode-context-katakana-mode? pc))
          (cadr res)
          (car res))))))

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
    (tutcode-context-set-guide-chars! pc ())
    (tutcode-context-set-child-context! pc ())
    (tutcode-context-set-child-type! pc ())
    (if (not (null? cpc))
      (tutcode-flush cpc))))

;;; 交ぜ書き変換中のn番目の候補を返す。
;;; @param pc コンテキストリスト
;;; @param n 対象の候補番号
(define (tutcode-get-nth-candidate pc n)
  (let* ((head (tutcode-context-head pc))
         (cand (skk-lib-get-nth-candidate
                tutcode-dic
                n
                (cons (string-list-concat head) "")
                ""
                #f)))
    cand))

;;; 記号入力モード時のn番目の候補を返す。
;;; @param n 対象の候補番号
(define (tutcode-get-nth-candidate-for-kigou-mode pc n)
 (car (nth n tutcode-kigoudic)))

;;; ヒストリ入力モード時のn番目の候補を返す。
;;; @param n 対象の候補番号
(define (tutcode-get-nth-candidate-for-history pc n)
  (list-ref (tutcode-context-history pc) n))

;;; 交ぜ書き変換中の現在選択中の候補を返す。
;;; @param pc コンテキストリスト
(define (tutcode-get-current-candidate pc)
  (tutcode-get-nth-candidate pc (tutcode-context-nth pc)))

;;; 記号入力モード時の現在選択中の候補を返す。
(define (tutcode-get-current-candidate-for-kigou-mode pc)
  (tutcode-get-nth-candidate-for-kigou-mode pc (tutcode-context-nth pc)))

;;; ヒストリ入力モード時の現在選択中の候補を返す。
(define (tutcode-get-current-candidate-for-history pc)
  (tutcode-get-nth-candidate-for-history pc (tutcode-context-nth pc)))

;;; 交ぜ書き変換で確定した文字列を返す。
;;; @param pc コンテキストリスト
;;; @return 確定した文字列
(define (tutcode-prepare-commit-string pc)
  (let ((res (tutcode-get-current-candidate pc))
        (suffix (tutcode-context-mazegaki-suffix pc))
        (nth (tutcode-context-nth pc)))
    ;; いつも特定のラベルキーで特定の候補を確定する使い方ができるように、
    ;; tutcode-enable-mazegaki-learning?が#fの場合は候補の並び順を変えない。
    ;; (例:「かい」の変換において、常にdキーで「悔」、eキーで「恢」を確定)
    (if (and tutcode-enable-mazegaki-learning?
             (> nth tutcode-mazegaki-fixed-priority-count))
      (let ((head-and-okuri-head
              (cons (string-list-concat (tutcode-context-head pc)) "")))
        ;; skk-lib-commit-candidateを呼ぶと学習が行われ、候補順が変更される
        (skk-lib-commit-candidate tutcode-dic head-and-okuri-head "" nth #f)
        ;; 先頭数個の候補順固定のため、前行の呼出で先頭になった候補を押し下げる
        (do
          ((i tutcode-mazegaki-fixed-priority-count (- i 1)))
          ((<= i 0))
          (skk-lib-commit-candidate tutcode-dic head-and-okuri-head ""
            tutcode-mazegaki-fixed-priority-count #f))
        (tutcode-save-personal-dictionary #f)))
    (tutcode-flush pc)
    (if (null? suffix)
      res
      (string-append res (string-list-concat suffix)))))

;;; 記号入力モード時に確定した文字列を返す。
(define (tutcode-prepare-commit-string-for-kigou-mode pc)
  (tutcode-get-current-candidate-for-kigou-mode pc))

;;; ヒストリ入力モード時に確定した文字列を返す。
(define (tutcode-prepare-commit-string-for-history pc)
  (tutcode-get-current-candidate-for-history pc))

;;; im-commit-rawを呼び出す。
;;; ただし、子コンテキストの場合は、editorかdialogに入力キーを渡す。
(define (tutcode-commit-raw pc key key-state)
  (tutcode-context-set-undo! pc ())
  (if (or tutcode-use-completion? tutcode-enable-fallback-surrounding-text?)
    (tutcode-append-commit-string pc (im-get-raw-key-str key key-state)))
  (let ((ppc (tutcode-context-parent-context pc)))
    (if (not (null? ppc))
      (case (tutcode-context-child-type ppc)
        ((tutcode-child-type-editor)
          (tutcode-editor-commit-raw (tutcode-context-editor ppc) key key-state))
        ((tutcode-child-type-dialog)
          (tutcode-dialog-commit-raw (tutcode-context-dialog ppc) key key-state))
        ((tutcode-child-type-seq2kanji)
          (tutcode-seq2kanji-commit-raw-from-child ppc key key-state)))
      (im-commit-raw pc))))

;;; im-commitを呼び出す。
;;; ただし、子コンテキストの場合は、editorかdialogに入力キーを渡す。
;;; @param str コミットする文字列
;;; @param opts オプション引数。
;;;  opt-skip-append-commit-strs? commit-strsへの追加を
;;;  スキップするかどうか。未指定時は#f。
;;;  opt-skip-append-history? historyへの追加を
;;;  スキップするかどうか。未指定時は#f。
(define (tutcode-commit pc str . opts)
  (tutcode-context-set-undo! pc ())
  (let-optionals* opts ((opt-skip-append-commit-strs? #f)
                        (opt-skip-append-history? #f))
    (if (and
          (or tutcode-use-completion? tutcode-enable-fallback-surrounding-text?)
          (not opt-skip-append-commit-strs?))
      (tutcode-append-commit-string pc str))
    (if (and (> tutcode-history-size 0)
             (not opt-skip-append-history?))
      (tutcode-append-history pc str)))
  (let ((ppc (tutcode-context-parent-context pc)))
    (if (not (null? ppc))
      (case (tutcode-context-child-type ppc)
        ((tutcode-child-type-editor)
          (tutcode-editor-commit (tutcode-context-editor ppc) str))
        ((tutcode-child-type-dialog)
          (tutcode-dialog-commit (tutcode-context-dialog ppc) str))
        ((tutcode-child-type-seq2kanji)
          (tutcode-seq2kanji-commit-from-child ppc str)))
      (im-commit pc str))))

;;; im-commitを呼び出すとともに、自動ヘルプ表示のチェックを行う
(define (tutcode-commit-with-auto-help pc)
  (let* ((head (tutcode-context-head pc))
         (yomi-len (tutcode-context-postfix-yomi-len pc))
         (yomi (and (not (zero? yomi-len))
                    (take (tutcode-context-mazegaki-yomi-all pc)
                          (abs yomi-len))))
         (suffix (tutcode-context-mazegaki-suffix pc))
         (state (tutcode-context-state pc))
         ;; 候補1個で自動確定された漢字が意図したものでなかった場合のundoを想定
         ;; (読み入力状態を再現。候補選択状態ではなく。選択中の候補番号は不要)
         (undo-data (and (eq? state 'tutcode-state-converting)
                         (list head (tutcode-context-latin-conv pc))))
         (res (tutcode-prepare-commit-string pc))) ; flushによりhead等がクリア
    (cond
      ((= yomi-len 0)
        (tutcode-commit pc res)
        (if undo-data
          (tutcode-undo-prepare pc state res undo-data)))
      ((> yomi-len 0)
        (tutcode-postfix-commit pc res yomi))
      (else
        (tutcode-selection-commit pc res yomi)))
    (tutcode-check-auto-help-window-begin pc
      (drop (string-to-list res) (length suffix))
      (append suffix head))))

;;; 交ぜ書き変換の候補選択時に、指定されたラベル文字に対応する候補を確定する
;;; @param ch 入力されたラベル文字
;;; @return 確定した場合#t
(define (tutcode-commit-by-label-key pc ch)
  ;; 現在候補ウィンドウに表示されていないラベル文字を入力した場合、
  ;; 現在以降の候補内において入力ラベル文字に対応する候補を確定する。
  ;; (学習機能をオフにして候補の並び順を固定にして使用する場合に、
  ;; next-page-keyを押す回数を減らし、
  ;; なるべく少ないキーで目的の候補を選べるようにするため)
  (let* ((nr (tutcode-context-nr-candidates pc))
         (nth (tutcode-context-nth pc))
         (idx
          (tutcode-get-idx-by-label-key ch nth tutcode-nr-candidate-max
            tutcode-nr-candidate-max tutcode-heading-label-char-list)))
    (if (and (>= idx 0)
             (< idx nr))
      (begin
        (tutcode-context-set-nth! pc idx)
        (tutcode-commit-with-auto-help pc)
        #t)
      (eq? tutcode-commit-candidate-by-label-key 'always))))

;;; 候補選択時に、指定されたラベル文字に対応する候補番号を計算する
;;; @param ch 入力されたラベル文字
;;; @param nth 現在選択されている候補の番号
;;; @param page-limit 候補選択ウィンドウでの各ページ内の候補数上限
;;;                   (補完の場合:補完候補+熟語ガイド)
;;; @param nr-in-page 候補選択ウィンドウでの各ページ内の候補数
;;;                   (補完の場合:補完候補のみ)
;;; @param heading-label-char-list ラベル文字の配列
;;; @return 候補番号
(define (tutcode-get-idx-by-label-key ch nth page-limit nr-in-page
        heading-label-char-list)
  (let*
    ((cur-page (if (= page-limit 0)
                  0
                  (quotient nth page-limit)))
     ;; 現在候補ウィンドウに表示中の候補リストの先頭の候補番号
     (cur-offset (* cur-page nr-in-page))
     (labellen (length heading-label-char-list))
     (cur-labels
       (list-tail heading-label-char-list (remainder cur-offset labellen)))
     (target-labels (member ch cur-labels))
     (offset (if target-labels
               (- (length cur-labels) (length target-labels))
               (+ (length cur-labels)
                  (- labellen
                     (length
                       (member ch heading-label-char-list))))))
     (idx (+ cur-offset offset)))
    idx))

;;; 記号入力モード時に、指定されたラベル文字に対応する候補を確定する
;;; @return 確定した場合#t
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
          (tutcode-prepare-commit-string-for-kigou-mode pc))
        #t)
      (eq? tutcode-commit-candidate-by-label-key 'always))))

;;; ヒストリ入力の候補選択時に、指定されたラベル文字に対応する候補を確定する
;;; @param ch 入力されたラベル文字
;;; @return 確定した場合#t
(define (tutcode-commit-by-label-key-for-history pc ch)
  (let* ((nr (tutcode-context-nr-candidates pc))
         (nth (tutcode-context-nth pc))
         (idx
          (tutcode-get-idx-by-label-key ch nth
            tutcode-nr-candidate-max-for-history
            tutcode-nr-candidate-max-for-history
            tutcode-heading-label-char-list-for-history)))
    (if (and (>= idx 0)
             (< idx nr))
      (begin
        (tutcode-context-set-nth! pc idx)
        (let ((str (tutcode-prepare-commit-string-for-history pc)))
          (tutcode-commit pc str)
          (tutcode-flush pc)
          (tutcode-check-auto-help-window-begin pc (string-to-list str) ()))
        #t)
      (eq? tutcode-commit-candidate-by-label-key 'always))))

;;; 補完/予測入力候補表示時に、指定されたラベル文字に対応する候補を確定する
;;; @param ch 入力されたラベル文字
;;; @param mode tutcode-context-predictingの値
;;; @return 確定した場合#t
(define (tutcode-commit-by-label-key-for-prediction pc ch mode)
  (let*
    ((nth (tutcode-context-prediction-index pc))
     (page-limit (tutcode-context-prediction-page-limit pc))
     (nr-in-page (tutcode-context-prediction-nr-in-page pc))
     (idx
      (tutcode-get-idx-by-label-key ch nth page-limit nr-in-page
        tutcode-heading-label-char-list-for-prediction))
     (nr (tutcode-lib-get-nr-predictions pc))
     ;; XXX:熟語ガイドのページ数の方が多い場合、
     ;;     補完候補はループして2順目以降の可能性あり(表形式candwinでない場合)
     (i (if (zero? nr) -1 (remainder idx nr))))
    (if (>= i 0)
      (begin
        (case mode
          ((tutcode-predicting-bushu)
            (tutcode-do-commit-prediction-for-bushu pc i))
          ((tutcode-predicting-interactive-bushu)
            (tutcode-do-commit-prediction-for-interactive-bushu pc i))
          ((tutcode-predicting-completion)
            (tutcode-do-commit-prediction pc i #t))
          (else
            (tutcode-do-commit-prediction pc i #f)))
        #t)
      (eq? tutcode-commit-candidate-by-label-key 'always))))

(define (tutcode-get-prediction-string pc idx)
  (tutcode-lib-get-nth-prediction pc idx))

(define (tutcode-learn-prediction-string pc idx completion?)
  (tutcode-lib-commit-nth-prediction pc idx completion?))

;;; 補完/予測入力候補を確定する
;;; @param completion? 補完かどうか
(define (tutcode-do-commit-prediction pc idx completion?)
  (let ((str (tutcode-get-prediction-string pc idx)))
    (tutcode-learn-prediction-string pc idx completion?)
    (tutcode-reset-candidate-window pc)
    (tutcode-commit pc str)
    (tutcode-flush pc)
    (tutcode-check-auto-help-window-begin pc (string-to-list str) ())))

;;; 部首合成変換時の予測入力候補を確定する
(define (tutcode-do-commit-prediction-for-bushu pc idx)
  (let ((str (tutcode-get-prediction-string pc idx)))
    (tutcode-reset-candidate-window pc)
    (tutcode-bushu-commit pc str)))

;;; 対話的部首合成変換時の候補を確定する
(define (tutcode-do-commit-prediction-for-interactive-bushu pc idx)
  (let ((str (tutcode-get-prediction-string pc idx)))
    (tutcode-reset-candidate-window pc)
    (tutcode-commit pc str)
    (tutcode-flush pc)
    (tutcode-check-auto-help-window-begin pc (string-to-list str) ())))

;;; 交ぜ書き変換辞書から、現在選択されている候補を削除する。
(define (tutcode-purge-candidate pc)
  (let ((res (skk-lib-purge-candidate
               tutcode-dic
               (cons (string-list-concat (tutcode-context-head pc)) "")
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

;;; commit文字列履歴リストhistoryに文字列を追加する。
;;; @param str 追加する文字列
(define (tutcode-append-history pc str)
  (let* ((history (tutcode-context-history pc))
         (new-history (cons str (delete str history))))
    (tutcode-context-set-history! pc
      (if (> (length new-history) tutcode-history-size)
        (take new-history tutcode-history-size)
        new-history))))

;;; 交ぜ書き変換を開始する
;;; @param yomi 変換対象の読み(文字列の逆順リスト)
;;; @param suffix 活用する語の変換を行う場合の活用語尾(文字列の逆順リスト)
;;; @param autocommit? 候補が1個の場合に自動的に確定するかどうか
;;; @param recursive-learning? 候補が無い場合に再帰登録モードに入るかどうか
;;; @return #t:候補が有った場合。#f:候補が無かった場合。
(define (tutcode-begin-conversion pc yomi suffix autocommit?
          recursive-learning?)
  (let*
    ((yomi-str (string-list-concat yomi))
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
              (tutcode-select-candidate pc 0))))
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

;;; 入力された漢字コードに対応する漢字を確定する
;;; @param str-list 漢字コード。入力された文字列のリスト(逆順)
(define (tutcode-begin-kanji-code-input pc str-list)
  (let ((kanji (ja-kanji-code-input str-list)))
    (if (and kanji (> (string-length kanji) 0))
      (begin
        (tutcode-commit pc kanji)
        (tutcode-flush pc)
        (tutcode-undo-prepare pc 'tutcode-state-code kanji str-list)
        (tutcode-check-auto-help-window-begin pc (list kanji) ())))))

;;; 子コンテキストを作成する。
;;; @param type 'tutcode-child-type-editorか'tutcode-child-type-dialog
(define (tutcode-setup-child-context pc type)
  (let ((cpc (tutcode-context-new (tutcode-context-uc pc)
              (tutcode-context-im pc))))
    (tutcode-context-set-child-context! pc cpc)
    (tutcode-context-set-child-type! pc type)
    (tutcode-context-set-parent-context! cpc pc)
    (if (eq? type 'tutcode-child-type-dialog)
      (tutcode-context-set-state! cpc 'tutcode-state-off)
      (tutcode-context-set-state! cpc 'tutcode-state-on))
    cpc))

;;; 記号入力モードを開始する。
;;; @param pc コンテキストリスト
(define (tutcode-begin-kigou-mode pc)
  (tutcode-context-set-nth! pc 0)
  (tutcode-context-set-nr-candidates! pc (length tutcode-kigoudic))
  (tutcode-context-set-state! pc 'tutcode-state-kigou)
  (tutcode-check-candidate-window-begin pc)
  (if (eq? (tutcode-context-candidate-window pc)
           'tutcode-candidate-window-kigou)
    (tutcode-select-candidate pc 0)))

;;; ヒストリ入力の候補表示を開始する
(define (tutcode-begin-history pc)
  (if (and (> tutcode-history-size 0)
           (pair? (tutcode-context-history pc)))
    (begin
      (tutcode-context-set-nth! pc 0)
      (tutcode-context-set-nr-candidates! pc
        (length (tutcode-context-history pc)))
      (tutcode-context-set-state! pc 'tutcode-state-history)
      (tutcode-check-candidate-window-begin pc)
      (if (eq? (tutcode-context-candidate-window pc)
               'tutcode-candidate-window-history)
        (tutcode-select-candidate pc 0)))))

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

;;; 交ぜ書き変換・記号入力モード・ヒストリ入力モード時に
;;; 候補ウィンドウの表示を開始する
(define (tutcode-check-candidate-window-begin pc)
  (if (and (eq? (tutcode-context-candidate-window pc)
                'tutcode-candidate-window-off)
           tutcode-use-candidate-window?
           (>= (tutcode-context-nth pc) (- tutcode-candidate-op-count 1)))
    (let ((state (tutcode-context-state pc)))
      (tutcode-activate-candidate-window pc
        (case state
          ((tutcode-state-kigou) 'tutcode-candidate-window-kigou)
          ((tutcode-state-history) 'tutcode-candidate-window-history)
          (else 'tutcode-candidate-window-converting))
        tutcode-candidate-window-activate-delay-for-mazegaki
        (tutcode-context-nr-candidates pc)
        (case state
          ((tutcode-state-kigou) tutcode-nr-candidate-max-for-kigou-mode)
          ((tutcode-state-history) tutcode-nr-candidate-max-for-history)
          (else tutcode-nr-candidate-max))))))

;;; 候補ウィンドウを表示する
;;; @param type 候補ウィンドウタイプ
;;; @param delay 候補ウィンドウ表示までの待ち時間[s]
;;; @param nr 候補数。delay後に計算する場合は-1
;;; @param display-limit ページ内候補数
(define (tutcode-activate-candidate-window pc type delay nr display-limit)
  (tutcode-context-set-candidate-window! pc type)
  (tutcode-context-set-candwin-delay-selected-index! pc -1)
  (if (tutcode-candidate-window-enable-delay? pc delay)
    (begin
      (tutcode-context-set-candwin-delay-waiting! pc #t)
      (im-delay-activate-candidate-selector pc delay))
    (begin
      (tutcode-context-set-candwin-delay-waiting! pc #f)
      (if (and tutcode-use-pseudo-table-style?
               (>= nr 0))
        (let ((pnr-pdl (tutcode-pseudo-table-style-setup pc nr display-limit)))
          (im-activate-candidate-selector pc (car pnr-pdl) (cadr pnr-pdl)))
        (im-activate-candidate-selector pc nr display-limit)))))

;;; 候補ウィンドウの遅延表示を行うかどうかを返す
;;; @param delay 遅延時間。0の場合は遅延表示はしない。
(define (tutcode-candidate-window-enable-delay? pc delay)
  (and tutcode-candidate-window-use-delay?
       (im-delay-activate-candidate-selector-supported? pc)
       (> delay 0)))

;;; 擬似表形式候補表示用の最初のページの候補リストを作成して
;;; pseudo-table-candsにsetする
;;; @param nr 候補数
;;; @param display-limit ページ内候補数
;;; @return '(nr display-limit) 擬似表形式の候補リストの候補数とページ内候補数
(define (tutcode-pseudo-table-style-setup pc nr display-limit)
  (if (= nr 0)
    '(0 0)
    (let* ((pcands (tutcode-pseudo-table-style-make-page pc 0 display-limit nr))
           (pdl (length pcands))
           (nr-page (+ (quotient nr display-limit)
                       (if (= 0 (remainder nr display-limit)) 0 1)))
           (pnr (* nr-page pdl))
           (pcands-all (make-vector nr-page #f)))
      (vector-set! pcands-all 0 pcands)
      (tutcode-context-set-pseudo-table-cands! pc pcands-all)
      (list pnr pdl))))

;;; 擬似表形式候補表示用の新ページの候補リストを作成して返す
(define (tutcode-pseudo-table-style-make-new-page pc)
  (let*
    ((dl-nr-nth (tutcode-candwin-limit-nr-nth pc))
     (dl (list-ref dl-nr-nth 0))
     (nr (list-ref dl-nr-nth 1))
     (nth (list-ref dl-nr-nth 2))
     (page (quotient nth dl))
     (start-index (* page dl))
     (end-index (+ start-index dl)))
    (tutcode-pseudo-table-style-make-page pc start-index end-index nr)))

;;; 候補ウィンドウに表示・選択中の候補の情報を返す
;;; @return (<ページ内候補数(display-limit)> <全候補数> <選択中の候補番号>)
(define (tutcode-candwin-limit-nr-nth pc)
  (cond
    ((eq? (tutcode-context-state pc) 'tutcode-state-kigou)
      (list tutcode-nr-candidate-max-for-kigou-mode
            (tutcode-context-nr-candidates pc)
            (tutcode-context-nth pc)))
    ((eq? (tutcode-context-state pc) 'tutcode-state-history)
      (list tutcode-nr-candidate-max-for-history
            (tutcode-context-nr-candidates pc)
            (tutcode-context-nth pc)))
    ((eq? (tutcode-context-candidate-window pc)
          'tutcode-candidate-window-predicting)
      (list (tutcode-context-prediction-page-limit pc)
            (tutcode-context-prediction-nr-all pc)
            (tutcode-context-prediction-index pc)))
    ((eq? (tutcode-context-candidate-window pc)
          'tutcode-candidate-window-stroke-help)
      (list tutcode-nr-candidate-max-for-kigou-mode
            (length (tutcode-context-stroke-help pc))
            0))
    ((eq? (tutcode-context-candidate-window pc)
          'tutcode-candidate-window-auto-help)
      (list tutcode-nr-candidate-max-for-kigou-mode
            (length (tutcode-context-auto-help pc))
            0))
    ((eq? (tutcode-context-state pc) 'tutcode-state-interactive-bushu)
      (list (tutcode-context-prediction-page-limit pc)
            (tutcode-context-prediction-nr-all pc)
            (tutcode-context-prediction-index pc)))
    ((eq? (tutcode-context-candidate-window pc)
          'tutcode-candidate-window-converting)
      (list tutcode-nr-candidate-max
            (tutcode-context-nr-candidates pc)
            (tutcode-context-nth pc)))
    (else
      (list tutcode-nr-candidate-max 0 0))))

;;; 擬似表形式候補表示用の指定した開始番号のページの候補リストを作成して返す
;;; @param start-index 開始番号
;;; @param end-index 終了番号
(define (tutcode-pseudo-table-style-make-page pc start-index end-index nr)
  (let ((cands
          (let loop
            ((idx start-index)
             (cands ()))
            (if (or (>= idx end-index) (>= idx nr))
              (reverse cands)
              (loop
                (+ idx 1)
                (cons (tutcode-get-candidate-handler-internal pc idx 0)
                      cands))))))
    ;; 最初のページ以外は、下半分ブロックが空でも省略しない
    ;; (全候補数は最初のページの候補数をもとに計算するので、
    ;; 最初のページで下半分ブロック有りなのに、最後のページで省略されると、
    ;; 候補が足りなくなってget-candidate時にエラーになる)
    (tutcode-table-in-vertical-candwin cands (= start-index 0))))

;;; 候補リスト上の候補番号を、擬似表形式上の候補番号に変換
(define (tutcode-pseudo-table-style-candwin-index pc idx)
  (let* ((vec (tutcode-context-pseudo-table-cands pc))
         (display-limit (length (vector-ref vec 0)))
         (page-limit (list-ref (tutcode-candwin-limit-nr-nth pc) 0))
         (page (quotient idx page-limit)))
    (* page display-limit))) ; XXX:candwinのページ単位のみ対応

;;; 擬似表形式上の候補番号を、候補リスト上の候補番号に変換
(define (tutcode-pseudo-table-style-scm-index pc idx)
  (let* ((vec (tutcode-context-pseudo-table-cands pc))
         (display-limit (length (vector-ref vec 0)))
         (page-limit (list-ref (tutcode-candwin-limit-nr-nth pc) 0))
         (page (quotient idx display-limit)))
    (* page page-limit))) ; XXX:candwinのページ単位のみ対応

;;; 候補ウィンドウ上で候補を選択する
;;; @param idx 選択する候補のインデックス番号
(define (tutcode-select-candidate pc idx)
  (if (tutcode-context-candwin-delay-waiting pc)
    ;; 遅延表示待ち中はcandwinは未作成のためim-select-candidateするとSEGV。
    ;; (XXX (uim api-docに合わせて)candwin側で対処した方がいいかもしれないが、
    ;;      shift-pageとの混在時の計算が面倒なので、とりあえずscm側で。)
    (tutcode-context-set-candwin-delay-selected-index! pc idx)
    (tutcode-pseudo-table-select-candidate pc idx)))

;;; 候補ウィンドウ上で候補を選択する(擬似表形式候補表示対応)
;;; @param idx 選択する候補のインデックス番号
(define (tutcode-pseudo-table-select-candidate pc idx)
  (if tutcode-use-pseudo-table-style?
    (im-select-candidate pc (tutcode-pseudo-table-style-candwin-index pc idx))
    (im-select-candidate pc idx)))

;;; 仮想鍵盤に表示する候補リストを作って返す
;;; @return 候補リスト(get-candidate-handler用形式)
(define (tutcode-stroke-help-make pc)
  (let*
    ((rkc (tutcode-context-rk-context pc))
     (seq (rk-context-seq rkc))
     (seqlen (length seq))
     (seq-rev (reverse seq))
     (guide-seqs
      (and
        (pair? seq)
        (pair? (tutcode-context-guide-chars pc))
        (rk-lib-find-partial-seqs seq-rev (tutcode-context-guide-chars pc))))
     (guide-alist (tutcode-stroke-help-guide-update-alist () seqlen
                    (if (pair? guide-seqs) guide-seqs ())))
     ;; 例:(("v" "玉+") ("a" "倉+") ("r" "石+"))
     (guide-candcombined
      (map
        (lambda (elem)
          (list (car elem) (string-list-concat (cdr elem))))
        guide-alist))
     ;; stroke-help. 例:(("k" "あ") ("i" "い") ("g" "*贈"))
     (label-cand-alist
      (if (or tutcode-use-stroke-help-window?
              (and
                (pair? guide-seqs)
                (eq? tutcode-stroke-help-with-kanji-combination-guide 'full)))
        (let*
          ((rule (rk-context-rule rkc))
           (ret (rk-lib-find-partial-seqs seq-rev rule))
           (katakana? (tutcode-context-katakana-mode? pc))
           (label-cand-alist
            (if (null? seq) ; tutcode-rule全部なめて作成→遅いのでキャッシュ
              (cond
                ((not tutcode-show-stroke-help-window-on-no-input?)
                  ())
                ((tutcode-kigou2-mode? pc)
                  tutcode-kigou-rule-stroke-help-top-page-alist)
                (katakana?
                  (if (not tutcode-stroke-help-top-page-katakana-alist)
                    (set! tutcode-stroke-help-top-page-katakana-alist
                      (tutcode-stroke-help-update-alist
                        () seqlen katakana? ret)))
                  tutcode-stroke-help-top-page-katakana-alist)
                (else
                  (if (not tutcode-stroke-help-top-page-alist)
                    (set! tutcode-stroke-help-top-page-alist
                      (tutcode-stroke-help-update-alist
                        () seqlen katakana? ret)))
                  tutcode-stroke-help-top-page-alist))
              (tutcode-stroke-help-update-alist () seqlen katakana? ret))))
          ;; 表示する候補文字列を、熟語ガイド(+)付き文字列に置き換える
          (for-each
            (lambda (elem)
              (let*
                ((label (car elem))
                 (label-cand (assoc label label-cand-alist)))
                (if label-cand
                  (set-cdr! label-cand (cdr elem)))))
            guide-candcombined)
          label-cand-alist)
        (if (eq? tutcode-stroke-help-with-kanji-combination-guide 'guide-only)
          guide-candcombined
          ()))))
    (if (null? label-cand-alist)
      ()
      (map
        (lambda (elem)
          (list (cadr elem) (car elem) ""))
        (reverse label-cand-alist)))))

;;; 仮想鍵盤の表示を開始する
(define (tutcode-check-stroke-help-window-begin pc)
  (if (eq? (tutcode-context-candidate-window pc) 'tutcode-candidate-window-off)
    (if (tutcode-candidate-window-enable-delay? pc
          tutcode-candidate-window-activate-delay-for-stroke-help)
      ;; XXX:何も表示しない場合にはタイマも動かないようにしたいところ
      (tutcode-activate-candidate-window pc
        'tutcode-candidate-window-stroke-help
        tutcode-candidate-window-activate-delay-for-stroke-help
        -1 -1)
      (let ((stroke-help (tutcode-stroke-help-make pc)))
        (if (pair? stroke-help)
          (begin
            (tutcode-context-set-stroke-help! pc stroke-help)
            (tutcode-activate-candidate-window pc
              'tutcode-candidate-window-stroke-help
              0
              (length stroke-help)
              tutcode-nr-candidate-max-for-kigou-mode)))))))

;;; 通常の候補ウィンドウに、表形式で候補を表示するために、
;;; 表形式の1行分を連結した形に変換する。
;;; (表形式候補ウィンドウ未対応で、縦に候補を並べるcandwin用。
;;;  uim-elで(setq uim-candidate-display-inline t)の場合等)
;;; @param cands ("表示文字列" "ラベル文字列" "注釈")のリスト
;;; @param omit-empty-block? 表の下半分(シフトキー領域)が空の場合に省略するか
;;; @return 変換後のリスト。
;;;  例:(("*や|*ま|*か|*あ|*は||*」|*】|*…|*・|*”||" "q" "") ...)
(define (tutcode-table-in-vertical-candwin cands omit-empty-block?)
  (let*
    ((layout (if (null? uim-candwin-prog-layout)
                uim-candwin-prog-layout-qwerty-jis
                uim-candwin-prog-layout))
     (vecsize (length layout))
     (vec (make-vector vecsize #f)))
    (for-each
      (lambda (elem)
        (let
          ((k (list-index (lambda (e) (string=? e (cadr elem))) layout)))
          (if k
            (vector-set! vec k (car elem)))))
      cands)
    (let*
      ;; 表の下半分(シフトキー領域)が空の場合は上半分だけ使う
      ((vecmax
        (if (not omit-empty-block?)
          vecsize
          (let loop ((k (* 13 4)))
            (if (>= k vecsize)
              (* 13 4)
              (if (string? (vector-ref vec k))
                vecsize
                (loop (+ k 1)))))))
       ;; 各列の最大幅を調べる
       (width-list0
        (let colloop
          ((col 12)
           (width-list ()))
          (if (negative? col)
            width-list
            (colloop
              (- col 1)
              (cons
                (let rowloop
                  ((k col)
                   (maxwidth -1))
                  (if (>= k vecmax)
                    maxwidth
                    (let*
                      ((elem (vector-ref vec k))
                       (width (if (string? elem) (string-length elem) -1)))
                      (rowloop
                        (+ k 13)
                        (if (> width maxwidth)
                          width
                          maxwidth)))))
                width-list)))))
       ;; 表の右端ブロックが空の場合は表示しない
       (colmax
        (if (any (lambda (x) (> x -1)) (take-right width-list0 3)) 13 10))
       (width-list (map (lambda (x) (if (< x 2) 2 x)) width-list0))
       ;; ラベルは、各行で、最初の中身のある桁に対応するものを使用
       (labels
        (let rowloop
          ((row 0)
           (labels ()))
          (if (>= (* row 13) vecmax)
            (reverse labels)
            (rowloop
              (+ row 1)
              (cons
                (let colloop
                  ((col 0))
                  (let ((k (+ (* row 13) col)))
                    (cond
                      ((>= col colmax)
                        (list-ref layout (* row 13)))
                      ((string? (vector-ref vec k))
                        (list-ref layout k))
                      (else
                        (colloop (+ col 1))))))
                labels))))))
      ;; 各行内の全列を連結して候補文字列を作る
      (let rowloop
        ((table (take! (vector->list vec) vecmax))
         (k 0)
         (res ()))
        (if (null? table)
          (reverse res)
          (let*
            ((line (take table 13))
             (line-sep
              (cdr
                (let colloop
                  ((col (- colmax 1))
                   (line-sep (if (= colmax 10) '("||") ())))
                  (if (negative? col)
                    line-sep
                    (colloop
                      (- col 1)
                      (append
                        (let*
                          ((elem (list-ref line col))
                           (elemlen (if (string? elem) (string-length elem) 0))
                           (width (list-ref width-list col))
                           (strlist
                            (if (zero? elemlen)
                              (make-list width " ")
                              ;; 中央に配置する
                              (letrec
                                ((padleft
                                  (lambda (pad strlist)
                                    (if (<= pad 0)
                                      strlist
                                      (padright
                                        (- pad 1)
                                        (cons " " strlist)))))
                                 (padright
                                  (lambda (pad strlist)
                                    (if (<= pad 0)
                                      strlist
                                      (padleft
                                        (- pad 1)
                                        (append strlist (list " ")))))))
                                (padleft (- width elemlen) (list elem))))))
                          (cons
                            (if (or (= col 5) (= col 10))
                              "||" ; ブロック区切りを目立たせる
                              "|")
                            strlist))
                        line-sep))))))
             (cand (apply string-append line-sep))
             (candlabel (list cand (list-ref labels (quotient k 13)) "")))
            (rowloop
              (drop table 13)
              (+ k 13)
              (cons candlabel res))))))))

;;; 仮想鍵盤の表示を行うかどうかの設定を一時的に切り替える(トグル)。
;;; (常に表示すると目ざわりなので。打ち方に迷ったときだけ表示したい。)
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
          (cond
            ((string? cand)
              cand)
            ((symbol? cand)
              (case cand
                ((tutcode-mazegaki-start) "◇")
                ((tutcode-latin-conv-start) "/")
                ((tutcode-kanji-code-input-start) "□")
                ((tutcode-history-start) "◎")
                ((tutcode-bushu-start) "◆")
                ((tutcode-interactive-bushu-start) "▼")
                ((tutcode-postfix-bushu-start) "▲")
                ((tutcode-selection-mazegaki-start) "△s")
                ((tutcode-selection-mazegaki-inflection-start) "―s")
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
                ((tutcode-selection-katakana-start) "カs")
                ((tutcode-postfix-katakana-start) "カ")
                ((tutcode-postfix-katakana-0-start) "カ0")
                ((tutcode-postfix-katakana-1-start) "カ1")
                ((tutcode-postfix-katakana-2-start) "カ2")
                ((tutcode-postfix-katakana-3-start) "カ3")
                ((tutcode-postfix-katakana-4-start) "カ4")
                ((tutcode-postfix-katakana-5-start) "カ5")
                ((tutcode-postfix-katakana-6-start) "カ6")
                ((tutcode-postfix-katakana-7-start) "カ7")
                ((tutcode-postfix-katakana-8-start) "カ8")
                ((tutcode-postfix-katakana-9-start) "カ9")
                ((tutcode-postfix-katakana-exclude-1) "ヵ1")
                ((tutcode-postfix-katakana-exclude-2) "ヵ2")
                ((tutcode-postfix-katakana-exclude-3) "ヵ3")
                ((tutcode-postfix-katakana-exclude-4) "ヵ4")
                ((tutcode-postfix-katakana-exclude-5) "ヵ5")
                ((tutcode-postfix-katakana-exclude-6) "ヵ6")
                ((tutcode-postfix-katakana-shrink-1) "か1")
                ((tutcode-postfix-katakana-shrink-2) "か2")
                ((tutcode-postfix-katakana-shrink-3) "か3")
                ((tutcode-postfix-katakana-shrink-4) "か4")
                ((tutcode-postfix-katakana-shrink-5) "か5")
                ((tutcode-postfix-katakana-shrink-6) "か6")
                ((tutcode-selection-kanji2seq-start) "/s")
                ((tutcode-postfix-kanji2seq-start) "/@")
                ((tutcode-postfix-kanji2seq-1-start) "/1")
                ((tutcode-postfix-kanji2seq-2-start) "/2")
                ((tutcode-postfix-kanji2seq-3-start) "/3")
                ((tutcode-postfix-kanji2seq-4-start) "/4")
                ((tutcode-postfix-kanji2seq-5-start) "/5")
                ((tutcode-postfix-kanji2seq-6-start) "/6")
                ((tutcode-postfix-kanji2seq-7-start) "/7")
                ((tutcode-postfix-kanji2seq-8-start) "/8")
                ((tutcode-postfix-kanji2seq-9-start) "/9")
                ((tutcode-selection-seq2kanji-start) "漢s")
                ((tutcode-clipboard-seq2kanji-start) "漢c")
                ((tutcode-postfix-seq2kanji-start) "漢@")
                ((tutcode-postfix-seq2kanji-1-start) "漢1")
                ((tutcode-postfix-seq2kanji-2-start) "漢2")
                ((tutcode-postfix-seq2kanji-3-start) "漢3")
                ((tutcode-postfix-seq2kanji-4-start) "漢4")
                ((tutcode-postfix-seq2kanji-5-start) "漢5")
                ((tutcode-postfix-seq2kanji-6-start) "漢6")
                ((tutcode-postfix-seq2kanji-7-start) "漢7")
                ((tutcode-postfix-seq2kanji-8-start) "漢8")
                ((tutcode-postfix-seq2kanji-9-start) "漢9")
                ((tutcode-auto-help-redisplay) "≪")
                ((tutcode-help) "？")
                ((tutcode-help-clipboard) "?c")
                ((tutcode-undo) "⇔")
                (else cand)))
            ((procedure? cand)
              "λ")))
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
;;; alistは以下のようにラベル文字と表示用文字列のリスト(逆順)。
;;; 例: (("v" "+" "玉") ("a" "+" "倉") ("r" "+" "石"))
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

;;; 自動ヘルプ表示のための候補リストを作成する。
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
;;; tutcode-auto-help-with-real-keys?が#tの場合(通常の候補ウィンドウ用)は、
;;; 以下のように表示する。
;;;   憂 lns
;;;   鬱 ▲林缶 nt cbo
;;;
;;; @param strlist 確定した文字列のリスト(逆順)
;;; @param yomilist 変換前の読みの文字列のリスト(逆順)
;;; @return 候補リスト(get-candidate-handler用形式)
(define (tutcode-auto-help-make pc strlist yomilist)
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
    (if (null? label-cands-alist)
      ()
      (map
        (lambda (elem)
          (list (string-list-concat (cdr elem)) (car elem) ""))
        label-cands-alist))))

;;; 部首合成変換・交ぜ書き変換で確定した文字の打ち方を表示する。
;;; @param strlist 確定した文字列のリスト(逆順)
;;; @param yomilist 変換前の読みの文字列のリスト(逆順)
;;; @param opt-immediate? 遅延無しですぐに表示するかどうか(オプション)
(define (tutcode-check-auto-help-window-begin pc strlist yomilist . opt-immediate?)
  (if (and (eq? (tutcode-context-candidate-window pc)
                'tutcode-candidate-window-off)
           tutcode-use-auto-help-window?)
    (let ((immediate? (:optional opt-immediate? #f)))
      (tutcode-context-set-guide-chars! pc ())
      (if (and (not immediate?)
               (tutcode-candidate-window-enable-delay? pc
                tutcode-candidate-window-activate-delay-for-auto-help))
        (begin
          (tutcode-context-set-auto-help! pc (list 'delaytmp strlist yomilist))
          (tutcode-activate-candidate-window pc
            'tutcode-candidate-window-auto-help
            tutcode-candidate-window-activate-delay-for-auto-help
            -1 -1))
        (let ((auto-help (tutcode-auto-help-make pc strlist yomilist)))
          (if (pair? auto-help)
            (begin
              (tutcode-context-set-auto-help! pc auto-help)
              (tutcode-activate-candidate-window pc
                'tutcode-candidate-window-auto-help
                0
                (length auto-help)
                tutcode-nr-candidate-max-for-kigou-mode))))))))

;;; カーソル位置の直前にある文字の打ち方を表示する。
;;; (surrounding text APIを使ってカーソル位置の直前にある文字を取得)
(define (tutcode-help pc)
  (let ((former-seq (tutcode-postfix-acquire-text pc 1)))
    (if (positive? (length former-seq))
      (tutcode-check-auto-help-window-begin pc former-seq () #t))))

;;; クリップボード内の文字の打ち方を表示する。
;;; (surrounding text APIを使ってクリップボードから文字を取得)
(define (tutcode-help-clipboard pc)
  (let*
    ((len (length tutcode-auto-help-cand-str-list))
     (latter-seq (tutcode-clipboard-acquire-text-wo-nl pc len)))
    (if (pair? latter-seq)
      (tutcode-check-auto-help-window-begin pc latter-seq () #t))))

;;; clipboardに対して入力シーケンス→漢字変換を開始する
(define (tutcode-begin-clipboard-seq2kanji-conversion pc)
  (let ((lst (tutcode-clipboard-acquire-text pc 'full)))
    (if (pair? lst)
      (let ((str (string-list-concat (tutcode-sequence->kanji-list pc lst))))
        (tutcode-commit pc str)
        (tutcode-undo-prepare pc 'tutcode-state-off str ())))))

;;; クリップボードから文字列を改行を除いて取得する
;;; @param len 取得する文字数
;;; @return 取得した文字列のリスト(逆順)
(define (tutcode-clipboard-acquire-text-wo-nl pc len)
  (let ((latter-seq (tutcode-clipboard-acquire-text pc len)))
    (and (pair? latter-seq)
         (delete "\n" latter-seq))))

;;; surrounding text APIを使ってクリップボードから文字を取得
;;; @param len 取得する文字数
;;; @return 取得した文字列のリスト(逆順)。取得できない場合は#f
(define (tutcode-clipboard-acquire-text pc len)
  (and-let*
    ((ustr (im-acquire-text pc 'clipboard 'beginning 0 len))
     (latter (ustr-latter-seq ustr))
     (latter-seq (and (pair? latter) (string-to-list (car latter)))))
    (and (not (null? latter-seq))
         latter-seq)))

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
    ((stime (time))
     (rule (rk-context-rule (tutcode-context-rk-context pc)))
     (stroke (tutcode-reverse-find-seq kanji rule)))
    (if stroke
      (begin
        (tutcode-stroke-help-guide-add-kanji
          pc (list (list stroke) (list kanji)))
        (tutcode-auto-help-update-stroke-alist-with-stroke
          label-cands-alist
          (cons (string-append (caar cand-list) "(" kanji ")") (cdar cand-list))
          stroke))
      (let ((decomposed (tutcode-auto-help-bushu-decompose kanji rule stime)))
        ;; 例: "繋" => (((("," "o"))("撃")) ((("f" "q"))("糸")))
        (if (not decomposed)
          label-cands-alist
          (let*
            ((bushu-strs (tutcode-auto-help-bushu-composition-strs decomposed))
             (helpstrlist (append (list "(" kanji "▲") bushu-strs '(")")))
             (helpstr (apply string-append helpstrlist))
             (alist
              (letrec
                ((update-stroke
                  (lambda (lst alist cand-list)
                    (if (or (null? lst) (null? cand-list))
                      (list alist cand-list)
                      (let
                        ((res
                          (if (tutcode-rule-element? (car lst))
                            (list
                              (tutcode-auto-help-update-stroke-alist-with-stroke
                                alist (car cand-list) (caar (car lst)))
                              (cdr cand-list))
                            (update-stroke (car lst) alist cand-list))))
                        (update-stroke (cdr lst) (car res) (cadr res)))))))
                (update-stroke decomposed label-cands-alist
                  (cons
                    (cons
                      (string-append (caar cand-list) helpstr)
                      (cdar cand-list))
                    (cdr cand-list))))))
            (tutcode-auto-help-bushu-composition-add-guide pc decomposed)
            (car alist)))))))

;;; tutcode-ruleの要素の形式((("," "o"))("撃"))かどうかを返す
(define (tutcode-rule-element? x)
  (and
    (pair? x)
    (pair? (car x))
    (pair? (caar x))
    (pair? (cdr x))
    (pair? (cadr x))
    (every string? (caar x))
    (every string? (cadr x))))

;;; 自動ヘルプ:tutcode-auto-help-bushu-decomposeで検索した、
;;; 部首合成方法で使う部首を、ガイド対象文字に追加する。
;;; @param decomposed tutcode-auto-help-bushu-decompose結果
(define (tutcode-auto-help-bushu-composition-add-guide pc decomposed)
  (if (not (null? decomposed))
    (begin
      (if (tutcode-rule-element? (car decomposed))
        (tutcode-stroke-help-guide-add-kanji pc (car decomposed))
        (tutcode-auto-help-bushu-composition-add-guide pc (car decomposed)))
      (tutcode-auto-help-bushu-composition-add-guide pc (cdr decomposed)))))

;;; 自動ヘルプ:tutcode-auto-help-bushu-decomposeで検索した、
;;; ストロークを含む部首合成方法から、
;;; 部首文字列のみを抜き出した部首合成方法を作る
;;; @param decomposed tutcode-auto-help-bushu-decompose結果
;;; @return 作成後の部首合成方法文字列リスト
(define (tutcode-auto-help-bushu-composition-strs decomposed)
  (tutcode-auto-help-bushu-composition-traverse decomposed ()
    (lambda (ele) (list (caadr ele))) "▲" ""))

;;; 自動ヘルプ:tutcode-auto-help-bushu-decomposeの検索結果のツリー構造から、
;;; 一部を抜き出したフラットなリストを作る
;;; @param decomposed tutcode-auto-help-bushu-decompose結果
;;; @param lst 作成中のリスト
;;; @param picker decomposedの要素(tutcode-rule-element)から
;;;   対象要素を抜き出すための関数
;;; @param branch-str 枝わかれを示すために結果リストに追加する文字列
;;; @param delim-str 各部首の区切りを示すために結果リストに追加する文字列
;;; @return 作成後のリスト
(define (tutcode-auto-help-bushu-composition-traverse decomposed lst picker
          branch-str delim-str)
  (if (null? decomposed)
    lst
    (let
      ((add
        (if (tutcode-rule-element? (car decomposed))
          (cons delim-str (picker (car decomposed)))
          (tutcode-auto-help-bushu-composition-traverse (car decomposed)
            (list branch-str) picker branch-str delim-str))))
      (tutcode-auto-help-bushu-composition-traverse (cdr decomposed)
        (append lst add) picker branch-str delim-str))))

;;; 自動ヘルプ:対象の1文字を入力するストロークをヘルプ用alistに追加する。
;;; @param label-cands-alist 元のalist
;;; @param kanji ヘルプ表示対象文字
;;; @return 更新後の自動ヘルプ用alist
(define (tutcode-auto-help-update-stroke-alist-normal-with-kanji
          pc label-cands-alist kanji)
  (let*
    ((stime (time))
     (rule (rk-context-rule (tutcode-context-rk-context pc)))
     (stroke (tutcode-reverse-find-seq kanji rule)))
    (if stroke
      (begin
        (tutcode-stroke-help-guide-add-kanji
          pc (list (list stroke) (list kanji)))
        (tutcode-auto-help-update-stroke-alist-normal-with-stroke
          label-cands-alist
          (cons (string-append kanji " ") stroke)
          kanji))
      (let ((decomposed (tutcode-auto-help-bushu-decompose kanji rule stime)))
        ;; 例: "繋" => (((("," "o"))("撃")) ((("f" "q"))("糸")))
        (if (not decomposed)
          label-cands-alist
          (let*
            ((bushu-strs (tutcode-auto-help-bushu-composition-strs decomposed))
             (helpstrlist (append (list kanji "▲") bushu-strs))
             (helpstr (apply string-append helpstrlist))
             (bushu-stroke
              (tutcode-auto-help-bushu-composition-traverse decomposed ()
                caar "" " ")))
            (tutcode-auto-help-bushu-composition-add-guide pc decomposed)
            (tutcode-auto-help-update-stroke-alist-normal-with-stroke
              label-cands-alist
              (cons helpstr bushu-stroke)
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
    (if (and help
             (> (length help) 0)
             (not (eq? (car help) 'delaytmp)))
      (tutcode-activate-candidate-window pc
        'tutcode-candidate-window-auto-help
        0
        (length help)
        tutcode-nr-candidate-max-for-kigou-mode))))

;;; 自動ヘルプ:直前のヘルプで候補ウィンドウに表示した内容をダンプ・commitする
;;; (部首合成シーケンス(例:"言▲▲西一早")をコピーしたい場合用)
(define (tutcode-auto-help-dump state pc)
  (if (eq? state 'tutcode-state-on)
    (let ((help (tutcode-context-auto-help pc)))
      (if (and help
               (> (length help) 0)
               (not (eq? (car help) 'delaytmp)))
        (let ((linecands
                (append-map
                  (lambda (elem)
                    (list (car elem) "\n"))
                  (tutcode-table-in-vertical-candwin help #t))))
          (tutcode-commit pc (apply string-append linecands) #t #t))))))

;;; preedit表示を更新する。
(define (tutcode-do-update-preedit pc)
  (let ((stat (tutcode-context-state pc))
        (cpc (tutcode-context-child-context pc))
        (cursor-shown? #f))
    (case stat
      ((tutcode-state-yomi)
        (im-pushback-preedit pc preedit-none "△")
        (let ((h (string-list-concat (tutcode-context-head pc))))
          (if (string? h)
            (im-pushback-preedit pc preedit-none h))))
      ((tutcode-state-code)
        (im-pushback-preedit pc preedit-none "□")
        (let ((h (string-list-concat (tutcode-context-head pc))))
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
                    (string-list-concat suffix))))))
          ;; child context's preedit
          (let ((h (string-list-concat (tutcode-context-head pc)))
                (editor (tutcode-context-editor pc))
                (dialog (tutcode-context-dialog pc)))
            (if (string? h)
              (im-pushback-preedit pc preedit-none h))
            (im-pushback-preedit pc preedit-none "【")
            (im-pushback-preedit pc preedit-none
              (case (tutcode-context-child-type pc)
                ((tutcode-child-type-editor)
                  (tutcode-editor-get-left-string editor))
                ((tutcode-child-type-dialog)
                  (tutcode-dialog-get-left-string dialog))))
            (tutcode-do-update-preedit cpc)
            (set! cursor-shown? #t)
            (im-pushback-preedit pc preedit-none
              (case (tutcode-context-child-type pc)
                ((tutcode-child-type-editor)
                  (tutcode-editor-get-right-string editor))
                ((tutcode-child-type-dialog)
                  (tutcode-dialog-get-right-string dialog))))
            (im-pushback-preedit pc preedit-none "】"))))
      ;; 部首合成変換のマーカ▲は文字列としてhead内で管理(再帰的部首合成のため)
      ((tutcode-state-bushu)
        (let ((h (string-list-concat (tutcode-context-head pc))))
          (if (string? h)
            (im-pushback-preedit pc preedit-none h))))
      ((tutcode-state-interactive-bushu)
        (im-pushback-preedit pc preedit-none "▼")
        (let ((h (string-list-concat (tutcode-context-head pc))))
          (if (string? h)
            (im-pushback-preedit pc preedit-none h)))
        (if tutcode-show-pending-rk?
          (im-pushback-preedit pc preedit-underline
            (rk-pending (tutcode-context-rk-context pc))))
        (im-pushback-preedit pc preedit-cursor "")
        (set! cursor-shown? #t)
        (if (> (tutcode-lib-get-nr-predictions pc) 0)
          (begin
            (im-pushback-preedit pc preedit-underline "=>")
            (im-pushback-preedit pc preedit-underline
              (tutcode-get-prediction-string pc
                (tutcode-context-prediction-index pc)))))) ; 熟語ガイド無し
      ((tutcode-state-kigou)
        ;; 候補ウィンドウ非表示時でも候補選択できるようにpreedit表示
        (im-pushback-preedit pc preedit-reverse
          (tutcode-get-current-candidate-for-kigou-mode pc)))
      ((tutcode-state-history)
        (im-pushback-preedit pc preedit-none "◎")
        (im-pushback-preedit pc preedit-none
          (tutcode-get-current-candidate-for-history pc)))
      ((tutcode-state-postfix-katakana)
        (im-pushback-preedit pc preedit-none "☆")
        (let ((h (string-list-concat (tutcode-context-head pc))))
          (if (string? h)
            (im-pushback-preedit pc preedit-none h))))
      ((tutcode-state-postfix-kanji2seq)
        (im-pushback-preedit pc preedit-none "／")
        (let ((h (string-list-concat (tutcode-context-head pc))))
          (if (string? h)
            (im-pushback-preedit pc preedit-none h))))
      ((tutcode-state-postfix-seq2kanji)
        (im-pushback-preedit pc preedit-none "￥")
        (let ((h (string-list-concat (tutcode-context-head pc))))
          (if (string? h)
            (im-pushback-preedit pc preedit-none h)))))
    (if (not cursor-shown?)
      (begin
        (if (and tutcode-show-pending-rk?
                 (memq stat '(tutcode-state-on tutcode-state-yomi
                              tutcode-state-bushu)))
          (im-pushback-preedit pc preedit-underline
            (rk-pending (tutcode-context-rk-context pc))))
        (im-pushback-preedit pc preedit-cursor "")))))

;;; preedit表示を更新する。
(define (tutcode-update-preedit pc)
  (im-clear-preedit pc)
  (tutcode-do-update-preedit (tutcode-find-root-context pc))
  (im-update-preedit pc))

;; called from tutcode-editor
;;; tutcode-editor側での編集完了時に呼ばれる。
;;; @param str エディタ側で確定された文字列
(define (tutcode-commit-editor-context pc str)
  (let* ((yomi-len (tutcode-context-postfix-yomi-len pc))
         (suffix (tutcode-context-mazegaki-suffix pc))
         (commit-str (if (null? suffix)
                         str
                         (string-append str (string-list-concat suffix)))))
    (tutcode-context-set-child-context! pc ())
    (tutcode-context-set-child-type! pc ())
    (cond
      ((= yomi-len 0)
        (tutcode-flush pc)
        (tutcode-commit pc commit-str))
      ((> yomi-len 0)
        (let ((yomi (take (tutcode-context-mazegaki-yomi-all pc) yomi-len)))
          (tutcode-postfix-commit pc commit-str yomi)
          (tutcode-flush pc)))
      (else
        (tutcode-selection-commit pc commit-str
          (tutcode-context-mazegaki-yomi-all pc))
        (tutcode-flush pc)))
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
        (let ((delay
                (if force-check?
                  0
                  tutcode-candidate-window-activate-delay-for-completion)))
          (if (tutcode-candidate-window-enable-delay? pc delay)
            (tutcode-activate-candidate-window pc
              'tutcode-candidate-window-predicting delay -1 -1)
            (if (tutcode-check-completion-make pc force-check? num)
              (tutcode-activate-candidate-window pc
                'tutcode-candidate-window-predicting
                0
                (tutcode-context-prediction-nr-all pc)
                (tutcode-context-prediction-page-limit pc)))))))))

;;; 補完候補を検索して候補リストを作成する
;;; @param force-check? 必ず検索を行うかどうか。
;;;  #fの場合は文字数が設定値未満の場合は検索しない。
;;; @param num commit-strsから検索対象にする文字数。0の場合は全て。
;;; @return #t:表示する候補あり, #f:表示する候補なし
(define (tutcode-check-completion-make pc force-check? num)
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
        (let ((str (string-list-concat commit-strs)))
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
                    (tutcode-context-set-predicting! pc
                      'tutcode-predicting-completion)))
                #t)
              ;; 補完候補が見つからない場合、1文字削った文字列を使って再検索
              ;; (直接tutcode-context-set-commit-strs!で文字を削ると、
              ;;  間違った文字を入力してBackspaceで消したときに、
              ;;  以前入力した文字列が削られているため、期待した補完にならない
              ;;  恐れあり。速度的には、直接削る方が速いけど)
              (if (> len 1)
                (tutcode-check-completion-make pc force-check? (- len 1))
                #f))))
        #f))
    #f))

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
        (let ((delay
              (if force-check?
                0
                tutcode-candidate-window-activate-delay-for-prediction)))
          (if (tutcode-candidate-window-enable-delay? pc delay)
            (tutcode-activate-candidate-window pc
              'tutcode-candidate-window-predicting delay -1 -1)
            (if (tutcode-check-prediction-make pc force-check?)
              (tutcode-activate-candidate-window pc
                'tutcode-candidate-window-predicting
                0
                (tutcode-context-prediction-nr-all pc)
                (tutcode-context-prediction-page-limit pc)))))))))

;;; 予測入力候補を検索して候補リストを作る
;;; @param force-check? 必ず検索を行うかどうか。
;;;  #fの場合は文字数が設定値未満の場合は検索しない。
;;; @return #t:表示する候補あり, #f:表示する候補なし
(define (tutcode-check-prediction-make pc force-check?)
  (if (eq? (tutcode-context-predicting pc) 'tutcode-predicting-off)
    (let* ((head (tutcode-context-head pc))
           (preedit-len (length head)))
      (if
        (or (>= preedit-len tutcode-prediction-start-char-count)
            force-check?)
        (let*
          ((preconv-str (string-list-concat head))
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
                  (tutcode-context-set-predicting! pc
                    'tutcode-predicting-prediction)
                  #t)
                #f))
            #f))
        #f))
    #f))

;;; 部首合成変換中に予測入力候補を検索して候補ウィンドウに(遅延)表示する
;;; @param char 入力された部首1
(define (tutcode-check-bushu-prediction pc char)
  (if (tutcode-candidate-window-enable-delay? pc
        tutcode-candidate-window-activate-delay-for-bushu-prediction)
    (begin
      (tutcode-context-set-prediction-bushu! pc char) ; 遅延呼出時用に一時保持
      (tutcode-activate-candidate-window pc
        'tutcode-candidate-window-predicting
        tutcode-candidate-window-activate-delay-for-bushu-prediction
        -1 -1))
    (tutcode-check-bushu-prediction-make pc char #t)))

;;; 部首合成変換中に予測入力候補を検索して候補リストを作成する
;;; @param char 入力された部首1
;;; @param show-candwin? 候補リストの作成後候補ウィンドウの表示を行うかどうか
;;; @return #t:表示する候補あり, #f:表示する候補なし
(define (tutcode-check-bushu-prediction-make pc char show-candwin?)
  (let ((res
          (case tutcode-bushu-conversion-algorithm
            ((tc-2.3.1-22.6)
              (tutcode-bushu-predict-tc23 char))
            (else ; 'tc-2.1+ml1925
              (tutcode-bushu-predict-tc21 char)))))
    (tutcode-context-set-prediction-bushu! pc res)
    (tutcode-context-set-prediction-bushu-page-start! pc 0)
    (tutcode-bushu-prediction-make-page pc 0 show-candwin?)))

;;; 部首合成変換中に予測入力候補を検索して候補リストを作成する
;;; @param char 入力された部首1
;;; @return (<部首2> <合成文字>)のリスト
(define (tutcode-bushu-predict-tc21 char)
  (let* ((res (tutcode-bushu-predict char tutcode-bushudic))
         (alt (assoc char tutcode-bushudic-altchar))
         (altres
          (if alt
            (tutcode-bushu-predict (cadr alt) tutcode-bushudic)
            ()))
         (resall (append res altres)))
    resall))

;;; 部首合成変換中に予測入力候補を検索して候補リストを作成する
;;; @param char 入力された部首1
;;; @return (<部首2> <合成文字>)のリスト
(define (tutcode-bushu-predict-tc23 char)
  (let ((gosei (tutcode-bushu-compose-tc23 (list char) #f)))
    (map
      (lambda (elem)
        (list #f elem))
      gosei)))

;;; 部首合成変換の予測入力候補のうち、
;;; 指定された番号から始まる1ページぶんの候補リストを作成する。
;;; @param start-index 開始番号
;;; @param show-candwin? 候補リストの作成後候補ウィンドウの表示を行うかどうか。
;;;  #fの場合は、候補リストの作成のみ行う(delay-activating-handler時)
;;; @return #t:表示する候補あり, #f:表示する候補なし
(define (tutcode-bushu-prediction-make-page pc start-index show-candwin?)
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
            (tutcode-context-set-predicting! pc 'tutcode-predicting-bushu)
            (if show-candwin?
              (tutcode-activate-candidate-window pc
                'tutcode-candidate-window-predicting
                0 nr-all page-limit))
            #t)
          #f))
      #f)))

;;; 補完候補と熟語ガイド表示のためのcandwin用パラメータを計算する
;;; @param nr 補完候補数
;;; @param nr-guide 熟語ガイド候補数
;;; @return (<全候補数> <ページごとの候補数上限> <ページごとの補完候補数上限>)
(define (tutcode-prediction-calc-window-param nr nr-guide)
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
     (rk-commit-flush
      (lambda ()
        (if (and tutcode-keep-illegal-sequence?
                 (pair? (rk-context-seq rkc)))
          (tutcode-commit pc (rk-pending rkc) #f #t))
        (rk-flush rkc)))
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
           (rk-commit-flush)
           (tutcode-context-set-commit-strs! pc ())
           (tutcode-context-set-state! pc 'tutcode-state-off)
           (tutcode-commit-raw pc key key-state)) ; ESCキーをアプリにも渡す
          ((tutcode-off-key? key key-state)
           (rk-commit-flush)
           (tutcode-context-set-commit-strs! pc ())
           (tutcode-context-set-state! pc 'tutcode-state-off))
          ((tutcode-on-key? key key-state) ; off-keyとon-keyが別キーの場合
           ;; 現在onでもoffでも、on-keyでonにしたいだけなのでcommit-rawを回避
           (rk-commit-flush))
          ((tutcode-kigou-toggle-key? key key-state)
           (rk-commit-flush)
           (tutcode-begin-kigou-mode pc))
          ((tutcode-kigou2-toggle-key? key key-state)
           (rk-commit-flush)
           (tutcode-toggle-kigou2-mode pc))
          ((and (tutcode-kana-toggle-key? key key-state)
                (not (tutcode-kigou2-mode? pc)))
           (rk-commit-flush)
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
           (rk-commit-flush)
           (if completing?
             ;; 補完中にbegin-completin-keyが押されたら対象文字を1減らして再補完
             ;; (意図しない文字列で補完された場合に、補完し直しができるように)
             ;; 対象が1文字未満になる場合は補完ウィンドウを閉じる(再表示しない)
             (let ((len (tutcode-context-commit-strs-used-len pc)))
               (if (> len 1)
                 (tutcode-check-completion pc #t (- len 1))))
             (tutcode-check-completion pc #t 0)))
          ((and (tutcode-paste-key? key key-state)
                (pair? (tutcode-context-parent-context pc)))
            (let ((latter-seq (tutcode-clipboard-acquire-text-wo-nl pc 'full)))
              (if (pair? latter-seq)
                (tutcode-commit pc (string-list-concat latter-seq)))))
          ((or
            (symbol? key)
            (and
              (modifier-key-mask key-state)
              (not (shift-key-mask key-state))))
           (rk-commit-flush)
           (tutcode-commit-raw pc key key-state))
          ;; 補完候補用ラベルキー?
          ((and completing?
                (tutcode-heading-label-char-for-prediction? key)
                (tutcode-commit-by-label-key-for-prediction pc
                  (charcode->string key) 'tutcode-predicting-completion)))
          ;; 正しくないキーシーケンスは全て捨てる(tc2に合わせた動作)。
          ;; (rk-push-key!すると、途中までのシーケンスは捨てられるが、
          ;; 間違ったキーは残ってしまうので、rk-push-key!は使えない)
          ((not (rk-expect-key? rkc (charcode->string key)))
           (if (> (length (rk-context-seq rkc)) 0)
             (begin
               (cond
                 ((tutcode-verbose-stroke-key? key key-state)
                   (tutcode-commit pc (rk-pending rkc) #f #t))
                 (tutcode-keep-illegal-sequence?
                   (tutcode-commit pc (rk-pending rkc) #f #t)
                   (tutcode-commit-raw pc key key-state)))
               (rk-flush rkc))
             ;; 単独のキー入力(TUT-Code入力でなくて)
             (tutcode-commit-raw pc key key-state)))
          (else
           (let ((res (tutcode-push-key! pc (charcode->string key))))
            (cond
              ((string? res)
                (tutcode-commit pc res #f (not (tutcode-kigou2-mode? pc)))
                (if (and tutcode-use-completion?
                         (> tutcode-completion-chars-min 0))
                  (tutcode-check-completion pc #f 0)))
              ((symbol? res)
                (case res
                  ((tutcode-mazegaki-start)
                    (tutcode-context-set-latin-conv! pc #f)
                    (tutcode-context-set-postfix-yomi-len! pc 0)
                    (tutcode-context-set-state! pc 'tutcode-state-yomi))
                  ((tutcode-latin-conv-start)
                    (tutcode-context-set-latin-conv! pc #t)
                    (tutcode-context-set-postfix-yomi-len! pc 0)
                    (tutcode-context-set-state! pc 'tutcode-state-yomi))
                  ((tutcode-kanji-code-input-start)
                    (tutcode-context-set-state! pc 'tutcode-state-code))
                  ((tutcode-bushu-start)
                    (tutcode-context-set-undo! pc ());再帰的部首合成変換のundo用
                    (tutcode-context-set-state! pc 'tutcode-state-bushu)
                    (tutcode-append-string pc "▲"))
                  ((tutcode-interactive-bushu-start)
                    (tutcode-context-set-prediction-nr! pc 0)
                    (tutcode-context-set-state! pc
                      'tutcode-state-interactive-bushu))
                  ((tutcode-history-start)
                    (tutcode-begin-history pc))
                  ((tutcode-undo)
                    (tutcode-undo pc))
                  ((tutcode-help)
                    (tutcode-help pc))
                  ((tutcode-help-clipboard)
                    (tutcode-help-clipboard pc))
                  ((tutcode-auto-help-redisplay)
                    (tutcode-auto-help-redisplay pc))
                  ((tutcode-postfix-bushu-start)
                    (tutcode-begin-postfix-bushu-conversion pc))
                  ((tutcode-postfix-mazegaki-start)
                    (tutcode-begin-postfix-mazegaki-conversion pc #f #f #f))
                  ((tutcode-postfix-mazegaki-1-start)
                    (tutcode-begin-postfix-mazegaki-conversion pc 1 #t
                      tutcode-use-recursive-learning?))
                  ((tutcode-postfix-mazegaki-2-start)
                    (tutcode-begin-postfix-mazegaki-conversion pc 2 #t
                      tutcode-use-recursive-learning?))
                  ((tutcode-postfix-mazegaki-3-start)
                    (tutcode-begin-postfix-mazegaki-conversion pc 3 #t
                      tutcode-use-recursive-learning?))
                  ((tutcode-postfix-mazegaki-4-start)
                    (tutcode-begin-postfix-mazegaki-conversion pc 4 #t
                      tutcode-use-recursive-learning?))
                  ((tutcode-postfix-mazegaki-5-start)
                    (tutcode-begin-postfix-mazegaki-conversion pc 5 #t
                      tutcode-use-recursive-learning?))
                  ((tutcode-postfix-mazegaki-6-start)
                    (tutcode-begin-postfix-mazegaki-conversion pc 6 #t
                      tutcode-use-recursive-learning?))
                  ((tutcode-postfix-mazegaki-7-start)
                    (tutcode-begin-postfix-mazegaki-conversion pc 7 #t
                      tutcode-use-recursive-learning?))
                  ((tutcode-postfix-mazegaki-8-start)
                    (tutcode-begin-postfix-mazegaki-conversion pc 8 #t
                      tutcode-use-recursive-learning?))
                  ((tutcode-postfix-mazegaki-9-start)
                    (tutcode-begin-postfix-mazegaki-conversion pc 9 #t
                      tutcode-use-recursive-learning?))
                  ((tutcode-postfix-mazegaki-inflection-start)
                    (tutcode-begin-postfix-mazegaki-inflection-conversion pc #f))
                  ((tutcode-postfix-mazegaki-inflection-1-start)
                    (tutcode-begin-postfix-mazegaki-inflection-conversion pc 1))
                  ((tutcode-postfix-mazegaki-inflection-2-start)
                    (tutcode-begin-postfix-mazegaki-inflection-conversion pc 2))
                  ((tutcode-postfix-mazegaki-inflection-3-start)
                    (tutcode-begin-postfix-mazegaki-inflection-conversion pc 3))
                  ((tutcode-postfix-mazegaki-inflection-4-start)
                    (tutcode-begin-postfix-mazegaki-inflection-conversion pc 4))
                  ((tutcode-postfix-mazegaki-inflection-5-start)
                    (tutcode-begin-postfix-mazegaki-inflection-conversion pc 5))
                  ((tutcode-postfix-mazegaki-inflection-6-start)
                    (tutcode-begin-postfix-mazegaki-inflection-conversion pc 6))
                  ((tutcode-postfix-mazegaki-inflection-7-start)
                    (tutcode-begin-postfix-mazegaki-inflection-conversion pc 7))
                  ((tutcode-postfix-mazegaki-inflection-8-start)
                    (tutcode-begin-postfix-mazegaki-inflection-conversion pc 8))
                  ((tutcode-postfix-mazegaki-inflection-9-start)
                    (tutcode-begin-postfix-mazegaki-inflection-conversion pc 9))
                  ((tutcode-postfix-katakana-start)
                    (tutcode-begin-postfix-katakana-conversion pc #f))
                  ((tutcode-postfix-katakana-0-start)
                    (tutcode-begin-postfix-katakana-conversion pc 0))
                  ((tutcode-postfix-katakana-1-start)
                    (tutcode-begin-postfix-katakana-conversion pc 1))
                  ((tutcode-postfix-katakana-2-start)
                    (tutcode-begin-postfix-katakana-conversion pc 2))
                  ((tutcode-postfix-katakana-3-start)
                    (tutcode-begin-postfix-katakana-conversion pc 3))
                  ((tutcode-postfix-katakana-4-start)
                    (tutcode-begin-postfix-katakana-conversion pc 4))
                  ((tutcode-postfix-katakana-5-start)
                    (tutcode-begin-postfix-katakana-conversion pc 5))
                  ((tutcode-postfix-katakana-6-start)
                    (tutcode-begin-postfix-katakana-conversion pc 6))
                  ((tutcode-postfix-katakana-7-start)
                    (tutcode-begin-postfix-katakana-conversion pc 7))
                  ((tutcode-postfix-katakana-8-start)
                    (tutcode-begin-postfix-katakana-conversion pc 8))
                  ((tutcode-postfix-katakana-9-start)
                    (tutcode-begin-postfix-katakana-conversion pc 9))
                  ((tutcode-postfix-katakana-exclude-1)
                    (tutcode-begin-postfix-katakana-conversion pc -1))
                  ((tutcode-postfix-katakana-exclude-2)
                    (tutcode-begin-postfix-katakana-conversion pc -2))
                  ((tutcode-postfix-katakana-exclude-3)
                    (tutcode-begin-postfix-katakana-conversion pc -3))
                  ((tutcode-postfix-katakana-exclude-4)
                    (tutcode-begin-postfix-katakana-conversion pc -4))
                  ((tutcode-postfix-katakana-exclude-5)
                    (tutcode-begin-postfix-katakana-conversion pc -5))
                  ((tutcode-postfix-katakana-exclude-6)
                    (tutcode-begin-postfix-katakana-conversion pc -6))
                  ((tutcode-postfix-katakana-shrink-1)
                    (tutcode-postfix-katakana-shrink pc 1))
                  ((tutcode-postfix-katakana-shrink-2)
                    (tutcode-postfix-katakana-shrink pc 2))
                  ((tutcode-postfix-katakana-shrink-3)
                    (tutcode-postfix-katakana-shrink pc 3))
                  ((tutcode-postfix-katakana-shrink-4)
                    (tutcode-postfix-katakana-shrink pc 4))
                  ((tutcode-postfix-katakana-shrink-5)
                    (tutcode-postfix-katakana-shrink pc 5))
                  ((tutcode-postfix-katakana-shrink-6)
                    (tutcode-postfix-katakana-shrink pc 6))
                  ((tutcode-postfix-kanji2seq-start)
                    (tutcode-begin-postfix-kanji2seq-conversion pc #f))
                  ((tutcode-postfix-kanji2seq-1-start)
                    (tutcode-begin-postfix-kanji2seq-conversion pc 1))
                  ((tutcode-postfix-kanji2seq-2-start)
                    (tutcode-begin-postfix-kanji2seq-conversion pc 2))
                  ((tutcode-postfix-kanji2seq-3-start)
                    (tutcode-begin-postfix-kanji2seq-conversion pc 3))
                  ((tutcode-postfix-kanji2seq-4-start)
                    (tutcode-begin-postfix-kanji2seq-conversion pc 4))
                  ((tutcode-postfix-kanji2seq-5-start)
                    (tutcode-begin-postfix-kanji2seq-conversion pc 5))
                  ((tutcode-postfix-kanji2seq-6-start)
                    (tutcode-begin-postfix-kanji2seq-conversion pc 6))
                  ((tutcode-postfix-kanji2seq-7-start)
                    (tutcode-begin-postfix-kanji2seq-conversion pc 7))
                  ((tutcode-postfix-kanji2seq-8-start)
                    (tutcode-begin-postfix-kanji2seq-conversion pc 8))
                  ((tutcode-postfix-kanji2seq-9-start)
                    (tutcode-begin-postfix-kanji2seq-conversion pc 9))
                  ((tutcode-postfix-seq2kanji-start)
                    (tutcode-begin-postfix-seq2kanji-conversion pc #f))
                  ((tutcode-postfix-seq2kanji-1-start)
                    (tutcode-begin-postfix-seq2kanji-conversion pc 1))
                  ((tutcode-postfix-seq2kanji-2-start)
                    (tutcode-begin-postfix-seq2kanji-conversion pc 2))
                  ((tutcode-postfix-seq2kanji-3-start)
                    (tutcode-begin-postfix-seq2kanji-conversion pc 3))
                  ((tutcode-postfix-seq2kanji-4-start)
                    (tutcode-begin-postfix-seq2kanji-conversion pc 4))
                  ((tutcode-postfix-seq2kanji-5-start)
                    (tutcode-begin-postfix-seq2kanji-conversion pc 5))
                  ((tutcode-postfix-seq2kanji-6-start)
                    (tutcode-begin-postfix-seq2kanji-conversion pc 6))
                  ((tutcode-postfix-seq2kanji-7-start)
                    (tutcode-begin-postfix-seq2kanji-conversion pc 7))
                  ((tutcode-postfix-seq2kanji-8-start)
                    (tutcode-begin-postfix-seq2kanji-conversion pc 8))
                  ((tutcode-postfix-seq2kanji-9-start)
                    (tutcode-begin-postfix-seq2kanji-conversion pc 9))
                  ((tutcode-selection-mazegaki-start)
                    (tutcode-begin-selection-mazegaki-conversion pc))
                  ((tutcode-selection-mazegaki-inflection-start)
                    (tutcode-begin-selection-mazegaki-inflection-conversion pc))
                  ((tutcode-selection-katakana-start)
                    (tutcode-begin-selection-katakana-conversion pc))
                  ((tutcode-selection-kanji2seq-start)
                    (tutcode-begin-selection-kanji2seq-conversion pc))
                  ((tutcode-selection-seq2kanji-start)
                    (tutcode-begin-selection-seq2kanji-conversion pc))
                  ((tutcode-clipboard-seq2kanji-start)
                    (tutcode-begin-clipboard-seq2kanji-conversion pc))))
              ((procedure? res)
                (res 'tutcode-state-on pc))))))))))

;;; 後置型部首合成変換を行う
(define (tutcode-begin-postfix-bushu-conversion pc)
  (and-let*
    ((former-seq (tutcode-postfix-acquire-text pc 2))
     (res (and (>= (length former-seq) 2)
               (tutcode-bushu-convert (cadr former-seq) (car former-seq)))))
    (tutcode-postfix-commit pc res former-seq)
    (tutcode-check-auto-help-window-begin pc (list res) ())))

;;; 後置型/前置型変換の確定をundoする
(define (tutcode-undo pc)
  (let ((undo (tutcode-context-undo pc)))
    (if (pair? undo)
      (let ((state (list-ref undo 0))
            (commit-len (list-ref undo 1))
            (data (list-ref undo 2)))
        (tutcode-postfix-delete-text pc commit-len)
        (case state
          ((tutcode-state-off) ; 後置型変換
            (tutcode-commit pc (string-list-concat data) #f #t))
          ((tutcode-state-converting)
            (tutcode-context-set-head! pc (list-ref data 0))
            (tutcode-context-set-latin-conv! pc (list-ref data 1))
            (tutcode-context-set-state! pc 'tutcode-state-yomi))
          ((tutcode-state-bushu)
            (tutcode-context-set-head! pc data)
            (tutcode-context-set-state! pc 'tutcode-state-bushu))
          ((tutcode-state-yomi)
            (tutcode-context-set-head! pc data)
            (tutcode-context-set-state! pc 'tutcode-state-yomi))
          ((tutcode-state-code)
            (tutcode-context-set-head! pc data)
            (tutcode-context-set-state! pc 'tutcode-state-code))
          )))))

;;; 後置型/前置型変換の確定をundoするための準備
;;; @param state 変換の種類('tutcode-state-converting)
;;; @param commit-str 確定文字列
;;; @param data 確定前の状態を再現するのに必要な情報
(define (tutcode-undo-prepare pc state commit-str data)
  (let ((commit-len (length (string-to-list commit-str))))
    ;; XXX: 多段undoは未対応
    (tutcode-context-set-undo! pc (list state commit-len data))))

;;; 後置型変換を確定する
;;; @param str 確定する文字列
;;; @param yomi-list 変換元の文字列(読み/部首)のリスト(逆順)
(define (tutcode-postfix-commit pc str yomi-list)
  ;; Firefoxで削除位置がずれるのを回避するためpreeditをclearしてからdelete-text
  (im-clear-preedit pc)
  (im-update-preedit pc)
  (tutcode-postfix-delete-text pc (length yomi-list))
  (tutcode-commit pc str)
  (tutcode-undo-prepare pc 'tutcode-state-off str yomi-list))

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

;;; ASCII文字かどうかを返す
;;; @param str 文字列
(define (tutcode-ascii? str)
  (let ((ch (string->ichar str)))
    (and ch (<= ch 127))))

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
      (let ((last-ascii? (and (pair? former-seq)
                              (tutcode-ascii? (car former-seq)))))
        (take-while
          (lambda (elem)
            (and
              ;; 日本語文字とASCII文字の境目があれば、そこまでを取得する
              (eq? (tutcode-ascii? elem) last-ascii?)
              ;; "、"や"。"以前の文字は読みに含めない。
              (not (member elem tutcode-postfix-mazegaki-terminate-char-list))))
          former-seq)))))

;;; 確定済文字列を取得する
;;; @param len 取得する文字数
;;; @return 取得した文字列のリスト(逆順)
(define (tutcode-postfix-acquire-text pc len)
  (let ((ppc (tutcode-context-parent-context pc)))
    (if (not (null? ppc))
      (case (tutcode-context-child-type ppc)
        ((tutcode-child-type-dialog)
          ())
        ((tutcode-child-type-editor)
          (let*
            ((ec (tutcode-context-editor ppc))
             (left-string (tutcode-editor-left-string ec)))
            (if (> (length left-string) len)
              (take left-string len)
              left-string)))
        ((tutcode-child-type-seq2kanji)
          (let ((head (tutcode-context-head ppc)))
            (if (> (length head) len)
              (take head len)
              head))))
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
      (case (tutcode-context-child-type ppc)
        ((tutcode-child-type-editor)
          (let*
            ((ec (tutcode-context-editor ppc))
             (left-string (tutcode-editor-left-string ec)))
            (tutcode-editor-set-left-string! ec
              (if (> (length left-string) len)
                (drop left-string len)
                ()))))
        ((tutcode-child-type-seq2kanji)
          (let ((head (tutcode-context-head ppc)))
            (tutcode-context-set-head! ppc
              (if (> (length head) len)
                (drop head len)
                ())))))
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
                (apply string-append
                  (make-list len tutcode-fallback-backspace-string))
                #t #t))))))))

;;; selectionに対して交ぜ書き変換を開始する
(define (tutcode-begin-selection-mazegaki-conversion pc)
  (let ((sel (tutcode-selection-acquire-text-wo-nl pc)))
    (if (pair? sel)
      (let ((sel-len (length sel)))
        (tutcode-context-set-mazegaki-yomi-len-specified! pc sel-len)
        (tutcode-context-set-postfix-yomi-len! pc (- sel-len)) ; 負:selection
        (tutcode-context-set-mazegaki-yomi-all! pc sel)
        (tutcode-begin-conversion pc sel () #t
          tutcode-use-recursive-learning?)))))

;;; selectionに対して活用する語として交ぜ書き変換を開始する
(define (tutcode-begin-selection-mazegaki-inflection-conversion pc)
  (let ((sel (tutcode-selection-acquire-text-wo-nl pc)))
    (if (pair? sel)
      (let ((sel-len (length sel)))
        (tutcode-context-set-mazegaki-yomi-len-specified! pc sel-len)
        (tutcode-context-set-postfix-yomi-len! pc (- sel-len)) ; 負:selection
        (tutcode-context-set-mazegaki-yomi-all! pc sel)
        (if (tutcode-mazegaki-inflection? sel)
          (tutcode-begin-conversion pc sel () #t
            tutcode-use-recursive-learning?)
          (tutcode-mazegaki-inflection-relimit-right pc
            sel-len sel-len #f))))))

;;; selectionに対してカタカナ変換を開始する
(define (tutcode-begin-selection-katakana-conversion pc)
  (let ((sel (tutcode-selection-acquire-text pc)))
    (if (pair? sel)
      (let* ((katakana (tutcode-katakana-convert sel
                        (not (tutcode-context-katakana-mode? pc))))
             (str (string-list-concat katakana)))
        (tutcode-selection-commit pc str sel)
        (if (= (length katakana) 1) ; 1文字の場合、自動ヘルプ表示(tc2と同様)
          (tutcode-check-auto-help-window-begin pc katakana ()))))))

;;; selectionに対して漢字→入力シーケンス変換を開始する
(define (tutcode-begin-selection-kanji2seq-conversion pc)
  (let ((sel (tutcode-selection-acquire-text pc)))
    (if (pair? sel)
      (tutcode-selection-commit pc
        (string-list-concat (tutcode-kanji-list->sequence pc sel)) sel))))

;;; selectionに対して入力シーケンス→漢字変換を開始する
(define (tutcode-begin-selection-seq2kanji-conversion pc)
  (let ((sel (tutcode-selection-acquire-text pc)))
    (if (pair? sel)
      (tutcode-selection-commit pc
        (string-list-concat (tutcode-sequence->kanji-list pc sel)) sel))))

;;; selectionに対する変換を確定する
;;; @param str 確定する文字列
;;; @param yomi-list 変換元の文字列(読み/部首)のリスト(逆順)
(define (tutcode-selection-commit pc str yomi-list)
  ;; commitするとselectionが上書きされるのでdelete-textは不要
  ;(im-delete-text pc 'selection 'beginning 0 'full)
  (tutcode-commit pc str)
  (tutcode-undo-prepare pc 'tutcode-state-off str yomi-list))

;;; selection文字列を改行を除いて取得する
;;; @return 取得した文字列のリスト(逆順)
(define (tutcode-selection-acquire-text-wo-nl pc)
  (let ((latter-seq (tutcode-selection-acquire-text pc)))
    (and (pair? latter-seq)
         (delete "\n" latter-seq))))

;;; selection文字列を取得する
;;; @return 取得した文字列のリスト(逆順)
(define (tutcode-selection-acquire-text pc)
  (and-let*
    ((ustr (im-acquire-text pc 'selection 'beginning 0 'full))
     (latter (ustr-latter-seq ustr))
     (latter-seq (and (pair? latter) (string-to-list (car latter)))))
    (and (not (null? latter-seq))
         latter-seq)))

;;; 後置型カタカナ変換を確定する
;;; @param yomi 読み
;;; @param katakana 読みをカタカナに変換した文字列リスト
;;; @param show-help? katakanaが1文字の場合に自動ヘルプを表示するかどうか
(define (tutcode-postfix-katakana-commit pc yomi katakana show-help?)
  (let ((str (string-list-concat katakana)))
    (tutcode-postfix-commit pc str yomi)
    (tutcode-flush pc)
    (if (and show-help?
             (= (length katakana) 1)) ; 1文字の場合、自動ヘルプ表示(tc2と同様)
      (tutcode-check-auto-help-window-begin pc katakana ()))))

;;; 後置型カタカナ変換を開始する
;;; @param yomi-len 指定された読みの文字数。指定されてない場合は#f。
;;;   0: ひらがなやーが続く間カタカナに変換する。
;;;   負の値: 絶対値の文字数をひらがなとして残してカタカナ変換。
;;;   (カタカナに変換する文字列が長くて文字数を数えるのが面倒な場合向け)
;;;   「例えばあぷりけーしょん」j2→「例えばアプリケーション」
(define (tutcode-begin-postfix-katakana-conversion pc yomi-len)
  (let*
    ((former-all (tutcode-postfix-katakana-acquire-yomi pc
      (if (and yomi-len (<= yomi-len 0)) #f yomi-len)))
     (former-seq
      (cond
        ((not yomi-len)
          former-all)
        ((>= yomi-len 0)
          former-all)
        (else
          (let ((len (- yomi-len)))
            (if (<= (length former-all) len)
              ()
              (drop-right former-all len)))))))
    (if yomi-len
      (let ((katakana (tutcode-katakana-convert former-seq
                        (not (tutcode-context-katakana-mode? pc)))))
        (tutcode-postfix-katakana-commit pc former-seq katakana #t))
      ;; 読みの文字数が指定されていない
      (if (pair? former-seq)
        (begin
          (tutcode-context-set-mazegaki-yomi-all! pc former-all)
          (tutcode-context-set-head! pc
            (tutcode-katakana-convert former-seq
              (not (tutcode-context-katakana-mode? pc))))
          (tutcode-context-set-state! pc 'tutcode-state-postfix-katakana))))))

;;; 直前の後置型カタカナ変換を縮める
;;; @param count 縮める文字数
(define (tutcode-postfix-katakana-shrink pc count)
  (let ((undo (tutcode-context-undo pc)))
    (if (and (pair? undo)
             (eq? (list-ref undo 0) 'tutcode-state-off)) ; 後置型変換
      (let* ((commit-len (list-ref undo 1))
             (yomi (list-ref undo 2))
             (yomi-len (length yomi)))
        (tutcode-postfix-delete-text pc commit-len)
        (receive (kata hira)
          (if (< count yomi-len)
            (split-at yomi (- yomi-len count))
            (values () yomi))
          (let ((str (string-list-concat
                      (append
                        (tutcode-katakana-convert kata
                          (not (tutcode-context-katakana-mode? pc)))
                        hira))))
            (tutcode-commit pc str)
            ;; shrinkを繰り返した際にcount文字ずつカタカナを縮められるように
            (tutcode-undo-prepare pc 'tutcode-state-off
              (string-list-concat kata) kata)))))))

;;; 後置型カタカナ変換の対象文字列を取得する
;;; @param yomi-len 指定された文字数。指定されてない場合は#f。
;;; @return 取得した文字列(文字列の逆順リスト)
(define (tutcode-postfix-katakana-acquire-yomi pc yomi-len)
  (let ((former-seq (tutcode-postfix-acquire-text pc
                     (or yomi-len tutcode-mazegaki-yomi-max))))
    (if yomi-len
      former-seq
      (let ((hira (take-while
                    (lambda (char)
                      (tutcode-postfix-katakana-acquire-char? pc char))
                    former-seq)))
        ;; 「キーとばりゅー」に対して、1文字残してカタカナ変換で
        ;; 「キーとバリュー」になるように、ひらがな列が「ー」で始まる場合は除去
        (reverse
          (drop-while
            (lambda (char)
              (member char tutcode-postfix-katakana-char-list))
            (reverse hira)))))))

;;; 後置型カタカナ変換対象文字(ひらがな、ー)かどうかを返す
(define (tutcode-postfix-katakana-acquire-char? pc char)
  (or (if (tutcode-context-katakana-mode? pc)
        (tutcode-katakana? char) ; カタカナモード時はカタカナ対象
        (tutcode-hiragana? char))
      (member char tutcode-postfix-katakana-char-list)))

;;; ひらがなかどうか
(define (tutcode-hiragana? s)
  (and (string>=? s "ぁ") (string<=? s "ん")))
;;; カタカナかどうか
(define (tutcode-katakana? s)
  (and (string>=? s "ァ") (string<=? s "ン")))

;;; 後置型カタカナ変換モード時のキー入力を処理する。
;;; @param key 入力されたキー
;;; @param key-state コントロールキー等の状態
(define (tutcode-proc-state-postfix-katakana c key key-state)
  (let*
    ((pc (tutcode-find-descendant-context c))
     (head (tutcode-context-head pc)))
    (cond
      ((tutcode-cancel-key? key key-state)
        (tutcode-flush pc))
      ((or (tutcode-commit-key? key key-state)
           (tutcode-return-key? key key-state))
        (tutcode-postfix-katakana-commit pc
          (take (tutcode-context-mazegaki-yomi-all pc) (length head))
          head #t))
      ((tutcode-mazegaki-relimit-right-key? key key-state)
        (if (> (length head) 1)
          (tutcode-context-set-head! pc (drop-right head 1))))
      ((tutcode-mazegaki-relimit-left-key? key key-state)
        (let ((yomi-all (tutcode-context-mazegaki-yomi-all pc))
              (cur-len (length head)))
          (if (> (length yomi-all) cur-len)
            (tutcode-context-set-head! pc
              (tutcode-katakana-convert
                (take yomi-all (+ cur-len 1))
                (not (tutcode-context-katakana-mode? pc)))))))
      (else
        (tutcode-postfix-katakana-commit pc
          (take (tutcode-context-mazegaki-yomi-all pc) (length head))
          head #f)
        (tutcode-proc-state-on pc key key-state)))))

;;; 漢字のリストを入力シーケンスに変換する。
;;; 漢字入力を終端するために入力された余分なverbose-stroke-keyは削除して返す。
;;; @param kanji-list 漢字文字列リスト(逆順)
;;; @return 入力シーケンス文字列リスト(逆順)。
;;;  このシーケンスを入力すればkanji-listが生成される。
;;;XXX:コード表に、ある漢字に対する複数のシーケンスがある場合、最初のものを使用
;;;    カタカナ入力用大文字定義もある場合、小文字シーケンスが使われる。
;;;    例:"CODE "と打鍵、"CODー"と表示、漢字→入力シーケンス変換すると"CODe "
(define (tutcode-kanji-list->sequence pc kanji-list)
  (let*
    ((rule (rk-context-rule (tutcode-context-rk-context pc)))
     (allseq
      (append-map
        (lambda (x)
          (let ((seq (tutcode-reverse-find-seq x rule)))
            ;; 直接入力できない場合は漢字のまま(XXX:部首合成方法まで表示?)
            (if seq (reverse seq) (list x))))
        kanji-list)))
    ;; 最後のverbose-stroke-key(例:":")は漢字入力を終端するため
    ;; 余分に入力された可能性があるので削る
    ;; 例: "undo:"→"趣o"→("o" "趣")、"code:"→"演各:"→(":" "各" "演")
    (if (and (pair? allseq)
             (tutcode-verbose-stroke-key? (string->ichar (car allseq)) 0))
      (cdr allseq)
      allseq)))

;;; 後置型の漢字→入力シーケンス変換を開始する
;;; @param yomi-len 指定された読みの文字数。指定されてない場合は#f。
;;;  0の場合は、#fと同様に扱うが、対象文字列選択モードに入らず、すぐに確定。
(define (tutcode-begin-postfix-kanji2seq-conversion pc yomi-len)
  (let*
    ((former-all (tutcode-postfix-acquire-text pc
      (if (and yomi-len (> yomi-len 0)) yomi-len tutcode-mazegaki-yomi-max)))
     (former-seq
      (if (and yomi-len (> yomi-len 0))
        former-all
        (let*
          ;; verbose-stroke-keyが" "(デフォルト)の場合、余分に入力されてると
          ;; 何もtakeされないので、余分なverbose-stroke-keyはスキップ。
          ;; (tutcode-kanji-list->sequenceで余分なverbose-stroke-keyを削除)
          ;; 例:"undo "→"趣・"→("・" "趣")、"code "→"演各 "→(" " "各" "演")
          ((first (safe-car former-all))
           (first-verbose-key?
            (tutcode-verbose-stroke-key? (and first (string->ichar first)) 0))
           (rest (if first-verbose-key? (cdr former-all) former-all))
           (seq
            (take-while
              (lambda (elem)
                (not (member elem
                      (append tutcode-postfix-kanji2seq-delimiter-char-list
                        '("\n" "\t")))))
              rest)))
          (if first-verbose-key?
            (cons first seq) ; delete-textする長さを合わせるためfirstは入れる
            seq)))))
    (if yomi-len
      (let ((seq (tutcode-kanji-list->sequence pc former-seq)))
        (tutcode-postfix-commit pc (string-list-concat seq) former-seq))
      ;; 読みの文字数が指定されていない
      (if (pair? former-seq)
        (begin
          (tutcode-context-set-mazegaki-yomi-all! pc former-all)
          (tutcode-context-set-postfix-yomi-len! pc (length former-seq))
          (tutcode-context-set-head! pc
            (tutcode-kanji-list->sequence pc former-seq))
          (tutcode-context-set-state! pc 'tutcode-state-postfix-kanji2seq))))))

;;; 後置型の漢字→入力シーケンス変換モード時のキー入力を処理する。
;;; @param key 入力されたキー
;;; @param key-state コントロールキー等の状態
(define (tutcode-proc-state-postfix-kanji2seq c key key-state)
  (let*
    ((pc (tutcode-find-descendant-context c))
     (head (tutcode-context-head pc))
     (yomi-len (tutcode-context-postfix-yomi-len pc))
     (yomi-all (tutcode-context-mazegaki-yomi-all pc))
     (update-context!
      (lambda (new-yomi-len)
        (tutcode-context-set-postfix-yomi-len! pc new-yomi-len)
        (tutcode-context-set-head! pc
          (tutcode-kanji-list->sequence pc (take yomi-all new-yomi-len)))))
     (commit
      (lambda ()
        (let*
          ((len (if (and tutcode-delete-leading-delimiter-on-postfix-kanji2seq?
                         (> (length yomi-all) yomi-len)
                         (member (list-ref yomi-all yomi-len)
                          tutcode-postfix-kanji2seq-delimiter-char-list))
                  (+ yomi-len 1)
                  yomi-len))
           (yomi (take yomi-all len)))
          (tutcode-postfix-commit pc (string-list-concat head) yomi)
          (tutcode-flush pc)))))
    (cond
      ((tutcode-cancel-key? key key-state)
        (tutcode-flush pc))
      ((or (tutcode-commit-key? key key-state)
           (tutcode-return-key? key key-state))
        (commit))
      ((tutcode-mazegaki-relimit-right-key? key key-state)
        (if (> yomi-len 1)
          (update-context! (- yomi-len 1))))
      ((tutcode-mazegaki-relimit-left-key? key key-state)
        (if (> (length yomi-all) yomi-len)
          (update-context! (+ yomi-len 1))))
      (else
        (commit)
        (tutcode-proc-state-on pc key key-state)))))

;;; 入力キーシーケンスを漢字に変換する
;;; @param sequence 入力キーシーケンス。文字列のリスト(逆順)
;;; @return 変換後の漢字文字列のリスト(逆順)
(define (tutcode-sequence->kanji-list pc sequence)
  (if (null? sequence)
    ()
    (let ((string->key-and-status
            (lambda (s)
              (let ((ch (string->ichar s)))
                (cond
                  ;; key-press-handlerに渡すため、symbolに変換(uim-key.c)
                  ;; (tutcode-return-key?等でマッチするようにするため)
                  ((not (integer? ch)) (cons ch 0)) ; sが漢字の場合chは#f
                  ((= ch 8) '(backspace . 0))
                  ((= ch 9) '(tab . 0))
                  ((= ch 10) '(return . 0))
                  ((= ch 27) '(escape . 0))
                  ((= ch 127) '(delete . 0))
                  ((ichar-control? ch)
                    (cons (ichar-downcase (+ ch 64)) 2)) ; ex. "<Control>j"
                  ((ichar-upper-case? ch)
                    ;; key-predicate用にshift-key-maskをset。
                    ;; downcaseするとruleと一致しなくなるのでそのまま。
                    (cons ch 1))
                  (else (cons ch 0))))))
          (key? (lambda (k) (or (integer? k) (key-symbol? k))))
          (commit-pending-rk
            (lambda (c)
              (let ((rkc (tutcode-context-rk-context c)))
                (if (pair? (rk-context-seq rkc))
                  (tutcode-commit c (rk-pending rkc) #f #t)))))
          (head-save (tutcode-context-head pc))
          ;; 対話的な操作時のみ意味のあるヘルプ表示等は一時的にオフにする
          ;; (補完/予測入力はひょっとして使うかもしれないのでそのまま)
          (use-candwin-save tutcode-use-candidate-window?)
          (use-stroke-help-win-save tutcode-use-stroke-help-window?)
          (use-auto-help-win-save tutcode-use-auto-help-window?)
          (use-kanji-combination-guide-save tutcode-use-kanji-combination-guide?)
          (stroke-help-with-guide-save
            tutcode-stroke-help-with-kanji-combination-guide)
          ;; child contextを作ってそこにkey-pressを食わせる
          (cpc (tutcode-setup-child-context pc 'tutcode-child-type-seq2kanji)))
      (tutcode-context-set-head! pc ()) ; 子contextでのcommitをheadにためる
      (set! tutcode-use-candidate-window? #f)
      (set! tutcode-use-stroke-help-window? #f)
      (set! tutcode-use-auto-help-window? #f)
      (set! tutcode-use-kanji-combination-guide? #f)
      (set! tutcode-stroke-help-with-kanji-combination-guide 'disable)
      (for-each
        (lambda (s)
          (let ((k-s (string->key-and-status s)))
            (if (key? (car k-s))
              (tutcode-key-press-handler-internal cpc (car k-s) (cdr k-s))
              (begin ; 漢字はそのまま
                (commit-pending-rk cpc)
                (tutcode-flush cpc)
                (tutcode-commit cpc s)))))
        (reverse sequence))
      (commit-pending-rk cpc) ; 最上位のpendingのみ確定。消えるとうれしくない
      ;; XXX:現状は確定済文字列のみ取得。未確定文字列は消える
      (let ((kanji-list (tutcode-context-head pc)))
        (tutcode-flush cpc)
        (tutcode-context-set-child-context! pc ())
        (tutcode-context-set-child-type! pc ())
        (tutcode-context-set-head! pc head-save)
        (set! tutcode-use-candidate-window? use-candwin-save)
        (set! tutcode-use-stroke-help-window? use-stroke-help-win-save)
        (set! tutcode-use-auto-help-window? use-auto-help-win-save)
        (set! tutcode-use-kanji-combination-guide?
          use-kanji-combination-guide-save)
        (set! tutcode-stroke-help-with-kanji-combination-guide
          stroke-help-with-guide-save)
        kanji-list))))

;;; 子コンテキストでのcommit
;;; @param str commitされた文字列
(define (tutcode-seq2kanji-commit-from-child pc str)
  (tutcode-context-set-head! pc
    (append (string-to-list str) (tutcode-context-head pc))))

;;; 子コンテキストでのcommit-raw
(define (tutcode-seq2kanji-commit-raw-from-child pc key key-state)
  (let ((raw-str
          (im-get-raw-key-str
            (cond
              ;; tutcode-sequence->kanji-listで変換したsymbolからcharcodeに戻す
              ((eq? key 'backspace) 8)
              ((eq? key 'tab) 9)
              ((eq? key 'return) 10)
              ((eq? key 'escape) 27)
              ((eq? key 'delete) 127)
              ((control-key-mask key-state)
                (- (ichar-upcase key) 64))
              ((shift-key-mask key-state)
                (ichar-upcase key))
              (else key))
            0)))
    (if raw-str
      (tutcode-seq2kanji-commit-from-child pc raw-str))))

;;; 後置型の入力シーケンス→漢字変換を開始する
;;; @param yomi-len 指定された読みの文字数。指定されてない場合は#f。
(define (tutcode-begin-postfix-seq2kanji-conversion pc yomi-len)
  (let*
    ((former-all (tutcode-postfix-acquire-text pc
                  (or yomi-len tutcode-mazegaki-yomi-max)))
     (former-seq
      (if yomi-len
        former-all
        ;; 行頭の場合、交ぜ書き変換の確定後の可能性があるので、改行を含める
        (receive
          (newlines rest)
          (span
            (lambda (x)
              (string=? x "\n"))
            former-all)
          (append newlines
            (take-while
              (lambda (elem)
                (and (tutcode-ascii? elem)
                     (not (string=? elem "\n"))))
              rest))))))
    (if (pair? former-seq)
      (let ((kanji-list (tutcode-sequence->kanji-list pc former-seq)))
        (if yomi-len
          (begin
            (tutcode-postfix-commit pc
              (string-list-concat kanji-list) former-seq)
            (tutcode-flush pc))
          ;; 読みの文字数が指定されていない
          (begin
            (tutcode-context-set-mazegaki-yomi-all! pc former-all)
            (tutcode-context-set-postfix-yomi-len! pc (length former-seq))
            (tutcode-context-set-head! pc kanji-list)
            (tutcode-context-set-state! pc
              'tutcode-state-postfix-seq2kanji)))))))

;;; 後置型の入力シーケンス→漢字変換モード時のキー入力を処理する。
;;; @param key 入力されたキー
;;; @param key-state コントロールキー等の状態
(define (tutcode-proc-state-postfix-seq2kanji c key key-state)
  (let*
    ((pc (tutcode-find-descendant-context c))
     (yomi-len (tutcode-context-postfix-yomi-len pc))
     (yomi-all (tutcode-context-mazegaki-yomi-all pc))
     (update-context!
      (lambda (new-yomi-len)
        (tutcode-context-set-postfix-yomi-len! pc new-yomi-len)
        (tutcode-context-set-head! pc
          (tutcode-sequence->kanji-list pc (take yomi-all new-yomi-len)))))
     (commit
      (lambda ()
        (tutcode-postfix-commit pc
          (string-list-concat (tutcode-context-head pc))
          (take yomi-all yomi-len))
        (tutcode-flush pc))))
    (cond
      ((tutcode-cancel-key? key key-state)
        (tutcode-flush pc))
      ((or (tutcode-commit-key? key key-state)
           (tutcode-return-key? key key-state))
        (commit))
      ((tutcode-mazegaki-relimit-right-key? key key-state)
        (if (> yomi-len 1)
          ;; 前置型交ぜ書きで確定されていない文字がある場合など、
          ;; relimit-rightすると変換後文字列が伸びる場合あり。
          ;; 例:"aljrk"→"" > "ljrk"→"設あ"
          (update-context! (- yomi-len 1))))
      ((tutcode-mazegaki-relimit-left-key? key key-state)
        (if (> (length yomi-all) yomi-len)
          (update-context! (+ yomi-len 1))))
      (else
        (commit)
        (tutcode-proc-state-on pc key key-state)))))

;;; 直接入力状態のときのキー入力を処理する。
;;; @param c コンテキストリスト
;;; @param key 入力されたキー
;;; @param key-state コントロールキー等の状態
(define (tutcode-proc-state-off c key key-state)
  (let ((pc (tutcode-find-descendant-context c)))
    (cond
      ((tutcode-on-key? key key-state)
        (tutcode-context-set-state! pc 'tutcode-state-on))
      ((tutcode-off-key? key key-state) ; on-keyとoff-keyが別キーの場合
        ) ; 現在onでもoffでも、off-keyでoffにしたいだけなのでcommit-rawを回避
      (else
        (tutcode-commit-raw pc key key-state)))))

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
            (tutcode-heading-label-char-for-kigou-mode? key)
            (tutcode-commit-by-label-key-for-kigou-mode pc
              (charcode->string key)))
        (if (eq? (tutcode-context-candidate-window pc)
                 'tutcode-candidate-window-kigou)
          (tutcode-select-candidate pc (tutcode-context-nth pc))))
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
      (else
        (tutcode-commit-raw pc key key-state)))))

;;; ヒストリ入力モード時のキー入力を処理する。
;;; @param key 入力されたキー
;;; @param key-state コントロールキー等の状態
(define (tutcode-proc-state-history c key key-state)
  (let ((pc (tutcode-find-descendant-context c)))
    (cond
      ((tutcode-next-candidate-key? key key-state)
        (tutcode-change-candidate-index pc 1))
      ((tutcode-prev-candidate-key? key key-state)
        (tutcode-change-candidate-index pc -1))
      ((tutcode-next-page-key? key key-state)
        (tutcode-change-candidate-index pc
          tutcode-nr-candidate-max-for-history))
      ((tutcode-prev-page-key? key key-state)
        (tutcode-change-candidate-index pc
          (- tutcode-nr-candidate-max-for-history)))
      ((tutcode-cancel-key? key key-state)
        (tutcode-flush pc))
      ((and (not (and (modifier-key-mask key-state)
                      (not (shift-key-mask key-state))))
            (tutcode-heading-label-char-for-history? key)
            (tutcode-commit-by-label-key-for-history pc
              (charcode->string key))))
      ((or (tutcode-commit-key? key key-state)
           (tutcode-return-key? key key-state))
        (let ((str (tutcode-prepare-commit-string-for-history pc)))
          (tutcode-commit pc str)
          (tutcode-flush pc)
          (tutcode-check-auto-help-window-begin pc (string-to-list str) ())))
      (else
        (tutcode-commit pc (tutcode-prepare-commit-string-for-history pc))
        (tutcode-flush pc)
        (tutcode-proc-state-on pc key key-state)))))

;;; 文字列リストをカタカナに変換する
;;; @param strlist 文字列のリスト
;;; @param to-katakana? カタカナに変換する場合は#t。ひらがなに変換する場合は#f
;;; @return 変換後の文字列リスト
(define (tutcode-katakana-convert strlist to-katakana?)
  ;;XXX:かなカナ混在時の反転(→カナかな)は未対応
  (let ((idx (if to-katakana? 1 0)))
    (map
      (lambda (e)
        (list-ref (ja-find-kana-list-from-rule ja-rk-rule e) idx))
      strlist)))

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
     (katakana-commit
      (lambda ()
        (let ((str
                (string-list-concat
                  (tutcode-katakana-convert head
                    (not (tutcode-context-katakana-mode? pc))))))
          (tutcode-commit pc str)
          (tutcode-flush pc)
          (tutcode-undo-prepare pc 'tutcode-state-yomi str head))))
     (rk-append-flush
      (lambda ()
        (if tutcode-keep-illegal-sequence?
          (tutcode-context-set-head! pc (append (rk-context-seq rkc) head)))
        (rk-flush rkc)))
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
           (rk-append-flush)
           (tutcode-context-kana-toggle pc))
          ((tutcode-kigou2-toggle-key? key key-state)
           (rk-append-flush)
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
           (tutcode-commit pc (string-list-concat head))
           (tutcode-flush pc))
          ((tutcode-cancel-key? key key-state)
           (tutcode-flush pc))
          ((tutcode-stroke-help-toggle-key? key key-state)
           (tutcode-toggle-stroke-help pc))
          ((and tutcode-use-prediction?
                (tutcode-begin-completion-key? key key-state))
           (rk-append-flush)
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
            (katakana-commit))
          ((tutcode-paste-key? key key-state)
            (let ((latter-seq (tutcode-clipboard-acquire-text-wo-nl pc 'full)))
              (if (pair? latter-seq)
                (tutcode-context-set-head! pc (append latter-seq head)))))
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
          ((and predicting?
                (tutcode-heading-label-char-for-prediction? key)
                (tutcode-commit-by-label-key-for-prediction pc
                  (charcode->string key) 'tutcode-predicting-prediction)))
          ((tutcode-context-latin-conv pc)
           (if (tutcode-begin-conv-key? key key-state) ; spaceキーでの変換開始?
             (if (not (null? head))
               (tutcode-begin-conversion-with-inflection pc #t)
               (tutcode-flush pc))
             (set! res (charcode->string key))))
          ((not (rk-expect-key? rkc (charcode->string key)))
           (if (> (length (rk-context-seq rkc)) 0)
             (begin
               (cond
                 ((tutcode-verbose-stroke-key? key key-state)
                   (tutcode-context-set-head! pc
                     (append (rk-context-seq rkc) head)))
                 (tutcode-keep-illegal-sequence?
                   (tutcode-context-set-head! pc
                     (append (rk-context-seq rkc) head))
                   (set! res (charcode->string key))))
               (rk-flush rkc))
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
           (if (eq? res 'tutcode-postfix-bushu-start)
            (begin
              (set! res
                (and (>= (length head) 2)
                     (tutcode-bushu-convert (cadr head) (car head))))
              (if res
                (begin
                  (tutcode-context-set-head! pc (cddr head))
                  (tutcode-check-auto-help-window-begin pc (list res) ())))))))
        (cond
          ((string? res)
            (tutcode-append-string pc res)
            (if (and tutcode-use-prediction?
                     (> tutcode-prediction-start-char-count 0)
                     ;; 後置型部首合成変換によるauto-help表示済時は何もしない
                     (eq? (tutcode-context-candidate-window pc)
                          'tutcode-candidate-window-off))
              (tutcode-check-prediction pc #f)))
          ((symbol? res)
            (case res
              ((tutcode-auto-help-redisplay)
                (tutcode-auto-help-redisplay pc))
              ;; 活用しない語として変換開始。候補が1つの場合は自動確定
              ((tutcode-postfix-mazegaki-start)
                (if (not (null? head))
                  (tutcode-begin-conversion-with-inflection pc #f)
                  (begin
                    (tutcode-flush pc)
                    (tutcode-begin-postfix-mazegaki-conversion pc #f #f #f))))
              ;; 活用する語として変換開始(postfix用キーシーケンスを流用)
              ((tutcode-postfix-mazegaki-inflection-start)
                (if (not (null? head))
                  (tutcode-begin-mazegaki-inflection-conversion pc)
                  (begin
                    (tutcode-flush pc)
                    (tutcode-begin-postfix-mazegaki-inflection-conversion pc #f))))
              ((tutcode-postfix-katakana-start)
                (if (not (null? head))
                  (katakana-commit)
                  (begin
                    (tutcode-flush pc)
                    (tutcode-begin-postfix-katakana-conversion pc #f))))
              ;; 漢字→入力シーケンス変換。主にclipboardからのpaste時用
              ((tutcode-postfix-kanji2seq-start)
                (if (not (null? head))
                  (let ((str
                          (string-list-concat
                            (tutcode-kanji-list->sequence pc head))))
                    (tutcode-commit pc str)
                    (tutcode-flush pc)
                    (tutcode-undo-prepare pc 'tutcode-state-yomi str head))
                  (begin
                    (tutcode-flush pc)
                    (tutcode-begin-postfix-kanji2seq-conversion pc #f))))))
          ((procedure? res)
            (res 'tutcode-state-yomi pc)))))))

;;; 漢字コード入力状態のときのキー入力を処理する。
;;; @param key 入力されたキー
;;; @param key-state コントロールキー等の状態
(define (tutcode-proc-state-code c key key-state)
  (let*
    ((pc (tutcode-find-descendant-context c))
     (head (tutcode-context-head pc))
     (res #f))
    (cond
      ((and tutcode-use-with-vi?
            (tutcode-vi-escape-key? key key-state))
        (tutcode-flush pc)
        (tutcode-context-set-state! pc 'tutcode-state-off)
        (tutcode-commit-raw pc key key-state)) ; ESCキーをアプリにも渡す
      ((tutcode-off-key? key key-state)
        (tutcode-flush pc)
        (tutcode-context-set-state! pc 'tutcode-state-off))
      ((tutcode-kigou-toggle-key? key key-state)
        (tutcode-flush pc)
        (tutcode-begin-kigou-mode pc))
      ((tutcode-kigou2-toggle-key? key key-state)
        (tutcode-flush pc)
        (if (not (tutcode-kigou2-mode? pc))
          (tutcode-toggle-kigou2-mode pc)))
      ((tutcode-backspace-key? key key-state)
        (if (pair? head)
          (tutcode-context-set-head! pc (cdr head))))
      ((tutcode-cancel-key? key key-state)
        (tutcode-flush pc))
      ((or (tutcode-commit-key? key key-state)
           (tutcode-return-key? key key-state))
        (tutcode-commit pc (string-list-concat head))
        (tutcode-flush pc))
      ((tutcode-paste-key? key key-state)
        (let ((latter-seq (tutcode-clipboard-acquire-text-wo-nl pc 'full)))
          (if (pair? latter-seq)
            (tutcode-context-set-head! pc (append latter-seq head)))))
      ((symbol? key)
        (tutcode-flush pc)
        (tutcode-proc-state-on pc key key-state))
      ((and (modifier-key-mask key-state)
            (not (shift-key-mask key-state)))
        (if (tutcode-begin-conv-key? key key-state) ; <Control>n等での変換開始?
          (if (pair? head)
            (tutcode-begin-kanji-code-input pc head)
            (tutcode-flush pc))
          (begin
            (tutcode-flush pc)
            (tutcode-proc-state-on pc key key-state))))
      ((tutcode-begin-conv-key? key key-state) ; spaceキーでの変換開始?
        (if (pair? head)
          (tutcode-begin-kanji-code-input pc head)
          (tutcode-flush pc)))
      (else
        (tutcode-append-string pc (charcode->string key))))))

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
      (eq? (tutcode-context-predicting pc) 'tutcode-predicting-bushu))
     (re-predict
      (lambda ()
        (if tutcode-use-bushu-prediction?
          (let ((prevchar (car (tutcode-context-head pc))))
            (if (not (string=? prevchar "▲"))
              (tutcode-check-bushu-prediction pc prevchar)))))))
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
            (set! res #f))
          (if (not res)
            (re-predict))))
      ((tutcode-cancel-key? key key-state)
        ;; 再帰的部首合成変換を(キャンセルして)一段戻す
        (set! res (car (tutcode-context-head pc)))
        (tutcode-context-set-head! pc (cdr (tutcode-context-head pc)))
        (if (not (string=? res "▲"))
          ;; もう1文字(▲のはず)を消して、▲を消す
          (tutcode-context-set-head! pc (cdr (tutcode-context-head pc))))
        (set! res #f)
        (if (= (length (tutcode-context-head pc)) 0)
          (tutcode-flush pc)
          (re-predict)))
      ((tutcode-stroke-help-toggle-key? key key-state)
       (tutcode-toggle-stroke-help pc))
      ((and predicting? (tutcode-next-page-key? key key-state))
       (tutcode-change-bushu-prediction-page pc #t))
      ((and predicting? (tutcode-prev-page-key? key key-state))
       (tutcode-change-bushu-prediction-page pc #f))
      ((tutcode-paste-key? key key-state)
        (let ((latter-seq (tutcode-clipboard-acquire-text-wo-nl pc 'full)))
          (if (pair? latter-seq)
            (let* ((head (tutcode-context-head pc))
                   (paste-res
                    (tutcode-bushu-convert-on-list
                      (reverse (append latter-seq head)) ())))
              (if (string? paste-res)
                (begin
                  (tutcode-commit pc paste-res)
                  (tutcode-flush pc)
                  (tutcode-undo-prepare pc 'tutcode-state-bushu paste-res head)
                  (tutcode-check-auto-help-window-begin pc (list paste-res) ()))
                (begin
                  (tutcode-context-set-head! pc paste-res)
                  (if (and tutcode-use-bushu-prediction?
                           (pair? paste-res)
                           (not (string=? (car paste-res) "▲")))
                    (tutcode-check-bushu-prediction pc (car paste-res)))))))))
      ((or
        (symbol? key)
        (and
          (modifier-key-mask key-state)
          (not (shift-key-mask key-state))))
       (tutcode-flush pc)
       (tutcode-proc-state-on pc key key-state))
      ;; 予測入力候補用ラベルキー?
      ((and predicting?
            (tutcode-heading-label-char-for-prediction? key)
            (tutcode-commit-by-label-key-for-prediction pc
              (charcode->string key) 'tutcode-predicting-bushu)))
      ((not (rk-expect-key? rkc (charcode->string key)))
       (if (> (length (rk-context-seq rkc)) 0)
         (begin
           (if (tutcode-verbose-stroke-key? key key-state)
             (set! res (last (rk-context-seq rkc))))
           (rk-flush rkc))
         (set! res (charcode->string key))))
      (else
       (set! res (tutcode-push-key! pc (charcode->string key)))))
    (cond
      ((string? res)
        ;; 再帰的に部首合成される場合があるので、head全体をundo用に保持
        (tutcode-undo-prepare pc 'tutcode-state-bushu " " ; " ":確定後は1文字
          (tutcode-context-head pc))
        (tutcode-begin-bushu-conversion pc res))
      ((symbol? res)
       (case res
        ((tutcode-bushu-start) ; 再帰的な部首合成変換
          (tutcode-append-string pc "▲"))
        ((tutcode-auto-help-redisplay)
          (tutcode-auto-help-redisplay pc))
        ((tutcode-undo) ; 再帰的な部首合成変換をundoする
          (let ((undo (tutcode-context-undo pc)))
            (if (pair? undo)
              (tutcode-context-set-head! pc (list-ref undo 2)))))
        ;;XXX 部首合成変換中は交ぜ書き変換等は無効にする
        ))
      ((procedure? res)
       (res 'tutcode-state-bushu pc)))))

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
          ;; 合成失敗時は入力し直しを待つ。予測入力候補は再表示
          (if tutcode-use-bushu-prediction?
            (if (string? (tutcode-context-prediction-bushu pc)) ; 遅延待ち中?
              (tutcode-check-bushu-prediction pc prevchar)
              ;; 予測入力候補リスト作成済の場合、前回表示したページから再表示
              (tutcode-bushu-prediction-make-page pc
                (tutcode-context-prediction-bushu-page-start pc) #t))))))))

;;; 部首合成変換で変換した文字を確定する
;;; @param convchar 変換後の文字
(define (tutcode-bushu-commit pc convchar)
  ;; 1番目の部首と▲を消す
  (tutcode-context-set-head! pc (cddr (tutcode-context-head pc)))
  (if (null? (tutcode-context-head pc))
    ;; 変換待ちの部首が残ってなければ、確定して終了
    (let ((undo-data (tutcode-context-undo pc))) ; commitするとクリアされる
      (tutcode-commit pc convchar)
      (tutcode-flush pc)
      (tutcode-context-set-undo! pc undo-data)
      (tutcode-check-auto-help-window-begin pc (list convchar) ()))
    ;; 部首がまだ残ってれば、再確認。
    ;; (合成した文字が2文字目ならば、連続して部首合成変換)
    (tutcode-begin-bushu-conversion pc convchar)))

;;; 部首合成変換をリストに対して適用する
;;; @param bushu-list 部首合成シーケンスのリスト。
;;;  例:("▲" "▲" "木" "▲" "人" "人" "条" "夫")
;;; @param conv-list 変換中のリスト(逆順)
;;; @return 合成完了時は変換後文字列。合成途中の場合は変換中リスト(逆順)。
;;;  例:"麩"
(define (tutcode-bushu-convert-on-list bushu-list conv-list)
  (if (null? bushu-list)
    conv-list
    (let ((bushu (car bushu-list))
          (prevchar (safe-car conv-list)))
      (if (or (not prevchar) (string=? prevchar "▲") (string=? bushu "▲"))
        ;; 1文字目 or 再帰開始
        (tutcode-bushu-convert-on-list (cdr bushu-list) (cons bushu conv-list))
        ;; 直前の文字が部首合成マーカでない→2文字目→変換開始
        (let ((convchar (tutcode-bushu-convert prevchar bushu)))
          (if (string? convchar) ; 合成成功?
            (if (or (null? (cdr conv-list)) (null? (cddr conv-list)))
              convchar ; 合成終了(bushu-listの残りは無視)
              (tutcode-bushu-convert-on-list
                (cons convchar (cdr bushu-list))
                (cddr conv-list))) ; 再帰的に合成
            ;; 合成失敗時は次の部首で試す
            (tutcode-bushu-convert-on-list (cdr bushu-list) conv-list)))))))

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
                      (tutcode-get-prediction-string pc
                        (tutcode-context-prediction-index pc))) ; 熟語ガイド無
                    ((> (length head) 0)
                      (string-list-concat (tutcode-context-head pc)))
                    (else
                      #f))))
             (if str (tutcode-commit pc str))
             (tutcode-flush pc)
             (if str (tutcode-check-auto-help-window-begin pc (list str) ()))))
          ((tutcode-cancel-key? key key-state)
           (tutcode-flush pc))
          ((tutcode-stroke-help-toggle-key? key key-state)
           (tutcode-toggle-stroke-help pc))
          ((tutcode-paste-key? key key-state)
            (let ((latter-seq (tutcode-clipboard-acquire-text-wo-nl pc 'full)))
              (if (pair? latter-seq)
                (begin
                  (tutcode-context-set-head! pc (append latter-seq head))
                  (tutcode-begin-interactive-bushu-conversion pc)))))
          ((or
            (symbol? key)
            (and
              (modifier-key-mask key-state)
              (not (shift-key-mask key-state))))
           (tutcode-flush pc)
           (tutcode-proc-state-on pc key key-state))
          ((and (tutcode-heading-label-char-for-prediction? key)
                (= (length (rk-context-seq rkc)) 0)
                (tutcode-commit-by-label-key-for-prediction pc
                  (charcode->string key)
                  'tutcode-predicting-interactive-bushu)))
          ((not (rk-expect-key? rkc (charcode->string key)))
           (if (> (length (rk-context-seq rkc)) 0)
             (begin
               (if (tutcode-verbose-stroke-key? key key-state)
                 (set! res (last (rk-context-seq rkc))))
               (rk-flush rkc))
             (set! res (charcode->string key))))
          (else
           (set! res (tutcode-push-key! pc (charcode->string key)))))
        (cond
          ((string? res)
            (tutcode-append-string pc res)
            (tutcode-begin-interactive-bushu-conversion pc))
          ((symbol? res)
           (case res
            ((tutcode-auto-help-redisplay)
              (tutcode-auto-help-redisplay pc))
            ;;XXX 部首合成変換中は交ぜ書き変換等は無効にする
            ))
          ((procedure? res)
           (res 'tutcode-state-interactive-bushu pc)))))))

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
            (tutcode-activate-candidate-window pc
              'tutcode-candidate-window-interactive-bushu
              tutcode-candidate-window-activate-delay-for-interactive-bushu
              (tutcode-context-prediction-nr-all pc)
              (tutcode-context-prediction-page-limit pc))
            (tutcode-select-candidate pc
              (tutcode-context-prediction-index pc)))
          ;; pasteされた複数の部首を全て使うと合成不能→1部首を削って再検索
          (tutcode-begin-interactive-bushu-conversion pc)))
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
                (tutcode-activate-candidate-window pc
                  'tutcode-candidate-window-interactive-bushu
                  tutcode-candidate-window-activate-delay-for-interactive-bushu
                  nr-all
                  page-limit)
                (tutcode-select-candidate pc 0)))))))))

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
      ((and tutcode-use-recursive-learning?
            (eq? (tutcode-context-state pc) 'tutcode-state-converting)
            (= nth (- nr 1))
            (>= new-nth nr))
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
        (tutcode-select-candidate pc (tutcode-context-nth pc))))))

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
    (tutcode-select-candidate pc compensated-n)))

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
    (tutcode-bushu-prediction-make-page pc n #t)))

;;; 候補ウィンドウを閉じる
(define (tutcode-reset-candidate-window pc)
  (if (not (eq? (tutcode-context-candidate-window pc)
                'tutcode-candidate-window-off))
    (begin
      (im-deactivate-candidate-selector pc)
      (tutcode-context-set-candidate-window! pc 'tutcode-candidate-window-off)
      (tutcode-context-set-predicting! pc 'tutcode-predicting-off)
      (tutcode-context-set-pseudo-table-cands! pc #f)
      (tutcode-context-set-candwin-delay-waiting! pc #f)
      (tutcode-context-set-candwin-delay-selected-index! pc -1))))

;;; 交ぜ書き変換の候補選択状態から、読み入力状態に戻す。
;;; @param pc コンテキストリスト
(define (tutcode-back-to-yomi-state pc)
  (let ((postfix-yomi-len (tutcode-context-postfix-yomi-len pc)))
    (cond
      ((= postfix-yomi-len 0)
        (tutcode-reset-candidate-window pc)
        (tutcode-context-set-state! pc 'tutcode-state-yomi)
        (tutcode-context-set-head! pc (tutcode-context-mazegaki-yomi-all pc))
        (tutcode-context-set-mazegaki-suffix! pc ())
        (tutcode-context-set-nr-candidates! pc 0))
      ((> postfix-yomi-len 0)
        (tutcode-flush pc))
      (else ; selection
        (im-clear-preedit pc)
        (im-update-preedit pc)
        ;; Firefoxやqt4の場合、preedit表示時にselectionが上書きされるようで、
        ;; cancelしても消えたままになるので、取得済のselection内容を書き戻す。
        ;; (selection状態が解除されるためFirefoxやqt4以外(leafpad等)ではうれし
        ;; くないが、消えるデメリットの方がselection解除のデメリットより大きい)
        ;; (再度acquire-textしての判定はFirefoxの場合pairが返るため判定不能)
        (tutcode-commit pc
          (string-list-concat (tutcode-context-mazegaki-yomi-all pc)) #t #t)
        (tutcode-flush pc)))))

;;; 交ぜ書き変換の辞書登録状態から、候補選択状態に戻す。
;;; @param pc コンテキストリスト
(define (tutcode-back-to-converting-state pc)
  (tutcode-context-set-nth! pc (- (tutcode-context-nr-candidates pc) 1))
  (tutcode-check-candidate-window-begin pc)
  (if (eq? (tutcode-context-candidate-window pc)
           'tutcode-candidate-window-converting)
    (tutcode-select-candidate pc (tutcode-context-nth pc)))
  (tutcode-context-set-state! pc 'tutcode-state-converting))

;;; 入力されたキーが候補ラベル文字かどうかを調べる
;;; @param key 入力されたキー
(define (tutcode-heading-label-char? key)
  (member (charcode->string key) tutcode-heading-label-char-list))

;;; 入力されたキーが記号入力モード時の候補ラベル文字かどうかを調べる
;;; @param key 入力されたキー
(define (tutcode-heading-label-char-for-kigou-mode? key)
  (member (charcode->string key) tutcode-heading-label-char-list-for-kigou-mode))

;;; 入力されたキーがヒストリ入力モード時の候補ラベル文字かどうかを調べる
;;; @param key 入力されたキー
(define (tutcode-heading-label-char-for-history? key)
  (member (charcode->string key) tutcode-heading-label-char-list-for-history))

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
      ((and (or (eq? tutcode-commit-candidate-by-label-key 'always)
                (eq? tutcode-commit-candidate-by-label-key 'havecand)
                (and (eq? tutcode-commit-candidate-by-label-key 'candwin)
                     (not (eq? (tutcode-context-candidate-window pc)
                               'tutcode-candidate-window-off))
                     (not (tutcode-context-candwin-delay-waiting pc))))
            (not (and (modifier-key-mask key-state)
                      (not (shift-key-mask key-state))))
            (> (tutcode-context-nr-candidates pc) 1)
            (tutcode-heading-label-char? key)
            (tutcode-commit-by-label-key pc (charcode->string key))))
      (else
        (let* ((postfix-yomi-len (tutcode-context-postfix-yomi-len pc))
               (yomi (and (not (zero? postfix-yomi-len))
                          (take (tutcode-context-mazegaki-yomi-all pc)
                                (abs postfix-yomi-len))))
               (commit-str (tutcode-prepare-commit-string pc)))
          (cond
            ((= postfix-yomi-len 0)
              (tutcode-commit pc commit-str))
            ((> postfix-yomi-len 0)
              (tutcode-postfix-commit pc commit-str yomi))
            (else
              (tutcode-selection-commit pc commit-str yomi))))
        (tutcode-proc-state-on pc key key-state)))))

;;; 部首合成変換を行う。
;;; @param c1 1番目の部首
;;; @param c2 2番目の部首
;;; @return 合成後の文字。合成できなかったときは#f
(define (tutcode-bushu-convert c1 c2)
  (case tutcode-bushu-conversion-algorithm
    ((tc-2.3.1-22.6)
      (tutcode-bushu-convert-tc23 c1 c2))
    ((kw-yamanobe)
      (tutcode-bushu-convert-kwyamanobe c1 c2))
    (else ; 'tc-2.1+ml1925
      (tutcode-bushu-convert-tc21 c1 c2))))

;;; 部首合成変換を行う。
;;; tc-2.1+[tcode-ml:1925]の部首合成アルゴリズムを使用。
;;; @param c1 1番目の部首
;;; @param c2 2番目の部首
;;; @return 合成後の文字。合成できなかったときは#f
(define (tutcode-bushu-convert-tc21 c1 c2)
  (if (null? tutcode-bushu-help)
    (set! tutcode-bushu-help (tutcode-bushu-help-load)))
  (and c1 c2
    (or
      (and tutcode-bushu-help (tutcode-bushu-compose c1 c2 tutcode-bushu-help))
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

;;; 部首合成変換を行う。
;;; 漢直Win+YAMANOBE部首合成アルゴリズムを使用。
;;; @param ca 1番目の部首
;;; @param cb 2番目の部首
;;; @return 合成後の文字。合成できなかったときは#f
(define (tutcode-bushu-convert-kwyamanobe ca cb)
  (if (null? tutcode-bushu-help)
    (set! tutcode-bushu-help (tutcode-bushu-help-load)))
  (and ca cb
    (or
      (and tutcode-bushu-help (tutcode-bushu-compose ca cb tutcode-bushu-help))
      (let
        ;; 合成後の文字が、合成前の2つの部首とは異なる
        ;; 新しい文字であることを確認する。
        ;; (string=?だと#fがあったときにエラーになるのでequal?を使用)
        ((newchar
          (lambda (new)
            (and new
              (not (equal? new ca))
              (not (equal? new cb))
              new)))
         (bushu-compose-sub
          (lambda (x y)
            (and x y
              (tutcode-bushu-compose x y tutcode-bushudic))))) ; no swap
        (or
          (newchar (bushu-compose-sub ca cb))
          (let
            ((a (tutcode-bushu-alternative ca))
             (b (tutcode-bushu-alternative cb))
             (compose-alt
              (lambda (cx cy x y)
                (and
                  (or
                    (not (string=? x cx))
                    (not (string=? y cy)))
                  (newchar (bushu-compose-sub x y))))))
            (or
              (compose-alt ca cb a b)
              (let*
                ((ad (tutcode-bushu-decompose a))
                 (bd (tutcode-bushu-decompose b))
                 (a1 (and ad (car ad)))
                 (a2 (and ad (cadr ad)))
                 (b1 (and bd (car bd)))
                 (b2 (and bd (cadr bd)))
                 (compose-newchar
                  (lambda (i1 i2)
                    (newchar (bushu-compose-sub i1 i2))))
                 (compose-l2r
                  (lambda (x y z)
                    (newchar (bushu-compose-sub (bushu-compose-sub x y) z))))
                 (compose-r2l
                  (lambda (x y z)
                    (newchar (bushu-compose-sub x (bushu-compose-sub y z)))))
                 (compose-lr
                  (lambda (a a1 a2 b b1 b2)
                    (or
                      (and a1 a2
                        (or
                          (compose-l2r a1 b a2)
                          (compose-r2l a1 a2 b)))
                      (and b1 b2
                        (or
                          (compose-l2r a b1 b2)
                          (compose-r2l b1 a b2))))))
                 (subtract
                  (lambda (a1 a2 b)
                    (or
                      (and (equal? a2 b) (newchar a1))
                      (and (equal? a1 b) (newchar a2))))))
                (or
                  (compose-lr a a1 a2 b b1 b2)
                  ;; 引き算
                  (subtract a1 a2 b)
                  (let*
                    ((ad1 (and a1 (tutcode-bushu-decompose a1)))
                     (ad2 (and a2 (tutcode-bushu-decompose a2)))
                     (a11 (and ad1 (car ad1)))
                     (a12 (and ad1 (cadr ad1)))
                     (a21 (and ad2 (car ad2)))
                     (a22 (and ad2 (cadr ad2)))
                     (bushu-convert-sub
                      (lambda (a a1 a11 a12 a2 a21 a22 b b1 b2)
                        (or
                          (and a2 a11 a12
                            (or
                              (and (equal? a12 b) (compose-newchar a11 a2))
                              (and (equal? a11 b) (compose-newchar a12 a2))))
                          (and a1 a21 a22
                            (or
                              (and (equal? a22 b) (compose-newchar a1 a21))
                              (and (equal? a21 b) (compose-newchar a1 a22))))
                          ;; 一方が部品による足し算
                          (compose-newchar a b1)
                          (compose-newchar a b2)
                          (compose-newchar a1 b)
                          (compose-newchar a2 b)
                          (and a1 a2 b1
                            (or
                              (compose-l2r a1 b1 a2)
                              (compose-r2l a1 a2 b1)))
                          (and a1 a2 b2
                            (or
                              (compose-l2r a1 b2 a2)
                              (compose-r2l a1 a2 b2)))
                          (and a1 b1 b2
                            (or
                              (compose-l2r a1 b1 b2)
                              (compose-r2l b1 a1 b2)))
                          (and a2 b1 b2
                            (or
                              (compose-l2r a2 b1 b2)
                              (compose-r2l b1 a2 b2)))
                          ;; 両方が部品による足し算
                          (compose-newchar a1 b1)
                          (compose-newchar a1 b2)
                          (compose-newchar a2 b1)
                          (compose-newchar a2 b2)
                          ;; 部品による引き算
                          (and a2 b1 (equal? a2 b1) (newchar a1))
                          (and a2 b2 (equal? a2 b2) (newchar a1))
                          (and a1 b1 (equal? a1 b1) (newchar a2))
                          (and a1 b2 (equal? a1 b2) (newchar a2))
                          (and a2 a11 a12
                            (or
                              (and (or (equal? a12 b1) (equal? a12 b2))
                                (compose-newchar a11 a2))
                              (and (or (equal? a11 b1) (equal? a11 b2))
                                (compose-newchar a12 a2))))
                          (and a1 a21 a22
                            (or
                              (and (or (equal? a22 b1) (equal? a22 b2))
                                (compose-newchar a1 a21))
                              (and (or (equal? a21 b1) (equal? a21 b2))
                                (compose-newchar a1 a22))))))))
                    (or
                      (bushu-convert-sub a a1 a11 a12 a2 a21 a22 b b1 b2)
                      ;; 文字の順序を逆にしてみる
                      (and (not (equal? ca cb))
                        (or
                          (newchar (bushu-compose-sub cb ca))
                          (compose-alt cb ca b a)
                          (compose-lr b b1 b2 a a1 a2)
                          (subtract b1 b2 a)
                          (let*
                            ((bd1 (and b1 (tutcode-bushu-decompose b1)))
                             (bd2 (and b2 (tutcode-bushu-decompose b2)))
                             (b11 (and bd1 (car bd1)))
                             (b12 (and bd1 (cadr bd1)))
                             (b21 (and bd2 (car bd2)))
                             (b22 (and bd2 (cadr bd2))))
                            (bushu-convert-sub b b1 b11 b12 b2 b21 b22 a a1 a2)
                            ))))))))))))))

;;; 部首合成変換:c1とc2を合成してできる文字を探して返す。
;;; 指定された順番で見つからなかった場合は、順番を入れかえて探す。
;;; @param c1 1番目の部首
;;; @param c2 2番目の部首
;;; @return 合成後の文字。合成できなかったときは#f
(define (tutcode-bushu-compose-sub c1 c2)
  (and c1 c2
    (or
      (tutcode-bushu-compose c1 c2 tutcode-bushudic)
      (tutcode-bushu-compose c2 c1 tutcode-bushudic))))

;;; 部首合成変換:c1とc2を合成してできる文字を探して返す。
;;; @param c1 1番目の部首
;;; @param c2 2番目の部首
;;; @return 合成後の文字。合成できなかったときは#f
(define (tutcode-bushu-compose c1 c2 bushudic)
  (let ((seq (rk-lib-find-seq (list c1 c2) bushudic)))
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
  (if (null? tutcode-reverse-bushudic-hash-table)
    (set! tutcode-reverse-bushudic-hash-table
      (tutcode-rule->reverse-hash-table tutcode-bushudic)))
  (let ((i (tutcode-euc-jp-string->ichar c)))
    (and i
      (hash-table-ref/default tutcode-reverse-bushudic-hash-table i #f))))

;;; tutcode-rule形式のリストから、逆引き検索(漢字から打鍵リストを取得)用の
;;; hash-tableを作る
;;; @param rule tutcode-rule形式のリスト
;;; @return hash-table
(define (tutcode-rule->reverse-hash-table rule)
  (alist->hash-table
    (filter-map
      (lambda (elem)
        (and-let*
          ((kanji (caadr elem))
           (kanji-string? (string? kanji)) ; 'tutcode-mazegaki-start等は除く
           (i (tutcode-euc-jp-string->ichar kanji)))
          (cons i (caar elem))))
      rule)))

;;; hash-tableのキー用に、漢字1文字の文字列から漢字コードに変換する
;;; @param s 文字列
;;; @return 漢字コード。文字列の長さが1でない場合は#f
(define (tutcode-euc-jp-string->ichar s)
  ;; ichar.scmのstring->ichar(string->charcode)のEUC-JP版
  (let ((sl (with-char-codec "EUC-JP"
              (lambda ()
                (string->list s)))))
    (cond
      ((null? sl)
        0)
      ((null? (cdr sl))
        (char->integer (car sl)))
      (else
        #f))))

;;; 自動ヘルプ:bushu.helpファイルを検索して対象文字のヘルプ(2つの部首)を取得する
;;; @param c 対象文字
;;; @return ヘルプに表示する情報(2つの部首のリスト)。見つからなかった場合は#f
(define (tutcode-bushu-help-lookup c)
  (and (not (string=? tutcode-bushu-help-filename "")) ; デフォルトは""
    (let*
      ((looked (tutcode-bushu-search c tutcode-bushu-help-filename))
       (lst (and looked (tutcode-bushu-parse-entry looked))))
      (and lst
        (>= (length lst) 2)
        (take lst 2)))))

;;; 自動ヘルプ:対象文字を部首合成するのに必要となる、
;;; 外字でない2つの文字のリストを返す
;;; 例: "繋" => (((("," "o"))("撃")) ((("f" "q"))("糸")))
;;; @param c 対象文字
;;; @param rule tutcode-rule
;;; @param stime 開始日時
;;; @return 対象文字の部首合成に必要な2つの文字とストロークのリスト。
;;;  見つからなかった場合は#f
(define (tutcode-auto-help-bushu-decompose c rule stime)
  (case tutcode-bushu-conversion-algorithm
    ((tc-2.3.1-22.6)
      (tutcode-auto-help-bushu-decompose-tc23 c rule stime))
    (else ; 'tc-2.1+ml1925
      (tutcode-auto-help-bushu-decompose-tc21 c rule stime))))

;;; 自動ヘルプ:対象文字を部首合成するのに必要となる、
;;; 外字でない2つの文字のリストを返す
;;; 例: "繋" => (((("," "o"))("撃")) ((("f" "q"))("糸")))
;;; @param c 対象文字
;;; @param rule tutcode-rule
;;; @param stime 開始日時
;;; @return 対象文字の部首合成に必要な2つの文字とストロークのリスト。
;;;  見つからなかった場合は#f
(define (tutcode-auto-help-bushu-decompose-tc21 c rule stime)
  (if (> (string->number (difftime (time) stime)) tutcode-auto-help-time-limit)
    #f
    (let*
      ((bushu (or (tutcode-bushu-help-lookup c)
                  (tutcode-bushu-decompose c)))
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
            (or
              (tutcode-auto-help-bushu-decompose-looking-bushudic
                tutcode-bushudic () 99
                (lambda (elem)
                  (tutcode-auto-help-get-stroke-list-with-right-part
                    c b1 b2 seq1 rule elem)))
              ;; 部首2では合成不能→部首2をさらに分解
              (let
                ((b2dec
                  (tutcode-auto-help-bushu-decompose-tc21 b2 rule stime)))
                (and b2dec
                  (list seq1 b2dec)))))
          ;; 部首2が直接入力可能
          ;; →(部首2)と(部首1を部品として持つ漢字)による合成が可能か?
          (and seq2 b1
            (or
              (tutcode-auto-help-bushu-decompose-looking-bushudic
                tutcode-bushudic () 99
                (lambda (elem)
                  (tutcode-auto-help-get-stroke-list-with-left-part
                    c b1 b2 seq2 rule elem)))
              ;; 部首1では合成不能→部首1をさらに分解
              (let
                ((b1dec
                  (tutcode-auto-help-bushu-decompose-tc21 b1 rule stime)))
                (and b1dec
                  (list b1dec seq2)))))
          ;; 部首1も部首2も直接入力不可→さらに分解
          (and b1 b2
            (let
              ((b1dec (tutcode-auto-help-bushu-decompose-tc21 b1 rule stime))
               (b2dec (tutcode-auto-help-bushu-decompose-tc21 b2 rule stime)))
              (and b1dec b2dec
                (list b1dec b2dec))))
          ;; XXX: 部品どうしの合成は未対応
          )))))

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
  (if (null? tutcode-bushu-help)
    (set! tutcode-bushu-help (tutcode-bushu-help-load)))
  (let*
    ((rules-help
      (if tutcode-bushu-help
        (rk-lib-find-partial-seqs (list str) tutcode-bushu-help)
        ()))
     (rules-dic (rk-lib-find-partial-seqs (list str) bushudic))
     (rules (append rules-help rules-dic)) ; 重複回避はbushu.help側で可能
     (words1 (map (lambda (elem) (cadaar elem)) rules))
     ;; (((str 部首2))(合成文字)) -> (部首2 合成文字)
     (word/cand1 (map (lambda (elem) (list (cadaar elem) (caadr elem))) rules))
     (more-cands
      (filter-map
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
                   (not (member bushu1 words1))
                   (list bushu1 gosei))
              (and (string=? str gosei) ; (((部首1 部首2))(str))
                   ;; XXX:この場合、strとbushu1でbushu2が合成できることを
                   ;;     確認すべきだが、tutcode-bushu-convertは遅いので省略
                   (list bushu1 bushu2)))))
          bushudic)))
    (append word/cand1 more-cands)))

;;; tutcode-ruleを逆引きして、変換後の文字から、入力キー列を取得する。
;;; 例: (tutcode-reverse-find-seq "あ" tutcode-rule) => ("r" "k")
;;; @param c 変換後の文字
;;; @param rule tutcode-rule
;;; @return 入力キーのリスト。tutcode-rule中にcが見つからなかった場合は#f
(define (tutcode-reverse-find-seq c rule)
  (and (string? c)
    (let*
      ((hash-table
        (if (eq? rule tutcode-kigou-rule)
          (begin
            (if (null? tutcode-reverse-kigou-rule-hash-table)
              (set! tutcode-reverse-kigou-rule-hash-table
                (tutcode-rule->reverse-hash-table rule)))
            tutcode-reverse-kigou-rule-hash-table)
          (begin
            (if (null? tutcode-reverse-rule-hash-table)
              (set! tutcode-reverse-rule-hash-table
                (tutcode-rule->reverse-hash-table rule)))
            tutcode-reverse-rule-hash-table)))
       (i (tutcode-euc-jp-string->ichar c)))
      (and i
        (hash-table-ref/default hash-table i #f)))))

;;; 現在のstateがpreeditを持つかどうかを返す。
;;; @param pc コンテキストリスト
(define (tutcode-state-has-preedit? pc)
  (or
    (not (null? (tutcode-context-child-context pc)))
    (memq (tutcode-context-state pc)
      '(tutcode-state-yomi tutcode-state-bushu tutcode-state-converting
        tutcode-state-interactive-bushu tutcode-state-kigou
        tutcode-state-code tutcode-state-history
        tutcode-state-postfix-katakana tutcode-state-postfix-kanji2seq
        tutcode-state-postfix-seq2kanji))))

;;; キーが押されたときの処理の振り分けを行う。
;;; @param c コンテキストリスト
;;; @param key 入力されたキー
;;; @param key-state コントロールキー等の状態
(define (tutcode-key-press-handler c key key-state)
  (if (ichar-control? key)
      (im-commit-raw c)
      (tutcode-key-press-handler-internal c key key-state)))

;;; キーが押されたときの処理の振り分けを行う。
;;; (seq2kanjiからの呼出用。ichar-control?文字がseq2kanjiを通しても残るように)
(define (tutcode-key-press-handler-internal c key key-state)
  (let ((pc (tutcode-find-descendant-context c)))
    (case (tutcode-context-state pc)
      ((tutcode-state-on)
       (let* ((rkc (tutcode-context-rk-context pc))
              (prev-pending-rk (rk-context-seq rkc)))
         (tutcode-proc-state-on pc key key-state)
         (if (or (and tutcode-show-pending-rk?
                      (or (pair? (rk-context-seq rkc))
                          (pair? prev-pending-rk))) ; prev-pending-rk消去用
               ;; 交ぜ書き変換や部首合成変換開始。△や▲を表示する
               (tutcode-state-has-preedit? c)
               ;; 文字数指定後置型交ぜ書き変換の再帰学習キャンセル
               (not (eq? (tutcode-find-descendant-context c) pc)))
           (tutcode-update-preedit pc))))
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
      ((tutcode-state-code)
       (tutcode-proc-state-code pc key key-state)
       (tutcode-update-preedit pc))
      ((tutcode-state-history)
       (tutcode-proc-state-history pc key key-state)
       (tutcode-update-preedit pc))
      ((tutcode-state-postfix-katakana)
       (tutcode-proc-state-postfix-katakana pc key key-state)
       (tutcode-update-preedit pc))
      ((tutcode-state-postfix-kanji2seq)
       (tutcode-proc-state-postfix-kanji2seq pc key key-state)
       (tutcode-update-preedit pc))
      ((tutcode-state-postfix-seq2kanji)
       (tutcode-proc-state-postfix-seq2kanji pc key key-state)
       (tutcode-update-preedit pc))
      (else
       (tutcode-proc-state-off pc key key-state)
       (if (or (and tutcode-show-pending-rk?
                    (pair? (rk-context-seq (tutcode-context-rk-context pc))))
               (tutcode-state-has-preedit? c)) ; 再帰学習時
         (tutcode-update-preedit pc))))
    (if (or tutcode-use-stroke-help-window?
            (not (eq? tutcode-stroke-help-with-kanji-combination-guide
                      'disable)))
      ;; editorの作成・削除の可能性があるのでdescendant-context取得し直し
      (let ((newpc (tutcode-find-descendant-context c)))
        (if
          (and
            (memq (tutcode-context-state newpc)
              '(tutcode-state-on tutcode-state-yomi tutcode-state-bushu
                tutcode-state-interactive-bushu))
            (not (tutcode-context-latin-conv newpc)))
          (tutcode-check-stroke-help-window-begin newpc))))))

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
  (if (not tutcode-show-pending-rk?)
    (let* ((tc (tutcode-find-descendant-context c))
           (rkc (tutcode-context-rk-context tc)))
      (rk-flush rkc))))

(define tutcode-place-handler tutcode-focus-in-handler)
(define tutcode-displace-handler tutcode-focus-out-handler)

;;; 候補ウィンドウが候補文字列を取得するために呼ぶ関数
(define (tutcode-get-candidate-handler c idx accel-enum-hint)
  (let ((tc (tutcode-find-descendant-context c)))
    (if tutcode-use-pseudo-table-style?
      (let* ((vec (tutcode-context-pseudo-table-cands tc))
             (dl (length (vector-ref vec 0)))
             (page (quotient idx dl))
             (pcands (vector-ref vec page))
             (cands
              (or pcands
                  (let ((cands (tutcode-pseudo-table-style-make-new-page tc)))
                    (vector-set! vec page cands)
                    cands))))
        (list-ref cands (remainder idx dl)))
      (tutcode-get-candidate-handler-internal tc idx accel-enum-hint))))

;;; 候補ウィンドウが候補文字列を取得するために呼ぶ関数
;;; (tutcode-pseudo-table-style-setupからの呼び出し用)
(define (tutcode-get-candidate-handler-internal tc idx accel-enum-hint)
  (cond
    ;; 記号入力
    ((eq? (tutcode-context-state tc) 'tutcode-state-kigou)
      (let* ((cand (tutcode-get-nth-candidate-for-kigou-mode tc idx))
             (n (remainder idx
                  (length tutcode-heading-label-char-list-for-kigou-mode)))
             (label (nth n tutcode-heading-label-char-list-for-kigou-mode)))
        ;; XXX:annotation表示は現状無効化されているので、常に""を返しておく
        (list cand label "")))
    ;; ヒストリ入力
    ((eq? (tutcode-context-state tc) 'tutcode-state-history)
      (let* ((cand (tutcode-get-nth-candidate-for-history tc idx))
             (n (remainder idx
                  (length tutcode-heading-label-char-list-for-history)))
             (label (nth n tutcode-heading-label-char-list-for-history)))
        (list cand label "")))
    ;; 補完/予測入力候補
    ((eq? (tutcode-context-candidate-window tc)
          'tutcode-candidate-window-predicting)
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
             (word (and (eq? (tutcode-context-predicting tc)
                             'tutcode-predicting-bushu)
                        (tutcode-lib-get-nth-word tc i)))
             (cand-guide
              (if word
                (string-append cand "(" word ")")
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
                  (string-list-concat
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
    ((eq? (tutcode-context-candidate-window tc)
          'tutcode-candidate-window-converting)
      (let* ((cand (tutcode-get-nth-candidate tc idx))
             (n (remainder idx (length tutcode-heading-label-char-list)))
             (label (nth n tutcode-heading-label-char-list)))
        (list cand label "")))
    (else
      (list "" "" ""))))

;;; 候補ウィンドウが候補を選択したときに呼ぶ関数。
;;; 表示中の候補が選択された場合、該当する候補を確定する。
;;; 表示されていない候補が選択された(候補ウィンドウ側で
;;; ページ移動操作が行われた)場合、内部の選択候補番号を更新するだけ。
(define (tutcode-set-candidate-index-handler c pidx)
  (let* ((pc (tutcode-find-descendant-context c))
         (candwin (tutcode-context-candidate-window pc))
         (idx (if tutcode-use-pseudo-table-style?
                ;; XXX:擬似表形式では、マウスによる候補選択は未対応。
                ;;     candwinページ内のクリック時は、ページ内最初の候補確定
                (tutcode-pseudo-table-style-scm-index pc pidx)
                pidx))
         ;; 仮想鍵盤上のクリックをキー入力として処理(ソフトキーボード)
         (label-to-key-press
          (lambda (label)
            (let ((key (string->ichar label)))
              (if key
                (tutcode-key-press-handler c key 0)))))
         (candlist-to-key-press
          (lambda (candlist)
            (let* ((candlabel (list-ref candlist idx))
                   (label (cadr candlabel)))
              (label-to-key-press label)))))
    (cond
      ((and (memq candwin '(tutcode-candidate-window-converting
                            tutcode-candidate-window-kigou
                            tutcode-candidate-window-history))
          (>= idx 0)
          (< idx (tutcode-context-nr-candidates pc)))
        (let*
          ((prev (tutcode-context-nth pc))
           (state (tutcode-context-state pc))
           (page-limit
            (case state
              ((tutcode-state-kigou) tutcode-nr-candidate-max-for-kigou-mode)
              ((tutcode-state-history) tutcode-nr-candidate-max-for-history)
              (else tutcode-nr-candidate-max)))
           (prev-page (quotient prev page-limit))
           (new-page (quotient idx page-limit)))
          (tutcode-context-set-nth! pc idx)
          (if (= new-page prev-page)
            (case state
              ((tutcode-state-kigou)
                (tutcode-commit pc
                  (tutcode-prepare-commit-string-for-kigou-mode pc)))
              ((tutcode-state-history)
                (let ((str (tutcode-prepare-commit-string-for-history pc)))
                  (tutcode-commit pc str)
                  (tutcode-flush pc)
                  (tutcode-check-auto-help-window-begin pc
                    (string-to-list str) ())))
              (else
                (tutcode-commit-with-auto-help pc))))
          (tutcode-update-preedit pc)))
      ((and (or (eq? candwin 'tutcode-candidate-window-predicting)
                (eq? candwin 'tutcode-candidate-window-interactive-bushu))
            (>= idx 0))
        (let*
          ((nr-in-page (tutcode-context-prediction-nr-in-page pc))
           (page-limit (tutcode-context-prediction-page-limit pc))
           (idx-in-page (remainder idx page-limit))
           (prev (tutcode-context-prediction-index pc))
           (prev-page (quotient prev page-limit))
           (new-page (quotient idx page-limit)))
          (tutcode-context-set-prediction-index! pc idx)
          (if (= new-page prev-page)
            (if (< idx-in-page nr-in-page)
              (let*
                ((nr-predictions (tutcode-lib-get-nr-predictions pc))
                 (p-idx (+ idx-in-page (* new-page nr-in-page)))
                 (i (remainder p-idx nr-predictions))
                 (mode (tutcode-context-predicting pc)))
                (if (eq? candwin 'tutcode-candidate-window-interactive-bushu)
                  (tutcode-do-commit-prediction-for-interactive-bushu pc i)
                  (if (eq? mode 'tutcode-predicting-bushu)
                    (tutcode-do-commit-prediction-for-bushu pc i)
                    (tutcode-do-commit-prediction pc i
                      (eq? mode 'tutcode-predicting-completion)))))
              ;; 熟語ガイド
              (let*
                ((guide (tutcode-context-guide pc))
                 (guide-len (length guide)))
                (if (positive? guide-len)
                  (let*
                    ((guide-idx-in-page (- idx-in-page nr-in-page))
                     (nr-guide-in-page (- page-limit nr-in-page))
                     (guide-idx (+ guide-idx-in-page
                                   (* new-page nr-guide-in-page)))
                     (n (remainder guide-idx guide-len))
                     (label-cands-alist (nth n guide))
                     (label (car label-cands-alist)))
                    (label-to-key-press label))))))
          (tutcode-update-preedit pc)))
        ((eq? candwin 'tutcode-candidate-window-stroke-help)
          (candlist-to-key-press (tutcode-context-stroke-help pc)))
        ((eq? candwin 'tutcode-candidate-window-auto-help)
          (candlist-to-key-press (tutcode-context-auto-help pc))))))

;;; 遅延表示に対応している候補ウィンドウが、待ち時間満了時に
;;; (候補数、ページ内候補表示数、選択されたインデックス番号)を
;;; 取得するために呼ぶ関数
;;; @return (nr display-limit selected-index)
(define (tutcode-delay-activating-handler c)
  (let* ((tc (tutcode-find-descendant-context c))
         (selected-index (tutcode-context-candwin-delay-selected-index tc)))
    (tutcode-context-set-candwin-delay-waiting! tc #f)
    (let
      ((res
        (cond
          ((eq? (tutcode-context-state tc) 'tutcode-state-kigou)
            (list tutcode-nr-candidate-max-for-kigou-mode
                  (tutcode-context-nr-candidates tc)))
          ((eq? (tutcode-context-state tc) 'tutcode-state-history)
            (list tutcode-nr-candidate-max-for-history
                  (tutcode-context-nr-candidates tc)))
          ((eq? (tutcode-context-candidate-window tc)
                'tutcode-candidate-window-predicting)
            (case (tutcode-context-state tc)
              ((tutcode-state-bushu)
                (if (tutcode-check-bushu-prediction-make tc
                      (tutcode-context-prediction-bushu tc) #f) ;候補リスト作成
                  (list (tutcode-context-prediction-page-limit tc)
                        (tutcode-context-prediction-nr-all tc))
                  (list (tutcode-context-prediction-page-limit tc) 0)))
              ((tutcode-state-on)
                (if (tutcode-check-completion-make tc #f 0)
                  (list (tutcode-context-prediction-page-limit tc)
                        (tutcode-context-prediction-nr-all tc))
                  (list (tutcode-context-prediction-page-limit tc) 0)))
              ((tutcode-state-yomi)
                (if (tutcode-check-prediction-make tc #f)
                  (list (tutcode-context-prediction-page-limit tc)
                        (tutcode-context-prediction-nr-all tc))
                  (list (tutcode-context-prediction-page-limit tc) 0)))
              (else
                '(0 0))))
          ((eq? (tutcode-context-candidate-window tc)
                'tutcode-candidate-window-stroke-help)
            (let ((stroke-help (tutcode-stroke-help-make tc)))
              (if (pair? stroke-help)
                (begin
                  (tutcode-context-set-stroke-help! tc stroke-help)
                  (list tutcode-nr-candidate-max-for-kigou-mode
                        (length stroke-help)))
                (list tutcode-nr-candidate-max-for-kigou-mode 0))))
          ((eq? (tutcode-context-candidate-window tc)
                'tutcode-candidate-window-auto-help)
            (let*
              ((tmp (cdr (tutcode-context-auto-help tc)))
               (strlist (car tmp))
               (yomilist (cadr tmp))
               (auto-help (tutcode-auto-help-make tc strlist yomilist)))
              (if (pair? auto-help)
                (begin
                  (tutcode-context-set-auto-help! tc auto-help)
                  (list tutcode-nr-candidate-max-for-kigou-mode
                        (length auto-help)))
                (list tutcode-nr-candidate-max-for-kigou-mode 0))))
          ((eq? (tutcode-context-state tc)
                'tutcode-state-interactive-bushu)
            (list (tutcode-context-prediction-page-limit tc)
                  (tutcode-context-prediction-nr-all tc)))
          ((eq? (tutcode-context-candidate-window tc)
                'tutcode-candidate-window-converting)
            (list tutcode-nr-candidate-max
                  (tutcode-context-nr-candidates tc)))
          (else
            (list tutcode-nr-candidate-max 0)))))
      (reverse
        (if (and tutcode-use-pseudo-table-style?
                 (> (cadr res) 0)) ; nrが0の場合はcandwinは表示されない
          (let ((pres
                  (tutcode-pseudo-table-style-setup tc (cadr res) (car res))))
            ;; candwin-indexはsetup呼出後に呼ぶ必要あり
            (cons (tutcode-pseudo-table-style-candwin-index tc selected-index)
                    (reverse pres)))
          (cons selected-index res))))))

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
          (make-subrule tutcode-katakana-sequence
            (list
              (lambda (state pc) (tutcode-context-set-katakana-mode! pc #t))))
          (make-subrule tutcode-hiragana-sequence
            (list
              (lambda (state pc) (tutcode-context-set-katakana-mode! pc #f))))
          (make-subrule tutcode-mazegaki-start-sequence
            '(tutcode-mazegaki-start))
          (make-subrule tutcode-latin-conv-start-sequence
            '(tutcode-latin-conv-start))
          (make-subrule tutcode-kanji-code-input-start-sequence
            '(tutcode-kanji-code-input-start))
          (make-subrule tutcode-history-start-sequence
            '(tutcode-history-start))
          (make-subrule tutcode-bushu-start-sequence
            '(tutcode-bushu-start))
          (and
            tutcode-use-interactive-bushu-conversion?
            (make-subrule tutcode-interactive-bushu-start-sequence
              '(tutcode-interactive-bushu-start)))
          (make-subrule tutcode-postfix-bushu-start-sequence
            '(tutcode-postfix-bushu-start))
          (make-subrule tutcode-selection-mazegaki-start-sequence
            '(tutcode-selection-mazegaki-start))
          (make-subrule tutcode-selection-mazegaki-inflection-start-sequence
            '(tutcode-selection-mazegaki-inflection-start))
          (make-subrule tutcode-selection-katakana-start-sequence
            '(tutcode-selection-katakana-start))
          (make-subrule tutcode-selection-kanji2seq-start-sequence
            '(tutcode-selection-kanji2seq-start))
          (make-subrule tutcode-selection-seq2kanji-start-sequence
            '(tutcode-selection-seq2kanji-start))
          (make-subrule tutcode-clipboard-seq2kanji-start-sequence
            '(tutcode-clipboard-seq2kanji-start))
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
          (make-subrule tutcode-postfix-katakana-start-sequence
            '(tutcode-postfix-katakana-start))
          (make-subrule tutcode-postfix-katakana-0-start-sequence
            '(tutcode-postfix-katakana-0-start))
          (make-subrule tutcode-postfix-katakana-1-start-sequence
            '(tutcode-postfix-katakana-1-start))
          (make-subrule tutcode-postfix-katakana-2-start-sequence
            '(tutcode-postfix-katakana-2-start))
          (make-subrule tutcode-postfix-katakana-3-start-sequence
            '(tutcode-postfix-katakana-3-start))
          (make-subrule tutcode-postfix-katakana-4-start-sequence
            '(tutcode-postfix-katakana-4-start))
          (make-subrule tutcode-postfix-katakana-5-start-sequence
            '(tutcode-postfix-katakana-5-start))
          (make-subrule tutcode-postfix-katakana-6-start-sequence
            '(tutcode-postfix-katakana-6-start))
          (make-subrule tutcode-postfix-katakana-7-start-sequence
            '(tutcode-postfix-katakana-7-start))
          (make-subrule tutcode-postfix-katakana-8-start-sequence
            '(tutcode-postfix-katakana-8-start))
          (make-subrule tutcode-postfix-katakana-9-start-sequence
            '(tutcode-postfix-katakana-9-start))
          (make-subrule tutcode-postfix-katakana-exclude-1-sequence
            '(tutcode-postfix-katakana-exclude-1))
          (make-subrule tutcode-postfix-katakana-exclude-2-sequence
            '(tutcode-postfix-katakana-exclude-2))
          (make-subrule tutcode-postfix-katakana-exclude-3-sequence
            '(tutcode-postfix-katakana-exclude-3))
          (make-subrule tutcode-postfix-katakana-exclude-4-sequence
            '(tutcode-postfix-katakana-exclude-4))
          (make-subrule tutcode-postfix-katakana-exclude-5-sequence
            '(tutcode-postfix-katakana-exclude-5))
          (make-subrule tutcode-postfix-katakana-exclude-6-sequence
            '(tutcode-postfix-katakana-exclude-6))
          (make-subrule tutcode-postfix-katakana-shrink-1-sequence
            '(tutcode-postfix-katakana-shrink-1))
          (make-subrule tutcode-postfix-katakana-shrink-2-sequence
            '(tutcode-postfix-katakana-shrink-2))
          (make-subrule tutcode-postfix-katakana-shrink-3-sequence
            '(tutcode-postfix-katakana-shrink-3))
          (make-subrule tutcode-postfix-katakana-shrink-4-sequence
            '(tutcode-postfix-katakana-shrink-4))
          (make-subrule tutcode-postfix-katakana-shrink-5-sequence
            '(tutcode-postfix-katakana-shrink-5))
          (make-subrule tutcode-postfix-katakana-shrink-6-sequence
            '(tutcode-postfix-katakana-shrink-6))
          (make-subrule tutcode-postfix-kanji2seq-start-sequence
            '(tutcode-postfix-kanji2seq-start))
          (make-subrule tutcode-postfix-kanji2seq-1-start-sequence
            '(tutcode-postfix-kanji2seq-1-start))
          (make-subrule tutcode-postfix-kanji2seq-2-start-sequence
            '(tutcode-postfix-kanji2seq-2-start))
          (make-subrule tutcode-postfix-kanji2seq-3-start-sequence
            '(tutcode-postfix-kanji2seq-3-start))
          (make-subrule tutcode-postfix-kanji2seq-4-start-sequence
            '(tutcode-postfix-kanji2seq-4-start))
          (make-subrule tutcode-postfix-kanji2seq-5-start-sequence
            '(tutcode-postfix-kanji2seq-5-start))
          (make-subrule tutcode-postfix-kanji2seq-6-start-sequence
            '(tutcode-postfix-kanji2seq-6-start))
          (make-subrule tutcode-postfix-kanji2seq-7-start-sequence
            '(tutcode-postfix-kanji2seq-7-start))
          (make-subrule tutcode-postfix-kanji2seq-8-start-sequence
            '(tutcode-postfix-kanji2seq-8-start))
          (make-subrule tutcode-postfix-kanji2seq-9-start-sequence
            '(tutcode-postfix-kanji2seq-9-start))
          (make-subrule tutcode-postfix-seq2kanji-start-sequence
            '(tutcode-postfix-seq2kanji-start))
          (make-subrule tutcode-postfix-seq2kanji-1-start-sequence
            '(tutcode-postfix-seq2kanji-1-start))
          (make-subrule tutcode-postfix-seq2kanji-2-start-sequence
            '(tutcode-postfix-seq2kanji-2-start))
          (make-subrule tutcode-postfix-seq2kanji-3-start-sequence
            '(tutcode-postfix-seq2kanji-3-start))
          (make-subrule tutcode-postfix-seq2kanji-4-start-sequence
            '(tutcode-postfix-seq2kanji-4-start))
          (make-subrule tutcode-postfix-seq2kanji-5-start-sequence
            '(tutcode-postfix-seq2kanji-5-start))
          (make-subrule tutcode-postfix-seq2kanji-6-start-sequence
            '(tutcode-postfix-seq2kanji-6-start))
          (make-subrule tutcode-postfix-seq2kanji-7-start-sequence
            '(tutcode-postfix-seq2kanji-7-start))
          (make-subrule tutcode-postfix-seq2kanji-8-start-sequence
            '(tutcode-postfix-seq2kanji-8-start))
          (make-subrule tutcode-postfix-seq2kanji-9-start-sequence
            '(tutcode-postfix-seq2kanji-9-start))
          (make-subrule tutcode-auto-help-redisplay-sequence
            '(tutcode-auto-help-redisplay))
          (make-subrule tutcode-auto-help-dump-sequence
            (list tutcode-auto-help-dump))
          (make-subrule tutcode-help-sequence
            '(tutcode-help))
          (make-subrule tutcode-help-clipboard-sequence
            '(tutcode-help-clipboard))
          (make-subrule tutcode-undo-sequence
            '(tutcode-undo)))))))

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

;;; selectionに対して指定された処理を適用した結果に置換する。
;;; ~/.uimでの使用例:
;;; (require "external-filter.scm")
;;; (define (tutcode-filter-fmt-quote state pc)
;;;   (tutcode-selection-filter pc
;;;     (lambda (str)
;;;       ;; 文書整形後、引用マークを行頭に付ける (nkf -e: uim-tutcodeはEUC-JP)
;;;       (external-filter-launch-command "nkf -e -f | sed -e 's/^/> /'" str))))
;;; (require "fmt-ja.scm")
;;; (define (tutcode-filter-fmt-ja state pc)
;;;   (tutcode-selection-filter pc
;;;     (lambda (str)
;;;       (apply string-append (fmt-ja-str str)))))
;;; (require "japan-util.scm")
;;; (define (tutcode-filter-ascii-fullkana state pc)
;;;   (tutcode-selection-filter pc
;;;     (lambda (str)
;;;       (string-list-concat
;;;         (japan-util-ascii-convert
;;;           (japan-util-halfkana-to-fullkana-convert
;;;             (string-to-list str)))))))
;;; (tutcode-rule-set-sequences!
;;;   `(((("a" "v" "q")) (,tutcode-filter-fmt-quote))
;;;     ((("a" "v" "f")) (,tutcode-filter-fmt-ja))
;;;     ((("a" "v" "a")) (,tutcode-filter-ascii-fullkana))))
;;; @param fn フィルタ処理関数。文字列を入力に取り、結果を文字列で返す。
(define (tutcode-selection-filter pc fn)
  (let ((sel (tutcode-selection-acquire-text pc)))
    (if (pair? sel)
      (let* ((str (string-list-concat sel))
             (res (fn str)))
        (if (and (string? res)
                 ;; 変更無しならcommitしない(セレクションが解除されないように)
                 (not (string=? res str)))
          (tutcode-selection-commit pc res sel))))))
