;;; ng-japanese-kana.scm: Kana composition rulesets for Japanese
;;;
;;; Copyright (c) 2004-2005 uim Project http://uim.freedesktop.org/
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
;;; THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
;;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
;;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;;; SUCH DAMAGE.
;;;;


(require "ng-japanese.scm")
(require "physical-key.scm")


;; character sensitive version
(define ja-kana-char-sensitive-core-ruleset
  '(
    (("3")      ("あ" "ア" "ｱ"))
    (("e")      ("い" "イ" "ｲ"))
    (("4")      ("う" "ウ" "ｳ"))
    (("5")      ("え" "エ" "ｴ"))
    (("6")      ("お" "オ" "ｵ"))

    (("t")      ("か" "カ" "ｶ"))
    (("g")      ("き" "キ" "ｷ"))
    (("h")      ("く" "ク" "ｸ"))
    ((":")      ("け" "ケ" "ｹ"))
    (("b")      ("こ" "コ" "ｺ"))

    (("x")      ("さ" "サ" "ｻ"))
    (("d")      ("し" "シ" "ｼ"))
    (("r")      ("す" "ス" "ｽ"))
    (("p")      ("せ" "セ" "ｾ"))
    (("c")      ("そ" "ソ" "ｿ"))

    (("q")      ("た" "タ" "ﾀ"))
    (("a")      ("ち" "チ" "ﾁ"))
    (("z")      ("つ" "ツ" "ﾂ"))
    (("w")      ("て" "テ" "ﾃ"))
    (("s")      ("と" "ト" "ﾄ"))

    (("u")      ("な" "ナ" "ﾅ"))
    (("i")      ("に" "ニ" "ﾆ"))
    (("1")      ("ぬ" "ヌ" "ﾇ"))
    ((",")      ("ね" "ネ" "ﾈ"))
    (("k")      ("の" "ノ" "ﾉ"))

    (("f")      ("は" "ハ" "ﾊ"))
    (("v")      ("ひ" "ヒ" "ﾋ"))
    (("2")      ("ふ" "フ" "ﾌ"))
    (("^")      ("へ" "ヘ" "ﾍ"))
    (("-")      ("ほ" "ホ" "ﾎ"))

    (("j")      ("ま" "マ" "ﾏ"))
    (("n")      ("み" "ミ" "ﾐ"))
    (("]")      ("む" "ム" "ﾑ"))  ;; be careful
    (("/")      ("め" "メ" "ﾒ"))
    (("m")      ("も" "モ" "ﾓ"))

    (("7")      ("や" "ヤ" "ﾔ"))
    (("8")      ("ゆ" "ユ" "ﾕ"))
    (("9")      ("よ" "ヨ" "ﾖ"))

    (("o")      ("ら" "ラ" "ﾗ"))
    (("l")      ("り" "リ" "ﾘ"))
    ((".")      ("る" "ル" "ﾙ"))
    ((";")      ("れ" "レ" "ﾚ"))
    (("\\")     ("ろ" "ロ" "ﾛ"))  ;; be careful

    (("0")      ("わ" "ワ" "ﾜ"))
    (("~")      ("を" "ヲ" "ｦ"))  ;; not proper key
    (("y")      ("ん" "ン" "ﾝ"))

    (("4" "@")  (("う" "゛") "ヴ" ("ｳ" "ﾞ")))

    (("t" "@")  ("が" "ガ" ("ｶ" "ﾞ")))
    (("g" "@")  ("ぎ" "ギ" ("ｷ" "ﾞ")))
    (("h" "@")  ("ぐ" "グ" ("ｸ" "ﾞ")))
    ((":" "@")  ("げ" "ゲ" ("ｹ" "ﾞ")))
    (("b" "@")  ("ご" "ゴ" ("ｺ" "ﾞ")))

    (("x" "@")  ("ざ" "ザ" ("ｻ" "ﾞ")))
    (("d" "@")  ("じ" "ジ" ("ｼ" "ﾞ")))
    (("r" "@")  ("ず" "ズ" ("ｽ" "ﾞ")))
    (("p" "@")  ("ぜ" "ゼ" ("ｾ" "ﾞ")))
    (("c" "@")  ("ぞ" "ゾ" ("ｿ" "ﾞ")))

    (("q" "@")  ("だ" "ダ" ("ﾀ" "ﾞ")))
    (("a" "@")  ("ぢ" "ヂ" ("ﾁ" "ﾞ")))
    (("z" "@")  ("づ" "ヅ" ("ﾂ" "ﾞ")))
    (("w" "@")  ("で" "デ" ("ﾃ" "ﾞ")))
    (("s" "@")  ("ど" "ド" ("ﾄ" "ﾞ")))

    (("f" "@")  ("ば" "バ" ("ﾊ" "ﾞ")))
    (("v" "@")  ("び" "ビ" ("ﾋ" "ﾞ")))
    (("2" "@")  ("ぶ" "ブ" ("ﾌ" "ﾞ")))
    (("^" "@")  ("べ" "ベ" ("ﾍ" "ﾞ")))
    (("-" "@")  ("ぼ" "ボ" ("ﾎ" "ﾞ")))

    (("f" "[")  ("ぱ" "パ" ("ﾊ" "ﾟ")))
    (("v" "[")  ("ぴ" "ピ" ("ﾋ" "ﾟ")))
    (("2" "[")  ("ぷ" "プ" ("ﾌ" "ﾟ")))
    (("^" "[")  ("ぺ" "ペ" ("ﾍ" "ﾟ")))
    (("-" "[")  ("ぽ" "ポ" ("ﾎ" "ﾟ")))

    (("#")      ("ぁ" "ァ" "ｧ"))
    (("E")      ("ぃ" "ィ" "ｨ"))
    (("$")      ("ぅ" "ゥ" "ｩ"))
    (("%")      ("ぇ" "ェ" "ｪ"))
    (("&")      ("ぉ" "ォ" "ｫ"))

    (("'")      ("ゃ" "ャ" "ｬ"))
    (("(")      ("ゅ" "ュ" "ｭ"))
    ((")")      ("ょ" "ョ" "ｮ"))

    (("Z")      ("っ" "ッ" "ｯ"))

    (("@")      ("゛" "゛" "ﾞ"))
    (("[")      ("゜" "゜" "ﾟ"))
    (("|")      ("ー" "ー" "ｰ"))  ;; be careful, not proper key
    ((">")      ("。" "。" "｡"))
    (("<")      ("、" "、" "､"))
    (("?")      ("・" "・" "･"))
    (("{")      ("「" "「" "｢"))
    (("}")      ("」" "」" "｣"))
    ))

;; physical key sensitive version
(define ja-kana-core-ruleset
  '(
    ((pkey_jp106_3)            ("あ" "ア" "ｱ"))
    ((pkey_jp106_e)            ("い" "イ" "ｲ"))
    ((pkey_jp106_4)            ("う" "ウ" "ｳ"))
    ((pkey_jp106_5)            ("え" "エ" "ｴ"))
    ((pkey_jp106_6)            ("お" "オ" "ｵ"))

    ((pkey_jp106_t)            ("か" "カ" "ｶ"))
    ((pkey_jp106_g)            ("き" "キ" "ｷ"))
    ((pkey_jp106_h)            ("く" "ク" "ｸ"))
    ((pkey_jp106_colon)        ("け" "ケ" "ｹ"))
    ((pkey_jp106_b)            ("こ" "コ" "ｺ"))

    ((pkey_jp106_x)            ("さ" "サ" "ｻ"))
    ((pkey_jp106_d)            ("し" "シ" "ｼ"))
    ((pkey_jp106_r)            ("す" "ス" "ｽ"))
    ((pkey_jp106_p)            ("せ" "セ" "ｾ"))
    ((pkey_jp106_c)            ("そ" "ソ" "ｿ"))

    ((pkey_jp106_q)            ("た" "タ" "ﾀ"))
    ((pkey_jp106_a)            ("ち" "チ" "ﾁ"))
    ((pkey_jp106_z)            ("つ" "ツ" "ﾂ"))
    ((pkey_jp106_w)            ("て" "テ" "ﾃ"))
    ((pkey_jp106_s)            ("と" "ト" "ﾄ"))

    ((pkey_jp106_u)            ("な" "ナ" "ﾅ"))
    ((pkey_jp106_i)            ("に" "ニ" "ﾆ"))
    ((pkey_jp106_1)            ("ぬ" "ヌ" "ﾇ"))
    ((pkey_jp106_comma)        ("ね" "ネ" "ﾈ"))
    ((pkey_jp106_k)            ("の" "ノ" "ﾉ"))

    ((pkey_jp106_f)            ("は" "ハ" "ﾊ"))
    ((pkey_jp106_v)            ("ひ" "ヒ" "ﾋ"))
    ((pkey_jp106_2)            ("ふ" "フ" "ﾌ"))
    ((pkey_jp106_asciicircum)  ("へ" "ヘ" "ﾍ"))
    ((pkey_jp106_minus)        ("ほ" "ホ" "ﾎ"))

    ((pkey_jp106_j)            ("ま" "マ" "ﾏ"))
    ((pkey_jp106_n)            ("み" "ミ" "ﾐ"))
    ((pkey_jp106_bracketright) ("む" "ム" "ﾑ"))  ;; be careful
    ((pkey_jp106_slash)        ("め" "メ" "ﾒ"))
    ((pkey_jp106_m)            ("も" "モ" "ﾓ"))

    ((pkey_jp106_7)            ("や" "ヤ" "ﾔ"))
    ((pkey_jp106_8)            ("ゆ" "ユ" "ﾕ"))
    ((pkey_jp106_9)            ("よ" "ヨ" "ﾖ"))

    ((pkey_jp106_o)            ("ら" "ラ" "ﾗ"))
    ((pkey_jp106_l)            ("り" "リ" "ﾘ"))
    ((pkey_jp106_period)       ("る" "ル" "ﾙ"))
    ((pkey_jp106_semicolon)    ("れ" "レ" "ﾚ"))
    ((pkey_jp106_backslash)    ("ろ" "ロ" "ﾛ"))  ;; be careful

    ((pkey_jp106_0)            ("わ" "ワ" "ﾜ"))
    ((pkey_jp106_0 mod_shift)  ("を" "ヲ" "ｦ"))
    ((pkey_jp106_y)            ("ん" "ン" "ﾝ"))

    ((pkey_jp106_4           pkey_jp106_at) (("う" "゛") "ヴ" ("ｳ" "ﾞ")))

    ((pkey_jp106_t           pkey_jp106_at) ("が" "ガ" ("ｶ" "ﾞ")))
    ((pkey_jp106_g           pkey_jp106_at) ("ぎ" "ギ" ("ｷ" "ﾞ")))
    ((pkey_jp106_h           pkey_jp106_at) ("ぐ" "グ" ("ｸ" "ﾞ")))
    ((pkey_jp106_colon       pkey_jp106_at) ("げ" "ゲ" ("ｹ" "ﾞ")))
    ((pkey_jp106_b           pkey_jp106_at) ("ご" "ゴ" ("ｺ" "ﾞ")))

    ((pkey_jp106_x           pkey_jp106_at) ("ざ" "ザ" ("ｻ" "ﾞ")))
    ((pkey_jp106_d           pkey_jp106_at) ("じ" "ジ" ("ｼ" "ﾞ")))
    ((pkey_jp106_r           pkey_jp106_at) ("ず" "ズ" ("ｽ" "ﾞ")))
    ((pkey_jp106_p           pkey_jp106_at) ("ぜ" "ゼ" ("ｾ" "ﾞ")))
    ((pkey_jp106_c           pkey_jp106_at) ("ぞ" "ゾ" ("ｿ" "ﾞ")))

    ((pkey_jp106_q           pkey_jp106_at) ("だ" "ダ" ("ﾀ" "ﾞ")))
    ((pkey_jp106_a           pkey_jp106_at) ("ぢ" "ヂ" ("ﾁ" "ﾞ")))
    ((pkey_jp106_z           pkey_jp106_at) ("づ" "ヅ" ("ﾂ" "ﾞ")))
    ((pkey_jp106_w           pkey_jp106_at) ("で" "デ" ("ﾃ" "ﾞ")))
    ((pkey_jp106_s           pkey_jp106_at) ("ど" "ド" ("ﾄ" "ﾞ")))

    ((pkey_jp106_f           pkey_jp106_at) ("ば" "バ" ("ﾊ" "ﾞ")))
    ((pkey_jp106_v           pkey_jp106_at) ("び" "ビ" ("ﾋ" "ﾞ")))
    ((pkey_jp106_2           pkey_jp106_at) ("ぶ" "ブ" ("ﾌ" "ﾞ")))
    ((pkey_jp106_asciicircum pkey_jp106_at) ("べ" "ベ" ("ﾍ" "ﾞ")))
    ((pkey_jp106_minus       pkey_jp106_at) ("ぼ" "ボ" ("ﾎ" "ﾞ")))

    ((pkey_jp106_f           pkey_jp106_bracketleft) ("ぱ" "パ" ("ﾊ" "ﾟ")))
    ((pkey_jp106_v           pkey_jp106_bracketleft) ("ぴ" "ピ" ("ﾋ" "ﾟ")))
    ((pkey_jp106_2           pkey_jp106_bracketleft) ("ぷ" "プ" ("ﾌ" "ﾟ")))
    ((pkey_jp106_asciicircum pkey_jp106_bracketleft) ("ぺ" "ペ" ("ﾍ" "ﾟ")))
    ((pkey_jp106_minus       pkey_jp106_bracketleft) ("ぽ" "ポ" ("ﾎ" "ﾟ")))

    ((pkey_jp106_3           mod_shift)     ("ぁ" "ァ" "ｧ"))
    ((pkey_jp106_e           mod_shift)     ("ぃ" "ィ" "ｨ"))
    ((pkey_jp106_4           mod_shift)     ("ぅ" "ゥ" "ｩ"))
    ((pkey_jp106_5           mod_shift)     ("ぇ" "ェ" "ｪ"))
    ((pkey_jp106_6           mod_shift)     ("ぉ" "ォ" "ｫ"))

    ((pkey_jp106_7           mod_shift)     ("ゃ" "ャ" "ｬ"))
    ((pkey_jp106_8           mod_shift)     ("ゅ" "ュ" "ｭ"))
    ((pkey_jp106_9           mod_shift)     ("ょ" "ョ" "ｮ"))

    ((pkey_jp106_z           mod_shift)     ("っ" "ッ" "ｯ"))

    ((pkey_jp106_at)                        ("゛" "゛" "ﾞ"))
    ((pkey_jp106_bracketleft)               ("゜" "゜" "ﾟ"))
    ((pkey_jp106_yen)                       ("ー" "ー" "ｰ"))  ;; be careful
    ((pkey_jp106_period       mod_shift)    ("。" "。" "｡"))
    ((pkey_jp106_comma        mod_shift)    ("、" "、" "､"))
    ((pkey_jp106_slash        mod_shift)    ("・" "・" "･"))
    ((pkey_jp106_bracketleft  mod_shift)    ("「" "「" "｢"))
    ((pkey_jp106_bracketright mod_shift)    ("」" "」" "｣"))  ;; be careful
    ))

(define ja-kana-ruleset-name-list
  '(char-sensitive-core
    core))

(ja-define-dedicated-rulesets 'ja-kana ja-kana-ruleset-name-list)

(define ja-kana-hiragana-ruleset ja-kana-hiragana-core-ruleset)
(define ja-kana-katakana-ruleset ja-kana-katakana-core-ruleset)
(define ja-kana-halfkana-ruleset ja-kana-halfkana-core-ruleset)
