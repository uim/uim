;;; japanese-nicola.scm: NICOLA composition rulesets for Japanese
;;;
;;; Copyright (c) 2005 uim Project http://uim.freedesktop.org/
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

;; See following pages for further information about the NICOLA kana
;; input method
;;
;; Draft JIS specification (Japanese)
;; http://nicola.sunicom.co.jp/spec/jisdraft.htm
;;
;; Specification (Japanese)
;; http://nicola.sunicom.co.jp/spec/kikaku.htm
;;
;; Keyboard layout variations
;; http://nicola.sunicom.co.jp/info2.html


;; The key mappings coded in this file are transcribed from the NICOLA
;; specifications by hand. No other copyrighted data are included.
;;   -- YamaKen 2005-02-11

;; This composition rulesets only sense strict simultaneous key
;; presses. Timer-based compensations is not available.

(require "ng-japanese.scm")
(require "physical-key.scm")


(define ja-nicola-core-ruleset
  '(
    ;; left hand side without shift
    ((pkey_jp106_q)          ("。" "。" "｡"))
    ((pkey_jp106_w)          ("か" "カ" "ｶ"))
    ((pkey_jp106_e)          ("た" "タ" "ﾀ"))
    ((pkey_jp106_r)          ("こ" "コ" "ｺ"))
    ((pkey_jp106_t)          ("さ" "サ" "ｻ"))
		               
    ((pkey_jp106_a)          ("う" "ウ" "ｳ"))
    ((pkey_jp106_s)          ("し" "シ" "ｼ"))
    ((pkey_jp106_d)          ("て" "テ" "ﾃ"))
    ((pkey_jp106_f)          ("け" "ケ" "ｹ"))
    ((pkey_jp106_g)          ("せ" "セ" "ｾ"))
		               
    ((pkey_jp106_z)          ("．" "．" "."))
    ((pkey_jp106_x)          ("ひ" "ヒ" "ﾋ"))
    ((pkey_jp106_c)          ("す" "ス" "ｽ"))
    ((pkey_jp106_v)          ("ふ" "フ" "ﾌ"))
    ((pkey_jp106_b)          ("へ" "ヘ" "ﾍ"))

    ;; right hand side without shift
    ((pkey_jp106_y)          ("ら" "ラ" "ﾗ"))
    ((pkey_jp106_u)          ("ち" "チ" "ﾁ"))
    ((pkey_jp106_i)          ("く" "ク" "ｸ"))
    ((pkey_jp106_o)          ("つ" "ツ" "ﾂ"))
    ((pkey_jp106_p)          ("，" "，" ","))
    ((pkey_jp106_at)         ("、" "、" "､"))

    ((pkey_jp106_h)          ("は" "ハ" "ﾊ"))
    ((pkey_jp106_j)          ("と" "ト" "ﾄ"))
    ((pkey_jp106_k)          ("き" "キ" "ｷ"))
    ((pkey_jp106_l)          ("い" "イ" "ｲ"))
    ((pkey_jp106_semicolon)  ("ん" "ン" "ﾝ"))
			       
    ((pkey_jp106_n)          ("め" "メ" "ﾒ"))
    ((pkey_jp106_m)          ("そ" "ソ" "ｿ"))
    ((pkey_jp106_comma)      ("ね" "ネ" "ﾈ"))
    ((pkey_jp106_period)     ("ほ" "ホ" "ﾎ"))
    ((pkey_jp106_slash)      ("・" "・" "･"))

    ;; left hand side with same-handed shift
    (((chord lkey_Thumb_Shift_L pkey_jp106_q))          ("ぁ" "ァ" "ｧ"))
    (((chord lkey_Thumb_Shift_L pkey_jp106_w))          ("え" "エ" "ｴ"))
    (((chord lkey_Thumb_Shift_L pkey_jp106_e))          ("り" "リ" "ﾘ"))
    (((chord lkey_Thumb_Shift_L pkey_jp106_r))          ("ゃ" "ャ" "ｬ"))
    (((chord lkey_Thumb_Shift_L pkey_jp106_t))          ("れ" "レ" "ﾚ"))
							 
    (((chord lkey_Thumb_Shift_L pkey_jp106_a))          ("を" "ヲ" "ｦ"))
    (((chord lkey_Thumb_Shift_L pkey_jp106_s))          ("あ" "ア" "ｱ"))
    (((chord lkey_Thumb_Shift_L pkey_jp106_d))          ("な" "ナ" "ﾅ"))
    (((chord lkey_Thumb_Shift_L pkey_jp106_f))          ("ゅ" "ュ" "ｭ"))
    (((chord lkey_Thumb_Shift_L pkey_jp106_g))          ("も" "モ" "ﾓ"))
							 
    (((chord lkey_Thumb_Shift_L pkey_jp106_z))          ("ぅ" "ゥ" "ｩ"))
    (((chord lkey_Thumb_Shift_L pkey_jp106_x))          ("ー" "ー" "ｰ"))
    (((chord lkey_Thumb_Shift_L pkey_jp106_c))          ("ろ" "ロ" "ﾛ"))
    (((chord lkey_Thumb_Shift_L pkey_jp106_v))          ("や" "ヤ" "ﾔ"))
    (((chord lkey_Thumb_Shift_L pkey_jp106_b))          ("ぃ" "ィ" "ｨ"))
							 
    ;; right hand side with same-handed shift		 
    (((chord lkey_Thumb_Shift_R pkey_jp106_y))          ("よ" "ヨ" "ﾖ"))
    (((chord lkey_Thumb_Shift_R pkey_jp106_u))          ("に" "ニ" "ﾆ"))
    (((chord lkey_Thumb_Shift_R pkey_jp106_i))          ("る" "ル" "ﾙ"))
    (((chord lkey_Thumb_Shift_R pkey_jp106_o))          ("ま" "マ" "ﾏ"))
    (((chord lkey_Thumb_Shift_R pkey_jp106_p))          ("ぇ" "ェ" "ｪ"))
    ;;(((chord lkey_Thumb_Shift_R pkey_jp106_at))         ("" "" ""))

    (((chord lkey_Thumb_Shift_R pkey_jp106_h))          ("み" "ミ" "ﾐ"))
    (((chord lkey_Thumb_Shift_R pkey_jp106_j))          ("お" "オ" "ｵ"))
    (((chord lkey_Thumb_Shift_R pkey_jp106_k))          ("の" "ノ" "ﾉ"))
    (((chord lkey_Thumb_Shift_R pkey_jp106_l))          ("ょ" "ョ" "ｮ"))
    (((chord lkey_Thumb_Shift_R pkey_jp106_semicolon))  ("っ" "ッ" "ｯ"))
							 
    (((chord lkey_Thumb_Shift_R pkey_jp106_n))          ("ぬ" "ヌ" "ﾇ"))
    (((chord lkey_Thumb_Shift_R pkey_jp106_m))          ("ゆ" "ユ" "ﾕ"))
    (((chord lkey_Thumb_Shift_R pkey_jp106_comma))      ("む" "ム" "ﾑ"))
    (((chord lkey_Thumb_Shift_R pkey_jp106_period))     ("わ" "ワ" "ﾜ"))
    (((chord lkey_Thumb_Shift_R pkey_jp106_slash))      ("ぉ" "ォ" "ｫ"))

    ;; left hand side with cross-shift
    ;;(((chord lkey_Thumb_Shift_R pkey_jp106_q))          ("" "" ""))
    (((chord lkey_Thumb_Shift_R pkey_jp106_w))          ("が" "ガ" ("ｶ" "ﾞ")))
    (((chord lkey_Thumb_Shift_R pkey_jp106_e))          ("だ" "ダ" ("ﾀ" "ﾞ")))
    (((chord lkey_Thumb_Shift_R pkey_jp106_r))          ("ご" "ゴ" ("ｺ" "ﾞ")))
    (((chord lkey_Thumb_Shift_R pkey_jp106_t))          ("ざ" "ザ" ("ｻ" "ﾞ")))
							 
    ;;(((chord lkey_Thumb_Shift_R pkey_jp106_a))   (("う" "゛") "ヴ" ("ｳ" "ﾞ")))
    (((chord lkey_Thumb_Shift_R pkey_jp106_s))          ("じ" "ジ" ("ｼ" "ﾞ")))
    (((chord lkey_Thumb_Shift_R pkey_jp106_d))          ("で" "デ" ("ﾃ" "ﾞ")))
    (((chord lkey_Thumb_Shift_R pkey_jp106_f))          ("げ" "ゲ" ("ｹ" "ﾞ")))
    (((chord lkey_Thumb_Shift_R pkey_jp106_g))          ("ぜ" "ゼ" ("ｾ" "ﾞ")))
							 
    ;;(((chord lkey_Thumb_Shift_R pkey_jp106_z))          ("" "" ""))
    (((chord lkey_Thumb_Shift_R pkey_jp106_x))          ("び" "ビ" ("ﾋ" "ﾞ")))
    (((chord lkey_Thumb_Shift_R pkey_jp106_c))          ("ず" "ズ" ("ｽ" "ﾞ")))
    (((chord lkey_Thumb_Shift_R pkey_jp106_v))          ("ぶ" "ブ" ("ﾌ" "ﾞ")))
    (((chord lkey_Thumb_Shift_R pkey_jp106_b))          ("べ" "ベ" ("ﾍ" "ﾞ")))
							 
    ;; right hand side with cross-shift			 
    (((chord lkey_Thumb_Shift_L pkey_jp106_y))          ("ぱ" "パ" ("ﾊ" "ﾟ")))
    (((chord lkey_Thumb_Shift_L pkey_jp106_u))          ("ぢ" "ヂ" ("ﾁ" "ﾞ")))
    (((chord lkey_Thumb_Shift_L pkey_jp106_i))          ("ぐ" "グ" ("ｸ" "ﾞ")))
    (((chord lkey_Thumb_Shift_L pkey_jp106_o))          ("づ" "ヅ" ("ﾂ" "ﾞ")))
    (((chord lkey_Thumb_Shift_L pkey_jp106_p))          ("ぴ" "ピ" ("ﾋ" "ﾟ")))
    ;;(((chord lkey_Thumb_Shift_L pkey_jp106_at))         ("" "" ""))

    (((chord lkey_Thumb_Shift_L pkey_jp106_h))          ("ば" "バ" ("ﾊ" "ﾞ")))
    (((chord lkey_Thumb_Shift_L pkey_jp106_j))          ("ど" "ド" ("ﾄ" "ﾞ")))
    (((chord lkey_Thumb_Shift_L pkey_jp106_k))          ("ぎ" "ギ" ("ｷ" "ﾞ")))
    (((chord lkey_Thumb_Shift_L pkey_jp106_l))          ("ぽ" "ポ" ("ﾎ" "ﾟ")))
    ;;(((chord lkey_Thumb_Shift_L pkey_jp106_semicolon))  ("" "" ""))
							 
    (((chord lkey_Thumb_Shift_L pkey_jp106_n))          ("ぷ" "プ" ("ﾌ" "ﾟ")))
    (((chord lkey_Thumb_Shift_L pkey_jp106_m))          ("ぞ" "ゾ" ("ｿ" "ﾞ")))
    (((chord lkey_Thumb_Shift_L pkey_jp106_comma))      ("ぺ" "ペ" ("ﾍ" "ﾟ")))
    (((chord lkey_Thumb_Shift_L pkey_jp106_period))     ("ぼ" "ボ" ("ﾎ" "ﾞ")))
    ;;(((chord lkey_Thumb_Shift_L pkey_jp106_slash))      ("" "" ""))
    ))

(define ja-nicola-postfixed-voiced-consonant-ruleset
  '(
    ;; right hand side without shift
    ((pkey_jp106_bracketleft) ("゛" "" ""))

    ;; right hand side with same-handed shift		 
    (((chord lkey_Thumb_Shift_R pkey_jp106_bracketleft)) ("゜" "" ""))

    ;; right hand side with cross-shift			 
    ;;(((chord lkey_Thumb_Shift_R pkey_jp106_bracketleft)) ("" "" ""))

    ;; left hand side without shift
    ;;((pkey_jp106_q         pkey_jp106_bracketleft) ("。" "。" "｡"))
    ((pkey_jp106_w         pkey_jp106_bracketleft) ("が" "ガ" ("ｶ" "ﾞ")))
    ((pkey_jp106_e         pkey_jp106_bracketleft) ("だ" "ダ" ("ﾀ" "ﾞ")))
    ((pkey_jp106_r         pkey_jp106_bracketleft) ("ご" "ゴ" ("ｺ" "ﾞ")))
    ((pkey_jp106_t         pkey_jp106_bracketleft) ("ざ" "ザ" ("ｻ" "ﾞ")))
		           
    ((pkey_jp106_a         pkey_jp106_bracketleft) (("う" "゛") "ヴ" ("ｳ" "ﾞ")))
    ((pkey_jp106_s         pkey_jp106_bracketleft) ("じ" "ジ" ("ｼ" "ﾞ")))
    ((pkey_jp106_d         pkey_jp106_bracketleft) ("で" "デ" ("ﾃ" "ﾞ")))
    ((pkey_jp106_f         pkey_jp106_bracketleft) ("げ" "ゲ" ("ｹ" "ﾞ")))
    ((pkey_jp106_g         pkey_jp106_bracketleft) ("ぜ" "ゼ" ("ｾ" "ﾞ")))
		           
    ;;((pkey_jp106_z         pkey_jp106_bracketleft) ("．" "．" "."))
    ((pkey_jp106_x         pkey_jp106_bracketleft) ("び" "ビ" ("ﾋ" "ﾞ")))
    ((pkey_jp106_c         pkey_jp106_bracketleft) ("ず" "ズ" ("ｽ" "ﾞ")))
    ((pkey_jp106_v         pkey_jp106_bracketleft) ("ぶ" "ブ" ("ﾌ" "ﾞ")))
    ((pkey_jp106_b         pkey_jp106_bracketleft) ("べ" "ベ" ("ﾍ" "ﾞ")))

    ;; right hand side without shift
    ;;((pkey_jp106_y         pkey_jp106_bracketleft) ("ら" "ラ" "ﾗ"))
    ((pkey_jp106_u         pkey_jp106_bracketleft) ("ぢ" "ヂ" ("ﾁ" "ﾞ")))
    ((pkey_jp106_i         pkey_jp106_bracketleft) ("ぐ" "グ" ("ｸ" "ﾞ")))
    ((pkey_jp106_o         pkey_jp106_bracketleft) ("づ" "ヅ" ("ﾂ" "ﾞ")))
    ;;((pkey_jp106_p         pkey_jp106_bracketleft) ("，" "，" ","))
    ;;((pkey_jp106_at        pkey_jp106_bracketleft) ("、" "、" "､"))

    ((pkey_jp106_h         pkey_jp106_bracketleft) ("ば" "バ" ("ﾊ" "ﾞ")))
    ((pkey_jp106_j         pkey_jp106_bracketleft) ("ど" "ド" ("ﾄ" "ﾞ")))
    ((pkey_jp106_k         pkey_jp106_bracketleft) ("ぎ" "ギ" ("ｷ" "ﾞ")))
    ;;((pkey_jp106_l         pkey_jp106_bracketleft) ("い" "イ" "ｲ"))
    ;;((pkey_jp106_semicolon pkey_jp106_bracketleft) ("ん" "ン" "ﾝ"))
			       
    ;;((pkey_jp106_n         pkey_jp106_bracketleft) ("め" "メ" "ﾒ"))
    ((pkey_jp106_m         pkey_jp106_bracketleft) ("ぞ" "ゾ" ("ｿ" "ﾞ")))
    ;;((pkey_jp106_comma     pkey_jp106_bracketleft) ("ね" "ネ" "ﾈ"))
    ((pkey_jp106_period    pkey_jp106_bracketleft) ("ぼ" "ボ" ("ﾎ" "ﾞ")))
    ;;((pkey_jp106_slash     pkey_jp106_bracketleft) ("・" "・" "･"))

    ;; left hand side without shift
    ((pkey_jp106_x      (chord lkey_Thumb_Shift_R pkey_jp106_bracketleft))
                                                      ("ぴ" "ピ" ("ﾋ" "ﾟ")))
    ((pkey_jp106_v      (chord lkey_Thumb_Shift_R pkey_jp106_bracketleft))
                                                      ("ぷ" "プ" ("ﾌ" "ﾟ")))
    ((pkey_jp106_b      (chord lkey_Thumb_Shift_R pkey_jp106_bracketleft))
                                                      ("ぺ" "ペ" ("ﾍ" "ﾟ")))

    ;; right hand side without shift
    ((pkey_jp106_h      (chord lkey_Thumb_Shift_R pkey_jp106_bracketleft))
                                                      ("ぱ" "パ" ("ﾊ" "ﾟ")))
    ((pkey_jp106_period (chord lkey_Thumb_Shift_R pkey_jp106_bracketleft))
                                                      ("ぽ" "ポ" ("ﾎ" "ﾟ")))
    ))

;;(define ja-nicola-jp106-pseudo-thumb-shift-ruleset
;;  '((((lkey_Henkan   press   peek)) (($1 lkey_Thumb_Shift_R loopback)))
;;    (((lkey_Henkan   release peek)) (($1 lkey_Thumb_Shift_R loopback)))
;;    (((lkey_Muhenkan press   peek)) (($1 lkey_Thumb_Shift_L loopback)))
;;    (((lkey_Muhenkan release peek)) (($1 lkey_Thumb_Shift_L loopback)))))

(define ja-nicola-ruleset-name-list
  '(core
    postfixed-voiced-consonant))

(ja-define-dedicated-rulesets 'ja-nicola ja-nicola-ruleset-name-list)

(define ja-nicola-hiragana-ruleset
  (append
   ja-nicola-hiragana-core-ruleset
   ja-nicola-hiragana-postfixed-voiced-consonant-ruleset
   ja-nicola-jp106-pseudo-thumb-shift-ruleset))

(define ja-nicola-katakana-ruleset
  (append
   ja-nicola-katakana-core-ruleset
   ja-nicola-katakana-postfixed-voiced-consonant-ruleset
   ja-nicola-jp106-pseudo-thumb-shift-ruleset))

(define ja-nicola-halfkana-ruleset
  (append
   ja-nicola-halfkana-core-ruleset
   ja-nicola-halfkana-postfixed-voiced-consonant-ruleset
   ja-nicola-jp106-pseudo-thumb-shift-ruleset))

(define ja-nicola-hiragana-ruletree
  (evmap-parse-ruleset ja-nicola-hiragana-ruleset))

(define ja-nicola-katakana-ruletree
  (evmap-parse-ruleset ja-nicola-katakana-ruleset))

(define ja-nicola-halfkana-ruletree
  (evmap-parse-ruleset ja-nicola-halfkana-ruleset))
