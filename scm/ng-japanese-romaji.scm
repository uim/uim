;;; ng-japanese-romaji.scm: Romaji composition rulesets for Japanese
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

;; See following pages for further information about romaji.
;;
;; http://en.wikipedia.org/wiki/Romaji
;; http://ja.wikipedia.org/wiki/%E3%83%AD%E3%83%BC%E3%83%9E%E5%AD%97

(require "ng-japanese.scm")


;;
;; double consonant: 促音
;;

;;    (("k" "k")     (("っ" "k")))
;;    (("k" "k" "a") (("っ" "か")))
;;    ...
;;    (("k" "k" "o") (("っ" "こ")))
;;    (("k" "k" "z") ("you can use any kk* sequence"))

(define ja-romaji-double-consonant-alist
  (map (lambda (c)
	 (cons c ja-kana-xtsu))
       '("k" "g" "s" "z" "j" "t" "c" "d" "h" "f" "b" "p"
	 ;;"m"  ;; rk contains this rule
	 "y" "r" "v" "w")))

(define ja-romaji-generate-double-consonant-ruleset
  (lambda (romaji-ruleset)
    (append-map (lambda (entry)
		  (let ((letter (car entry))
			(kana (cdr entry)))
		    (filter-map (lambda (rule)
				  (let* ((seq (evmap-rule-event-seq rule))
					 (res (evmap-rule-action-seq rule))
					 (listified (map (lambda (elem)
							   (if (list? elem)
							       elem
							       (list elem)))
							 res)))
				    (and (string=? letter
						   (car seq))
					 (list (cons letter seq)
					       (map cons kana listified)))))
				romaji-ruleset)))
		ja-romaji-double-consonant-alist)))

(define ja-romaji-double-consonant-guide-ruleset
  (map (lambda (entry)
	 (let ((letter (car entry))
	       (kana (cdr entry)))
	   (list (list letter letter)
		 (map cons
		      kana
		      (make-list (length kana)
				 (list letter))))))
       ja-romaji-double-consonant-alist))

;;
;; the rulesets
;;

(define ja-romaji-basic-ruleset
  '(
    ;; あ行
    (("a")         ("あ" "ア" "ｱ"))
    (("i")         ("い" "イ" "ｲ"))
    (("u")         ("う" "ウ" "ｳ"))
    (("e")         ("え" "エ" "ｴ"))
    (("o")         ("お" "オ" "ｵ"))

    ;; か行
    (("k" "a")     ("か" "カ" "ｶ"))
    (("k" "i")     ("き" "キ" "ｷ"))
    (("k" "u")     ("く" "ク" "ｸ"))
    (("k" "e")     ("け" "ケ" "ｹ"))
    (("k" "o")     ("こ" "コ" "ｺ"))

    (("k" "y" "a") (("き" "ゃ") ("キ" "ャ") ("ｷ" "ｬ")))
    (("k" "y" "i") (("き" "ぃ") ("キ" "ィ") ("ｷ" "ｨ")))
    (("k" "y" "u") (("き" "ゅ") ("キ" "ュ") ("ｷ" "ｭ")))
    (("k" "y" "e") (("き" "ぇ") ("キ" "ェ") ("ｷ" "ｪ")))
    (("k" "y" "o") (("き" "ょ") ("キ" "ョ") ("ｷ" "ｮ")))

    (("g" "a")     ("が" "ガ" ("ｶ" "ﾞ")))
    (("g" "i")     ("ぎ" "ギ" ("ｷ" "ﾞ")))
    (("g" "u")     ("ぐ" "グ" ("ｸ" "ﾞ")))
    (("g" "e")     ("げ" "ゲ" ("ｹ" "ﾞ")))
    (("g" "o")     ("ご" "ゴ" ("ｺ" "ﾞ")))

    (("g" "y" "a") (("ぎ" "ゃ") ("ギ" "ャ") ("ｷ" "ﾞ" "ｬ")))
    (("g" "y" "i") (("ぎ" "ぃ") ("ギ" "ィ") ("ｷ" "ﾞ" "ｨ")))
    (("g" "y" "u") (("ぎ" "ゅ") ("ギ" "ュ") ("ｷ" "ﾞ" "ｭ")))
    (("g" "y" "e") (("ぎ" "ぇ") ("ギ" "ェ") ("ｷ" "ﾞ" "ｪ")))
    (("g" "y" "o") (("ぎ" "ょ") ("ギ" "ョ") ("ｷ" "ﾞ" "ｮ")))

    ;; さ行
    (("s" "a")     ("さ" "サ" "ｻ"))
    (("s" "i")     ("し" "シ" "ｼ"))
    (("s" "u")     ("す" "ス" "ｽ"))
    (("s" "e")     ("せ" "セ" "ｾ"))
    (("s" "o")     ("そ" "ソ" "ｿ"))

    (("s" "y" "a") (("し" "ゃ") ("シ" "ャ") ("ｼ" "ｬ")))
    (("s" "y" "i") (("し" "ぃ") ("シ" "ィ") ("ｼ" "ｨ")))
    (("s" "y" "u") (("し" "ゅ") ("シ" "ュ") ("ｼ" "ｭ")))
    (("s" "y" "e") (("し" "ぇ") ("シ" "ェ") ("ｼ" "ｪ")))
    (("s" "y" "o") (("し" "ょ") ("シ" "ョ") ("ｼ" "ｮ")))

    (("z" "a")     ("ざ" "ザ" ("ｻ" "ﾞ")))
    (("z" "i")     ("じ" "ジ" ("ｼ" "ﾞ")))
    (("z" "u")     ("ず" "ズ" ("ｽ" "ﾞ")))
    (("z" "e")     ("ぜ" "ゼ" ("ｾ" "ﾞ")))
    (("z" "o")     ("ぞ" "ゾ" ("ｿ" "ﾞ")))

    (("z" "y" "a") (("じ" "ゃ") ("ジ" "ャ") ("ｼ" "ﾞ" "ｬ")))
    (("z" "y" "i") (("じ" "ぃ") ("ジ" "ィ") ("ｼ" "ﾞ" "ｨ")))
    (("z" "y" "u") (("じ" "ゅ") ("ジ" "ュ") ("ｼ" "ﾞ" "ｭ")))
    (("z" "y" "e") (("じ" "ぇ") ("ジ" "ェ") ("ｼ" "ﾞ" "ｪ")))
    (("z" "y" "o") (("じ" "ょ") ("ジ" "ョ") ("ｼ" "ﾞ" "ｮ")))

    ;; た行
    (("t" "a")     ("た" "タ" "ﾀ"))
    (("t" "i")     ("ち" "チ" "ﾁ"))
    (("t" "u")     ("つ" "ツ" "ﾂ"))
    (("t" "e")     ("て" "テ" "ﾃ"))
    (("t" "o")     ("と" "ト" "ﾄ"))

    (("t" "y" "a") (("ち" "ゃ") ("チ" "ャ") ("ﾁ" "ｬ")))
    (("t" "y" "i") (("ち" "ぃ") ("チ" "ィ") ("ﾁ" "ｨ")))
    (("t" "y" "u") (("ち" "ゅ") ("チ" "ュ") ("ﾁ" "ｭ")))
    (("t" "y" "e") (("ち" "ぇ") ("チ" "ェ") ("ﾁ" "ｪ")))
    (("t" "y" "o") (("ち" "ょ") ("チ" "ョ") ("ﾁ" "ｮ")))

    (("d" "a")     ("だ" "ダ" ("ﾀ" "ﾞ")))
    (("d" "i")     ("ぢ" "ヂ" ("ﾁ" "ﾞ")))
    (("d" "u")     ("づ" "ヅ" ("ﾂ" "ﾞ")))
    (("d" "e")     ("で" "デ" ("ﾃ" "ﾞ")))
    (("d" "o")     ("ど" "ド" ("ﾄ" "ﾞ")))

    (("d" "y" "a") (("ぢ" "ゃ") ("ヂ" "ャ") ("ﾁ" "ﾞ" "ｬ")))
    (("d" "y" "i") (("ぢ" "ぃ") ("ヂ" "ィ") ("ﾁ" "ﾞ" "ｨ")))
    (("d" "y" "u") (("ぢ" "ゅ") ("ヂ" "ュ") ("ﾁ" "ﾞ" "ｭ")))
    (("d" "y" "e") (("ぢ" "ぇ") ("ヂ" "ェ") ("ﾁ" "ﾞ" "ｪ")))
    (("d" "y" "o") (("ぢ" "ょ") ("ヂ" "ョ") ("ﾁ" "ﾞ" "ｮ")))

    ;; な行
    (("n" "a")     ("な" "ナ" "ﾅ"))
    (("n" "i")     ("に" "ニ" "ﾆ"))
    (("n" "u")     ("ぬ" "ヌ" "ﾇ"))
    (("n" "e")     ("ね" "ネ" "ﾈ"))
    (("n" "o")     ("の" "ノ" "ﾉ"))

    (("n" "y" "a") (("に" "ゃ") ("ニ" "ャ") ("ﾆ" "ｬ")))
    (("n" "y" "i") (("に" "ぃ") ("ニ" "ィ") ("ﾆ" "ｨ")))
    (("n" "y" "u") (("に" "ゅ") ("ニ" "ュ") ("ﾆ" "ｭ")))
    (("n" "y" "e") (("に" "ぇ") ("ニ" "ェ") ("ﾆ" "ｪ")))
    (("n" "y" "o") (("に" "ょ") ("ニ" "ョ") ("ﾆ" "ｮ")))

    ;; は行
    (("h" "a")     ("は" "ハ" "ﾊ"))
    (("h" "i")     ("ひ" "ヒ" "ﾋ"))
    (("h" "u")     ("ふ" "フ" "ﾌ"))
    (("h" "e")     ("へ" "ヘ" "ﾍ"))
    (("h" "o")     ("ほ" "ホ" "ﾎ"))

    (("h" "y" "a") (("ひ" "ゃ") ("ヒ" "ャ") ("ﾋ" "ｬ")))
    (("h" "y" "i") (("ひ" "ぃ") ("ヒ" "ィ") ("ﾋ" "ｨ")))
    (("h" "y" "u") (("ひ" "ゅ") ("ヒ" "ュ") ("ﾋ" "ｭ")))
    (("h" "y" "e") (("ひ" "ぇ") ("ヒ" "ェ") ("ﾋ" "ｪ")))
    (("h" "y" "o") (("ひ" "ょ") ("ヒ" "ョ") ("ﾋ" "ｮ")))

    (("f" "a")     (("ふ" "ぁ") ("フ" "ァ") ("ﾌ" "ｧ")))
    (("f" "i")     (("ふ" "ぃ") ("フ" "ィ") ("ﾌ" "ｨ")))
    (("f" "e")     (("ふ" "ぇ") ("フ" "ェ") ("ﾌ" "ｪ")))
    (("f" "o")     (("ふ" "ぉ") ("フ" "ォ") ("ﾌ" "ｫ")))

    (("b" "a")     ("ば" "バ" ("ﾊ" "ﾞ")))
    (("b" "i")     ("び" "ビ" ("ﾋ" "ﾞ")))
    (("b" "u")     ("ぶ" "ブ" ("ﾌ" "ﾞ")))
    (("b" "e")     ("べ" "ベ" ("ﾍ" "ﾞ")))
    (("b" "o")     ("ぼ" "ボ" ("ﾎ" "ﾞ")))

    (("b" "y" "a") (("び" "ゃ") ("ビ" "ャ") ("ﾋ" "ﾞ" "ｬ")))
    (("b" "y" "i") (("び" "ぃ") ("ビ" "ィ") ("ﾋ" "ﾞ" "ｨ")))
    (("b" "y" "u") (("び" "ゅ") ("ビ" "ュ") ("ﾋ" "ﾞ" "ｭ")))
    (("b" "y" "e") (("び" "ぇ") ("ビ" "ェ") ("ﾋ" "ﾞ" "ｪ")))
    (("b" "y" "o") (("び" "ょ") ("ビ" "ョ") ("ﾋ" "ﾞ" "ｮ")))

    (("p" "a")     ("ぱ" "パ" ("ﾊ" "ﾟ")))
    (("p" "i")     ("ぴ" "ピ" ("ﾋ" "ﾟ")))
    (("p" "u")     ("ぷ" "プ" ("ﾌ" "ﾟ")))
    (("p" "e")     ("ぺ" "ペ" ("ﾍ" "ﾟ")))
    (("p" "o")     ("ぽ" "ポ" ("ﾎ" "ﾟ")))

    (("p" "y" "a") (("ぴ" "ゃ") ("ピ" "ャ") ("ﾋ" "ﾟ" "ｬ")))
    (("p" "y" "i") (("ぴ" "ぃ") ("ピ" "ィ") ("ﾋ" "ﾟ" "ｨ")))
    (("p" "y" "u") (("ぴ" "ゅ") ("ピ" "ュ") ("ﾋ" "ﾟ" "ｭ")))
    (("p" "y" "e") (("ぴ" "ぇ") ("ピ" "ェ") ("ﾋ" "ﾟ" "ｪ")))
    (("p" "y" "o") (("ぴ" "ょ") ("ピ" "ョ") ("ﾋ" "ﾟ" "ｮ")))

    ;; ま行
    (("m" "a")     ("ま" "マ" "ﾏ"))
    (("m" "i")     ("み" "ミ" "ﾐ"))
    (("m" "u")     ("む" "ム" "ﾑ"))
    (("m" "e")     ("め" "メ" "ﾒ"))
    (("m" "o")     ("も" "モ" "ﾓ"))

    (("m" "y" "a") (("み" "ゃ") ("ミ" "ャ") ("ﾐ" "ｬ")))
    (("m" "y" "i") (("み" "ぃ") ("ミ" "ィ") ("ﾐ" "ｨ")))
    (("m" "y" "u") (("み" "ゅ") ("ミ" "ュ") ("ﾐ" "ｭ")))
    (("m" "y" "e") (("み" "ぇ") ("ミ" "ェ") ("ﾐ" "ｪ")))
    (("m" "y" "o") (("み" "ょ") ("ミ" "ョ") ("ﾐ" "ｮ")))

    ;; や行
    (("y" "a")     ("や" "ヤ" "ﾔ"))
    (("y" "i")     ("い" "イ" "ｲ"))  ;; rk does not contain this rule
    (("y" "u")     ("ゆ" "ユ" "ﾕ"))
    (("y" "e")     (("い" "ぇ") ("イ" "ェ") ("ｲ" "ｪ")))
    (("y" "o")     ("よ" "ヨ" "ﾖ"))

    ;; ら行
    (("r" "a")     ("ら" "ラ" "ﾗ"))
    (("r" "i")     ("り" "リ" "ﾘ"))
    (("r" "u")     ("る" "ル" "ﾙ"))
    (("r" "e")     ("れ" "レ" "ﾚ"))
    (("r" "o")     ("ろ" "ロ" "ﾛ"))

    (("r" "y" "a") (("り" "ゃ") ("リ" "ャ") ("ﾘ" "ｬ")))
    (("r" "y" "i") (("り" "ぃ") ("リ" "ィ") ("ﾘ" "ｨ")))
    (("r" "y" "u") (("り" "ゅ") ("リ" "ュ") ("ﾘ" "ｭ")))
    (("r" "y" "e") (("り" "ぇ") ("リ" "ェ") ("ﾘ" "ｪ")))
    (("r" "y" "o") (("り" "ょ") ("リ" "ョ") ("ﾘ" "ｮ")))

    ;; わ行
    (("w" "a")     ("わ" "ワ" "ﾜ"))
    (("w" "i")     (("う" "ぃ") ("ウ" "ィ") ("ｳ" "ｨ")))
    (("w" "u")     ("う" "ウ" "ｳ"))
    (("w" "e")     (("う" "ぇ") ("ウ" "ェ") ("ｳ" "ｪ")))
    (("w" "o")     ("を" "ヲ" "ｦ"))

    ;; う゛
    (("v" "a")     (("う" "゛" "ぁ") ("ヴ" "ァ") ("ｳ" "ﾞ" "ｧ")))
    (("v" "i")     (("う" "゛" "ぃ") ("ヴ" "ィ") ("ｳ" "ﾞ" "ｨ")))
    (("v" "u")     (("う" "゛") "ヴ" ("ｳ" "ﾞ")))
    (("v" "e")     (("う" "゛" "ぇ") ("ヴ" "ェ") ("ｳ" "ﾞ" "ｪ")))
    (("v" "o")     (("う" "゛" "ぉ") ("ヴ" "ォ") ("ｳ" "ﾞ" "ｫ")))

    (("v" "y" "a") (("う" "゛" "ゃ") ("ヴ" "ャ") ("ｳ" "ﾞ" "ｬ")))
    (("v" "y" "u") (("う" "゛" "ゅ") ("ヴ" "ュ") ("ｳ" "ﾞ" "ｭ")))
    (("v" "y" "o") (("う" "゛" "ょ") ("ヴ" "ョ") ("ｳ" "ﾞ" "ｮ")))

    ;; ん
    (("n" (char-nonvowel press peek loopback)) ("ん" "ン" "ﾝ"))
    ;; must be placed after above "nk" rule
    (("n" "n")                        ("ん" "ン" "ﾝ"))
;;    (("n" (char-nonvowel press peek)) (("ん" ($3 loopback))
;;				       ("ン" ($3 loopback))
;;				       ("ﾝ"  ($3 loopback))))

    ;; 記号
    (("-")         ("ー" "ー" "ｰ"))
    (("[")         ("「" "「" "｢"))
    (("]")         ("」" "」" "｣"))
    ))

(define ja-romaji-basic-double-consonant-ruleset
  (ja-romaji-generate-double-consonant-ruleset
   ja-romaji-basic-ruleset))

(define ja-romaji-x-prefixed-small-kana-ruleset
  '((("x" "a")         ("ぁ" "ァ" "ｧ"))
    (("x" "i")         ("ぃ" "ィ" "ｨ"))
    (("x" "u")         ("ぅ" "ゥ" "ｩ"))
    (("x" "e")         ("ぇ" "ェ" "ｪ"))
    (("x" "o")         ("ぉ" "ォ" "ｫ"))

    (("x" "y" "a")     ("ゃ" "ャ" "ｬ"))
    (("x" "y" "i")     ("ぃ" "ィ" "ｨ"))
    (("x" "y" "u")     ("ゅ" "ュ" "ｭ"))
    (("x" "y" "e")     ("ぇ" "ェ" "ｪ"))
    (("x" "y" "o")     ("ょ" "ョ" "ｮ"))

    (("x" "t" "u")     ("っ" "ッ" "ｯ"))
    (("x" "t" "s" "u") ("っ" "ッ" "ｯ"))))

(define ja-romaji-l-prefixed-small-kana-ruleset
  '((("l" "a")         ("ぁ" "ァ" "ｧ"))
    (("l" "i")         ("ぃ" "ィ" "ｨ"))
    (("l" "u")         ("ぅ" "ゥ" "ｩ"))
    (("l" "e")         ("ぇ" "ェ" "ｪ"))
    (("l" "o")         ("ぉ" "ォ" "ｫ"))

    ;; rk does not contain these rules
    (("l" "y" "a")     ("ゃ" "ャ" "ｬ"))
    (("l" "y" "i")     ("ぃ" "ィ" "ｨ"))
    (("l" "y" "u")     ("ゅ" "ュ" "ｭ"))
    (("l" "y" "e")     ("ぇ" "ェ" "ｪ"))
    (("l" "y" "o")     ("ょ" "ョ" "ｮ"))

    (("l" "t" "u")     ("っ" "ッ" "ｯ"))
    (("l" "t" "s" "u") ("っ" "ッ" "ｯ"))))

;; rk contains these rules
(define ja-romaji-minor-ruleset
  '(
    (("d" "s" "u") ("づ" "ヅ" ("ﾂ" "ﾞ")))

    (("x" "w" "a") ("ゎ" "ヮ" "ﾜ"))
    (("x" "w" "i") ("ゐ" "ヰ" "ｨ"))
    (("x" "w" "e") ("ゑ" "ヱ" "ｪ"))
		   
    ;; "一ヶ月", "10ヵ条"
    (("x" "c" "a") ("ヵ" "ヵ" ""))
    (("x" "k" "a") ("ヵ" "ヵ" ""))
    (("x" "k" "e") ("ヶ" "ヶ" ""))
    ))

;; 拗音
(define ja-romaji-minor-contracted-ruleset
  '(
    (("j" "y" "a") (("じ" "ゃ") ("ジ" "ャ") ("ｼ" "ﾞ" "ｬ")))
    (("j" "y" "i") (("じ" "ぃ") ("ジ" "ィ") ("ｼ" "ﾞ" "ｨ")))
    (("j" "y" "u") (("じ" "ゅ") ("ジ" "ュ") ("ｼ" "ﾞ" "ｭ")))
    (("j" "y" "e") (("じ" "ぇ") ("ジ" "ェ") ("ｼ" "ﾞ" "ｪ")))
    (("j" "y" "o") (("じ" "ょ") ("ジ" "ョ") ("ｼ" "ﾞ" "ｮ")))

    (("t" "s" "a") (("つ" "ぁ") ("ツ" "ァ") ("ﾂ" "ｧ")))
    (("t" "s" "i") (("つ" "ぃ") ("ツ" "ィ") ("ﾂ" "ｨ")))
    (("t" "s" "e") (("つ" "ぇ") ("ツ" "ェ") ("ﾂ" "ｪ")))
    (("t" "s" "o") (("つ" "ぉ") ("ツ" "ォ") ("ﾂ" "ｫ")))

    (("c" "y" "a") (("ち" "ゃ") ("チ" "ャ") ("ﾁ" "ｬ")))
    (("c" "y" "i") (("ち" "ぃ") ("チ" "ィ") ("ﾁ" "ｨ")))
    (("c" "y" "u") (("ち" "ゅ") ("チ" "ュ") ("ﾁ" "ｭ")))
    (("c" "y" "e") (("ち" "ぇ") ("チ" "ェ") ("ﾁ" "ｪ")))
    (("c" "y" "o") (("ち" "ょ") ("チ" "ョ") ("ﾁ" "ｮ")))

    (("f" "y" "a") (("ふ" "ゃ") ("フ" "ャ") ("ﾌ" "ｬ")))
    (("f" "y" "i") (("ふ" "ぃ") ("フ" "ィ") ("ﾌ" "ｨ")))
    (("f" "y" "u") (("ふ" "ゅ") ("フ" "ュ") ("ﾌ" "ｭ")))
    (("f" "y" "e") (("ふ" "ぇ") ("フ" "ェ") ("ﾌ" "ｪ")))
    (("f" "y" "o") (("ふ" "ょ") ("フ" "ョ") ("ﾌ" "ｮ")))

    ;; rk contains these rules. but it conflicts with
    ;; ja-romaji-l-prefixed-small-kana-ruleset
    ;;(("l" "y" "a") (("り" "ゃ") ("リ" "ャ") ("ﾘ" "ｬ")))
    ;;(("l" "y" "i") (("り" "ぃ") ("リ" "ィ") ("ﾘ" "ｨ")))
    ;;(("l" "y" "u") (("り" "ゅ") ("リ" "ュ") ("ﾘ" "ｭ")))
    ;;(("l" "y" "e") (("り" "ぇ") ("リ" "ェ") ("ﾘ" "ｪ")))
    ;;(("l" "y" "o") (("り" "ょ") ("リ" "ョ") ("ﾘ" "ｮ")))

    (("w" "h" "a") (("う" "ぁ") ("ウ" "ァ") ("ｳ" "ｧ")))
    (("w" "h" "i") (("う" "ぃ") ("ウ" "ィ") ("ｳ" "ｨ")))
    (("w" "h" "u") ("う" "ウ" "ｳ"))
    (("w" "h" "e") (("う" "ぇ") ("ウ" "ェ") ("ｳ" "ｪ")))
    (("w" "h" "o") (("う" "ぉ") ("ウ" "ォ") ("ｳ" "ｫ")))

    (("d" "h" "a") (("で" "ゃ") ("デ" "ャ") ("ﾃ" "ﾞ" "ｬ")))
    (("d" "h" "i") (("で" "ぃ") ("デ" "ィ") ("ﾃ" "ﾞ" "ｨ")))
    (("d" "h" "u") (("で" "ゅ") ("デ" "ュ") ("ﾃ" "ﾞ" "ｭ")))
    (("d" "h" "e") (("で" "ぇ") ("デ" "ェ") ("ﾃ" "ﾞ" "ｪ")))
    (("d" "h" "o") (("で" "ょ") ("デ" "ョ") ("ﾃ" "ﾞ" "ｮ")))

    (("d" "w" "a") (("ど" "ぁ") ("ド" "ァ") ("ﾄ" "ﾞ" "ｧ")))
    (("d" "w" "i") (("ど" "ぃ") ("ド" "ィ") ("ﾄ" "ﾞ" "ｨ")))
    (("d" "w" "u") (("ど" "ぅ") ("ド" "ゥ") ("ﾄ" "ﾞ" "ｩ")))
    (("d" "w" "e") (("ど" "ぇ") ("ド" "ェ") ("ﾄ" "ﾞ" "ｪ")))
    (("d" "w" "o") (("ど" "ぉ") ("ド" "ォ") ("ﾄ" "ﾞ" "ｫ")))

    (("k" "w" "a") (("く" "ぁ") ("ク" "ァ") ("ｸ" "ｧ")))
    (("k" "w" "i") (("く" "ぃ") ("ク" "ィ") ("ｸ" "ｨ")))
    (("k" "w" "u") (("く" "ぅ") ("ク" "ゥ") ("ｸ" "ｩ")))
    (("k" "w" "e") (("く" "ぇ") ("ク" "ェ") ("ｸ" "ｪ")))
    (("k" "w" "o") (("く" "ぉ") ("ク" "ォ") ("ｸ" "ｫ")))

    (("s" "w" "a") (("す" "ぁ") ("ス" "ァ") ("ｽ" "ｧ")))
    (("s" "w" "i") (("す" "ぃ") ("ス" "ィ") ("ｽ" "ｨ")))
    (("s" "w" "u") (("す" "ぅ") ("ス" "ゥ") ("ｽ" "ｩ")))
    (("s" "w" "e") (("す" "ぇ") ("ス" "ェ") ("ｽ" "ｪ")))
    (("s" "w" "o") (("す" "ぉ") ("ス" "ォ") ("ｽ" "ｫ")))

    (("t" "w" "a") (("と" "ぁ") ("ト" "ァ") ("ﾄ" "ｧ")))
    (("t" "w" "i") (("と" "ぃ") ("ト" "ィ") ("ﾄ" "ｨ")))
    (("t" "w" "u") (("と" "ぅ") ("ト" "ゥ") ("ﾄ" "ｩ")))
    (("t" "w" "e") (("と" "ぇ") ("ト" "ェ") ("ﾄ" "ｪ")))
    (("t" "w" "o") (("と" "ぉ") ("ト" "ォ") ("ﾄ" "ｫ")))

    (("t" "h" "a") (("て" "ゃ") ("テ" "ャ") ("ﾃ" "ｬ")))
    (("t" "h" "i") (("て" "ぃ") ("テ" "ィ") ("ﾃ" "ｨ")))
    (("t" "h" "u") (("て" "ゅ") ("テ" "ュ") ("ﾃ" "ｭ")))
    (("t" "h" "e") (("て" "ぇ") ("テ" "ェ") ("ﾃ" "ｪ")))
    (("t" "h" "o") (("て" "ょ") ("テ" "ョ") ("ﾃ" "ｮ")))

    (("h" "w" "a") (("ふ" "ぁ") ("フ" "ァ") ("ﾌ" "ｧ")))
    (("h" "w" "i") (("ふ" "ぃ") ("フ" "ィ") ("ﾌ" "ｨ")))
    (("h" "w" "e") (("ふ" "ぇ") ("フ" "ェ") ("ﾌ" "ｪ")))
    (("h" "w" "o") (("ふ" "ぉ") ("フ" "ォ") ("ﾌ" "ｫ")))

    (("f" "w" "a") (("ふ" "ぁ") ("フ" "ァ") ("ﾌ" "ｧ")))
    (("f" "w" "i") (("ふ" "ぃ") ("フ" "ィ") ("ﾌ" "ｨ")))
    (("f" "w" "u") (("ふ" "ぅ") ("フ" "ゥ") ("ﾌ" "ｩ")))
    (("f" "w" "e") (("ふ" "ぇ") ("フ" "ェ") ("ﾌ" "ｪ")))
    (("f" "w" "o") (("ふ" "ぉ") ("フ" "ォ") ("ﾌ" "ｫ")))

    (("q" "w" "a") (("く" "ぁ") ("ク" "ァ") ("ｸ" "ｧ")))
    (("q" "w" "i") (("く" "ぃ") ("ク" "ィ") ("ｸ" "ｨ")))
    (("q" "w" "u") (("く" "ぅ") ("ク" "ゥ") ("ｸ" "ｩ")))
    (("q" "w" "e") (("く" "ぇ") ("ク" "ェ") ("ｸ" "ｪ")))
    (("q" "w" "o") (("く" "ぉ") ("ク" "ォ") ("ｸ" "ｫ")))

    (("q" "y" "a") (("く" "ゃ") ("ク" "ャ") ("ｸ" "ｬ")))
    (("q" "y" "i") (("く" "ぃ") ("ク" "ィ") ("ｸ" "ｨ")))
    (("q" "y" "u") (("く" "ゅ") ("ク" "ュ") ("ｸ" "ｭ")))
    (("q" "y" "e") (("く" "ぇ") ("ク" "ェ") ("ｸ" "ｪ")))
    (("q" "y" "o") (("く" "ょ") ("ク" "ョ") ("ｸ" "ｮ")))

    (("g" "w" "a") (("ぐ" "ぁ") ("グ" "ァ") ("ｸ" "ﾞ" "ｧ")))
    (("g" "w" "i") (("ぐ" "ぃ") ("グ" "ィ") ("ｸ" "ﾞ" "ｨ")))
    (("g" "w" "u") (("ぐ" "ぅ") ("グ" "ゥ") ("ｸ" "ﾞ" "ｩ")))
    (("g" "w" "e") (("ぐ" "ぇ") ("グ" "ェ") ("ｸ" "ﾞ" "ｪ")))
    (("g" "w" "o") (("ぐ" "ぉ") ("グ" "ォ") ("ｸ" "ﾞ" "ｫ")))
    ))

(define ja-romaji-minor-contracted-double-consonant-ruleset
  (ja-romaji-generate-double-consonant-ruleset
   ja-romaji-minor-contracted-ruleset))

(define ja-romaji-skk-like-symbol-ruleset
  '((("z" "k") ("↑" "↑" ""))
    (("z" "j") ("↓" "↓" ""))
    (("z" "h") ("←" "←" ""))
    (("z" "l") ("→" "→" ""))
    (("z" "-") ("〜" "〜" ""))
    (("z" "[") ("『" "『" ""))
    (("z" "]") ("』" "』" ""))
    (("z" ",") ("‥" "‥" ("･" "･")))
    (("z" ".") ("…" "…" ("･" "･" "･")))
    (("z" "/") ("・" "・" "･"))))

;;
;; Hepburn style romaji: ヘボン式ローマ字
;;

(define ja-romaji-hepburn-ruleset
  '(
    (("s" "h" "a") (("し" "ゃ") ("シ" "ャ") ("ｼ" "ｬ")))
    (("s" "h" "i") ("し" "シ" "ｼ"))
    (("s" "h" "u") (("し" "ゅ") ("シ" "ュ") ("ｼ" "ｭ")))
    (("s" "h" "e") (("し" "ぇ") ("シ" "ェ") ("ｼ" "ｪ")))
    (("s" "h" "o") (("し" "ょ") ("シ" "ョ") ("ｼ" "ｮ")))
		   
    (("j" "a")     (("じ" "ゃ") ("ジ" "ャ") ("ｼ" "ﾞ" "ｬ")))
    (("j" "i")     ("じ" "ジ" ("ｼ" "ﾞ")))
    (("j" "u")     (("じ" "ゅ") ("ジ" "ュ") ("ｼ" "ﾞ" "ｭ")))
    (("j" "e")     (("じ" "ぇ") ("ジ" "ェ") ("ｼ" "ﾞ" "ｪ")))
    (("j" "o")     (("じ" "ょ") ("ジ" "ョ") ("ｼ" "ﾞ" "ｮ")))

    (("t" "s" "u") ("つ" "ツ" "ﾂ"))

    (("c" "h" "a") (("ち" "ゃ") ("チ" "ャ") ("ﾁ" "ｬ")))
    (("c" "h" "i") ("ち" "チ" "ﾁ"))
    (("c" "h" "u") (("ち" "ゅ") ("チ" "ュ") ("ﾁ" "ｭ")))
    (("c" "h" "e") (("ち" "ぇ") ("チ" "ェ") ("ﾁ" "ｪ")))
    (("c" "h" "o") (("ち" "ょ") ("チ" "ョ") ("ﾁ" "ｮ")))
		   
    (("f" "u")     ("ふ" "フ" "ﾌ"))
    ))

(define ja-romaji-hepburn-double-consonant-ruleset
  (ja-romaji-generate-double-consonant-ruleset
   ja-romaji-hepburn-ruleset))

;; ん
(define ja-romaji-hepburn-n-ruleset
  '(
    ;; "namba" → "なんば"
    (("m" ("b" press peek)) (("ん" ($3 loopback))
			     ("ン" ($3 loopback))
			     ("ﾝ"  ($3 loopback))))

    ;; "homma" → "ほんま"
    (("m" ("m" press peek)) (("ん" ($3 loopback))
			     ("ン" ($3 loopback))
			     ("ﾝ"  ($3 loopback))))

    ;; "kampo" → "かんぽ"
    (("m" ("p" press peek)) (("ん" ($3 loopback))
			     ("ン" ($3 loopback))
			     ("ﾝ"  ($3 loopback))))))

;; おお、おう (experimental)
(define ja-romaji-hepburn-oh-ruleset
  '((("o" "h"     (char-nonvowel press peek)) (("お" "お" ($5 loopback))
					       ("オ" "オ" ($5 loopback))
					       ("ｵ" "ｵ"   ($5 loopback))))
    (("k" "o" "h" (char-nonvowel press peek)) (("こ" "う" ($7 loopback))
					       ("コ" "ウ" ($7 loopback))
					       ("ｺ" "ｳ"   ($7 loopback))))
    (("g" "o" "h" (char-nonvowel press peek)) (("ご" "う" ($7 loopback))
					       ("ゴ" "ウ" ($7 loopback))
					       ("ｺ" "ﾞ" "ｳ" ($7 loopback))))
    (("s" "o" "h" (char-nonvowel press peek)) (("そ" "う" ($7 loopback))
					       ("ソ" "ウ" ($7 loopback))
					       ("ｿ" "ｳ"   ($7 loopback))))
    (("z" "o" "h" (char-nonvowel press peek)) (("ぞ" "う" ($7 loopback))
					       ("ゾ" "ウ" ($7 loopback))
					       ("ｿ" "ﾞ" "ｳ" ($7 loopback))))
    (("j" "o" "h" (char-nonvowel press peek)) (("じ" "ょ" "う" ($7 loopback))
					       ("ジ" "ョ" "ウ" ($7 loopback))
					       ("ｼ" "ﾞ" "ｮ" "ｳ" ($7 loopback))))
    (("t" "o" "h" (char-nonvowel press peek)) (("と" "う" ($7 loopback))
					       ("ト" "ウ" ($7 loopback))
					       ("ﾄ" "ｳ"   ($7 loopback))))
    (("d" "o" "h" (char-nonvowel press peek)) (("ど" "う" ($7 loopback))
					       ("ド" "ウ" ($7 loopback))
					       ("ﾄ" "ﾞ" "ｳ" ($7 loopback))))
    (("n" "o" "h" (char-nonvowel press peek)) (("の" "う" ($7 loopback))
					       ("ノ" "ウ" ($7 loopback))
					       ("ﾉ" "ｳ"   ($7 loopback))))
    (("h" "o" "h" (char-nonvowel press peek)) (("ほ" "う" ($7 loopback))
					       ("ホ" "ウ" ($7 loopback))
					       ("ﾎ" "ｳ"   ($7 loopback))))
;;    (("f" "o" "h" (char-nonvowel press peek)) (("ふ" "ぉ" "う" ($7 loopback))
;;					       ("フ" "ォ" "ウ" ($7 loopback))
;;					       ("ﾌ" "ｫ" "ｳ"    ($7 loopback))))
    (("b" "o" "h" (char-nonvowel press peek)) (("ぼ" "う" ($7 loopback))
					       ("ボ" "ウ" ($7 loopback))
					       ("ﾎ" "ﾞ" "ｳ" ($7 loopback))))
    (("p" "o" "h" (char-nonvowel press peek)) (("ぽ" "う" ($7 loopback))
					       ("ポ" "ウ" ($7 loopback))
					       ("ﾎ" "ﾟ" "ｳ" ($7 loopback))))
    (("m" "o" "h" (char-nonvowel press peek)) (("も" "う" ($7 loopback))
					       ("モ" "ウ" ($7 loopback))
					       ("ﾓ" "ｳ"   ($7 loopback))))
    (("y" "o" "h" (char-nonvowel press peek)) (("よ" "う" ($7 loopback))
					       ("ヨ" "ウ" ($7 loopback))
					       ("ﾖ" "ｳ"   ($7 loopback))))
    (("r" "o" "h" (char-nonvowel press peek)) (("ろ" "う" ($7 loopback))
					       ("ロ" "ウ" ($7 loopback))
					       ("ﾛ" "ｳ"   ($7 loopback))))))

;; 変則促音 変換中表示
(define ja-romaji-hepburn-irregular-double-consonant-guide-ruleset
  '((("t" "c")         (("っ" "c") ("ッ" "c") ("ｯ" "c")))))

;; 変則促音
(define ja-romaji-hepburn-irregular-double-consonant-ruleset
  '((("t" "c" "h" "a") (("っ" "ち" "ゃ") ("ッ" "チ" "ャ") ("ｯ" "ﾁ" "ｬ")))
    (("t" "c" "h" "i") (("っ" "ち" "ぃ") ("ッ" "チ" "ィ") ("ｯ" "ﾁ" "ｨ")))
    (("t" "c" "h" "u") (("っ" "ち" "ゅ") ("ッ" "チ" "ュ") ("ｯ" "ﾁ" "ｭ")))
    ;;(("t" "c" "h" "e") (("っ" "ち" "ぇ") ("ッ" "チ" "ェ") ("ｯ" "ﾁ" "ｪ")))
    (("t" "c" "h" "o") (("っ" "ち" "ょ") ("ッ" "チ" "ョ") ("ｯ" "ﾁ" "ｮ")))))


;; separate romaji rulesets into 3 parts dedicated for hiragana,
;; katakana and halfkana
(define ja-romaji-ruleset-name-list
  '(double-consonant-guide
    basic
    basic-double-consonant
    x-prefixed-small-kana
    l-prefixed-small-kana
    minor
    minor-contracted
    minor-contracted-double-consonant
    skk-like-symbol
    hepburn
    hepburn-double-consonant
    hepburn-n
    hepburn-oh
    hepburn-irregular-double-consonant-guide
    hepburn-irregular-double-consonant))

(ja-define-dedicated-rulesets 'ja-romaji ja-romaji-ruleset-name-list)
		      

;; may be replaced with more efficient way for ruleset composition(merging)
(define ja-romaji-hiragana-ruleset
  (append
   ja-fullwidth-space-ruleset
   ja-fullwidth-kana-period-ruleset
   ja-fullwidth-kana-comma-ruleset
   ja-fullwidth-basic-symbol-ruleset
   ja-fullwidth-number-ruleset
   ja-fullwidth-alphabet-ruleset
   ja-romaji-hiragana-basic-ruleset
   ja-romaji-hiragana-double-consonant-guide-ruleset
   ja-romaji-hiragana-basic-double-consonant-ruleset
   ja-romaji-hiragana-x-prefixed-small-kana-ruleset
   ja-romaji-hiragana-l-prefixed-small-kana-ruleset
   ja-romaji-hiragana-minor-ruleset
   ja-romaji-hiragana-minor-contracted-ruleset
   ja-romaji-hiragana-minor-contracted-double-consonant-ruleset
   ja-romaji-hiragana-hepburn-ruleset
   ja-romaji-hiragana-hepburn-double-consonant-ruleset
   ja-romaji-hiragana-hepburn-n-ruleset
   ja-romaji-hiragana-hepburn-oh-ruleset
   ja-romaji-hiragana-hepburn-irregular-double-consonant-guide-ruleset
   ja-romaji-hiragana-hepburn-irregular-double-consonant-ruleset
   ja-romaji-hiragana-skk-like-symbol-ruleset))

(define ja-romaji-katakana-ruleset
  (append
   ja-fullwidth-space-ruleset
   ja-fullwidth-kana-period-ruleset
   ja-fullwidth-kana-comma-ruleset
   ja-fullwidth-basic-symbol-ruleset
   ja-fullwidth-number-ruleset
   ja-fullwidth-alphabet-ruleset
   ja-romaji-katakana-basic-ruleset
   ja-romaji-katakana-double-consonant-guide-ruleset
   ja-romaji-katakana-basic-double-consonant-ruleset
   ja-romaji-katakana-x-prefixed-small-kana-ruleset
   ja-romaji-katakana-l-prefixed-small-kana-ruleset
   ja-romaji-katakana-minor-ruleset
   ja-romaji-katakana-minor-contracted-ruleset
   ja-romaji-katakana-minor-contracted-double-consonant-ruleset
   ja-romaji-katakana-hepburn-ruleset
   ja-romaji-katakana-hepburn-double-consonant-ruleset
   ja-romaji-katakana-hepburn-n-ruleset
   ja-romaji-katakana-hepburn-oh-ruleset
   ja-romaji-katakana-hepburn-irregular-double-consonant-guide-ruleset
   ja-romaji-katakana-hepburn-irregular-double-consonant-ruleset
   ja-romaji-katakana-skk-like-symbol-ruleset))

(define ja-romaji-halfkana-ruleset
  (append
   ja-halfwidth-space-ruleset
   ja-halfwidth-kana-period-ruleset
   ja-halfwidth-kana-comma-ruleset
   ja-halfwidth-basic-symbol-ruleset
   ja-halfwidth-number-ruleset
   ja-halfwidth-alphabet-ruleset
   ja-romaji-halfkana-basic-ruleset
   ja-romaji-halfkana-double-consonant-guide-ruleset
   ja-romaji-halfkana-basic-double-consonant-ruleset
   ja-romaji-halfkana-x-prefixed-small-kana-ruleset
   ja-romaji-halfkana-l-prefixed-small-kana-ruleset
   ja-romaji-halfkana-minor-ruleset
   ja-romaji-halfkana-minor-contracted-ruleset
   ja-romaji-halfkana-minor-contracted-double-consonant-ruleset
   ja-romaji-halfkana-hepburn-ruleset
   ja-romaji-halfkana-hepburn-double-consonant-ruleset
   ja-romaji-halfkana-hepburn-n-ruleset
   ja-romaji-halfkana-hepburn-oh-ruleset
   ja-romaji-halfkana-hepburn-irregular-double-consonant-guide-ruleset
   ja-romaji-halfkana-hepburn-irregular-double-consonant-ruleset
   ja-romaji-halfkana-skk-like-symbol-ruleset))

(define ja-romaji-hiragana-ruletree
  (evmap-parse-ruleset ja-romaji-hiragana-ruleset))

(define ja-romaji-katakana-ruletree
  (evmap-parse-ruleset ja-romaji-katakana-ruleset))

(define ja-romaji-halfkana-ruletree
  (evmap-parse-ruleset ja-romaji-halfkana-ruleset))
