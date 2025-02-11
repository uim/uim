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

;; UTF-8

(require-extension (srfi 1 2))
(require-custom "japanese-custom-utf8.scm")
(require "util.scm")

(define ja-rk-rule-additional
  '(
    ((("d" "s" "u"). ())("づ" "ヅ" "ﾂﾞ"))

    ((("d" "h" "a"). ())(("で" "デ" "ﾃﾞ") ("ゃ" "ャ" "ｬ")))
    ((("d" "h" "i"). ())(("で" "デ" "ﾃﾞ") ("ぃ" "ィ" "ｨ")))
    ((("d" "h" "u"). ())(("で" "デ" "ﾃﾞ") ("ゅ" "ュ" "ｭ")))
    ((("d" "h" "e"). ())(("で" "デ" "ﾃﾞ") ("ぇ" "ェ" "ｪ")))
    ((("d" "h" "o"). ())(("で" "デ" "ﾃﾞ") ("ょ" "ョ" "ｮ")))

    ((("d" "w" "a"). ())(("ど" "ド" "ﾄﾞ") ("ぁ" "ァ" "ｧ")))
    ((("d" "w" "i"). ())(("ど" "ド" "ﾄﾞ") ("ぃ" "ィ" "ｨ")))
    ((("d" "w" "u"). ())(("ど" "ド" "ﾄﾞ") ("ぅ" "ゥ" "ｩ")))
    ((("d" "w" "e"). ())(("ど" "ド" "ﾄﾞ") ("ぇ" "ェ" "ｪ")))
    ((("d" "w" "o"). ())(("ど" "ド" "ﾄﾞ") ("ぉ" "ォ" "ｫ")))

    ((("k" "w" "a"). ())(("く" "ク" "ｸ") ("ぁ" "ァ" "ｧ")))
    ((("k" "w" "i"). ())(("く" "ク" "ｸ") ("ぃ" "ィ" "ｨ")))
    ((("k" "w" "u"). ())(("く" "ク" "ｸ") ("ぅ" "ゥ" "ｩ")))
    ((("k" "w" "e"). ())(("く" "ク" "ｸ") ("ぇ" "ェ" "ｪ")))
    ((("k" "w" "o"). ())(("く" "ク" "ｸ") ("ぉ" "ォ" "ｫ")))

    ((("s" "h" "a"). ())(("し" "シ" "ｼ") ("ゃ" "ャ" "ｬ")))
    ((("s" "h" "i"). ())("し" "シ" "ｼ"))
    ((("s" "h" "u"). ())(("し" "シ" "ｼ") ("ゅ" "ュ" "ｭ")))
    ((("s" "h" "e"). ())(("し" "シ" "ｼ") ("ぇ" "ェ" "ｪ")))
    ((("s" "h" "o"). ())(("し" "シ" "ｼ") ("ょ" "ョ" "ｮ")))

    ((("s" "w" "a"). ())(("す" "ス" "ｽ") ("ぁ" "ァ" "ｧ")))
    ((("s" "w" "i"). ())(("す" "ス" "ｽ") ("ぃ" "ィ" "ｨ")))
    ((("s" "w" "u"). ())(("す" "ス" "ｽ") ("ぅ" "ゥ" "ｩ")))
    ((("s" "w" "e"). ())(("す" "ス" "ｽ") ("ぇ" "ェ" "ｪ")))
    ((("s" "w" "o"). ())(("す" "ス" "ｽ") ("ぉ" "ォ" "ｫ")))

    ((("t" "w" "a"). ())(("と" "ト" "ﾄ") ("ぁ" "ァ" "ｧ")))
    ((("t" "w" "i"). ())(("と" "ト" "ﾄ") ("ぃ" "ィ" "ｨ")))
    ((("t" "w" "u"). ())(("と" "ト" "ﾄ") ("ぅ" "ゥ" "ｩ")))
    ((("t" "w" "e"). ())(("と" "ト" "ﾄ") ("ぇ" "ェ" "ｪ")))
    ((("t" "w" "o"). ())(("と" "ト" "ﾄ") ("ぉ" "ォ" "ｫ")))

    ((("t" "h" "a"). ())(("て" "テ" "ﾃ") ("ゃ" "ャ" "ｬ")))
    ((("t" "h" "i"). ())(("て" "テ" "ﾃ") ("ぃ" "ィ" "ｨ")))
    ((("t" "h" "u"). ())(("て" "テ" "ﾃ") ("ゅ" "ュ" "ｭ")))
    ((("t" "h" "e"). ())(("て" "テ" "ﾃ") ("ぇ" "ェ" "ｪ")))
    ((("t" "h" "o"). ())(("て" "テ" "ﾃ") ("ょ" "ョ" "ｮ")))

    ((("h" "w" "a"). ())(("ふ" "フ" "ﾌ") ("ぁ" "ァ" "ｧ")))
    ((("h" "w" "i"). ())(("ふ" "フ" "ﾌ") ("ぃ" "ィ" "ｨ")))
    ((("h" "w" "e"). ())(("ふ" "フ" "ﾌ") ("ぇ" "ェ" "ｪ")))
    ((("h" "w" "o"). ())(("ふ" "フ" "ﾌ") ("ぉ" "ォ" "ｫ")))

    ((("f" "w" "a"). ())(("ふ" "フ" "ﾌ") ("ぁ" "ァ" "ｧ")))
    ((("f" "w" "i"). ())(("ふ" "フ" "ﾌ") ("ぃ" "ィ" "ｨ")))
    ((("f" "w" "u"). ())(("ふ" "フ" "ﾌ") ("ぅ" "ゥ" "ｩ")))
    ((("f" "w" "e"). ())(("ふ" "フ" "ﾌ") ("ぇ" "ェ" "ｪ")))
    ((("f" "w" "o"). ())(("ふ" "フ" "ﾌ") ("ぉ" "ォ" "ｫ")))

    ((("x" "w" "a"). ())("ゎ" "ヮ" "ﾜ"))
    ((("x" "w" "i"). ())("ゐ" "ヰ" "ｨ"))
    ((("x" "w" "e"). ())("ゑ" "ヱ" "ｪ"))

    ((("w" "y" "i"). ())("ゐ" "ヰ" "ｨ"))
    ((("w" "y" "e"). ())("ゑ" "ヱ" "ｪ"))

    ((("c" "h" "a"). ())(("ち" "チ" "ﾁ") ("ゃ" "ャ" "ｬ")))
    ((("c" "h" "i"). ())("ち" "チ" "ﾁ"))
    ((("c" "h" "u"). ())(("ち" "チ" "ﾁ") ("ゅ" "ュ" "ｭ")))
    ((("c" "h" "e"). ())(("ち" "チ" "ﾁ") ("ぇ" "ェ" "ｪ")))
    ((("c" "h" "o"). ())(("ち" "チ" "ﾁ") ("ょ" "ョ" "ｮ")))

    ((("q" "w" "a"). ())(("く" "ク" "ｸ") ("ぁ" "ァ" "ｧ")))
    ((("q" "w" "i"). ())(("く" "ク" "ｸ") ("ぃ" "ィ" "ｨ")))
    ((("q" "w" "u"). ())(("く" "ク" "ｸ") ("ぅ" "ゥ" "ｩ")))
    ((("q" "w" "e"). ())(("く" "ク" "ｸ") ("ぇ" "ェ" "ｪ")))
    ((("q" "w" "o"). ())(("く" "ク" "ｸ") ("ぉ" "ォ" "ｫ")))

    ((("q" "y" "a"). ())(("く" "ク" "ｸ") ("ゃ" "ャ" "ｬ")))
    ((("q" "y" "i"). ())(("く" "ク" "ｸ") ("ぃ" "ィ" "ｨ")))
    ((("q" "y" "u"). ())(("く" "ク" "ｸ") ("ゅ" "ュ" "ｭ")))
    ((("q" "y" "e"). ())(("く" "ク" "ｸ") ("ぇ" "ェ" "ｪ")))
    ((("q" "y" "o"). ())(("く" "ク" "ｸ") ("ょ" "ョ" "ｮ")))

    ((("g" "w" "a"). ())(("ぐ" "グ" "ｸﾞ") ("ぁ" "ァ" "ｧ")))
    ((("g" "w" "i"). ())(("ぐ" "グ" "ｸﾞ") ("ぃ" "ィ" "ｨ")))
    ((("g" "w" "u"). ())(("ぐ" "グ" "ｸﾞ") ("ぅ" "ゥ" "ｩ")))
    ((("g" "w" "e"). ())(("ぐ" "グ" "ｸﾞ") ("ぇ" "ェ" "ｪ")))
    ((("g" "w" "o"). ())(("ぐ" "グ" "ｸﾞ") ("ぉ" "ォ" "ｫ")))

    ((("z" "w" "a"). ())(("ず" "ズ" "ｽﾞ") ("ぁ" "ァ" "ｧ")))
    ((("z" "w" "i"). ())(("ず" "ズ" "ｽﾞ") ("ぃ" "ィ" "ｨ")))
    ((("z" "w" "u"). ())(("ず" "ズ" "ｽﾞ") ("ぅ" "ゥ" "ｩ")))
    ((("z" "w" "e"). ())(("ず" "ズ" "ｽﾞ") ("ぇ" "ェ" "ｪ")))
    ((("z" "w" "o"). ())(("ず" "ズ" "ｽﾞ") ("ぉ" "ォ" "ｫ")))

    ;((("n" "w" "a"). ())(("ぬ" "ヌ" "ﾇ") ("ぁ" "ァ" "ｧ")))
    ;((("n" "w" "i"). ())(("ぬ" "ヌ" "ﾇ") ("ぃ" "ィ" "ｨ")))
    ;((("n" "w" "u"). ())(("ぬ" "ヌ" "ﾇ") ("ぅ" "ゥ" "ｩ")))
    ;((("n" "w" "e"). ())(("ぬ" "ヌ" "ﾇ") ("ぇ" "ェ" "ｪ")))
    ;((("n" "w" "o"). ())(("ぬ" "ヌ" "ﾇ") ("ぉ" "ォ" "ｫ")))

    ((("b" "w" "a"). ())(("ぶ" "ブ" "ﾌﾞ") ("ぁ" "ァ" "ｧ")))
    ((("b" "w" "i"). ())(("ぶ" "ブ" "ﾌﾞ") ("ぃ" "ィ" "ｨ")))
    ((("b" "w" "u"). ())(("ぶ" "ブ" "ﾌﾞ") ("ぅ" "ゥ" "ｩ")))
    ((("b" "w" "e"). ())(("ぶ" "ブ" "ﾌﾞ") ("ぇ" "ェ" "ｪ")))
    ((("b" "w" "o"). ())(("ぶ" "ブ" "ﾌﾞ") ("ぉ" "ォ" "ｫ")))

    ((("p" "w" "a"). ())(("ぷ" "プ" "ﾌﾟ") ("ぁ" "ァ" "ｧ")))
    ((("p" "w" "i"). ())(("ぷ" "プ" "ﾌﾟ") ("ぃ" "ィ" "ｨ")))
    ((("p" "w" "u"). ())(("ぷ" "プ" "ﾌﾟ") ("ぅ" "ゥ" "ｩ")))
    ((("p" "w" "e"). ())(("ぷ" "プ" "ﾌﾟ") ("ぇ" "ェ" "ｪ")))
    ((("p" "w" "o"). ())(("ぷ" "プ" "ﾌﾟ") ("ぉ" "ォ" "ｫ")))

    ((("m" "w" "a"). ())(("む" "ム" "ﾑ") ("ぁ" "ァ" "ｧ")))
    ((("m" "w" "i"). ())(("む" "ム" "ﾑ") ("ぃ" "ィ" "ｨ")))
    ((("m" "w" "u"). ())(("む" "ム" "ﾑ") ("ぅ" "ゥ" "ｩ")))
    ((("m" "w" "e"). ())(("む" "ム" "ﾑ") ("ぇ" "ェ" "ｪ")))
    ((("m" "w" "o"). ())(("む" "ム" "ﾑ") ("ぉ" "ォ" "ｫ")))

    ((("y" "w" "a"). ())(("ゆ" "ユ" "ﾕ") ("ぁ" "ァ" "ｧ")))
    ((("y" "w" "i"). ())(("ゆ" "ユ" "ﾕ") ("ぃ" "ィ" "ｨ")))
    ((("y" "w" "u"). ())(("ゆ" "ユ" "ﾕ") ("ぅ" "ゥ" "ｩ")))
    ((("y" "w" "e"). ())(("ゆ" "ユ" "ﾕ") ("ぇ" "ェ" "ｪ")))
    ((("y" "w" "o"). ())(("ゆ" "ユ" "ﾕ") ("ぉ" "ォ" "ｫ")))

    ((("r" "w" "a"). ())(("る" "ル" "ﾙ") ("ぁ" "ァ" "ｧ")))
    ((("r" "w" "i"). ())(("る" "ル" "ﾙ") ("ぃ" "ィ" "ｨ")))
    ((("r" "w" "u"). ())(("る" "ル" "ﾙ") ("ぅ" "ゥ" "ｩ")))
    ((("r" "w" "e"). ())(("る" "ル" "ﾙ") ("ぇ" "ェ" "ｪ")))
    ((("r" "w" "o"). ())(("る" "ル" "ﾙ") ("ぉ" "ォ" "ｫ")))

    ((("d" "'" "i"). ())(("で" "デ" "ﾃﾞ") ("ぃ" "ィ" "ｨ")))
    ((("d" "'" "y" "u"). ())(("で" "デ" "ﾃﾞ") ("ゅ" "ュ" "ｭ")))

    ((("d" "'" "u"). ())(("ど" "ド" "ﾄﾞ") ("ぅ" "ゥ" "ｩ")))

    ((("t" "'" "i"). ())(("て" "テ" "ﾃ") ("ぃ" "ィ" "ｨ")))
    ((("t" "'" "y" "u"). ())(("て" "テ" "ﾃ") ("ゅ" "ュ" "ｭ")))

    ((("t" "'" "u"). ())(("と" "ト" "ﾄ") ("ぅ" "ゥ" "ｩ")))

    ))

(define ja-rk-rule (append ja-rk-rule-basic ja-rk-rule-additional))

(define ja-wide-rule
  '(("a" "ａ")
    ("b" "ｂ")
    ("c" "ｃ")
    ("d" "ｄ")
    ("e" "ｅ")
    ("f" "ｆ")
    ("g" "ｇ")
    ("h" "ｈ")
    ("i" "ｉ")
    ("j" "ｊ")
    ("k" "ｋ")
    ("l" "ｌ")
    ("m" "ｍ")
    ("n" "ｎ")
    ("o" "ｏ")
    ("p" "ｐ")
    ("q" "ｑ")
    ("r" "ｒ")
    ("s" "ｓ")
    ("t" "ｔ")
    ("u" "ｕ")
    ("v" "ｖ")
    ("w" "ｗ")
    ("x" "ｘ")
    ("y" "ｙ")
    ("z" "ｚ")
    ("A" "Ａ")
    ("B" "Ｂ")
    ("C" "Ｃ")
    ("D" "Ｄ")
    ("E" "Ｅ")
    ("F" "Ｆ")
    ("G" "Ｇ")
    ("H" "Ｈ")
    ("I" "Ｉ")
    ("J" "Ｊ")
    ("K" "Ｋ")
    ("L" "Ｌ")
    ("M" "Ｍ")
    ("N" "Ｎ")
    ("O" "Ｏ")
    ("P" "Ｐ")
    ("Q" "Ｑ")
    ("R" "Ｒ")
    ("S" "Ｓ")
    ("T" "Ｔ")
    ("U" "Ｕ")
    ("V" "Ｖ")
    ("W" "Ｗ")
    ("X" "Ｘ")
    ("Y" "Ｙ")
    ("Z" "Ｚ")

    ("1" "１")
    ("2" "２")
    ("3" "３")
    ("4" "４")
    ("5" "５")
    ("6" "６")
    ("7" "７")
    ("8" "８")
    ("9" "９")
    ("0" "０")

    ("-" "−")
    ("," "，")
    ("." "．")
    ("!" "！")
    ("\"" "”")
    ("#" "＃")
    ("$" "＄")
    ("%" "％")
    ("&" "＆")
    ("'" "’")
    ("(" "（")
    (")" "）")
    ("~" "〜")
    ("=" "＝")
    ("^" "＾")
    ("\\" "＼")
    ("yen" "￥")
    ("|" "｜")
    ("`" "‘")
    ("@" "＠")
    ("{" "｛")
    ("[" "［")
    ("+" "＋")
    (";" "；")
    ("*" "＊")
    (":" "：")
    ("}" "｝")
    ("]" "］")
    ("<" "＜")
    (">" "＞")
    ("?" "？")
    ("/" "／")
    ("_"  "＿")
    (" " "　")
    ))

;;
;; 2004-08-30 Takuro Ashie <ashie@homa.ne.jp>
;;
;;   It's a ad-hoc way to detect vowel and consonant in roma string.
;;   FIXME!
;;
(define ja-vowel-table
 '(("a" "a")
   ("i" "i")
   ("u" "u")
   ("e" "e")
   ("o" "o")
    ))

(define ja-consonant-syllable-table
 '(("b" "")
   ("c" "")
   ("d" "")
   ("f" "fa")
   ("g" "")
   ("h" "")
   ("j" "ji")
   ("k" "")
   ("l" "")
   ("m" "")
   ("n" "nn")
   ("p" "")
   ("q" "")
   ("r" "")
   ("s" "")
   ("t" "")
   ("v" "vu")
   ("w" "wu")
   ("x" "")
   ("y" "")
   ("z" "")
   ("ky" "ki")
   ("gy" "gi")
   ("sy" "si")
   ("zy" "zi")
   ("jy" "ji")
   ("ty" "ti")
   ("ts" "tu")
   ("cy" "ti")
   ("dy" "di")
   ("ny" "ni")
   ("hy" "hi")
   ("fy" "fu")
   ("by" "bi")
   ("py" "pi")
   ("my" "mi")
   ("ly" "li")
   ("ry" "ri")
   ("wh" "wu")
   ("ds" "du")
   ("dh" "de")
   ("dw" "do")
   ("kw" "ku")
   ("sh" "si")
   ("sw" "su")
   ("tw" "to")
   ("th" "te")
   ("hw" "hu")
   ("fw" "fu")
   ("xw" "wa")
   ("wy" "wi")
   ("ch" "ti")
   ("qw" "ku")
   ("qy" "ku")
   ("gw" "gu")
   ("zw" "zu")
   ("bw" "bu")
   ("pw" "pu")
   ("mw" "mu")
   ("yw" "yu")
   ("rw" "ru")
   ))

(define ja-default-small-tsu-roma "ltu")

;; "ja-direct-rule" seems to be used to commit a character immediately
;; even when japanese-context (i.e. preedit mode) is on.  I don't think the
;; rule is needed normally.  So I leave it null by default.  -- ekato
(define ja-direct-rule
  '(
    ))

;; space on (hiragana katakana halfkana) input mode
(define ja-space
  '("　" "　" " "))

;; space on (halfwidth-alnum fullwidth-alnum) input mode
(define ja-alnum-space
  '(" " "　"))

;;
(define ja-find-rec
  (lambda (c rule)
    (if (null? rule)
	#f
	(let ((r (car rule)))
	  (if (string=? c (car r))
	      (cadr r)
	      (ja-find-rec c (cdr rule)))))))

(define ja-wide
  (lambda (c)
    (or (ja-find-rec c ja-wide-rule)
        c)))

(define ja-direct
  (lambda (c)
    (ja-find-rec c ja-direct-rule)))

(define ja-vowel
  (lambda (c)
    (ja-find-rec c ja-vowel-table)))

(define ja-consonant-to-syllable
  (lambda (c)
    (ja-find-rec c ja-consonant-syllable-table)))

;;
;; 2004-08-30 Takuro Ashie <ashie@homa.ne.jp>
;;
;; ja-string-list-to-wide-alphabet
;;
;;   Convert alphabets in string list to wide alphabets.
;;   This procedure is ad-hoc. Maybe more generalize is needed.
;;
(define ja-string-list-to-wide-alphabet
  (lambda (char-list)
    (if (not (null? char-list))
        (string-append (ja-string-list-to-wide-alphabet (cdr char-list))
                       (ja-wide (car char-list)))
        "")))


;; Convert a invalid roma consonant at the end of string-list to a valid roma
;; consonant or valid roma string.
;;
;; "Invalid roma string-list" will be generated while editing a preedit string:
;;
;;     Convert a "n" which is followed by a vowel to "nn":
;;       1. at first, type a following string:
;;          ("ka" "n" "ki")
;;       2: press backspace (or move the cursor):
;;          ("ka" "n")
;;       3. type a vowel:
;;          ("ka" "n" "i")
;;       4. On this case, this procedure converts the list to:
;;          ("ka" "nn" "i")
;;
;;     Fix a broken "っ":
;;       1.  at fisrt, type a following string:
;;             ("a" "t" "ti")
;;       2.  press backspace (or move the cursor):
;;             ("a" "t")
;;       3.  type remaining strings:
;;             ("a" "t" "ka" "nn" "be" "-")
;;      (3'. On this case, this procedure converts the list to:
;;             ("a" "t") -> ("a" "ltu"))
;;       4.  On this case, this procedure converts the list to:
;;             ("a" "k" "ka" "nn" "be" "-")
(define ja-fix-deleted-raw-str-to-valid-roma!
  (lambda (raw-str)
    (if (not (null? (car raw-str)))
	(let ((lst (car raw-str)))
	  (if (ja-consonant-to-syllable (car lst))
	      (if (= (string-length (ja-consonant-to-syllable (car lst))) 2)
		  (set-car! lst (ja-consonant-to-syllable (car lst)))
		  (set-car! lst ja-default-small-tsu-roma)))))))

;; not sure this is the good place and the procedure is well written...
(define (list-seq-contained? large small)
  (define (list-seq-partial-equal-internal ll sl n)
    (let ((len (length sl)))
      (if (> len (length ll))
	  #f
	  (if (= len (length (filter-map equal? ll sl)))
	      n
	      (list-seq-partial-equal-internal (cdr ll) sl (+ n 1))))))
  (if (null? small)
      #f
      (list-seq-partial-equal-internal large small 0)))

;; revise string list contains "う゛"
;; (("゛") ("う")) -> ("う゛")
(define ja-join-vu
  (lambda (lst)
    (let ((sub (member "゛" lst)))
      (if (and
	   sub
	   (not (null? (cdr sub)))
	   (string=? (car (cdr sub)) "う"))
	  (append
	   (list-head lst (- (length lst) (length sub)))
	   '("う゛")
	   (ja-join-vu (list-tail lst (+ (- (length lst) (length sub)) 2))))
	  (if (and
	       sub
	       (member "゛" (cdr sub)))
	      (append
	       (list-head lst (+ (- (length lst) (length sub)) 1))
	       (ja-join-vu (cdr sub)))
	      lst)))))

;; get ("あ" "ア" "ｱ") from "あ"
(define ja-find-kana-list-from-rule
  (lambda (rule str)
    (if (not (null? rule))
	(if (pair? (member str (car (cdr (car rule)))))
	    (car (cdr (car rule)))
	    (ja-find-kana-list-from-rule (cdr rule) str))
        (if (string=?  str "゛")
	    (list "゛" "゛" "ﾞ")
	    (list str str str)))))

;; (("じ" "ジ" "ｼﾞ") ("ん" "ン" "ﾝ") ("か" "カ" "ｶ")) from ("じ" "ん" "か")
(define ja-make-kana-str-list
  (lambda (sl)
    (if (not (null? sl))
	(append (list (ja-find-kana-list-from-rule ja-rk-rule-basic (car sl)))
		(ja-make-kana-str-list (cdr sl)))
	'())))

(define ja-type-direct	       -1)
(define ja-type-hiragana	0)
(define ja-type-katakana	1)
(define ja-type-halfkana	2)
(define ja-type-halfwidth-alnum	3)
(define ja-type-fullwidth-alnum	4)

(define ja-opposite-kana
  (lambda (kana)
    (cond
     ((= kana ja-type-hiragana)
      ja-type-katakana)
     ((= kana ja-type-katakana)
      ja-type-hiragana)
     ((= kana ja-type-halfkana)
      ja-type-hiragana))))

;; getting required type of kana string from above kana-str-list
;; (ja-make-kana-str
;;  (("じ" "ジ" "ｼﾞ") ("ん" "ン" "ﾝ") ("か" "カ" "ｶ"))
;;  ja-type-katakana)
;;  -> "カンジ"
(define ja-make-kana-str
  (lambda (sl type)
    (let ((get-str-by-type
	   (lambda (l)
	     (cond
	      ((= type ja-type-hiragana)
	       (caar l))
	      ((= type ja-type-katakana)
	       (car (cdar l)))
	      ((= type ja-type-halfkana)
	       (cadr (cdar l)))))))
      (if (not (null? sl))
	  (string-append (ja-make-kana-str (cdr sl) type)
			 (get-str-by-type sl))
	  ""))))

(define japanese-roma-set-yen-representation
  (lambda ()
    ;; Since ordinary Japanese users press the "yen sign" key on
    ;; Japanese keyboard in alphanumeric-mode "to input character code
    ;; 134" rather than "to input yen sign symbol", I changed the
    ;; fullwidth yen sign with backslash.  -- YamaKen 2007-09-17
    ;;(set-symbol-value! 'yen "￥")  ;; XXX
    (set-symbol-value! 'yen "\\")
    ))

;; TODO: Support new custom type string-list.
(define japanese-auto-start-henkan-keyword-list '("、" "。" "．" "，" "？" "」" "！" "；" "：" ")" ";" ":" "）" "”" "】" "』" "》" "〉" "｝" "］" "〕" "}" "]" "?" "." "," "!"))

(define ja-rk-rule-consonant-to-keep
  (map (lambda (c)
         (if (= (string-length c) 1)
           (list (cons (list c) '()) (list c c c))
           (let ((lst (reverse (string-to-list c))))
             (list (cons lst '()) (list (list (car lst) (car lst) (car lst))
                                        (list (cadr lst) (cadr lst) (cadr lst)))))))
       (filter (lambda (x) (not (string=? "n" x)))
               (map car ja-consonant-syllable-table))))

(define ja-rk-rule-keep-consonant-update
  (lambda ()
    (if ja-rk-rule-keep-consonant?
      (set! ja-rk-rule (append ja-rk-rule-consonant-to-keep
                               ja-rk-rule-basic
                               ja-rk-rule-additional))
      (set! ja-rk-rule (append ja-rk-rule-basic
                               ja-rk-rule-additional)))))

;; In ja-rk-rule-update,
;; don't set ja-rk-rule-basic to ja-rk-rule-basic-uim
;;
;; If you do so, when the following conditions are met
;;   1) users call ja-rk-rule-update in ~/.uim
;;   2) ja-rk-rule-type is 'uim (default value)
;; ja-rk-rule-basic is always overridden
;; with ja-rk-rule-basic-uim.
;; This is an unwanted behavior for users.
(define ja-rk-rule-update
  (lambda ()
    (and
      (eq? ja-rk-rule-type 'custom)
      (set! ja-rk-rule-basic
        (ja-rk-rule-table->rule ja-rk-rule-table-basic)))
    (ja-rk-rule-keep-consonant-update)))

;;; Convert EUC-JP code to EUC-JP string (cf. ucs->utf8-string in ichar.scm)
(define (ja-euc-jp-code->euc-jp-string code)
  (with-char-codec "EUC-JP"
    (lambda ()
      (let ((str (list->string (list (integer->char code)))))
        (with-char-codec "ISO-8859-1"
          (lambda ()
            (%%string-reconstruct! str)))))))

;;; Convert JIS code(ISO-2022-JP) to EUC-JP string
(define (ja-jis-code->euc-jp-string state jis1 jis2)
  (let
    ((ej0 (if (eq? state 'jisx0213-plane2) #x8f 0))
     (ej1 (+ jis1 #x80))
     (ej2 (+ jis2 #x80)))
    (and
      ;; sigscheme/src/encoding.c:eucjp_int2str()
      (<= #xa1 ej1 #xfe) ; IN_GR94()
      (if (= ej0 #x8f) ; SS3?
        (<= #xa1 ej2 #xfe) ; IN_GR94()
        (<= #xa0 ej2 #xff)) ; IN_GR96()
      (iconv-code-conv (iconv-open "UTF-8" "EUC-JP")
		       (ja-euc-jp-code->euc-jp-string
			(+ (* ej0 #x10000) (* ej1 #x100) ej2))))))

;;; Convert reverse string list of JIS code to one EUC-JP kanji string
;;; ("d" "2" "0" "5") -> "亅"
(define (ja-kanji-code-input-jis str-list)
  (and-let*
    ((length=4? (= (length str-list) 4))
     (str1 (string-list-concat (take-right str-list 2)))
     (str2 (string-list-concat (take str-list 2)))
     (jis1 (string->number str1 16))
     (jis2 (string->number str2 16)))
    (ja-jis-code->euc-jp-string 'jisx0213-plane1 jis1 jis2)))

;;; Convert reverse string list of Kuten code to one EUC-JP kanji string
;;; ("3" "1" "-" "8" "4" "-" "1") -> "亅"
(define (ja-kanji-code-input-kuten str-list)
  (let*
    ((numlist (string-split (string-list-concat str-list) "-"))
     (men-exists? (>= (length numlist) 3))
     (men (if men-exists? (string->number (list-ref numlist 0)) 1))
     (ku (string->number (list-ref numlist (if men-exists? 1 0))))
     (ten (string->number (list-ref numlist (if men-exists? 2 1)))))
    (and men ku ten (<= 1 men 2)
      (ja-jis-code->euc-jp-string
        (if (= men 2) 'jisx0213-plane2 'jisx0213-plane1)
        (+ ku #x20) (+ ten #x20)))))

;;; Convert reverse string list of UCS to one EUC-JP kanji string
;;; ("5" "8" "E" "4" "+" "U") -> "亅"
(define (ja-kanji-code-input-ucs str-list)
  (and-let*
    ((str-list-1 (drop-right str-list 1)) ; drop last "U"
     (not-only-u? (not (null? str-list-1)))
     (ucs-str (if (string=? (last str-list-1) "+")
                (drop-right str-list-1 1)
                str-list-1))
     (ucs (string->number (string-list-concat ucs-str) 16))
     ;; range check to avoid error
     (valid? ; sigscheme/src/sigschemeinternal.h:ICHAR_VALID_UNICODEP()
      (or
        (<= 0 ucs #xd7ff)
        (<= #xe000 ucs #x10ffff))))
     (ucs->utf8-string ucs)))

;;; Convert reverse string list to one EUC-JP kanji string
(define (ja-kanji-code-input str-list)
  (cond
    ((string-ci=? (last str-list) "u")
      (ja-kanji-code-input-ucs str-list))
    ((member "-" str-list)
      (ja-kanji-code-input-kuten str-list))
    (else
      (ja-kanji-code-input-jis str-list))))

;;
(require "rk.scm")

(ja-rk-rule-update)
