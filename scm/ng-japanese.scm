;;; ng-japanese.scm: Character composition rulesets for Japanese
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

(require "util.scm")
(require "ng-key.scm")
(require "evmap.scm")
;; temporary workaround for ja-immediate-commit-ruleset
(require-custom "japanese-romaji-custom.scm")


;;
;; kana definitions
;;

(define-record 'ja-kana
  '((hiragana "")
    (katakana "")
    (halfkana "")  ;; JIS X 0201 kana
    ;;(manyou   "")  ;; Manyou-gana
    ;;(kusachu  "")  ;; kana representations for a closed community
    ;;(gal      "")  ;; kana representations for a closed community
    ))

(define-record 'ja-kana-props
  '((id            #f)
    (extractor     #f)
    (indication-id #f)
    (label         "")
    (short-desc    "")))

(define ja-kana-props-alist
  (list
   (list 'hiragana
	 ja-kana-hiragana
	 'figure_ja_hiragana
	 "Hiragana"
	 "")
   (list 'katakana
	 ja-kana-katakana
	 'figure_ja_katakana
	 "Katakana"
	 "")
   (list 'halfkana
	 ja-kana-halfkana
	 'figure_ja_halfkana
	 "Halfwidth katakana"
	 "")
;;   (list 'manyou
;;	 ja-kana-manyou
;;	 'figure_ja_manyou
;;	 "Manyou-gana"
;;	 "")
;;   (list 'kusachu
;;	 ja-kana-kusachu
;;	 'figure_ja_kusachu
;;	 "Kusachu ideographs"
;;	 "")
;;   (list 'gal
;;	 ja-kana-gal
;;	 'figure_ja_gal
;;	 "Gal ideographs"
;;	 "")
   ))

;; Japanese kana composition rulesets will be rewritten using
;; following symbolic kana references in future version of uim to
;; modify kana representations easily, reduce memory consumptions, and
;; to prevent human errors when copying each definitions into
;; rulesets.

;; ¤¢¹Ô
(define ja-kana-a    '("¤¢" "¥¢" "Ž±"))
(define ja-kana-i    '("¤¤" "¥¤" "Ž²"))
(define ja-kana-u    '("¤¦" "¥¦" "Ž³"))
(define ja-kana-e    '("¤¨" "¥¨" "Ž´"))
(define ja-kana-o    '("¤ª" "¥ª" "Žµ"))

;;¤«¹Ô		     
(define ja-kana-ka   '("¤«" "¥«" "Ž¶"))
(define ja-kana-ki   '("¤­" "¥­" "Ž·"))
(define ja-kana-ku   '("¤¯" "¥¯" "Ž¸"))
(define ja-kana-ke   '("¤±" "¥±" "Ž¹"))
(define ja-kana-ko   '("¤³" "¥³" "Žº"))

(define ja-kana-kya  '(("¤­" "¤ã") ("¥­" "¥ã") ("Ž·" "Ž¬")))
(define ja-kana-kyi  '(("¤­" "¤£") ("¥­" "¥£") ("Ž·" "Ž¨")))
(define ja-kana-kyu  '(("¤­" "¤å") ("¥­" "¥å") ("Ž·" "Ž­")))
(define ja-kana-kye  '(("¤­" "¤§") ("¥­" "¥§") ("Ž·" "Žª")))
(define ja-kana-kyo  '(("¤­" "¤ç") ("¥­" "¥ç") ("Ž·" "Ž®")))

(define ja-kana-ga   '("¤¬" "¥¬" ("Ž¶" "ŽÞ")))
(define ja-kana-gi   '("¤®" "¥®" ("Ž·" "ŽÞ")))
(define ja-kana-gu   '("¤°" "¥°" ("Ž¸" "ŽÞ")))
(define ja-kana-ge   '("¤²" "¥²" ("Ž¹" "ŽÞ")))
(define ja-kana-go   '("¤´" "¥´" ("Žº" "ŽÞ")))

(define ja-kana-gya  '(("¤®" "¤ã") ("¥®" "¥ã") ("Ž·" "ŽÞ" "Ž¬")))
(define ja-kana-gyi  '(("¤®" "¤£") ("¥®" "¥£") ("Ž·" "ŽÞ" "Ž¨")))
(define ja-kana-gyu  '(("¤®" "¤å") ("¥®" "¥å") ("Ž·" "ŽÞ" "Ž­")))
(define ja-kana-gye  '(("¤®" "¤§") ("¥®" "¥§") ("Ž·" "ŽÞ" "Žª")))
(define ja-kana-gyo  '(("¤®" "¤ç") ("¥®" "¥ç") ("Ž·" "ŽÞ" "Ž®")))

(define ja-kana-xtsu '("¤Ã" "¥Ã" "Ž¯"))

;; Can be extended as follows if you want
;;(define ja-kana-sa '("¤µ" "¥µ" "Ž»" "º¸" "¡Þ" "¡Þ"))
;;(define ja-kana-de '("¤Ç" "¥Ç" ("ŽÃ" "ŽÞ") "Å¥" "¤Æ~" "¦Ó¡·"))

;;
;; Japanese ruleset generator
;;

(define ja-extract-dedicated-ruleset
  (lambda (ruleset kana-extractor)
    (map (lambda (rule)
	   (let ((kana (kana-extractor (evmap-rule-action-seq rule))))
	     (list (evmap-rule-event-seq rule)
		   (if (pair? kana)
		       kana
		       (list kana)))))
	 ruleset)))

(define ja-define-dedicated-ruleset
  (lambda (prefix name kana)
    (let* ((ruleset-name (symbolconc prefix '- kana '- name '-ruleset))
	   (src-ruleset-name (symbolconc prefix '- name '-ruleset))
	   (src-ruleset (symbol-value src-ruleset-name))
	   (kana-extractor (ja-kana-props-extractor
			    (assq kana ja-kana-props-alist)))
	   (extracted-ruleset (ja-extract-dedicated-ruleset
			       src-ruleset
			       kana-extractor)))
      (eval (list 'define ruleset-name (list 'quote extracted-ruleset))
	    toplevel-env))))

(define ja-define-dedicated-rulesets
  (lambda (prefix ruleset-name-list)
    (for-each (lambda (ruleset-name)
		(for-each (lambda (kana)
			    (ja-define-dedicated-ruleset
			     prefix ruleset-name kana))
			  (map ja-kana-props-id ja-kana-props-alist)))
	      ruleset-name-list)))


;;
;; symbols and punctuations
;;

;; space
(define ja-halfwidth-space-ruleset
  '(((" ") (" "))))

(define ja-fullwidth-space-ruleset
  '(((" ") ("¡¡"))))

;; hyphen
(define ja-halfwidth-hyphen-ruleset
  '((("-") ("-"))))

(define ja-fullwidth-hyphen-ruleset
  '((("-") ("¡Ý"))))

;; comma
;; Should be replaced with more elaborated configuration method.
(define ja-halfwidth-comma-ruleset
  '(((",") (","))))

(define ja-fullwidth-comma-ruleset
  '(((",") ("¡¤"))))

(define ja-fullwidth-kana-comma-ruleset
  '(((",") ("¡¢"))))

(define ja-halfwidth-kana-comma-ruleset
  '(((",") ("Ž¤"))))

;; period
;; Should be replaced with more elaborated configuration method.
(define ja-halfwidth-period-ruleset
  '(((".") ("."))))

(define ja-fullwidth-period-ruleset
  '(((".") ("¡¥"))))

(define ja-fullwidth-kana-period-ruleset
  '(((".") ("¡£"))))

(define ja-halfwidth-kana-period-ruleset
  '(((".") ("Ž¡"))))

;; basic symbols
;; separate these entries into appropriate classes on demand.
;;   (map (compose print (lambda (s) (list s s)) list charcode->string)
;;        (append (iota 48 33) (iota 65 58) (iota 97 91)  (iota 127 123)))
(define ja-halfwidth-basic-symbol-ruleset
  '(
    ;;((" ") (" "))
    ((("!" mod_ignore_Shift))  ("!"))
    ((("\"" mod_ignore_Shift)) ("\""))
    ((("#" mod_ignore_Shift))  ("#"))
    ((("$" mod_ignore_Shift))  ("$"))
    ((("%" mod_ignore_Shift))  ("%"))
    ((("&" mod_ignore_Shift))  ("&"))
    ((("'" mod_ignore_Shift))  ("'"))
    ((("(" mod_ignore_Shift))  ("("))
    (((")" mod_ignore_Shift))  (")"))
    ((("*" mod_ignore_Shift))  ("*"))
    ((("+" mod_ignore_Shift))  ("+"))
    ;;((("," mod_ignore_Shift))  (","))
    ;;((("-" mod_ignore_Shift))  ("-"))
    ;;((("." mod_ignore_Shift))  ("."))
    ((("/" mod_ignore_Shift))  ("/"))
    (((":" mod_ignore_Shift))  (":"))
    (((";" mod_ignore_Shift))  (";"))
    ((("<" mod_ignore_Shift))  ("<"))
    ((("=" mod_ignore_Shift))  ("="))
    (((">" mod_ignore_Shift))  (">"))
    ((("?" mod_ignore_Shift))  ("?"))
    ((("@" mod_ignore_Shift))  ("@"))
    ((("[" mod_ignore_Shift))  ("["))
    ((("\\" mod_ignore_Shift)) ("\\"))
    ((("]" mod_ignore_Shift))  ("]"))
    ((("^" mod_ignore_Shift))  ("^"))
    ((("_" mod_ignore_Shift))  ("_"))
    ((("`" mod_ignore_Shift))  ("`"))
    ((("{" mod_ignore_Shift))  ("{"))
    ((("|" mod_ignore_Shift))  ("|"))
    ((("}" mod_ignore_Shift))  ("}"))
    ((("~" mod_ignore_Shift))  ("~"))
    ))

(define ja-fullwidth-basic-symbol-ruleset
  '(
    ;;((" ")  ("¡¡"))
    ((("!" mod_ignore_Shift))  ("¡ª"))
    ((("\"" mod_ignore_Shift)) ("¡É"))
    ((("#" mod_ignore_Shift))  ("¡ô"))
    ((("$" mod_ignore_Shift))  ("¡ð"))
    ((("%" mod_ignore_Shift))  ("¡ó"))
    ((("&" mod_ignore_Shift))  ("¡õ"))
    ((("'" mod_ignore_Shift))  ("¡Ç"))
    ((("(" mod_ignore_Shift))  ("¡Ê"))
    (((")" mod_ignore_Shift))  ("¡Ë"))
    ((("*" mod_ignore_Shift))  ("¡ö"))
    ((("+" mod_ignore_Shift))  ("¡Ü"))
    ;;((("," mod_ignore_Shift))  ("¡¤"))
    ;;((("-" mod_ignore_Shift))  ("¡Ý"))
    ;;((("." mod_ignore_Shift))  ("¡¥"))
    ((("/" mod_ignore_Shift))  ("¡¿"))
    (((":" mod_ignore_Shift))  ("¡§"))
    (((";" mod_ignore_Shift))  ("¡¨"))
    ((("<" mod_ignore_Shift))  ("¡ã"))
    ((("=" mod_ignore_Shift))  ("¡á"))
    (((">" mod_ignore_Shift))  ("¡ä"))
    ((("?" mod_ignore_Shift))  ("¡©"))
    ((("@" mod_ignore_Shift))  ("¡÷"))
    ((("[" mod_ignore_Shift))  ("¡Î"))
    ((("\\" mod_ignore_Shift)) ("¡À"))
    ((("]" mod_ignore_Shift))  ("¡Ï"))
    ((("^" mod_ignore_Shift))  ("¡°"))
    ((("_" mod_ignore_Shift))  ("¡²"))
    ((("`" mod_ignore_Shift))  ("¡Æ"))
    ((("{" mod_ignore_Shift))  ("¡Ð"))
    ((("|" mod_ignore_Shift))  ("¡Ã"))
    ((("}" mod_ignore_Shift))  ("¡Ñ"))
    ((("~" mod_ignore_Shift))  ("¡Á"))
    ))

;; numbers
(define ja-halfwidth-number-ruleset
  '((("1") ("1"))
    (("2") ("2"))
    (("3") ("3"))
    (("4") ("4"))
    (("5") ("5"))
    (("6") ("6"))
    (("7") ("7"))
    (("8") ("8"))
    (("9") ("9"))
    (("0") ("0"))))

(define ja-fullwidth-number-ruleset
  '((("1") ("£±"))
    (("2") ("£²"))
    (("3") ("£³"))
    (("4") ("£´"))
    (("5") ("£µ"))
    (("6") ("£¶"))
    (("7") ("£·"))
    (("8") ("£¸"))
    (("9") ("£¹"))
    (("0") ("£°"))))

;; alphabets
(define ja-halfwidth-alphabet-ruleset
  '(((("a" mod_ignore_Shift)) ("a"))
    ((("b" mod_ignore_Shift)) ("b"))
    ((("c" mod_ignore_Shift)) ("c"))
    ((("d" mod_ignore_Shift)) ("d"))
    ((("e" mod_ignore_Shift)) ("e"))
    ((("f" mod_ignore_Shift)) ("f"))
    ((("g" mod_ignore_Shift)) ("g"))
    ((("h" mod_ignore_Shift)) ("h"))
    ((("i" mod_ignore_Shift)) ("i"))
    ((("j" mod_ignore_Shift)) ("j"))
    ((("k" mod_ignore_Shift)) ("k"))
    ((("l" mod_ignore_Shift)) ("l"))
    ((("m" mod_ignore_Shift)) ("m"))
    ((("n" mod_ignore_Shift)) ("n"))
    ((("o" mod_ignore_Shift)) ("o"))
    ((("p" mod_ignore_Shift)) ("p"))
    ((("q" mod_ignore_Shift)) ("q"))
    ((("r" mod_ignore_Shift)) ("r"))
    ((("s" mod_ignore_Shift)) ("s"))
    ((("t" mod_ignore_Shift)) ("t"))
    ((("u" mod_ignore_Shift)) ("u"))
    ((("v" mod_ignore_Shift)) ("v"))
    ((("w" mod_ignore_Shift)) ("w"))
    ((("x" mod_ignore_Shift)) ("x"))
    ((("y" mod_ignore_Shift)) ("y"))
    ((("z" mod_ignore_Shift)) ("z"))

    ((("A" mod_ignore_Shift)) ("A"))
    ((("B" mod_ignore_Shift)) ("B"))
    ((("C" mod_ignore_Shift)) ("C"))
    ((("D" mod_ignore_Shift)) ("D"))
    ((("E" mod_ignore_Shift)) ("E"))
    ((("F" mod_ignore_Shift)) ("F"))
    ((("G" mod_ignore_Shift)) ("G"))
    ((("H" mod_ignore_Shift)) ("H"))
    ((("I" mod_ignore_Shift)) ("I"))
    ((("J" mod_ignore_Shift)) ("J"))
    ((("K" mod_ignore_Shift)) ("K"))
    ((("L" mod_ignore_Shift)) ("L"))
    ((("M" mod_ignore_Shift)) ("M"))
    ((("N" mod_ignore_Shift)) ("N"))
    ((("O" mod_ignore_Shift)) ("O"))
    ((("P" mod_ignore_Shift)) ("P"))
    ((("Q" mod_ignore_Shift)) ("Q"))
    ((("R" mod_ignore_Shift)) ("R"))
    ((("S" mod_ignore_Shift)) ("S"))
    ((("T" mod_ignore_Shift)) ("T"))
    ((("U" mod_ignore_Shift)) ("U"))
    ((("V" mod_ignore_Shift)) ("V"))
    ((("W" mod_ignore_Shift)) ("W"))
    ((("X" mod_ignore_Shift)) ("X"))
    ((("Y" mod_ignore_Shift)) ("Y"))
    ((("Z" mod_ignore_Shift)) ("Z"))))

(define ja-fullwidth-alphabet-ruleset
  '(((("a" mod_ignore_Shift)) ("£á"))
    ((("b" mod_ignore_Shift)) ("£â"))
    ((("c" mod_ignore_Shift)) ("£ã"))
    ((("d" mod_ignore_Shift)) ("£ä"))
    ((("e" mod_ignore_Shift)) ("£å"))
    ((("f" mod_ignore_Shift)) ("£æ"))
    ((("g" mod_ignore_Shift)) ("£ç"))
    ((("h" mod_ignore_Shift)) ("£è"))
    ((("i" mod_ignore_Shift)) ("£é"))
    ((("j" mod_ignore_Shift)) ("£ê"))
    ((("k" mod_ignore_Shift)) ("£ë"))
    ((("l" mod_ignore_Shift)) ("£ì"))
    ((("m" mod_ignore_Shift)) ("£í"))
    ((("n" mod_ignore_Shift)) ("£î"))
    ((("o" mod_ignore_Shift)) ("£ï"))
    ((("p" mod_ignore_Shift)) ("£ð"))
    ((("q" mod_ignore_Shift)) ("£ñ"))
    ((("r" mod_ignore_Shift)) ("£ò"))
    ((("s" mod_ignore_Shift)) ("£ó"))
    ((("t" mod_ignore_Shift)) ("£ô"))
    ((("u" mod_ignore_Shift)) ("£õ"))
    ((("v" mod_ignore_Shift)) ("£ö"))
    ((("w" mod_ignore_Shift)) ("£÷"))
    ((("x" mod_ignore_Shift)) ("£ø"))
    ((("y" mod_ignore_Shift)) ("£ù"))
    ((("z" mod_ignore_Shift)) ("£ú"))

    ((("A" mod_ignore_Shift)) ("£Á"))
    ((("B" mod_ignore_Shift)) ("£Â"))
    ((("C" mod_ignore_Shift)) ("£Ã"))
    ((("D" mod_ignore_Shift)) ("£Ä"))
    ((("E" mod_ignore_Shift)) ("£Å"))
    ((("F" mod_ignore_Shift)) ("£Æ"))
    ((("G" mod_ignore_Shift)) ("£Ç"))
    ((("H" mod_ignore_Shift)) ("£È"))
    ((("I" mod_ignore_Shift)) ("£É"))
    ((("J" mod_ignore_Shift)) ("£Ê"))
    ((("K" mod_ignore_Shift)) ("£Ë"))
    ((("L" mod_ignore_Shift)) ("£Ì"))
    ((("M" mod_ignore_Shift)) ("£Í"))
    ((("N" mod_ignore_Shift)) ("£Î"))
    ((("O" mod_ignore_Shift)) ("£Ï"))
    ((("P" mod_ignore_Shift)) ("£Ð"))
    ((("Q" mod_ignore_Shift)) ("£Ñ"))
    ((("R" mod_ignore_Shift)) ("£Ò"))
    ((("S" mod_ignore_Shift)) ("£Ó"))
    ((("T" mod_ignore_Shift)) ("£Ô"))
    ((("U" mod_ignore_Shift)) ("£Õ"))
    ((("V" mod_ignore_Shift)) ("£Ö"))
    ((("W" mod_ignore_Shift)) ("£×"))
    ((("X" mod_ignore_Shift)) ("£Ø"))
    ((("Y" mod_ignore_Shift)) ("£Ù"))
    ((("Z" mod_ignore_Shift)) ("£Ú"))))

;; This ruleset will not be used in ordinary input method. Direct
;; input mode passes through almost of key events instead of using
;; this ruleset.
(define ja-halfwidth-alphanumeric-ruleset
  (append
   ja-halfwidth-space-ruleset
   ja-halfwidth-hyphen-ruleset
   ja-halfwidth-comma-ruleset
   ja-halfwidth-period-ruleset
   ja-halfwidth-basic-symbol-ruleset
   ja-halfwidth-number-ruleset
   ja-halfwidth-alphabet-ruleset))

(define ja-fullwidth-alphanumeric-ruleset
  (append
   ja-fullwidth-space-ruleset
   ja-fullwidth-hyphen-ruleset
   ja-fullwidth-comma-ruleset
   ja-fullwidth-period-ruleset
   ja-fullwidth-basic-symbol-ruleset
   ja-fullwidth-number-ruleset
   ja-fullwidth-alphabet-ruleset))

(define ja-direct-ruleset '())

;; Although the ruleset contains 'romaji' participants, this ruleset
;; is prepared for all japanese rulsets. It should be reorganized as
;; appropriately.
(define ja-immediate-commit-ruleset
  (append
   (symbol-value ja-romaji-fullwidth-space-ruleset)
   ;;(symbol-value ja-romaji-fullwidth-basic-symbol-ruleset)
   ;;(symbol-value ja-romaji-fullwidth-number-ruleset)
   ))

(define ja-halfwidth-alphanumeric-ruletree
  (evmap-parse-ruleset ja-halfwidth-alphanumeric-ruleset))

(define ja-fullwidth-alphanumeric-ruletree
  (evmap-parse-ruleset ja-fullwidth-alphanumeric-ruleset))

(define ja-direct-ruletree
  (evmap-parse-ruleset ja-direct-ruleset))

(define ja-immediate-commit-ruletree
  (evmap-parse-ruleset ja-immediate-commit-ruleset))
