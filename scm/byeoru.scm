;;; byeoru.scm: a Hangul input module for uim.
;;;
;;; Copyright (c) 2003-2009 uim Project http://code.google.com/p/uim/
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

(require "util.scm")
(require "ustr.scm")
(require-custom "generic-key-custom.scm")
(require "rk.scm")

(require-custom "byeoru-custom.scm")
(require-custom "byeoru-key-custom.scm")
(require "byeoru-symbols.scm")


;;; --------------------------------------
;;;  Hangul syllable composition routines
;;; --------------------------------------

;; These jamo names are different from those used in the Unicode standard,
;; which doesn't matter anyway.
(define byeoru-choseong-alist
  '((choseong-void         .  0)
    (choseong-giyeog       .  1)
    (choseong-ssanggiyeog  .  2)
    (choseong-nieun        .  3)
    (choseong-digeud       .  4)
    (choseong-ssangdigeud  .  5)
    (choseong-rieul        .  6)
    (choseong-mieum        .  7)
    (choseong-bieub        .  8)
    (choseong-ssangbieub   .  9)
    (choseong-sios         . 10)
    (choseong-ssangsios    . 11)
    (choseong-ieung        . 12)
    (choseong-jieuj        . 13)
    (choseong-ssangjieuj   . 14)
    (choseong-chieuch      . 15)
    (choseong-kieuk        . 16)
    (choseong-tieut        . 17)
    (choseong-pieup        . 18)
    (choseong-hieuh        . 19)))

(define byeoru-jungseong-alist
  '((jungseong-void	   .  0)
    (jungseong-a   	   .  1)
    (jungseong-ae  	   .  2)
    (jungseong-ya  	   .  3)
    (jungseong-yae 	   .  4)
    (jungseong-eo  	   .  5)
    (jungseong-e   	   .  6)
    (jungseong-yeo 	   .  7)
    (jungseong-ye  	   .  8)
    (jungseong-o   	   .  9)
    (jungseong-wa  	   . 10)
    (jungseong-wae 	   . 11)
    (jungseong-oe  	   . 12)
    (jungseong-yo  	   . 13)
    (jungseong-u   	   . 14)
    (jungseong-wo  	   . 15)
    (jungseong-we  	   . 16)
    (jungseong-wi  	   . 17)
    (jungseong-yu  	   . 18)
    (jungseong-eu  	   . 19)
    (jungseong-ui  	   . 20)
    (jungseong-i   	   . 21)))

(define byeoru-jongseong-alist
  '((jongseong-void        .  0)
    (jongseong-giyeog      .  1)
    (jongseong-ssanggiyeog .  2)
    (jongseong-giyeogsios  .  3)
    (jongseong-nieun       .  4)
    (jongseong-nieunjieuj  .  5)
    (jongseong-nieunhieuh  .  6)
    (jongseong-digeud      .  7)
    (jongseong-rieul       .  8)
    (jongseong-rieulgiyeog .  9)
    (jongseong-rieulmieum  . 10)
    (jongseong-rieulbieub  . 11)
    (jongseong-rieulsios   . 12)
    (jongseong-rieultieut  . 13)
    (jongseong-rieulpieup  . 14)
    (jongseong-rieulhieuh  . 15)
    (jongseong-mieum       . 16)
    (jongseong-bieub       . 17)
    (jongseong-bieubsios   . 18)
    (jongseong-sios        . 19)
    (jongseong-ssangsios   . 20)
    (jongseong-ieung       . 21)
    (jongseong-jieuj       . 22)
    (jongseong-chieuch     . 23)
    (jongseong-kieuk       . 24)
    (jongseong-tieut       . 25)
    (jongseong-pieup       . 26)
    (jongseong-hieuh       . 27)))

(define byeoru-compound-jamo-alist
  '(((jungseong-o      . jungseong-a     ) . jungseong-wa 	  )
    ((jungseong-o      . jungseong-ae    ) . jungseong-wae	  )
    ((jungseong-o      . jungseong-i     ) . jungseong-oe 	  )
    ((jungseong-u      . jungseong-eo    ) . jungseong-wo 	  )
    ((jungseong-u      . jungseong-e     ) . jungseong-we 	  )
    ((jungseong-u      . jungseong-i     ) . jungseong-wi 	  )
    ((jungseong-eu     . jungseong-i     ) . jungseong-ui 	  )
    ((jongseong-giyeog . jongseong-sios  ) . jongseong-giyeogsios )
    ((jongseong-nieun  . jongseong-jieuj ) . jongseong-nieunjieuj )
    ((jongseong-nieun  . jongseong-hieuh ) . jongseong-nieunhieuh )
    ((jongseong-rieul  . jongseong-giyeog) . jongseong-rieulgiyeog)
    ((jongseong-rieul  . jongseong-mieum ) . jongseong-rieulmieum )
    ((jongseong-rieul  . jongseong-bieub ) . jongseong-rieulbieub )
    ((jongseong-rieul  . jongseong-sios  ) . jongseong-rieulsios  )
    ((jongseong-rieul  . jongseong-tieut ) . jongseong-rieultieut )
    ((jongseong-rieul  . jongseong-pieup ) . jongseong-rieulpieup )
    ((jongseong-rieul  . jongseong-hieuh ) . jongseong-rieulhieuh )
    ((jongseong-bieub  . jongseong-sios  ) . jongseong-bieubsios  )))

(define byeoru-double-jamo-alist
  '(((choseong-giyeog  . choseong-giyeog ) . choseong-ssanggiyeog )
    ((choseong-digeud  . choseong-digeud ) . choseong-ssangdigeud )
    ((choseong-bieub   . choseong-bieub  ) . choseong-ssangbieub  )
    ((choseong-sios    . choseong-sios   ) . choseong-ssangsios   )
    ((choseong-jieuj   . choseong-jieuj  ) . choseong-ssangjieuj  )
    ((jongseong-giyeog . jongseong-giyeog) . jongseong-ssanggiyeog)
    ((jongseong-sios   . jongseong-sios  ) . jongseong-ssangsios  )))

(define byeoru-transition-alist
  (let ((sta '(start     . 0))
	(ch1 '(choseong  . 1))
	(ch2 '(choseong  . 2))
	(ch3 '(choseong  . 3))
	(ch4 '(choseong  . 4))
	(ju1 '(jungseong . 1))
	(ju2 '(jungseong . 2))
	(ju3 '(jungseong . 3))
	(ju4 '(jungseong . 4))
	(jo1 '(jongseong . 1))
	(jo2 '(jongseong . 2))
	(jo3 '(jongseong . 3))
	(jo4 '(jongseong . 4)))
    (list
     (list sta . (ch1 ch2 ch3 ch4 ju1 ju2 ju3 ju4 jo1 jo2 jo3 jo4))
     (list ch1 . (ju1 ju2 ju3 ju4 jo1 jo2 jo3 jo4))
     (list ch2 . (ch4))
     (list ch3 . (ch4 ju1 ju2 ju3 ju4 jo1 jo2 jo3 jo4))
     (list ch4 . (ju1 ju2 ju3 ju4 jo1 jo2 jo3 jo4))
     (list ju1 . (jo1 jo2 jo3 jo4))
     (list ju2 . (ju4))
     (list ju3 . (ju4 jo1 jo2 jo3 jo4))
     (list ju4 . (jo1 jo2 jo3 jo4))
     (list jo1 . ())
     (list jo2 . (jo4))
     (list jo3 . (jo4))
     (list jo4 . ()))))

;; Expands a key choices list like
;; ((jongseong-bieub . (1 4)))
;; => ((jongseong-bieub . 1) (jongseong-bieub . 4)))
(define byeoru-expand-layout
  (lambda args
    (let ((layout (car args))
	  (kons (if (null? (cdr args)) cons list))
	  (kdr  (if (null? (cdr args)) cdr cadr)))
      (map (lambda (elm1)
	     (let ((cands (kdr elm1)))
	       (kons (car elm1)
		     (if (list? cands)
			 (append-map
			  (lambda (elm2)
			    (let ((class (car elm2))
				  (nos (cdr elm2)))
			      (if (list? nos)
				  (map (lambda (no) (cons class no)) nos)
				  (list elm2))))
			  cands)
			 cands))))
	   layout))))

(define byeoru-layout-hangul2hanterm
  (byeoru-expand-layout
   ;; Unshifted keys
   '(("q" (choseong-bieub   . 1) (jongseong-bieub   . (3 4)))
     ("w" (choseong-jieuj   . 1) (jongseong-jieuj   . (1 4)))
     ("e" (choseong-digeud  . 1) (jongseong-digeud  . 1))
     ("r" (choseong-giyeog  . 1) (jongseong-giyeog  . (3 4)))
     ("t" (choseong-sios    . 1) (jongseong-sios    . (1 4)))
     ("y" (jungseong-yo     . 1))
     ("u" (jungseong-yeo    . 1))
     ("i" (jungseong-ya     . 1))
     ("o" (jungseong-ae     . (1 4)))
     ("p" (jungseong-e      . (1 4)))
     ("a" (choseong-mieum   . 1) (jongseong-mieum   . (1 4)))
     ("s" (choseong-nieun   . 1) (jongseong-nieun   . 3))
     ("d" (choseong-ieung   . 1) (jongseong-ieung   . 1))
     ("f" (choseong-rieul   . 1) (jongseong-rieul   . 3))
     ("g" (choseong-hieuh   . 1) (jongseong-hieuh   . (1 4)))
     ("h" (jungseong-o      . 3))
     ("j" (jungseong-eo     . (1 4)))
     ("k" (jungseong-a      . (1 4)))
     ("l" (jungseong-i      . (1 4)))
     ("z" (choseong-kieuk   . 1) (jongseong-kieuk   . 1))
     ("x" (choseong-tieut   . 1) (jongseong-tieut   . (1 4)))
     ("c" (choseong-chieuch . 1) (jongseong-chieuch . 1))
     ("v" (choseong-pieup   . 1) (jongseong-pieup   . (1 4)))
     ("b" (jungseong-yu     . 1))
     ("n" (jungseong-u      . 3))
     ("m" (jungseong-eu     . 3))
     ;; Shifted keys
     ("Q" (choseong-ssangbieub  . 5))
     ("W" (choseong-ssangjieuj  . 5))
     ("E" (choseong-ssangdigeud . 5))
     ("R" (choseong-ssanggiyeog . 5) (jongseong-ssanggiyeog . 5))
     ("T" (choseong-ssangsios   . 5) (jongseong-ssangsios   . 5))
     ("O" (jungseong-yae        . 1))
     ("P" (jungseong-ye         . 1)))))

(define byeoru-layout-hangul2
  (byeoru-expand-layout
   ;; Unshifted keys
   '(("q" (choseong-bieub   . 1) (jongseong-bieub   . (3 4)))
     ("w" (choseong-jieuj   . 1) (jongseong-jieuj   . (1 4)))
     ("e" (choseong-digeud  . 1) (jongseong-digeud  . 1))
     ("r" (choseong-giyeog  . 1) (jongseong-giyeog  . (3 4)))
     ("t" (choseong-sios    . 1) (jongseong-sios    . (1 4)))
     ("y" (jungseong-yo     . 1))
     ("u" (jungseong-yeo    . 1))
     ("i" (jungseong-ya     . 1))
     ("o" (jungseong-ae     . (1 4)))
     ("p" (jungseong-e      . (1 4)))
     ("a" (choseong-mieum   . 1) (jongseong-mieum   . (1 4)))
     ("s" (choseong-nieun   . 1) (jongseong-nieun   . 3))
     ("d" (choseong-ieung   . 1) (jongseong-ieung   . 1))
     ("f" (choseong-rieul   . 1) (jongseong-rieul   . 3))
     ("g" (choseong-hieuh   . 1) (jongseong-hieuh   . (1 4)))
     ("h" (jungseong-o      . 3))
     ("j" (jungseong-eo     . (1 4)))
     ("k" (jungseong-a      . (1 4)))
     ("l" (jungseong-i      . (1 4)))
     ("z" (choseong-kieuk   . 1) (jongseong-kieuk   . 1))
     ("x" (choseong-tieut   . 1) (jongseong-tieut   . (1 4)))
     ("c" (choseong-chieuch . 1) (jongseong-chieuch . 1))
     ("v" (choseong-pieup   . 1) (jongseong-pieup   . (1 4)))
     ("b" (jungseong-yu     . 1))
     ("n" (jungseong-u      . 3))
     ("m" (jungseong-eu     . 3))
     ;; Shifted keys
     ("Q" (choseong-ssangbieub  . 5))
     ("W" (choseong-ssangjieuj  . 5))
     ("E" (choseong-ssangdigeud . 5))
     ("R" (choseong-ssanggiyeog . 5) (jongseong-ssanggiyeog . 5))
     ("T" (choseong-ssangsios   . 5) (jongseong-ssangsios   . 5))
     ("Y" (jungseong-yo     . 1))
     ("U" (jungseong-yeo    . 1))
     ("I" (jungseong-ya     . 1))
     ("O" (jungseong-yae    . 1))
     ("P" (jungseong-ye     . 1))
     ("A" (choseong-mieum   . 1) (jongseong-mieum   . (1 4)))
     ("S" (choseong-nieun   . 1) (jongseong-nieun   . 3))
     ("D" (choseong-ieung   . 1) (jongseong-ieung   . 1))
     ("F" (choseong-rieul   . 1) (jongseong-rieul   . 3))
     ("G" (choseong-hieuh   . 1) (jongseong-hieuh   . (1 4)))
     ("H" (jungseong-o      . 3))
     ("J" (jungseong-eo     . (1 4)))
     ("K" (jungseong-a      . (1 4)))
     ("L" (jungseong-i      . (1 4)))
     ("Z" (choseong-kieuk   . 1) (jongseong-kieuk   . 1))
     ("X" (choseong-tieut   . 1) (jongseong-tieut   . (1 4)))
     ("C" (choseong-chieuch . 1) (jongseong-chieuch . 1))
     ("V" (choseong-pieup   . 1) (jongseong-pieup   . (1 4)))
     ("B" (jungseong-yu     . 1))
     ("N" (jungseong-u      . 3))
     ("M" (jungseong-eu     . 3)))))

;; The following definitions of 3-beol variants [final, 390, no-shift]
;; are based on the US keyboard layout.  A user of a different layout
;; (such as dvorak, Japanese) may want to write their own definition
;; in ~/.uim.
(define byeoru-layout-strict3final
  (byeoru-expand-layout
   ;; Unshifted keys
   '(("`" . "*")
     ("1" (jongseong-hieuh       . 1))
     ("2" (jongseong-ssangsios   . 5))
     ("3" (jongseong-bieub       . 1))
     ("4" (jungseong-yo	    	 . 1))
     ("5" (jungseong-yu	    	 . 1))
     ("6" (jungseong-ya	    	 . 1))
     ("7" (jungseong-ye	    	 . 1))
     ("8" (jungseong-ui	    	 . 4))
     ("9" (jungseong-u	    	 . 2))
     ("0" (choseong-kieuk	 . 1))
     ("-" . ")")
     ("=" . ">")
     ("q" (jongseong-sios	 . 1))
     ("w" (jongseong-rieul       . 1))
     ("e" (jungseong-yeo	 . 1))
     ("r" (jungseong-ae	    	 . (1 4)))
     ("t" (jungseong-eo	    	 . (1 4)))
     ("y" (choseong-rieul	 . 1))
     ("u" (choseong-digeud	 . (3 5)))
     ("i" (choseong-mieum	 . 1))
     ("o" (choseong-chieuch	 . 1))
     ("p" (choseong-pieup	 . 1))
     ("[" . "(")
     ("]" . "<")
     ("\\" . ":")
     ("a" (jongseong-ieung       . 1))
     ("s" (jongseong-nieun       . 1))
     ("d" (jungseong-i	    	 . (1 4)))
     ("f" (jungseong-a	    	 . (1 4)))
     ("g" (jungseong-eu	    	 . 1))
     ("h" (choseong-nieun	 . 1))
     ("j" (choseong-ieung	 . 1))
     ("k" (choseong-giyeog	 . (3 5)))
     ("l" (choseong-jieuj	 . (3 5)))
     (";" (choseong-bieub	 . (3 5)))
     ("'" (choseong-tieut	 . 1))
     ("z" (jongseong-mieum       . 1))
     ("x" (jongseong-giyeog      . 1))
     ("c" (jungseong-e	    	 . (1 4)))
     ("v" (jungseong-o	    	 . 1))
     ("b" (jungseong-u	    	 . 1))
     ("n" (choseong-sios	 . (3 5)))
     ("m" (choseong-hieuh	 . 1))
     ("/" (jungseong-o	         . 2))
     ;; Shifted keys
     ("~" . 8251)		; U+203B, REFERENCE MARK
     ("!" (jongseong-ssanggiyeog . 5))
     ("@" (jongseong-rieulgiyeog . 4))
     ("#" (jongseong-jieuj	 . 1))
     ("$" (jongseong-rieulpieup  . 4))
     ("%" (jongseong-rieultieut  . 4))
     ("^" . "=")
     ("&" . 8220)		; U+201C, LEFT DOUBLE QUOTATION MARK
     ("*" . 8221)		; U+201D, RIGHT DOUBLE QUOTATION MARK
     ("(" . "'")
     (")" . "~")
     ("_" . ";")
     ("Q" (jongseong-pieup	 . 1))
     ("W" (jongseong-tieut	 . 1))
     ("E" (jongseong-nieunjieuj  . 4))
     ("R" (jongseong-rieulhieuh  . 4))
     ("T" (jongseong-rieulsios   . 4))
     ("Y" . "5")
     ("U" . "6")
     ("I" . "7")
     ("O" . "8")
     ("P" . "9")
     ("{" . "%")
     ("}" . "/")
     ("|" . "\\")
;;     ("|" . 8361)		; U+20A9, WON SIGN
     ("A" (jongseong-digeud      . 1))
     ("S" (jongseong-nieunhieuh  . 4))
     ("D" (jongseong-rieulbieub  . 4))
     ("F" (jongseong-rieulmieum  . 4))
     ("G" (jungseong-yae	 . 1))
     ("H" . "0")
     ("J" . "1")
     ("K" . "2")
     ("L" . "3")
     (":" . "4")
     ("\"" . 183)		; U+00B7, MIDDLE DOT
     ("Z" (jongseong-chieuch     . 1))
     ("X" (jongseong-bieubsios   . 4))
     ("C" (jongseong-kieuk	 . 1))
     ("V" (jongseong-giyeogsios  . 4))
     ("B" . "?")
     ("N" . "-")
     ("M" . "\"")
     ("<" . ",")
     (">" . ".")
     ("?" . "!"))))

(define byeoru-layout-generous3final
  (byeoru-expand-layout
   ;; Unshifted keys
   '(("`" . "*")
     ("1" (jongseong-hieuh       . (1 4)))
     ("2" (jongseong-ssangsios   . 5))
     ("3" (jongseong-bieub       . (3 4)))
     ("4" (jungseong-yo	    	 . 1))
     ("5" (jungseong-yu	    	 . 1))
     ("6" (jungseong-ya	    	 . 1))
     ("7" (jungseong-ye	    	 . 1))
     ("8" (jungseong-ui	    	 . 4))
     ("9" (jungseong-u	    	 . 3))
     ("0" (choseong-kieuk	 . 1))
     ("-" . ")")
     ("=" . ">")
     ("q" (jongseong-sios	 . (3 4 5)))
     ("w" (jongseong-rieul       . 3))
     ("e" (jungseong-yeo	 . 1))
     ("r" (jungseong-ae	    	 . (1 4)))
     ("t" (jungseong-eo	    	 . (1 4)))
     ("y" (choseong-rieul	 . 1))
     ("u" (choseong-digeud	 . (3 5)))
     ("i" (choseong-mieum	 . 1))
     ("o" (choseong-chieuch	 . 1))
     ("p" (choseong-pieup	 . 1))
     ("[" . "(")
     ("]" . "<")
     ("\\" . ":")
     ("a" (jongseong-ieung       . 1))
     ("s" (jongseong-nieun       . 3))
     ("d" (jungseong-i	    	 . (1 4)))
     ("f" (jungseong-a	    	 . (1 4)))
     ("g" (jungseong-eu	    	 . 3))
     ("h" (choseong-nieun	 . 1))
     ("j" (choseong-ieung	 . 1))
     ("k" (choseong-giyeog	 . (3 5)))
     ("l" (choseong-jieuj	 . (3 5)))
     (";" (choseong-bieub	 . (3 5)))
     ("'" (choseong-tieut	 . 1))
     ("z" (jongseong-mieum       . (1 4)))
     ("x" (jongseong-giyeog      . (3 4 5)))
     ("c" (jungseong-e	    	 . (1 4)))
     ("v" (jungseong-o	    	 . 3))
     ("b" (jungseong-u	    	 . 3))
     ("n" (choseong-sios	 . (3 5)))
     ("m" (choseong-hieuh	 . 1))
     ("/" (jungseong-o	         . 3))
     ;; Shifted keys
     ("~" . 8251)		; U+203B, REFERENCE MARK
     ("!" (jongseong-ssanggiyeog . 5))
     ("@" (jongseong-rieulgiyeog . 4))
     ("#" (jongseong-jieuj	 . (1 4)))
     ("$" (jongseong-rieulpieup  . 4))
     ("%" (jongseong-rieultieut  . 4))
     ("^" . "=")
     ("&" . 8220)		; U+201C, LEFT DOUBLE QUOTATION MARK
     ("*" . 8221)		; U+201D, RIGHT DOUBLE QUOTATION MARK
     ("(" . "'")
     (")" . "~")
     ("_" . ";")
     ("Q" (jongseong-pieup	 . (1 4)))
     ("W" (jongseong-tieut	 . (1 4)))
     ("E" (jongseong-nieunjieuj  . 4))
     ("R" (jongseong-rieulhieuh  . 4))
     ("T" (jongseong-rieulsios   . 4))
     ("Y" . "5")
     ("U" . "6")
     ("I" . "7")
     ("O" . "8")
     ("P" . "9")
     ("{" . "%")
     ("}" . "/")
     ("|" . "\\")
;;     ("|" . 8361)		; U+20A9, WON SIGN
     ("A" (jongseong-digeud      . 1))
     ("S" (jongseong-nieunhieuh  . 4))
     ("D" (jongseong-rieulbieub  . 4))
     ("F" (jongseong-rieulmieum  . 4))
     ("G" (jungseong-yae	 . 1))
     ("H" . "0")
     ("J" . "1")
     ("K" . "2")
     ("L" . "3")
     (":" . "4")
     ("\"". 183)		; U+00B7, MIDDLE DOT
     ("Z" (jongseong-chieuch     . 1))
     ("X" (jongseong-bieubsios   . 4))
     ("C" (jongseong-kieuk	 . 1))
     ("V" (jongseong-giyeogsios  . 4))
     ("B" . "?")
     ("N" . "-")
     ("M" . "\"")
     ("<" . ",")
     (">" . ".")
     ("?" . "!"))))

(define byeoru-layout-strict390
  (byeoru-expand-layout
   ;; Unshifted keys
   '(("1" (jongseong-hieuh       . 1))
     ("2" (jongseong-ssangsios   . 5))
     ("3" (jongseong-bieub       . (1 4)))
     ("4" (jungseong-yo	   	 . 1))
     ("5" (jungseong-yu	   	 . 1))
     ("6" (jungseong-ya	   	 . 1))
     ("7" (jungseong-ye	   	 . 1))
     ("8" (jungseong-ui	   	 . 4))
     ("9" (jungseong-u	   	 . 2))
     ("0" (choseong-kieuk	 . 1))
     ("q" (jongseong-sios	 . (1 4)))
     ("w" (jongseong-rieul       . 3))
     ("e" (jungseong-yeo	 . 1))
     ("r" (jungseong-ae	   	 . (1 4)))
     ("t" (jungseong-eo	   	 . (1 4)))
     ("y" (choseong-rieul	 . 1))
     ("u" (choseong-digeud       . (3 5)))
     ("i" (choseong-mieum	 . 1))
     ("o" (choseong-chieuch      . 1))
     ("p" (choseong-pieup	 . 1))
     ("a" (jongseong-ieung       . 1))
     ("s" (jongseong-nieun       . 3))
     ("d" (jungseong-i	   	 . (1 4)))
     ("f" (jungseong-a	   	 . (1 4)))
     ("g" (jungseong-eu	   	 . 1))
     ("h" (choseong-nieun	 . 1))
     ("j" (choseong-ieung	 . 1))
     ("k" (choseong-giyeog       . (3 5)))
     ("l" (choseong-jieuj	 . (3 5)))
     (";" (choseong-bieub	 . (3 5)))
     ("'" (choseong-tieut	 . 1))
     ("z" (jongseong-mieum       . 1))
     ("x" (jongseong-giyeog      . 3))
     ("c" (jungseong-e	   	 . (1 4)))
     ("v" (jungseong-o	   	 . 1))
     ("b" (jungseong-u	   	 . 1))
     ("n" (choseong-sios	 . (3 5)))
     ("m" (choseong-hieuh	 . 1))
     ("/" (jungseong-o	         . 2))
     ;; Shifted keys
     ("!" (jongseong-jieuj       . (1 4)))
     ("Q" (jongseong-pieup       . (1 4)))
     ("W" (jongseong-tieut       . (1 4)))
     ("E" (jongseong-kieuk       . 1))
     ("R" (jungseong-yae	 . 1))
     ("T" . ";")
     ("Y" . "<")
     ("U" . "7")
     ("I" . "8")
     ("O" . "9")
     ("P" . ">")
     ("A" (jongseong-digeud      . 1))
     ("S" (jongseong-nieunhieuh  . 4))
     ("D" (jongseong-rieulgiyeog . 4))
     ("F" (jongseong-ssanggiyeog . 5))
     ("G" . "/")
     ("H" . "'")
     ("J" . "4")
     ("K" . "5")
     ("L" . "6")
     ("Z" (jongseong-chieuch     . 1))
     ("X" (jongseong-bieubsios   . 4))
     ("C" (jongseong-rieulmieum  . 4))
     ("V" (jongseong-rieulhieuh  . 4))
     ("B" . "!")
     ("N" . "0")
     ("M" . "1")
     ("<" . "2")
     (">" . "3"))))

(define byeoru-layout-generous390
  (byeoru-expand-layout
   ;; Unshifted keys
   '(("1" (jongseong-hieuh       . (1 4)))
     ("2" (jongseong-ssangsios   . 5))
     ("3" (jongseong-bieub       . (3 4)))
     ("4" (jungseong-yo	    	 . 1))
     ("5" (jungseong-yu	    	 . 1))
     ("6" (jungseong-ya	    	 . 1))
     ("7" (jungseong-ye	    	 . 1))
     ("8" (jungseong-ui	    	 . 4))
     ("9" (jungseong-u	    	 . 3))
     ("0" (choseong-kieuk	 . 1))
     ("q" (jongseong-sios	 . (3 4 5)))
     ("w" (jongseong-rieul       . 3))
     ("e" (jungseong-yeo	 . 1))
     ("r" (jungseong-ae	    	 . (1 4)))
     ("t" (jungseong-eo	    	 . (1 4)))
     ("y" (choseong-rieul	 . 1))
     ("u" (choseong-digeud       . (3 5)))
     ("i" (choseong-mieum	 . 1))
     ("o" (choseong-chieuch      . 1))
     ("p" (choseong-pieup	 . 1))
     ("a" (jongseong-ieung       . 1))
     ("s" (jongseong-nieun       . 3))
     ("d" (jungseong-i	    	 . (1 4)))
     ("f" (jungseong-a	    	 . (1 4)))
     ("g" (jungseong-eu	    	 . 3))
     ("h" (choseong-nieun	 . 1))
     ("j" (choseong-ieung	 . 1))
     ("k" (choseong-giyeog       . (3 5)))
     ("l" (choseong-jieuj	 . (3 5)))
     (";" (choseong-bieub	 . (3 5)))
     ("'" (choseong-tieut	 . 1))
     ("z" (jongseong-mieum       . (1 4)))
     ("x" (jongseong-giyeog      . (3 4 5)))
     ("c" (jungseong-e	    	 . (1 4)))
     ("v" (jungseong-o	    	 . 3))
     ("b" (jungseong-u	    	 . 3))
     ("n" (choseong-sios	 . (3 5)))
     ("m" (choseong-hieuh	 . 1))
     ("/" (jungseong-o	         . 3))
     ;; Shifted keys
     ("!" (jongseong-jieuj       . (1 4)))
     ("Q" (jongseong-pieup       . (1 4)))
     ("W" (jongseong-tieut       . (1 4)))
     ("E" (jongseong-kieuk       . 1))
     ("R" (jungseong-yae	 . 1))
     ("T" . ";")
     ("Y" . "<")
     ("U" . "7")
     ("I" . "8")
     ("O" . "9")
     ("P" . ">")
     ("A" (jongseong-digeud      . 1))
     ("S" (jongseong-nieunhieuh  . 4))
     ("D" (jongseong-rieulgiyeog . 4))
     ("F" (jongseong-ssanggiyeog . 5))
     ("G" . "/")
     ("H" . "'")
     ("J" . "4")
     ("K" . "5")
     ("L" . "6")
     ("Z" (jongseong-chieuch     . 1))
     ("X" (jongseong-bieubsios   . 4))
     ("C" (jongseong-rieulmieum  . 4))
     ("V" (jongseong-rieulhieuh  . 4))
     ("B" . "!")
     ("N" . "0")
     ("M" . "1")
     ("<" . "2")
     (">" . "3"))))

(define byeoru-layout-no-shift
  (byeoru-expand-layout
   ;; Unshifted keys
   '(("`" . 183)
     ("1" (jongseong-hieuh     . (1 4)))
     ("2" (jongseong-ssangsios . 5))
     ("3" (jongseong-bieub     . (3 4)))
     ("4" (jungseong-yo	       . 1))
     ("5" (jungseong-yu	       . 1))
     ("6" (jungseong-ya	       . 1))
     ("7" (jungseong-ye	       . 1))
     ("8" (jungseong-ui	       . 4))
     ("9" (choseong-kieuk      . 1))
     ("0" (jungseong-yae       . 1))
     ("-" (jongseong-jieuj     . (1 4)))
     ("=" (jongseong-chieuch   . 1))
     ("q" (jongseong-sios      . (3 4 5)))
     ("w" (jongseong-rieul     . 3))
     ("e" (jungseong-yeo       . 1))
     ("r" (jungseong-ae	       . (1 4)))
     ("t" (jungseong-eo	       . (1 4)))
     ("y" (choseong-rieul      . 1))
     ("u" (choseong-digeud     . (3 5)))
     ("i" (choseong-mieum      . 1))
     ("o" (choseong-chieuch    . 1))
     ("p" (choseong-pieup      . 1))
     ("[" (jongseong-tieut     . (1 4)))
     ("]" (jongseong-pieup     . (1 4)))
     ("\\" (jongseong-kieuk    . 1))
     ("a" (jongseong-ieung     . 1))
     ("s" (jongseong-nieun     . 3))
     ("d" (jungseong-i	       . (1 4)))
     ("f" (jungseong-a	       . (1 4)))
     ("g" (jungseong-eu	       . 3))
     ("h" (choseong-nieun      . 1))
     ("j" (choseong-ieung      . 1))
     ("k" (choseong-giyeog     . (3 5)))
     ("l" (choseong-jieuj      . (3 5)))
     (";" (choseong-bieub      . (3 5)))
     ("'" (choseong-tieut      . 1))
     ("z" (jongseong-mieum     . (1 4)))
     ("x" (jongseong-giyeog    . (3 4 5)))
     ("c" (jungseong-e	       . (1 4)))
     ("v" (jungseong-o	       . 3))
     ("b" (jungseong-u	       . 3))
     ("n" (choseong-sios       . (3 5)))
     ("m" (choseong-hieuh      . 1))
     ("/" (jongseong-digeud    . 1))
     ;; Shifted keys
     ("Q" (jongseong-sios      . (3 4 5)))
     ("W" (jongseong-rieul     . 3))
     ("E" (jungseong-yeo       . 1))
     ("R" (jungseong-ae	       . (1 4)))
     ("T" . ";")
     ("Y" . "<")
     ("U" . "7")
     ("I" . "8")
     ("O" . "9")
     ("P" . ">")
     ("A" (jongseong-ieung     . 1))
     ("S" . "[")
     ("D" . "]")
     ("F" (jungseong-a	       . (1 4)))
     ("G" . "/")
     ("H" . "'")
     ("J" . "4")
     ("K" . "5")
     ("L" . "6")
     ("Z" . "-")
     ("X" . "=")
     ("C" . "\\")
     ("V" (jungseong-o	       . 3))
     ("B" . "!")
     ("N" . "0")
     ("M" . "1")
     ("<" . "2")
     (">" . "3"))))

(define-record 'byeoru-automata
  '((state		      (start . 0))
    (choices-history	      ())
    (unsorted-choices-history ())
    (chosen-jamos	      ())
    (composing-char	      (0 0 0))
    (composed-char	      (0 0 0))))

(define (byeoru-choseong? jamo)
  (assoc jamo byeoru-choseong-alist))

(define (byeoru-jungseong? jamo)
  (assoc jamo byeoru-jungseong-alist))

(define (byeoru-jongseong? jamo)
  (assoc jamo byeoru-jongseong-alist))

(define (byeoru-compound? jamo)
  (find (lambda (item) (eq? jamo (cdr item)))
	byeoru-compound-jamo-alist))
	 
(define (byeoru-double? jamo)
  (find (lambda (item) (eq? jamo (cdr item)))
	byeoru-double-jamo-alist))

(define (byeoru-combine-compound jamo1 jamo2)
  (let ((entry (assoc (cons jamo1 jamo2) byeoru-compound-jamo-alist)))
    (and entry (cdr entry))))

(define (byeoru-combine-double jamo1 jamo2)
  (let ((entry (assoc (cons jamo1 jamo2) byeoru-double-jamo-alist)))
    (and entry (cdr entry))))

(define (byeoru-combine-comp-or-double jamo1 jamo2)
  (if (eq? jamo1 jamo2)
      (byeoru-combine-double jamo1 jamo2)
      (byeoru-combine-compound jamo1 jamo2)))

(define (byeoru-jamo-class jamo)
  (cond
   ((byeoru-choseong?  jamo) 'choseong)
   ((byeoru-jungseong? jamo) 'jungseong)
   ((byeoru-jongseong? jamo) 'jongseong)))

(define (byeoru-jamos-to-johab jamos)
  (let* ((jamos (reverse jamos))
	 (get-johab-code
	  (lambda (class-test alist)
	    (cond
	     ((null? jamos)
	      0)
	     ((class-test (car jamos))
	      (let ((code (cdr (assq
				(if (and (not (null? (cdr jamos)))
					 (class-test (cadr jamos)))
				    (let ((j (byeoru-combine-comp-or-double
					      (car jamos) (cadr jamos))))
				      (set! jamos (cdr jamos))
				      j)
				    (car jamos))
				alist))))
		(set! jamos (cdr jamos))
		code))
	     (else
	      0))))
	 (cho  (get-johab-code byeoru-choseong?  byeoru-choseong-alist))
	 (jung (get-johab-code byeoru-jungseong? byeoru-jungseong-alist))
	 (jong (get-johab-code byeoru-jongseong? byeoru-jongseong-alist)))
    (list cho jung jong)))

(define (byeoru-transition-allowed? state dest)
  (let ((allowed (assoc state byeoru-transition-alist)))
    (member dest (cdr allowed))))

(define (byeoru-comp-or-double-forbidden? state dest)
  (let ((state-class (car state))
	(state-no (cdr state))
	(dest-class (car dest)))
    (and (eq? state-class dest-class)
	 (or (= state-no 2) (= state-no 3)))))

(define (byeoru-automata-reset! ba)
  (byeoru-automata-set-state! ba '(start . 0))
  (byeoru-automata-set-chosen-jamos! ba '())
  (byeoru-automata-set-choices-history! ba '())
  (byeoru-automata-set-composing-char! ba '(0 0 0))
  (byeoru-automata-set-unsorted-choices-history! ba '()))

(define (byeoru-automata-eat-ordered-key ba choices)
  ;; A few jamo and state choices are assigned to each key.  For
  ;; example, '((choseong-giyeog . 1) (jongseong-giyeog . 3)
  ;; (jongseong-giyeog . 4))) are assigned to "r" in the hangul2
  ;; layout, which means that "r" key can be interpreted as one of the
  ;; these three possibilities.  They are tried in the order from left
  ;; to right, and the first one that can be used for continuing
  ;; syllable composition is used.  If no choice can be used for
  ;; composition, the syllable is completed, and composition of a new
  ;; syllable begins.
  (let loop ((chs choices))
    (let* ((state (byeoru-automata-state ba))
	   (state-class (car state))
	   (state-no (cdr state))
	   (chosen-jamos (byeoru-automata-chosen-jamos ba))
	   (chs-hist (byeoru-automata-choices-history ba)))

      (cond

       ((null? chs)
	;; No valid jamo choice found, so we have to break the syllable.
	(if (and (byeoru-jungseong? (caar choices))
		 (eq? state-class 'jongseong)
		 (memv state-no '(1 3 4))
		 (byeoru-choseong? (caar (car chs-hist))))
	    ;; A 2-beol layout may give rise to a transformation like
	    ;; (consonant vowel consonant) + vowel
	    ;; => (consonant vowel) (consonant + vowel)
	    (let ((last-choices (car chs-hist)))
	      (byeoru-automata-backspace ba)
	      (byeoru-automata-set-composed-char!
	       ba (byeoru-jamos-to-johab (byeoru-automata-chosen-jamos ba)))
	      (byeoru-automata-reset! ba)
	      (byeoru-automata-eat-ordered-key ba last-choices)
	      (byeoru-automata-eat-ordered-key ba choices))
	    ;; For a 3-beol layout, just begin a new syllable with
	    ;; the new key.
	    (begin
	      (byeoru-automata-set-composed-char!
	       ba (byeoru-automata-composing-char ba))
	      (byeoru-automata-reset! ba)
	      (byeoru-automata-eat-ordered-key ba choices)))

	'char-break)

       ((let* ((ch (car chs))
	       (jamo (car ch))
	       (dest-no (cdr ch))
	       (p-dest-class (byeoru-jamo-class jamo))
	       (p-dest (cons p-dest-class (if (= dest-no 5) 4 dest-no))))

	  (define (combine-jongseongs)
	    (let loop1 ((chs1 (car chs-hist)))
	      (cond
	       ((null? chs1)
		#f)
	       ((let ((jamo1 (caar chs1))
		      (no1 (cdar chs1)))
		  (and
		   (byeoru-jongseong? jamo1)
		   (= no1 3)
		   (let loop2 ((chs2 chs))
		     (cond
		      ((null? chs2)
		       #f)
		      ((let ((jamo2 (caar chs2))
			     (no2 (cdar chs2)))
			 (and
			  (byeoru-jongseong? jamo2)
			  (or
			   (and (= no2 4)
				(byeoru-combine-compound jamo1 jamo2))
			   (and (= no2 5)
				(eq? jamo1 jamo2)))
			  (begin
			    (set! chosen-jamos (cons jamo1 (cdr chosen-jamos)))
			    (set! jamo jamo2)
			    (set! p-dest (cons p-dest-class 4))
			    #t))))
		      (else
		       (loop2 (cdr chs2))))))))
	       (else
		(loop1 (cdr chs1))))))

	  (and
	   (byeoru-transition-allowed? state p-dest)
	   (case dest-no
	    ;; dest-no 5 is used to control double-striking composition
	    ;; of a double jamo, separately from composition of a
	    ;; (heterogeneous) compound jamo.
	    ((5)
	     (if (byeoru-double? jamo)
		 ;; a double jamo key cannot be the second key
		 ;; for a double jamo.
		 (not (byeoru-comp-or-double-forbidden? state p-dest))
		 ;; (variable) jamo must be the second key for a double jamo,
		 (and (not (null? chosen-jamos)) ; so, a first key needed,
		      ;; that is the same as (variable) jamo.
		      (eq? (car chosen-jamos) jamo))))
	    ((4)
	     (if (byeoru-compound? jamo)
		 ;; a compound jamo key cannot be the second key
		 ;; for a compound jamo.
		 (not (byeoru-comp-or-double-forbidden? state p-dest))
		 ;; (variable) jamo must be the second key for a compound,
		 (and (not (null? chosen-jamos)) ; so, a first key needed,
		      ;; that can be combined with (variable) jamo.
		      (byeoru-combine-compound (car chosen-jamos) jamo))))
	    (else #t))
	   (not (and (eq? p-dest-class 'jongseong)
		     (eq? state-class 'choseong)
		     ;; choseong -> jongseong transition is allowed
		     ;; for a 3-beol layout, so that it can compose a
		     ;; syllable of the form (choseong jongseong).  A
		     ;; 2-beol layout cannot do this since it does not
		     ;; distinguish between choseong and jongseong.
		     ;; However, we use this transition for a
		     ;; transformation like
		     ;; (choseong-giyeog) + jongseong-sios =>
		     ;; (jongseong-giyeog jongseong-sios)
		     ;; to enable input of ㄳ with a 2-beol layout.
		     (byeoru-choseong? (caar choices)) ; 2-beol layout?
		     (or (equal? state '(choseong . 4))
			 ;; do not allow something like
			 ;; (choseong-giyeog choseong-giyeog) +
			 ;; jongseong-giyeog => (choseong-giyeog
			 ;; jongseong-giyeog jongseong-giyeog) which
			 ;; might happen in romaja layout mode.
			 (not (combine-jongseongs)))))

	   ;; A valid jamo choice found.  Keep composing.
	   (begin
	     (byeoru-automata-set-chosen-jamos! ba (cons jamo chosen-jamos))
	     (byeoru-automata-set-choices-history! ba (cons choices chs-hist))
	     (byeoru-automata-set-state! ba p-dest)
	     (byeoru-automata-set-composing-char!
	      ba (byeoru-jamos-to-johab
		  (byeoru-automata-chosen-jamos ba)))

	     'composing))))

       (else
	(loop (cdr chs)))))))

(define (byeoru-orderedness)
  (let ((can-be-orderless
	 (cadr (assoc byeoru-layout byeoru-layout-alist))))
    (if can-be-orderless byeoru-jamo-orderedness 'ordered)))

(define (byeoru-cmp-class choices1 choices2)
  (let* ((byeoru-class-order
	  (lambda (class)
	    (cdr (assoc class '((choseong  . 1)
				(jungseong . 2)
				(jongseong . 3))))))
	 (jamo1 (caar choices1))
	 (jamo2 (caar choices2))
	 (order1 (byeoru-class-order (byeoru-jamo-class jamo1)))
	 (order2 (byeoru-class-order (byeoru-jamo-class jamo2))))
    (if (= order1 order2)
	(if (eq? jamo1 jamo2)
	    0
	    (cond
	     ((byeoru-combine-compound jamo1 jamo2) -1)
	     ((byeoru-combine-compound jamo2 jamo2)  1)
	     (else 0)))
	(- order1 order2))))

(define (byeoru-insert-choices choices choices-list)
  (if (or (null? choices-list)
	  (>= (byeoru-cmp-class choices (car choices-list)) 0))
      (cons choices choices-list)
      (cons (car choices-list)
	    (byeoru-insert-choices choices (cdr choices-list)))))

(define (byeoru-test-list ba choices-list)
  (let loop ((rev-chs-list (reverse choices-list)))
    (cond
     ((null? rev-chs-list)
      'composing)
     ((eq? (byeoru-automata-eat-ordered-key ba (car rev-chs-list))
	   'char-break)
      'char-break)
     (else
      (loop (cdr rev-chs-list))))))

(define (byeoru-eat-list f ba lst)
  (and (not (null? lst))
       (let loop ((rev-lst (reverse lst)))
	 (let ((res (f ba (car rev-lst))))
	   (if (null? (cdr rev-lst))
	       res
	       (loop (cdr rev-lst)))))))

(define (byeoru-automata-eat-orderless-key ba choices)
  (let ((uch (byeoru-automata-unsorted-choices-history ba))
	(class (byeoru-jamo-class (caar choices))))
    ;; Even though we allow keystroke orders to be interchanged, two
    ;; keystrokes of the same class should be consecutive.  Otherwise,
    ;; we break the syllable.
    (if (and (memq class (map (lambda (elm)
				(byeoru-jamo-class (caar elm))) uch))
	     (not (eq? class (byeoru-jamo-class (caar (car uch)))))
	     ;; But, in more-orderless mode, we only require that two
	     ;; keystrokes of choseong class be consecutive, for
	     ;; syllable breaks to be well defined.  All other
	     ;; disorders are allowed.
	     (or (not (eq? (byeoru-orderedness) 'more-orderless))
		 (eq? class 'choseong)))
	(byeoru-automata-eat-ordered-key ba choices)
	(let* ((chs-hist (byeoru-automata-choices-history ba))
	       (new-chs-hist (cons choices chs-hist))
	       (new-sorted-chs-hist (byeoru-insert-choices
				     choices chs-hist))
	       (res (begin
		      (byeoru-automata-reset! ba)
		      (byeoru-test-list ba new-sorted-chs-hist))))
	  (if (eq? res 'char-break)
	      (begin
		(byeoru-automata-reset! ba)
		(byeoru-eat-list
		 byeoru-automata-eat-ordered-key ba new-chs-hist)))
	  res))))

(define (byeoru-automata-eat-key ba choices)
  (let ((uch (byeoru-automata-unsorted-choices-history ba))
	(res
	 (case (byeoru-orderedness)
	   ((ordered)
	    (byeoru-automata-eat-ordered-key ba choices))
	   ((orderless more-orderless)
	    (byeoru-automata-eat-orderless-key ba choices)))))
    (byeoru-automata-set-unsorted-choices-history!
     ba (if (eq? res 'char-break)
	    (byeoru-automata-choices-history ba)
	    (cons choices uch)))
    res))

(define (byeoru-automata-backspace ba)
  (let ((chs-hist (byeoru-automata-choices-history ba)))
    (and (not (null? chs-hist))
	 (let ((new-chs-hist (cdr chs-hist)))
	   (byeoru-automata-reset! ba)
	   (byeoru-eat-list byeoru-automata-eat-ordered-key ba new-chs-hist)
	   (byeoru-automata-set-unsorted-choices-history! ba new-chs-hist)
	   #t))))


;;; ----------------------------
;;;  Hangul encoding in Unicode
;;; ----------------------------

;; Hangul choseong giyeog, U+1100.
(define byeoru-ucs-code-choseong-giyeog 4352)

;; Hangul jungseong a, U+1161.
(define byeoru-ucs-code-jungseong-a 4449)

;; Hangul jongseong giyeog, U+11A8.
(define byeoru-ucs-code-jongseong-giyeog 4520)

;; Hangul choseong filler, U+115F.
(define byeoru-ucs-code-choseong-filler 4447)

;; Hangul jungseong filler, U+1160.
(define byeoru-ucs-code-jungseong-filler 4448)

;; Hangul syllables block begins at U+AC00, 가.
(define byeoru-ucs-code-ga 44032)

;; What I call johab here is not related to the KSSM combination
;; (johab) code, but is a list having the form (cho jung jong), where
;; each element is the number listed in
;; byeoru-{cho,jung,jong}seong-alist.
(define (byeoru-johab-to-ucs johab)
  (let ((cho (car johab))
	(jung (cadr johab))
	(jong (nth 2 johab)))
    (+ byeoru-ucs-code-ga (* (- cho 1) 21 28) (* (- jung 1) 28) jong)))

;; This is the way an isolated jamo is encoded in the Unicode standard.
;; However, it doesn't seem to be well supported currently.
(define byeoru-choseong-jamo-utf8-list
  (map ucs-to-utf8-string
       (cons byeoru-ucs-code-choseong-filler
	     (list-tabulate
	      19 (lambda (n) (+ n byeoru-ucs-code-choseong-giyeog))))))

(define byeoru-jungseong-jamo-utf8-list
  (map ucs-to-utf8-string
       (cons byeoru-ucs-code-jungseong-filler
	     (list-tabulate
	      21 (lambda (n) (+ n byeoru-ucs-code-jungseong-a))))))

(define byeoru-jongseong-jamo-utf8-list
  (cons "" (map ucs-to-utf8-string
		(list-tabulate
		 27 (lambda (n) (+ n byeoru-ucs-code-jongseong-giyeog))))))

;; So we show an incomplete syllable as a sequence of
;; Hangul compatibility jamos by default.
(define byeoru-choseong-compatibility-jamo-utf8-list
  (cons "" (map ucs-to-utf8-string
		'(12593 12594 12596 12599 12600 12601 12609 12610 12611 12613
		  12614 12615 12616 12617 12618 12619 12620 12621 12622))))

(define byeoru-jungseong-compatibility-jamo-utf8-list
  (cons "" (map ucs-to-utf8-string
		'(12623 12624 12625 12626 12627 12628 12629 12630 12631 12632
		  12633 12634 12635 12636 12637 12638 12639 12640 12641 12642
		  12643))))

(define byeoru-jongseong-compatibility-jamo-utf8-list
  (cons "" (map ucs-to-utf8-string
		'(12593 12594 12595 12596 12597 12598 12599 12601 12602 12603
		  12604 12605 12606 12607 12608 12609 12610 12612 12613 12614
		  12615 12616 12618 12619 12620 12621 12622))))

(define (byeoru-johab-to-utf8-string johab)
  (let ((cho (car johab))
	(jung (cadr johab))
	(jong (nth 2 johab)))
    (cond
     ((and (= cho 0) (= jung 0) (= jong 0))
      "")
     ;; We are basically using Normalization Form C.
     ((and (not (= cho 0)) (not (= jung 0)))
      (ucs-to-utf8-string (byeoru-johab-to-ucs johab)))
     (else
      (let ((cho-l (if byeoru-compatibility-jamos-for-incomplete-syllables?
		       byeoru-choseong-compatibility-jamo-utf8-list
		       byeoru-choseong-jamo-utf8-list))
	    (jung-l (if byeoru-compatibility-jamos-for-incomplete-syllables?
			byeoru-jungseong-compatibility-jamo-utf8-list
			byeoru-jungseong-jamo-utf8-list))
	    (jong-l (if byeoru-compatibility-jamos-for-incomplete-syllables?
			byeoru-jongseong-compatibility-jamo-utf8-list
			byeoru-jongseong-jamo-utf8-list)))
	(string-append
	 (nth cho cho-l) (nth jung jung-l) (nth jong jong-l)))))))


;;; ------------------------
;;;  Input context handlers
;;; ------------------------

(define byeoru-romaja-rule
  (byeoru-expand-layout
   '(((("g"))         ((choseong-giyeog . (3 5)) (jongseong-giyeog . (3 4 5))))
     ;; gg, dd, bb, vv, ss, jj, zz are composed by automata.
     ((("k" "k"))     ((choseong-ssanggiyeog . 1) (jongseong-ssanggiyeog . 5)))
     ((("q" "q"))     ((choseong-ssanggiyeog . 1) (jongseong-ssanggiyeog . 5)))
     ((("c")) 	      ((choseong-ssanggiyeog . 1) (jongseong-ssanggiyeog . 5)))
     ((("n")) 	      ((choseong-nieun       . 1) (jongseong-nieun       . 3)))
     ((("d")) 	      ((choseong-digeud  . (3 5)) (jongseong-digeud      . 1)))
     ((("t" "t"))     ((choseong-ssangdigeud . 1)))
     ((("r")) 	      ((choseong-rieul       . 1) (jongseong-rieul       . 3)))
     ((("l")) 	      ((choseong-rieul       . 1) (jongseong-rieul       . 3)))
     ((("m")) 	      ((choseong-mieum       . 1) (jongseong-mieum   . (1 4))))
     ((("b")) 	      ((choseong-bieub   . (3 5)) (jongseong-bieub   . (3 4))))
     ((("v")) 	      ((choseong-bieub   . (3 5)) (jongseong-bieub   . (3 4))))
     ((("p" "p"))     ((choseong-ssangbieub  . 1)))
     ((("f" "f"))     ((choseong-ssangbieub  . 1)))
     ((("s"))         ((choseong-sios    . (3 5)) (jongseong-sios  . (3 4 5))))
     ((("x"))         ((choseong-ieung       . 1)))
     ((("n" "g"))     ((jongseong-ieung      . 1)))
     ((("j"))         ((choseong-jieuj   . (3 5)) (jongseong-jieuj   . (1 4))))
     ((("z"))         ((choseong-jieuj   . (3 5)) (jongseong-jieuj   . (1 4))))
     ((("c" "h"))     ((choseong-chieuch     . 1) (jongseong-chieuch 	 . 1)))
     ((("k"))         ((choseong-kieuk       . 1) (jongseong-kieuk   	 . 1)))
     ((("q"))         ((choseong-kieuk       . 1) (jongseong-kieuk   	 . 1)))
     ((("t"))         ((choseong-tieut       . 1) (jongseong-tieut   . (1 4))))
     ((("p"))         ((choseong-pieup       . 1) (jongseong-pieup   . (1 4))))
     ((("f"))         ((choseong-pieup       . 1) (jongseong-pieup   . (1 4))))
     ((("h"))         ((choseong-hieuh       . 1) (jongseong-hieuh   . (1 4))))
     ((("a"))         ((jungseong-a          . 1)))
     ((("a" "e"))     ((jungseong-ae         . 1)))
     ((("y" "a"))     ((jungseong-ya         . 1)))
     ((("i" "a"))     ((jungseong-ya         . 1)))
     ((("y" "a" "e")) ((jungseong-yae        . 1)))
     ((("i" "a" "e")) ((jungseong-yae        . 1)))
     ((("e" "o"))     ((jungseong-eo         . 1)))
     ((("e"))         ((jungseong-e          . 1)))
     ((("y" "e" "o")) ((jungseong-yeo        . 1)))
     ((("i" "e" "o")) ((jungseong-yeo        . 1)))
     ((("y" "e"))     ((jungseong-ye         . 1)))
     ((("i" "e"))     ((jungseong-ye         . 1)))
     ((("o"))         ((jungseong-o          . 1)))
     ((("w" "a"))     ((jungseong-wa	     . 4)))
     ((("u" "a"))     ((jungseong-wa	     . 4)))
     ((("o" "a"))     ((jungseong-wa	     . 4)))
     ((("w" "a" "e")) ((jungseong-wae	     . 4)))
     ((("u" "a" "e")) ((jungseong-wae	     . 4)))
     ((("o" "a" "e")) ((jungseong-wae	     . 4)))
     ((("o" "e"))     ((jungseong-oe	     . 4)))
     ((("w" "o" "e")) ((jungseong-oe	     . 4)))
     ((("u" "o" "e")) ((jungseong-oe	     . 4)))
     ((("o" "i"))     ((jungseong-oe 	     . 4)))
     ((("y" "o"))     ((jungseong-yo 	     . 1)))
     ((("i" "o"))     ((jungseong-yo 	     . 1)))
     ((("u"))         ((jungseong-u  	     . 1)))
     ((("w"))         ((jungseong-u  	     . 1)))
     ((("o" "o"))     ((jungseong-u  	     . 1)))
     ((("w" "o"))     ((jungseong-wo 	     . 4)))
     ((("w" "e" "o")) ((jungseong-wo 	     . 4))) ; Not present in HWP.
     ((("u" "o"))     ((jungseong-wo 	     . 4)))
     ((("w" "e"))     ((jungseong-we 	     . 4)))
     ((("u" "e"))     ((jungseong-we 	     . 4)))
     ((("w" "i"))     ((jungseong-wi 	     . 4)))
     ((("y" "u"))     ((jungseong-yu 	     . 1)))
     ((("i" "u"))     ((jungseong-yu 	     . 1)))
     ((("e" "u"))     ((jungseong-eu 	     . 1)))
     ((("u" "i"))     ((jungseong-ui 	     . 4)))
     ((("e" "u" "i")) ((jungseong-ui 	     . 4)))
     ((("i"))         ((jungseong-i  	     . 1)))
     ((("y"))         ((jungseong-i  	     . 1)))
     ((("e" "e"))     ((jungseong-i  	     . 1))))
   list))

(define byeoru-context-rec-spec
  (append
   context-rec-spec
   (list
    (list 'on              #f)
    (list 'automata	   #f)
    (list 'rkc             #f)		; for romaja input.
    (list 'key-hist        '())
    (list 'commit-by-word? byeoru-commit-by-word?)
    (list 'word-ustr	   #f)
    (list 'convl-ustr	   #f)
    (list 'convr-ustr	   #f)
    (list 'preedit	   '())
    (list 'mode		   'hangul)
    (list 'dic-entry	   #f)
    (list 'cands	   #f)
    (list 'cand-no	   0)
    (list 'menu-no	   0)
    (list 'cache	   '())
    )))
(define-record 'byeoru-context byeoru-context-rec-spec)
(define byeoru-context-new-internal byeoru-context-new)

(define (byeoru-context-new id im)
  (let ((bc (byeoru-context-new-internal id im)))
    (byeoru-context-set-widgets! bc byeoru-widgets)
    (byeoru-context-set-automata! bc (byeoru-automata-new))
    (byeoru-context-set-rkc! bc (rk-context-new byeoru-romaja-rule #f #f))
    (byeoru-context-set-word-ustr! bc (ustr-new '()))
    (byeoru-context-set-convl-ustr! bc (ustr-new '()))
    (byeoru-context-set-convr-ustr! bc (ustr-new '()))
    bc))

(define (byeoru-flush-automata bc)
  (let* ((ba (byeoru-context-automata bc))
	 (composing (byeoru-johab-to-utf8-string
		     (byeoru-automata-composing-char ba))))
    (if (not (string=? composing ""))
	(begin
	  (ustr-insert-elem! (byeoru-context-word-ustr bc) composing)
	  (byeoru-automata-reset! ba)))
    (rk-flush (byeoru-context-rkc bc))
    (byeoru-context-set-key-hist! bc '())))

(define (byeoru-make-whole-string bc)
  (let ((word (byeoru-context-word-ustr bc)))
    (apply string-append (ustr-whole-seq word))))

(define (byeoru-clear! bc)
  (ustr-clear! (byeoru-context-word-ustr bc))
  (byeoru-context-set-mode! bc 'hangul))

(define (byeoru-commit bc str)
  (if (not (string=? str "")) (im-commit bc str)))

(define (byeoru-flush bc)
  (byeoru-flush-automata bc)
  (byeoru-commit bc (byeoru-make-whole-string bc))
  (byeoru-clear! bc))

(define (byeoru-prepare-activation bc)
  (byeoru-flush bc)
  (byeoru-update-preedit bc))

(register-action 'action_byeoru_direct
		 (lambda (bc)
		   '(ko_direct
		     "A"
		     ;; Change this to a more reasonable name.
		     "영문"
		     "영문 입력모드"))
		 (lambda (bc)
		   (not (byeoru-context-on bc)))
		 (lambda (bc)
		   (byeoru-prepare-activation bc)
		   (byeoru-context-set-on! bc #f)))

(register-action 'action_byeoru_hangulchar
		 (lambda (bc)
		   '(ko_hangulchar
		     "가"
		     "한글 글자"
		     "한글 글자단위 입력모드"))
		 (lambda (bc)
		   (and (byeoru-context-on bc)
			(not (byeoru-context-commit-by-word? bc))))
		 (lambda (bc)
		   (byeoru-prepare-activation bc)
		   (byeoru-context-set-on! bc #t)
		   (byeoru-context-set-commit-by-word?! bc #f)))

(register-action 'action_byeoru_hangulword
		 (lambda (bc)
		   '(ko_hangulword
		     "단"
		     "한글 단어"
		     "한글 단어단위 입력모드"))
		 (lambda (bc)
		   (and (byeoru-context-on bc)
			(byeoru-context-commit-by-word? bc)))
		 (lambda (bc)
		   (byeoru-prepare-activation bc)
		   (byeoru-context-set-on! bc #t)
		   (byeoru-context-set-commit-by-word?! bc #t)))

(define byeoru-input-mode-actions
  '(action_byeoru_direct
    action_byeoru_hangulchar
    action_byeoru_hangulword))

(define byeoru-widgets '(widget_byeoru_input_mode))

(define default-widget_byeoru_input_mode 'action_byeoru_direct)

(register-widget 'widget_byeoru_input_mode
		 (activity-indicator-new byeoru-input-mode-actions)
		 (actions-new byeoru-input-mode-actions))

(define (byeoru-init-handler id im arg)
  (byeoru-context-new id im))

;; Test that the input is not control-purpose but graphical character.
;; This procedure is needed since byeoru-layout alists do not have
;; modifier key information other than Shift.
;;
;; TODO:
;; - CHECK: is this a right way to check shift-only?
(define byeoru-non-control-key?
  (let ((shift-or-no-modifier? (make-key-predicate '("<Shift>" ""))))
    (lambda (key key-state)
      (shift-or-no-modifier? -1 key-state))))

(define (byeoru-key-to-choices key key-state)
  (and (byeoru-non-control-key? key key-state)
       (let* ((layout (symbol-value byeoru-layout))
	      (pressed-key
	       (charcode->string
		;; avoid case change due to caps lock.
		(if (shift-key-mask key-state)
		    (ichar-upcase key) (ichar-downcase key))))
	      (entry (assoc pressed-key layout)))
	 (and entry
	      (let ((choices (cdr entry)))
		(if (number? choices)
		    (ucs-to-utf8-string choices)
		    choices))))))

(define byeoru-dic-filename "byeoru-dic.scm")
(define byeoru-load-dic-hook '())

(define (byeoru-add-hook hook-sym proc)
  (set-symbol-value! hook-sym (cons proc (symbol-value hook-sym))))

(define (byeoru-call-hook-procs hook)
  (for-each (lambda (proc) (proc)) hook))

(define (byeoru-look-up-dic word)
  (if (not (symbol-bound? 'byeoru-dic))
      (begin
	(require byeoru-dic-filename)
	(byeoru-call-hook-procs byeoru-load-dic-hook)))
  (assoc word byeoru-dic))

(define (byeoru-add-dic-entry kons)
  (let* ((id (car kons))
	 (found (assoc id byeoru-dic)))

    (define (update-cands cands new-cands)
      (fold
       (lambda (new lis)
	 (let ((new-str (if (pair? new) (car new) new)))
	   (cons new
		 (remove
		  (lambda (elm)
		    (let ((elm-str (if (pair? elm) (car elm) elm)))
		      (string=? elm-str new-str))) lis))))
       cands new-cands))

    (if found
	(set-cdr! found (update-cands (cdr found) (reverse (cdr kons))))
	(set! byeoru-dic (cons kons byeoru-dic)))))

(define (byeoru-begin-conv bc)
  (byeoru-flush-automata bc)
  (let* ((word  (byeoru-context-word-ustr  bc))
	 (convl (byeoru-context-convl-ustr bc))
	 (convr (byeoru-context-convr-ustr bc))
	 (entry (begin
		  (ustr-set-whole-seq! convl (ustr-former-seq word))
		  (ustr-cursor-move-beginning! convl)
		  (let loopl ()
		    (cond
		     ((ustr-cursor-at-end? convl)
		      #f)
		     ((begin
			(ustr-set-whole-seq! convr (ustr-latter-seq convl))
			(let loopr ()
			  (cond
			   ((ustr-cursor-at-beginning? convr)
			    #f)
			   ((byeoru-look-up-dic
			     (apply string-append (ustr-former-seq convr))))
			   (else
			    (ustr-cursor-move-backward! convr)
			    (loopr))))))
		     (else
		      (ustr-cursor-move-forward! convl)
		      (loopl)))))))
    (and entry
	 (let ((max (- (length entry) 1)))
	   (byeoru-context-set-dic-entry! bc entry)
	   (byeoru-context-set-cands! bc (cdr entry))
	   (byeoru-context-set-mode! bc 'conv)
	   (byeoru-update-preedit bc)
	   ;; CHECK: is the following statement true?
	   ;; We should update the preedit to place the candidate window
	   ;; at a correct position.
	   (im-activate-candidate-selector bc max byeoru-nr-candidate-max)
	   (byeoru-context-set-cand-no! bc 0)
	   (im-select-candidate bc 0)
	   #t))))

(define (byeoru-break-char bc)
  (let ((ba (byeoru-context-automata bc)))
    (ustr-insert-elem! (byeoru-context-word-ustr bc)
		       (byeoru-johab-to-utf8-string
			(byeoru-automata-composed-char ba)))
    (if (not (byeoru-context-commit-by-word? bc))
	(begin
	  (byeoru-commit bc (byeoru-make-whole-string bc))
	  (byeoru-clear! bc)))))

;; Yes, I know this routine is ugly, but it works!
;; This procedure uses an rk to translate, according to
;; byeoru-romaja-rule, a sequence of romaja keys to a list of possible
;; jamos, which is fed into a Hangul automata.  When a new romaja key
;; is pressed, the last-pressed key in the automata is backspaced and
;; the updated key from the rk is pushed into the automata, until the
;; rk sequence can grow no longer.  It keeps track of the history of
;; romaja key presses since the backspace key is supposed to delete a
;; romaja, not a jamo.
(define (byeoru-feed-romaja-key bc key key-state)

  (define (flush-automata)
    (byeoru-flush-automata bc)
    (if (not (byeoru-context-commit-by-word? bc))
	(begin
	  (byeoru-commit bc (byeoru-make-whole-string bc))
	  (byeoru-clear! bc))))

  (and
   (byeoru-non-control-key? key key-state)
   (begin
     ;; Shift key forces a syllable under composition to be completed.
     ;; E.g., gagga becomes 각가,
     ;; while gaGga becomes 가까.
     (if (shift-key-mask key-state)
	 (flush-automata))
     (let* ((ba (byeoru-context-automata bc))
	    (first-key? (null? (byeoru-context-key-hist bc)))
	    (rkc (byeoru-context-rkc bc))
	    (last-pend (rk-pending rkc))
	    (last-seq (rk-context-seq rkc))
	    (key-str (charcode->string (ichar-downcase key)))
	    (res (rk-push-key! rkc key-str))
	    (pend (rk-pending rkc))
	    (cur-seq (rk-current-seq rkc))
	    (choices (and cur-seq (cadr cur-seq))))

       (define (byeoru-prepend-ieung)
	 (byeoru-automata-backspace ba)
	 (byeoru-automata-eat-key ba '((choseong-ieung . 1)))
	 (byeoru-automata-eat-key ba choices))

       (and
	(not (string=? pend ""))
	(list? choices)
	(let ((jungseong? (byeoru-jungseong? (caar choices))))
	  (if (not res) (byeoru-automata-backspace ba))
	  (if (and jungseong? (string=? last-pend "ng"))
	      ;; Note that HWP does not treat "ch" in this way.
	      ;; E.g., gochi becomes 고치
	      ;; while songi becomes 손기.
	      (begin
		(byeoru-automata-backspace ba)
		(byeoru-automata-eat-key ba '((jongseong-nieun . 1)))
		(flush-automata)
		(byeoru-automata-eat-key ba '((choseong-giyeog . 1)))
		(byeoru-context-set-key-hist! bc '(103))
		(rk-push-key! rkc key-str)))
	  (if (eq? (byeoru-automata-eat-key ba choices) 'char-break)
	      (begin
		(byeoru-break-char bc)
		(byeoru-context-set-key-hist! bc '())
		(if jungseong?
		    (if (= (length (byeoru-automata-chosen-jamos ba)) 1)
			(byeoru-prepend-ieung)
			(byeoru-context-set-key-hist!
			 bc (if (string=? last-pend "ch")
				'(104 99)
				(list (string->charcode (car last-seq))))))))
	      (if (and jungseong? first-key?
		       (not (and byeoru-shifted-romaja-isolates-vowel?
				 (shift-key-mask key-state))))
		  (byeoru-prepend-ieung)))
	  (byeoru-context-set-key-hist!
	   bc (cons key (byeoru-context-key-hist bc)))
	  #t))))))
  
(define (byeoru-backspace-romaja bc)
  (let ((key-hist (byeoru-context-key-hist bc)))
    (and (not (null? key-hist))
	 (begin
	   (byeoru-automata-reset! (byeoru-context-automata bc))
	   (rk-flush (byeoru-context-rkc bc))
	   (byeoru-context-set-key-hist! bc '())
	   (let loop ((rev-key-hist (reverse (cdr key-hist))))
	     (or (null? rev-key-hist)
		 (begin
		   (byeoru-feed-romaja-key bc (car rev-key-hist) 0)
		   (loop (cdr rev-key-hist)))))))))

(define (byeoru-feed-hangul-key bc key key-state)
  (let ((choices (byeoru-key-to-choices key key-state)))
    (and (list? choices)
	 (begin
	   (if (eq? (byeoru-automata-eat-key (byeoru-context-automata bc)
					     choices)
		    'char-break)
	       (byeoru-break-char bc))
	   #t))))

(define (byeoru-proc-input-state-with-preedit bc key key-state)
  (let* ((word (byeoru-context-word-ustr bc))
	 (by-word? (byeoru-context-commit-by-word? bc)))

    (define (commit-former-string)
      (byeoru-commit bc (apply string-append (ustr-former-seq word)))
      (ustr-clear-former! word))

    (cond

     ;; Hangul mode off.
     ((or (byeoru-latin-key? key key-state)
	  (and byeoru-esc-turns-off? (eq? key 'escape)))
      (byeoru-flush bc)
      (if (eq? key 'escape)
	  (im-commit-raw bc))
      (byeoru-context-set-on! bc #f))

     ((byeoru-backspace-key? key key-state)
      (if (not (if (eq? byeoru-layout 'byeoru-layout-romaja)
		   (byeoru-backspace-romaja bc)
		   (byeoru-automata-backspace (byeoru-context-automata bc))))
	  (ustr-cursor-delete-backside! word)))

     ((and (byeoru-delete-key? key key-state) by-word?)
      (byeoru-flush-automata bc)
      (if (ustr-cursor-at-end? word)
	  (begin
	    (byeoru-commit bc (byeoru-make-whole-string bc))
	    (byeoru-clear! bc)
	    (im-commit-raw bc))
	  (ustr-cursor-delete-frontside! word)))

     ((and (byeoru-go-left-key? key key-state) by-word?)
      (byeoru-flush-automata bc)
      (ustr-cursor-move-backward! word))

     ((and (byeoru-go-right-key? key key-state) by-word?)
      (byeoru-flush-automata bc)
      (if (ustr-cursor-at-end? word)
	  (begin
	    (byeoru-commit bc (byeoru-make-whole-string bc))
	    (byeoru-clear! bc)
	    (im-commit-raw bc))
	  (ustr-cursor-move-forward! word)))

     ((and (byeoru-beginning-of-preedit-key? key key-state) by-word?)
      (byeoru-flush-automata bc)
      (ustr-cursor-move-beginning! word))

     ((and (byeoru-end-of-preedit-key? key key-state) by-word?)
      (byeoru-flush-automata bc)
      (if (ustr-cursor-at-end? word)
	  (begin
	    (byeoru-commit bc (byeoru-make-whole-string bc))
	    (byeoru-clear! bc)
	    (im-commit-raw bc))
	  (ustr-cursor-move-end! word)))

     ((byeoru-conversion-key? key key-state)
      (if (not (byeoru-begin-conv bc))
	  (commit-former-string)))

     ;; Hangul jamo.
     ((if (eq? byeoru-layout 'byeoru-layout-romaja)
	  (byeoru-feed-romaja-key bc key key-state)
	  (byeoru-feed-hangul-key bc key key-state)))

     ;; Commit the word.
     (else
      (byeoru-flush-automata bc)
      (let ((choices (or (eq? byeoru-layout 'byeoru-layout-romaja)
			 (byeoru-key-to-choices key key-state))))
	(if (string? choices)
	    (begin
	      (ustr-insert-elem! word choices)
	      (commit-former-string))
	    (begin
	      (commit-former-string)
	      (im-commit-raw bc))))))))

(define (byeoru-show-menu bc)
  (let* ((cands (append (byeoru-context-cache bc)
			byeoru-menu-symbols
			'(commit-by-word-switch)))
	 (max (length cands)))
    (byeoru-context-set-cands! bc cands)
    (byeoru-context-set-mode! bc 'menu)
    (im-activate-candidate-selector bc max max)
    (im-select-candidate bc (byeoru-context-menu-no bc))))

(define (byeoru-proc-input-state-no-preedit bc key key-state)
  (cond

   ;; Hangul mode off.
   ((byeoru-latin-key? key key-state)
    (byeoru-context-set-on! bc #f))

   ((byeoru-conversion-key? key key-state)
    (byeoru-show-menu bc))
   
   ;; Hangul jamo.
   ((if (eq? byeoru-layout 'byeoru-layout-romaja)
	(byeoru-feed-romaja-key bc key key-state)
	(byeoru-feed-hangul-key bc key key-state)))
   
   ;; Commit a single key.
   (else
    (let ((choices (or (eq? byeoru-layout 'byeoru-layout-romaja)
		       (byeoru-key-to-choices key key-state))))
      (if (string? choices)
	  (byeoru-commit bc choices)
	  (im-commit-raw bc))
      (if (and byeoru-esc-turns-off? (eq? key 'escape))
	  (byeoru-context-set-on! bc #f))))))

(define (byeoru-has-preedit? bc)
  (let ((ba (byeoru-context-automata bc)))
    (not (and (ustr-empty? (byeoru-context-word-ustr bc))
	      (equal? (byeoru-automata-composing-char ba) '(0 0 0))))))

(define (byeoru-proc-input-state bc key key-state)
  (if (byeoru-has-preedit? bc)
      (byeoru-proc-input-state-with-preedit bc key key-state)
      (byeoru-proc-input-state-no-preedit bc key key-state)))

(define (byeoru-move-candidate bc offset)
  (let* ((cands (byeoru-context-cands bc))
	 (max (length cands))
	 (mode (byeoru-context-mode bc))
	 (n (+ (case mode
		 ((conv symbol) (byeoru-context-cand-no bc))
		 ((menu) (byeoru-context-menu-no bc)))
	       offset))
	 (compensated-n (cond
			 ((>= n max)
			  0)
			 ((< n 0)
			  (- max 1))
			 (else
			  n))))
    (case mode
      ((conv symbol)
       (byeoru-context-set-cand-no! bc compensated-n))
      ((menu)
       (byeoru-context-set-menu-no! bc compensated-n)))
    (im-select-candidate bc compensated-n)))

(define (byeoru-cancel-conv bc)
  (im-deactivate-candidate-selector bc)
  (case (byeoru-context-mode bc)
    ((conv)
     (byeoru-context-set-mode! bc 'hangul)
     (if (not (byeoru-context-commit-by-word? bc))
	 (begin
	   (byeoru-commit bc (byeoru-make-whole-string bc))
	   (byeoru-clear! bc))))
    ((menu)
     (byeoru-context-set-mode! bc 'hangul))
    ((symbol)
     (byeoru-show-menu bc))))

(define (byeoru-commit-converted-part bc)
  (let* ((cands (byeoru-context-cands bc))
	 (cand (nth (byeoru-context-cand-no bc) cands))
	 (entry (byeoru-context-dic-entry bc))
	 (convl (byeoru-context-convl-ustr bc))
	 (convr (byeoru-context-convr-ustr bc))
	 (word (byeoru-context-word-ustr bc)))
    (byeoru-commit bc
		   (apply string-append
			  (append (ustr-former-seq convl)
				  (list (if (pair? cand) (car cand) cand)))))
    (set-cdr! entry (cons cand (delete cand cands eq?)))
    (im-deactivate-candidate-selector bc)
    (byeoru-context-set-mode! bc 'hangul)
    (ustr-set-former-seq! word (ustr-latter-seq convr))))

(define (byeoru-select-menu-or-symbol bc)
  (let* ((cands (byeoru-context-cands bc))
	 (cache (byeoru-context-cache bc)))

    (define (update-cache str)
      (let ((cached (find (lambda (elm) (string=? elm str)) cache)))
	(byeoru-context-set-cache!
	 bc (if cached
		(cons cached (delete cached cache eq?))
		(let ((new-cache (cons str cache)))
		  (if (> (length new-cache) byeoru-symbol-cache-size)
		      (truncate-list new-cache
				     byeoru-symbol-cache-size)
		      new-cache))))))

    (im-deactivate-candidate-selector bc)
    (case (byeoru-context-mode bc)
      ((menu)
       (let ((cand (nth (byeoru-context-menu-no bc) cands)))
	 (cond
	  ((string? cand)
	   (byeoru-commit bc cand)
	   (byeoru-context-set-mode! bc 'hangul)
	   (byeoru-context-set-menu-no! bc 0)
	   (update-cache cand))
	  ((pair? cand)
	   (let ((max (length (cdr cand))))
	     (byeoru-context-set-cands! bc (cdr cand))
	     (byeoru-context-set-mode! bc 'symbol)
	     (im-activate-candidate-selector
	      bc max byeoru-nr-candidate-max)
	     (byeoru-context-set-cand-no! bc 0)
	     (im-select-candidate bc 0)))
	  ((symbol? cand)
	   (byeoru-context-set-commit-by-word?!
	    bc (not (byeoru-context-commit-by-word? bc)))
	   (byeoru-context-set-mode! bc 'hangul)
	   (byeoru-context-set-menu-no! bc 0)))))
      ((symbol)
       (let* ((cand (nth (byeoru-context-cand-no bc) cands))
	      (str (if (number? cand)
		       (ucs-to-utf8-string cand)
		       cand))
	      (menu-item (nth (- (byeoru-context-menu-no bc) (length cache))
			      byeoru-menu-symbols)))
	 (byeoru-commit bc str)
	 (set-cdr! menu-item (cons cand (delete cand cands eq?)))
	 (set! byeoru-menu-symbols
	       (cons menu-item (delete menu-item byeoru-menu-symbols eq?)))
	 (byeoru-context-set-mode! bc 'hangul)
	 (byeoru-context-set-menu-no! bc 0)
	 (update-cache str))))))

(define (byeoru-proc-other-states bc key key-state)
  (cond
   ((byeoru-prev-page-key? key key-state)
    (im-shift-page-candidate bc #f))
   ((byeoru-next-page-key? key key-state)
    (im-shift-page-candidate bc #t))
   ((byeoru-next-candidate-key? key key-state)
    (byeoru-move-candidate bc 1))
   ((byeoru-prev-candidate-key? key key-state)
    (byeoru-move-candidate bc -1))
   ((byeoru-cancel-key? key key-state)
    (byeoru-cancel-conv bc))
   ((byeoru-commit-key? key key-state)
    (if (eq? (byeoru-context-mode bc) 'conv)
	(byeoru-commit-converted-part bc)
	(byeoru-select-menu-or-symbol bc)))))

(define (byeoru-begin-input bc)
  (byeoru-context-set-on! bc #t))

(define (byeoru-proc-raw-state bc key key-state)
  (if (byeoru-on-key? key key-state)
      (byeoru-begin-input bc)
      (im-commit-raw bc)))

(define (byeoru-converting-state-preedit bc)
  (let ((convl (byeoru-context-convl-ustr bc))
	(convr (byeoru-context-convr-ustr bc))
	(word (byeoru-context-word-ustr bc))
	(underline
	 (if (byeoru-context-commit-by-word? bc) preedit-underline 0)))
    (list
     (and (not (ustr-cursor-at-beginning? convl))
	  (cons preedit-underline
		(apply string-append (ustr-former-seq convl))))
     (cons (bitwise-ior preedit-reverse underline preedit-cursor)
	   (apply string-append (ustr-former-seq convr)))
     (and (not (ustr-cursor-at-end? convr))
	  (cons preedit-underline
		(apply string-append (ustr-latter-seq convr))))
;;       (cons preedit-cursor "")
     (and (not (ustr-cursor-at-end? word))
	  (cons preedit-underline
		(apply string-append (ustr-latter-seq word)))))))

(define (byeoru-input-state-preedit bc)
  (let ((word (byeoru-context-word-ustr bc))
	(composing (byeoru-johab-to-utf8-string
		    (byeoru-automata-composing-char
		     (byeoru-context-automata bc))))
;; Underlining a composing character leads to a confusing appearance.
;; This should be made customizable.
;;	(underline
;;	 (if (byeoru-context-commit-by-word? bc) preedit-underline 0))
	)
    (list
     (and (not (ustr-cursor-at-beginning? word))
	  (cons preedit-underline
		(apply string-append (ustr-former-seq word))))
     (and (not (string=? composing ""))
	  (cons preedit-reverse composing))
     (and (byeoru-has-preedit? bc)
	  (cons preedit-cursor ""))
     (and (not (ustr-cursor-at-end? word))
	  (cons preedit-underline
		(apply string-append (ustr-latter-seq word)))))))

(define (byeoru-update-preedit bc)
  (let ((segments (if (byeoru-context-on bc)
		      (if (eq? (byeoru-context-mode bc) 'conv)
			  (byeoru-converting-state-preedit bc)
			  (byeoru-input-state-preedit bc))
		      '())))
    (if (not (equal? segments (byeoru-context-preedit bc)))
	(begin
	  (byeoru-context-set-preedit! bc segments)
	  (context-update-preedit bc segments)))))

(define (byeoru-key-press-handler bc key key-state)
  (if (byeoru-context-on bc)
      (if (eq? (byeoru-context-mode bc) 'hangul)
	  (byeoru-proc-input-state bc key key-state)
	  (byeoru-proc-other-states bc key key-state))
      (byeoru-proc-raw-state bc key key-state))
  (byeoru-update-preedit bc))

(define (byeoru-key-release-handler bc key key-state)
  (if (or (ichar-control? key)
	  (not (byeoru-context-on bc)))
      ;; don't discard key release event for apps
      (im-commit-raw bc)))

(define (byeoru-deactivate-candidate-selector bc)
  (if (not (eq? (byeoru-context-mode bc) 'hangul))
      (im-deactivate-candidate-selector bc)))

(define (byeoru-reset-handler bc)
  (if (byeoru-context-on bc)
      (begin
	(byeoru-deactivate-candidate-selector bc)
	(byeoru-flush-automata bc)
	;; reset-handler does not commit a string
	(byeoru-clear! bc)
	(byeoru-update-preedit bc))))

(define (byeoru-focus-out-handler bc)
  (if (byeoru-context-on bc)
      (begin
	(byeoru-deactivate-candidate-selector bc)
	(byeoru-flush bc)
	(byeoru-update-preedit bc))))

(define (byeoru-displace-handler bc)
  (if (byeoru-context-on bc)
      (begin
	(byeoru-deactivate-candidate-selector bc)
	(byeoru-flush bc)
	(byeoru-update-preedit bc))))

(define (byeoru-get-candidate-handler bc idx accel-enum-hint)
  (let* ((cands (byeoru-context-cands bc))
	 (cand (nth idx cands)))
    (cond
     ((symbol? cand)
      (list (if (byeoru-context-commit-by-word? bc) "글자단위" "단어단위")
	    (digit->string (+ idx 1)) ""))
     ((number? cand)
      (list (ucs-to-utf8-string cand)
	    (digit->string (+ idx 1)) ""))
     ((string? cand)
      ;; What's the use of the last ""?
      (list cand (digit->string (+ idx 1)) ""))
     ((list? cand)
      (list (car cand) (digit->string (+ idx 1)) ""))
     ((pair? cand)
      (list (string-append (car cand) "  " (cdr cand))
	    (digit->string (+ idx 1)) "")))))

(define (byeoru-set-candidate-index-handler bc idx)
  (case (byeoru-context-mode bc)
    ((conv symbol)
     (byeoru-context-set-cand-no! bc idx))
    ((menu)
     (byeoru-context-set-menu-no! bc idx))))

(register-im
 'byeoru
 "ko"
 "UTF-8"
 byeoru-im-name-label
 byeoru-im-short-desc
 #f					; init-arg
 byeoru-init-handler
 #f					; release-handler
 context-mode-handler
 byeoru-key-press-handler
 byeoru-key-release-handler
 byeoru-reset-handler
 byeoru-get-candidate-handler
 byeoru-set-candidate-index-handler
 context-prop-activate-handler
 #f
 #f
 byeoru-focus-out-handler
 #f
 byeoru-displace-handler
)

;; Local Variables:
;; mode: scheme
;; coding: utf-8
;; End:
