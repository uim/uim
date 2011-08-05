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

;; Japanese EUC

(require-extension (srfi 1 2))
(require-custom "japanese-custom.scm")

(define ja-rk-rule-basic
  '(
    ((("-"). ())("¡¼" "¡¼" "Ž°"))
    (((","). ())("¡¢" "¡¢" "Ž¤"))
    ((("."). ())("¡£" "¡£" "Ž¡"))
    ((("!"). ())("¡ª" "¡ª" "!"))
    ((("\""). ())("¡É" "¡É" "\""))
    ((("#"). ())("¡ô" "¡ô" "#"))
    ((("$"). ())("¡ð" "¡ð" "$"))
    ((("%"). ())("¡ó" "¡ó" "%"))
    ((("&"). ())("¡õ" "¡õ" "&"))
    ((("'"). ())("¡Ç" "¡Ç" "'"))
    ((("("). ())("¡Ê" "¡Ê" "("))
    (((")"). ())("¡Ë" "¡Ë" ")"))
    ((("~"). ())("¡Á" "¡Á" "~"))
    ((("="). ())("¡á" "¡á" "="))
    ((("^"). ())("¡°" "¡°" "^"))
    ((("\\"). ())("¡À" "¡À" "\\"))
    ((("|"). ())("¡Ã" "¡Ã" "|"))
    ((("`"). ())("¡Æ" "¡Æ" "`"))
    ((("@"). ())("¡÷" "¡÷" "@"))
    ((("{"). ())("¡Ð" "¡Ð" "{"))
    ((("["). ())("¡Ö" "¡Ö" "Ž¢"))
    ((("+"). ())("¡Ü" "¡Ü" "+"))
    (((";"). ())("¡¨" "¡¨" ";"))
    ((("*"). ())("¡ö" "¡ö" "*"))
    (((":"). ())("¡§" "¡§" ":"))
    ((("}"). ())("¡Ñ" "¡Ñ" "}"))
    ((("]"). ())("¡×" "¡×" "Ž£"))
    ((("<"). ())("¡ã" "¡ã" "<"))
    (((">"). ())("¡ä" "¡ä" ">"))
    ((("?"). ())("¡©" "¡©" "?"))
    ((("/"). ())("¡¿" "¡¿" "/"))
    ((("_"). ())("¡²" "¡²" "_"))
    ;; Since ordinary Japanese users press the "yen sign" key on
    ;; Japanese keyboard in romaji-halfwidth-kana-mode "to input
    ;; character code 134" rather than "to input yen sign symbol", I
    ;; changed the fullwidth yen sign with backslash.
    ;;   -- YamaKen 2007-09-17
    ;; ((("yen"). ())("¡ï" "¡ï" "¡ï")) ;; XXX
    ((("yen"). ())("¡ï" "¡ï" "\\"))

    ((("1"). ())("1" "1" "1"))
    ((("2"). ())("2" "2" "2"))
    ((("3"). ())("3" "3" "3"))
    ((("4"). ())("4" "4" "4"))
    ((("5"). ())("5" "5" "5"))
    ((("6"). ())("6" "6" "6"))
    ((("7"). ())("7" "7" "7"))
    ((("8"). ())("8" "8" "8"))
    ((("9"). ())("9" "9" "9"))
    ((("0"). ())("0" "0" "0"))

    ((("a"). ())("¤¢" "¥¢" "Ž±"))
    ((("i"). ())("¤¤" "¥¤" "Ž²"))
    ((("u"). ())("¤¦" "¥¦" "Ž³"))
    ((("e"). ())("¤¨" "¥¨" "Ž´"))
    ((("o"). ())("¤ª" "¥ª" "Žµ"))

    ((("x" "a"). ())("¤¡" "¥¡" "Ž§"))
    ((("x" "i"). ())("¤£" "¥£" "Ž¨"))
    ((("x" "y" "i"). ())("¤£" "¥£" "Ž¨"))
    ((("x" "u"). ())("¤¥" "¥¥" "Ž©"))
    ((("x" "e"). ())("¤§" "¥§" "Žª"))
    ((("x" "y" "e"). ())("¤§" "¥§" "Žª"))
    ((("x" "o"). ())("¤©" "¥©" "Ž«"))

    ((("l" "a"). ())("¤¡" "¥¡" "Ž§"))
    ((("l" "i"). ())("¤£" "¥£" "Ž¨"))
    ((("l" "u"). ())("¤¥" "¥¥" "Ž©"))
    ((("l" "e"). ())("¤§" "¥§" "Žª"))
    ((("l" "o"). ())("¤©" "¥©" "Ž«"))

    ((("k" "k"). ("k"))("¤Ã" "¥Ã" "Ž¯"))

    ((("k" "a"). ())("¤«" "¥«" "Ž¶"))
    ((("k" "i"). ())("¤­" "¥­" "Ž·"))
    ((("k" "u"). ())("¤¯" "¥¯" "Ž¸"))
    ((("k" "e"). ())("¤±" "¥±" "Ž¹"))
    ((("k" "o"). ())("¤³" "¥³" "Žº"))
    ((("k" "y" "a"). ())(("¤­" "¥­" "Ž·") ("¤ã" "¥ã" "Ž¬")))
    ((("k" "y" "i"). ())(("¤­" "¥­" "Ž·") ("¤£" "¥£" "Ž¨")))
    ((("k" "y" "u"). ())(("¤­" "¥­" "Ž·") ("¤å" "¥å" "Ž­")))
    ((("k" "y" "e"). ())(("¤­" "¥­" "Ž·") ("¤§" "¥§" "Žª")))
    ((("k" "y" "o"). ())(("¤­" "¥­" "Ž·") ("¤ç" "¥ç" "Ž®")))

    ((("g" "g"). ("g"))("¤Ã" "¥Ã" "Ž¯"))


    ((("g" "a"). ())("¤¬" "¥¬" "Ž¶ŽÞ"))
    ((("g" "i"). ())("¤®" "¥®" "Ž·ŽÞ"))
    ((("g" "u"). ())("¤°" "¥°" "Ž¸ŽÞ"))
    ((("g" "e"). ())("¤²" "¥²" "Ž¹ŽÞ"))
    ((("g" "o"). ())("¤´" "¥´" "ŽºŽÞ"))

    ((("g" "y" "a"). ())(("¤®" "¥®" "Ž·ŽÞ") ("¤ã" "¥ã" "Ž¬")))
    ((("g" "y" "i"). ())(("¤®" "¥®" "Ž·ŽÞ") ("¤£" "¥£" "Ž¨")))
    ((("g" "y" "u"). ())(("¤®" "¥®" "Ž·ŽÞ") ("¤å" "¥å" "Ž­")))
    ((("g" "y" "e"). ())(("¤®" "¥®" "Ž·ŽÞ") ("¤§" "¥§" "Žª")))
    ((("g" "y" "o"). ())(("¤®" "¥®" "Ž·ŽÞ") ("¤ç" "¥ç" "Ž®")))

    ((("q" "a"). ())(("¤¯" "¥¯" "Ž¸") ("¤¡" "¥¡" "Ž§")))
    ((("q" "i"). ())(("¤¯" "¥¯" "Ž¸") ("¤£" "¥£" "Ž¨")))
    ((("q" "u"). ())("¤¯" "¥¯" "Ž¸"))
    ((("q" "e"). ())(("¤¯" "¥¯" "Ž¸") ("¤§" "¥§" "Žª")))
    ((("q" "o"). ())(("¤¯" "¥¯" "Ž¸") ("¤©" "¥©" "Ž«")))

    ((("s" "s"). ("s"))("¤Ã" "¥Ã" "Ž¯"))

    ((("s" "a"). ())("¤µ" "¥µ" "Ž»"))
    ((("s" "i"). ())("¤·" "¥·" "Ž¼"))
    ((("s" "u"). ())("¤¹" "¥¹" "Ž½"))
    ((("s" "e"). ())("¤»" "¥»" "Ž¾"))
    ((("s" "o"). ())("¤½" "¥½" "Ž¿"))

    ((("s" "y" "a"). ())(("¤·" "¥·" "Ž¼") ("¤ã" "¥ã" "Ž¬")))
    ((("s" "y" "i"). ())(("¤·" "¥·" "Ž¼") ("¤£" "¥£" "Ž¨")))
    ((("s" "y" "u"). ())(("¤·" "¥·" "Ž¼") ("¤å" "¥å" "Ž­")))
    ((("s" "y" "e"). ())(("¤·" "¥·" "Ž¼") ("¤§" "¥§" "Žª")))
    ((("s" "y" "o"). ())(("¤·" "¥·" "Ž¼") ("¤ç" "¥ç" "Ž®")))

    ((("z" "z"). ("z"))("¤Ã" "¥Ã" "Ž¯"))

    ((("z" "a"). ())("¤¶" "¥¶" "Ž»ŽÞ"))
    ((("z" "i"). ())("¤¸" "¥¸" "Ž¼ŽÞ"))
    ((("z" "u"). ())("¤º" "¥º" "Ž½ŽÞ"))
    ((("z" "e"). ())("¤¼" "¥¼" "Ž¾ŽÞ"))
    ((("z" "o"). ())("¤¾" "¥¾" "Ž¿ŽÞ"))
    ((("z" "y" "a"). ())(("¤¸" "¥¸" "Ž¼ŽÞ") ("¤ã" "¥ã" "Ž¬")))
    ((("z" "y" "i"). ())(("¤¸" "¥¸" "Ž¼ŽÞ") ("¤£" "¥£" "Ž¨")))
    ((("z" "y" "u"). ())(("¤¸" "¥¸" "Ž¼ŽÞ") ("¤å" "¥å" "Ž­")))
    ((("z" "y" "e"). ())(("¤¸" "¥¸" "Ž¼ŽÞ") ("¤§" "¥§" "Žª")))
    ((("z" "y" "o"). ())(("¤¸" "¥¸" "Ž¼ŽÞ") ("¤ç" "¥ç" "Ž®")))

    ((("j" "j"). ("j"))("¤Ã" "¥Ã" "Ž¯"))

    ((("j" "a"). ())(("¤¸" "¥¸" "Ž¼ŽÞ") ("¤ã" "¥ã" "Ž¬")))
    ((("j" "i"). ())("¤¸" "¥¸" "Ž¼ŽÞ"))
    ((("j" "u"). ())(("¤¸" "¥¸" "Ž¼ŽÞ") ("¤å" "¥å" "Ž­")))
    ((("j" "e"). ())(("¤¸" "¥¸" "Ž¼ŽÞ") ("¤§" "¥§" "Žª")))
    ((("j" "o"). ())(("¤¸" "¥¸" "Ž¼ŽÞ") ("¤ç" "¥ç" "Ž®")))

    ((("j" "y" "a"). ())(("¤¸" "¥¸" "Ž¼ŽÞ") ("¤ã" "¥ã" "Ž¬")))
    ((("j" "y" "i"). ())(("¤¸" "¥¸" "Ž¼ŽÞ") ("¤£" "¥£" "Ž¨")))
    ((("j" "y" "u"). ())(("¤¸" "¥¸" "Ž¼ŽÞ") ("¤å" "¥å" "Ž­")))
    ((("j" "y" "e"). ())(("¤¸" "¥¸" "Ž¼ŽÞ") ("¤§" "¥§" "Žª")))
    ((("j" "y" "o"). ())(("¤¸" "¥¸" "Ž¼ŽÞ") ("¤ç" "¥ç" "Ž®")))

    ((("t" "t"). ("t"))("¤Ã" "¥Ã" "Ž¯"))
    ((("t" "c"). ("c"))("¤Ã" "¥Ã" "Ž¯"))

    ((("t" "a"). ())("¤¿" "¥¿" "ŽÀ"))
    ((("t" "i"). ())("¤Á" "¥Á" "ŽÁ"))
    ((("t" "u"). ())("¤Ä" "¥Ä" "ŽÂ"))
    ((("t" "e"). ())("¤Æ" "¥Æ" "ŽÃ"))
    ((("t" "o"). ())("¤È" "¥È" "ŽÄ"))

    ((("t" "y" "a"). ())(("¤Á" "¥Á" "ŽÁ") ("¤ã" "¥ã" "Ž¬")))
    ((("t" "y" "i"). ())(("¤Á" "¥Á" "ŽÁ") ("¤£" "¥£" "Ž¨")))
    ((("t" "y" "u"). ())(("¤Á" "¥Á" "ŽÁ") ("¤å" "¥å" "Ž­")))
    ((("t" "y" "e"). ())(("¤Á" "¥Á" "ŽÁ") ("¤§" "¥§" "Žª")))
    ((("t" "y" "o"). ())(("¤Á" "¥Á" "ŽÁ") ("¤ç" "¥ç" "Ž®")))

    ((("t" "s" "a"). ())(("¤Ä" "¥Ä" "ŽÂ") ("¤¡" "¥¡" "Ž§")))
    ((("t" "s" "i"). ())(("¤Ä" "¥Ä" "ŽÂ") ("¤£" "¥£" "Ž¨")))
    ((("t" "s" "u"). ())("¤Ä" "¥Ä" "ŽÂ"))
    ((("t" "s" "e"). ())(("¤Ä" "¥Ä" "ŽÂ") ("¤§" "¥§" "Žª")))
    ((("t" "s" "o"). ())(("¤Ä" "¥Ä" "ŽÂ") ("¤©" "¥©" "Ž«")))

    ((("c" "y" "a"). ())(("¤Á" "¥Á" "ŽÁ") ("¤ã" "¥ã" "Ž¬")))
    ((("c" "y" "i"). ())(("¤Á" "¥Á" "ŽÁ") ("¤£" "¥£" "Ž¨")))
    ((("c" "y" "u"). ())(("¤Á" "¥Á" "ŽÁ") ("¤å" "¥å" "Ž­")))
    ((("c" "y" "e"). ())(("¤Á" "¥Á" "ŽÁ") ("¤§" "¥§" "Žª")))
    ((("c" "y" "o"). ())(("¤Á" "¥Á" "ŽÁ") ("¤ç" "¥ç" "Ž®")))

    ((("x" "t" "u"). ())("¤Ã" "¥Ã" "Ž¯"))
    ((("x" "t" "s" "u"). ())("¤Ã" "¥Ã" "Ž¯"))
    ((("c" "c"). ("c"))("¤Ã" "¥Ã" "Ž¯"))

    ((("d" "d"). ("d"))("¤Ã" "¥Ã" "Ž¯"))

    ((("d" "a"). ())("¤À" "¥À" "ŽÀŽÞ"))
    ((("d" "i"). ())("¤Â" "¥Â" "ŽÁŽÞ"))
    ((("d" "u"). ())("¤Å" "¥Å" "ŽÂŽÞ"))
    ((("d" "e"). ())("¤Ç" "¥Ç" "ŽÃŽÞ"))
    ((("d" "o"). ())("¤É" "¥É" "ŽÄŽÞ"))

    ((("d" "y" "a"). ())(("¤Â" "¥Â" "ŽÁŽÞ") ("¤ã" "¥ã" "Ž¬")))
    ((("d" "y" "i"). ())(("¤Â" "¥Â" "ŽÁŽÞ") ("¤£" "¥£" "Ž¨")))
    ((("d" "y" "u"). ())(("¤Â" "¥Â" "ŽÁŽÞ") ("¤å" "¥å" "Ž­")))
    ((("d" "y" "e"). ())(("¤Â" "¥Â" "ŽÁŽÞ") ("¤§" "¥§" "Žª")))
    ((("d" "y" "o"). ())(("¤Â" "¥Â" "ŽÁŽÞ") ("¤ç" "¥ç" "Ž®")))

    ((("n" "n"). ())("¤ó" "¥ó" "ŽÝ"))
    ((("n" "'"). ())("¤ó" "¥ó" "ŽÝ"))
    ((("n"). ())("¤ó" "¥ó" "ŽÝ"))

    ((("n" "a"). ())("¤Ê" "¥Ê" "ŽÅ"))
    ((("n" "i"). ())("¤Ë" "¥Ë" "ŽÆ"))
    ((("n" "u"). ())("¤Ì" "¥Ì" "ŽÇ"))
    ((("n" "e"). ())("¤Í" "¥Í" "ŽÈ"))
    ((("n" "o"). ())("¤Î" "¥Î" "ŽÉ"))

    ((("n" "y" "a"). ())(("¤Ë" "¥Ë" "ŽÆ") ("¤ã" "¥ã" "Ž¬")))
    ((("n" "y" "i"). ())(("¤Ë" "¥Ë" "ŽÆ") ("¤£" "¥£" "Ž¨")))
    ((("n" "y" "u"). ())(("¤Ë" "¥Ë" "ŽÆ") ("¤å" "¥å" "Ž­")))
    ((("n" "y" "e"). ())(("¤Ë" "¥Ë" "ŽÆ") ("¤§" "¥§" "Žª")))
    ((("n" "y" "o"). ())(("¤Ë" "¥Ë" "ŽÆ") ("¤ç" "¥ç" "Ž®")))

    ((("h" "h"). ("h"))("¤Ã" "¥Ã" "Ž¯"))

    ((("h" "a"). ())("¤Ï" "¥Ï" "ŽÊ"))
    ((("h" "i"). ())("¤Ò" "¥Ò" "ŽË"))
    ((("h" "u"). ())("¤Õ" "¥Õ" "ŽÌ"))
    ((("h" "e"). ())("¤Ø" "¥Ø" "ŽÍ"))
    ((("h" "o"). ())("¤Û" "¥Û" "ŽÎ"))

    ((("h" "y" "a"). ())(("¤Ò" "¥Ò" "ŽË") ("¤ã" "¥ã" "Ž¬")))
    ((("h" "y" "i"). ())(("¤Ò" "¥Ò" "ŽË") ("¤£" "¥£" "Ž¨")))
    ((("h" "y" "u"). ())(("¤Ò" "¥Ò" "ŽË") ("¤å" "¥å" "Ž­")))
    ((("h" "y" "e"). ())(("¤Ò" "¥Ò" "ŽË") ("¤§" "¥§" "Žª")))
    ((("h" "y" "o"). ())(("¤Ò" "¥Ò" "ŽË") ("¤ç" "¥ç" "Ž®")))

    ((("f" "f"). ("f"))("¤Ã" "¥Ã" "Ž¯"))

    ((("f" "a"). ())(("¤Õ" "¥Õ" "ŽÌ") ("¤¡" "¥¡" "Ž§")))
    ((("f" "i"). ())(("¤Õ" "¥Õ" "ŽÌ") ("¤£" "¥£" "Ž¨")))
    ((("f" "u"). ())("¤Õ" "¥Õ" "ŽÌ"))
    ((("f" "e"). ())(("¤Õ" "¥Õ" "ŽÌ") ("¤§" "¥§" "Žª")))
    ((("f" "o"). ())(("¤Õ" "¥Õ" "ŽÌ") ("¤©" "¥©" "Ž«")))

    ((("f" "y" "a"). ())(("¤Õ" "¥Õ" "ŽÌ") ("¤ã" "¥ã" "Ž¬")))
    ((("f" "y" "i"). ())(("¤Õ" "¥Õ" "ŽÌ") ("¤£" "¥£" "Ž¨")))
    ((("f" "y" "u"). ())(("¤Õ" "¥Õ" "ŽÌ") ("¤å" "¥å" "Ž­")))
    ((("f" "y" "e"). ())(("¤Õ" "¥Õ" "ŽÌ") ("¤§" "¥§" "Žª")))
    ((("f" "y" "o"). ())(("¤Õ" "¥Õ" "ŽÌ") ("¤ç" "¥ç" "Ž®")))

    ((("b" "b"). ("b"))("¤Ã" "¥Ã" "Ž¯"))

    ((("b" "a"). ())("¤Ð" "¥Ð" "ŽÊŽÞ"))
    ((("b" "i"). ())("¤Ó" "¥Ó" "ŽËŽÞ"))
    ((("b" "u"). ())("¤Ö" "¥Ö" "ŽÌŽÞ"))
    ((("b" "e"). ())("¤Ù" "¥Ù" "ŽÍŽÞ"))
    ((("b" "o"). ())("¤Ü" "¥Ü" "ŽÎŽÞ"))

    ((("b" "y" "a"). ())(("¤Ó" "¥Ó" "ŽËŽÞ") ("¤ã" "¥ã" "Ž¬")))
    ((("b" "y" "i"). ())(("¤Ó" "¥Ó" "ŽËŽÞ") ("¤£" "¥£" "Ž¨")))
    ((("b" "y" "u"). ())(("¤Ó" "¥Ó" "ŽËŽÞ") ("¤å" "¥å" "Ž­")))
    ((("b" "y" "e"). ())(("¤Ó" "¥Ó" "ŽËŽÞ") ("¤§" "¥§" "Žª")))
    ((("b" "y" "o"). ())(("¤Ó" "¥Ó" "ŽËŽÞ") ("¤ç" "¥ç" "Ž®")))

    ((("p" "p"). ("p"))("¤Ã" "¥Ã" "Ž¯"))

    ((("p" "a"). ())("¤Ñ" "¥Ñ" "ŽÊŽß"))
    ((("p" "i"). ())("¤Ô" "¥Ô" "ŽËŽß"))
    ((("p" "u"). ())("¤×" "¥×" "ŽÌŽß"))
    ((("p" "e"). ())("¤Ú" "¥Ú" "ŽÍŽß"))
    ((("p" "o"). ())("¤Ý" "¥Ý" "ŽÎŽß"))

    ((("p" "y" "a"). ())(("¤Ô" "¥Ô" "ŽËŽß") ("¤ã" "¥ã" "Ž¬")))
    ((("p" "y" "i"). ())(("¤Ô" "¥Ô" "ŽËŽß") ("¤£" "¥£" "Ž¨")))
    ((("p" "y" "u"). ())(("¤Ô" "¥Ô" "ŽËŽß") ("¤å" "¥å" "Ž­")))
    ((("p" "y" "e"). ())(("¤Ô" "¥Ô" "ŽËŽß") ("¤§" "¥§" "Žª")))
    ((("p" "y" "o"). ())(("¤Ô" "¥Ô" "ŽËŽß") ("¤ç" "¥ç" "Ž®")))

    ((("m" "m"). ("m"))("¤Ã" "¥Ã" "Ž¯"))

    ((("m" "b"). ("b"))("¤ó" "¥ó" "ŽÝ"))
    ((("m" "p"). ("p"))("¤ó" "¥ó" "ŽÝ"))

    ((("m" "a"). ())("¤Þ" "¥Þ" "ŽÏ"))
    ((("m" "i"). ())("¤ß" "¥ß" "ŽÐ"))
    ((("m" "u"). ())("¤à" "¥à" "ŽÑ"))
    ((("m" "e"). ())("¤á" "¥á" "ŽÒ"))
    ((("m" "o"). ())("¤â" "¥â" "ŽÓ"))

    ((("m" "y" "a"). ())(("¤ß" "¥ß" "ŽÐ") ("¤ã" "¥ã" "Ž¬")))
    ((("m" "y" "i"). ())(("¤ß" "¥ß" "ŽÐ") ("¤£" "¥£" "Ž¨")))
    ((("m" "y" "u"). ())(("¤ß" "¥ß" "ŽÐ") ("¤å" "¥å" "Ž­")))
    ((("m" "y" "e"). ())(("¤ß" "¥ß" "ŽÐ") ("¤§" "¥§" "Žª")))
    ((("m" "y" "o"). ())(("¤ß" "¥ß" "ŽÐ") ("¤ç" "¥ç" "Ž®")))

    ((("y" "y"). ("y"))("¤Ã" "¥Ã" "Ž¯"))

    ((("y" "a"). ())("¤ä" "¥ä" "ŽÔ"))
    ((("y" "u"). ())("¤æ" "¥æ" "ŽÕ"))
    ((("y" "e"). ())(("¤¤" "¥¤" "Ž²") ("¤§" "¥§" "Žª")))
    ((("y" "o"). ())("¤è" "¥è" "ŽÖ"))

    ((("x" "c" "a"). ())("¥õ" "¥õ" "Ž¶"))
    ((("x" "k" "a"). ())("¥õ" "¥õ" "Ž¶"))
    ((("x" "k" "e"). ())("¥ö" "¥ö" "Ž¹"))

    ((("x" "y" "a"). ())("¤ã" "¥ã" "Ž¬"))
    ((("x" "y" "u"). ())("¤å" "¥å" "Ž­"))
    ((("x" "y" "o"). ())("¤ç" "¥ç" "Ž®"))

    ((("r" "r"). ("r"))("¤Ã" "¥Ã" "Ž¯"))

    ((("r" "a"). ())("¤é" "¥é" "Ž×"))
    ((("r" "i"). ())("¤ê" "¥ê" "ŽØ"))
    ((("r" "u"). ())("¤ë" "¥ë" "ŽÙ"))
    ((("r" "e"). ())("¤ì" "¥ì" "ŽÚ"))
    ((("r" "o"). ())("¤í" "¥í" "ŽÛ"))

    ((("l" "t" "u"). ())("¤Ã" "¥Ã" "Ž¯"))
    ((("l" "t" "s" "u"). ())("¤Ã" "¥Ã" "Ž¯"))

    ((("l" "y" "a"). ())("¤ã" "¥ã" "Ž¬"))
    ((("l" "y" "i"). ())("¤£" "¥£" "Ž¨"))
    ((("l" "y" "u"). ())("¤å" "¥å" "Ž­"))
    ((("l" "y" "e"). ())("¤§" "¥§" "Žª"))
    ((("l" "y" "o"). ())("¤ç" "¥ç" "Ž®"))

    ((("r" "y" "a"). ())(("¤ê" "¥ê" "ŽØ") ("¤ã" "¥ã" "Ž¬")))
    ((("r" "y" "i"). ())(("¤ê" "¥ê" "ŽØ") ("¤£" "¥£" "Ž¨")))
    ((("r" "y" "u"). ())(("¤ê" "¥ê" "ŽØ") ("¤å" "¥å" "Ž­")))
    ((("r" "y" "e"). ())(("¤ê" "¥ê" "ŽØ") ("¤§" "¥§" "Žª")))
    ((("r" "y" "o"). ())(("¤ê" "¥ê" "ŽØ") ("¤ç" "¥ç" "Ž®")))

    ((("w" "w"). ("w"))("¤Ã" "¥Ã" "Ž¯"))

    ((("w" "a"). ())("¤ï" "¥ï" "ŽÜ"))
    ((("w" "i"). ())(("¤¦" "¥¦" "Ž³") ("¤£" "¥£" "Ž¨")))
    ((("w" "u"). ())("¤¦" "¥¦" "Ž³"))
    ((("w" "e"). ())(("¤¦" "¥¦" "Ž³") ("¤§" "¥§" "Žª")))
    ((("w" "o"). ())("¤ò" "¥ò" "Ž¦"))
    ((("w" "h" "a"). ())(("¤¦" "¥¦" "Ž³") ("¤¡" "¥¡" "Ž§")))
    ((("w" "h" "i"). ())(("¤¦" "¥¦" "Ž³") ("¤£" "¥£" "Ž¨")))
    ((("w" "h" "u"). ())("¤¦" "¥¦" "Ž³"))
    ((("w" "h" "e"). ())(("¤¦" "¥¦" "Ž³") ("¤§" "¥§" "Žª")))
    ((("w" "h" "o"). ())(("¤¦" "¥¦" "Ž³") ("¤©" "¥©" "Ž«")))

    ((("v" "v"). ("v"))("¤Ã" "¥Ã" "Ž¯"))

    ((("v" "a"). ())(("¤¦¡«" "¥ô" "Ž³ŽÞ") ("¤¡" "¥¡" "Ž§")))
    ((("v" "i"). ())(("¤¦¡«" "¥ô" "Ž³ŽÞ") ("¤£" "¥£" "Ž¨")))
    ((("v" "u"). ())("¤¦¡«" "¥ô" "Ž³ŽÞ"))
    ((("v" "e"). ())(("¤¦¡«" "¥ô" "Ž³ŽÞ") ("¤§" "¥§" "Žª")))
    ((("v" "o"). ())(("¤¦¡«" "¥ô" "Ž³ŽÞ") ("¤©" "¥©" "Ž«")))

    ((("v" "y" "a"). ())(("¤¦¡«" "¥ô" "Ž³ŽÞ") ("¤ã" "¥ã" "Ž¬")))
    ((("v" "y" "u"). ())(("¤¦¡«" "¥ô" "Ž³ŽÞ") ("¤å" "¥å" "Ž­")))
    ((("v" "y" "o"). ())(("¤¦¡«" "¥ô" "Ž³ŽÞ") ("¤ç" "¥ç" "Ž®")))

    ((("z" "k"). ())("¢¬" "¢¬" ""))
    ((("z" "j"). ())("¢­" "¢­" ""))
    ((("z" "h"). ())("¢«" "¢«" ""))
    ((("z" "l"). ())("¢ª" "¢ª" ""))
    ((("z" "-"). ())("¡Á" "¡Á" ""))
    ((("z" "["). ())("¡Ø" "¡Ø" ""))
    ((("z" "]"). ())("¡Ù" "¡Ù" ""))
    ((("z" ","). ())("¡Å" "¡Å" ""))
    ((("z" "."). ())("¡Ä" "¡Ä" ""))
    ((("z" "/"). ())("¡¦" "¡¦" "Ž¥"))
    ))

(define ja-rk-rule-additional
  '(
    ((("d" "s" "u"). ())("¤Å" "¥Å" "ŽÂŽÞ"))

    ((("d" "h" "a"). ())(("¤Ç" "¥Ç" "ŽÃŽÞ") ("¤ã" "¥ã" "Ž¬")))
    ((("d" "h" "i"). ())(("¤Ç" "¥Ç" "ŽÃŽÞ") ("¤£" "¥£" "Ž¨")))
    ((("d" "h" "u"). ())(("¤Ç" "¥Ç" "ŽÃŽÞ") ("¤å" "¥å" "Ž­")))
    ((("d" "h" "e"). ())(("¤Ç" "¥Ç" "ŽÃŽÞ") ("¤§" "¥§" "Žª")))
    ((("d" "h" "o"). ())(("¤Ç" "¥Ç" "ŽÃŽÞ") ("¤ç" "¥ç" "Ž®")))

    ((("d" "w" "a"). ())(("¤É" "¥É" "ŽÄŽÞ") ("¤¡" "¥¡" "Ž§")))
    ((("d" "w" "i"). ())(("¤É" "¥É" "ŽÄŽÞ") ("¤£" "¥£" "Ž¨")))
    ((("d" "w" "u"). ())(("¤É" "¥É" "ŽÄŽÞ") ("¤¥" "¥¥" "Ž©")))
    ((("d" "w" "e"). ())(("¤É" "¥É" "ŽÄŽÞ") ("¤§" "¥§" "Žª")))
    ((("d" "w" "o"). ())(("¤É" "¥É" "ŽÄŽÞ") ("¤©" "¥©" "Ž«")))

    ((("k" "w" "a"). ())(("¤¯" "¥¯" "Ž¸") ("¤¡" "¥¡" "Ž§")))
    ((("k" "w" "i"). ())(("¤¯" "¥¯" "Ž¸") ("¤£" "¥£" "Ž¨")))
    ((("k" "w" "u"). ())(("¤¯" "¥¯" "Ž¸") ("¤¥" "¥¥" "Ž©")))
    ((("k" "w" "e"). ())(("¤¯" "¥¯" "Ž¸") ("¤§" "¥§" "Žª")))
    ((("k" "w" "o"). ())(("¤¯" "¥¯" "Ž¸") ("¤©" "¥©" "Ž«")))

    ((("s" "h" "a"). ())(("¤·" "¥·" "Ž¼") ("¤ã" "¥ã" "Ž¬")))
    ((("s" "h" "i"). ())("¤·" "¥·" "Ž¼"))
    ((("s" "h" "u"). ())(("¤·" "¥·" "Ž¼") ("¤å" "¥å" "Ž­")))
    ((("s" "h" "e"). ())(("¤·" "¥·" "Ž¼") ("¤§" "¥§" "Žª")))
    ((("s" "h" "o"). ())(("¤·" "¥·" "Ž¼") ("¤ç" "¥ç" "Ž®")))

    ((("s" "w" "a"). ())(("¤¹" "¥¹" "Ž½") ("¤¡" "¥¡" "Ž§")))
    ((("s" "w" "i"). ())(("¤¹" "¥¹" "Ž½") ("¤£" "¥£" "Ž¨")))
    ((("s" "w" "u"). ())(("¤¹" "¥¹" "Ž½") ("¤¥" "¥¥" "Ž©")))
    ((("s" "w" "e"). ())(("¤¹" "¥¹" "Ž½") ("¤§" "¥§" "Žª")))
    ((("s" "w" "o"). ())(("¤¹" "¥¹" "Ž½") ("¤©" "¥©" "Ž«")))

    ((("t" "w" "a"). ())(("¤È" "¥È" "ŽÄ") ("¤¡" "¥¡" "Ž§")))
    ((("t" "w" "i"). ())(("¤È" "¥È" "ŽÄ") ("¤£" "¥£" "Ž¨")))
    ((("t" "w" "u"). ())(("¤È" "¥È" "ŽÄ") ("¤¥" "¥¥" "Ž©")))
    ((("t" "w" "e"). ())(("¤È" "¥È" "ŽÄ") ("¤§" "¥§" "Žª")))
    ((("t" "w" "o"). ())(("¤È" "¥È" "ŽÄ") ("¤©" "¥©" "Ž«")))

    ((("t" "h" "a"). ())(("¤Æ" "¥Æ" "ŽÃ") ("¤ã" "¥ã" "Ž¬")))
    ((("t" "h" "i"). ())(("¤Æ" "¥Æ" "ŽÃ") ("¤£" "¥£" "Ž¨")))
    ((("t" "h" "u"). ())(("¤Æ" "¥Æ" "ŽÃ") ("¤å" "¥å" "Ž­")))
    ((("t" "h" "e"). ())(("¤Æ" "¥Æ" "ŽÃ") ("¤§" "¥§" "Žª")))
    ((("t" "h" "o"). ())(("¤Æ" "¥Æ" "ŽÃ") ("¤ç" "¥ç" "Ž®")))

    ((("h" "w" "a"). ())(("¤Õ" "¥Õ" "ŽÌ") ("¤¡" "¥¡" "Ž§")))
    ((("h" "w" "i"). ())(("¤Õ" "¥Õ" "ŽÌ") ("¤£" "¥£" "Ž¨")))
    ((("h" "w" "e"). ())(("¤Õ" "¥Õ" "ŽÌ") ("¤§" "¥§" "Žª")))
    ((("h" "w" "o"). ())(("¤Õ" "¥Õ" "ŽÌ") ("¤©" "¥©" "Ž«")))

    ((("f" "w" "a"). ())(("¤Õ" "¥Õ" "ŽÌ") ("¤¡" "¥¡" "Ž§")))
    ((("f" "w" "i"). ())(("¤Õ" "¥Õ" "ŽÌ") ("¤£" "¥£" "Ž¨")))
    ((("f" "w" "u"). ())(("¤Õ" "¥Õ" "ŽÌ") ("¤¥" "¥¥" "Ž©")))
    ((("f" "w" "e"). ())(("¤Õ" "¥Õ" "ŽÌ") ("¤§" "¥§" "Žª")))
    ((("f" "w" "o"). ())(("¤Õ" "¥Õ" "ŽÌ") ("¤©" "¥©" "Ž«")))

    ((("x" "w" "a"). ())("¤î" "¥î" "ŽÜ"))
    ((("x" "w" "i"). ())("¤ð" "¥ð" "Ž¨"))
    ((("x" "w" "e"). ())("¤ñ" "¥ñ" "Žª"))

    ((("w" "y" "i"). ())("¤ð" "¥ð" "Ž¨"))
    ((("w" "y" "e"). ())("¤ñ" "¥ñ" "Žª"))

    ((("c" "h" "a"). ())(("¤Á" "¥Á" "ŽÁ") ("¤ã" "¥ã" "Ž¬")))
    ((("c" "h" "i"). ())("¤Á" "¥Á" "ŽÁ"))
    ((("c" "h" "u"). ())(("¤Á" "¥Á" "ŽÁ") ("¤å" "¥å" "Ž­")))
    ((("c" "h" "e"). ())(("¤Á" "¥Á" "ŽÁ") ("¤§" "¥§" "Žª")))
    ((("c" "h" "o"). ())(("¤Á" "¥Á" "ŽÁ") ("¤ç" "¥ç" "Ž®")))

    ((("q" "w" "a"). ())(("¤¯" "¥¯" "Ž¸") ("¤¡" "¥¡" "Ž§")))
    ((("q" "w" "i"). ())(("¤¯" "¥¯" "Ž¸") ("¤£" "¥£" "Ž¨")))
    ((("q" "w" "u"). ())(("¤¯" "¥¯" "Ž¸") ("¤¥" "¥¥" "Ž©")))
    ((("q" "w" "e"). ())(("¤¯" "¥¯" "Ž¸") ("¤§" "¥§" "Žª")))
    ((("q" "w" "o"). ())(("¤¯" "¥¯" "Ž¸") ("¤©" "¥©" "Ž«")))

    ((("q" "y" "a"). ())(("¤¯" "¥¯" "Ž¸") ("¤ã" "¥ã" "Ž¬")))
    ((("q" "y" "i"). ())(("¤¯" "¥¯" "Ž¸") ("¤£" "¥£" "Ž¨")))
    ((("q" "y" "u"). ())(("¤¯" "¥¯" "Ž¸") ("¤å" "¥å" "Ž­")))
    ((("q" "y" "e"). ())(("¤¯" "¥¯" "Ž¸") ("¤§" "¥§" "Žª")))
    ((("q" "y" "o"). ())(("¤¯" "¥¯" "Ž¸") ("¤ç" "¥ç" "Ž®")))

    ((("g" "w" "a"). ())(("¤°" "¥°" "Ž¸ŽÞ") ("¤¡" "¥¡" "Ž§")))
    ((("g" "w" "i"). ())(("¤°" "¥°" "Ž¸ŽÞ") ("¤£" "¥£" "Ž¨")))
    ((("g" "w" "u"). ())(("¤°" "¥°" "Ž¸ŽÞ") ("¤¥" "¥¥" "Ž©")))
    ((("g" "w" "e"). ())(("¤°" "¥°" "Ž¸ŽÞ") ("¤§" "¥§" "Žª")))
    ((("g" "w" "o"). ())(("¤°" "¥°" "Ž¸ŽÞ") ("¤©" "¥©" "Ž«")))

    ((("z" "w" "a"). ())(("¤º" "¥º" "Ž½ŽÞ") ("¤¡" "¥¡" "Ž§")))
    ((("z" "w" "i"). ())(("¤º" "¥º" "Ž½ŽÞ") ("¤£" "¥£" "Ž¨")))
    ((("z" "w" "u"). ())(("¤º" "¥º" "Ž½ŽÞ") ("¤¥" "¥¥" "Ž©")))
    ((("z" "w" "e"). ())(("¤º" "¥º" "Ž½ŽÞ") ("¤§" "¥§" "Žª")))
    ((("z" "w" "o"). ())(("¤º" "¥º" "Ž½ŽÞ") ("¤©" "¥©" "Ž«")))

    ;((("n" "w" "a"). ())(("¤Ì" "¥Ì" "ŽÇ") ("¤¡" "¥¡" "Ž§")))
    ;((("n" "w" "i"). ())(("¤Ì" "¥Ì" "ŽÇ") ("¤£" "¥£" "Ž¨")))
    ;((("n" "w" "u"). ())(("¤Ì" "¥Ì" "ŽÇ") ("¤¥" "¥¥" "Ž©")))
    ;((("n" "w" "e"). ())(("¤Ì" "¥Ì" "ŽÇ") ("¤§" "¥§" "Žª")))
    ;((("n" "w" "o"). ())(("¤Ì" "¥Ì" "ŽÇ") ("¤©" "¥©" "Ž«")))

    ((("b" "w" "a"). ())(("¤Ö" "¥Ö" "ŽÌŽÞ") ("¤¡" "¥¡" "Ž§")))
    ((("b" "w" "i"). ())(("¤Ö" "¥Ö" "ŽÌŽÞ") ("¤£" "¥£" "Ž¨")))
    ((("b" "w" "u"). ())(("¤Ö" "¥Ö" "ŽÌŽÞ") ("¤¥" "¥¥" "Ž©")))
    ((("b" "w" "e"). ())(("¤Ö" "¥Ö" "ŽÌŽÞ") ("¤§" "¥§" "Žª")))
    ((("b" "w" "o"). ())(("¤Ö" "¥Ö" "ŽÌŽÞ") ("¤©" "¥©" "Ž«")))

    ((("p" "w" "a"). ())(("¤×" "¥×" "ŽÌŽß") ("¤¡" "¥¡" "Ž§")))
    ((("p" "w" "i"). ())(("¤×" "¥×" "ŽÌŽß") ("¤£" "¥£" "Ž¨")))
    ((("p" "w" "u"). ())(("¤×" "¥×" "ŽÌŽß") ("¤¥" "¥¥" "Ž©")))
    ((("p" "w" "e"). ())(("¤×" "¥×" "ŽÌŽß") ("¤§" "¥§" "Žª")))
    ((("p" "w" "o"). ())(("¤×" "¥×" "ŽÌŽß") ("¤©" "¥©" "Ž«")))

    ((("m" "w" "a"). ())(("¤à" "¥à" "ŽÑ") ("¤¡" "¥¡" "Ž§")))
    ((("m" "w" "i"). ())(("¤à" "¥à" "ŽÑ") ("¤£" "¥£" "Ž¨")))
    ((("m" "w" "u"). ())(("¤à" "¥à" "ŽÑ") ("¤¥" "¥¥" "Ž©")))
    ((("m" "w" "e"). ())(("¤à" "¥à" "ŽÑ") ("¤§" "¥§" "Žª")))
    ((("m" "w" "o"). ())(("¤à" "¥à" "ŽÑ") ("¤©" "¥©" "Ž«")))

    ((("y" "w" "a"). ())(("¤æ" "¥æ" "ŽÕ") ("¤¡" "¥¡" "Ž§")))
    ((("y" "w" "i"). ())(("¤æ" "¥æ" "ŽÕ") ("¤£" "¥£" "Ž¨")))
    ((("y" "w" "u"). ())(("¤æ" "¥æ" "ŽÕ") ("¤¥" "¥¥" "Ž©")))
    ((("y" "w" "e"). ())(("¤æ" "¥æ" "ŽÕ") ("¤§" "¥§" "Žª")))
    ((("y" "w" "o"). ())(("¤æ" "¥æ" "ŽÕ") ("¤©" "¥©" "Ž«")))

    ((("r" "w" "a"). ())(("¤ë" "¥ë" "ŽÙ") ("¤¡" "¥¡" "Ž§")))
    ((("r" "w" "i"). ())(("¤ë" "¥ë" "ŽÙ") ("¤£" "¥£" "Ž¨")))
    ((("r" "w" "u"). ())(("¤ë" "¥ë" "ŽÙ") ("¤¥" "¥¥" "Ž©")))
    ((("r" "w" "e"). ())(("¤ë" "¥ë" "ŽÙ") ("¤§" "¥§" "Žª")))
    ((("r" "w" "o"). ())(("¤ë" "¥ë" "ŽÙ") ("¤©" "¥©" "Ž«")))

    ((("d" "'" "i"). ())(("¤Ç" "¥Ç" "ŽÃŽÞ") ("¤£" "¥£" "Ž¨")))
    ((("d" "'" "y" "u"). ())(("¤Ç" "¥Ç" "ŽÃŽÞ") ("¤å" "¥å" "Ž­")))

    ((("d" "'" "u"). ())(("¤É" "¥É" "ŽÄŽÞ") ("¤¥" "¥¥" "Ž©")))

    ((("t" "'" "i"). ())(("¤Æ" "¥Æ" "ŽÃ") ("¤£" "¥£" "Ž¨")))
    ((("t" "'" "y" "u"). ())(("¤Æ" "¥Æ" "ŽÃ") ("¤å" "¥å" "Ž­")))

    ((("t" "'" "u"). ())(("¤È" "¥È" "ŽÄ") ("¤¥" "¥¥" "Ž©")))
 
    ))

(define ja-rk-rule (append ja-rk-rule-basic ja-rk-rule-additional))

(define ja-wide-rule
  '(("a" "£á")
    ("b" "£â")
    ("c" "£ã")
    ("d" "£ä")
    ("e" "£å")
    ("f" "£æ")
    ("g" "£ç")
    ("h" "£è")
    ("i" "£é")
    ("j" "£ê")
    ("k" "£ë")
    ("l" "£ì")
    ("m" "£í")
    ("n" "£î")
    ("o" "£ï")
    ("p" "£ð")
    ("q" "£ñ")
    ("r" "£ò")
    ("s" "£ó")
    ("t" "£ô")
    ("u" "£õ")
    ("v" "£ö")
    ("w" "£÷")
    ("x" "£ø")
    ("y" "£ù")
    ("z" "£ú")
    ("A" "£Á")
    ("B" "£Â")
    ("C" "£Ã")
    ("D" "£Ä")
    ("E" "£Å")
    ("F" "£Æ")
    ("G" "£Ç")
    ("H" "£È")
    ("I" "£É")
    ("J" "£Ê")
    ("K" "£Ë")
    ("L" "£Ì")
    ("M" "£Í")
    ("N" "£Î")
    ("O" "£Ï")
    ("P" "£Ð")
    ("Q" "£Ñ")
    ("R" "£Ò")
    ("S" "£Ó")
    ("T" "£Ô")
    ("U" "£Õ")
    ("V" "£Ö")
    ("W" "£×")
    ("X" "£Ø")
    ("Y" "£Ù")
    ("Z" "£Ú")

    ("1" "£±")
    ("2" "£²")
    ("3" "£³")
    ("4" "£´")
    ("5" "£µ")
    ("6" "£¶")
    ("7" "£·")
    ("8" "£¸")
    ("9" "£¹")
    ("0" "£°")

    ("-" "¡Ý")
    ("," "¡¤")
    ("." "¡¥")
    ("!" "¡ª")
    ("\"" "¡É")
    ("#" "¡ô")
    ("$" "¡ð")
    ("%" "¡ó")
    ("&" "¡õ")
    ("'" "¡Ç")
    ("(" "¡Ê")
    (")" "¡Ë")
    ("~" "¡Á")
    ("=" "¡á")
    ("^" "¡°")
    ("\\" "¡À")
    ("yen" "¡ï")
    ("|" "¡Ã")
    ("`" "¡Æ")
    ("@" "¡÷")
    ("{" "¡Ð")
    ("[" "¡Î")
    ("+" "¡Ü")
    (";" "¡¨")
    ("*" "¡ö")
    (":" "¡§")
    ("}" "¡Ñ")
    ("]" "¡Ï")
    ("<" "¡ã")
    (">" "¡ä")
    ("?" "¡©")
    ("/" "¡¿")
    ("_"  "¡²")
    (" " "¡¡")
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
  '("¡¡" "¡¡" " "))

;; space on (halfwidth-alnum fullwidth-alnum) input mode
(define ja-alnum-space
  '(" " "¡¡"))

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
;;     Fix a broken "¤Ã":
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

;; revise string list contains "¤¦¡«"
;; (("¡«") ("¤¦")) -> ("¤¦¡«")
(define ja-join-vu
  (lambda (lst)
    (let ((sub (member "¡«" lst)))
      (if (and
	   sub
	   (not (null? (cdr sub)))
	   (string=? (car (cdr sub)) "¤¦"))
	  (append
	   (list-head lst (- (length lst) (length sub)))
	   '("¤¦¡«")
	   (ja-join-vu (list-tail lst (+ (- (length lst) (length sub)) 2))))
	  (if (and
	       sub
	       (member "¡«" (cdr sub)))
	      (append
	       (list-head lst (+ (- (length lst) (length sub)) 1))
	       (ja-join-vu (cdr sub)))
	      lst)))))

;; get ("¤¢" "¥¢" "Ž±") from "¤¢"
(define ja-find-kana-list-from-rule
  (lambda (rule str)
    (if (not (null? rule))
	(if (pair? (member str (car (cdr (car rule)))))
	    (car (cdr (car rule)))
	    (ja-find-kana-list-from-rule (cdr rule) str))
        (if (string=?  str "¡«")
	    (list "¡«" "¡«" "ŽÞ")
	    (list str str str)))))

;; (("¤¸" "¥¸" "Ž¼ŽÞ") ("¤ó" "¥ó" "ŽÝ") ("¤«" "¥«" "Ž¶")) from ("¤¸" "¤ó" "¤«")
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
;;  (("¤¸" "¥¸" "Ž¼ŽÞ") ("¤ó" "¥ó" "ŽÝ") ("¤«" "¥«" "Ž¶"))
;;  ja-type-katakana)
;;  -> "¥«¥ó¥¸"
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
    ;;(set-symbol-value! 'yen "¡ï")  ;; XXX
    (set-symbol-value! 'yen "\\")
    ))

;; TODO: Support new custom type string-list.
(define japanese-auto-start-henkan-keyword-list '("¡¢" "¡£" "¡¥" "¡¤" "¡©" "¡×" "¡ª" "¡¨" "¡§" ")" ";" ":" "¡Ë" "¡É" "¡Û" "¡Ù" "¡Õ" "¡Ó" "¡Ñ" "¡Ï" "¡Í" "}" "]" "?" "." "," "!"))

(define ja-rk-rule-consonant-to-keep
  (map (lambda (c)
         (if (= (string-length c) 1)
           (list (cons (list c) '()) (list c c c))
           (let ((lst (reverse (string-to-list c))))
             (list (cons lst '()) (list (list (car lst) (car lst) (car lst))
                                        (list (cadr lst) (cadr lst) (cadr lst)))))))
       (filter (lambda (x) (not (string=? "n" x)))
               (map car ja-consonant-syllable-table))))

(define ja-rk-rule-update
  (lambda ()
    (if ja-rk-rule-keep-consonant?
      (set! ja-rk-rule (append ja-rk-rule-consonant-to-keep
                               ja-rk-rule-basic
                               ja-rk-rule-additional))
      (set! ja-rk-rule (append ja-rk-rule-basic
                               ja-rk-rule-additional)))))


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
      (ja-euc-jp-code->euc-jp-string
        (+ (* ej0 #x10000) (* ej1 #x100) ej2)))))

;;; Convert reverse string list of JIS code to one EUC-JP kanji string
;;; ("d" "2" "0" "5") -> "Ð­"
(define (ja-kanji-code-input-jis str-list)
  (and-let*
    ((length=4? (= (length str-list) 4))
     (str1 (string-list-concat (take-right str-list 2)))
     (str2 (string-list-concat (take str-list 2)))
     (jis1 (string->number str1 16))
     (jis2 (string->number str2 16)))
    (ja-jis-code->euc-jp-string 'jisx0213-plane1 jis1 jis2)))

;;; Convert reverse string list of Kuten code to one EUC-JP kanji string
;;; ("3" "1" "-" "8" "4" "-" "1") -> "Ð­"
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
;;; ("5" "8" "E" "4" "+" "U") -> "Ð­"
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
        (<= #xe000 ucs #x10ffff)))
     (utf8-str (ucs->utf8-string ucs))
     (ic (iconv-open "EUC-JP" "UTF-8")))
    (let ((eucj-str (iconv-code-conv ic utf8-str)))
      (iconv-release ic)
      eucj-str)))

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
