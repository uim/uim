;;;
;;; Copyright (c) 2003,2004 uim Project http://uim.freedesktop.org/
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

;; from GTK
(require "generic.scm")
;;
(define ipa-rule
  '(
((("&" ))("ɣ"))
((("'" ))("ˈ"))
((("/" "'" ))("ˊ"))
((("/" "/" ))("/"))
((("/" "3" ))("ɛ"))
((("/" "A" ))("ɒ"))
((("/" "R" ))("ʁ"))
((("/" "a" ))("ɐ"))
((("/" "c" ))("ɔ"))
((("/" "e" ))("ə"))
((("/" "h" ))("ɥ"))
((("/" "m" ))("ɯ"))
((("/" "r" ))("ɹ"))
((("/" "v" ))("ʌ"))
((("/" "w" ))("ʍ"))
((("/" "y" ))("ʎ"))
((("3" ))("ʒ"))
(((":" ))("ː"))
((("A" ))("ɑ"))
((("E" ))("ɛ"))
((("I" ))("ɪ"))
((("L" ))("ʟ"))
((("M" ))("ʍ"))
((("O" ))("O"))
((("O" "E" ))("ɶ"))
((("R" ))("ʀ"))
((("U" ))("ʊ"))
((("Y" ))("ʏ"))
((("`" ))("ˌ"))
((("a" ))("a"))
((("a" "e" ))("æ"))
((("c" ))("c"))
((("c" "," ))("ç"))
((("d" ))("d"))
((("d" "'" ))("d"))
((("d" "h" ))("ð"))
((("e" ))("e"))
((("e" "-" ))("ɚ"))
((("e" "|" ))("ɚ"))
((("g" ))("g"))
((("g" "n" ))("ɲ"))
((("i" ))("i"))
((("i" "-" ))("ɨ"))
((("n" ))("n"))
((("n" "g" ))("ŋ"))
((("o" ))("o"))
((("o" "-" ))("ɵ"))
((("o" "/" ))("ø"))
((("o" "e" ))("œ"))
((("o" "|" ))("ɑ"))
((("s" ))("s"))
((("s" "h" ))("ʃ"))
((("t" ))("t"))
((("t" "h" ))("θ"))
((("u" ))("u"))
((("u" "-" ))("ʉ"))
((("z" ))("z"))
((("z" "h" ))("ʒ"))
((("|" "o" ))("ɒ"))
((("~" ))("̃"))))


(define ipa-init-handler
  (lambda (id im arg)
    (generic-context-new id im ipa-rule #f)))

(generic-register-im
 'ipa "" "UTF-8" (N_ "International Phonetic Alphabet") ipa-init-handler)
