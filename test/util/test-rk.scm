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
;;;

;; These tests are passed at revision 6605 (new repository)

(define-module test.util.test-rk
  (use test.unit.test-case)
  (use test.uim-test))
(select-module test.util.test-rk)

(define (setup)
  (uim-test-setup)
  (uim-eval
   '(define test-rk-rule '(((("a"). ()) ("あ" "ア" "ｱ"))
                           ((("i"). ()) ("い" "イ" "ｲ"))
                           ((("u"). ()) ("う" "ウ" "ｳ"))
                           ((("e"). ()) ("え" "エ" "ｴ"))
                           ((("o"). ()) ("お" "オ" "ｵ"))

                           ((("k" "a"). ()) ("か" "カ" "ｶ"))
                           ((("k" "i"). ()) ("き" "キ" "ｷ"))
                           ((("k" "u"). ()) ("く" "ク" "ｸ"))
                           ((("k" "e"). ()) ("け" "ケ" "ｹ"))
                           ((("k" "o"). ()) ("こ" "コ" "ｺ"))
                           ((("k" "y" "a"). ()) ("きゃ" "キャ" "ｷｬ"))
                           ((("k" "y" "i"). ()) ("きぃ" "キィ" "ｷｨ"))
                           ((("k" "y" "u"). ()) ("きゅ" "キュ" "ｷｭ"))
                           ((("k" "y" "e"). ()) ("きぇ" "キェ" "ｷｪ"))
                           ((("k" "y" "o"). ()) ("きょ" "キョ" "ｷｮ"))

                           ((("s" "s"). ("s")) ("っ" "ッ" "ｯ"))
                           ((("s" "a"). ()) ("さ" "サ" "ｻ"))
                           ((("s" "i"). ()) ("し" "シ" "ｼ"))
                           ((("s" "u"). ()) ("す" "ス" "ｽ"))
                           ((("s" "e"). ()) ("せ" "セ" "ｾ"))
                           ((("s" "o"). ()) ("そ" "ソ" "ｿ"))

                           ((("p" "p"). ("p")) ("っ" "ッ" "ｯ"))))))

(define (teardown)
  (uim-test-teardown))

(define (test-rk-lib-find-seq)
  ;; non existence seq does not match
  (assert-uim-false '(rk-lib-find-seq () test-rk-rule))
  (assert-uim-false '(rk-lib-find-seq '("") test-rk-rule))

  ;; test first rule
  (assert-uim-equal '((("a"). ())("あ" "ア" "ｱ"))
                    '(rk-lib-find-seq '("a") test-rk-rule))

  (assert-uim-equal '((("i"). ())("い" "イ" "ｲ"))
                    '(rk-lib-find-seq '("i") test-rk-rule))
  (assert-uim-equal '((("o"). ())("お" "オ" "ｵ"))
                    '(rk-lib-find-seq '("o") test-rk-rule))
  ;; non existence seq does not match
  (assert-uim-false '(rk-lib-find-seq '("z") test-rk-rule))

  (assert-uim-equal '((("k" "y" "a"). ())("きゃ" "キャ" "ｷｬ"))
                    '(rk-lib-find-seq '("k" "y" "a") test-rk-rule))
  (assert-uim-equal '((("k" "y" "i"). ())("きぃ" "キィ" "ｷｨ"))
                    '(rk-lib-find-seq '("k" "y" "i") test-rk-rule))
  (assert-uim-equal '((("k" "y" "o"). ())("きょ" "キョ" "ｷｮ"))
                    '(rk-lib-find-seq '("k" "y" "o") test-rk-rule))
  ;; partial seq does not match
  (assert-uim-false '(rk-lib-find-seq '("k" "y") test-rk-rule))
  (assert-uim-false '(rk-lib-find-seq '("k" "y" "y") test-rk-rule))

  (assert-uim-equal '((("s" "s"). ("s"))("っ" "ッ" "ｯ"))
                    '(rk-lib-find-seq '("s" "s") test-rk-rule))
  ;; partial seq does not match
  (assert-uim-false '(rk-lib-find-seq '("s") test-rk-rule))
  (assert-uim-false '(rk-lib-find-seq '("s" "s" "s") test-rk-rule))

  ;; test last rule
  (assert-uim-equal '((("p" "p"). ("p"))("っ" "ッ" "ｯ"))
                    '(rk-lib-find-seq '("p" "p") test-rk-rule))
  (assert-uim-false '(rk-lib-find-seq '("p") test-rk-rule))
  (assert-uim-false '(rk-lib-find-seq '("p" "p" "p") test-rk-rule))
  #f)

(define (test-rk-lib-find-partial-seq)
  ;; null sequence matches first rule
  (assert-uim-equal '((("a"). ())("あ" "ア" "ｱ"))
                    '(rk-lib-find-partial-seq () test-rk-rule))
  ;; non existence seq does not match
  (assert-uim-false '(rk-lib-find-partial-seq '("") test-rk-rule))

  ;; test first rule: exact key does not match
  (assert-uim-false '(rk-lib-find-partial-seq '("a") test-rk-rule))
  (assert-uim-false '(rk-lib-find-partial-seq '("i") test-rk-rule))
  (assert-uim-false '(rk-lib-find-partial-seq '("o") test-rk-rule))
  ;; non existence seq does not match
  (assert-uim-false '(rk-lib-find-partial-seq '("z") test-rk-rule))

  ;; exact key does not match
  (assert-uim-false '(rk-lib-find-partial-seq '("k" "y" "a")
                                                    test-rk-rule))
  (assert-uim-false '(rk-lib-find-partial-seq '("k" "y" "i")
                                                    test-rk-rule))
  (assert-uim-false '(rk-lib-find-partial-seq '("k" "y" "o")
                                                    test-rk-rule))
  ;; partial seq matches first entry
  (assert-uim-equal '((("k" "a"). ())("か" "カ" "ｶ"))
                    '(rk-lib-find-partial-seq '("k") test-rk-rule))
  (assert-uim-equal '((("k" "y" "a"). ())("きゃ" "キャ" "ｷｬ"))
                    '(rk-lib-find-partial-seq '("k" "y") test-rk-rule))
  ;; non existence seq does not match
  (assert-uim-false '(rk-lib-find-partial-seq '("k" "y" "y") test-rk-rule))

  ;; exact key does not match
  (assert-uim-false '(rk-lib-find-partial-seq '("s" "s") test-rk-rule))
  ;; partial match
  (assert-uim-equal '((("s" "s"). ("s"))("っ" "ッ" "ｯ"))
                    '(rk-lib-find-partial-seq '("s") test-rk-rule))
  ;; non existence seq does not match
  (assert-uim-false '(rk-lib-find-partial-seq '("s" "s" "s")
                                                    test-rk-rule))

  ;; test last rule
  (assert-uim-false '(rk-lib-find-partial-seq '("p" "p") test-rk-rule))
  (assert-uim-equal '((("p" "p"). ("p"))("っ" "ッ" "ｯ"))
                    '(rk-lib-find-partial-seq '("p") test-rk-rule))
  (assert-uim-false '(rk-lib-find-partial-seq '("p" "p" "p")
                                                    test-rk-rule))
  #f)

(define (test-rk-lib-expect-seq)
  (assert-uim-equal '("p" "s" "s" "s" "s" "s" "s"
                  "k" "k" "k" "k" "k" "k" "k" "k" "k" "k"
                  "o" "e" "u" "i" "a")
                    '(rk-lib-expect-seq () test-rk-rule))
  (assert-uim-equal '("y" "y" "y" "y" "y" "o" "e" "u" "i" "a")
                    '(rk-lib-expect-seq '("k") test-rk-rule))
  (assert-uim-equal '("o" "e" "u" "i" "a")
                    '(rk-lib-expect-seq '("k" "y") test-rk-rule))
  (assert-uim-equal '("o" "e" "u" "i" "a")
                    '(rk-lib-expect-seq '("k" "y") test-rk-rule))
  ;; rk-lib-expect-seq returns null list on exact match
  (assert-uim-equal ()
                    '(rk-lib-expect-seq '("k" "y" "a") test-rk-rule))
  ;; rk-lib-expect-seq returns null list on fail
  (assert-uim-equal ()
                    '(rk-lib-expect-seq '("k" "y" "a" "a") test-rk-rule))
  (assert-uim-equal '("o" "e" "u" "i" "a")
                    '(rk-lib-expect-seq '("k" "y") test-rk-rule))

  (assert-uim-equal '("o" "e" "u" "i" "a" "s")
                    '(rk-lib-expect-seq '("s") test-rk-rule))
  (assert-uim-equal ()
                    '(rk-lib-expect-seq '("s" "s") test-rk-rule))
  (assert-uim-equal '("p")
                    '(rk-lib-expect-seq '("p") test-rk-rule))
  (assert-uim-equal ()
                    '(rk-lib-expect-seq '("p" "p") test-rk-rule))
  #f)

(provide "test/util/test-rk")
