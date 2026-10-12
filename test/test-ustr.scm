;;; Copyright (c) 2003-2026 uim Project https://github.com/uim/uim
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

(require-extension (unittest))

(require "ustr.scm")

(define-record 'ja-kana
  '((hiragana "")
    (katakana "")
    (hankaku  "")))

(define ustr-f #f)
(define ustr-fl #f)
(define ustr-l #f)
(define ustra-f #f)
(define ustra-fl #f)
(define ustra-l #f)
(define ustrj-f #f)
(define ustrj-fl #f)
(define ustrj-l #f)
(define ustre #f)

;; Some tests change the sequences. So they are made from copies.
(define (setup)
  (set! ustr-f (ustr-new (list-copy '(("h" . "H") ("e" . "E") ("l" . "L")
                                      ("l" . "L") ("o" . "O")))))
  (set! ustr-fl (ustr-new (list-copy '(("h" . "H") ("e" . "E") ("l" . "L")))
                          (list-copy '(("l" . "L") ("o" . "O")))))
  (set! ustr-l (ustr-new '()
                         (list-copy '(("h" . "H") ("e" . "E") ("l" . "L")
                                      ("l" . "L") ("o" . "O")))))
  (set! ustra-f (ustr-new (list-copy '("h" "e" "l" "l" "o"))))
  (set! ustra-fl (ustr-new (list-copy '("h" "e" "l"))
                           (list-copy '("l" "o"))))
  (set! ustra-l (ustr-new '()
                          (list-copy '("h" "e" "l" "l" "o"))))
  (set! ustrj-f (ustr-new (list-copy '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ")
                                       ("ん" "ン" "ﾝ") ("ご" "ゴ" "ｺﾞ")
                                       ("じゃ" "ジャ" "ｼﾞｬ")))))
  (set! ustrj-fl (ustr-new (list-copy '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ")
                                        ("ん" "ン" "ﾝ")))
                           (list-copy '(("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ")))))
  (set! ustrj-l (ustr-new '()
                          (list-copy '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ")
                                       ("ん" "ン" "ﾝ") ("ご" "ゴ" "ｺﾞ")
                                       ("じゃ" "ジャ" "ｼﾞｬ")))))
  (set! ustre (ustr-new '())))

(test-begin "ustr-new")
(setup)
;; single sequence goes into former
(test-equal '(("o" "l" "l" "e" "h") . ())
            (ustr-new '("h" "e" "l" "l" "o")))
;; dual sequences are go into former and latter
(test-equal '(("l" "e" "h") . ("l" "o"))
            (ustr-new '("h" "e" "l")
                      '("l" "o")))
;; latter sequence only
(test-equal '(() . ("h" "e" "l" "l" "o"))
            (ustr-new '()
                      '("h" "e" "l" "l" "o")))
(test-end)

(test-begin "ustr-whole-seq")
(setup)
(test-equal '(("h" . "H") ("e" . "E") ("l" . "L") ("l" . "L") ("o" . "O"))
            (ustr-whole-seq ustr-fl))
(test-equal '(("h" . "H") ("e" . "E") ("l" . "L") ("l" . "L") ("o" . "O"))
            (ustr-whole-seq ustr-f))
(test-equal '(("h" . "H") ("e" . "E") ("l" . "L") ("l" . "L") ("o" . "O"))
            (ustr-whole-seq ustr-l))
(test-equal '("h" "e" "l" "l" "o")
            (ustr-whole-seq ustra-fl))
(test-equal '("h" "e" "l" "l" "o")
            (ustr-whole-seq ustra-f))
(test-equal '("h" "e" "l" "l" "o")
            (ustr-whole-seq ustra-l))
(test-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
              ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
            (ustr-whole-seq ustrj-fl))
(test-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
              ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
            (ustr-whole-seq ustrj-f))
(test-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
              ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
            (ustr-whole-seq ustrj-l))
(test-end)

(test-begin "ustr-former-seq")
(setup)
(test-equal '(("h" . "H") ("e" . "E") ("l" . "L"))
            (ustr-former-seq ustr-fl))
(test-equal '(("h" . "H") ("e" . "E") ("l" . "L") ("l" . "L") ("o" . "O"))
            (ustr-former-seq ustr-f))
(test-equal '()
            (ustr-former-seq ustr-l))
(test-equal '("h" "e" "l")
            (ustr-former-seq ustra-fl))
(test-equal '("h" "e" "l" "l" "o")
            (ustr-former-seq ustra-f))
(test-equal '()
            (ustr-former-seq ustra-l))
(test-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ"))
            (ustr-former-seq ustrj-fl))
(test-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
              ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
            (ustr-former-seq ustrj-f))
(test-equal '()
            (ustr-former-seq ustrj-l))
(test-end)

(test-begin "ustr-latter-seq")
(setup)
(test-equal '(("l" . "L") ("o" . "O"))
            (ustr-latter-seq ustr-fl))
(test-equal '()
            (ustr-latter-seq ustr-f))
(test-equal '(("h" . "H") ("e" . "E") ("l" . "L") ("l" . "L") ("o" . "O"))
            (ustr-latter-seq ustr-l))
(test-equal '("l" "o")
            (ustr-latter-seq ustra-fl))
(test-equal '()
            (ustr-latter-seq ustra-f))
(test-equal '("h" "e" "l" "l" "o")
            (ustr-latter-seq ustra-l))
(test-equal '(("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
            (ustr-latter-seq ustrj-fl))
(test-equal '()
            (ustr-latter-seq ustrj-f))
(test-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
              ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
            (ustr-latter-seq ustrj-l))
(test-end)

(test-begin "ustr-set-whole-seq!")
(setup)
;; former-latter
(test-equal '("h" "e" "l" "l" "o")
            (ustr-whole-seq ustra-fl))
(test-equal '("h" "e" "l")
            (ustr-former-seq ustra-fl))
(test-equal '("l" "o")
            (ustr-latter-seq ustra-fl))
(ustr-set-whole-seq! ustra-fl '("w" "o" "r" "L" "d"))
(test-equal '("w" "o" "r" "L" "d")
            (ustr-whole-seq ustra-fl))
(test-equal '("w" "o" "r" "L" "d")
            (ustr-former-seq ustra-fl))
(test-equal '()
            (ustr-latter-seq ustra-fl))
;; former
(test-equal '("h" "e" "l" "l" "o")
            (ustr-whole-seq ustra-f))
(test-equal '("h" "e" "l" "l" "o")
            (ustr-former-seq ustra-f))
(test-equal '()
            (ustr-latter-seq ustra-f))
(ustr-set-whole-seq! ustra-f '("w" "o" "r" "L" "d"))
(test-equal '("w" "o" "r" "L" "d")
            (ustr-whole-seq ustra-f))
(test-equal '("w" "o" "r" "L" "d")
            (ustr-former-seq ustra-f))
(test-equal '()
            (ustr-latter-seq ustra-f))
;; latter
(test-equal '("h" "e" "l" "l" "o")
            (ustr-whole-seq ustra-l))
(test-equal '()
            (ustr-former-seq ustra-l))
(test-equal '("h" "e" "l" "l" "o")
            (ustr-latter-seq ustra-l))
(ustr-set-whole-seq! ustra-l '("w" "o" "r" "L" "d"))
(test-equal '("w" "o" "r" "L" "d")
            (ustr-whole-seq ustra-l))
(test-equal '("w" "o" "r" "L" "d")
            (ustr-former-seq ustra-l))
(test-equal '()
            (ustr-latter-seq ustra-l))
(test-end)

(test-begin "ustr-set-former-seq!")
(setup)
;; former-latter
(test-equal '("h" "e" "l" "l" "o")
            (ustr-whole-seq ustra-fl))
(test-equal '("h" "e" "l")
            (ustr-former-seq ustra-fl))
(test-equal '("l" "o")
            (ustr-latter-seq ustra-fl))
(ustr-set-former-seq! ustra-fl '("w" "o" "r" "L" "d"))
(test-equal '("w" "o" "r" "L" "d" "l" "o")
            (ustr-whole-seq ustra-fl))
(test-equal '("w" "o" "r" "L" "d")
            (ustr-former-seq ustra-fl))
(test-equal '("l" "o")
            (ustr-latter-seq ustra-fl))
;; former
(test-equal '("h" "e" "l" "l" "o")
            (ustr-whole-seq ustra-f))
(test-equal '("h" "e" "l" "l" "o")
            (ustr-former-seq ustra-f))
(test-equal '()
            (ustr-latter-seq ustra-f))
(ustr-set-former-seq! ustra-f '("w" "o" "r" "L" "d"))
(test-equal '("w" "o" "r" "L" "d")
            (ustr-whole-seq ustra-f))
(test-equal '("w" "o" "r" "L" "d")
            (ustr-former-seq ustra-f))
(test-equal '()
            (ustr-latter-seq ustra-f))
;; latter
(test-equal '("h" "e" "l" "l" "o")
            (ustr-whole-seq ustra-l))
(test-equal '()
            (ustr-former-seq ustra-l))
(test-equal '("h" "e" "l" "l" "o")
            (ustr-latter-seq ustra-l))
(ustr-set-former-seq! ustra-l '("w" "o" "r" "L" "d"))
(test-equal '("w" "o" "r" "L" "d" "h" "e" "l" "l" "o")
            (ustr-whole-seq ustra-l))
(test-equal '("w" "o" "r" "L" "d")
            (ustr-former-seq ustra-l))
(test-equal '("h" "e" "l" "l" "o")
            (ustr-latter-seq ustra-l))
(test-end)

(test-begin "ustr-set-latter-seq!")
(setup)
;; former-latter
(test-equal '("h" "e" "l" "l" "o")
            (ustr-whole-seq ustra-fl))
(test-equal '("h" "e" "l")
            (ustr-former-seq ustra-fl))
(test-equal '("l" "o")
            (ustr-latter-seq ustra-fl))
(ustr-set-latter-seq! ustra-fl '("w" "o" "r" "L" "d"))
(test-equal '("h" "e" "l" "w" "o" "r" "L" "d")
            (ustr-whole-seq ustra-fl))
(test-equal '("h" "e" "l")
            (ustr-former-seq ustra-fl))
(test-equal '("w" "o" "r" "L" "d")
            (ustr-latter-seq ustra-fl))
;; former
(test-equal '("h" "e" "l" "l" "o")
            (ustr-whole-seq ustra-f))
(test-equal '("h" "e" "l" "l" "o")
            (ustr-former-seq ustra-f))
(test-equal '()
            (ustr-latter-seq ustra-f))
(ustr-set-latter-seq! ustra-f '("w" "o" "r" "L" "d"))
(test-equal '("h" "e" "l" "l" "o" "w" "o" "r" "L" "d")
            (ustr-whole-seq ustra-f))
(test-equal '("h" "e" "l" "l" "o")
            (ustr-former-seq ustra-f))
(test-equal '("w" "o" "r" "L" "d")
            (ustr-latter-seq ustra-f))
;; latter
(test-equal '("h" "e" "l" "l" "o")
            (ustr-whole-seq ustra-l))
(test-equal '()
            (ustr-former-seq ustra-l))
(test-equal '("h" "e" "l" "l" "o")
            (ustr-latter-seq ustra-l))
(ustr-set-latter-seq! ustra-l '("w" "o" "r" "L" "d"))
(test-equal '("w" "o" "r" "L" "d")
            (ustr-whole-seq ustra-l))
(test-equal '()
            (ustr-former-seq ustra-l))
(test-equal '("w" "o" "r" "L" "d")
            (ustr-latter-seq ustra-l))
(test-end)

(test-begin "ustr-empty?")
(setup)
(test-false (ustr-empty? ustr-fl))
(test-false (ustr-empty? ustr-f))
(test-false (ustr-empty? ustr-l))

(test-false (ustr-empty? ustra-fl))
(test-false (ustr-empty? ustra-f))
(test-false (ustr-empty? ustra-l))

(test-false (ustr-empty? ustrj-fl))
(test-false (ustr-empty? ustrj-f))
(test-false (ustr-empty? ustrj-l))

(test-equal '(() . ())
            ustre)
(test-eq #t (ustr-empty? ustre))
(test-end)

(test-begin "ustr-clear!")
(setup)
(test-false (ustr-empty? ustra-fl))
(ustr-clear! ustra-fl)
(test-eq #t (ustr-empty? ustra-fl))

(test-false (ustr-empty? ustra-f))
(ustr-clear! ustra-f)
(test-eq #t (ustr-empty? ustra-f))

(test-false (ustr-empty? ustra-l))
(ustr-clear! ustra-l)
(test-eq #t (ustr-empty? ustra-l))

(test-eq #t (ustr-empty? ustre))
(ustr-clear! ustre)
(test-eq #t (ustr-empty? ustre))
(test-end)

(test-begin "ustr-clear-former!")
(setup)
(test-false (ustr-empty? ustra-fl))
(ustr-clear-former! ustra-fl)
(test-false (ustr-empty? ustra-fl))

(test-false (ustr-empty? ustra-f))
(ustr-clear-former! ustra-f)
(test-eq #t (ustr-empty? ustra-f))

(test-false (ustr-empty? ustra-l))
(ustr-clear-former! ustra-l)
(test-false (ustr-empty? ustra-l))

(test-eq #t (ustr-empty? ustre))
(ustr-clear-former! ustre)
(test-eq #t (ustr-empty? ustre))
(test-end)

(test-begin "ustr-clear-latter!")
(setup)
(test-false (ustr-empty? ustra-fl))
(ustr-clear-latter! ustra-fl)
(test-false (ustr-empty? ustra-fl))

(test-false (ustr-empty? ustra-f))
(ustr-clear-latter! ustra-f)
(test-false (ustr-empty? ustra-f))

(test-false (ustr-empty? ustra-l))
(ustr-clear-latter! ustra-l)
(test-eq #t (ustr-empty? ustra-l))

(test-eq #t (ustr-empty? ustre))
(ustr-clear-latter! ustre)
(test-eq #t (ustr-empty? ustre))
(test-end)

(test-begin "ustr-copy!")
(setup)
(test-false (equal? ustr-fl ustra-fl))
(ustr-copy! ustr-fl ustra-fl)
(test-eq #t (equal? ustr-fl ustra-fl))

(test-false (equal? ustr-f ustra-f))
(ustr-copy! ustr-f ustra-f)
(test-eq #t (equal? ustr-f ustra-f))

(test-false (equal? ustr-l ustra-l))
(ustr-copy! ustr-l ustra-l)
(test-eq #t (equal? ustr-l ustra-l))
(test-end)

(test-begin "ustr=")
(setup)
(test-eq #t (ustr= equal? ustr-fl ustr-fl))
(test-eq #t (ustr= equal? ustr-fl ustr-f))
(test-eq #t (ustr= equal? ustr-f ustr-fl))
(test-eq #t (ustr= equal? ustr-fl ustr-l))
(test-eq #t (ustr= equal? ustr-l ustr-fl))
(test-eq #t (ustr= equal? ustr-f ustr-f))
(test-eq #t (ustr= equal? ustr-l ustr-f))
(test-eq #t (ustr= equal? ustr-f ustr-l))
(test-eq #t (ustr= equal? ustr-l ustr-l))
(test-false (ustr= equal? ustr-l ustre))
(test-false (ustr= equal? ustre ustr-l))

(test-eq #t (ustr= string=? ustra-fl ustra-fl))
(test-eq #t (ustr= string=? ustra-fl ustra-f))
(test-eq #t (ustr= string=? ustra-f ustra-fl))
(test-eq #t (ustr= string=? ustra-fl ustra-l))
(test-eq #t (ustr= string=? ustra-l ustra-fl))
(test-eq #t (ustr= string=? ustra-f ustra-f))
(test-eq #t (ustr= string=? ustra-l ustra-f))
(test-eq #t (ustr= string=? ustra-f ustra-l))
(test-eq #t (ustr= string=? ustra-l ustra-l))
(test-false (ustr= string=? ustra-fl ustre))
(test-false (ustr= string=? ustre ustra-fl))
(test-false (ustr= string=? ustra-f ustre))
(test-false (ustr= string=? ustre ustra-f))
(test-false (ustr= string=? ustra-l ustre))
(test-false (ustr= string=? ustre ustra-l))
(ustr-set-former-seq! ustra-f '("h" "e" "l" "l" "o" "!"))
(test-false (ustr= string=? ustra-fl ustra-f))
(test-false (ustr= string=? ustra-f ustra-fl))
(test-false (ustr= string=? ustra-l ustra-f))
(test-false (ustr= string=? ustra-f ustra-l))
(test-eq #t (ustr= string=? ustra-f ustra-f))

(test-eq #t (ustr= equal? ustrj-fl ustrj-fl))
(test-eq #t (ustr= equal? ustrj-fl ustrj-f))
(test-eq #t (ustr= equal? ustrj-f ustrj-fl))
(test-eq #t (ustr= equal? ustrj-fl ustrj-l))
(test-eq #t (ustr= equal? ustrj-l ustrj-fl))
(test-eq #t (ustr= equal? ustrj-f ustrj-f))
(test-eq #t (ustr= equal? ustrj-l ustrj-f))
(test-eq #t (ustr= equal? ustrj-f ustrj-l))
(test-eq #t (ustr= equal? ustrj-l ustrj-l))
(test-false (ustr= equal? ustrj-l ustre))
(test-false (ustr= equal? ustre ustrj-l))
(test-end)

(test-begin "ustr-length")
(setup)
(test-equal 5
            (ustr-length ustr-fl))
(test-equal 5
            (ustr-length ustr-f))
(test-equal 5
            (ustr-length ustr-l))
(test-equal 5
            (ustr-length ustra-fl))
(test-equal 5
            (ustr-length ustra-f))
(test-equal 5
            (ustr-length ustra-l))
(test-equal 5
            (ustr-length ustrj-fl))
(test-equal 5
            (ustr-length ustrj-f))
(test-equal 5
            (ustr-length ustrj-l))
(test-equal 0
            (ustr-length ustre))
(test-end)

(test-begin "ustr-nth")
(setup)
(test-equal "h"
            (ustr-nth ustra-fl 0))
(test-equal "e"
            (ustr-nth ustra-fl 1))
(test-equal "l"
            (ustr-nth ustra-fl 2))
(test-equal "l"
            (ustr-nth ustra-fl 3))
(test-equal "o"
            (ustr-nth ustra-fl 4))
(test-error (ustr-nth ustra-fl 5))
(test-error (ustr-nth ustra-fl -1))

(test-equal "h"
            (ustr-nth ustra-f 0))
(test-equal "e"
            (ustr-nth ustra-f 1))
(test-equal "l"
            (ustr-nth ustra-f 2))
(test-equal "l"
            (ustr-nth ustra-f 3))
(test-equal "o"
            (ustr-nth ustra-f 4))
(test-error (ustr-nth ustra-f 5))
(test-error (ustr-nth ustra-f -1))

(test-equal "h"
            (ustr-nth ustra-l 0))
(test-equal "e"
            (ustr-nth ustra-l 1))
(test-equal "l"
            (ustr-nth ustra-l 2))
(test-equal "l"
            (ustr-nth ustra-l 3))
(test-equal "o"
            (ustr-nth ustra-l 4))
(test-error (ustr-nth ustra-l 5))
(test-error (ustr-nth ustra-l -1))

(test-error (ustr-nth ustre 0))
(test-end)

(test-begin "ustr-set-nth!")
(setup)
(test-equal "h"
            (ustr-nth ustra-fl 0))
(test-equal "e"
            (ustr-nth ustra-fl 1))
(test-equal "l"
            (ustr-nth ustra-fl 2))
(test-equal "l"
            (ustr-nth ustra-fl 3))
(test-equal "o"
            (ustr-nth ustra-fl 4))
(test-error (ustr-nth ustra-fl 5))
(test-error (ustr-nth ustra-fl -1))
;; position 0
(test-equal 5
            (ustr-length ustra-fl))
(test-equal 3
            (ustr-cursor-pos ustra-fl))
(ustr-set-nth! ustra-fl 0 "H")
(test-equal "H"
            (ustr-nth ustra-fl 0))
(test-equal '("H" "e" "l")
            (ustr-former-seq ustra-fl))
(test-equal '("l" "o")
            (ustr-latter-seq ustra-fl))
(test-equal 5
            (ustr-length ustra-fl))
(test-equal 3
            (ustr-cursor-pos ustra-fl))
;; position 1
(test-equal 5
            (ustr-length ustra-fl))
(test-equal 3
            (ustr-cursor-pos ustra-fl))
(ustr-set-nth! ustra-fl 1 "E")
(test-equal "E"
            (ustr-nth ustra-fl 1))
(test-equal '("H" "E" "l")
            (ustr-former-seq ustra-fl))
(test-equal '("l" "o")
            (ustr-latter-seq ustra-fl))
(test-equal 5
            (ustr-length ustra-fl))
(test-equal 3
            (ustr-cursor-pos ustra-fl))
;; position 2
(test-equal 5
            (ustr-length ustra-fl))
(test-equal 3
            (ustr-cursor-pos ustra-fl))
(ustr-set-nth! ustra-fl 2 "L")
(test-equal "L"
            (ustr-nth ustra-fl 2))
(test-equal '("H" "E" "L")
            (ustr-former-seq ustra-fl))
(test-equal '("l" "o")
            (ustr-latter-seq ustra-fl))
(test-equal 5
            (ustr-length ustra-fl))
(test-equal 3
            (ustr-cursor-pos ustra-fl))
;; position 3
(test-equal 5
            (ustr-length ustra-fl))
(test-equal 3
            (ustr-cursor-pos ustra-fl))
(ustr-set-nth! ustra-fl 3 "|")
(test-equal "|"
            (ustr-nth ustra-fl 3))
(test-equal '("H" "E" "L")
            (ustr-former-seq ustra-fl))
(test-equal '("|" "o")
            (ustr-latter-seq ustra-fl))
(test-equal 5
            (ustr-length ustra-fl))
(test-equal 3
            (ustr-cursor-pos ustra-fl))
;; position 4
(test-equal 5
            (ustr-length ustra-fl))
(test-equal 3
            (ustr-cursor-pos ustra-fl))
(ustr-set-nth! ustra-fl 4 "O")
(test-equal "O"
            (ustr-nth ustra-fl 4))
(test-equal '("H" "E" "L")
            (ustr-former-seq ustra-fl))
(test-equal '("|" "O")
            (ustr-latter-seq ustra-fl))
(test-equal 5
            (ustr-length ustra-fl))
(test-equal 3
            (ustr-cursor-pos ustra-fl))
;; position 5
(test-equal 5
            (ustr-length ustra-fl))
(test-equal 3
            (ustr-cursor-pos ustra-fl))
(test-error (ustr-set-nth! ustra-fl 5 "5"))
(test-error (ustr-nth ustra-fl 5))
(test-equal '("H" "E" "L")
            (ustr-former-seq ustra-fl))
(test-equal '("|" "O")
            (ustr-latter-seq ustra-fl))
(test-equal 5
            (ustr-length ustra-fl))
(test-equal 3
            (ustr-cursor-pos ustra-fl))
;; position 5 in former-str
(test-equal '("h" "e" "l" "l" "o")
            (ustr-former-seq ustra-f))
(test-equal '()
            (ustr-latter-seq ustra-f))
(test-equal 5
            (ustr-length ustra-f))
(test-equal 5
            (ustr-cursor-pos ustra-f))
(test-error (ustr-set-nth! ustra-f 5 "5"))
(test-error (ustr-nth ustra-f 5))
(test-equal '("h" "e" "l" "l" "o")
            (ustr-former-seq ustra-f))
(test-equal '()
            (ustr-latter-seq ustra-f))
(test-equal 5
            (ustr-length ustra-f))
(test-equal 5
            (ustr-cursor-pos ustra-f))
;; position 4 in former-str
(test-equal '("h" "e" "l" "l" "o")
            (ustr-former-seq ustra-f))
(test-equal '()
            (ustr-latter-seq ustra-f))
(test-equal 5
            (ustr-length ustra-f))
(test-equal 5
            (ustr-cursor-pos ustra-f))
(ustr-set-nth! ustra-f 4 "O")
(test-equal "O"
            (ustr-nth ustra-f 4))
(test-equal '("h" "e" "l" "l" "O")
            (ustr-former-seq ustra-f))
(test-equal '()
            (ustr-latter-seq ustra-f))
(test-equal 5
            (ustr-length ustra-f))
(test-equal 5
            (ustr-cursor-pos ustra-f))
;; position 0 in latter-str
(test-equal '()
            (ustr-former-seq ustra-l))
(test-equal '("h" "e" "l" "l" "o")
            (ustr-latter-seq ustra-l))
(test-equal 5
            (ustr-length ustra-l))
(test-equal 0
            (ustr-cursor-pos ustra-l))
(ustr-set-nth! ustra-l 0 "H")
(test-equal "H"
            (ustr-nth ustra-l 0))
(test-equal '()
            (ustr-former-seq ustra-l))
(test-equal '("H" "e" "l" "l" "o")
            (ustr-latter-seq ustra-l))
(test-equal 5
            (ustr-length ustra-l))
(test-equal 0
            (ustr-cursor-pos ustra-l))
(test-end)

(test-begin "ustr-ref")
(setup)
(test-eq #t (eq? (ustr-ref ustra-fl 0)
                 (nthcdr 2 (ustr-former ustra-fl))))
(test-eq #t (eq? (ustr-ref ustra-fl 1)
                 (nthcdr 1 (ustr-former ustra-fl))))
(test-eq #t (eq? (ustr-ref ustra-fl 2)
                 (nthcdr 0 (ustr-former ustra-fl))))
(test-eq #t (eq? (ustr-ref ustra-fl 3)
                 (nthcdr 0 (ustr-latter ustra-fl))))
(test-eq #t (eq? (ustr-ref ustra-fl 4)
                 (nthcdr 1 (ustr-latter ustra-fl))))
(test-error (ustr-ref ustra-fl 5))
(test-error (ustr-ref ustra-fl -1))
;; former-str
(test-eq #t (eq? (ustr-ref ustra-f 4)
                 (nthcdr 0 (ustr-former ustra-f))))
(test-error (ustr-ref ustra-f 5))
;; latter-str
(test-eq #t (eq? (ustr-ref ustra-l 0)
                 (nthcdr 0 (ustr-latter ustra-l))))
(test-end)

(test-begin "ustr-append!")
(setup)
;; former-latter
(test-equal '("ん" "ン" "ﾝ")
            (ustr-cursor-backside ustrj-fl))
(test-equal 5
            (ustr-length ustrj-fl))
(test-equal 3
            (ustr-cursor-pos ustrj-fl))
(ustr-append! ustrj-fl '(("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ")
                         ("よ" "ヨ" "ﾖ")))
(test-equal '("ん" "ン" "ﾝ")
            (ustr-cursor-backside ustrj-fl))
(test-equal 8
            (ustr-length ustrj-fl))
(test-equal 3
            (ustr-cursor-pos ustrj-fl))
(test-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
              ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ")
              ("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ") ("よ" "ヨ" "ﾖ"))
            (ustr-whole-seq ustrj-fl))
;; former
(test-equal '("じゃ" "ジャ" "ｼﾞｬ")
            (ustr-cursor-backside ustrj-f))
(test-equal 5
            (ustr-length ustrj-f))
(test-equal 5
            (ustr-cursor-pos ustrj-f))
(ustr-append! ustrj-f '(("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ")
                        ("よ" "ヨ" "ﾖ")))
(test-equal '("じゃ" "ジャ" "ｼﾞｬ")
            (ustr-cursor-backside ustrj-f))
(test-equal 8
            (ustr-length ustrj-f))
(test-equal 5
            (ustr-cursor-pos ustrj-f))
(test-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
              ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ")
              ("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ") ("よ" "ヨ" "ﾖ"))
            (ustr-whole-seq ustrj-f))
;; latter
(test-error (ustr-cursor-backside ustrj-l))
(test-equal 5
            (ustr-length ustrj-l))
(test-equal 0
            (ustr-cursor-pos ustrj-l))
(ustr-append! ustrj-l '(("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ")
                        ("よ" "ヨ" "ﾖ")))
(test-error (ustr-cursor-backside ustrj-l))
(test-equal 8
            (ustr-length ustrj-l))
(test-equal 0
            (ustr-cursor-pos ustrj-l))
(test-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
              ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ")
              ("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ") ("よ" "ヨ" "ﾖ"))
            (ustr-whole-seq ustrj-l))
;; empty
(test-error (ustr-cursor-backside ustre))
(test-equal 0
            (ustr-length ustre))
(test-equal 0
            (ustr-cursor-pos ustre))
(ustr-append! ustre '(("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ")
                      ("よ" "ヨ" "ﾖ")))
(test-error (ustr-cursor-backside ustre))
(test-equal 3
            (ustr-length ustre))
(test-equal 0
            (ustr-cursor-pos ustre))
(test-equal '(("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ") ("よ" "ヨ" "ﾖ"))
            (ustr-whole-seq ustre))
(test-end)

(test-begin "ustr-append!-#2")
(setup)
;; former-latter
(test-equal '("ん" "ン" "ﾝ")
            (ustr-cursor-backside ustrj-fl))
(test-equal 5
            (ustr-length ustrj-fl))
(test-equal 3
            (ustr-cursor-pos ustrj-fl))
(ustr-append! ustrj-fl '())
(test-equal '("ん" "ン" "ﾝ")
            (ustr-cursor-backside ustrj-fl))
(test-equal 5
            (ustr-length ustrj-fl))
(test-equal 3
            (ustr-cursor-pos ustrj-fl))
(test-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
              ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
            (ustr-whole-seq ustrj-fl))
;; former
(test-equal '("じゃ" "ジャ" "ｼﾞｬ")
            (ustr-cursor-backside ustrj-f))
(test-equal 5
            (ustr-length ustrj-f))
(test-equal 5
            (ustr-cursor-pos ustrj-f))
(ustr-append! ustrj-f '())
(test-equal '("じゃ" "ジャ" "ｼﾞｬ")
            (ustr-cursor-backside ustrj-f))
(test-equal 5
            (ustr-length ustrj-f))
(test-equal 5
            (ustr-cursor-pos ustrj-f))
(test-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
              ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
            (ustr-whole-seq ustrj-f))
;; latter
(test-error (ustr-cursor-backside ustrj-l))
(test-equal 5
            (ustr-length ustrj-l))
(test-equal 0
            (ustr-cursor-pos ustrj-l))
(ustr-append! ustrj-l '())
(test-error (ustr-cursor-backside ustrj-l))
(test-equal 5
            (ustr-length ustrj-l))
(test-equal 0
            (ustr-cursor-pos ustrj-l))
(test-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
              ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
            (ustr-whole-seq ustrj-l))
;; empty
(test-error (ustr-cursor-backside ustre))
(test-equal 0
            (ustr-length ustre))
(test-equal 0
            (ustr-cursor-pos ustre))
(ustr-append! ustre '())
(test-error (ustr-cursor-backside ustre))
(test-equal 0
            (ustr-length ustre))
(test-equal 0
            (ustr-cursor-pos ustre))
(test-end)

(test-begin "ustr-prepend!")
(setup)
;; former-latter
(test-equal '("ん" "ン" "ﾝ")
            (ustr-cursor-backside ustrj-fl))
(test-equal 5
            (ustr-length ustrj-fl))
(test-equal 3
            (ustr-cursor-pos ustrj-fl))
(ustr-prepend! ustrj-fl '(("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ")
                          ("よ" "ヨ" "ﾖ")))
(test-equal '("ん" "ン" "ﾝ")
            (ustr-cursor-backside ustrj-fl))
(test-equal 8
            (ustr-length ustrj-fl))
(test-equal 6
            (ustr-cursor-pos ustrj-fl))
(test-equal '(("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ") ("よ" "ヨ" "ﾖ")
              ("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
              ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
            (ustr-whole-seq ustrj-fl))
;; former
(test-equal '("じゃ" "ジャ" "ｼﾞｬ")
            (ustr-cursor-backside ustrj-f))
(test-equal 5
            (ustr-length ustrj-f))
(test-equal 5
            (ustr-cursor-pos ustrj-f))
(ustr-prepend! ustrj-f '(("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ")
                         ("よ" "ヨ" "ﾖ")))
(test-equal '("じゃ" "ジャ" "ｼﾞｬ")
            (ustr-cursor-backside ustrj-f))
(test-equal 8
            (ustr-length ustrj-f))
(test-equal 8
            (ustr-cursor-pos ustrj-f))
(test-equal '(("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ") ("よ" "ヨ" "ﾖ")
              ("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
              ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
            (ustr-whole-seq ustrj-f))
;; latter
(test-error (ustr-cursor-backside ustrj-l))
(test-equal 5
            (ustr-length ustrj-l))
(test-equal 0
            (ustr-cursor-pos ustrj-l))
(ustr-prepend! ustrj-l '(("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ")
                         ("よ" "ヨ" "ﾖ")))
(test-equal '("よ" "ヨ" "ﾖ")
            (ustr-cursor-backside ustrj-l))
(test-equal 8
            (ustr-length ustrj-l))
(test-equal 3
            (ustr-cursor-pos ustrj-l))
(test-equal '(("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ") ("よ" "ヨ" "ﾖ")
              ("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
              ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
            (ustr-whole-seq ustrj-l))
;; empty
(test-error (ustr-cursor-backside ustre))
(test-equal 0
            (ustr-length ustre))
(test-equal 0
            (ustr-cursor-pos ustre))
(ustr-prepend! ustre '(("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ")
                       ("よ" "ヨ" "ﾖ")))
(test-equal '("よ" "ヨ" "ﾖ")
            (ustr-cursor-backside ustre))
(test-equal 3
            (ustr-length ustre))
(test-equal 3
            (ustr-cursor-pos ustre))
(test-equal '(("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ") ("よ" "ヨ" "ﾖ"))
            (ustr-whole-seq ustre))
(test-end)

(test-begin "ustr-prepend!-#2")
(setup)
;; former-latter
(test-equal '("ん" "ン" "ﾝ")
            (ustr-cursor-backside ustrj-fl))
(test-equal 5
            (ustr-length ustrj-fl))
(test-equal 3
            (ustr-cursor-pos ustrj-fl))
(ustr-prepend! ustrj-fl '())
(test-equal '("ん" "ン" "ﾝ")
            (ustr-cursor-backside ustrj-fl))
(test-equal 5
            (ustr-length ustrj-fl))
(test-equal 3
            (ustr-cursor-pos ustrj-fl))
(test-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
              ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
            (ustr-whole-seq ustrj-fl))
;; former
(test-equal '("じゃ" "ジャ" "ｼﾞｬ")
            (ustr-cursor-backside ustrj-f))
(test-equal 5
            (ustr-length ustrj-f))
(test-equal 5
            (ustr-cursor-pos ustrj-f))
(ustr-prepend! ustrj-f '())
(test-equal '("じゃ" "ジャ" "ｼﾞｬ")
            (ustr-cursor-backside ustrj-f))
(test-equal 5
            (ustr-length ustrj-f))
(test-equal 5
            (ustr-cursor-pos ustrj-f))
(test-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
              ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
            (ustr-whole-seq ustrj-f))
;; latter
(test-error (ustr-cursor-backside ustrj-l))
(test-equal 5
            (ustr-length ustrj-l))
(test-equal 0
            (ustr-cursor-pos ustrj-l))
(ustr-prepend! ustrj-l '())
(test-error (ustr-cursor-backside ustrj-l))
(test-equal 5
            (ustr-length ustrj-l))
(test-equal 0
            (ustr-cursor-pos ustrj-l))
(test-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
              ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
            (ustr-whole-seq ustrj-l))
;; empty
(test-error (ustr-cursor-backside ustre))
(test-equal 0
            (ustr-length ustre))
(test-equal 0
            (ustr-cursor-pos ustre))
(ustr-prepend! ustre '())
(test-error (ustr-cursor-backside ustre))
(test-equal 0
            (ustr-length ustre))
(test-equal 0
            (ustr-cursor-pos ustre))
(test-end)

(test-begin "map-ustr-whole")
(setup)
;; former-latter
(test-equal '("h" "e" "l" "l" "o")
            (map-ustr-whole car ustr-fl))
(test-equal '("H" "E" "L" "L" "O")
            (map-ustr-whole cdr ustr-fl))
(test-equal '("に" "ほ" "ん" "ご" "じゃ")
            (map-ustr-whole ja-kana-hiragana ustrj-fl))
(test-equal '("ニ" "ホ" "ン" "ゴ" "ジャ")
            (map-ustr-whole ja-kana-katakana ustrj-fl))
(test-equal '("ﾆ" "ﾎ" "ﾝ" "ｺﾞ" "ｼﾞｬ")
            (map-ustr-whole ja-kana-hankaku ustrj-fl))
;; former
(test-equal '("h" "e" "l" "l" "o")
            (map-ustr-whole car ustr-f))
(test-equal '("H" "E" "L" "L" "O")
            (map-ustr-whole cdr ustr-f))
(test-equal '("に" "ほ" "ん" "ご" "じゃ")
            (map-ustr-whole ja-kana-hiragana ustrj-f))
(test-equal '("ニ" "ホ" "ン" "ゴ" "ジャ")
            (map-ustr-whole ja-kana-katakana ustrj-f))
(test-equal '("ﾆ" "ﾎ" "ﾝ" "ｺﾞ" "ｼﾞｬ")
            (map-ustr-whole ja-kana-hankaku ustrj-f))
;; latter
(test-equal '("h" "e" "l" "l" "o")
            (map-ustr-whole car ustr-l))
(test-equal '("H" "E" "L" "L" "O")
            (map-ustr-whole cdr ustr-l))
(test-equal '("に" "ほ" "ん" "ご" "じゃ")
            (map-ustr-whole ja-kana-hiragana ustrj-l))
(test-equal '("ニ" "ホ" "ン" "ゴ" "ジャ")
            (map-ustr-whole ja-kana-katakana ustrj-l))
(test-equal '("ﾆ" "ﾎ" "ﾝ" "ｺﾞ" "ｼﾞｬ")
            (map-ustr-whole ja-kana-hankaku ustrj-l))
;; empty
(test-equal '()
            (map-ustr-whole cdr ustre))
(test-end)

(test-begin "map-ustr-former")
(setup)
;; former-latter
(test-equal '("h" "e" "l")
            (map-ustr-former car ustr-fl))
(test-equal '("H" "E" "L")
            (map-ustr-former cdr ustr-fl))
(test-equal '("に" "ほ" "ん")
            (map-ustr-former ja-kana-hiragana ustrj-fl))
(test-equal '("ニ" "ホ" "ン")
            (map-ustr-former ja-kana-katakana ustrj-fl))
(test-equal '("ﾆ" "ﾎ" "ﾝ")
            (map-ustr-former ja-kana-hankaku ustrj-fl))
;; former
(test-equal '("h" "e" "l" "l" "o")
            (map-ustr-former car ustr-f))
(test-equal '("H" "E" "L" "L" "O")
            (map-ustr-former cdr ustr-f))
(test-equal '("に" "ほ" "ん" "ご" "じゃ")
            (map-ustr-former ja-kana-hiragana ustrj-f))
(test-equal '("ニ" "ホ" "ン" "ゴ" "ジャ")
            (map-ustr-former ja-kana-katakana ustrj-f))
(test-equal '("ﾆ" "ﾎ" "ﾝ" "ｺﾞ" "ｼﾞｬ")
            (map-ustr-former ja-kana-hankaku ustrj-f))
;; latter
(test-equal '()
            (map-ustr-former car ustr-l))
(test-equal '()
            (map-ustr-former cdr ustr-l))
(test-equal '()
            (map-ustr-former ja-kana-hiragana ustrj-l))
(test-equal '()
            (map-ustr-former ja-kana-katakana ustrj-l))
(test-equal '()
            (map-ustr-former ja-kana-hankaku ustrj-l))
;; empty
(test-equal '()
            (map-ustr-former cdr ustre))
(test-end)

(test-begin "map-ustr-latter")
(setup)
;; former-latter
(test-equal '("l" "o")
            (map-ustr-latter car ustr-fl))
(test-equal '("L" "O")
            (map-ustr-latter cdr ustr-fl))
(test-equal '("ご" "じゃ")
            (map-ustr-latter ja-kana-hiragana ustrj-fl))
(test-equal '("ゴ" "ジャ")
            (map-ustr-latter ja-kana-katakana ustrj-fl))
(test-equal '("ｺﾞ" "ｼﾞｬ")
            (map-ustr-latter ja-kana-hankaku ustrj-fl))
;; former
(test-equal '()
            (map-ustr-latter car ustr-f))
(test-equal '()
            (map-ustr-latter cdr ustr-f))
(test-equal '()
            (map-ustr-latter ja-kana-hiragana ustrj-f))
(test-equal '()
            (map-ustr-latter ja-kana-katakana ustrj-f))
(test-equal '()
            (map-ustr-latter ja-kana-hankaku ustrj-f))
;; latter
(test-equal '("h" "e" "l" "l" "o")
            (map-ustr-latter car ustr-l))
(test-equal '("H" "E" "L" "L" "O")
            (map-ustr-latter cdr ustr-l))
(test-equal '("に" "ほ" "ん" "ご" "じゃ")
            (map-ustr-latter ja-kana-hiragana ustrj-l))
(test-equal '("ニ" "ホ" "ン" "ゴ" "ジャ")
            (map-ustr-latter ja-kana-katakana ustrj-l))
(test-equal '("ﾆ" "ﾎ" "ﾝ" "ｺﾞ" "ｼﾞｬ")
            (map-ustr-latter ja-kana-hankaku ustrj-l))
;; empty
(test-equal '()
            (map-ustr-latter cdr ustre))
(test-end)

(test-begin "append-map-ustr-whole")
(setup)
(test-equal '("ニ" "ﾆ" "ホ" "ﾎ" "ン" "ﾝ" "ゴ" "ｺﾞ" "ジャ" "ｼﾞｬ")
            (append-map-ustr-whole cdr ustrj-fl))
(test-equal '("ニ" "ﾆ" "ホ" "ﾎ" "ン" "ﾝ" "ゴ" "ｺﾞ" "ジャ" "ｼﾞｬ")
            (append-map-ustr-whole cdr ustrj-f))
(test-equal '("ニ" "ﾆ" "ホ" "ﾎ" "ン" "ﾝ" "ゴ" "ｺﾞ" "ジャ" "ｼﾞｬ")
            (append-map-ustr-whole cdr ustrj-l))
(test-equal '()
            (append-map-ustr-whole cdr ustre))
(test-end)

(test-begin "append-map-ustr-former")
(setup)
(test-equal '("ニ" "ﾆ" "ホ" "ﾎ" "ン" "ﾝ")
            (append-map-ustr-former cdr ustrj-fl))
(test-equal '("ニ" "ﾆ" "ホ" "ﾎ" "ン" "ﾝ" "ゴ" "ｺﾞ" "ジャ" "ｼﾞｬ")
            (append-map-ustr-former cdr ustrj-f))
(test-equal '()
            (append-map-ustr-former cdr ustrj-l))
(test-equal '()
            (append-map-ustr-former cdr ustre))
(test-end)

(test-begin "append-map-ustr-latter")
(setup)
(test-equal '("ゴ" "ｺﾞ" "ジャ" "ｼﾞｬ")
            (append-map-ustr-latter cdr ustrj-fl))
(test-equal '()
            (append-map-ustr-latter cdr ustrj-f))
(test-equal '("ニ" "ﾆ" "ホ" "ﾎ" "ン" "ﾝ" "ゴ" "ｺﾞ" "ジャ" "ｼﾞｬ")
            (append-map-ustr-latter cdr ustrj-l))
(test-equal '()
            (append-map-ustr-latter cdr ustre))
(test-end)

(test-begin "string-append-map-ustr-whole")
(setup)
;; former-latter
(test-equal "hello"
            (string-append-map-ustr-whole car ustr-fl))
(test-equal "HELLO"
            (string-append-map-ustr-whole cdr ustr-fl))
(test-equal "にほんごじゃ"
            (string-append-map-ustr-whole ja-kana-hiragana
                                          ustrj-fl))
(test-equal "ニホンゴジャ"
            (string-append-map-ustr-whole ja-kana-katakana
                                          ustrj-fl))
(test-equal "ﾆﾎﾝｺﾞｼﾞｬ"
            (string-append-map-ustr-whole ja-kana-hankaku
                                          ustrj-fl))
;; former
(test-equal "hello"
            (string-append-map-ustr-whole car ustr-f))
(test-equal "HELLO"
            (string-append-map-ustr-whole cdr ustr-f))
(test-equal "にほんごじゃ"
            (string-append-map-ustr-whole ja-kana-hiragana
                                          ustrj-f))
(test-equal "ニホンゴジャ"
            (string-append-map-ustr-whole ja-kana-katakana
                                          ustrj-f))
(test-equal "ﾆﾎﾝｺﾞｼﾞｬ"
            (string-append-map-ustr-whole ja-kana-hankaku
                                          ustrj-f))
;; latter
(test-equal "hello"
            (string-append-map-ustr-whole car ustr-l))
(test-equal "HELLO"
            (string-append-map-ustr-whole cdr ustr-l))
(test-equal "にほんごじゃ"
            (string-append-map-ustr-whole ja-kana-hiragana
                                          ustrj-l))
(test-equal "ニホンゴジャ"
            (string-append-map-ustr-whole ja-kana-katakana
                                          ustrj-l))
(test-equal "ﾆﾎﾝｺﾞｼﾞｬ"
            (string-append-map-ustr-whole ja-kana-hankaku
                                          ustrj-l))
;; empty
(test-equal ""
            (string-append-map-ustr-whole cdr ustre))
(test-end)

(test-begin "string-append-map-ustr-former")
(setup)
;; former-latter
(test-equal "hel"
            (string-append-map-ustr-former car ustr-fl))
(test-equal "HEL"
            (string-append-map-ustr-former cdr ustr-fl))
(test-equal "にほん"
            (string-append-map-ustr-former ja-kana-hiragana
                                           ustrj-fl))
(test-equal "ニホン"
            (string-append-map-ustr-former ja-kana-katakana
                                           ustrj-fl))
(test-equal "ﾆﾎﾝ"
            (string-append-map-ustr-former ja-kana-hankaku
                                           ustrj-fl))
;; former
(test-equal "hello"
            (string-append-map-ustr-former car ustr-f))
(test-equal "HELLO"
            (string-append-map-ustr-former cdr ustr-f))
(test-equal "にほんごじゃ"
            (string-append-map-ustr-former ja-kana-hiragana
                                           ustrj-f))
(test-equal "ニホンゴジャ"
            (string-append-map-ustr-former ja-kana-katakana
                                           ustrj-f))
(test-equal "ﾆﾎﾝｺﾞｼﾞｬ"
            (string-append-map-ustr-former ja-kana-hankaku
                                           ustrj-f))
;; latter
(test-equal ""
            (string-append-map-ustr-former car ustr-l))
(test-equal ""
            (string-append-map-ustr-former cdr ustr-l))
(test-equal ""
            (string-append-map-ustr-former ja-kana-hiragana
                                           ustrj-l))
(test-equal ""
            (string-append-map-ustr-former ja-kana-katakana
                                           ustrj-l))
(test-equal ""
            (string-append-map-ustr-former ja-kana-hankaku
                                           ustrj-l))
;; empty
(test-equal ""
            (string-append-map-ustr-former cdr ustre))
(test-end)

(test-begin "string-append-map-ustr-latter")
(setup)
;; former-latter
(test-equal "lo"
            (string-append-map-ustr-latter car ustr-fl))
(test-equal "LO"
            (string-append-map-ustr-latter cdr ustr-fl))
(test-equal "ごじゃ"
            (string-append-map-ustr-latter ja-kana-hiragana
                                           ustrj-fl))
(test-equal "ゴジャ"
            (string-append-map-ustr-latter ja-kana-katakana
                                           ustrj-fl))
(test-equal "ｺﾞｼﾞｬ"
            (string-append-map-ustr-latter ja-kana-hankaku
                                           ustrj-fl))
;; former
(test-equal ""
            (string-append-map-ustr-latter car ustr-f))
(test-equal ""
            (string-append-map-ustr-latter cdr ustr-f))
(test-equal ""
            (string-append-map-ustr-latter ja-kana-hiragana
                                           ustrj-f))
(test-equal ""
            (string-append-map-ustr-latter ja-kana-katakana
                                           ustrj-f))
(test-equal ""
            (string-append-map-ustr-latter ja-kana-hankaku
                                           ustrj-f))
;; latter
(test-equal "hello"
            (string-append-map-ustr-latter car ustr-l))
(test-equal "HELLO"
            (string-append-map-ustr-latter cdr ustr-l))
(test-equal "にほんごじゃ"
            (string-append-map-ustr-latter ja-kana-hiragana
                                           ustrj-l))
(test-equal "ニホンゴジャ"
            (string-append-map-ustr-latter ja-kana-katakana
                                           ustrj-l))
(test-equal "ﾆﾎﾝｺﾞｼﾞｬ"
            (string-append-map-ustr-latter ja-kana-hankaku
                                           ustrj-l))
;; empty
(test-equal ""
            (string-append-map-ustr-latter cdr ustre))
(test-end)

(test-begin "ustr-cursor-at-beginning?")
(setup)
(test-false (ustr-cursor-at-beginning? ustra-fl))
(test-false (ustr-cursor-at-beginning? ustra-f))
(test-eq #t (ustr-cursor-at-beginning? ustra-l))
(test-eq #t (ustr-cursor-at-beginning? ustre))
(test-end)

(test-begin "ustr-cursor-at-end?")
(setup)
(test-false (ustr-cursor-at-end? ustra-fl))
(test-eq #t (ustr-cursor-at-end? ustra-f))
(test-false (ustr-cursor-at-end? ustra-l))
(test-eq #t (ustr-cursor-at-end? ustre))
(test-end)

(test-begin "ustr-cursor-pos")
(setup)
(test-equal 3
            (ustr-cursor-pos ustr-fl))
(test-equal 5
            (ustr-cursor-pos ustr-f))
(test-equal 0
            (ustr-cursor-pos ustr-l))

(test-equal 3
            (ustr-cursor-pos ustrj-fl))
(test-equal 5
            (ustr-cursor-pos ustrj-f))
(test-equal 0
            (ustr-cursor-pos ustrj-l))

(test-equal 3
            (ustr-cursor-pos ustra-fl))
(test-equal 5
            (ustr-cursor-pos ustra-f))
(test-equal 0
            (ustr-cursor-pos ustra-l))

(test-equal 0
            (ustr-cursor-pos ustre))
(test-end)

(test-begin "ustr-set-cursor-pos!")
(setup)
;; former-latter
(test-equal 3
            (ustr-cursor-pos ustra-fl))
(test-eq #t (ustr-set-cursor-pos! ustra-fl 0))
(test-equal 0
            (ustr-cursor-pos ustra-fl))
(test-eq #t (ustr-set-cursor-pos! ustra-fl 5))
(test-equal 5
            (ustr-cursor-pos ustra-fl))
(test-eq #t (ustr-set-cursor-pos! ustra-fl 2))
(test-equal 2
            (ustr-cursor-pos ustra-fl))
(test-false (ustr-set-cursor-pos! ustra-fl -1))
(test-false (ustr-set-cursor-pos! ustra-fl 6))
;; empty
(test-equal 0
            (ustr-cursor-pos ustre))
(test-eq #t (ustr-set-cursor-pos! ustra-fl 0))
(test-equal 0
            (ustr-cursor-pos ustre))
(test-false (ustr-set-cursor-pos! ustre -1))
(test-false (ustr-set-cursor-pos! ustre 1))
(test-end)

(test-begin "ustr-cursor-move!")
(setup)
(test-equal 3
            (ustr-cursor-pos ustra-fl))
(test-false (ustr-cursor-move! ustra-fl -4))
(test-equal 3
            (ustr-cursor-pos ustra-fl))

(test-false (ustr-cursor-move! ustra-fl 3))
(test-equal 3
            (ustr-cursor-pos ustra-fl))

(test-eq #t (ustr-cursor-move! ustra-fl 2))
(test-equal 5
            (ustr-cursor-pos ustra-fl))

(test-eq #t (ustr-cursor-move! ustra-fl -3))
(test-equal 2
            (ustr-cursor-pos ustra-fl))

(test-eq #t (ustr-cursor-move! ustra-fl 1))
(test-equal 3
            (ustr-cursor-pos ustra-fl))

(test-eq #t (ustr-cursor-move! ustra-fl 0))
(test-equal 3
            (ustr-cursor-pos ustra-fl))

(test-eq #t (ustr-cursor-move! ustra-fl -3))
(test-equal 0
            (ustr-cursor-pos ustra-fl))

(test-equal 0
            (ustr-cursor-pos ustre))
(test-eq #t (ustr-cursor-move! ustra-fl 0))
(test-equal 0
            (ustr-cursor-pos ustre))

(test-false (ustr-cursor-move! ustre -1))
(test-false (ustr-cursor-move! ustre 1))
(test-end)

(test-begin "ustr-cursor-move-backward!")
(setup)
(test-equal 3
            (ustr-cursor-pos ustra-fl))

(ustr-cursor-move-backward! ustra-fl)
(test-equal 2
            (ustr-cursor-pos ustra-fl))

(ustr-cursor-move-backward! ustra-fl)
(test-equal 1
            (ustr-cursor-pos ustra-fl))

(ustr-cursor-move-backward! ustra-fl)
(test-equal 0
            (ustr-cursor-pos ustra-fl))

(ustr-cursor-move-backward! ustra-fl)
(test-equal 0
            (ustr-cursor-pos ustra-fl))
;; start from end of string
(test-equal 5
            (ustr-cursor-pos ustra-f))
(ustr-cursor-move-backward! ustra-f)
(test-equal 4
            (ustr-cursor-pos ustra-f))
(test-end)

(test-begin "ustr-cursor-move-forward!")
(setup)
(test-equal 3
            (ustr-cursor-pos ustra-fl))

(ustr-cursor-move-forward! ustra-fl)
(test-equal 4
            (ustr-cursor-pos ustra-fl))

(ustr-cursor-move-forward! ustra-fl)
(test-equal 5
            (ustr-cursor-pos ustra-fl))

(ustr-cursor-move-forward! ustra-fl)
(test-equal 5
            (ustr-cursor-pos ustra-fl))

;; start from beginning of string
(test-equal 0
            (ustr-cursor-pos ustra-l))
(ustr-cursor-move-forward! ustra-l)
(test-equal 1
            (ustr-cursor-pos ustra-l))
(test-end)

(test-begin "ustr-cursor-move-beginning!")
(setup)
(test-equal 3
            (ustr-cursor-pos ustra-fl))

(ustr-cursor-move-beginning! ustra-fl)
(test-equal 0
            (ustr-cursor-pos ustra-fl))

(ustr-cursor-move-beginning! ustra-fl)
(test-equal 0
            (ustr-cursor-pos ustra-fl))

;; start from end of string
(test-equal 5
            (ustr-cursor-pos ustra-f))
(ustr-cursor-move-beginning! ustra-f)
(test-equal 0
            (ustr-cursor-pos ustra-f))
(test-end)

(test-begin "ustr-cursor-move-end!")
(setup)
(test-equal 3
            (ustr-cursor-pos ustra-fl))

(ustr-cursor-move-end! ustra-fl)
(test-equal 5
            (ustr-cursor-pos ustra-fl))

(ustr-cursor-move-end! ustra-fl)
(test-equal 5
            (ustr-cursor-pos ustra-fl))

;; start from beginning of string
(test-equal 0
            (ustr-cursor-pos ustra-l))
(ustr-cursor-move-end! ustra-l)
(test-equal 5
            (ustr-cursor-pos ustra-l))
(test-end)

(test-begin "ustr-cursor-frontside")
(setup)
;; former-latter
(test-equal '("l" . "L")
            (ustr-cursor-frontside ustr-fl))
(test-equal '("ご" "ゴ" "ｺﾞ")
            (ustr-cursor-frontside ustrj-fl))
(test-equal "l"
            (ustr-cursor-frontside ustra-fl))
;; former
(test-error (ustr-cursor-frontside ustr-f))
(test-error (ustr-cursor-frontside ustrj-f))
(test-error (ustr-cursor-frontside ustra-f))
;; latter
(test-equal '("h" . "H")
            (ustr-cursor-frontside ustr-l))
(test-equal '("に" "ニ" "ﾆ")
            (ustr-cursor-frontside ustrj-l))
(test-equal "h"
            (ustr-cursor-frontside ustra-l))
;; empty
(test-error (ustr-cursor-frontside ustre))
(test-end)

(test-begin "ustr-cursor-backside")
(setup)
;; former-latter
(test-equal '("l" . "L")
            (ustr-cursor-backside ustr-fl))
(test-equal '("ん" "ン" "ﾝ")
            (ustr-cursor-backside ustrj-fl))
(test-equal "l"
            (ustr-cursor-backside ustra-fl))
;; former
(test-equal '("o" . "O")
            (ustr-cursor-backside ustr-f))
(test-equal '("じゃ" "ジャ" "ｼﾞｬ")
            (ustr-cursor-backside ustrj-f))
(test-equal "o"
            (ustr-cursor-backside ustra-f))
;; latter
(test-error (ustr-cursor-backside ustr-l))
(test-error (ustr-cursor-backside ustrj-l))
(test-error (ustr-cursor-backside ustra-l))
;; empty
(test-error (ustr-cursor-backside ustre))
(test-end)

(test-begin "ustr-cursor-delete-frontside!")
(setup)
;; former-latter
(test-equal '("ご" "ゴ" "ｺﾞ")
            (ustr-cursor-frontside ustrj-fl))
(test-equal 5
            (ustr-length ustrj-fl))
(test-equal 3
            (ustr-cursor-pos ustrj-fl))
(test-eq #t (ustr-cursor-delete-frontside! ustrj-fl))
(test-equal '("じゃ" "ジャ" "ｼﾞｬ")
            (ustr-cursor-frontside ustrj-fl))
(test-equal 4
            (ustr-length ustrj-fl))
(test-equal 3
            (ustr-cursor-pos ustrj-fl))
(test-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
              ("じゃ" "ジャ" "ｼﾞｬ"))
            (ustr-whole-seq ustrj-fl))
;; former
(test-error (ustr-cursor-frontside ustrj-f))
(test-equal 5
            (ustr-length ustrj-f))
(test-equal 5
            (ustr-cursor-pos ustrj-f))
(test-false (ustr-cursor-delete-frontside! ustrj-f))
(test-error (ustr-cursor-frontside ustrj-f))
(test-equal 5
            (ustr-length ustrj-f))
(test-equal 5
            (ustr-cursor-pos ustrj-f))
(test-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
              ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
            (ustr-whole-seq ustrj-f))
;; latter
(test-equal '("に" "ニ" "ﾆ")
            (ustr-cursor-frontside ustrj-l))
(test-equal 5
            (ustr-length ustrj-l))
(test-equal 0
            (ustr-cursor-pos ustrj-l))
(test-eq #t (ustr-cursor-delete-frontside! ustrj-l))
(test-equal '("ほ" "ホ" "ﾎ")
            (ustr-cursor-frontside ustrj-l))
(test-equal 4
            (ustr-length ustrj-l))
(test-equal 0
            (ustr-cursor-pos ustrj-l))
(test-equal '(("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
              ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
            (ustr-whole-seq ustrj-l))
;; empty
(test-error (ustr-cursor-frontside ustre))
(test-equal 0
            (ustr-length ustre))
(test-equal 0
            (ustr-cursor-pos ustre))
(test-false (ustr-cursor-delete-frontside! ustre))
(test-error (ustr-cursor-frontside ustre))
(test-equal 0
            (ustr-length ustre))
(test-equal 0
            (ustr-cursor-pos ustre))
(test-end)

(test-begin "ustr-cursor-delete-backside!")
(setup)
;; former-latter
(test-equal '("ん" "ン" "ﾝ")
            (ustr-cursor-backside ustrj-fl))
(test-equal 5
            (ustr-length ustrj-fl))
(test-equal 3
            (ustr-cursor-pos ustrj-fl))
(test-eq #t (ustr-cursor-delete-backside! ustrj-fl))
(test-equal '("ほ" "ホ" "ﾎ")
            (ustr-cursor-backside ustrj-fl))
(test-equal 4
            (ustr-length ustrj-fl))
(test-equal 2
            (ustr-cursor-pos ustrj-fl))
(test-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ")
              ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
            (ustr-whole-seq ustrj-fl))
;; former
(test-equal '("じゃ" "ジャ" "ｼﾞｬ")
            (ustr-cursor-backside ustrj-f))
(test-equal 5
            (ustr-length ustrj-f))
(test-equal 5
            (ustr-cursor-pos ustrj-f))
(test-eq #t (ustr-cursor-delete-backside! ustrj-f))
(test-equal '("ご" "ゴ" "ｺﾞ")
            (ustr-cursor-backside ustrj-f))
(test-equal 4
            (ustr-length ustrj-f))
(test-equal 4
            (ustr-cursor-pos ustrj-f))
(test-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
              ("ご" "ゴ" "ｺﾞ"))
            (ustr-whole-seq ustrj-f))
;; latter
(test-error (ustr-cursor-backside ustrj-l))
(test-equal 5
            (ustr-length ustrj-l))
(test-equal 0
            (ustr-cursor-pos ustrj-l))
(test-false (ustr-cursor-delete-backside! ustrj-l))
(test-error (ustr-cursor-backside ustrj-l))
(test-equal 5
            (ustr-length ustrj-l))
(test-equal 0
            (ustr-cursor-pos ustrj-l))
(test-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
              ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
            (ustr-whole-seq ustrj-l))
;; empty
(test-error (ustr-cursor-backside ustre))
(test-equal 0
            (ustr-length ustre))
(test-false (ustr-cursor-delete-backside! ustre))
(test-error (ustr-cursor-backside ustre))
(test-equal 0
            (ustr-length ustre))
(test-end)

(test-begin "ustr-insert-elem!")
(setup)
;; former-latter
(test-equal '("ん" "ン" "ﾝ")
            (ustr-cursor-backside ustrj-fl))
(test-equal 5
            (ustr-length ustrj-fl))
(test-equal 3
            (ustr-cursor-pos ustrj-fl))
(ustr-insert-elem! ustrj-fl '("んー" "ンー" "ﾝｰ"))
(test-equal '("んー" "ンー" "ﾝｰ")
            (ustr-cursor-backside ustrj-fl))
(test-equal 6
            (ustr-length ustrj-fl))
(test-equal 4
            (ustr-cursor-pos ustrj-fl))
(test-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
              ("んー" "ンー" "ﾝｰ")
              ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
            (ustr-whole-seq ustrj-fl))
;; former
(test-equal '("じゃ" "ジャ" "ｼﾞｬ")
            (ustr-cursor-backside ustrj-f))
(test-equal 5
            (ustr-length ustrj-f))
(test-equal 5
            (ustr-cursor-pos ustrj-f))
(ustr-insert-elem! ustrj-f '("んー" "ンー" "ﾝｰ"))
(test-equal '("んー" "ンー" "ﾝｰ")
            (ustr-cursor-backside ustrj-f))
(test-equal 6
            (ustr-length ustrj-f))
(test-equal 6
            (ustr-cursor-pos ustrj-f))
(test-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
              ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ") ("んー" "ンー" "ﾝｰ"))
            (ustr-whole-seq ustrj-f))
;; latter
(test-error (ustr-cursor-backside ustrj-l))
(test-equal 5
            (ustr-length ustrj-l))
(test-equal 0
            (ustr-cursor-pos ustrj-l))
(ustr-insert-elem! ustrj-l '("んー" "ンー" "ﾝｰ"))
(test-equal '("んー" "ンー" "ﾝｰ")
            (ustr-cursor-backside ustrj-l))
(test-equal 6
            (ustr-length ustrj-l))
(test-equal 1
            (ustr-cursor-pos ustrj-l))
(test-equal '(("んー" "ンー" "ﾝｰ")
              ("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
              ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
            (ustr-whole-seq ustrj-l))
;; empty
(test-error (ustr-cursor-backside ustre))
(test-equal 0
            (ustr-length ustre))
(ustr-insert-elem! ustre '("んー" "ンー" "ﾝｰ"))
(test-equal '("んー" "ンー" "ﾝｰ")
            (ustr-cursor-backside ustre))
(test-equal 1
            (ustr-length ustre))
(test-equal '(("んー" "ンー" "ﾝｰ"))
            (ustr-whole-seq ustre))
(test-end)

(test-begin "ustr-cursor-set-frontside!")
(setup)
;; former-latter
(test-equal '("ご" "ゴ" "ｺﾞ")
            (ustr-cursor-frontside ustrj-fl))
(test-equal 5
            (ustr-length ustrj-fl))
(test-equal 3
            (ustr-cursor-pos ustrj-fl))
(test-eq #t (ustr-cursor-set-frontside! ustrj-fl
                                        '("んー" "ンー" "ﾝｰ")))
(test-equal '("んー" "ンー" "ﾝｰ")
            (ustr-cursor-frontside ustrj-fl))
(test-equal 5
            (ustr-length ustrj-fl))
(test-equal 3
            (ustr-cursor-pos ustrj-fl))
(test-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
              ("んー" "ンー" "ﾝｰ") ("じゃ" "ジャ" "ｼﾞｬ"))
            (ustr-whole-seq ustrj-fl))
;; former
(test-error (ustr-cursor-frontside ustrj-f))
(test-equal 5
            (ustr-length ustrj-f))
(test-equal 5
            (ustr-cursor-pos ustrj-f))
(test-false (ustr-cursor-set-frontside! ustrj-f
                                        '("んー" "ンー" "ﾝｰ")))
(test-equal 5
            (ustr-length ustrj-f))
(test-equal 5
            (ustr-cursor-pos ustrj-f))
(test-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
              ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
            (ustr-whole-seq ustrj-f))
;; latter
(test-equal '("に" "ニ" "ﾆ")
            (ustr-cursor-frontside ustrj-l))
(test-equal 5
            (ustr-length ustrj-l))
(test-equal 0
            (ustr-cursor-pos ustrj-l))
(test-eq #t (ustr-cursor-set-frontside! ustrj-l
                                        '("んー" "ンー" "ﾝｰ")))
(test-equal '("んー" "ンー" "ﾝｰ")
            (ustr-cursor-frontside ustrj-l))
(test-equal 5
            (ustr-length ustrj-l))
(test-equal 0
            (ustr-cursor-pos ustrj-l))
(test-equal '(("んー" "ンー" "ﾝｰ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
              ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
            (ustr-whole-seq ustrj-l))
;; empty
(test-error (ustr-cursor-frontside ustre))
(test-equal 0
            (ustr-length ustre))
(test-false (ustr-cursor-set-frontside! ustre
                                        '("んー" "ンー" "ﾝｰ")))
(test-error (ustr-cursor-frontside ustre))
(test-equal 0
            (ustr-length ustre))
(test-equal '()
            (ustr-whole-seq ustre))
(test-end)

(test-begin "ustr-cursor-set-backside!")
(setup)
;; former-latter
(test-equal '("ん" "ン" "ﾝ")
            (ustr-cursor-backside ustrj-fl))
(test-equal 5
            (ustr-length ustrj-fl))
(test-equal 3
            (ustr-cursor-pos ustrj-fl))
(test-eq #t (ustr-cursor-set-backside! ustrj-fl
                                       '("んー" "ンー" "ﾝｰ")))
(test-equal '("んー" "ンー" "ﾝｰ")
            (ustr-cursor-backside ustrj-fl))
(test-equal 5
            (ustr-length ustrj-fl))
(test-equal 3
            (ustr-cursor-pos ustrj-fl))
(test-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("んー" "ンー" "ﾝｰ")
              ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
            (ustr-whole-seq ustrj-fl))
;; former
(test-equal '("じゃ" "ジャ" "ｼﾞｬ")
            (ustr-cursor-backside ustrj-f))
(test-equal 5
            (ustr-length ustrj-f))
(test-equal 5
            (ustr-cursor-pos ustrj-f))
(test-eq #t (ustr-cursor-set-backside! ustrj-f
                                       '("んー" "ンー" "ﾝｰ")))
(test-equal '("んー" "ンー" "ﾝｰ")
            (ustr-cursor-backside ustrj-f))
(test-equal 5
            (ustr-length ustrj-f))
(test-equal 5
            (ustr-cursor-pos ustrj-f))
(test-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
              ("ご" "ゴ" "ｺﾞ") ("んー" "ンー" "ﾝｰ"))
            (ustr-whole-seq ustrj-f))
;; latter
(test-error (ustr-cursor-backside ustrj-l))
(test-equal 5
            (ustr-length ustrj-l))
(test-equal 0
            (ustr-cursor-pos ustrj-l))
(test-false (ustr-cursor-set-backside! ustrj-l
                                       '("んー" "ンー" "ﾝｰ")))
(test-error (ustr-cursor-backside ustrj-l))
(test-equal 5
            (ustr-length ustrj-l))
(test-equal 0
            (ustr-cursor-pos ustrj-l))
(test-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
              ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
            (ustr-whole-seq ustrj-l))
;; empty
(test-error (ustr-cursor-backside ustre))
(test-equal 0
            (ustr-length ustre))
(test-false (ustr-cursor-set-backside! ustre
                                       '("んー" "ンー" "ﾝｰ")))
(test-error (ustr-cursor-backside ustre))
(test-equal 0
            (ustr-length ustre))
(test-equal '()
            (ustr-whole-seq ustre))
(test-end)

(test-begin "ustr-insert-seq!")
(setup)
;; former-latter
(test-equal '("ん" "ン" "ﾝ")
            (ustr-cursor-backside ustrj-fl))
(test-equal 5
            (ustr-length ustrj-fl))
(test-equal 3
            (ustr-cursor-pos ustrj-fl))
(ustr-insert-seq! ustrj-fl '(("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ")
                             ("よ" "ヨ" "ﾖ")))
(test-equal '("よ" "ヨ" "ﾖ")
            (ustr-cursor-backside ustrj-fl))
(test-equal 8
            (ustr-length ustrj-fl))
(test-equal 6
            (ustr-cursor-pos ustrj-fl))
(test-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
              ("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ") ("よ" "ヨ" "ﾖ")
              ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
            (ustr-whole-seq ustrj-fl))
;; former
(test-equal '("じゃ" "ジャ" "ｼﾞｬ")
            (ustr-cursor-backside ustrj-f))
(test-equal 5
            (ustr-length ustrj-f))
(test-equal 5
            (ustr-cursor-pos ustrj-f))
(ustr-insert-seq! ustrj-f '(("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ")
                            ("よ" "ヨ" "ﾖ")))
(test-equal '("よ" "ヨ" "ﾖ")
            (ustr-cursor-backside ustrj-f))
(test-equal 8
            (ustr-length ustrj-f))
(test-equal 8
            (ustr-cursor-pos ustrj-f))
(test-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
              ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ")
              ("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ") ("よ" "ヨ" "ﾖ"))
            (ustr-whole-seq ustrj-f))
;; latter
(test-error (ustr-cursor-backside ustrj-l))
(test-equal 5
            (ustr-length ustrj-l))
(test-equal 0
            (ustr-cursor-pos ustrj-l))
(ustr-insert-seq! ustrj-l '(("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ")
                            ("よ" "ヨ" "ﾖ")))
(test-equal '("よ" "ヨ" "ﾖ")
            (ustr-cursor-backside ustrj-l))
(test-equal 8
            (ustr-length ustrj-l))
(test-equal 3
            (ustr-cursor-pos ustrj-l))
(test-equal '(("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ") ("よ" "ヨ" "ﾖ")
              ("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
              ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
            (ustr-whole-seq ustrj-l))
;; empty
(test-error (ustr-cursor-backside ustre))
(test-equal 0
            (ustr-length ustre))
(test-equal 0
            (ustr-cursor-pos ustre))
(ustr-insert-seq! ustre '(("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ")
                          ("よ" "ヨ" "ﾖ")))
(test-equal '("よ" "ヨ" "ﾖ")
            (ustr-cursor-backside ustre))
(test-equal 3
            (ustr-length ustre))
(test-equal 3
            (ustr-cursor-pos ustre))
(test-equal '(("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ") ("よ" "ヨ" "ﾖ"))
            (ustr-whole-seq ustre))
(test-end)

(test-begin "ustr-insert-seq!-#2")
(setup)
;; former-latter
(test-equal '("ん" "ン" "ﾝ")
            (ustr-cursor-backside ustrj-fl))
(test-equal 5
            (ustr-length ustrj-fl))
(test-equal 3
            (ustr-cursor-pos ustrj-fl))
(ustr-insert-seq! ustrj-fl '())
(test-equal '("ん" "ン" "ﾝ")
            (ustr-cursor-backside ustrj-fl))
(test-equal 5
            (ustr-length ustrj-fl))
(test-equal 3
            (ustr-cursor-pos ustrj-fl))
(test-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
              ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
            (ustr-whole-seq ustrj-fl))
;; former
(test-equal '("じゃ" "ジャ" "ｼﾞｬ")
            (ustr-cursor-backside ustrj-f))
(test-equal 5
            (ustr-length ustrj-f))
(test-equal 5
            (ustr-cursor-pos ustrj-f))
(ustr-insert-seq! ustrj-f '())
(test-equal '("じゃ" "ジャ" "ｼﾞｬ")
            (ustr-cursor-backside ustrj-f))
(test-equal 5
            (ustr-length ustrj-f))
(test-equal 5
            (ustr-cursor-pos ustrj-f))
(test-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
              ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
            (ustr-whole-seq ustrj-f))
;; latter
(test-error (ustr-cursor-backside ustrj-l))
(test-equal 5
            (ustr-length ustrj-l))
(test-equal 0
            (ustr-cursor-pos ustrj-l))
(ustr-insert-seq! ustrj-l '())
(test-error (ustr-cursor-backside ustrj-l))
(test-equal 5
            (ustr-length ustrj-l))
(test-equal 0
            (ustr-cursor-pos ustrj-l))
(test-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
              ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
            (ustr-whole-seq ustrj-l))
;; empty
(test-error (ustr-cursor-backside ustre))
(test-equal 0
            (ustr-length ustre))
(test-equal 0
            (ustr-cursor-pos ustre))
(ustr-insert-seq! ustre '())
(test-error (ustr-cursor-backside ustre))
(test-equal 0
            (ustr-length ustre))
(test-equal 0
            (ustr-cursor-pos ustre))
(test-end)

(test-report-result)
