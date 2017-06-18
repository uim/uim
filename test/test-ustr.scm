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

;; These tests are passed at revision 6600 (new repository)

(define-module test.test-ustr
  (use test.unit.test-case)
  (use test.uim-test))
(select-module test.test-ustr)

(define (setup)
  (uim-test-setup)
  (uim-eval '(require "ustr.scm"))
  (uim-eval '(define-record 'ja-kana
               '((hiragana "")
                 (katakana "")
                 (hankaku  ""))))
  (uim-eval
   '(begin
      (define ustr-f (ustr-new '(("h" . "H") ("e" . "E") ("l" . "L")
                                 ("l" . "L") ("o" . "O"))))
      (define ustr-fl (ustr-new '(("h" . "H") ("e" . "E") ("l" . "L"))
                                '(("l" . "L") ("o" . "O"))))
      (define ustr-l (ustr-new ()
                               '(("h" . "H") ("e" . "E") ("l" . "L")
                                 ("l" . "L") ("o" . "O"))))
      (define ustra-f (ustr-new '("h" "e" "l" "l" "o")))
      (define ustra-fl (ustr-new '("h" "e" "l")
                                 '("l" "o")))
      (define ustra-l (ustr-new ()
                                '("h" "e" "l" "l" "o")))
      (define ustrj-f (ustr-new '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ")
                                  ("ん" "ン" "ﾝ") ("ご" "ゴ" "ｺﾞ")
                                  ("じゃ" "ジャ" "ｼﾞｬ"))))
      (define ustrj-fl (ustr-new '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ")
                                   ("ん" "ン" "ﾝ"))
                                 '(("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))))
      (define ustrj-l (ustr-new ()
                                '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ")
                                  ("ん" "ン" "ﾝ") ("ご" "ゴ" "ｺﾞ")
                                  ("じゃ" "ジャ" "ｼﾞｬ"))))
      (define ustre (ustr-new ())))))

(define (teardown)
  (uim-test-teardown))

(define (test-ustr-new)
  ;; single sequence goes into former
  (assert-uim-equal '(("o" "l" "l" "e" "h") . ())
                    '(ustr-new '("h" "e" "l" "l" "o")))
  ;; dual sequences are go into former and latter
  (assert-uim-equal '(("l" "e" "h") . ("l" "o"))
                    '(ustr-new '("h" "e" "l")
                               '("l" "o")))
  ;; latter sequence only
  (assert-uim-equal '(() . ("h" "e" "l" "l" "o"))
                    '(ustr-new ()
                               '("h" "e" "l" "l" "o"))))

(define (test-ustr-whole-seq)
  (assert-uim-equal '(("h" . "H") ("e" . "E") ("l" . "L") ("l" . "L") ("o" . "O"))
                    '(ustr-whole-seq ustr-fl))
  (assert-uim-equal '(("h" . "H") ("e" . "E") ("l" . "L") ("l" . "L") ("o" . "O"))
                    '(ustr-whole-seq ustr-f))
  (assert-uim-equal '(("h" . "H") ("e" . "E") ("l" . "L") ("l" . "L") ("o" . "O"))
                    '(ustr-whole-seq ustr-l))
  (assert-uim-equal '("h" "e" "l" "l" "o")
                    '(ustr-whole-seq ustra-fl))
  (assert-uim-equal '("h" "e" "l" "l" "o")
                    '(ustr-whole-seq ustra-f))
  (assert-uim-equal '("h" "e" "l" "l" "o")
                    '(ustr-whole-seq ustra-l))
  (assert-uim-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                      ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
                    '(ustr-whole-seq ustrj-fl))
  (assert-uim-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                      ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
                    '(ustr-whole-seq ustrj-f))
  (assert-uim-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                      ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
                    '(ustr-whole-seq ustrj-l)))

(define (test-ustr-former-seq)
  (assert-uim-equal '(("h" . "H") ("e" . "E") ("l" . "L"))
                    '(ustr-former-seq ustr-fl))
  (assert-uim-equal '(("h" . "H") ("e" . "E") ("l" . "L") ("l" . "L") ("o" . "O"))
                    '(ustr-former-seq ustr-f))
  (assert-uim-equal '()
                    '(ustr-former-seq ustr-l))
  (assert-uim-equal '("h" "e" "l")
                    '(ustr-former-seq ustra-fl))
  (assert-uim-equal '("h" "e" "l" "l" "o")
                    '(ustr-former-seq ustra-f))
  (assert-uim-equal '()
                    '(ustr-former-seq ustra-l))
  (assert-uim-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ"))
                    '(ustr-former-seq ustrj-fl))
  (assert-uim-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                      ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
                    '(ustr-former-seq ustrj-f))
  (assert-uim-equal '()
                    '(ustr-former-seq ustrj-l)))

(define (test-ustr-latter-seq)
  (assert-uim-equal '(("l" . "L") ("o" . "O"))
                    '(ustr-latter-seq ustr-fl))
  (assert-uim-equal '()
                    '(ustr-latter-seq ustr-f))
  (assert-uim-equal '(("h" . "H") ("e" . "E") ("l" . "L") ("l" . "L") ("o" . "O"))
                    '(ustr-latter-seq ustr-l))
  (assert-uim-equal '("l" "o")
                    '(ustr-latter-seq ustra-fl))
  (assert-uim-equal '()
                    '(ustr-latter-seq ustra-f))
  (assert-uim-equal '("h" "e" "l" "l" "o")
                    '(ustr-latter-seq ustra-l))
  (assert-uim-equal '(("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
                    '(ustr-latter-seq ustrj-fl))
  (assert-uim-equal '()
                    '(ustr-latter-seq ustrj-f))
  (assert-uim-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                      ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
                    '(ustr-latter-seq ustrj-l)))

(define (test-ustr-set-whole-seq!)
  ;; former-latter
  (assert-uim-equal '("h" "e" "l" "l" "o")
                    '(ustr-whole-seq ustra-fl))
  (assert-uim-equal '("h" "e" "l")
                    '(ustr-former-seq ustra-fl))
  (assert-uim-equal '("l" "o")
                    '(ustr-latter-seq ustra-fl))
  (uim-eval '(ustr-set-whole-seq! ustra-fl '("w" "o" "r" "L" "d")))
  (assert-uim-equal '("w" "o" "r" "L" "d")
                    '(ustr-whole-seq ustra-fl))
  (assert-uim-equal '("w" "o" "r" "L" "d")
                    '(ustr-former-seq ustra-fl))
  (assert-uim-equal '()
                    '(ustr-latter-seq ustra-fl))
  ;; former
  (assert-uim-equal '("h" "e" "l" "l" "o")
                    '(ustr-whole-seq ustra-f))
  (assert-uim-equal '("h" "e" "l" "l" "o")
                    '(ustr-former-seq ustra-f))
  (assert-uim-equal '()
                    '(ustr-latter-seq ustra-f))
  (uim-eval '(ustr-set-whole-seq! ustra-f '("w" "o" "r" "L" "d")))
  (assert-uim-equal '("w" "o" "r" "L" "d")
                    '(ustr-whole-seq ustra-f))
  (assert-uim-equal '("w" "o" "r" "L" "d")
                    '(ustr-former-seq ustra-f))
  (assert-uim-equal '()
                    '(ustr-latter-seq ustra-f))
  ;; latter
  (assert-uim-equal '("h" "e" "l" "l" "o")
                    '(ustr-whole-seq ustra-l))
  (assert-uim-equal '()
                    '(ustr-former-seq ustra-l))
  (assert-uim-equal '("h" "e" "l" "l" "o")
                    '(ustr-latter-seq ustra-l))
  (uim-eval '(ustr-set-whole-seq! ustra-l '("w" "o" "r" "L" "d")))
  (assert-uim-equal '("w" "o" "r" "L" "d")
                    '(ustr-whole-seq ustra-l))
  (assert-uim-equal '("w" "o" "r" "L" "d")
                    '(ustr-former-seq ustra-l))
  (assert-uim-equal '()
                    '(ustr-latter-seq ustra-l)))

(define (test-ustr-set-former-seq!)
  ;; former-latter
  (assert-uim-equal '("h" "e" "l" "l" "o")
                    '(ustr-whole-seq ustra-fl))
  (assert-uim-equal '("h" "e" "l")
                    '(ustr-former-seq ustra-fl))
  (assert-uim-equal '("l" "o")
                    '(ustr-latter-seq ustra-fl))
  (uim-eval '(ustr-set-former-seq! ustra-fl '("w" "o" "r" "L" "d")))
  (assert-uim-equal '("w" "o" "r" "L" "d" "l" "o")
                    '(ustr-whole-seq ustra-fl))
  (assert-uim-equal '("w" "o" "r" "L" "d")
                    '(ustr-former-seq ustra-fl))
  (assert-uim-equal '("l" "o")
                    '(ustr-latter-seq ustra-fl))
  ;; former
  (assert-uim-equal '("h" "e" "l" "l" "o")
                    '(ustr-whole-seq ustra-f))
  (assert-uim-equal '("h" "e" "l" "l" "o")
                    '(ustr-former-seq ustra-f))
  (assert-uim-equal '()
                    '(ustr-latter-seq ustra-f))
  (uim-eval '(ustr-set-former-seq! ustra-f '("w" "o" "r" "L" "d")))
  (assert-uim-equal '("w" "o" "r" "L" "d")
                    '(ustr-whole-seq ustra-f))
  (assert-uim-equal '("w" "o" "r" "L" "d")
                    '(ustr-former-seq ustra-f))
  (assert-uim-equal '()
                    '(ustr-latter-seq ustra-f))
  ;; latter
  (assert-uim-equal '("h" "e" "l" "l" "o")
                    '(ustr-whole-seq ustra-l))
  (assert-uim-equal '()
                    '(ustr-former-seq ustra-l))
  (assert-uim-equal '("h" "e" "l" "l" "o")
                    '(ustr-latter-seq ustra-l))
  (uim-eval '(ustr-set-former-seq! ustra-l '("w" "o" "r" "L" "d")))
  (assert-uim-equal '("w" "o" "r" "L" "d" "h" "e" "l" "l" "o")
                    '(ustr-whole-seq ustra-l))
  (assert-uim-equal '("w" "o" "r" "L" "d")
                    '(ustr-former-seq ustra-l))
  (assert-uim-equal '("h" "e" "l" "l" "o")
                    '(ustr-latter-seq ustra-l)))

(define (test-ustr-set-latter-seq!)
  ;; former-latter
  (assert-uim-equal '("h" "e" "l" "l" "o")
                    '(ustr-whole-seq ustra-fl))
  (assert-uim-equal '("h" "e" "l")
                    '(ustr-former-seq ustra-fl))
  (assert-uim-equal '("l" "o")
                    '(ustr-latter-seq ustra-fl))
  (uim-eval '(ustr-set-latter-seq! ustra-fl '("w" "o" "r" "L" "d")))
  (assert-uim-equal '("h" "e" "l" "w" "o" "r" "L" "d")
                    '(ustr-whole-seq ustra-fl))
  (assert-uim-equal '("h" "e" "l")
                    '(ustr-former-seq ustra-fl))
  (assert-uim-equal '("w" "o" "r" "L" "d")
                    '(ustr-latter-seq ustra-fl))
  ;; former
  (assert-uim-equal '("h" "e" "l" "l" "o")
                    '(ustr-whole-seq ustra-f))
  (assert-uim-equal '("h" "e" "l" "l" "o")
                    '(ustr-former-seq ustra-f))
  (assert-uim-equal '()
                    '(ustr-latter-seq ustra-f))
  (uim-eval '(ustr-set-latter-seq! ustra-f '("w" "o" "r" "L" "d")))
  (assert-uim-equal '("h" "e" "l" "l" "o" "w" "o" "r" "L" "d")
                    '(ustr-whole-seq ustra-f))
  (assert-uim-equal '("h" "e" "l" "l" "o")
                    '(ustr-former-seq ustra-f))
  (assert-uim-equal '("w" "o" "r" "L" "d")
                    '(ustr-latter-seq ustra-f))
  ;; latter
  (assert-uim-equal '("h" "e" "l" "l" "o")
                    '(ustr-whole-seq ustra-l))
  (assert-uim-equal '()
                    '(ustr-former-seq ustra-l))
  (assert-uim-equal '("h" "e" "l" "l" "o")
                    '(ustr-latter-seq ustra-l))
  (uim-eval '(ustr-set-latter-seq! ustra-l '("w" "o" "r" "L" "d")))
  (assert-uim-equal '("w" "o" "r" "L" "d")
                    '(ustr-whole-seq ustra-l))
  (assert-uim-equal '()
                    '(ustr-former-seq ustra-l))
  (assert-uim-equal '("w" "o" "r" "L" "d")
                    '(ustr-latter-seq ustra-l)))

(define (test-ustr-empty?)
  (assert-false (uim-bool '(ustr-empty? ustr-fl)))
  (assert-false (uim-bool '(ustr-empty? ustr-f)))
  (assert-false (uim-bool '(ustr-empty? ustr-l)))

  (assert-false (uim-bool '(ustr-empty? ustra-fl)))
  (assert-false (uim-bool '(ustr-empty? ustra-f)))
  (assert-false (uim-bool '(ustr-empty? ustra-l)))

  (assert-false (uim-bool '(ustr-empty? ustrj-fl)))
  (assert-false (uim-bool '(ustr-empty? ustrj-f)))
  (assert-false (uim-bool '(ustr-empty? ustrj-l)))

  (assert-uim-equal '(() . ())
                    'ustre)
  (assert-uim-true  '(ustr-empty? ustre)))

(define (test-ustr-clear!)
  (assert-uim-false '(ustr-empty? ustra-fl))
  (uim-eval '(ustr-clear! ustra-fl))
  (assert-uim-true  '(ustr-empty? ustra-fl))

  (assert-uim-false '(ustr-empty? ustra-f))
  (uim-eval '(ustr-clear! ustra-f))
  (assert-uim-true  '(ustr-empty? ustra-f))

  (assert-uim-false '(ustr-empty? ustra-l))
  (uim-eval '(ustr-clear! ustra-l))
  (assert-uim-true  '(ustr-empty? ustra-l))

  (assert-uim-true  '(ustr-empty? ustre))
  (uim-eval '(ustr-clear! ustre))
  (assert-uim-true  '(ustr-empty? ustre)))

(define (test-ustr-clear-former!)
  (assert-uim-false '(ustr-empty? ustra-fl))
  (uim-eval '(ustr-clear-former! ustra-fl))
  (assert-uim-false '(ustr-empty? ustra-fl))

  (assert-uim-false '(ustr-empty? ustra-f))
  (uim-eval '(ustr-clear-former! ustra-f))
  (assert-uim-true  '(ustr-empty? ustra-f))

  (assert-uim-false '(ustr-empty? ustra-l))
  (uim-eval '(ustr-clear-former! ustra-l))
  (assert-uim-false '(ustr-empty? ustra-l))

  (assert-uim-true  '(ustr-empty? ustre))
  (uim-eval '(ustr-clear-former! ustre))
  (assert-uim-true  '(ustr-empty? ustre)))

(define (test-ustr-clear-latter!)
  (assert-uim-false '(ustr-empty? ustra-fl))
  (uim-eval '(ustr-clear-latter! ustra-fl))
  (assert-uim-false '(ustr-empty? ustra-fl))

  (assert-uim-false '(ustr-empty? ustra-f))
  (uim-eval '(ustr-clear-latter! ustra-f))
  (assert-uim-false '(ustr-empty? ustra-f))

  (assert-uim-false '(ustr-empty? ustra-l))
  (uim-eval '(ustr-clear-latter! ustra-l))
  (assert-uim-true  '(ustr-empty? ustra-l))

  (assert-uim-true  '(ustr-empty? ustre))
  (uim-eval '(ustr-clear-latter! ustre))
  (assert-uim-true  '(ustr-empty? ustre)))

(define (test-ustr-copy!)
  (assert-uim-false '(equal? ustr-fl ustra-fl))
  (uim-eval '(ustr-copy! ustr-fl ustra-fl))
  (assert-uim-true '(equal? ustr-fl ustra-fl))

  (assert-uim-false '(equal? ustr-f ustra-f))
  (uim-eval '(ustr-copy! ustr-f ustra-f))
  (assert-uim-true '(equal? ustr-f ustra-f))

  (assert-uim-false '(equal? ustr-l ustra-l))
  (uim-eval '(ustr-copy! ustr-l ustra-l))
  (assert-uim-true '(equal? ustr-l ustra-l)))

(define (test-ustr=)
  (assert-uim-true  '(ustr= equal? ustr-fl ustr-fl))
  (assert-uim-true  '(ustr= equal? ustr-fl ustr-f))
  (assert-uim-true  '(ustr= equal? ustr-f ustr-fl))
  (assert-uim-true  '(ustr= equal? ustr-fl ustr-l))
  (assert-uim-true  '(ustr= equal? ustr-l ustr-fl))
  (assert-uim-true  '(ustr= equal? ustr-f ustr-f))
  (assert-uim-true  '(ustr= equal? ustr-l ustr-f))
  (assert-uim-true  '(ustr= equal? ustr-f ustr-l))
  (assert-uim-true  '(ustr= equal? ustr-l ustr-l))
  (assert-uim-false '(ustr= equal? ustr-l ustre))
  (assert-uim-false '(ustr= equal? ustre ustr-l))

  (assert-uim-true  '(ustr= string=? ustra-fl ustra-fl))
  (assert-uim-true  '(ustr= string=? ustra-fl ustra-f))
  (assert-uim-true  '(ustr= string=? ustra-f ustra-fl))
  (assert-uim-true  '(ustr= string=? ustra-fl ustra-l))
  (assert-uim-true  '(ustr= string=? ustra-l ustra-fl))
  (assert-uim-true  '(ustr= string=? ustra-f ustra-f))
  (assert-uim-true  '(ustr= string=? ustra-l ustra-f))
  (assert-uim-true  '(ustr= string=? ustra-f ustra-l))
  (assert-uim-true  '(ustr= string=? ustra-l ustra-l))
  (assert-uim-false '(ustr= string=? ustra-fl ustre))
  (assert-uim-false '(ustr= string=? ustre ustra-fl))
  (assert-uim-false '(ustr= string=? ustra-f ustre))
  (assert-uim-false '(ustr= string=? ustre ustra-f))
  (assert-uim-false '(ustr= string=? ustra-l ustre))
  (assert-uim-false '(ustr= string=? ustre ustra-l))
  (uim-eval '(ustr-set-former-seq! ustra-f '("h" "e" "l" "l" "o" "!")))
  (assert-uim-false '(ustr= string=? ustra-fl ustra-f))
  (assert-uim-false '(ustr= string=? ustra-f ustra-fl))
  (assert-uim-false '(ustr= string=? ustra-l ustra-f))
  (assert-uim-false '(ustr= string=? ustra-f ustra-l))
  (assert-uim-true  '(ustr= string=? ustra-f ustra-f))

  (assert-uim-true  '(ustr= equal? ustrj-fl ustrj-fl))
  (assert-uim-true  '(ustr= equal? ustrj-fl ustrj-f))
  (assert-uim-true  '(ustr= equal? ustrj-f ustrj-fl))
  (assert-uim-true  '(ustr= equal? ustrj-fl ustrj-l))
  (assert-uim-true  '(ustr= equal? ustrj-l ustrj-fl))
  (assert-uim-true  '(ustr= equal? ustrj-f ustrj-f))
  (assert-uim-true  '(ustr= equal? ustrj-l ustrj-f))
  (assert-uim-true  '(ustr= equal? ustrj-f ustrj-l))
  (assert-uim-true  '(ustr= equal? ustrj-l ustrj-l))
  (assert-uim-false '(ustr= equal? ustrj-l ustre))
  (assert-uim-false '(ustr= equal? ustre ustrj-l)))

(define (test-ustr-length)
  (assert-uim-equal 5
                    '(ustr-length ustr-fl))
  (assert-uim-equal 5
                    '(ustr-length ustr-f))
  (assert-uim-equal 5
                    '(ustr-length ustr-l))
  (assert-uim-equal 5
                    '(ustr-length ustra-fl))
  (assert-uim-equal 5
                    '(ustr-length ustra-f))
  (assert-uim-equal 5
                    '(ustr-length ustra-l))
  (assert-uim-equal 5
                    '(ustr-length ustrj-fl))
  (assert-uim-equal 5
                    '(ustr-length ustrj-f))
  (assert-uim-equal 5
                    '(ustr-length ustrj-l))
  (assert-uim-equal 0
                    '(ustr-length ustre)))

(define (test-ustr-nth)
  (assert-uim-equal "h"
                    '(ustr-nth ustra-fl 0))
  (assert-uim-equal "e"
                    '(ustr-nth ustra-fl 1))
  (assert-uim-equal "l"
                    '(ustr-nth ustra-fl 2))
  (assert-uim-equal "l"
                    '(ustr-nth ustra-fl 3))
  (assert-uim-equal "o"
                    '(ustr-nth ustra-fl 4))
  (assert-uim-error '(ustr-nth ustra-fl 5))
  (assert-uim-error '(ustr-nth ustra-fl -1))

  (assert-uim-equal "h"
                    '(ustr-nth ustra-f 0))
  (assert-uim-equal "e"
                    '(ustr-nth ustra-f 1))
  (assert-uim-equal "l"
                    '(ustr-nth ustra-f 2))
  (assert-uim-equal "l"
                    '(ustr-nth ustra-f 3))
  (assert-uim-equal "o"
                    '(ustr-nth ustra-f 4))
  (assert-uim-error '(ustr-nth ustra-f 5))
  (assert-uim-error '(ustr-nth ustra-f -1))

  (assert-uim-equal "h"
                    '(ustr-nth ustra-l 0))
  (assert-uim-equal "e"
                    '(ustr-nth ustra-l 1))
  (assert-uim-equal "l"
                    '(ustr-nth ustra-l 2))
  (assert-uim-equal "l"
                    '(ustr-nth ustra-l 3))
  (assert-uim-equal "o"
                    '(ustr-nth ustra-l 4))
  (assert-uim-error '(ustr-nth ustra-l 5))
  (assert-uim-error '(ustr-nth ustra-l -1))

  (assert-uim-error '(ustr-nth ustre 0)))

(define (test-ustr-set-nth!)
  (assert-uim-equal "h"
                    '(ustr-nth ustra-fl 0))
  (assert-uim-equal "e"
                    '(ustr-nth ustra-fl 1))
  (assert-uim-equal "l"
                    '(ustr-nth ustra-fl 2))
  (assert-uim-equal "l"
                    '(ustr-nth ustra-fl 3))
  (assert-uim-equal "o"
                    '(ustr-nth ustra-fl 4))
  (assert-uim-error '(ustr-nth ustra-fl 5))
  (assert-uim-error '(ustr-nth ustra-fl -1))
  ;; position 0
  (assert-uim-equal 5
                    '(ustr-length ustra-fl))
  (assert-uim-equal 3
                    '(ustr-cursor-pos ustra-fl))
  (uim-eval '(ustr-set-nth! ustra-fl 0 "H"))
  (assert-uim-equal "H"
                    '(ustr-nth ustra-fl 0))
  (assert-uim-equal '("H" "e" "l")
                    '(ustr-former-seq ustra-fl))
  (assert-uim-equal '("l" "o")
                    '(ustr-latter-seq ustra-fl))
  (assert-uim-equal 5
                    '(ustr-length ustra-fl))
  (assert-uim-equal 3
                    '(ustr-cursor-pos ustra-fl))
  ;; position 1
  (assert-uim-equal 5
                    '(ustr-length ustra-fl))
  (assert-uim-equal 3
                    '(ustr-cursor-pos ustra-fl))
  (uim-eval '(ustr-set-nth! ustra-fl 1 "E"))
  (assert-uim-equal "E"
                    '(ustr-nth ustra-fl 1))
  (assert-uim-equal '("H" "E" "l")
                    '(ustr-former-seq ustra-fl))
  (assert-uim-equal '("l" "o")
                    '(ustr-latter-seq ustra-fl))
  (assert-uim-equal 5
                    '(ustr-length ustra-fl))
  (assert-uim-equal 3
                    '(ustr-cursor-pos ustra-fl))
  ;; position 2
  (assert-uim-equal 5
                    '(ustr-length ustra-fl))
  (assert-uim-equal 3
                    '(ustr-cursor-pos ustra-fl))
  (uim-eval '(ustr-set-nth! ustra-fl 2 "L"))
  (assert-uim-equal "L"
                    '(ustr-nth ustra-fl 2))
  (assert-uim-equal '("H" "E" "L")
                    '(ustr-former-seq ustra-fl))
  (assert-uim-equal '("l" "o")
                    '(ustr-latter-seq ustra-fl))
  (assert-uim-equal 5
                    '(ustr-length ustra-fl))
  (assert-uim-equal 3
                    '(ustr-cursor-pos ustra-fl))
  ;; position 3
  (assert-uim-equal 5
                    '(ustr-length ustra-fl))
  (assert-uim-equal 3
                    '(ustr-cursor-pos ustra-fl))
  (uim-eval '(ustr-set-nth! ustra-fl 3 "|"))
  (assert-uim-equal "|"
                    '(ustr-nth ustra-fl 3))
  (assert-uim-equal '("H" "E" "L")
                    '(ustr-former-seq ustra-fl))
  (assert-uim-equal '("|" "o")
                    '(ustr-latter-seq ustra-fl))
  (assert-uim-equal 5
                    '(ustr-length ustra-fl))
  (assert-uim-equal 3
                    '(ustr-cursor-pos ustra-fl))
  ;; position 4
  (assert-uim-equal 5
                    '(ustr-length ustra-fl))
  (assert-uim-equal 3
                    '(ustr-cursor-pos ustra-fl))
  (uim-eval '(ustr-set-nth! ustra-fl 4 "O"))
  (assert-uim-equal "O"
                    '(ustr-nth ustra-fl 4))
  (assert-uim-equal '("H" "E" "L")
                    '(ustr-former-seq ustra-fl))
  (assert-uim-equal '("|" "O")
                    '(ustr-latter-seq ustra-fl))
  (assert-uim-equal 5
                    '(ustr-length ustra-fl))
  (assert-uim-equal 3
                    '(ustr-cursor-pos ustra-fl))
  ;; position 5
  (assert-uim-equal 5
                    '(ustr-length ustra-fl))
  (assert-uim-equal 3
                    '(ustr-cursor-pos ustra-fl))
  (assert-uim-error '(ustr-set-nth! ustra-fl 5 "5"))
  (assert-uim-error '(ustr-nth ustra-fl 5))
  (assert-uim-equal '("H" "E" "L")
                    '(ustr-former-seq ustra-fl))
  (assert-uim-equal '("|" "O")
                    '(ustr-latter-seq ustra-fl))
  (assert-uim-equal 5
                    '(ustr-length ustra-fl))
  (assert-uim-equal 3
                    '(ustr-cursor-pos ustra-fl))
  ;; position 5 in former-str
  (assert-uim-equal '("h" "e" "l" "l" "o")
                    '(ustr-former-seq ustra-f))
  (assert-uim-equal '()
                    '(ustr-latter-seq ustra-f))
  (assert-uim-equal 5
                    '(ustr-length ustra-f))
  (assert-uim-equal 5
                    '(ustr-cursor-pos ustra-f))
  (assert-uim-error '(ustr-set-nth! ustra-f 5 "5"))
  (assert-uim-error '(ustr-nth ustra-f 5))
  (assert-uim-equal '("h" "e" "l" "l" "o")
                    '(ustr-former-seq ustra-f))
  (assert-uim-equal '()
                    '(ustr-latter-seq ustra-f))
  (assert-uim-equal 5
                    '(ustr-length ustra-f))
  (assert-uim-equal 5
                    '(ustr-cursor-pos ustra-f))
  ;; position 4 in former-str
  (assert-uim-equal '("h" "e" "l" "l" "o")
                    '(ustr-former-seq ustra-f))
  (assert-uim-equal '()
                    '(ustr-latter-seq ustra-f))
  (assert-uim-equal 5
                    '(ustr-length ustra-f))
  (assert-uim-equal 5
                    '(ustr-cursor-pos ustra-f))
  (uim-eval '(ustr-set-nth! ustra-f 4 "O"))
  (assert-uim-equal "O"
                    '(ustr-nth ustra-f 4))
  (assert-uim-equal '("h" "e" "l" "l" "O")
                    '(ustr-former-seq ustra-f))
  (assert-uim-equal '()
                    '(ustr-latter-seq ustra-f))
  (assert-uim-equal 5
                    '(ustr-length ustra-f))
  (assert-uim-equal 5
                    '(ustr-cursor-pos ustra-f))
  ;; position 0 in latter-str
  (assert-uim-equal '()
                    '(ustr-former-seq ustra-l))
  (assert-uim-equal '("h" "e" "l" "l" "o")
                    '(ustr-latter-seq ustra-l))
  (assert-uim-equal 5
                    '(ustr-length ustra-l))
  (assert-uim-equal 0
                    '(ustr-cursor-pos ustra-l))
  (uim-eval '(ustr-set-nth! ustra-l 0 "H"))
  (assert-uim-equal "H"
                    '(ustr-nth ustra-l 0))
  (assert-uim-equal '()
                    '(ustr-former-seq ustra-l))
  (assert-uim-equal '("H" "e" "l" "l" "o")
                    '(ustr-latter-seq ustra-l))
  (assert-uim-equal 5
                    '(ustr-length ustra-l))
  (assert-uim-equal 0
                    '(ustr-cursor-pos ustra-l)))

(define (test-ustr-ref)
  (assert-true  (uim-bool '(eq? (ustr-ref ustra-fl 0)
                                (nthcdr 2 (ustr-former ustra-fl)))))
  (assert-true  (uim-bool '(eq? (ustr-ref ustra-fl 1)
                                (nthcdr 1 (ustr-former ustra-fl)))))
  (assert-true  (uim-bool '(eq? (ustr-ref ustra-fl 2)
                                (nthcdr 0 (ustr-former ustra-fl)))))
  (assert-true  (uim-bool '(eq? (ustr-ref ustra-fl 3)
                                (nthcdr 0 (ustr-latter ustra-fl)))))
  (assert-true  (uim-bool '(eq? (ustr-ref ustra-fl 4)
                                (nthcdr 1 (ustr-latter ustra-fl)))))
  (assert-uim-error '(ustr-ref ustra-fl 5))
  (assert-uim-error '(ustr-ref ustra-fl -1))
  ;; former-str
  (assert-true  (uim-bool '(eq? (ustr-ref ustra-f 4)
                                (nthcdr 0 (ustr-former ustra-f)))))
  (assert-uim-error '(ustr-ref ustra-f 5))
  ;; latter-str
  (assert-true  (uim-bool '(eq? (ustr-ref ustra-l 0)
                                (nthcdr 0 (ustr-latter ustra-l))))))

(define (test-ustr-append!)
  ;; former-latter
  (assert-uim-equal '("ん" "ン" "ﾝ")
                    '(ustr-cursor-backside ustrj-fl))
  (assert-uim-equal 5
                    '(ustr-length ustrj-fl))
  (assert-uim-equal 3
                    '(ustr-cursor-pos ustrj-fl))
  (uim-eval '(ustr-append! ustrj-fl '(("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ")
                                      ("よ" "ヨ" "ﾖ"))))
  (assert-uim-equal '("ん" "ン" "ﾝ")
                    '(ustr-cursor-backside ustrj-fl))
  (assert-uim-equal 8
                    '(ustr-length ustrj-fl))
  (assert-uim-equal 3
                    '(ustr-cursor-pos ustrj-fl))
  (assert-uim-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                      ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ")
                      ("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ") ("よ" "ヨ" "ﾖ"))
                    '(ustr-whole-seq ustrj-fl))
  ;; former
  (assert-uim-equal '("じゃ" "ジャ" "ｼﾞｬ")
                    '(ustr-cursor-backside ustrj-f))
  (assert-uim-equal 5
                    '(ustr-length ustrj-f))
  (assert-uim-equal 5
                    '(ustr-cursor-pos ustrj-f))
  (uim-eval '(ustr-append! ustrj-f '(("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ")
                                     ("よ" "ヨ" "ﾖ"))))
  (assert-uim-equal '("じゃ" "ジャ" "ｼﾞｬ")
                    '(ustr-cursor-backside ustrj-f))
  (assert-uim-equal 8
                    '(ustr-length ustrj-f))
  (assert-uim-equal 5
                    '(ustr-cursor-pos ustrj-f))
  (assert-uim-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                      ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ")
                      ("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ") ("よ" "ヨ" "ﾖ"))
                    '(ustr-whole-seq ustrj-f))
  ;; latter
  (assert-uim-error '(ustr-cursor-backside ustrj-l))
  (assert-uim-equal 5
                    '(ustr-length ustrj-l))
  (assert-uim-equal 0
                    '(ustr-cursor-pos ustrj-l))
  (uim-eval '(ustr-append! ustrj-l '(("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ")
                                     ("よ" "ヨ" "ﾖ"))))
  (assert-uim-error '(ustr-cursor-backside ustrj-l))
  (assert-uim-equal 8
                    '(ustr-length ustrj-l))
  (assert-uim-equal 0
                    '(ustr-cursor-pos ustrj-l))
  (assert-uim-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                      ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ")
                      ("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ") ("よ" "ヨ" "ﾖ"))
                    '(ustr-whole-seq ustrj-l))
  ;; empty
  (assert-uim-error '(ustr-cursor-backside ustre))
  (assert-uim-equal 0
                    '(ustr-length ustre))
  (assert-uim-equal 0
                    '(ustr-cursor-pos ustre))
  (uim-eval '(ustr-append! ustre '(("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ")
                                   ("よ" "ヨ" "ﾖ"))))
  (assert-uim-error '(ustr-cursor-backside ustre))
  (assert-uim-equal 3
                    '(ustr-length ustre))
  (assert-uim-equal 0
                    '(ustr-cursor-pos ustre))
  (assert-uim-equal '(("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ") ("よ" "ヨ" "ﾖ"))
                    '(ustr-whole-seq ustre)))

(define (test-ustr-append!-#2)
  ;; former-latter
  (assert-uim-equal '("ん" "ン" "ﾝ")
                    '(ustr-cursor-backside ustrj-fl))
  (assert-uim-equal 5
                    '(ustr-length ustrj-fl))
  (assert-uim-equal 3
                    '(ustr-cursor-pos ustrj-fl))
  (uim-eval '(ustr-append! ustrj-fl ()))
  (assert-uim-equal '("ん" "ン" "ﾝ")
                    '(ustr-cursor-backside ustrj-fl))
  (assert-uim-equal 5
                    '(ustr-length ustrj-fl))
  (assert-uim-equal 3
                    '(ustr-cursor-pos ustrj-fl))
  (assert-uim-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                      ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
                    '(ustr-whole-seq ustrj-fl))
  ;; former
  (assert-uim-equal '("じゃ" "ジャ" "ｼﾞｬ")
                    '(ustr-cursor-backside ustrj-f))
  (assert-uim-equal 5
                    '(ustr-length ustrj-f))
  (assert-uim-equal 5
                    '(ustr-cursor-pos ustrj-f))
  (uim-eval '(ustr-append! ustrj-f ()))
  (assert-uim-equal '("じゃ" "ジャ" "ｼﾞｬ")
                    '(ustr-cursor-backside ustrj-f))
  (assert-uim-equal 5
                    '(ustr-length ustrj-f))
  (assert-uim-equal 5
                    '(ustr-cursor-pos ustrj-f))
  (assert-uim-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                      ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
                    '(ustr-whole-seq ustrj-f))
  ;; latter
  (assert-uim-error '(ustr-cursor-backside ustrj-l))
  (assert-uim-equal 5
                    '(ustr-length ustrj-l))
  (assert-uim-equal 0
                    '(ustr-cursor-pos ustrj-l))
  (uim-eval '(ustr-append! ustrj-l ()))
  (assert-uim-error '(ustr-cursor-backside ustrj-l))
  (assert-uim-equal 5
                    '(ustr-length ustrj-l))
  (assert-uim-equal 0
                    '(ustr-cursor-pos ustrj-l))
  (assert-uim-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                      ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
                    '(ustr-whole-seq ustrj-l))
  ;; empty
  (assert-uim-error '(ustr-cursor-backside ustre))
  (assert-uim-equal 0
                    '(ustr-length ustre))
  (assert-uim-equal 0
                    '(ustr-cursor-pos ustre))
  (uim-eval '(ustr-append! ustre ()))
  (assert-uim-error '(ustr-cursor-backside ustre))
  (assert-uim-equal 0
                    '(ustr-length ustre))
  (assert-uim-equal 0
                    '(ustr-cursor-pos ustre)))

(define (test-ustr-prepend!)
  ;; former-latter
  (assert-uim-equal '("ん" "ン" "ﾝ")
                    '(ustr-cursor-backside ustrj-fl))
  (assert-uim-equal 5
                    '(ustr-length ustrj-fl))
  (assert-uim-equal 3
                    '(ustr-cursor-pos ustrj-fl))
  (uim-eval '(ustr-prepend! ustrj-fl '(("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ")
                                       ("よ" "ヨ" "ﾖ"))))
  (assert-uim-equal '("ん" "ン" "ﾝ")
                    '(ustr-cursor-backside ustrj-fl))
  (assert-uim-equal 8
                    '(ustr-length ustrj-fl))
  (assert-uim-equal 6
                    '(ustr-cursor-pos ustrj-fl))
  (assert-uim-equal '(("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ") ("よ" "ヨ" "ﾖ")
                      ("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                      ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
                    '(ustr-whole-seq ustrj-fl))
  ;; former
  (assert-uim-equal '("じゃ" "ジャ" "ｼﾞｬ")
                    '(ustr-cursor-backside ustrj-f))
  (assert-uim-equal 5
                    '(ustr-length ustrj-f))
  (assert-uim-equal 5
                    '(ustr-cursor-pos ustrj-f))
  (uim-eval '(ustr-prepend! ustrj-f '(("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ")
                                      ("よ" "ヨ" "ﾖ"))))
  (assert-uim-equal '("じゃ" "ジャ" "ｼﾞｬ")
                    '(ustr-cursor-backside ustrj-f))
  (assert-uim-equal 8
                    '(ustr-length ustrj-f))
  (assert-uim-equal 8
                    '(ustr-cursor-pos ustrj-f))
  (assert-uim-equal '(("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ") ("よ" "ヨ" "ﾖ")
                      ("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                      ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
                    '(ustr-whole-seq ustrj-f))
  ;; latter
  (assert-uim-error '(ustr-cursor-backside ustrj-l))
  (assert-uim-equal 5
                    '(ustr-length ustrj-l))
  (assert-uim-equal 0
                    '(ustr-cursor-pos ustrj-l))
  (uim-eval '(ustr-prepend! ustrj-l '(("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ")
                                      ("よ" "ヨ" "ﾖ"))))
  (assert-uim-equal '("よ" "ヨ" "ﾖ")
                    '(ustr-cursor-backside ustrj-l))
  (assert-uim-equal 8
                    '(ustr-length ustrj-l))
  (assert-uim-equal 3
                    '(ustr-cursor-pos ustrj-l))
  (assert-uim-equal '(("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ") ("よ" "ヨ" "ﾖ")
                      ("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                      ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
                    '(ustr-whole-seq ustrj-l))
  ;; empty
  (assert-uim-error '(ustr-cursor-backside ustre))
  (assert-uim-equal 0
                    '(ustr-length ustre))
  (assert-uim-equal 0
                    '(ustr-cursor-pos ustre))
  (uim-eval '(ustr-prepend! ustre '(("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ")
                                    ("よ" "ヨ" "ﾖ"))))
  (assert-uim-equal '("よ" "ヨ" "ﾖ")
                    '(ustr-cursor-backside ustre))
  (assert-uim-equal 3
                    '(ustr-length ustre))
  (assert-uim-equal 3
                    '(ustr-cursor-pos ustre))
  (assert-uim-equal '(("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ") ("よ" "ヨ" "ﾖ"))
                    '(ustr-whole-seq ustre)))

(define (test-ustr-prepend!-#2)
  ;; former-latter
  (assert-uim-equal '("ん" "ン" "ﾝ")
                    '(ustr-cursor-backside ustrj-fl))
  (assert-uim-equal 5
                    '(ustr-length ustrj-fl))
  (assert-uim-equal 3
                    '(ustr-cursor-pos ustrj-fl))
  (uim-eval '(ustr-prepend! ustrj-fl ()))
  (assert-uim-equal '("ん" "ン" "ﾝ")
                    '(ustr-cursor-backside ustrj-fl))
  (assert-uim-equal 5
                    '(ustr-length ustrj-fl))
  (assert-uim-equal 3
                    '(ustr-cursor-pos ustrj-fl))
  (assert-uim-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                      ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
                    '(ustr-whole-seq ustrj-fl))
  ;; former
  (assert-uim-equal '("じゃ" "ジャ" "ｼﾞｬ")
                    '(ustr-cursor-backside ustrj-f))
  (assert-uim-equal 5
                    '(ustr-length ustrj-f))
  (assert-uim-equal 5
                    '(ustr-cursor-pos ustrj-f))
  (uim-eval '(ustr-prepend! ustrj-f ()))
  (assert-uim-equal '("じゃ" "ジャ" "ｼﾞｬ")
                    '(ustr-cursor-backside ustrj-f))
  (assert-uim-equal 5
                    '(ustr-length ustrj-f))
  (assert-uim-equal 5
                    '(ustr-cursor-pos ustrj-f))
  (assert-uim-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                      ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
                    '(ustr-whole-seq ustrj-f))
  ;; latter
  (assert-uim-error '(ustr-cursor-backside ustrj-l))
  (assert-uim-equal 5
                    '(ustr-length ustrj-l))
  (assert-uim-equal 0
                    '(ustr-cursor-pos ustrj-l))
  (uim-eval '(ustr-prepend! ustrj-l ()))
  (assert-uim-error '(ustr-cursor-backside ustrj-l))
  (assert-uim-equal 5
                    '(ustr-length ustrj-l))
  (assert-uim-equal 0
                    '(ustr-cursor-pos ustrj-l))
  (assert-uim-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                      ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
                    '(ustr-whole-seq ustrj-l))
  ;; empty
  (assert-uim-error '(ustr-cursor-backside ustre))
  (assert-uim-equal 0
                    '(ustr-length ustre))
  (assert-uim-equal 0
                    '(ustr-cursor-pos ustre))
  (uim-eval '(ustr-prepend! ustre ()))
  (assert-uim-error '(ustr-cursor-backside ustre))
  (assert-uim-equal 0
                    '(ustr-length ustre))
  (assert-uim-equal 0
                    '(ustr-cursor-pos ustre)))

(define (test-map-ustr-whole)
  ;; former-latter
  (assert-uim-equal '("h" "e" "l" "l" "o")
                    '(map-ustr-whole car ustr-fl))
  (assert-uim-equal '("H" "E" "L" "L" "O")
                    '(map-ustr-whole cdr ustr-fl))
  (assert-uim-equal '("に" "ほ" "ん" "ご" "じゃ")
                    '(map-ustr-whole ja-kana-hiragana ustrj-fl))
  (assert-uim-equal '("ニ" "ホ" "ン" "ゴ" "ジャ")
                    '(map-ustr-whole ja-kana-katakana ustrj-fl))
  (assert-uim-equal '("ﾆ" "ﾎ" "ﾝ" "ｺﾞ" "ｼﾞｬ")
                    '(map-ustr-whole ja-kana-hankaku ustrj-fl))
  ;; former
  (assert-uim-equal '("h" "e" "l" "l" "o")
                    '(map-ustr-whole car ustr-f))
  (assert-uim-equal '("H" "E" "L" "L" "O")
                    '(map-ustr-whole cdr ustr-f))
  (assert-uim-equal '("に" "ほ" "ん" "ご" "じゃ")
                    '(map-ustr-whole ja-kana-hiragana ustrj-f))
  (assert-uim-equal '("ニ" "ホ" "ン" "ゴ" "ジャ")
                    '(map-ustr-whole ja-kana-katakana ustrj-f))
  (assert-uim-equal '("ﾆ" "ﾎ" "ﾝ" "ｺﾞ" "ｼﾞｬ")
                    '(map-ustr-whole ja-kana-hankaku ustrj-f))
  ;; latter
  (assert-uim-equal '("h" "e" "l" "l" "o")
                    '(map-ustr-whole car ustr-l))
  (assert-uim-equal '("H" "E" "L" "L" "O")
                    '(map-ustr-whole cdr ustr-l))
  (assert-uim-equal '("に" "ほ" "ん" "ご" "じゃ")
                    '(map-ustr-whole ja-kana-hiragana ustrj-l))
  (assert-uim-equal '("ニ" "ホ" "ン" "ゴ" "ジャ")
                    '(map-ustr-whole ja-kana-katakana ustrj-l))
  (assert-uim-equal '("ﾆ" "ﾎ" "ﾝ" "ｺﾞ" "ｼﾞｬ")
                    '(map-ustr-whole ja-kana-hankaku ustrj-l))
  ;; empty
  (assert-uim-equal '()
                    '(map-ustr-whole cdr ustre)))

(define (test-map-ustr-former)
  ;; former-latter
  (assert-uim-equal '("h" "e" "l")
                    '(map-ustr-former car ustr-fl))
  (assert-uim-equal '("H" "E" "L")
                    '(map-ustr-former cdr ustr-fl))
  (assert-uim-equal '("に" "ほ" "ん")
                    '(map-ustr-former ja-kana-hiragana ustrj-fl))
  (assert-uim-equal '("ニ" "ホ" "ン")
                    '(map-ustr-former ja-kana-katakana ustrj-fl))
  (assert-uim-equal '("ﾆ" "ﾎ" "ﾝ")
                    '(map-ustr-former ja-kana-hankaku ustrj-fl))
  ;; former
  (assert-uim-equal '("h" "e" "l" "l" "o")
                    '(map-ustr-former car ustr-f))
  (assert-uim-equal '("H" "E" "L" "L" "O")
                    '(map-ustr-former cdr ustr-f))
  (assert-uim-equal '("に" "ほ" "ん" "ご" "じゃ")
                    '(map-ustr-former ja-kana-hiragana ustrj-f))
  (assert-uim-equal '("ニ" "ホ" "ン" "ゴ" "ジャ")
                    '(map-ustr-former ja-kana-katakana ustrj-f))
  (assert-uim-equal '("ﾆ" "ﾎ" "ﾝ" "ｺﾞ" "ｼﾞｬ")
                    '(map-ustr-former ja-kana-hankaku ustrj-f))
  ;; latter
  (assert-uim-equal '()
                    '(map-ustr-former car ustr-l))
  (assert-uim-equal '()
                    '(map-ustr-former cdr ustr-l))
  (assert-uim-equal '()
                    '(map-ustr-former ja-kana-hiragana ustrj-l))
  (assert-uim-equal '()
                    '(map-ustr-former ja-kana-katakana ustrj-l))
  (assert-uim-equal '()
                    '(map-ustr-former ja-kana-hankaku ustrj-l))
  ;; empty
  (assert-uim-equal '()
                    '(map-ustr-former cdr ustre)))

(define (test-map-ustr-latter)
  ;; former-latter
  (assert-uim-equal '("l" "o")
                    '(map-ustr-latter car ustr-fl))
  (assert-uim-equal '("L" "O")
                    '(map-ustr-latter cdr ustr-fl))
  (assert-uim-equal '("ご" "じゃ")
                    '(map-ustr-latter ja-kana-hiragana ustrj-fl))
  (assert-uim-equal '("ゴ" "ジャ")
                    '(map-ustr-latter ja-kana-katakana ustrj-fl))
  (assert-uim-equal '("ｺﾞ" "ｼﾞｬ")
                    '(map-ustr-latter ja-kana-hankaku ustrj-fl))
  ;; former
  (assert-uim-equal '()
                    '(map-ustr-latter car ustr-f))
  (assert-uim-equal '()
                    '(map-ustr-latter cdr ustr-f))
  (assert-uim-equal '()
                    '(map-ustr-latter ja-kana-hiragana ustrj-f))
  (assert-uim-equal '()
                    '(map-ustr-latter ja-kana-katakana ustrj-f))
  (assert-uim-equal '()
                    '(map-ustr-latter ja-kana-hankaku ustrj-f))
  ;; latter
  (assert-uim-equal '("h" "e" "l" "l" "o")
                    '(map-ustr-latter car ustr-l))
  (assert-uim-equal '("H" "E" "L" "L" "O")
                    '(map-ustr-latter cdr ustr-l))
  (assert-uim-equal '("に" "ほ" "ん" "ご" "じゃ")
                    '(map-ustr-latter ja-kana-hiragana ustrj-l))
  (assert-uim-equal '("ニ" "ホ" "ン" "ゴ" "ジャ")
                    '(map-ustr-latter ja-kana-katakana ustrj-l))
  (assert-uim-equal '("ﾆ" "ﾎ" "ﾝ" "ｺﾞ" "ｼﾞｬ")
                    '(map-ustr-latter ja-kana-hankaku ustrj-l))
  ;; empty
  (assert-uim-equal '()
                    '(map-ustr-latter cdr ustre)))

(define (test-append-map-ustr-whole)
  (assert-uim-equal '("ニ" "ﾆ" "ホ" "ﾎ" "ン" "ﾝ" "ゴ" "ｺﾞ" "ジャ" "ｼﾞｬ")
                    '(append-map-ustr-whole cdr ustrj-fl))
  (assert-uim-equal '("ニ" "ﾆ" "ホ" "ﾎ" "ン" "ﾝ" "ゴ" "ｺﾞ" "ジャ" "ｼﾞｬ")
                    '(append-map-ustr-whole cdr ustrj-f))
  (assert-uim-equal '("ニ" "ﾆ" "ホ" "ﾎ" "ン" "ﾝ" "ゴ" "ｺﾞ" "ジャ" "ｼﾞｬ")
                    '(append-map-ustr-whole cdr ustrj-l))
  (assert-uim-equal '()
                    '(append-map-ustr-whole cdr ustre)))

(define (test-append-map-ustr-former)
  (assert-uim-equal '("ニ" "ﾆ" "ホ" "ﾎ" "ン" "ﾝ")
                    '(append-map-ustr-former cdr ustrj-fl))
  (assert-uim-equal '("ニ" "ﾆ" "ホ" "ﾎ" "ン" "ﾝ" "ゴ" "ｺﾞ" "ジャ" "ｼﾞｬ")
                    '(append-map-ustr-former cdr ustrj-f))
  (assert-uim-equal '()
                    '(append-map-ustr-former cdr ustrj-l))
  (assert-uim-equal '()
                    '(append-map-ustr-former cdr ustre)))

(define (test-append-map-ustr-latter)
  (assert-uim-equal '("ゴ" "ｺﾞ" "ジャ" "ｼﾞｬ")
                    '(append-map-ustr-latter cdr ustrj-fl))
  (assert-uim-equal '()
                    '(append-map-ustr-latter cdr ustrj-f))
  (assert-uim-equal '("ニ" "ﾆ" "ホ" "ﾎ" "ン" "ﾝ" "ゴ" "ｺﾞ" "ジャ" "ｼﾞｬ")
                    '(append-map-ustr-latter cdr ustrj-l))
  (assert-uim-equal '()
                    '(append-map-ustr-latter cdr ustre)))

(define (test-string-append-map-ustr-whole)
  ;; former-latter
  (assert-uim-equal "hello"
                    '(string-append-map-ustr-whole car ustr-fl))
  (assert-uim-equal "HELLO"
                    '(string-append-map-ustr-whole cdr ustr-fl))
  (assert-uim-equal "にほんごじゃ"
                    (uim '(string-append-map-ustr-whole ja-kana-hiragana
                                                        ustrj-fl)))
  (assert-uim-equal "ニホンゴジャ"
                    (uim '(string-append-map-ustr-whole ja-kana-katakana
                                                        ustrj-fl)))
  (assert-uim-equal "ﾆﾎﾝｺﾞｼﾞｬ"
                    (uim '(string-append-map-ustr-whole ja-kana-hankaku
                                                        ustrj-fl)))
  ;; former
  (assert-uim-equal "hello"
                    '(string-append-map-ustr-whole car ustr-f))
  (assert-uim-equal "HELLO"
                    '(string-append-map-ustr-whole cdr ustr-f))
  (assert-uim-equal "にほんごじゃ"
                    (uim '(string-append-map-ustr-whole ja-kana-hiragana
                                                        ustrj-f)))
  (assert-uim-equal "ニホンゴジャ"
                    (uim '(string-append-map-ustr-whole ja-kana-katakana
                                                        ustrj-f)))
  (assert-uim-equal "ﾆﾎﾝｺﾞｼﾞｬ"
                    (uim '(string-append-map-ustr-whole ja-kana-hankaku
                                                        ustrj-f)))
  ;; latter
  (assert-uim-equal "hello"
                    '(string-append-map-ustr-whole car ustr-l))
  (assert-uim-equal "HELLO"
                    '(string-append-map-ustr-whole cdr ustr-l))
  (assert-uim-equal "にほんごじゃ"
                    (uim '(string-append-map-ustr-whole ja-kana-hiragana
                                                        ustrj-l)))
  (assert-uim-equal "ニホンゴジャ"
                    (uim '(string-append-map-ustr-whole ja-kana-katakana
                                                        ustrj-l)))
  (assert-uim-equal "ﾆﾎﾝｺﾞｼﾞｬ"
                    (uim '(string-append-map-ustr-whole ja-kana-hankaku
                                                        ustrj-l)))
  ;; empty
  (assert-uim-equal ""
                    '(string-append-map-ustr-whole cdr ustre)))

(define (test-string-append-map-ustr-former)
  ;; former-latter
  (assert-uim-equal "hel"
                    '(string-append-map-ustr-former car ustr-fl))
  (assert-uim-equal "HEL"
                    '(string-append-map-ustr-former cdr ustr-fl))
  (assert-uim-equal "にほん"
                    (uim '(string-append-map-ustr-former ja-kana-hiragana
                                                         ustrj-fl)))
  (assert-uim-equal "ニホン"
                    (uim '(string-append-map-ustr-former ja-kana-katakana
                                                         ustrj-fl)))
  (assert-uim-equal "ﾆﾎﾝ"
                    (uim '(string-append-map-ustr-former ja-kana-hankaku
                                                         ustrj-fl)))
  ;; former
  (assert-uim-equal "hello"
                    '(string-append-map-ustr-former car ustr-f))
  (assert-uim-equal "HELLO"
                    '(string-append-map-ustr-former cdr ustr-f))
  (assert-uim-equal "にほんごじゃ"
                    (uim '(string-append-map-ustr-former ja-kana-hiragana
                                                         ustrj-f)))
  (assert-uim-equal "ニホンゴジャ"
                    (uim '(string-append-map-ustr-former ja-kana-katakana
                                                         ustrj-f)))
  (assert-uim-equal "ﾆﾎﾝｺﾞｼﾞｬ"
                    (uim '(string-append-map-ustr-former ja-kana-hankaku
                                                         ustrj-f)))
  ;; latter
  (assert-uim-equal ""
                    '(string-append-map-ustr-former car ustr-l))
  (assert-uim-equal ""
                    '(string-append-map-ustr-former cdr ustr-l))
  (assert-uim-equal ""
                    (uim '(string-append-map-ustr-former ja-kana-hiragana
                                                         ustrj-l)))
  (assert-uim-equal ""
                    (uim '(string-append-map-ustr-former ja-kana-katakana
                                                         ustrj-l)))
  (assert-uim-equal ""
                    (uim '(string-append-map-ustr-former ja-kana-hankaku
                                                         ustrj-l)))
  ;; empty
  (assert-uim-equal ""
                    '(string-append-map-ustr-former cdr ustre)))

(define (test-string-append-map-ustr-latter)
  ;; former-latter
  (assert-uim-equal "lo"
                    '(string-append-map-ustr-latter car ustr-fl))
  (assert-uim-equal "LO"
                    '(string-append-map-ustr-latter cdr ustr-fl))
  (assert-uim-equal "ごじゃ"
                    (uim '(string-append-map-ustr-latter ja-kana-hiragana
                                                         ustrj-fl)))
  (assert-uim-equal "ゴジャ"
                    (uim '(string-append-map-ustr-latter ja-kana-katakana
                                                         ustrj-fl)))
  (assert-uim-equal "ｺﾞｼﾞｬ"
                    (uim '(string-append-map-ustr-latter ja-kana-hankaku
                                                         ustrj-fl)))
  ;; former
  (assert-uim-equal ""
                    '(string-append-map-ustr-latter car ustr-f))
  (assert-uim-equal ""
                    '(string-append-map-ustr-latter cdr ustr-f))
  (assert-uim-equal ""
                    (uim '(string-append-map-ustr-latter ja-kana-hiragana
                                                         ustrj-f)))
  (assert-uim-equal ""
                    (uim '(string-append-map-ustr-latter ja-kana-katakana
                                                         ustrj-f)))
  (assert-uim-equal ""
                    (uim '(string-append-map-ustr-latter ja-kana-hankaku
                                                         ustrj-f)))
  ;; latter
  (assert-uim-equal "hello"
                    '(string-append-map-ustr-latter car ustr-l))
  (assert-uim-equal "HELLO"
                    '(string-append-map-ustr-latter cdr ustr-l))
  (assert-uim-equal "にほんごじゃ"
                    (uim '(string-append-map-ustr-latter ja-kana-hiragana
                                                         ustrj-l)))
  (assert-uim-equal "ニホンゴジャ"
                    (uim '(string-append-map-ustr-latter ja-kana-katakana
                                                         ustrj-l)))
  (assert-uim-equal "ﾆﾎﾝｺﾞｼﾞｬ"
                    (uim '(string-append-map-ustr-latter ja-kana-hankaku
                                                         ustrj-l)))
  ;; empty
  (assert-uim-equal ""
                    '(string-append-map-ustr-latter cdr ustre)))

(define (test-ustr-cursor-at-beginning?)
  (assert-uim-false '(ustr-cursor-at-beginning? ustra-fl))
  (assert-uim-false '(ustr-cursor-at-beginning? ustra-f))
  (assert-uim-true  '(ustr-cursor-at-beginning? ustra-l))
  (assert-uim-true  '(ustr-cursor-at-beginning? ustre)))

(define (test-ustr-cursor-at-end?)
  (assert-uim-false '(ustr-cursor-at-end? ustra-fl))
  (assert-uim-true  '(ustr-cursor-at-end? ustra-f))
  (assert-uim-false '(ustr-cursor-at-end? ustra-l))
  (assert-uim-true  '(ustr-cursor-at-end? ustre)))

(define (test-ustr-cursor-pos)
  (assert-uim-equal 3
                    '(ustr-cursor-pos ustr-fl))
  (assert-uim-equal 5
                    '(ustr-cursor-pos ustr-f))
  (assert-uim-equal 0
                    '(ustr-cursor-pos ustr-l))

  (assert-uim-equal 3
                    '(ustr-cursor-pos ustrj-fl))
  (assert-uim-equal 5
                    '(ustr-cursor-pos ustrj-f))
  (assert-uim-equal 0
                    '(ustr-cursor-pos ustrj-l))

  (assert-uim-equal 3
                    '(ustr-cursor-pos ustra-fl))
  (assert-uim-equal 5
                    '(ustr-cursor-pos ustra-f))
  (assert-uim-equal 0
                    '(ustr-cursor-pos ustra-l))

  (assert-uim-equal 0
                    '(ustr-cursor-pos ustre)))

(define (test-ustr-set-cursor-pos!)
  ;; former-latter
  (assert-uim-equal 3
                    '(ustr-cursor-pos ustra-fl))
  (assert-uim-true  '(ustr-set-cursor-pos! ustra-fl 0))
  (assert-uim-equal 0
                    '(ustr-cursor-pos ustra-fl))
  (assert-uim-true  '(ustr-set-cursor-pos! ustra-fl 5))
  (assert-uim-equal 5
                    '(ustr-cursor-pos ustra-fl))
  (assert-uim-true  '(ustr-set-cursor-pos! ustra-fl 2))
  (assert-uim-equal 2
                    '(ustr-cursor-pos ustra-fl))
  (assert-uim-false '(ustr-set-cursor-pos! ustra-fl -1))
  (assert-uim-false '(ustr-set-cursor-pos! ustra-fl 6))
  ;; empty
  (assert-uim-equal 0
                    '(ustr-cursor-pos ustre))
  (assert-uim-true  '(ustr-set-cursor-pos! ustra-fl 0))
  (assert-uim-equal 0
                    '(ustr-cursor-pos ustre))
  (assert-uim-false '(ustr-set-cursor-pos! ustre -1))
  (assert-uim-false '(ustr-set-cursor-pos! ustre 1)))

(define (test-ustr-cursor-move!)
  (assert-uim-equal 3
                    '(ustr-cursor-pos ustra-fl))
  (assert-uim-false '(ustr-cursor-move! ustra-fl -4))
  (assert-uim-equal 3
                    '(ustr-cursor-pos ustra-fl))

  (assert-uim-false '(ustr-cursor-move! ustra-fl 3))
  (assert-uim-equal 3
                    '(ustr-cursor-pos ustra-fl))

  (assert-uim-true  '(ustr-cursor-move! ustra-fl 2))
  (assert-uim-equal 5
                    '(ustr-cursor-pos ustra-fl))

  (assert-uim-true  '(ustr-cursor-move! ustra-fl -3))
  (assert-uim-equal 2
                    '(ustr-cursor-pos ustra-fl))

  (assert-uim-true  '(ustr-cursor-move! ustra-fl 1))
  (assert-uim-equal 3
                    '(ustr-cursor-pos ustra-fl))

  (assert-uim-true  '(ustr-cursor-move! ustra-fl 0))
  (assert-uim-equal 3
                    '(ustr-cursor-pos ustra-fl))

  (assert-uim-true  '(ustr-cursor-move! ustra-fl -3))
  (assert-uim-equal 0
                    '(ustr-cursor-pos ustra-fl))

  (assert-uim-equal 0
                    '(ustr-cursor-pos ustre))
  (assert-uim-true  '(ustr-cursor-move! ustra-fl 0))
  (assert-uim-equal 0
                    '(ustr-cursor-pos ustre))

  (assert-uim-false '(ustr-cursor-move! ustre -1))
  (assert-uim-false '(ustr-cursor-move! ustre 1)))

(define (test-ustr-cursor-move-backward!)
  (assert-uim-equal 3
                    '(ustr-cursor-pos ustra-fl))

  (uim-eval '(ustr-cursor-move-backward! ustra-fl))
  (assert-uim-equal 2
                    '(ustr-cursor-pos ustra-fl))

  (uim-eval '(ustr-cursor-move-backward! ustra-fl))
  (assert-uim-equal 1
                    '(ustr-cursor-pos ustra-fl))

  (uim-eval '(ustr-cursor-move-backward! ustra-fl))
  (assert-uim-equal 0
                    '(ustr-cursor-pos ustra-fl))

  (uim-eval '(ustr-cursor-move-backward! ustra-fl))
  (assert-uim-equal 0
                    '(ustr-cursor-pos ustra-fl))
  ;; start from end of string
  (assert-uim-equal 5
                    '(ustr-cursor-pos ustra-f))
  (uim-eval '(ustr-cursor-move-backward! ustra-f))
  (assert-uim-equal 4
                    '(ustr-cursor-pos ustra-f)))

(define (test-ustr-cursor-move-forward!)
  (assert-uim-equal 3
                    '(ustr-cursor-pos ustra-fl))

  (uim-eval '(ustr-cursor-move-forward! ustra-fl))
  (assert-uim-equal 4
                    '(ustr-cursor-pos ustra-fl))

  (uim-eval '(ustr-cursor-move-forward! ustra-fl))
  (assert-uim-equal 5
                    '(ustr-cursor-pos ustra-fl))

  (uim-eval '(ustr-cursor-move-forward! ustra-fl))
  (assert-uim-equal 5
                    '(ustr-cursor-pos ustra-fl))

  ;; start from beginning of string
  (assert-uim-equal 0
                    '(ustr-cursor-pos ustra-l))
  (uim-eval '(ustr-cursor-move-forward! ustra-l))
  (assert-uim-equal 1
                    '(ustr-cursor-pos ustra-l)))

(define (test-ustr-cursor-move-beginning!)
  (assert-uim-equal 3
                    '(ustr-cursor-pos ustra-fl))

  (uim-eval '(ustr-cursor-move-beginning! ustra-fl))
  (assert-uim-equal 0
                    '(ustr-cursor-pos ustra-fl))

  (uim-eval '(ustr-cursor-move-beginning! ustra-fl))
  (assert-uim-equal 0
                    '(ustr-cursor-pos ustra-fl))

  ;; start from end of string
  (assert-uim-equal 5
                    '(ustr-cursor-pos ustra-f))
  (uim-eval '(ustr-cursor-move-beginning! ustra-f))
  (assert-uim-equal 0
                    '(ustr-cursor-pos ustra-f)))

(define (test-ustr-cursor-move-end!)
  (assert-uim-equal 3
                    '(ustr-cursor-pos ustra-fl))

  (uim-eval '(ustr-cursor-move-end! ustra-fl))
  (assert-uim-equal 5
                    '(ustr-cursor-pos ustra-fl))

  (uim-eval '(ustr-cursor-move-end! ustra-fl))
  (assert-uim-equal 5
                    '(ustr-cursor-pos ustra-fl))

  ;; start from beginning of string
  (assert-uim-equal 0
                    '(ustr-cursor-pos ustra-l))
  (uim-eval '(ustr-cursor-move-end! ustra-l))
  (assert-uim-equal 5
                    '(ustr-cursor-pos ustra-l)))

(define (test-ustr-cursor-frontside)
  ;; former-latter
  (assert-uim-equal '("l" . "L")
                    '(ustr-cursor-frontside ustr-fl))
  (assert-uim-equal '("ご" "ゴ" "ｺﾞ")
                    '(ustr-cursor-frontside ustrj-fl))
  (assert-uim-equal "l"
                    '(ustr-cursor-frontside ustra-fl))
  ;; former
  (assert-uim-error '(ustr-cursor-frontside ustr-f))
  (assert-uim-error '(ustr-cursor-frontside ustrj-f))
  (assert-uim-error '(ustr-cursor-frontside ustra-f))
  ;; latter
  (assert-uim-equal '("h" . "H")
                    '(ustr-cursor-frontside ustr-l))
  (assert-uim-equal '("に" "ニ" "ﾆ")
                    '(ustr-cursor-frontside ustrj-l))
  (assert-uim-equal "h"
                    '(ustr-cursor-frontside ustra-l))
  ;; empty
  (assert-uim-error '(ustr-cursor-frontside ustre)))

(define (test-ustr-cursor-backside)
  ;; former-latter
  (assert-uim-equal '("l" . "L")
                    '(ustr-cursor-backside ustr-fl))
  (assert-uim-equal '("ん" "ン" "ﾝ")
                    '(ustr-cursor-backside ustrj-fl))
  (assert-uim-equal "l"
                    '(ustr-cursor-backside ustra-fl))
  ;; former
  (assert-uim-equal '("o" . "O")
                    '(ustr-cursor-backside ustr-f))
  (assert-uim-equal '("じゃ" "ジャ" "ｼﾞｬ")
                    '(ustr-cursor-backside ustrj-f))
  (assert-uim-equal "o"
                    '(ustr-cursor-backside ustra-f))
  ;; latter
  (assert-uim-error '(ustr-cursor-backside ustr-l))
  (assert-uim-error '(ustr-cursor-backside ustrj-l))
  (assert-uim-error '(ustr-cursor-backside ustra-l))
  ;; empty
  (assert-uim-error '(ustr-cursor-backside ustre)))

(define (test-ustr-cursor-delete-frontside!)
  ;; former-latter
  (assert-uim-equal '("ご" "ゴ" "ｺﾞ")
                    '(ustr-cursor-frontside ustrj-fl))
  (assert-uim-equal 5
                    '(ustr-length ustrj-fl))
  (assert-uim-equal 3
                    '(ustr-cursor-pos ustrj-fl))
  (assert-uim-true  '(ustr-cursor-delete-frontside! ustrj-fl))
  (assert-uim-equal '("じゃ" "ジャ" "ｼﾞｬ")
                    '(ustr-cursor-frontside ustrj-fl))
  (assert-uim-equal 4
                    '(ustr-length ustrj-fl))
  (assert-uim-equal 3
                    '(ustr-cursor-pos ustrj-fl))
  (assert-uim-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                      ("じゃ" "ジャ" "ｼﾞｬ"))
                    '(ustr-whole-seq ustrj-fl))
  ;; former
  (assert-uim-error '(ustr-cursor-frontside ustrj-f))
  (assert-uim-equal 5
                    '(ustr-length ustrj-f))
  (assert-uim-equal 5
                    '(ustr-cursor-pos ustrj-f))
  (assert-uim-false '(ustr-cursor-delete-frontside! ustrj-f))
  (assert-uim-error '(ustr-cursor-frontside ustrj-f))
  (assert-uim-equal 5
                    '(ustr-length ustrj-f))
  (assert-uim-equal 5
                    '(ustr-cursor-pos ustrj-f))
  (assert-uim-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                      ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
                    '(ustr-whole-seq ustrj-f))
  ;; latter
  (assert-uim-equal '("に" "ニ" "ﾆ")
                    '(ustr-cursor-frontside ustrj-l))
  (assert-uim-equal 5
                    '(ustr-length ustrj-l))
  (assert-uim-equal 0
                    '(ustr-cursor-pos ustrj-l))
  (assert-uim-true  '(ustr-cursor-delete-frontside! ustrj-l))
  (assert-uim-equal '("ほ" "ホ" "ﾎ")
                    '(ustr-cursor-frontside ustrj-l))
  (assert-uim-equal 4
                    '(ustr-length ustrj-l))
  (assert-uim-equal 0
                    '(ustr-cursor-pos ustrj-l))
  (assert-uim-equal '(("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                      ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
                    '(ustr-whole-seq ustrj-l))
  ;; empty
  (assert-uim-error '(ustr-cursor-frontside ustre))
  (assert-uim-equal 0
                    '(ustr-length ustre))
  (assert-uim-equal 0
                    '(ustr-cursor-pos ustre))
  (assert-uim-false '(ustr-cursor-delete-frontside! ustre))
  (assert-uim-error '(ustr-cursor-frontside ustre))
  (assert-uim-equal 0
                    '(ustr-length ustre))
  (assert-uim-equal 0
                    '(ustr-cursor-pos ustre)))

(define (test-ustr-cursor-delete-backside!)
  ;; former-latter
  (assert-uim-equal '("ん" "ン" "ﾝ")
                    '(ustr-cursor-backside ustrj-fl))
  (assert-uim-equal 5
                    '(ustr-length ustrj-fl))
  (assert-uim-equal 3
                    '(ustr-cursor-pos ustrj-fl))
  (assert-uim-true  '(ustr-cursor-delete-backside! ustrj-fl))
  (assert-uim-equal '("ほ" "ホ" "ﾎ")
                    '(ustr-cursor-backside ustrj-fl))
  (assert-uim-equal 4
                    '(ustr-length ustrj-fl))
  (assert-uim-equal 2
                    '(ustr-cursor-pos ustrj-fl))
  (assert-uim-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ")
                      ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
                    '(ustr-whole-seq ustrj-fl))
  ;; former
  (assert-uim-equal '("じゃ" "ジャ" "ｼﾞｬ")
                    '(ustr-cursor-backside ustrj-f))
  (assert-uim-equal 5
                    '(ustr-length ustrj-f))
  (assert-uim-equal 5
                    '(ustr-cursor-pos ustrj-f))
  (assert-uim-true  '(ustr-cursor-delete-backside! ustrj-f))
  (assert-uim-equal '("ご" "ゴ" "ｺﾞ")
                    '(ustr-cursor-backside ustrj-f))
  (assert-uim-equal 4
                    '(ustr-length ustrj-f))
  (assert-uim-equal 4
                    '(ustr-cursor-pos ustrj-f))
  (assert-uim-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                      ("ご" "ゴ" "ｺﾞ"))
                    '(ustr-whole-seq ustrj-f))
  ;; latter
  (assert-uim-error '(ustr-cursor-backside ustrj-l))
  (assert-uim-equal 5
                    '(ustr-length ustrj-l))
  (assert-uim-equal 0
                    '(ustr-cursor-pos ustrj-l))
  (assert-uim-false '(ustr-cursor-delete-backside! ustrj-l))
  (assert-uim-error '(ustr-cursor-backside ustrj-l))
  (assert-uim-equal 5
                    '(ustr-length ustrj-l))
  (assert-uim-equal 0
                    '(ustr-cursor-pos ustrj-l))
  (assert-uim-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                      ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
                    '(ustr-whole-seq ustrj-l))
  ;; empty
  (assert-uim-error '(ustr-cursor-backside ustre))
  (assert-uim-equal 0
                    '(ustr-length ustre))
  (assert-uim-false '(ustr-cursor-delete-backside! ustre))
  (assert-uim-error '(ustr-cursor-backside ustre))
  (assert-uim-equal 0
                    '(ustr-length ustre)))

(define (test-ustr-insert-elem!)
  ;; former-latter
  (assert-uim-equal '("ん" "ン" "ﾝ")
                    '(ustr-cursor-backside ustrj-fl))
  (assert-uim-equal 5
                    '(ustr-length ustrj-fl))
  (assert-uim-equal 3
                    '(ustr-cursor-pos ustrj-fl))
  (uim-eval '(ustr-insert-elem! ustrj-fl '("んー" "ンー" "ﾝｰ")))
  (assert-uim-equal '("んー" "ンー" "ﾝｰ")
                    '(ustr-cursor-backside ustrj-fl))
  (assert-uim-equal 6
                    '(ustr-length ustrj-fl))
  (assert-uim-equal 4
                    '(ustr-cursor-pos ustrj-fl))
  (assert-uim-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                      ("んー" "ンー" "ﾝｰ")
                      ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
                    '(ustr-whole-seq ustrj-fl))
  ;; former
  (assert-uim-equal '("じゃ" "ジャ" "ｼﾞｬ")
                    '(ustr-cursor-backside ustrj-f))
  (assert-uim-equal 5
                    '(ustr-length ustrj-f))
  (assert-uim-equal 5
                    '(ustr-cursor-pos ustrj-f))
  (uim-eval '(ustr-insert-elem! ustrj-f '("んー" "ンー" "ﾝｰ")))
  (assert-uim-equal '("んー" "ンー" "ﾝｰ")
                    '(ustr-cursor-backside ustrj-f))
  (assert-uim-equal 6
                    '(ustr-length ustrj-f))
  (assert-uim-equal 6
                    '(ustr-cursor-pos ustrj-f))
  (assert-uim-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                      ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ") ("んー" "ンー" "ﾝｰ"))
                    '(ustr-whole-seq ustrj-f))
  ;; latter
  (assert-uim-error '(ustr-cursor-backside ustrj-l))
  (assert-uim-equal 5
                    '(ustr-length ustrj-l))
  (assert-uim-equal 0
                    '(ustr-cursor-pos ustrj-l))
  (uim-eval '(ustr-insert-elem! ustrj-l '("んー" "ンー" "ﾝｰ")))
  (assert-uim-equal '("んー" "ンー" "ﾝｰ")
                    '(ustr-cursor-backside ustrj-l))
  (assert-uim-equal 6
                    '(ustr-length ustrj-l))
  (assert-uim-equal 1
                    '(ustr-cursor-pos ustrj-l))
  (assert-uim-equal '(("んー" "ンー" "ﾝｰ")
                      ("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                      ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
                    '(ustr-whole-seq ustrj-l))
  ;; empty
  (assert-uim-error '(ustr-cursor-backside ustre))
  (assert-uim-equal 0
                    '(ustr-length ustre))
  (uim-eval '(ustr-insert-elem! ustre '("んー" "ンー" "ﾝｰ")))
  (assert-uim-equal '("んー" "ンー" "ﾝｰ")
                    '(ustr-cursor-backside ustre))
  (assert-uim-equal 1
                    '(ustr-length ustre))
  (assert-uim-equal '(("んー" "ンー" "ﾝｰ"))
                    '(ustr-whole-seq ustre)))

(define (test-ustr-cursor-set-frontside!)
  ;; former-latter
  (assert-uim-equal '("ご" "ゴ" "ｺﾞ")
                    '(ustr-cursor-frontside ustrj-fl))
  (assert-uim-equal 5
                    '(ustr-length ustrj-fl))
  (assert-uim-equal 3
                    '(ustr-cursor-pos ustrj-fl))
  (assert-true (uim-bool '(ustr-cursor-set-frontside! ustrj-fl
                                                      '("んー" "ンー" "ﾝｰ"))))
  (assert-uim-equal '("んー" "ンー" "ﾝｰ")
                    '(ustr-cursor-frontside ustrj-fl))
  (assert-uim-equal 5
                    '(ustr-length ustrj-fl))
  (assert-uim-equal 3
                    '(ustr-cursor-pos ustrj-fl))
  (assert-uim-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                      ("んー" "ンー" "ﾝｰ") ("じゃ" "ジャ" "ｼﾞｬ"))
                    '(ustr-whole-seq ustrj-fl))
  ;; former
  (assert-uim-error '(ustr-cursor-frontside ustrj-f))
  (assert-uim-equal 5
                    '(ustr-length ustrj-f))
  (assert-uim-equal 5
                    '(ustr-cursor-pos ustrj-f))
  (assert-false (uim-bool '(ustr-cursor-set-frontside! ustrj-f
                                                       '("んー" "ンー" "ﾝｰ"))))
  (assert-uim-equal 5
                    '(ustr-length ustrj-f))
  (assert-uim-equal 5
                    '(ustr-cursor-pos ustrj-f))
  (assert-uim-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                      ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
                    '(ustr-whole-seq ustrj-f))
  ;; latter
  (assert-uim-equal '("に" "ニ" "ﾆ")
                    '(ustr-cursor-frontside ustrj-l))
  (assert-uim-equal 5
                    '(ustr-length ustrj-l))
  (assert-uim-equal 0
                    '(ustr-cursor-pos ustrj-l))
  (assert-true  (uim-bool '(ustr-cursor-set-frontside! ustrj-l
                                                       '("んー" "ンー" "ﾝｰ"))))
  (assert-uim-equal '("んー" "ンー" "ﾝｰ")
                    '(ustr-cursor-frontside ustrj-l))
  (assert-uim-equal 5
                    '(ustr-length ustrj-l))
  (assert-uim-equal 0
                    '(ustr-cursor-pos ustrj-l))
  (assert-uim-equal '(("んー" "ンー" "ﾝｰ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                      ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
                    '(ustr-whole-seq ustrj-l))
  ;; empty
  (assert-uim-error '(ustr-cursor-frontside ustre))
  (assert-uim-equal 0
                    '(ustr-length ustre))
  (assert-false (uim-bool '(ustr-cursor-set-frontside! ustre
                                                       '("んー" "ンー" "ﾝｰ"))))
  (assert-uim-error '(ustr-cursor-frontside ustre))
  (assert-uim-equal 0
                    '(ustr-length ustre))
  (assert-uim-equal '()
                    '(ustr-whole-seq ustre)))

(define (test-ustr-cursor-set-backside!)
  ;; former-latter
  (assert-uim-equal '("ん" "ン" "ﾝ")
                    '(ustr-cursor-backside ustrj-fl))
  (assert-uim-equal 5
                    '(ustr-length ustrj-fl))
  (assert-uim-equal 3
                    '(ustr-cursor-pos ustrj-fl))
  (assert-true  (uim-bool '(ustr-cursor-set-backside! ustrj-fl
                                                      '("んー" "ンー" "ﾝｰ"))))
  (assert-uim-equal '("んー" "ンー" "ﾝｰ")
                    '(ustr-cursor-backside ustrj-fl))
  (assert-uim-equal 5
                    '(ustr-length ustrj-fl))
  (assert-uim-equal 3
                    '(ustr-cursor-pos ustrj-fl))
  (assert-uim-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("んー" "ンー" "ﾝｰ")
                      ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
                    '(ustr-whole-seq ustrj-fl))
  ;; former
  (assert-uim-equal '("じゃ" "ジャ" "ｼﾞｬ")
                    '(ustr-cursor-backside ustrj-f))
  (assert-uim-equal 5
                    '(ustr-length ustrj-f))
  (assert-uim-equal 5
                    '(ustr-cursor-pos ustrj-f))
  (assert-true  (uim-bool '(ustr-cursor-set-backside! ustrj-f
                                                      '("んー" "ンー" "ﾝｰ"))))
  (assert-uim-equal '("んー" "ンー" "ﾝｰ")
                    '(ustr-cursor-backside ustrj-f))
  (assert-uim-equal 5
                    '(ustr-length ustrj-f))
  (assert-uim-equal 5
                    '(ustr-cursor-pos ustrj-f))
  (assert-uim-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                      ("ご" "ゴ" "ｺﾞ") ("んー" "ンー" "ﾝｰ"))
                    '(ustr-whole-seq ustrj-f))
  ;; latter
  (assert-uim-error '(ustr-cursor-backside ustrj-l))
  (assert-uim-equal 5
                    '(ustr-length ustrj-l))
  (assert-uim-equal 0
                    '(ustr-cursor-pos ustrj-l))
  (assert-false (uim-bool '(ustr-cursor-set-backside! ustrj-l
                                                      '("んー" "ンー" "ﾝｰ"))))
  (assert-uim-error '(ustr-cursor-backside ustrj-l))
  (assert-uim-equal 5
                    '(ustr-length ustrj-l))
  (assert-uim-equal 0
                    '(ustr-cursor-pos ustrj-l))
  (assert-uim-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                      ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
                    '(ustr-whole-seq ustrj-l))
  ;; empty
  (assert-uim-error '(ustr-cursor-backside ustre))
  (assert-uim-equal 0
                    '(ustr-length ustre))
  (assert-false (uim-bool '(ustr-cursor-set-backside! ustre
                                                      '("んー" "ンー" "ﾝｰ"))))
  (assert-uim-error '(ustr-cursor-backside ustre))
  (assert-uim-equal 0
                    '(ustr-length ustre))
  (assert-uim-equal '()
                    '(ustr-whole-seq ustre)))

(define (test-ustr-insert-seq!)
  ;; former-latter
  (assert-uim-equal '("ん" "ン" "ﾝ")
                    '(ustr-cursor-backside ustrj-fl))
  (assert-uim-equal 5
                    '(ustr-length ustrj-fl))
  (assert-uim-equal 3
                    '(ustr-cursor-pos ustrj-fl))
  (uim-eval '(ustr-insert-seq! ustrj-fl '(("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ")
                                          ("よ" "ヨ" "ﾖ"))))
  (assert-uim-equal '("よ" "ヨ" "ﾖ")
                    '(ustr-cursor-backside ustrj-fl))
  (assert-uim-equal 8
                    '(ustr-length ustrj-fl))
  (assert-uim-equal 6
                    '(ustr-cursor-pos ustrj-fl))
  (assert-uim-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                      ("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ") ("よ" "ヨ" "ﾖ")
                      ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
                    '(ustr-whole-seq ustrj-fl))
  ;; former
  (assert-uim-equal '("じゃ" "ジャ" "ｼﾞｬ")
                    '(ustr-cursor-backside ustrj-f))
  (assert-uim-equal 5
                    '(ustr-length ustrj-f))
  (assert-uim-equal 5
                    '(ustr-cursor-pos ustrj-f))
  (uim-eval '(ustr-insert-seq! ustrj-f '(("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ")
                                         ("よ" "ヨ" "ﾖ"))))
  (assert-uim-equal '("よ" "ヨ" "ﾖ")
                    '(ustr-cursor-backside ustrj-f))
  (assert-uim-equal 8
                    '(ustr-length ustrj-f))
  (assert-uim-equal 8
                    '(ustr-cursor-pos ustrj-f))
  (assert-uim-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                      ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ")
                      ("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ") ("よ" "ヨ" "ﾖ"))
                    '(ustr-whole-seq ustrj-f))
  ;; latter
  (assert-uim-error '(ustr-cursor-backside ustrj-l))
  (assert-uim-equal 5
                    '(ustr-length ustrj-l))
  (assert-uim-equal 0
                    '(ustr-cursor-pos ustrj-l))
  (uim-eval '(ustr-insert-seq! ustrj-l '(("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ")
                                         ("よ" "ヨ" "ﾖ"))))
  (assert-uim-equal '("よ" "ヨ" "ﾖ")
                    '(ustr-cursor-backside ustrj-l))
  (assert-uim-equal 8
                    '(ustr-length ustrj-l))
  (assert-uim-equal 3
                    '(ustr-cursor-pos ustrj-l))
  (assert-uim-equal '(("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ") ("よ" "ヨ" "ﾖ")
                      ("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                      ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
                    '(ustr-whole-seq ustrj-l))
  ;; empty
  (assert-uim-error '(ustr-cursor-backside ustre))
  (assert-uim-equal 0
                    '(ustr-length ustre))
  (assert-uim-equal 0
                    '(ustr-cursor-pos ustre))
  (uim-eval '(ustr-insert-seq! ustre '(("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ")
                                       ("よ" "ヨ" "ﾖ"))))
  (assert-uim-equal '("よ" "ヨ" "ﾖ")
                    '(ustr-cursor-backside ustre))
  (assert-uim-equal 3
                    '(ustr-length ustre))
  (assert-uim-equal 3
                    '(ustr-cursor-pos ustre))
  (assert-uim-equal '(("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ") ("よ" "ヨ" "ﾖ"))
                    '(ustr-whole-seq ustre)))

(define (test-ustr-insert-seq!-#2)
  ;; former-latter
  (assert-uim-equal '("ん" "ン" "ﾝ")
                    '(ustr-cursor-backside ustrj-fl))
  (assert-uim-equal 5
                    '(ustr-length ustrj-fl))
  (assert-uim-equal 3
                    '(ustr-cursor-pos ustrj-fl))
  (uim-eval '(ustr-insert-seq! ustrj-fl ()))
  (assert-uim-equal '("ん" "ン" "ﾝ")
                    '(ustr-cursor-backside ustrj-fl))
  (assert-uim-equal 5
                    '(ustr-length ustrj-fl))
  (assert-uim-equal 3
                    '(ustr-cursor-pos ustrj-fl))
  (assert-uim-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                      ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
                    '(ustr-whole-seq ustrj-fl))
  ;; former
  (assert-uim-equal '("じゃ" "ジャ" "ｼﾞｬ")
                    '(ustr-cursor-backside ustrj-f))
  (assert-uim-equal 5
                    '(ustr-length ustrj-f))
  (assert-uim-equal 5
                    '(ustr-cursor-pos ustrj-f))
  (uim-eval '(ustr-insert-seq! ustrj-f ()))
  (assert-uim-equal '("じゃ" "ジャ" "ｼﾞｬ")
                    '(ustr-cursor-backside ustrj-f))
  (assert-uim-equal 5
                    '(ustr-length ustrj-f))
  (assert-uim-equal 5
                    '(ustr-cursor-pos ustrj-f))
  (assert-uim-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                      ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
                    '(ustr-whole-seq ustrj-f))
  ;; latter
  (assert-uim-error '(ustr-cursor-backside ustrj-l))
  (assert-uim-equal 5
                    '(ustr-length ustrj-l))
  (assert-uim-equal 0
                    '(ustr-cursor-pos ustrj-l))
  (uim-eval '(ustr-insert-seq! ustrj-l ()))
  (assert-uim-error '(ustr-cursor-backside ustrj-l))
  (assert-uim-equal 5
                    '(ustr-length ustrj-l))
  (assert-uim-equal 0
                    '(ustr-cursor-pos ustrj-l))
  (assert-uim-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                      ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
                    '(ustr-whole-seq ustrj-l))
  ;; empty
  (assert-uim-error '(ustr-cursor-backside ustre))
  (assert-uim-equal 0
                    '(ustr-length ustre))
  (assert-uim-equal 0
                    '(ustr-cursor-pos ustre))
  (uim-eval '(ustr-insert-seq! ustre ()))
  (assert-uim-error '(ustr-cursor-backside ustre))
  (assert-uim-equal 0
                    '(ustr-length ustre))
  (assert-uim-equal 0
                    '(ustr-cursor-pos ustre)))

(provide "test/test-ustr")
