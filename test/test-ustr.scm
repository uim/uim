#!/usr/bin/env gosh

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
  (uim-eval '(define ustr-f (ustr-new '(("h" . "H") ("e" . "E") ("l" . "L")
                                        ("l" . "L") ("o" . "O")))))
  (uim-eval '(define ustr-fl (ustr-new '(("h" . "H") ("e" . "E") ("l" . "L"))
                                       '(("l" . "L") ("o" . "O")))))
  (uim-eval '(define ustr-l (ustr-new ()
                                      '(("h" . "H") ("e" . "E") ("l" . "L")
                                        ("l" . "L") ("o" . "O")))))
  (uim-eval '(define ustra-f (ustr-new '("h" "e" "l" "l" "o"))))
  (uim-eval '(define ustra-fl (ustr-new '("h" "e" "l")
                                        '("l" "o"))))
  (uim-eval '(define ustra-l (ustr-new ()
                                       '("h" "e" "l" "l" "o"))))
  (uim-eval '(define ustrj-f (ustr-new '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ")
                                         ("ん" "ン" "ﾝ") ("ご" "ゴ" "ｺﾞ")
                                         ("じゃ" "ジャ" "ｼﾞｬ")))))
  (uim-eval '(define ustrj-fl (ustr-new '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ")
                                          ("ん" "ン" "ﾝ"))
                                        '(("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ")))))
  (uim-eval '(define ustrj-l (ustr-new ()
                                       '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ")
                                         ("ん" "ン" "ﾝ") ("ご" "ゴ" "ｺﾞ")
                                         ("じゃ" "ジャ" "ｼﾞｬ")))))
  (uim-eval '(define ustre (ustr-new ()))))

(define (teardown)
  (uim-test-teardown))

(define (test-ustr-new)
  ;; single sequence goes into former
  (assert-equal '(("o" "l" "l" "e" "h") . ())
                (uim '(ustr-new '("h" "e" "l" "l" "o"))))
  ;; dual sequences are go into former and latter
  (assert-equal '(("l" "e" "h") . ("l" "o"))
                (uim '(ustr-new '("h" "e" "l")
                                '("l" "o"))))
  ;; latter sequence only
  (assert-equal '(() . ("h" "e" "l" "l" "o"))
                (uim '(ustr-new ()
                                '("h" "e" "l" "l" "o")))))

(define (test-ustr-whole-seq)
  (assert-equal '(("h" . "H") ("e" . "E") ("l" . "L") ("l" . "L") ("o" . "O"))
                (uim '(ustr-whole-seq ustr-fl)))
  (assert-equal '(("h" . "H") ("e" . "E") ("l" . "L") ("l" . "L") ("o" . "O"))
                (uim '(ustr-whole-seq ustr-f)))
  (assert-equal '(("h" . "H") ("e" . "E") ("l" . "L") ("l" . "L") ("o" . "O"))
                (uim '(ustr-whole-seq ustr-l)))
  (assert-equal '("h" "e" "l" "l" "o")
                (uim '(ustr-whole-seq ustra-fl)))
  (assert-equal '("h" "e" "l" "l" "o")
                (uim '(ustr-whole-seq ustra-f)))
  (assert-equal '("h" "e" "l" "l" "o")
                (uim '(ustr-whole-seq ustra-l)))
  (assert-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                  ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
                (uim '(ustr-whole-seq ustrj-fl)))
  (assert-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                  ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
                (uim '(ustr-whole-seq ustrj-f)))
  (assert-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                  ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
                (uim '(ustr-whole-seq ustrj-l))))

(define (test-ustr-former-seq)
  (assert-equal '(("h" . "H") ("e" . "E") ("l" . "L"))
                (uim '(ustr-former-seq ustr-fl)))
  (assert-equal '(("h" . "H") ("e" . "E") ("l" . "L") ("l" . "L") ("o" . "O"))
                (uim '(ustr-former-seq ustr-f)))
  (assert-equal '()
                (uim '(ustr-former-seq ustr-l)))
  (assert-equal '("h" "e" "l")
                (uim '(ustr-former-seq ustra-fl)))
  (assert-equal '("h" "e" "l" "l" "o")
                (uim '(ustr-former-seq ustra-f)))
  (assert-equal '()
                (uim '(ustr-former-seq ustra-l)))
  (assert-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ"))
                (uim '(ustr-former-seq ustrj-fl)))
  (assert-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                  ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
                (uim '(ustr-former-seq ustrj-f)))
  (assert-equal '()
                (uim '(ustr-former-seq ustrj-l))))

(define (test-ustr-latter-seq)
  (assert-equal '(("l" . "L") ("o" . "O"))
                (uim '(ustr-latter-seq ustr-fl)))
  (assert-equal '()
                (uim '(ustr-latter-seq ustr-f)))
  (assert-equal '(("h" . "H") ("e" . "E") ("l" . "L") ("l" . "L") ("o" . "O"))
                (uim '(ustr-latter-seq ustr-l)))
  (assert-equal '("l" "o")
                (uim '(ustr-latter-seq ustra-fl)))
  (assert-equal '()
                (uim '(ustr-latter-seq ustra-f)))
  (assert-equal '("h" "e" "l" "l" "o")
                (uim '(ustr-latter-seq ustra-l)))
  (assert-equal '(("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
                (uim '(ustr-latter-seq ustrj-fl)))
  (assert-equal '()
                (uim '(ustr-latter-seq ustrj-f)))
  (assert-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                  ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
                (uim '(ustr-latter-seq ustrj-l))))

(define (test-ustr-set-whole-seq!)
  ;; former-latter
  (assert-equal '("h" "e" "l" "l" "o")
                (uim '(ustr-whole-seq ustra-fl)))
  (assert-equal '("h" "e" "l")
                (uim '(ustr-former-seq ustra-fl)))
  (assert-equal '("l" "o")
                (uim '(ustr-latter-seq ustra-fl)))
  (uim-eval '(ustr-set-whole-seq! ustra-fl '("w" "o" "r" "L" "d")))
  (assert-equal '("w" "o" "r" "L" "d")
                (uim '(ustr-whole-seq ustra-fl)))
  (assert-equal '("w" "o" "r" "L" "d")
                (uim '(ustr-former-seq ustra-fl)))
  (assert-equal '()
                (uim '(ustr-latter-seq ustra-fl)))
  ;; former
  (assert-equal '("h" "e" "l" "l" "o")
                (uim '(ustr-whole-seq ustra-f)))
  (assert-equal '("h" "e" "l" "l" "o")
                (uim '(ustr-former-seq ustra-f)))
  (assert-equal '()
                (uim '(ustr-latter-seq ustra-f)))
  (uim-eval '(ustr-set-whole-seq! ustra-f '("w" "o" "r" "L" "d")))
  (assert-equal '("w" "o" "r" "L" "d")
                (uim '(ustr-whole-seq ustra-f)))
  (assert-equal '("w" "o" "r" "L" "d")
                (uim '(ustr-former-seq ustra-f)))
  (assert-equal '()
                (uim '(ustr-latter-seq ustra-f)))
  ;; latter
  (assert-equal '("h" "e" "l" "l" "o")
                (uim '(ustr-whole-seq ustra-l)))
  (assert-equal '()
                (uim '(ustr-former-seq ustra-l)))
  (assert-equal '("h" "e" "l" "l" "o")
                (uim '(ustr-latter-seq ustra-l)))
  (uim-eval '(ustr-set-whole-seq! ustra-l '("w" "o" "r" "L" "d")))
  (assert-equal '("w" "o" "r" "L" "d")
                (uim '(ustr-whole-seq ustra-l)))
  (assert-equal '("w" "o" "r" "L" "d")
                (uim '(ustr-former-seq ustra-l)))
  (assert-equal '()
                (uim '(ustr-latter-seq ustra-l))))

(define (test-ustr-set-former-seq!)
  ;; former-latter
  (assert-equal '("h" "e" "l" "l" "o")
                (uim '(ustr-whole-seq ustra-fl)))
  (assert-equal '("h" "e" "l")
                (uim '(ustr-former-seq ustra-fl)))
  (assert-equal '("l" "o")
                (uim '(ustr-latter-seq ustra-fl)))
  (uim-eval '(ustr-set-former-seq! ustra-fl '("w" "o" "r" "L" "d")))
  (assert-equal '("w" "o" "r" "L" "d" "l" "o")
                (uim '(ustr-whole-seq ustra-fl)))
  (assert-equal '("w" "o" "r" "L" "d")
                (uim '(ustr-former-seq ustra-fl)))
  (assert-equal '("l" "o")
                (uim '(ustr-latter-seq ustra-fl)))
  ;; former
  (assert-equal '("h" "e" "l" "l" "o")
                (uim '(ustr-whole-seq ustra-f)))
  (assert-equal '("h" "e" "l" "l" "o")
                (uim '(ustr-former-seq ustra-f)))
  (assert-equal '()
                (uim '(ustr-latter-seq ustra-f)))
  (uim-eval '(ustr-set-former-seq! ustra-f '("w" "o" "r" "L" "d")))
  (assert-equal '("w" "o" "r" "L" "d")
                (uim '(ustr-whole-seq ustra-f)))
  (assert-equal '("w" "o" "r" "L" "d")
                (uim '(ustr-former-seq ustra-f)))
  (assert-equal '()
                (uim '(ustr-latter-seq ustra-f)))
  ;; latter
  (assert-equal '("h" "e" "l" "l" "o")
                (uim '(ustr-whole-seq ustra-l)))
  (assert-equal '()
                (uim '(ustr-former-seq ustra-l)))
  (assert-equal '("h" "e" "l" "l" "o")
                (uim '(ustr-latter-seq ustra-l)))
  (uim-eval '(ustr-set-former-seq! ustra-l '("w" "o" "r" "L" "d")))
  (assert-equal '("w" "o" "r" "L" "d" "h" "e" "l" "l" "o")
                (uim '(ustr-whole-seq ustra-l)))
  (assert-equal '("w" "o" "r" "L" "d")
                (uim '(ustr-former-seq ustra-l)))
  (assert-equal '("h" "e" "l" "l" "o")
                (uim '(ustr-latter-seq ustra-l))))

(define (test-ustr-set-latter-seq!)
  ;; former-latter
  (assert-equal '("h" "e" "l" "l" "o")
                (uim '(ustr-whole-seq ustra-fl)))
  (assert-equal '("h" "e" "l")
                (uim '(ustr-former-seq ustra-fl)))
  (assert-equal '("l" "o")
                (uim '(ustr-latter-seq ustra-fl)))
  (uim-eval '(ustr-set-latter-seq! ustra-fl '("w" "o" "r" "L" "d")))
  (assert-equal '("h" "e" "l" "w" "o" "r" "L" "d")
                (uim '(ustr-whole-seq ustra-fl)))
  (assert-equal '("h" "e" "l")
                (uim '(ustr-former-seq ustra-fl)))
  (assert-equal '("w" "o" "r" "L" "d")
                (uim '(ustr-latter-seq ustra-fl)))
  ;; former
  (assert-equal '("h" "e" "l" "l" "o")
                (uim '(ustr-whole-seq ustra-f)))
  (assert-equal '("h" "e" "l" "l" "o")
                (uim '(ustr-former-seq ustra-f)))
  (assert-equal '()
                (uim '(ustr-latter-seq ustra-f)))
  (uim-eval '(ustr-set-latter-seq! ustra-f '("w" "o" "r" "L" "d")))
  (assert-equal '("h" "e" "l" "l" "o" "w" "o" "r" "L" "d")
                (uim '(ustr-whole-seq ustra-f)))
  (assert-equal '("h" "e" "l" "l" "o")
                (uim '(ustr-former-seq ustra-f)))
  (assert-equal '("w" "o" "r" "L" "d")
                (uim '(ustr-latter-seq ustra-f)))
  ;; latter
  (assert-equal '("h" "e" "l" "l" "o")
                (uim '(ustr-whole-seq ustra-l)))
  (assert-equal '()
                (uim '(ustr-former-seq ustra-l)))
  (assert-equal '("h" "e" "l" "l" "o")
                (uim '(ustr-latter-seq ustra-l)))
  (uim-eval '(ustr-set-latter-seq! ustra-l '("w" "o" "r" "L" "d")))
  (assert-equal '("w" "o" "r" "L" "d")
                (uim '(ustr-whole-seq ustra-l)))
  (assert-equal '()
                (uim '(ustr-former-seq ustra-l)))
  (assert-equal '("w" "o" "r" "L" "d")
                (uim '(ustr-latter-seq ustra-l))))

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

  (assert-equal '(() . ())
                (uim 'ustre))
  (assert-true  (uim-bool '(ustr-empty? ustre))))

(define (test-ustr-clear!)
  (assert-false (uim-bool '(ustr-empty? ustra-fl)))
  (uim-eval '(ustr-clear! ustra-fl))
  (assert-true  (uim-bool '(ustr-empty? ustra-fl)))

  (assert-false (uim-bool '(ustr-empty? ustra-f)))
  (uim-eval '(ustr-clear! ustra-f))
  (assert-true  (uim-bool '(ustr-empty? ustra-f)))

  (assert-false (uim-bool '(ustr-empty? ustra-l)))
  (uim-eval '(ustr-clear! ustra-l))
  (assert-true  (uim-bool '(ustr-empty? ustra-l)))

  (assert-true  (uim-bool '(ustr-empty? ustre)))
  (uim-eval '(ustr-clear! ustre))
  (assert-true  (uim-bool '(ustr-empty? ustre))))

(define (test-ustr-clear-former!)
  (assert-false (uim-bool '(ustr-empty? ustra-fl)))
  (uim-eval '(ustr-clear-former! ustra-fl))
  (assert-false (uim-bool '(ustr-empty? ustra-fl)))

  (assert-false (uim-bool '(ustr-empty? ustra-f)))
  (uim-eval '(ustr-clear-former! ustra-f))
  (assert-true  (uim-bool '(ustr-empty? ustra-f)))

  (assert-false (uim-bool '(ustr-empty? ustra-l)))
  (uim-eval '(ustr-clear-former! ustra-l))
  (assert-false (uim-bool '(ustr-empty? ustra-l)))

  (assert-true  (uim-bool '(ustr-empty? ustre)))
  (uim-eval '(ustr-clear-former! ustre))
  (assert-true  (uim-bool '(ustr-empty? ustre))))

(define (test-ustr-clear-latter!)
  (assert-false (uim-bool '(ustr-empty? ustra-fl)))
  (uim-eval '(ustr-clear-latter! ustra-fl))
  (assert-false (uim-bool '(ustr-empty? ustra-fl)))

  (assert-false (uim-bool '(ustr-empty? ustra-f)))
  (uim-eval '(ustr-clear-latter! ustra-f))
  (assert-false (uim-bool '(ustr-empty? ustra-f)))

  (assert-false (uim-bool '(ustr-empty? ustra-l)))
  (uim-eval '(ustr-clear-latter! ustra-l))
  (assert-true  (uim-bool '(ustr-empty? ustra-l)))

  (assert-true  (uim-bool '(ustr-empty? ustre)))
  (uim-eval '(ustr-clear-latter! ustre))
  (assert-true  (uim-bool '(ustr-empty? ustre))))

(define (test-ustr-copy!)
  (assert-false (uim-bool '(equal? ustr-fl ustra-fl)))
  (uim-eval '(ustr-copy! ustr-fl ustra-fl))
  (assert-true (uim-bool '(equal? ustr-fl ustra-fl)))

  (assert-false (uim-bool '(equal? ustr-f ustra-f)))
  (uim-eval '(ustr-copy! ustr-f ustra-f))
  (assert-true (uim-bool '(equal? ustr-f ustra-f)))

  (assert-false (uim-bool '(equal? ustr-l ustra-l)))
  (uim-eval '(ustr-copy! ustr-l ustra-l))
  (assert-true (uim-bool '(equal? ustr-l ustra-l))))

(define (test-ustr=)
  (assert-true  (uim-bool '(ustr= equal? ustr-fl ustr-fl)))
  (assert-true  (uim-bool '(ustr= equal? ustr-fl ustr-f)))
  (assert-true  (uim-bool '(ustr= equal? ustr-f ustr-fl)))
  (assert-true  (uim-bool '(ustr= equal? ustr-fl ustr-l)))
  (assert-true  (uim-bool '(ustr= equal? ustr-l ustr-fl)))
  (assert-true  (uim-bool '(ustr= equal? ustr-f ustr-f)))
  (assert-true  (uim-bool '(ustr= equal? ustr-l ustr-f)))
  (assert-true  (uim-bool '(ustr= equal? ustr-f ustr-l)))
  (assert-true  (uim-bool '(ustr= equal? ustr-l ustr-l)))
  (assert-false (uim-bool '(ustr= equal? ustr-l ustre)))
  (assert-false (uim-bool '(ustr= equal? ustre ustr-l)))

  (assert-true  (uim-bool '(ustr= string=? ustra-fl ustra-fl)))
  (assert-true  (uim-bool '(ustr= string=? ustra-fl ustra-f)))
  (assert-true  (uim-bool '(ustr= string=? ustra-f ustra-fl)))
  (assert-true  (uim-bool '(ustr= string=? ustra-fl ustra-l)))
  (assert-true  (uim-bool '(ustr= string=? ustra-l ustra-fl)))
  (assert-true  (uim-bool '(ustr= string=? ustra-f ustra-f)))
  (assert-true  (uim-bool '(ustr= string=? ustra-l ustra-f)))
  (assert-true  (uim-bool '(ustr= string=? ustra-f ustra-l)))
  (assert-true  (uim-bool '(ustr= string=? ustra-l ustra-l)))
  (assert-false (uim-bool '(ustr= string=? ustra-fl ustre)))
  (assert-false (uim-bool '(ustr= string=? ustre ustra-fl)))
  (assert-false (uim-bool '(ustr= string=? ustra-f ustre)))
  (assert-false (uim-bool '(ustr= string=? ustre ustra-f)))
  (assert-false (uim-bool '(ustr= string=? ustra-l ustre)))
  (assert-false (uim-bool '(ustr= string=? ustre ustra-l)))
  (uim-eval '(ustr-set-former-seq! ustra-f '("h" "e" "l" "l" "o" "!")))
  (assert-false (uim-bool '(ustr= string=? ustra-fl ustra-f)))
  (assert-false (uim-bool '(ustr= string=? ustra-f ustra-fl)))
  (assert-false (uim-bool '(ustr= string=? ustra-l ustra-f)))
  (assert-false (uim-bool '(ustr= string=? ustra-f ustra-l)))
  (assert-true  (uim-bool '(ustr= string=? ustra-f ustra-f)))

  (assert-true  (uim-bool '(ustr= equal? ustrj-fl ustrj-fl)))
  (assert-true  (uim-bool '(ustr= equal? ustrj-fl ustrj-f)))
  (assert-true  (uim-bool '(ustr= equal? ustrj-f ustrj-fl)))
  (assert-true  (uim-bool '(ustr= equal? ustrj-fl ustrj-l)))
  (assert-true  (uim-bool '(ustr= equal? ustrj-l ustrj-fl)))
  (assert-true  (uim-bool '(ustr= equal? ustrj-f ustrj-f)))
  (assert-true  (uim-bool '(ustr= equal? ustrj-l ustrj-f)))
  (assert-true  (uim-bool '(ustr= equal? ustrj-f ustrj-l)))
  (assert-true  (uim-bool '(ustr= equal? ustrj-l ustrj-l)))
  (assert-false (uim-bool '(ustr= equal? ustrj-l ustre)))
  (assert-false (uim-bool '(ustr= equal? ustre ustrj-l))))

(define (test-ustr-length)
  (assert-equal 5
                (uim '(ustr-length ustr-fl)))
  (assert-equal 5
                (uim '(ustr-length ustr-f)))
  (assert-equal 5
                (uim '(ustr-length ustr-l)))
  (assert-equal 5
                (uim '(ustr-length ustra-fl)))
  (assert-equal 5
                (uim '(ustr-length ustra-f)))
  (assert-equal 5
                (uim '(ustr-length ustra-l)))
  (assert-equal 5
                (uim '(ustr-length ustrj-fl)))
  (assert-equal 5
                (uim '(ustr-length ustrj-f)))
  (assert-equal 5
                (uim '(ustr-length ustrj-l)))
  (assert-equal 0
                (uim '(ustr-length ustre))))

(define (test-ustr-nth)
  (assert-equal "h"
                (uim '(ustr-nth ustra-fl 0)))
  (assert-equal "e"
                (uim '(ustr-nth ustra-fl 1)))
  (assert-equal "l"
                (uim '(ustr-nth ustra-fl 2)))
  (assert-equal "l"
                (uim '(ustr-nth ustra-fl 3)))
  (assert-equal "o"
                (uim '(ustr-nth ustra-fl 4)))
  (assert-error (lambda ()
                  (uim '(ustr-nth ustra-fl 5))))
  (assert-error (lambda ()
                  (uim '(ustr-nth ustra-fl -1))))

  (assert-equal "h"
                (uim '(ustr-nth ustra-f 0)))
  (assert-equal "e"
                (uim '(ustr-nth ustra-f 1)))
  (assert-equal "l"
                (uim '(ustr-nth ustra-f 2)))
  (assert-equal "l"
                (uim '(ustr-nth ustra-f 3)))
  (assert-equal "o"
                (uim '(ustr-nth ustra-f 4)))
  (assert-error (lambda ()
                  (uim '(ustr-nth ustra-f 5))))
  (assert-error (lambda ()
                  (uim '(ustr-nth ustra-f -1))))

  (assert-equal "h"
                (uim '(ustr-nth ustra-l 0)))
  (assert-equal "e"
                (uim '(ustr-nth ustra-l 1)))
  (assert-equal "l"
                (uim '(ustr-nth ustra-l 2)))
  (assert-equal "l"
                (uim '(ustr-nth ustra-l 3)))
  (assert-equal "o"
                (uim '(ustr-nth ustra-l 4)))
  (assert-error (lambda ()
                  (uim '(ustr-nth ustra-l 5))))
  (assert-error (lambda ()
                  (uim '(ustr-nth ustra-l -1))))

  (assert-error (lambda ()
                  (uim '(ustr-nth ustre 0)))))

(define (test-ustr-set-nth!)
  (assert-equal "h"
                (uim '(ustr-nth ustra-fl 0)))
  (assert-equal "e"
                (uim '(ustr-nth ustra-fl 1)))
  (assert-equal "l"
                (uim '(ustr-nth ustra-fl 2)))
  (assert-equal "l"
                (uim '(ustr-nth ustra-fl 3)))
  (assert-equal "o"
                (uim '(ustr-nth ustra-fl 4)))
  (assert-error (lambda ()
                  (uim '(ustr-nth ustra-fl 5))))
  (assert-error (lambda ()
                  (uim '(ustr-nth ustra-fl -1))))
  ;; position 0
  (assert-equal 5
                (uim '(ustr-length ustra-fl)))
  (assert-equal 3
                (uim '(ustr-cursor-pos ustra-fl)))
  (uim-eval '(ustr-set-nth! ustra-fl 0 "H"))
  (assert-equal "H"
                (uim '(ustr-nth ustra-fl 0)))
  (assert-equal '("H" "e" "l")
                (uim '(ustr-former-seq ustra-fl)))
  (assert-equal '("l" "o")
                (uim '(ustr-latter-seq ustra-fl)))
  (assert-equal 5
                (uim '(ustr-length ustra-fl)))
  (assert-equal 3
                (uim '(ustr-cursor-pos ustra-fl)))
  ;; position 1
  (assert-equal 5
                (uim '(ustr-length ustra-fl)))
  (assert-equal 3
                (uim '(ustr-cursor-pos ustra-fl)))
  (uim-eval '(ustr-set-nth! ustra-fl 1 "E"))
  (assert-equal "E"
                (uim '(ustr-nth ustra-fl 1)))
  (assert-equal '("H" "E" "l")
                (uim '(ustr-former-seq ustra-fl)))
  (assert-equal '("l" "o")
                (uim '(ustr-latter-seq ustra-fl)))
  (assert-equal 5
                (uim '(ustr-length ustra-fl)))
  (assert-equal 3
                (uim '(ustr-cursor-pos ustra-fl)))
  ;; position 2
  (assert-equal 5
                (uim '(ustr-length ustra-fl)))
  (assert-equal 3
                (uim '(ustr-cursor-pos ustra-fl)))
  (uim-eval '(ustr-set-nth! ustra-fl 2 "L"))
  (assert-equal "L"
                (uim '(ustr-nth ustra-fl 2)))
  (assert-equal '("H" "E" "L")
                (uim '(ustr-former-seq ustra-fl)))
  (assert-equal '("l" "o")
                (uim '(ustr-latter-seq ustra-fl)))
  (assert-equal 5
                (uim '(ustr-length ustra-fl)))
  (assert-equal 3
                (uim '(ustr-cursor-pos ustra-fl)))
  ;; position 3
  (assert-equal 5
                (uim '(ustr-length ustra-fl)))
  (assert-equal 3
                (uim '(ustr-cursor-pos ustra-fl)))
  (uim-eval '(ustr-set-nth! ustra-fl 3 "|"))
  (assert-equal "|"
                (uim '(ustr-nth ustra-fl 3)))
  (assert-equal '("H" "E" "L")
                (uim '(ustr-former-seq ustra-fl)))
  (assert-equal '("|" "o")
                (uim '(ustr-latter-seq ustra-fl)))
  (assert-equal 5
                (uim '(ustr-length ustra-fl)))
  (assert-equal 3
                (uim '(ustr-cursor-pos ustra-fl)))
  ;; position 4
  (assert-equal 5
                (uim '(ustr-length ustra-fl)))
  (assert-equal 3
                (uim '(ustr-cursor-pos ustra-fl)))
  (uim-eval '(ustr-set-nth! ustra-fl 4 "O"))
  (assert-equal "O"
                (uim '(ustr-nth ustra-fl 4)))
  (assert-equal '("H" "E" "L")
                (uim '(ustr-former-seq ustra-fl)))
  (assert-equal '("|" "O")
                (uim '(ustr-latter-seq ustra-fl)))
  (assert-equal 5
                (uim '(ustr-length ustra-fl)))
  (assert-equal 3
                (uim '(ustr-cursor-pos ustra-fl)))
  ;; position 5
  (assert-equal 5
                (uim '(ustr-length ustra-fl)))
  (assert-equal 3
                (uim '(ustr-cursor-pos ustra-fl)))
  (assert-error (lambda ()
                  (uim '(ustr-set-nth! ustra-fl 5 "5"))))
  (assert-error (lambda ()
                  (uim '(ustr-nth ustra-fl 5))))
  (assert-equal '("H" "E" "L")
                (uim '(ustr-former-seq ustra-fl)))
  (assert-equal '("|" "O")
                (uim '(ustr-latter-seq ustra-fl)))
  (assert-equal 5
                (uim '(ustr-length ustra-fl)))
  (assert-equal 3
                (uim '(ustr-cursor-pos ustra-fl)))
  ;; position 5 in former-str
  (assert-equal '("h" "e" "l" "l" "o")
                (uim '(ustr-former-seq ustra-f)))
  (assert-equal '()
                (uim '(ustr-latter-seq ustra-f)))
  (assert-equal 5
                (uim '(ustr-length ustra-f)))
  (assert-equal 5
                (uim '(ustr-cursor-pos ustra-f)))
  (assert-error (lambda ()
                  (uim '(ustr-set-nth! ustra-f 5 "5"))))
  (assert-error (lambda ()
                  (uim '(ustr-nth ustra-f 5))))
  (assert-equal '("h" "e" "l" "l" "o")
                (uim '(ustr-former-seq ustra-f)))
  (assert-equal '()
                (uim '(ustr-latter-seq ustra-f)))
  (assert-equal 5
                (uim '(ustr-length ustra-f)))
  (assert-equal 5
                (uim '(ustr-cursor-pos ustra-f)))
  ;; position 4 in former-str
  (assert-equal '("h" "e" "l" "l" "o")
                (uim '(ustr-former-seq ustra-f)))
  (assert-equal '()
                (uim '(ustr-latter-seq ustra-f)))
  (assert-equal 5
                (uim '(ustr-length ustra-f)))
  (assert-equal 5
                (uim '(ustr-cursor-pos ustra-f)))
  (uim-eval '(ustr-set-nth! ustra-f 4 "O"))
  (assert-equal "O"
                (uim '(ustr-nth ustra-f 4)))
  (assert-equal '("h" "e" "l" "l" "O")
                (uim '(ustr-former-seq ustra-f)))
  (assert-equal '()
                (uim '(ustr-latter-seq ustra-f)))
  (assert-equal 5
                (uim '(ustr-length ustra-f)))
  (assert-equal 5
                (uim '(ustr-cursor-pos ustra-f)))
  ;; position 0 in latter-str
  (assert-equal '()
                (uim '(ustr-former-seq ustra-l)))
  (assert-equal '("h" "e" "l" "l" "o")
                (uim '(ustr-latter-seq ustra-l)))
  (assert-equal 5
                (uim '(ustr-length ustra-l)))
  (assert-equal 0
                (uim '(ustr-cursor-pos ustra-l)))
  (uim-eval '(ustr-set-nth! ustra-l 0 "H"))
  (assert-equal "H"
                (uim '(ustr-nth ustra-l 0)))
  (assert-equal '()
                (uim '(ustr-former-seq ustra-l)))
  (assert-equal '("H" "e" "l" "l" "o")
                (uim '(ustr-latter-seq ustra-l)))
  (assert-equal 5
                (uim '(ustr-length ustra-l)))
  (assert-equal 0
                (uim '(ustr-cursor-pos ustra-l))))

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
  (assert-error (lambda ()
                  (uim '(ustr-ref ustra-fl 5))))
  (assert-error (lambda ()
                  (uim '(ustr-ref ustra-fl -1))))
  ;; former-str
  (assert-true  (uim-bool '(eq? (ustr-ref ustra-f 4)
                                (nthcdr 0 (ustr-former ustra-f)))))
  (assert-error (lambda ()
                  (uim '(ustr-ref ustra-f 5))))
  ;; latter-str
  (assert-true  (uim-bool '(eq? (ustr-ref ustra-l 0)
                                (nthcdr 0 (ustr-latter ustra-l))))))

(define (test-ustr-append!)
  ;; former-latter
  (assert-equal '("ん" "ン" "ﾝ")
                (uim '(ustr-cursor-backside ustrj-fl)))
  (assert-equal 5
                (uim '(ustr-length ustrj-fl)))
  (assert-equal 3
                (uim '(ustr-cursor-pos ustrj-fl)))
  (uim-eval '(ustr-append! ustrj-fl '(("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ")
                                      ("よ" "ヨ" "ﾖ"))))
  (assert-equal '("ん" "ン" "ﾝ")
                (uim '(ustr-cursor-backside ustrj-fl)))
  (assert-equal 8
                (uim '(ustr-length ustrj-fl)))
  (assert-equal 3
                (uim '(ustr-cursor-pos ustrj-fl)))
  (assert-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                  ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ")
                  ("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ") ("よ" "ヨ" "ﾖ"))
                (uim '(ustr-whole-seq ustrj-fl)))
  ;; former
  (assert-equal '("じゃ" "ジャ" "ｼﾞｬ")
                (uim '(ustr-cursor-backside ustrj-f)))
  (assert-equal 5
                (uim '(ustr-length ustrj-f)))
  (assert-equal 5
                (uim '(ustr-cursor-pos ustrj-f)))
  (uim-eval '(ustr-append! ustrj-f '(("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ")
                                     ("よ" "ヨ" "ﾖ"))))
  (assert-equal '("じゃ" "ジャ" "ｼﾞｬ")
                (uim '(ustr-cursor-backside ustrj-f)))
  (assert-equal 8
                (uim '(ustr-length ustrj-f)))
  (assert-equal 5
                (uim '(ustr-cursor-pos ustrj-f)))
  (assert-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                  ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ")
                  ("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ") ("よ" "ヨ" "ﾖ"))
                (uim '(ustr-whole-seq ustrj-f)))
  ;; latter
  (assert-error (lambda ()
                  (uim '(ustr-cursor-backside ustrj-l))))
  (assert-equal 5
                (uim '(ustr-length ustrj-l)))
  (assert-equal 0
                (uim '(ustr-cursor-pos ustrj-l)))
  (uim-eval '(ustr-append! ustrj-l '(("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ")
                                     ("よ" "ヨ" "ﾖ"))))
  (assert-error (lambda ()
                  (uim '(ustr-cursor-backside ustrj-l))))
  (assert-equal 8
                (uim '(ustr-length ustrj-l)))
  (assert-equal 0
                (uim '(ustr-cursor-pos ustrj-l)))
  (assert-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                  ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ")
                  ("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ") ("よ" "ヨ" "ﾖ"))
                (uim '(ustr-whole-seq ustrj-l)))
  ;; empty
  (assert-error (lambda ()
                  (uim '(ustr-cursor-backside ustre))))
  (assert-equal 0
                (uim '(ustr-length ustre)))
  (assert-equal 0
                (uim '(ustr-cursor-pos ustre)))
  (uim-eval '(ustr-append! ustre '(("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ")
                                   ("よ" "ヨ" "ﾖ"))))
  (assert-error (lambda ()
                  (uim '(ustr-cursor-backside ustre))))
  (assert-equal 3
                (uim '(ustr-length ustre)))
  (assert-equal 0
                (uim '(ustr-cursor-pos ustre)))
  (assert-equal '(("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ") ("よ" "ヨ" "ﾖ"))
                (uim '(ustr-whole-seq ustre))))

(define (test-ustr-append!-#2)
  ;; former-latter
  (assert-equal '("ん" "ン" "ﾝ")
                (uim '(ustr-cursor-backside ustrj-fl)))
  (assert-equal 5
                (uim '(ustr-length ustrj-fl)))
  (assert-equal 3
                (uim '(ustr-cursor-pos ustrj-fl)))
  (uim-eval '(ustr-append! ustrj-fl ()))
  (assert-equal '("ん" "ン" "ﾝ")
                (uim '(ustr-cursor-backside ustrj-fl)))
  (assert-equal 5
                (uim '(ustr-length ustrj-fl)))
  (assert-equal 3
                (uim '(ustr-cursor-pos ustrj-fl)))
  (assert-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                  ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
                (uim '(ustr-whole-seq ustrj-fl)))
  ;; former
  (assert-equal '("じゃ" "ジャ" "ｼﾞｬ")
                (uim '(ustr-cursor-backside ustrj-f)))
  (assert-equal 5
                (uim '(ustr-length ustrj-f)))
  (assert-equal 5
                (uim '(ustr-cursor-pos ustrj-f)))
  (uim-eval '(ustr-append! ustrj-f ()))
  (assert-equal '("じゃ" "ジャ" "ｼﾞｬ")
                (uim '(ustr-cursor-backside ustrj-f)))
  (assert-equal 5
                (uim '(ustr-length ustrj-f)))
  (assert-equal 5
                (uim '(ustr-cursor-pos ustrj-f)))
  (assert-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                  ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
                (uim '(ustr-whole-seq ustrj-f)))
  ;; latter
  (assert-error (lambda ()
                  (uim '(ustr-cursor-backside ustrj-l))))
  (assert-equal 5
                (uim '(ustr-length ustrj-l)))
  (assert-equal 0
                (uim '(ustr-cursor-pos ustrj-l)))
  (uim-eval '(ustr-append! ustrj-l ()))
  (assert-error (lambda ()
                  (uim '(ustr-cursor-backside ustrj-l))))
  (assert-equal 5
                (uim '(ustr-length ustrj-l)))
  (assert-equal 0
                (uim '(ustr-cursor-pos ustrj-l)))
  (assert-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                  ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
                (uim '(ustr-whole-seq ustrj-l)))
  ;; empty
  (assert-error (lambda ()
                  (uim '(ustr-cursor-backside ustre))))
  (assert-equal 0
                (uim '(ustr-length ustre)))
  (assert-equal 0
                (uim '(ustr-cursor-pos ustre)))
  (uim-eval '(ustr-append! ustre ()))
  (assert-error (lambda ()
                  (uim '(ustr-cursor-backside ustre))))
  (assert-equal 0
                (uim '(ustr-length ustre)))
  (assert-equal 0
                (uim '(ustr-cursor-pos ustre))))

(define (test-ustr-prepend!)
  ;; former-latter
  (assert-equal '("ん" "ン" "ﾝ")
                (uim '(ustr-cursor-backside ustrj-fl)))
  (assert-equal 5
                (uim '(ustr-length ustrj-fl)))
  (assert-equal 3
                (uim '(ustr-cursor-pos ustrj-fl)))
  (uim-eval '(ustr-prepend! ustrj-fl '(("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ")
                                       ("よ" "ヨ" "ﾖ"))))
  (assert-equal '("ん" "ン" "ﾝ")
                (uim '(ustr-cursor-backside ustrj-fl)))
  (assert-equal 8
                (uim '(ustr-length ustrj-fl)))
  (assert-equal 6
                (uim '(ustr-cursor-pos ustrj-fl)))
  (assert-equal '(("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ") ("よ" "ヨ" "ﾖ")
                  ("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                  ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
                (uim '(ustr-whole-seq ustrj-fl)))
  ;; former
  (assert-equal '("じゃ" "ジャ" "ｼﾞｬ")
                (uim '(ustr-cursor-backside ustrj-f)))
  (assert-equal 5
                (uim '(ustr-length ustrj-f)))
  (assert-equal 5
                (uim '(ustr-cursor-pos ustrj-f)))
  (uim-eval '(ustr-prepend! ustrj-f '(("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ")
                                      ("よ" "ヨ" "ﾖ"))))
  (assert-equal '("じゃ" "ジャ" "ｼﾞｬ")
                (uim '(ustr-cursor-backside ustrj-f)))
  (assert-equal 8
                (uim '(ustr-length ustrj-f)))
  (assert-equal 8
                (uim '(ustr-cursor-pos ustrj-f)))
  (assert-equal '(("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ") ("よ" "ヨ" "ﾖ")
                  ("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                  ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
                (uim '(ustr-whole-seq ustrj-f)))
  ;; latter
  (assert-error (lambda ()
                  (uim '(ustr-cursor-backside ustrj-l))))
  (assert-equal 5
                (uim '(ustr-length ustrj-l)))
  (assert-equal 0
                (uim '(ustr-cursor-pos ustrj-l)))
  (uim-eval '(ustr-prepend! ustrj-l '(("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ")
                                      ("よ" "ヨ" "ﾖ"))))
  (assert-equal '("よ" "ヨ" "ﾖ")
                (uim '(ustr-cursor-backside ustrj-l)))
  (assert-equal 8
                (uim '(ustr-length ustrj-l)))
  (assert-equal 3
                (uim '(ustr-cursor-pos ustrj-l)))
  (assert-equal '(("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ") ("よ" "ヨ" "ﾖ")
                  ("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                  ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
                (uim '(ustr-whole-seq ustrj-l)))
  ;; empty
  (assert-error (lambda ()
                  (uim '(ustr-cursor-backside ustre))))
  (assert-equal 0
                (uim '(ustr-length ustre)))
  (assert-equal 0
                (uim '(ustr-cursor-pos ustre)))
  (uim-eval '(ustr-prepend! ustre '(("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ")
                                    ("よ" "ヨ" "ﾖ"))))
  (assert-equal '("よ" "ヨ" "ﾖ")
                (uim '(ustr-cursor-backside ustre)))
  (assert-equal 3
                (uim '(ustr-length ustre)))
  (assert-equal 3
                (uim '(ustr-cursor-pos ustre)))
  (assert-equal '(("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ") ("よ" "ヨ" "ﾖ"))
                (uim '(ustr-whole-seq ustre))))

(define (test-ustr-prepend!-#2)
  ;; former-latter
  (assert-equal '("ん" "ン" "ﾝ")
                (uim '(ustr-cursor-backside ustrj-fl)))
  (assert-equal 5
                (uim '(ustr-length ustrj-fl)))
  (assert-equal 3
                (uim '(ustr-cursor-pos ustrj-fl)))
  (uim-eval '(ustr-prepend! ustrj-fl ()))
  (assert-equal '("ん" "ン" "ﾝ")
                (uim '(ustr-cursor-backside ustrj-fl)))
  (assert-equal 5
                (uim '(ustr-length ustrj-fl)))
  (assert-equal 3
                (uim '(ustr-cursor-pos ustrj-fl)))
  (assert-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                  ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
                (uim '(ustr-whole-seq ustrj-fl)))
  ;; former
  (assert-equal '("じゃ" "ジャ" "ｼﾞｬ")
                (uim '(ustr-cursor-backside ustrj-f)))
  (assert-equal 5
                (uim '(ustr-length ustrj-f)))
  (assert-equal 5
                (uim '(ustr-cursor-pos ustrj-f)))
  (uim-eval '(ustr-prepend! ustrj-f ()))
  (assert-equal '("じゃ" "ジャ" "ｼﾞｬ")
                (uim '(ustr-cursor-backside ustrj-f)))
  (assert-equal 5
                (uim '(ustr-length ustrj-f)))
  (assert-equal 5
                (uim '(ustr-cursor-pos ustrj-f)))
  (assert-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                  ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
                (uim '(ustr-whole-seq ustrj-f)))
  ;; latter
  (assert-error (lambda ()
                  (uim '(ustr-cursor-backside ustrj-l))))
  (assert-equal 5
                (uim '(ustr-length ustrj-l)))
  (assert-equal 0
                (uim '(ustr-cursor-pos ustrj-l)))
  (uim-eval '(ustr-prepend! ustrj-l ()))
  (assert-error (lambda ()
                  (uim '(ustr-cursor-backside ustrj-l))))
  (assert-equal 5
                (uim '(ustr-length ustrj-l)))
  (assert-equal 0
                (uim '(ustr-cursor-pos ustrj-l)))
  (assert-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                  ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
                (uim '(ustr-whole-seq ustrj-l)))
  ;; empty
  (assert-error (lambda ()
                  (uim '(ustr-cursor-backside ustre))))
  (assert-equal 0
                (uim '(ustr-length ustre)))
  (assert-equal 0
                (uim '(ustr-cursor-pos ustre)))
  (uim-eval '(ustr-prepend! ustre ()))
  (assert-error (lambda ()
                  (uim '(ustr-cursor-backside ustre))))
  (assert-equal 0
                (uim '(ustr-length ustre)))
  (assert-equal 0
                (uim '(ustr-cursor-pos ustre))))

(define (test-map-ustr-whole)
  ;; former-latter
  (assert-equal '("h" "e" "l" "l" "o")
                (uim '(map-ustr-whole car ustr-fl)))
  (assert-equal '("H" "E" "L" "L" "O")
                (uim '(map-ustr-whole cdr ustr-fl)))
  (assert-equal '("に" "ほ" "ん" "ご" "じゃ")
                (uim '(map-ustr-whole ja-kana-hiragana ustrj-fl)))
  (assert-equal '("ニ" "ホ" "ン" "ゴ" "ジャ")
                (uim '(map-ustr-whole ja-kana-katakana ustrj-fl)))
  (assert-equal '("ﾆ" "ﾎ" "ﾝ" "ｺﾞ" "ｼﾞｬ")
                (uim '(map-ustr-whole ja-kana-hankaku ustrj-fl)))
  ;; former
  (assert-equal '("h" "e" "l" "l" "o")
                (uim '(map-ustr-whole car ustr-f)))
  (assert-equal '("H" "E" "L" "L" "O")
                (uim '(map-ustr-whole cdr ustr-f)))
  (assert-equal '("に" "ほ" "ん" "ご" "じゃ")
                (uim '(map-ustr-whole ja-kana-hiragana ustrj-f)))
  (assert-equal '("ニ" "ホ" "ン" "ゴ" "ジャ")
                (uim '(map-ustr-whole ja-kana-katakana ustrj-f)))
  (assert-equal '("ﾆ" "ﾎ" "ﾝ" "ｺﾞ" "ｼﾞｬ")
                (uim '(map-ustr-whole ja-kana-hankaku ustrj-f)))
  ;; latter
  (assert-equal '("h" "e" "l" "l" "o")
                (uim '(map-ustr-whole car ustr-l)))
  (assert-equal '("H" "E" "L" "L" "O")
                (uim '(map-ustr-whole cdr ustr-l)))
  (assert-equal '("に" "ほ" "ん" "ご" "じゃ")
                (uim '(map-ustr-whole ja-kana-hiragana ustrj-l)))
  (assert-equal '("ニ" "ホ" "ン" "ゴ" "ジャ")
                (uim '(map-ustr-whole ja-kana-katakana ustrj-l)))
  (assert-equal '("ﾆ" "ﾎ" "ﾝ" "ｺﾞ" "ｼﾞｬ")
                (uim '(map-ustr-whole ja-kana-hankaku ustrj-l)))
  ;; empty
  (assert-equal '()
                (uim '(map-ustr-whole cdr ustre))))

(define (test-map-ustr-former)
  ;; former-latter
  (assert-equal '("h" "e" "l")
                (uim '(map-ustr-former car ustr-fl)))
  (assert-equal '("H" "E" "L")
                (uim '(map-ustr-former cdr ustr-fl)))
  (assert-equal '("に" "ほ" "ん")
                (uim '(map-ustr-former ja-kana-hiragana ustrj-fl)))
  (assert-equal '("ニ" "ホ" "ン")
                (uim '(map-ustr-former ja-kana-katakana ustrj-fl)))
  (assert-equal '("ﾆ" "ﾎ" "ﾝ")
                (uim '(map-ustr-former ja-kana-hankaku ustrj-fl)))
  ;; former
  (assert-equal '("h" "e" "l" "l" "o")
                (uim '(map-ustr-former car ustr-f)))
  (assert-equal '("H" "E" "L" "L" "O")
                (uim '(map-ustr-former cdr ustr-f)))
  (assert-equal '("に" "ほ" "ん" "ご" "じゃ")
                (uim '(map-ustr-former ja-kana-hiragana ustrj-f)))
  (assert-equal '("ニ" "ホ" "ン" "ゴ" "ジャ")
                (uim '(map-ustr-former ja-kana-katakana ustrj-f)))
  (assert-equal '("ﾆ" "ﾎ" "ﾝ" "ｺﾞ" "ｼﾞｬ")
                (uim '(map-ustr-former ja-kana-hankaku ustrj-f)))
  ;; latter
  (assert-equal '()
                (uim '(map-ustr-former car ustr-l)))
  (assert-equal '()
                (uim '(map-ustr-former cdr ustr-l)))
  (assert-equal '()
                (uim '(map-ustr-former ja-kana-hiragana ustrj-l)))
  (assert-equal '()
                (uim '(map-ustr-former ja-kana-katakana ustrj-l)))
  (assert-equal '()
                (uim '(map-ustr-former ja-kana-hankaku ustrj-l)))
  ;; empty
  (assert-equal '()
                (uim '(map-ustr-former cdr ustre))))

(define (test-map-ustr-latter)
  ;; former-latter
  (assert-equal '("l" "o")
                (uim '(map-ustr-latter car ustr-fl)))
  (assert-equal '("L" "O")
                (uim '(map-ustr-latter cdr ustr-fl)))
  (assert-equal '("ご" "じゃ")
                (uim '(map-ustr-latter ja-kana-hiragana ustrj-fl)))
  (assert-equal '("ゴ" "ジャ")
                (uim '(map-ustr-latter ja-kana-katakana ustrj-fl)))
  (assert-equal '("ｺﾞ" "ｼﾞｬ")
                (uim '(map-ustr-latter ja-kana-hankaku ustrj-fl)))
  ;; former
  (assert-equal '()
                (uim '(map-ustr-latter car ustr-f)))
  (assert-equal '()
                (uim '(map-ustr-latter cdr ustr-f)))
  (assert-equal '()
                (uim '(map-ustr-latter ja-kana-hiragana ustrj-f)))
  (assert-equal '()
                (uim '(map-ustr-latter ja-kana-katakana ustrj-f)))
  (assert-equal '()
                (uim '(map-ustr-latter ja-kana-hankaku ustrj-f)))
  ;; latter
  (assert-equal '("h" "e" "l" "l" "o")
                (uim '(map-ustr-latter car ustr-l)))
  (assert-equal '("H" "E" "L" "L" "O")
                (uim '(map-ustr-latter cdr ustr-l)))
  (assert-equal '("に" "ほ" "ん" "ご" "じゃ")
                (uim '(map-ustr-latter ja-kana-hiragana ustrj-l)))
  (assert-equal '("ニ" "ホ" "ン" "ゴ" "ジャ")
                (uim '(map-ustr-latter ja-kana-katakana ustrj-l)))
  (assert-equal '("ﾆ" "ﾎ" "ﾝ" "ｺﾞ" "ｼﾞｬ")
                (uim '(map-ustr-latter ja-kana-hankaku ustrj-l)))
  ;; empty
  (assert-equal '()
                (uim '(map-ustr-latter cdr ustre))))

(define (test-append-map-ustr-whole)
  (assert-equal '("ニ" "ﾆ" "ホ" "ﾎ" "ン" "ﾝ" "ゴ" "ｺﾞ" "ジャ" "ｼﾞｬ")
                (uim '(append-map-ustr-whole cdr ustrj-fl)))
  (assert-equal '("ニ" "ﾆ" "ホ" "ﾎ" "ン" "ﾝ" "ゴ" "ｺﾞ" "ジャ" "ｼﾞｬ")
                (uim '(append-map-ustr-whole cdr ustrj-f)))
  (assert-equal '("ニ" "ﾆ" "ホ" "ﾎ" "ン" "ﾝ" "ゴ" "ｺﾞ" "ジャ" "ｼﾞｬ")
                (uim '(append-map-ustr-whole cdr ustrj-l)))
  (assert-equal '()
                (uim '(append-map-ustr-whole cdr ustre))))

(define (test-append-map-ustr-former)
  (assert-equal '("ニ" "ﾆ" "ホ" "ﾎ" "ン" "ﾝ")
                (uim '(append-map-ustr-former cdr ustrj-fl)))
  (assert-equal '("ニ" "ﾆ" "ホ" "ﾎ" "ン" "ﾝ" "ゴ" "ｺﾞ" "ジャ" "ｼﾞｬ")
                (uim '(append-map-ustr-former cdr ustrj-f)))
  (assert-equal '()
                (uim '(append-map-ustr-former cdr ustrj-l)))
  (assert-equal '()
                (uim '(append-map-ustr-former cdr ustre))))

(define (test-append-map-ustr-latter)
  (assert-equal '("ゴ" "ｺﾞ" "ジャ" "ｼﾞｬ")
                (uim '(append-map-ustr-latter cdr ustrj-fl)))
  (assert-equal '()
                (uim '(append-map-ustr-latter cdr ustrj-f)))
  (assert-equal '("ニ" "ﾆ" "ホ" "ﾎ" "ン" "ﾝ" "ゴ" "ｺﾞ" "ジャ" "ｼﾞｬ")
                (uim '(append-map-ustr-latter cdr ustrj-l)))
  (assert-equal '()
                (uim '(append-map-ustr-latter cdr ustre))))

(define (test-string-append-map-ustr-whole)
  ;; former-latter
  (assert-equal "hello"
                (uim '(string-append-map-ustr-whole car ustr-fl)))
  (assert-equal "HELLO"
                (uim '(string-append-map-ustr-whole cdr ustr-fl)))
  (assert-equal "にほんごじゃ"
                (uim '(string-append-map-ustr-whole ja-kana-hiragana
                                                    ustrj-fl)))
  (assert-equal "ニホンゴジャ"
                (uim '(string-append-map-ustr-whole ja-kana-katakana
                                                    ustrj-fl)))
  (assert-equal "ﾆﾎﾝｺﾞｼﾞｬ"
                (uim '(string-append-map-ustr-whole ja-kana-hankaku
                                                    ustrj-fl)))
  ;; former
  (assert-equal "hello"
                (uim '(string-append-map-ustr-whole car ustr-f)))
  (assert-equal "HELLO"
                (uim '(string-append-map-ustr-whole cdr ustr-f)))
  (assert-equal "にほんごじゃ"
                (uim '(string-append-map-ustr-whole ja-kana-hiragana
                                                    ustrj-f)))
  (assert-equal "ニホンゴジャ"
                (uim '(string-append-map-ustr-whole ja-kana-katakana
                                                    ustrj-f)))
  (assert-equal "ﾆﾎﾝｺﾞｼﾞｬ"
                (uim '(string-append-map-ustr-whole ja-kana-hankaku
                                                    ustrj-f)))
  ;; latter
  (assert-equal "hello"
                (uim '(string-append-map-ustr-whole car ustr-l)))
  (assert-equal "HELLO"
                (uim '(string-append-map-ustr-whole cdr ustr-l)))
  (assert-equal "にほんごじゃ"
                (uim '(string-append-map-ustr-whole ja-kana-hiragana
                                                    ustrj-l)))
  (assert-equal "ニホンゴジャ"
                (uim '(string-append-map-ustr-whole ja-kana-katakana
                                                    ustrj-l)))
  (assert-equal "ﾆﾎﾝｺﾞｼﾞｬ"
                (uim '(string-append-map-ustr-whole ja-kana-hankaku
                                                    ustrj-l)))
  ;; empty
  (assert-equal ""
                (uim '(string-append-map-ustr-whole cdr ustre))))

(define (test-string-append-map-ustr-former)
  ;; former-latter
  (assert-equal "hel"
                (uim '(string-append-map-ustr-former car ustr-fl)))
  (assert-equal "HEL"
                (uim '(string-append-map-ustr-former cdr ustr-fl)))
  (assert-equal "にほん"
                (uim '(string-append-map-ustr-former ja-kana-hiragana
                                                     ustrj-fl)))
  (assert-equal "ニホン"
                (uim '(string-append-map-ustr-former ja-kana-katakana
                                                     ustrj-fl)))
  (assert-equal "ﾆﾎﾝ"
                (uim '(string-append-map-ustr-former ja-kana-hankaku
                                                     ustrj-fl)))
  ;; former
  (assert-equal "hello"
                (uim '(string-append-map-ustr-former car ustr-f)))
  (assert-equal "HELLO"
                (uim '(string-append-map-ustr-former cdr ustr-f)))
  (assert-equal "にほんごじゃ"
                (uim '(string-append-map-ustr-former ja-kana-hiragana
                                                     ustrj-f)))
  (assert-equal "ニホンゴジャ"
                (uim '(string-append-map-ustr-former ja-kana-katakana
                                                     ustrj-f)))
  (assert-equal "ﾆﾎﾝｺﾞｼﾞｬ"
                (uim '(string-append-map-ustr-former ja-kana-hankaku
                                                     ustrj-f)))
  ;; latter
  (assert-equal ""
                (uim '(string-append-map-ustr-former car ustr-l)))
  (assert-equal ""
                (uim '(string-append-map-ustr-former cdr ustr-l)))
  (assert-equal ""
                (uim '(string-append-map-ustr-former ja-kana-hiragana
                                                     ustrj-l)))
  (assert-equal ""
                (uim '(string-append-map-ustr-former ja-kana-katakana
                                                     ustrj-l)))
  (assert-equal ""
                (uim '(string-append-map-ustr-former ja-kana-hankaku
                                                     ustrj-l)))
  ;; empty
  (assert-equal ""
                (uim '(string-append-map-ustr-former cdr ustre))))

(define (test-string-append-map-ustr-latter)
  ;; former-latter
  (assert-equal "lo"
                (uim '(string-append-map-ustr-latter car ustr-fl)))
  (assert-equal "LO"
                (uim '(string-append-map-ustr-latter cdr ustr-fl)))
  (assert-equal "ごじゃ"
                (uim '(string-append-map-ustr-latter ja-kana-hiragana
                                                     ustrj-fl)))
  (assert-equal "ゴジャ"
                (uim '(string-append-map-ustr-latter ja-kana-katakana
                                                     ustrj-fl)))
  (assert-equal "ｺﾞｼﾞｬ"
                (uim '(string-append-map-ustr-latter ja-kana-hankaku
                                                     ustrj-fl)))
  ;; former
  (assert-equal ""
                (uim '(string-append-map-ustr-latter car ustr-f)))
  (assert-equal ""
                (uim '(string-append-map-ustr-latter cdr ustr-f)))
  (assert-equal ""
                (uim '(string-append-map-ustr-latter ja-kana-hiragana
                                                     ustrj-f)))
  (assert-equal ""
                (uim '(string-append-map-ustr-latter ja-kana-katakana
                                                     ustrj-f)))
  (assert-equal ""
                (uim '(string-append-map-ustr-latter ja-kana-hankaku
                                                     ustrj-f)))
  ;; latter
  (assert-equal "hello"
                (uim '(string-append-map-ustr-latter car ustr-l)))
  (assert-equal "HELLO"
                (uim '(string-append-map-ustr-latter cdr ustr-l)))
  (assert-equal "にほんごじゃ"
                (uim '(string-append-map-ustr-latter ja-kana-hiragana
                                                     ustrj-l)))
  (assert-equal "ニホンゴジャ"
                (uim '(string-append-map-ustr-latter ja-kana-katakana
                                                     ustrj-l)))
  (assert-equal "ﾆﾎﾝｺﾞｼﾞｬ"
                (uim '(string-append-map-ustr-latter ja-kana-hankaku
                                                     ustrj-l)))
  ;; empty
  (assert-equal ""
                (uim '(string-append-map-ustr-latter cdr ustre))))

(define (test-ustr-cursor-at-beginning?)
  (assert-false (uim-bool '(ustr-cursor-at-beginning? ustra-fl)))
  (assert-false (uim-bool '(ustr-cursor-at-beginning? ustra-f)))
  (assert-true  (uim-bool '(ustr-cursor-at-beginning? ustra-l)))
  (assert-true  (uim-bool '(ustr-cursor-at-beginning? ustre))))

(define (test-ustr-cursor-at-end?)
  (assert-false (uim-bool '(ustr-cursor-at-end? ustra-fl)))
  (assert-true  (uim-bool '(ustr-cursor-at-end? ustra-f)))
  (assert-false (uim-bool '(ustr-cursor-at-end? ustra-l)))
  (assert-true  (uim-bool '(ustr-cursor-at-end? ustre))))

(define (test-ustr-cursor-pos)
  (assert-equal 3
                (uim '(ustr-cursor-pos ustr-fl)))
  (assert-equal 5
                (uim '(ustr-cursor-pos ustr-f)))
  (assert-equal 0
                (uim '(ustr-cursor-pos ustr-l)))

  (assert-equal 3
                (uim '(ustr-cursor-pos ustrj-fl)))
  (assert-equal 5
                (uim '(ustr-cursor-pos ustrj-f)))
  (assert-equal 0
                (uim '(ustr-cursor-pos ustrj-l)))

  (assert-equal 3
                (uim '(ustr-cursor-pos ustra-fl)))
  (assert-equal 5
                (uim '(ustr-cursor-pos ustra-f)))
  (assert-equal 0
                (uim '(ustr-cursor-pos ustra-l)))

  (assert-equal 0
                (uim '(ustr-cursor-pos ustre))))

(define (test-ustr-set-cursor-pos!)
  ;; former-latter
  (assert-equal 3
                (uim '(ustr-cursor-pos ustra-fl)))
  (assert-true  (uim-bool '(ustr-set-cursor-pos! ustra-fl 0)))
  (assert-equal 0
                (uim '(ustr-cursor-pos ustra-fl)))
  (assert-true  (uim-bool '(ustr-set-cursor-pos! ustra-fl 5)))
  (assert-equal 5
                (uim '(ustr-cursor-pos ustra-fl)))
  (assert-true  (uim-bool '(ustr-set-cursor-pos! ustra-fl 2)))
  (assert-equal 2
                (uim '(ustr-cursor-pos ustra-fl)))
  (assert-false (uim-bool '(ustr-set-cursor-pos! ustra-fl -1)))
  (assert-false (uim-bool '(ustr-set-cursor-pos! ustra-fl 6)))
  ;; empty
  (assert-equal 0
                (uim '(ustr-cursor-pos ustre)))
  (assert-true  (uim-bool '(ustr-set-cursor-pos! ustra-fl 0)))
  (assert-equal 0
                (uim '(ustr-cursor-pos ustre)))
  (assert-false (uim-bool '(ustr-set-cursor-pos! ustre -1)))
  (assert-false (uim-bool '(ustr-set-cursor-pos! ustre 1))))

(define (test-ustr-cursor-move!)
  (assert-equal 3
                (uim '(ustr-cursor-pos ustra-fl)))
  (assert-false (uim-bool '(ustr-cursor-move! ustra-fl -4)))
  (assert-equal 3
                (uim '(ustr-cursor-pos ustra-fl)))

  (assert-false (uim-bool '(ustr-cursor-move! ustra-fl 3)))
  (assert-equal 3
                (uim '(ustr-cursor-pos ustra-fl)))

  (assert-true  (uim-bool '(ustr-cursor-move! ustra-fl 2)))
  (assert-equal 5
                (uim '(ustr-cursor-pos ustra-fl)))

  (assert-true  (uim-bool '(ustr-cursor-move! ustra-fl -3)))
  (assert-equal 2
                (uim '(ustr-cursor-pos ustra-fl)))

  (assert-true  (uim-bool '(ustr-cursor-move! ustra-fl 1)))
  (assert-equal 3
                (uim '(ustr-cursor-pos ustra-fl)))

  (assert-true  (uim-bool '(ustr-cursor-move! ustra-fl 0)))
  (assert-equal 3
                (uim '(ustr-cursor-pos ustra-fl)))

  (assert-true  (uim-bool '(ustr-cursor-move! ustra-fl -3)))
  (assert-equal 0
                (uim '(ustr-cursor-pos ustra-fl)))

  (assert-equal 0
                (uim '(ustr-cursor-pos ustre)))
  (assert-true  (uim-bool '(ustr-cursor-move! ustra-fl 0)))
  (assert-equal 0
                (uim '(ustr-cursor-pos ustre)))

  (assert-false (uim-bool '(ustr-cursor-move! ustre -1)))
  (assert-false (uim-bool '(ustr-cursor-move! ustre 1))))

(define (test-ustr-cursor-move-backward!)
  (assert-equal 3
                (uim '(ustr-cursor-pos ustra-fl)))

  (uim-eval '(ustr-cursor-move-backward! ustra-fl))
  (assert-equal 2
                (uim '(ustr-cursor-pos ustra-fl)))

  (uim-eval '(ustr-cursor-move-backward! ustra-fl))
  (assert-equal 1
                (uim '(ustr-cursor-pos ustra-fl)))

  (uim-eval '(ustr-cursor-move-backward! ustra-fl))
  (assert-equal 0
                (uim '(ustr-cursor-pos ustra-fl)))

  (uim-eval '(ustr-cursor-move-backward! ustra-fl))
  (assert-equal 0
                (uim '(ustr-cursor-pos ustra-fl)))
  ;; start from end of string
  (assert-equal 5
                (uim '(ustr-cursor-pos ustra-f)))
  (uim-eval '(ustr-cursor-move-backward! ustra-f))
  (assert-equal 4
                (uim '(ustr-cursor-pos ustra-f))))

(define (test-ustr-cursor-move-forward!)
  (assert-equal 3
                (uim '(ustr-cursor-pos ustra-fl)))

  (uim-eval '(ustr-cursor-move-forward! ustra-fl))
  (assert-equal 4
                (uim '(ustr-cursor-pos ustra-fl)))

  (uim-eval '(ustr-cursor-move-forward! ustra-fl))
  (assert-equal 5
                (uim '(ustr-cursor-pos ustra-fl)))

  (uim-eval '(ustr-cursor-move-forward! ustra-fl))
  (assert-equal 5
                (uim '(ustr-cursor-pos ustra-fl)))

  ;; start from beginning of string
  (assert-equal 0
                (uim '(ustr-cursor-pos ustra-l)))
  (uim-eval '(ustr-cursor-move-forward! ustra-l))
  (assert-equal 1
                (uim '(ustr-cursor-pos ustra-l))))

(define (test-ustr-cursor-move-beginning!)
  (assert-equal 3
                (uim '(ustr-cursor-pos ustra-fl)))

  (uim-eval '(ustr-cursor-move-beginning! ustra-fl))
  (assert-equal 0
                (uim '(ustr-cursor-pos ustra-fl)))

  (uim-eval '(ustr-cursor-move-beginning! ustra-fl))
  (assert-equal 0
                (uim '(ustr-cursor-pos ustra-fl)))

  ;; start from end of string
  (assert-equal 5
                (uim '(ustr-cursor-pos ustra-f)))
  (uim-eval '(ustr-cursor-move-beginning! ustra-f))
  (assert-equal 0
                (uim '(ustr-cursor-pos ustra-f))))

(define (test-ustr-cursor-move-end!)
  (assert-equal 3
                (uim '(ustr-cursor-pos ustra-fl)))

  (uim-eval '(ustr-cursor-move-end! ustra-fl))
  (assert-equal 5
                (uim '(ustr-cursor-pos ustra-fl)))

  (uim-eval '(ustr-cursor-move-end! ustra-fl))
  (assert-equal 5
                (uim '(ustr-cursor-pos ustra-fl)))

  ;; start from beginning of string
  (assert-equal 0
                (uim '(ustr-cursor-pos ustra-l)))
  (uim-eval '(ustr-cursor-move-end! ustra-l))
  (assert-equal 5
                (uim '(ustr-cursor-pos ustra-l))))

(define (test-ustr-cursor-frontside)
  ;; former-latter
  (assert-equal '("l" . "L")
                (uim '(ustr-cursor-frontside ustr-fl)))
  (assert-equal '("ご" "ゴ" "ｺﾞ")
                (uim '(ustr-cursor-frontside ustrj-fl)))
  (assert-equal "l"
                (uim '(ustr-cursor-frontside ustra-fl)))
  ;; former
  (assert-error (lambda ()
                  (uim '(ustr-cursor-frontside ustr-f))))
  (assert-error (lambda ()
                  (uim '(ustr-cursor-frontside ustrj-f))))
  (assert-error (lambda ()
                  (uim '(ustr-cursor-frontside ustra-f))))
  ;; latter
  (assert-equal '("h" . "H")
                (uim '(ustr-cursor-frontside ustr-l)))
  (assert-equal '("に" "ニ" "ﾆ")
                (uim '(ustr-cursor-frontside ustrj-l)))
  (assert-equal "h"
                (uim '(ustr-cursor-frontside ustra-l)))
  ;; empty
  (assert-error (lambda ()
                  (uim '(ustr-cursor-frontside ustre)))))

(define (test-ustr-cursor-backside)
  ;; former-latter
  (assert-equal '("l" . "L")
                (uim '(ustr-cursor-backside ustr-fl)))
  (assert-equal '("ん" "ン" "ﾝ")
                (uim '(ustr-cursor-backside ustrj-fl)))
  (assert-equal "l"
                (uim '(ustr-cursor-backside ustra-fl)))
  ;; former
  (assert-equal '("o" . "O")
                (uim '(ustr-cursor-backside ustr-f)))
  (assert-equal '("じゃ" "ジャ" "ｼﾞｬ")
                (uim '(ustr-cursor-backside ustrj-f)))
  (assert-equal "o"
                (uim '(ustr-cursor-backside ustra-f)))
  ;; latter
  (assert-error (lambda ()
                  (uim '(ustr-cursor-backside ustr-l))))
  (assert-error (lambda ()
                  (uim '(ustr-cursor-backside ustrj-l))))
  (assert-error (lambda ()
                  (uim '(ustr-cursor-backside ustra-l))))
  ;; empty
  (assert-error (lambda ()
                  (uim '(ustr-cursor-backside ustre)))))

(define (test-ustr-cursor-delete-frontside!)
  ;; former-latter
  (assert-equal '("ご" "ゴ" "ｺﾞ")
                (uim '(ustr-cursor-frontside ustrj-fl)))
  (assert-equal 5
                (uim '(ustr-length ustrj-fl)))
  (assert-equal 3
                (uim '(ustr-cursor-pos ustrj-fl)))
  (assert-true  (uim-bool '(ustr-cursor-delete-frontside! ustrj-fl)))
  (assert-equal '("じゃ" "ジャ" "ｼﾞｬ")
                (uim '(ustr-cursor-frontside ustrj-fl)))
  (assert-equal 4
                (uim '(ustr-length ustrj-fl)))
  (assert-equal 3
                (uim '(ustr-cursor-pos ustrj-fl)))
  (assert-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                  ("じゃ" "ジャ" "ｼﾞｬ"))
                (uim '(ustr-whole-seq ustrj-fl)))
  ;; former
  (assert-error (lambda ()
                  (uim '(ustr-cursor-frontside ustrj-f))))
  (assert-equal 5
                (uim '(ustr-length ustrj-f)))
  (assert-equal 5
                (uim '(ustr-cursor-pos ustrj-f)))
  (assert-false (uim-bool '(ustr-cursor-delete-frontside! ustrj-f)))
  (assert-error (lambda ()
                  (uim '(ustr-cursor-frontside ustrj-f))))
  (assert-equal 5
                (uim '(ustr-length ustrj-f)))
  (assert-equal 5
                (uim '(ustr-cursor-pos ustrj-f)))
  (assert-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                  ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
                (uim '(ustr-whole-seq ustrj-f)))
  ;; latter
  (assert-equal '("に" "ニ" "ﾆ")
                (uim '(ustr-cursor-frontside ustrj-l)))
  (assert-equal 5
                (uim '(ustr-length ustrj-l)))
  (assert-equal 0
                (uim '(ustr-cursor-pos ustrj-l)))
  (assert-true  (uim-bool '(ustr-cursor-delete-frontside! ustrj-l)))
  (assert-equal '("ほ" "ホ" "ﾎ")
                (uim '(ustr-cursor-frontside ustrj-l)))
  (assert-equal 4
                (uim '(ustr-length ustrj-l)))
  (assert-equal 0
                (uim '(ustr-cursor-pos ustrj-l)))
  (assert-equal '(("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                  ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
                (uim '(ustr-whole-seq ustrj-l)))
  ;; empty
  (assert-error (lambda ()
                  (uim '(ustr-cursor-frontside ustre))))
  (assert-equal 0
                (uim '(ustr-length ustre)))
  (assert-equal 0
                (uim '(ustr-cursor-pos ustre)))
  (assert-false (uim-bool '(ustr-cursor-delete-frontside! ustre)))
  (assert-error (lambda ()
                  (uim '(ustr-cursor-frontside ustre))))
  (assert-equal 0
                (uim '(ustr-length ustre)))
  (assert-equal 0
                (uim '(ustr-cursor-pos ustre))))

(define (test-ustr-cursor-delete-backside!)
  ;; former-latter
  (assert-equal '("ん" "ン" "ﾝ")
                (uim '(ustr-cursor-backside ustrj-fl)))
  (assert-equal 5
                (uim '(ustr-length ustrj-fl)))
  (assert-equal 3
                (uim '(ustr-cursor-pos ustrj-fl)))
  (assert-true  (uim-bool '(ustr-cursor-delete-backside! ustrj-fl)))
  (assert-equal '("ほ" "ホ" "ﾎ")
                (uim '(ustr-cursor-backside ustrj-fl)))
  (assert-equal 4
                (uim '(ustr-length ustrj-fl)))
  (assert-equal 2
                (uim '(ustr-cursor-pos ustrj-fl)))
  (assert-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ")
                  ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
                (uim '(ustr-whole-seq ustrj-fl)))
  ;; former
  (assert-equal '("じゃ" "ジャ" "ｼﾞｬ")
                (uim '(ustr-cursor-backside ustrj-f)))
  (assert-equal 5
                (uim '(ustr-length ustrj-f)))
  (assert-equal 5
                (uim '(ustr-cursor-pos ustrj-f)))
  (assert-true  (uim-bool '(ustr-cursor-delete-backside! ustrj-f)))
  (assert-equal '("ご" "ゴ" "ｺﾞ")
                (uim '(ustr-cursor-backside ustrj-f)))
  (assert-equal 4
                (uim '(ustr-length ustrj-f)))
  (assert-equal 4
                (uim '(ustr-cursor-pos ustrj-f)))
  (assert-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                  ("ご" "ゴ" "ｺﾞ"))
                (uim '(ustr-whole-seq ustrj-f)))
  ;; latter
  (assert-error (lambda ()
                  (uim '(ustr-cursor-backside ustrj-l))))
  (assert-equal 5
                (uim '(ustr-length ustrj-l)))
  (assert-equal 0
                (uim '(ustr-cursor-pos ustrj-l)))
  (assert-false (uim-bool '(ustr-cursor-delete-backside! ustrj-l)))
  (assert-error (lambda ()
                  (uim '(ustr-cursor-backside ustrj-l))))
  (assert-equal 5
                (uim '(ustr-length ustrj-l)))
  (assert-equal 0
                (uim '(ustr-cursor-pos ustrj-l)))
  (assert-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                  ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
                (uim '(ustr-whole-seq ustrj-l)))
  ;; empty
  (assert-error (lambda ()
                  (uim '(ustr-cursor-backside ustre))))
  (assert-equal 0
                (uim '(ustr-length ustre)))
  (assert-false (uim-bool '(ustr-cursor-delete-backside! ustre)))
  (assert-error (lambda ()
                  (uim '(ustr-cursor-backside ustre))))
  (assert-equal 0
                (uim '(ustr-length ustre))))

(define (test-ustr-insert-elem!)
  ;; former-latter
  (assert-equal '("ん" "ン" "ﾝ")
                (uim '(ustr-cursor-backside ustrj-fl)))
  (assert-equal 5
                (uim '(ustr-length ustrj-fl)))
  (assert-equal 3
                (uim '(ustr-cursor-pos ustrj-fl)))
  (uim-eval '(ustr-insert-elem! ustrj-fl '("んー" "ンー" "ﾝｰ")))
  (assert-equal '("んー" "ンー" "ﾝｰ")
                (uim '(ustr-cursor-backside ustrj-fl)))
  (assert-equal 6
                (uim '(ustr-length ustrj-fl)))
  (assert-equal 4
                (uim '(ustr-cursor-pos ustrj-fl)))
  (assert-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                  ("んー" "ンー" "ﾝｰ")
                  ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
                (uim '(ustr-whole-seq ustrj-fl)))
  ;; former
  (assert-equal '("じゃ" "ジャ" "ｼﾞｬ")
                (uim '(ustr-cursor-backside ustrj-f)))
  (assert-equal 5
                (uim '(ustr-length ustrj-f)))
  (assert-equal 5
                (uim '(ustr-cursor-pos ustrj-f)))
  (uim-eval '(ustr-insert-elem! ustrj-f '("んー" "ンー" "ﾝｰ")))
  (assert-equal '("んー" "ンー" "ﾝｰ")
                (uim '(ustr-cursor-backside ustrj-f)))
  (assert-equal 6
                (uim '(ustr-length ustrj-f)))
  (assert-equal 6
                (uim '(ustr-cursor-pos ustrj-f)))
  (assert-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                  ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ") ("んー" "ンー" "ﾝｰ"))
                (uim '(ustr-whole-seq ustrj-f)))
  ;; latter
  (assert-error (lambda ()
                  (uim '(ustr-cursor-backside ustrj-l))))
  (assert-equal 5
                (uim '(ustr-length ustrj-l)))
  (assert-equal 0
                (uim '(ustr-cursor-pos ustrj-l)))
  (uim-eval '(ustr-insert-elem! ustrj-l '("んー" "ンー" "ﾝｰ")))
  (assert-equal '("んー" "ンー" "ﾝｰ")
                (uim '(ustr-cursor-backside ustrj-l)))
  (assert-equal 6
                (uim '(ustr-length ustrj-l)))
  (assert-equal 1
                (uim '(ustr-cursor-pos ustrj-l)))
  (assert-equal '(("んー" "ンー" "ﾝｰ")
                  ("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                  ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
                (uim '(ustr-whole-seq ustrj-l)))
  ;; empty
  (assert-error (lambda ()
                  (uim '(ustr-cursor-backside ustre))))
  (assert-equal 0
                (uim '(ustr-length ustre)))
  (uim-eval '(ustr-insert-elem! ustre '("んー" "ンー" "ﾝｰ")))
  (assert-equal '("んー" "ンー" "ﾝｰ")
                (uim '(ustr-cursor-backside ustre)))
  (assert-equal 1
                (uim '(ustr-length ustre)))
  (assert-equal '(("んー" "ンー" "ﾝｰ"))
                (uim '(ustr-whole-seq ustre))))

(define (test-ustr-cursor-set-frontside!)
  ;; former-latter
  (assert-equal '("ご" "ゴ" "ｺﾞ")
                (uim '(ustr-cursor-frontside ustrj-fl)))
  (assert-equal 5
                (uim '(ustr-length ustrj-fl)))
  (assert-equal 3
                (uim '(ustr-cursor-pos ustrj-fl)))
  (assert-true (uim-bool '(ustr-cursor-set-frontside! ustrj-fl
                                                      '("んー" "ンー" "ﾝｰ"))))
  (assert-equal '("んー" "ンー" "ﾝｰ")
                (uim '(ustr-cursor-frontside ustrj-fl)))
  (assert-equal 5
                (uim '(ustr-length ustrj-fl)))
  (assert-equal 3
                (uim '(ustr-cursor-pos ustrj-fl)))
  (assert-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                  ("んー" "ンー" "ﾝｰ") ("じゃ" "ジャ" "ｼﾞｬ"))
                (uim '(ustr-whole-seq ustrj-fl)))
  ;; former
  (assert-error (lambda ()
                  (uim '(ustr-cursor-frontside ustrj-f))))
  (assert-equal 5
                (uim '(ustr-length ustrj-f)))
  (assert-equal 5
                (uim '(ustr-cursor-pos ustrj-f)))
  (assert-false (uim-bool '(ustr-cursor-set-frontside! ustrj-f
                                                       '("んー" "ンー" "ﾝｰ"))))
  (assert-equal 5
                (uim '(ustr-length ustrj-f)))
  (assert-equal 5
                (uim '(ustr-cursor-pos ustrj-f)))
  (assert-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                  ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
                (uim '(ustr-whole-seq ustrj-f)))
  ;; latter
  (assert-equal '("に" "ニ" "ﾆ")
                (uim '(ustr-cursor-frontside ustrj-l)))
  (assert-equal 5
                (uim '(ustr-length ustrj-l)))
  (assert-equal 0
                (uim '(ustr-cursor-pos ustrj-l)))
  (assert-true  (uim-bool '(ustr-cursor-set-frontside! ustrj-l
                                                       '("んー" "ンー" "ﾝｰ"))))
  (assert-equal '("んー" "ンー" "ﾝｰ")
                (uim '(ustr-cursor-frontside ustrj-l)))
  (assert-equal 5
                (uim '(ustr-length ustrj-l)))
  (assert-equal 0
                (uim '(ustr-cursor-pos ustrj-l)))
  (assert-equal '(("んー" "ンー" "ﾝｰ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                  ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
                (uim '(ustr-whole-seq ustrj-l)))
  ;; empty
  (assert-error (lambda ()
                  (uim '(ustr-cursor-frontside ustre))))
  (assert-equal 0
                (uim '(ustr-length ustre)))
  (assert-false (uim-bool '(ustr-cursor-set-frontside! ustre
                                                       '("んー" "ンー" "ﾝｰ"))))
  (assert-error (lambda ()
                  (uim '(ustr-cursor-frontside ustre))))
  (assert-equal 0
                (uim '(ustr-length ustre)))
  (assert-equal '()
                (uim '(ustr-whole-seq ustre))))

(define (test-ustr-cursor-set-backside!)
  ;; former-latter
  (assert-equal '("ん" "ン" "ﾝ")
                (uim '(ustr-cursor-backside ustrj-fl)))
  (assert-equal 5
                (uim '(ustr-length ustrj-fl)))
  (assert-equal 3
                (uim '(ustr-cursor-pos ustrj-fl)))
  (assert-true  (uim-bool '(ustr-cursor-set-backside! ustrj-fl
                                                      '("んー" "ンー" "ﾝｰ"))))
  (assert-equal '("んー" "ンー" "ﾝｰ")
                (uim '(ustr-cursor-backside ustrj-fl)))
  (assert-equal 5
                (uim '(ustr-length ustrj-fl)))
  (assert-equal 3
                (uim '(ustr-cursor-pos ustrj-fl)))
  (assert-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("んー" "ンー" "ﾝｰ")
                  ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
                (uim '(ustr-whole-seq ustrj-fl)))
  ;; former
  (assert-equal '("じゃ" "ジャ" "ｼﾞｬ")
                (uim '(ustr-cursor-backside ustrj-f)))
  (assert-equal 5
                (uim '(ustr-length ustrj-f)))
  (assert-equal 5
                (uim '(ustr-cursor-pos ustrj-f)))
  (assert-true  (uim-bool '(ustr-cursor-set-backside! ustrj-f
                                                      '("んー" "ンー" "ﾝｰ"))))
  (assert-equal '("んー" "ンー" "ﾝｰ")
                (uim '(ustr-cursor-backside ustrj-f)))
  (assert-equal 5
                (uim '(ustr-length ustrj-f)))
  (assert-equal 5
                (uim '(ustr-cursor-pos ustrj-f)))
  (assert-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                  ("ご" "ゴ" "ｺﾞ") ("んー" "ンー" "ﾝｰ"))
                (uim '(ustr-whole-seq ustrj-f)))
  ;; latter
  (assert-error (lambda ()
                  (uim '(ustr-cursor-backside ustrj-l))))
  (assert-equal 5
                (uim '(ustr-length ustrj-l)))
  (assert-equal 0
                (uim '(ustr-cursor-pos ustrj-l)))
  (assert-false (uim-bool '(ustr-cursor-set-backside! ustrj-l
                                                      '("んー" "ンー" "ﾝｰ"))))
  (assert-error (lambda ()
                  (uim '(ustr-cursor-backside ustrj-l))))
  (assert-equal 5
                (uim '(ustr-length ustrj-l)))
  (assert-equal 0
                (uim '(ustr-cursor-pos ustrj-l)))
  (assert-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                  ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
                (uim '(ustr-whole-seq ustrj-l)))
  ;; empty
  (assert-error (lambda ()
                  (uim '(ustr-cursor-backside ustre))))
  (assert-equal 0
                (uim '(ustr-length ustre)))
  (assert-false (uim-bool '(ustr-cursor-set-backside! ustre
                                                      '("んー" "ンー" "ﾝｰ"))))
  (assert-error (lambda ()
                  (uim '(ustr-cursor-backside ustre))))
  (assert-equal 0
                (uim '(ustr-length ustre)))
  (assert-equal '()
                (uim '(ustr-whole-seq ustre))))

(define (test-ustr-insert-seq!)
  ;; former-latter
  (assert-equal '("ん" "ン" "ﾝ")
                (uim '(ustr-cursor-backside ustrj-fl)))
  (assert-equal 5
                (uim '(ustr-length ustrj-fl)))
  (assert-equal 3
                (uim '(ustr-cursor-pos ustrj-fl)))
  (uim-eval '(ustr-insert-seq! ustrj-fl '(("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ")
                                          ("よ" "ヨ" "ﾖ"))))
  (assert-equal '("よ" "ヨ" "ﾖ")
                (uim '(ustr-cursor-backside ustrj-fl)))
  (assert-equal 8
                (uim '(ustr-length ustrj-fl)))
  (assert-equal 6
                (uim '(ustr-cursor-pos ustrj-fl)))
  (assert-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                  ("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ") ("よ" "ヨ" "ﾖ")
                  ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
                (uim '(ustr-whole-seq ustrj-fl)))
  ;; former
  (assert-equal '("じゃ" "ジャ" "ｼﾞｬ")
                (uim '(ustr-cursor-backside ustrj-f)))
  (assert-equal 5
                (uim '(ustr-length ustrj-f)))
  (assert-equal 5
                (uim '(ustr-cursor-pos ustrj-f)))
  (uim-eval '(ustr-insert-seq! ustrj-f '(("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ")
                                         ("よ" "ヨ" "ﾖ"))))
  (assert-equal '("よ" "ヨ" "ﾖ")
                (uim '(ustr-cursor-backside ustrj-f)))
  (assert-equal 8
                (uim '(ustr-length ustrj-f)))
  (assert-equal 8
                (uim '(ustr-cursor-pos ustrj-f)))
  (assert-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                  ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ")
                  ("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ") ("よ" "ヨ" "ﾖ"))
                (uim '(ustr-whole-seq ustrj-f)))
  ;; latter
  (assert-error (lambda ()
                  (uim '(ustr-cursor-backside ustrj-l))))
  (assert-equal 5
                (uim '(ustr-length ustrj-l)))
  (assert-equal 0
                (uim '(ustr-cursor-pos ustrj-l)))
  (uim-eval '(ustr-insert-seq! ustrj-l '(("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ")
                                         ("よ" "ヨ" "ﾖ"))))
  (assert-equal '("よ" "ヨ" "ﾖ")
                (uim '(ustr-cursor-backside ustrj-l)))
  (assert-equal 8
                (uim '(ustr-length ustrj-l)))
  (assert-equal 3
                (uim '(ustr-cursor-pos ustrj-l)))
  (assert-equal '(("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ") ("よ" "ヨ" "ﾖ")
                  ("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                  ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
                (uim '(ustr-whole-seq ustrj-l)))
  ;; empty
  (assert-error (lambda ()
                  (uim '(ustr-cursor-backside ustre))))
  (assert-equal 0
                (uim '(ustr-length ustre)))
  (assert-equal 0
                (uim '(ustr-cursor-pos ustre)))
  (uim-eval '(ustr-insert-seq! ustre '(("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ")
                                       ("よ" "ヨ" "ﾖ"))))
  (assert-equal '("よ" "ヨ" "ﾖ")
                (uim '(ustr-cursor-backside ustre)))
  (assert-equal 3
                (uim '(ustr-length ustre)))
  (assert-equal 3
                (uim '(ustr-cursor-pos ustre)))
  (assert-equal '(("んー" "ンー" "ﾝｰ") ("か" "カ" "ｶ") ("よ" "ヨ" "ﾖ"))
                (uim '(ustr-whole-seq ustre))))

(define (test-ustr-insert-seq!-#2)
  ;; former-latter
  (assert-equal '("ん" "ン" "ﾝ")
                (uim '(ustr-cursor-backside ustrj-fl)))
  (assert-equal 5
                (uim '(ustr-length ustrj-fl)))
  (assert-equal 3
                (uim '(ustr-cursor-pos ustrj-fl)))
  (uim-eval '(ustr-insert-seq! ustrj-fl ()))
  (assert-equal '("ん" "ン" "ﾝ")
                (uim '(ustr-cursor-backside ustrj-fl)))
  (assert-equal 5
                (uim '(ustr-length ustrj-fl)))
  (assert-equal 3
                (uim '(ustr-cursor-pos ustrj-fl)))
  (assert-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                  ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
                (uim '(ustr-whole-seq ustrj-fl)))
  ;; former
  (assert-equal '("じゃ" "ジャ" "ｼﾞｬ")
                (uim '(ustr-cursor-backside ustrj-f)))
  (assert-equal 5
                (uim '(ustr-length ustrj-f)))
  (assert-equal 5
                (uim '(ustr-cursor-pos ustrj-f)))
  (uim-eval '(ustr-insert-seq! ustrj-f ()))
  (assert-equal '("じゃ" "ジャ" "ｼﾞｬ")
                (uim '(ustr-cursor-backside ustrj-f)))
  (assert-equal 5
                (uim '(ustr-length ustrj-f)))
  (assert-equal 5
                (uim '(ustr-cursor-pos ustrj-f)))
  (assert-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                  ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
                (uim '(ustr-whole-seq ustrj-f)))
  ;; latter
  (assert-error (lambda ()
                  (uim '(ustr-cursor-backside ustrj-l))))
  (assert-equal 5
                (uim '(ustr-length ustrj-l)))
  (assert-equal 0
                (uim '(ustr-cursor-pos ustrj-l)))
  (uim-eval '(ustr-insert-seq! ustrj-l ()))
  (assert-error (lambda ()
                  (uim '(ustr-cursor-backside ustrj-l))))
  (assert-equal 5
                (uim '(ustr-length ustrj-l)))
  (assert-equal 0
                (uim '(ustr-cursor-pos ustrj-l)))
  (assert-equal '(("に" "ニ" "ﾆ") ("ほ" "ホ" "ﾎ") ("ん" "ン" "ﾝ")
                  ("ご" "ゴ" "ｺﾞ") ("じゃ" "ジャ" "ｼﾞｬ"))
                (uim '(ustr-whole-seq ustrj-l)))
  ;; empty
  (assert-error (lambda ()
                  (uim '(ustr-cursor-backside ustre))))
  (assert-equal 0
                (uim '(ustr-length ustre)))
  (assert-equal 0
                (uim '(ustr-cursor-pos ustre)))
  (uim-eval '(ustr-insert-seq! ustre ()))
  (assert-error (lambda ()
                  (uim '(ustr-cursor-backside ustre))))
  (assert-equal 0
                (uim '(ustr-length ustre)))
  (assert-equal 0
                (uim '(ustr-cursor-pos ustre))))
