#! /usr/bin/env sscm -C UTF-8
;; -*- buffer-file-coding-system: utf-8 -*-

;;  Filename : test-eq.scm
;;  About    : unit tests for eq?
;;
;;  Copyright (C) 2006 YAMAMOTO Kengo <yamaken AT bp.iij4u.or.jp>
;;
;;  All rights reserved.
;;
;;  Redistribution and use in source and binary forms, with or without
;;  modification, are permitted provided that the following conditions
;;  are met:
;;
;;  1. Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer.
;;  2. Redistributions in binary form must reproduce the above copyright
;;     notice, this list of conditions and the following disclaimer in the
;;     documentation and/or other materials provided with the distribution.
;;  3. Neither the name of authors nor the names of its contributors
;;     may be used to endorse or promote products derived from this software
;;     without specific prior written permission.
;;
;;  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS
;;  IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
;;  THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;;  PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR
;;  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;;  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;;  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(load "test/unittest.scm")

(define tn test-name)

(tn "eq? invalid form")
(assert-error  (tn) (lambda () (eq?)))
(assert-error  (tn) (lambda () (eq? #f)))
(assert-error  (tn) (lambda () (eq? #f #f #f)))

(tn "eq? different types")
(assert-eq? (tn) #f (eq? 1 #\1))
(assert-eq? (tn) #f (eq? #\1 "1"))
(assert-eq? (tn) #f (eq? #\1 '("1")))
(assert-eq? (tn) #f (eq? '#("1") '("1")))

(tn "eq? boolean")
(assert-eq? (tn) #t (eq? #f #f))
(assert-eq? (tn) #f (eq? #f #t))
(assert-eq? (tn) #f (eq? #t #f))
(assert-eq? (tn) #t (eq? #t #t))

(tn "eq? null")
(assert-eq? (tn) #t (eq? '() '()))
(if (and (provided? "sigscheme")
         (provided? "siod-bugs"))
    (begin
      (assert-eq? (tn) #t (eq? #f '()))
      (assert-eq? (tn) #t (eq? '() #f)))
    (begin
      (assert-eq? (tn) #f (eq? #f '()))
      (assert-eq? (tn) #f (eq? '() #f))))
(if (symbol-bound? 'vector?)
    (begin
      (assert-eq? (tn) #f (eq? '() '#()))
      (assert-eq? (tn) #f (eq? '#() '()))))

(tn "eq? #<eof>")
(if (provided? "sigscheme")
    (begin
      (assert-eq? (tn) #t (eq? (eof) (eof)))
      (assert-eq? (tn) #f (eq? (eof) (undef)))
      (assert-eq? (tn) #f (eq? (undef) (eof)))
      (assert-eq? (tn) #f (eq? '() (eof)))
      (assert-eq? (tn) #f (eq? (eof) '()))
      (assert-eq? (tn) #f (eq? #f (eof)))
      (assert-eq? (tn) #f (eq? (eof) #f))))

(tn "eq? #<undef>")
(if (provided? "sigscheme")
    (begin
      (assert-eq? (tn) #t (eq? (undef) (undef)))
      (assert-eq? (tn) #f (eq? (eof) (undef)))
      (assert-eq? (tn) #f (eq? (undef) (eof)))
      (assert-eq? (tn) #f (eq? '() (undef)))
      (assert-eq? (tn) #f (eq? (undef) '()))
      (assert-eq? (tn) #f (eq? #f (undef)))
      (assert-eq? (tn) #f (eq? (undef) #f))))

(tn "eq? integer")
(if (and (provided? "sigscheme")
         (provided? "immediate-number-only"))
    (begin
      (assert-eq? (tn) #t (eq? 0 0))
      (assert-eq? (tn) #t (eq? 1 1))
      (assert-eq? (tn) #t (eq? 3 3))
      (assert-eq? (tn) #t (eq? -1 -1))
      (assert-eq? (tn) #t (eq? -3 -3))

      (assert-eq? (tn) #f (eq? 0 1))
      (assert-eq? (tn) #f (eq? 1 0))
      (assert-eq? (tn) #f (eq? 1 3))
      (assert-eq? (tn) #f (eq? 3 1))
      (assert-eq? (tn) #f (eq? -1 1))
      (assert-eq? (tn) #f (eq? 1 -1))
      (assert-eq? (tn) #f (eq? -3 3))
      (assert-eq? (tn) #f (eq? 3 -3))
      (assert-eq? (tn) #f (eq? -1 -3))
      (assert-eq? (tn) #f (eq? -3 -1))))

(tn "eq? symbol")
(assert-eq? (tn) #t (eq? 'symbol 'symbol))
(assert-eq? (tn) #f (eq? 'symbol1 'symbol2))
(if (and (provided? "sigscheme")
         (provided? "strict-r5rs"))
    (begin
      (assert-eq? (tn) #t (eq? 'symbol 'SYMBOL))
      (assert-eq? (tn) #t (eq? 'SYMBOL 'symbol))
      (assert-eq? (tn) #t (eq? 'symbol 'Symbol))
      (assert-eq? (tn) #t (eq? 'Symbol 'symbol))
      (assert-eq? (tn) #t (eq? 'symbol 'syMBoL))
      (assert-eq? (tn) #t (eq? 'syMBoL 'symbol)))
    (begin
      (assert-eq? (tn) #f (eq? 'symbol 'SYMBOL))
      (assert-eq? (tn) #f (eq? 'SYMBOL 'symbol))
      (assert-eq? (tn) #f (eq? 'symbol 'Symbol))
      (assert-eq? (tn) #f (eq? 'Symbol 'symbol))
      (assert-eq? (tn) #f (eq? 'symbol 'syMBoL))
      (assert-eq? (tn) #f (eq? 'syMBoL 'symbol))))

(tn "eq? singlebyte char")
(if (provided? "sigscheme")
    (if (provided? "immediate-char-only")
        (begin
          (assert-eq? (tn) #t (eq? #\a #\a))
          (assert-eq? (tn) #f (eq? #\a #\b))
          (assert-eq? (tn) #f (eq? #\b #\a))
          (assert-eq? (tn) #t (eq? #\b #\b)))
        (begin
          (assert-eq? (tn) #f (eq? #\a #\a))
          (assert-eq? (tn) #f (eq? #\a #\b))
          (assert-eq? (tn) #f (eq? #\b #\a))
          (assert-eq? (tn) #f (eq? #\b #\b)))))

(let ((c1 #\a)
      (c2 #\b))
  (assert-eq? (tn) #t (eq? c1 c1))
  (assert-eq? (tn) #t (eq? c2 c2)))

(tn "eq? multibyte char")
(if (provided? "sigscheme")
    (if (provided? "immediate-char-only")
        (begin
          (assert-eq? (tn) #t (eq? #\あ #\あ))
          (assert-eq? (tn) #f (eq? #\あ #\い))
          (assert-eq? (tn) #f (eq? #\い #\あ))
          (assert-eq? (tn) #t (eq? #\い #\い)))
        (begin
          (assert-eq? (tn) #f (eq? #\あ #\あ))
          (assert-eq? (tn) #f (eq? #\あ #\い))
          (assert-eq? (tn) #f (eq? #\い #\あ))
          (assert-eq? (tn) #f (eq? #\い #\い)))))

(let ((c1 #\あ)
      (c2 #\い))
  (assert-eq? (tn) #t (eq? c1 c1))
  (assert-eq? (tn) #t (eq? c2 c2)))

(tn "eq? singlebyte string")
(if (provided? "sigscheme")
    (begin
      (assert-eq? (tn) #f (eq? "" ""))
      (assert-eq? (tn) #f (eq? "a" "a"))
      (assert-eq? (tn) #f (eq? "b" "b"))
      (assert-eq? (tn) #f (eq? "aBc12!" "aBc12!"))))
(let ((s1 "")
      (s2 "a")
      (s3 "b")
      (s4 "aBc12!"))
  (assert-eq? (tn) #t (eq? s1 s1))
  (assert-eq? (tn) #t (eq? s2 s2))
  (assert-eq? (tn) #t (eq? s3 s3))
  (assert-eq? (tn) #t (eq? s4 s4)))
(assert-eq? (tn) #f (eq? "" "a"))
(assert-eq? (tn) #f (eq? "a" ""))
(assert-eq? (tn) #f (eq? "a" "b"))
(assert-eq? (tn) #f (eq? "b" "a"))
(assert-eq? (tn) #f (eq? "a" "A"))
(assert-eq? (tn) #f (eq? "A" "a"))
(assert-eq? (tn) #f (eq? "aBc123!" "aBc12!"))
(assert-eq? (tn) #f (eq? "aBc12!" "aBc123!"))

(tn "eq? multibyte string")
(if (provided? "sigscheme")
    (begin
      (assert-eq? (tn) #f (eq? "あ" "あ"))
      (assert-eq? (tn) #f (eq? "い" "い"))
      (assert-eq? (tn) #f (eq? "あ0イう12!" "あ0イう12!"))))
(let ((s1 "あ")
      (s2 "い")
      (s3 "あ0イう12!"))
  (assert-eq? (tn) #t (eq? s1 s1))
  (assert-eq? (tn) #t (eq? s2 s2))
  (assert-eq? (tn) #t (eq? s3 s3)))
(assert-eq? (tn) #f (eq? "" "あ"))
(assert-eq? (tn) #f (eq? "あ" ""))
(assert-eq? (tn) #f (eq? "あ" "い"))
(assert-eq? (tn) #f (eq? "い" "あ"))
(assert-eq? (tn) #f (eq? "あ" "ア"))
(assert-eq? (tn) #f (eq? "ア" "あ"))
(assert-eq? (tn) #f (eq? "あ0イうぇ12!" "あ0イう12!"))
(assert-eq? (tn) #f (eq? "あ0イう12!" "あ0イうぇ12!"))

(tn "eq? procedure")
(assert-eq? (tn) #t (eq? + +))
(assert-eq? (tn) #f (eq? + -))
(assert-eq? (tn) #f (eq? - +))
(assert-eq? (tn) #t (eq? - -))
(let ((plus +))
  (assert-eq? (tn) #t (eq? + plus))
  (assert-eq? (tn) #t (eq? plus +))
  (assert-eq? (tn) #t (eq? plus plus)))

(tn "eq? syntax")
(assert-eq? (tn) #t (eq? if if))
(assert-eq? (tn) #f (eq? if set!))
(assert-eq? (tn) #f (eq? set! if))
(assert-eq? (tn) #t (eq? set! set!))
;; (define syntax if) is an invalid form

(tn "eq? macro")
(if (symbol-bound? 'let-syntax)
    (let-syntax ((macro1 (syntax-rules ()
                           ((_) 'macro1-expanded)))
                 (macro2 (syntax-rules ()
                           ((_) 'macro2-expanded))))
      (assert-eq? (tn) #t (eq? macro1 macro1))
      (assert-eq? (tn) #f (eq? macro2 macro1))
      (assert-eq? (tn) #f (eq? macro1 macro2))
      (assert-eq? (tn) #t (eq? macro2 macro2))))

(tn "eq? closure")
(let ((closure (lambda () #t)))
  (assert-eq? (tn) #t (eq? closure closure))
  (if (provided? "sigscheme")
      (begin
        (assert-eq? (tn) #f (eq? closure (lambda () #t)))
        (assert-eq? (tn) #f (eq? (lambda () #t) closure))
        (assert-eq? (tn) #f (eq? (lambda () #t) (lambda () #t))))))

(tn "eq? stateful closure")
(let ((stateful (lambda ()
                  (let ((state 0))
                    (lambda ()
                      (set! state (+ state 1))
                      state)))))
  (assert-eq? (tn) #t (eq? stateful stateful))
  (assert-eq? (tn) #f (eq? (stateful) (stateful))))

(let ((may-be-optimized-out (lambda ()
                              (let ((state 0))
                                (lambda ()
                                  (set! state (+ state 1))
                                  0)))))
  (assert-eq? (tn) #t (eq? may-be-optimized-out may-be-optimized-out))
  (if (provided? "sigscheme")
      (assert-eq? (tn) #f (eq? (may-be-optimized-out) (may-be-optimized-out)))))

(letrec ((may-be-unified1 (lambda ()
                            (if (eq? may-be-unified1
                                     may-be-unified2)
                                'optimized-out
                                'not-unified1)))
         (may-be-unified2 (lambda ()
                            (if (eq? may-be-unified1
                                     may-be-unified2)
                                'optimized-out
                                'not-unified2))))
  (if (provided? "sigscheme")
      (begin
        (assert-eq? (tn) #f (eq? may-be-unified1 may-be-unified2))
        (assert-eq? (tn) #f (eq? (may-be-unified1) (may-be-unified2))))
      (begin
        ;; other implementations may pass this
        ;;(assert-eq? (tn) #t (eq? may-be-unified1 may-be-unified2))
        ;;(assert-eq? (tn) #t (eq? (may-be-unified1) (may-be-unified2)))
        )))

(tn "eq? continuation")
(call-with-current-continuation
 (lambda (k1)
   (call-with-current-continuation
    (lambda (k2)
      (assert-eq? (tn) #t (eq? k1 k1))
      (assert-eq? (tn) #f (eq? k1 k2))
      (assert-eq? (tn) #f (eq? k2 k1))
      (assert-eq? (tn) #t (eq? k2 k2))
      (let ((cont k1))
        (assert-eq? (tn) #t (eq? cont cont))
        (assert-eq? (tn) #t (eq? cont k1))
        (assert-eq? (tn) #t (eq? k1 cont))
        (assert-eq? (tn) #f (eq? cont k2))
        (assert-eq? (tn) #f (eq? k2 cont)))))))

(tn "eq? port")
(assert-eq? (tn) #t (eq? (current-output-port) (current-output-port)))
(assert-eq? (tn) #f (eq? (current-input-port) (current-output-port)))
(assert-eq? (tn) #f (eq? (current-output-port) (current-input-port)))
(assert-eq? (tn) #t (eq? (current-input-port) (current-input-port)))
(let ((port (current-input-port)))
  (assert-eq? (tn) #t (eq? port port))
  (assert-eq? (tn) #t (eq? (current-input-port) port))
  (assert-eq? (tn) #t (eq? port (current-input-port)))
  (assert-eq? (tn) #f (eq? (current-output-port) port))
  (assert-eq? (tn) #f (eq? port (current-output-port))))

(tn "eq? pair")
(if (provided? "sigscheme")
    (begin
      (assert-eq? (tn) #f (eq? '(#t . #t) '(#t . #t)))
      (assert-eq? (tn) #f (eq? '(#f . #t) '(#f . #t)))
      (assert-eq? (tn) #f (eq? '(#t . #f) '(#t . #f)))
      (assert-eq? (tn) #f (eq? '(#f . #t) '(#t . #f)))
      (assert-eq? (tn) #f (eq? '(#\a . #\a) '(#\a . #\a)))
      (assert-eq? (tn) #f (eq? '(#\a . #\b) '(#\a . #\b)))
      (assert-eq? (tn) #f (eq? '(#\b . #\a) '(#\b . #\a)))
      (assert-eq? (tn) #f (eq? '(#\a . #\b) '(#\b . #\a)))
      (assert-eq? (tn) #f (eq? '("a" . "a") '("a" . "a")))
      (assert-eq? (tn) #f (eq? '("a" . "b") '("a" . "b")))
      (assert-eq? (tn) #f (eq? '("b" . "a") '("b" . "a")))
      (assert-eq? (tn) #f (eq? '("a" . "b") '("b" . "a")))))

(assert-eq? (tn) #f (eq? (cons #t #t) (cons #t #t)))
(assert-eq? (tn) #f (eq? (cons #f #t) (cons #f #t)))
(assert-eq? (tn) #f (eq? (cons #t #f) (cons #t #f)))
(assert-eq? (tn) #f (eq? (cons #f #t) (cons #t #f)))
(assert-eq? (tn) #f (eq? (cons #\a #\a) (cons #\a #\a)))
(assert-eq? (tn) #f (eq? (cons #\a #\b) (cons #\a #\b)))
(assert-eq? (tn) #f (eq? (cons #\b #\a) (cons #\b #\a)))
(assert-eq? (tn) #f (eq? (cons #\a #\b) (cons #\b #\a)))
(assert-eq? (tn) #f (eq? (cons "a" "a") (cons "a" "a")))
(assert-eq? (tn) #f (eq? (cons "a" "b") (cons "a" "b")))
(assert-eq? (tn) #f (eq? (cons "b" "a") (cons "b" "a")))
(assert-eq? (tn) #f (eq? (cons "a" "b") (cons "b" "a")))

(tn "eq? list")
(if (provided? "sigscheme")
    (begin
      (assert-eq? (tn) #f (eq? '(#f) '(#f)))
      (assert-eq? (tn) #f (eq? '(#f) '(#t)))
      (assert-eq? (tn) #f (eq? '(#t) '(#f)))
      (assert-eq? (tn) #f (eq? '(#t) '(#t)))
      (assert-eq? (tn) #f (eq? '((#f)) '((#f))))
      (assert-eq? (tn) #f (eq? '((#f)) '((#t))))
      (assert-eq? (tn) #f (eq? '((#t)) '((#f))))
      (assert-eq? (tn) #f (eq? '((#t)) '((#t))))
      (assert-eq? (tn) #f (eq? '(1) '(1)))
      (assert-eq? (tn) #f (eq? '(1) '(0)))
      (assert-eq? (tn) #f (eq? '(1 3 5 0 13)
                               '(1 3 5 0 13)))
      (assert-eq? (tn) #f (eq? '(1 3 2 0 13)
                               '(1 3 5 0 13)))
      (assert-eq? (tn) #f (eq? '(1 3 (5 0 13))
                               '(1 3 (5 0 13))))
      (assert-eq? (tn) #f (eq? '(1 3 (2 0 13))
                               '(1 3 (5 0 13))))
      (assert-eq? (tn) #f (eq? '((1)) '((1))))
      (assert-eq? (tn) #f (eq? '((1)) '((0))))
      (assert-eq? (tn) #f (eq? '((1) (3) (5) (0) (13))
                               '((1) (3) (5) (0) (13))))
      (assert-eq? (tn) #f (eq? '((1) (3) (2) (0) (13))
                               '((1) (3) (5) (0) (13))))
      (assert-eq? (tn) #f (eq? '(#\a) '(#\a)))
      (assert-eq? (tn) #f (eq? '(#\a) '(#\b)))
      (assert-eq? (tn) #f (eq? '(#\b) '(#\a)))
      (assert-eq? (tn) #f (eq? '((#\a)) '((#\a))))
      (assert-eq? (tn) #f (eq? '((#\a)) '((#\b))))
      (assert-eq? (tn) #f (eq? '((#\b)) '((#\a))))))

(assert-eq? (tn) #f (eq? (list #f) (list #f)))
(assert-eq? (tn) #f (eq? (list #f) (list #t)))
(assert-eq? (tn) #f (eq? (list #t) (list #f)))
(assert-eq? (tn) #f (eq? (list #t) (list #t)))
(assert-eq? (tn) #f (eq? (list (list #f)) (list (list #f))))
(assert-eq? (tn) #f (eq? (list (list #f)) (list (list #t))))
(assert-eq? (tn) #f (eq? (list (list #t)) (list (list #f))))
(assert-eq? (tn) #f (eq? (list (list #t)) (list (list #t))))
(assert-eq? (tn) #f (eq? (list 1) (list 1)))
(assert-eq? (tn) #f (eq? (list 1) (list 0)))
(assert-eq? (tn) #f (eq? (list 1 3 5 0 13)
                         (list 1 3 5 0 13)))
(assert-eq? (tn) #f (eq? (list 1 3 2 0 13)
                         (list 1 3 5 0 13)))
(assert-eq? (tn) #f (eq? (list 1 3 (list 5 0 13))
                         (list 1 3 (list 5 0 13))))
(assert-eq? (tn) #f (eq? (list 1 3 (list 2 0 13))
                         (list 1 3 (list 5 0 13))))
(assert-eq? (tn) #f (eq? (list (list 1)) (list (list 1))))
(assert-eq? (tn) #f (eq? (list (list 1)) (list (list 0))))
(assert-eq? (tn) #f (eq? (list (list 1) (list 3) (list 5) (list 0) (list 13))
                         (list (list 1) (list 3) (list 5) (list 0) (list 13))))
(assert-eq? (tn) #f (eq? (list (list 1) (list 3) (list 2) (list 0) (list 13))
                         (list (list 1) (list 3) (list 5) (list 0) (list 13))))
(assert-eq? (tn) #f (eq? (list #\a) (list #\a)))
(assert-eq? (tn) #f (eq? (list #\a) (list #\b)))
(assert-eq? (tn) #f (eq? (list #\b) (list #\a)))
(assert-eq? (tn) #f (eq? (list (list #\a)) (list (list #\a))))
(assert-eq? (tn) #f (eq? (list (list #\a)) (list (list #\b))))
(assert-eq? (tn) #f (eq? (list (list #\b)) (list (list #\a))))

(if (provided? "sigscheme")
    (begin
      (assert-eq? (tn) #f (eq? '("") '("")))
      (assert-eq? (tn) #f (eq? '(("")) '((""))))
      (assert-eq? (tn) #f (eq? '("aBc12!")
                               '("aBc12!")))
      (assert-eq? (tn) #f (eq? '("あ0イう12!")
                               '("あ0イう12!")))
      (assert-eq? (tn) #f (eq? '("a" "" "aB1" ("3c" "d") "a")
                               '("a" "" "aB1" ("3c" "d") "a")))
      (assert-eq? (tn) #f (eq? '(("aBc12!"))
                               '(("aBc12!"))))
      (assert-eq? (tn) #f (eq? '(("あ0イう12!"))
                               '(("あ0イう12!"))))))

(assert-eq? (tn) #f (eq? (list "") (list "")))
(assert-eq? (tn) #f (eq? (list (list "")) (list (list ""))))
(assert-eq? (tn) #f (eq? (list "aBc12!")
                         (list "aBc12!")))
(assert-eq? (tn) #f (eq? (list "あ0イう12!")
                         (list "あ0イう12!")))
(assert-eq? (tn) #f (eq? (list "a" "" "aB1" (list "3c" "d") "a")
                         (list "a" "" "aB1" (list "3c" "d") "a")))
(assert-eq? (tn) #f (eq? (list (list "aBc12!"))
                         (list (list "aBc12!"))))
(assert-eq? (tn) #f (eq? (list (list "あ0イう12!"))
                         (list (list "あ0イう12!"))))

(if (provided? "sigscheme")
    (begin
      (assert-eq? (tn) #f (eq? '("aBc123!")
                               '("aBc12!")))
      (assert-eq? (tn) #f (eq? '("あ0イぅ12!")
                               '("あ0イう12!")))
      (assert-eq? (tn) #f (eq? '("a" "" "aB1" ("3c" "e") "a")
                               '("a" "" "aB1" ("3c" "d") "a")))
      (assert-eq? (tn) #f (eq? '(("aBc123!"))
                               '(("aBc12!"))))
      (assert-eq? (tn) #f (eq? '(("あ0イぅ12!"))
                               '(("あ0イう12!"))))))

(assert-eq? (tn) #f (eq? (list "aBc123!")
                         (list "aBc12!")))
(assert-eq? (tn) #f (eq? (list "あ0イぅ12!")
                         (list "あ0イう12!")))
(assert-eq? (tn) #f (eq? (list "a" "" "aB1" (list "3c" "e") "a")
                         (list "a" "" "aB1" (list "3c" "d") "a")))
(assert-eq? (tn) #f (eq? (list (list "aBc123!"))
                         (list (list "aBc12!"))))
(assert-eq? (tn) #f (eq? (list (list "あ0イぅ12!"))
                         (list (list "あ0イう12!"))))

(if (provided? "sigscheme")
    (begin
      (assert-eq? (tn) #f
                  (eq? '(0 #\a "" #("vE" -1 (#\?)) 23 + "aBc" (-1 #\b ("Ls")) #t)
                       '(0 #\a "" #("vE" -1 (#\?)) 23 + "aBc" (-1 #\b ("Ls")) #t)))
      (assert-eq? (tn) #f
                  (eq? '(0 #\a "" #("vE" -1 (#\?)) 23 + "aBc" (-1 #\b ("Ls")) #t)
                       '(0 #\a "" #("vE" -1 (#\?)) 23 + "aBc" (-1 #\b ("L")) #t)))
      (assert-eq? (tn) #f
                  (eq? '(0 #\a "" #("vE" -1 (#\?)) 23 + "aBc" (-1 #\b ("Ls")) #t)
                       '(0 #\a "" ("vE" -1 (#\?)) 23 + "aBc" (-1 #\b ("Ls")) #t)))
      (assert-eq? (tn) #f
                  (eq? '(0 #\a "" #("vE" -1 (#\?)) 23 + "aBc" (-1 #\b ("Ls")) #t)
                       '(0 #\a "" #("vE" -1 (#\?)) 23 + "aBc" #t)))
      (assert-eq? (tn) #f
                  (eq? '(0 #\a "" #("vE" -1 (#\?)) 23 + "aBc" #t)
                       '(0 #\a "" #("vE" -1 (#\?)) 23 + "aBc" (-1 #\b ("Ls")) #t)))))

(assert-eq? (tn) #f
            (eq? (list 0 #\a "" (vector "vE" -1 '(#\?)) 23 + "aBc" (list -1 #\b '("Ls")) #t)
                 (list 0 #\a "" (vector "vE" -1 '(#\?)) 23 + "aBc" (list -1 #\b '("Ls")) #t)))
(assert-eq? (tn) #f
            (eq? (list 0 #\a "" (vector "vE" -1 '(#\?)) 23 + "aBc" (list -1 #\b '("Ls")) #t)
                 (list 0 #\a "" (vector "vE" -1 '(#\?)) 23 + "aBc" (list -1 #\b '("L")) #t)))
(assert-eq? (tn) #f
            (eq? (list 0 #\a "" (vector "vE" -1 '(#\?)) 23 + "aBc" (list -1 #\b '("Ls")) #t)
                 (list 0 #\a "" (list "vE" -1 '(#\?))  23 + "aBc" (list -1 #\b '("Ls")) #t)))
(assert-eq? (tn) #f
            (eq? (list 0 #\a "" (vector "vE" -1 '(#\?)) 23 + "aBc" (list -1 #\b '("Ls")) #t)
                 (list 0 #\a "" (vector "vE" -1 '(#\?)) 23 + "aBc" #t)))
(assert-eq? (tn) #f
            (eq? (list 0 #\a "" (vector "vE" -1 '(#\?)) 23 + "aBc" #t)
                 (list 0 #\a "" (vector "vE" -1 '(#\?)) 23 + "aBc" (list -1 #\b '("Ls")) #t)))

(tn "eq? empty vector")
(if (provided? "sigscheme")
    (assert-eq? (tn) #f (eq? '#() '#())))
(assert-eq? (tn) #f (eq? (vector) (vector)))

(let ((v1 '#())
      (v2 (vector)))
  (assert-eq? (tn) #t (eq? v1 v1))
  (assert-eq? (tn) #t (eq? v2 v2))
  (assert-eq? (tn) #f (eq? v1 v2)))

(tn "eq? vector")
(if (provided? "sigscheme")
    (begin
      (assert-eq? (tn) #f (eq? '#(#f) '#(#f)))
      (assert-eq? (tn) #f (eq? '#(#f) '#(#t)))
      (assert-eq? (tn) #f (eq? '#(#t) '#(#f)))
      (assert-eq? (tn) #f (eq? '#(#t) '#(#t)))
      (assert-eq? (tn) #f (eq? '#(#(#f)) '#(#(#f))))
      (assert-eq? (tn) #f (eq? '#(#(#f)) '#(#(#t))))
      (assert-eq? (tn) #f (eq? '#(#(#t)) '#(#(#f))))
      (assert-eq? (tn) #f (eq? '#(#(#t)) '#(#(#t))))
      (assert-eq? (tn) #f (eq? '#(1) '#(1)))
      (assert-eq? (tn) #f (eq? '#(1) '#(0)))
      (assert-eq? (tn) #f (eq? '#(1 3 5 0 13)
                               '#(1 3 5 0 13)))
      (assert-eq? (tn) #f (eq? '#(1 3 2 0 13)
                               '#(1 3 5 0 13)))
      (assert-eq? (tn) #f (eq? '#(1 3 #(5 0 13))
                               '#(1 3 #(5 0 13))))
      (assert-eq? (tn) #f (eq? '#(1 3 #(2 0 13))
                               '#(1 3 #(5 0 13))))
      (assert-eq? (tn) #f (eq? '#(#(1)) '#(#(1))))
      (assert-eq? (tn) #f (eq? '#(#(1)) '#(#(0))))
      (assert-eq? (tn) #f (eq? '#(#(1) #(3) #(5) #(0) #(13))
                               '#(#(1) #(3) #(5) #(0) #(13))))
      (assert-eq? (tn) #f (eq? '#(#(1) #(3) #(2) #(0) #(13))
                               '#(#(1) #(3) #(5) #(0) #(13))))
      (assert-eq? (tn) #f (eq? '#(#\a) '#(#\a)))
      (assert-eq? (tn) #f (eq? '#(#\a) '#(#\b)))
      (assert-eq? (tn) #f (eq? '#(#\b) '#(#\a)))
      (assert-eq? (tn) #f (eq? '#(#(#\a)) '#(#(#\a))))
      (assert-eq? (tn) #f (eq? '#(#(#\a)) '#(#(#\b))))
      (assert-eq? (tn) #f (eq? '#(#(#\b)) '#(#(#\a))))))

(assert-eq? (tn) #f (eq? (vector #f) (vector #f)))
(assert-eq? (tn) #f (eq? (vector #f) (vector #t)))
(assert-eq? (tn) #f (eq? (vector #t) (vector #f)))
(assert-eq? (tn) #f (eq? (vector #t) (vector #t)))
(assert-eq? (tn) #f (eq? (vector (vector #f)) (vector (vector #f))))
(assert-eq? (tn) #f (eq? (vector (vector #f)) (vector (vector #t))))
(assert-eq? (tn) #f (eq? (vector (vector #t)) (vector (vector #f))))
(assert-eq? (tn) #f (eq? (vector (vector #t)) (vector (vector #t))))
(assert-eq? (tn) #f (eq? (vector 1) (vector 1)))
(assert-eq? (tn) #f (eq? (vector 1) (vector 0)))
(assert-eq? (tn) #f (eq? (vector 1 3 5 0 13)
                         (vector 1 3 5 0 13)))
(assert-eq? (tn) #f (eq? (vector 1 3 2 0 13)
                         (vector 1 3 5 0 13)))
(assert-eq? (tn) #f (eq? (vector 1 3 (vector 5 0 13))
                         (vector 1 3 (vector 5 0 13))))
(assert-eq? (tn) #f (eq? (vector 1 3 (vector 2 0 13))
                         (vector 1 3 (vector 5 0 13))))
(assert-eq? (tn) #f (eq? (vector (vector 1)) (vector (vector 1))))
(assert-eq? (tn) #f (eq? (vector (vector 1)) (vector (vector 0))))
(assert-eq? (tn) #f (eq? (vector (vector 1) (vector 3) (vector 5) (vector 0) (vector 13))
                         (vector (vector 1) (vector 3) (vector 5) (vector 0) (vector 13))))
(assert-eq? (tn) #f (eq? (vector (vector 1) (vector 3) (vector 2) (vector 0) (vector 13))
                         (vector (vector 1) (vector 3) (vector 5) (vector 0) (vector 13))))
(assert-eq? (tn) #f (eq? (vector #\a) (vector #\a)))
(assert-eq? (tn) #f (eq? (vector #\a) (vector #\b)))
(assert-eq? (tn) #f (eq? (vector #\b) (vector #\a)))
(assert-eq? (tn) #f (eq? (vector (vector #\a)) (vector (vector #\a))))
(assert-eq? (tn) #f (eq? (vector (vector #\a)) (vector (vector #\b))))
(assert-eq? (tn) #f (eq? (vector (vector #\b)) (vector (vector #\a))))

(if (provided? "sigscheme")
    (begin
      (assert-eq? (tn) #f (eq? '#("") '#("")))
      (assert-eq? (tn) #f (eq? '#(#("")) '#(#(""))))
      (assert-eq? (tn) #f (eq? '#("aBc12!")
                               '#("aBc12!")))
      (assert-eq? (tn) #f (eq? '#("あ0イう12!")
                               '#("あ0イう12!")))
      (assert-eq? (tn) #f (eq? '#("a" "" "aB1" #("3c" "d") "a")
                               '#("a" "" "aB1" #("3c" "d") "a")))
      (assert-eq? (tn) #f (eq? '#(#("aBc12!"))
                               '#(#("aBc12!"))))
      (assert-eq? (tn) #f (eq? '#(#("あ0イう12!"))
                               '#(#("あ0イう12!"))))))

(assert-eq? (tn) #f (eq? (vector "") (vector "")))
(assert-eq? (tn) #f (eq? (vector (vector "")) (vector (vector ""))))
(assert-eq? (tn) #f (eq? (vector "aBc12!")
                         (vector "aBc12!")))
(assert-eq? (tn) #f (eq? (vector "あ0イう12!")
                         (vector "あ0イう12!")))
(assert-eq? (tn) #f (eq? (vector "a" "" "aB1" (vector "3c" "d") "a")
                         (vector "a" "" "aB1" (vector "3c" "d") "a")))
(assert-eq? (tn) #f (eq? (vector (vector "aBc12!"))
                         (vector (vector "aBc12!"))))
(assert-eq? (tn) #f (eq? (vector (vector "あ0イう12!"))
                         (vector (vector "あ0イう12!"))))

(if (provided? "sigscheme")
    (begin
      (assert-eq? (tn) #f (eq? '#("aBc123!")
                               '#("aBc12!")))
      (assert-eq? (tn) #f (eq? '#("あ0イぅ12!")
                               '#("あ0イう12!")))
      (assert-eq? (tn) #f (eq? '#("a" "" "aB1" #("3c" "e") "a")
                               '#("a" "" "aB1" #("3c" "d") "a")))
      (assert-eq? (tn) #f (eq? '#(#("aBc123!"))
                               '#(#("aBc12!"))))
      (assert-eq? (tn) #f (eq? '#(#("あ0イぅ12!"))
                               '#(#("あ0イう12!"))))))

(assert-eq? (tn) #f (eq? (vector "aBc123!")
                         (vector "aBc12!")))
(assert-eq? (tn) #f (eq? (vector "あ0イぅ12!")
                         (vector "あ0イう12!")))
(assert-eq? (tn) #f (eq? (vector "a" "" "aB1" (vector "3c" "e") "a")
                         (vector "a" "" "aB1" (vector "3c" "d") "a")))
(assert-eq? (tn) #f (eq? (vector (vector "aBc123!"))
                         (vector (vector "aBc12!"))))
(assert-eq? (tn) #f (eq? (vector (vector "あ0イぅ12!"))
                         (vector (vector "あ0イう12!"))))

(if (provided? "sigscheme")
    (begin
      (assert-eq? (tn) #f
                  (eq? '#(0 #\a "" #("vE" -1 (#\?)) 23 + "aBc" (-1 #\b ("Ls")) #t)
                       '#(0 #\a "" #("vE" -1 (#\?)) 23 + "aBc" (-1 #\b ("Ls")) #t)))
      (assert-eq? (tn) #f
                  (eq? '#(0 #\a "" #("vE" -1 (#\?)) 23 + "aBc" (-1 #\b ("Ls")) #t)
                       '#(0 #\a "" #("vE" -1 (#\?)) 23 + "aBc" (-1 #\b ("L")) #t)))
      (assert-eq? (tn) #f
                  (eq? '#(0 #\a "" #("vE" -1 (#\?)) 23 + "aBc" (-1 #\b ("Ls")) #t)
                       '#(0 #\a "" ("vE" -1 (#\?))  23 + "aBc" (-1 #\b ("Ls")) #t)))
      (assert-eq? (tn) #f
                  (eq? '#(0 #\a "" #("vE" -1 (#\?)) 23 + "aBc" (-1 #\b ("Ls")) #t)
                       '#(0 #\a "" #("vE" -1 (#\?)) 23 + "aBc" #t)))
      (assert-eq? (tn) #f
                  (eq? '#(0 #\a "" #("vE" -1 (#\?)) 23 + "aBc" #t)
                       '#(0 #\a "" #("vE" -1 (#\?)) 23 + "aBc" (-1 #\b ("Ls")) #t)))))

(assert-eq? (tn) #f
            (eq? (vector 0 #\a "" (vector "vE" -1 '(#\?)) 23 + "aBc" (vector -1 #\b '("Ls")) #t)
                 (vector 0 #\a "" (vector "vE" -1 '(#\?)) 23 + "aBc" (vector -1 #\b '("Ls")) #t)))
(assert-eq? (tn) #f
            (eq? (vector 0 #\a "" (vector "vE" -1 '(#\?)) 23 + "aBc" (vector -1 #\b '("Ls")) #t)
                 (vector 0 #\a "" (vector "vE" -1 '(#\?)) 23 + "aBc" (vector -1 #\b '("L")) #t)))
(assert-eq? (tn) #f
            (eq? (vector 0 #\a "" (vector "vE" -1 '(#\?)) 23 + "aBc" (vector -1 #\b '("Ls")) #t)
                 (vector 0 #\a "" (list "vE" -1 '(#\?))   23 + "aBc" (vector -1 #\b '("Ls")) #t)))
(assert-eq? (tn) #f
            (eq? (vector 0 #\a "" (vector "vE" -1 '(#\?)) 23 + "aBc" (vector -1 #\b '("Ls")) #t)
                 (vector 0 #\a "" (vector "vE" -1 '(#\?)) 23 + "aBc" #t)))
(assert-eq? (tn) #f
            (eq? (vector 0 #\a "" (vector "vE" -1 '(#\?)) 23 + "aBc" #t)
                 (vector 0 #\a "" (vector "vE" -1 '(#\?)) 23 + "aBc" (vector -1 #\b '("Ls")) #t)))


(total-report)
