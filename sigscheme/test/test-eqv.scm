#! /usr/bin/env sscm -C UTF-8
;; -*- buffer-file-coding-system: utf-8 -*-

;;  Filename : test-eqv.scm
;;  About    : unit tests for eqv?
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
(define case-insensitive-symbol? #f)

(tn "eqv? invalid form")
(assert-error  (tn) (lambda () (eqv?)))
(assert-error  (tn) (lambda () (eqv? #f)))
(assert-error  (tn) (lambda () (eqv? #f #f #f)))

(tn "eqv? different types")
(assert-eq? (tn) #f (eqv? 1 #\1))
(assert-eq? (tn) #f (eqv? #\1 "1"))
(assert-eq? (tn) #f (eqv? #\1 '("1")))
(assert-eq? (tn) #f (eqv? '#("1") '("1")))

(tn "eqv? boolean")
(assert-eq? (tn) #t (eqv? #f #f))
(assert-eq? (tn) #f (eqv? #f #t))
(assert-eq? (tn) #f (eqv? #t #f))
(assert-eq? (tn) #t (eqv? #t #t))

(tn "eqv? null")
(assert-eq? (tn) #t (eqv? '() '()))
(if (and (provided? "sigscheme")
         (provided? "siod-bugs"))
    (begin
      (assert-eq? (tn) #t (eqv? #f '()))
      (assert-eq? (tn) #t (eqv? '() #f)))
    (begin
      (assert-eq? (tn) #f (eqv? #f '()))
      (assert-eq? (tn) #f (eqv? '() #f))))
(if (symbol-bound? 'vector?)
    (begin
      (assert-eq? (tn) #f (eqv? '() '#()))
      (assert-eq? (tn) #f (eqv? '#() '()))))

(tn "eqv? #<eof>")
(if (provided? "sigscheme")
    (begin
      (assert-eq? (tn) #t (eqv? (eof) (eof)))
      (assert-eq? (tn) #f (eqv? (eof) (undef)))
      (assert-eq? (tn) #f (eqv? (undef) (eof)))
      (assert-eq? (tn) #f (eqv? '() (eof)))
      (assert-eq? (tn) #f (eqv? (eof) '()))
      (assert-eq? (tn) #f (eqv? #f (eof)))
      (assert-eq? (tn) #f (eqv? (eof) #f))))

(tn "eqv? #<undef>")
(if (provided? "sigscheme")
    (begin
      (assert-eq? (tn) #t (eqv? (undef) (undef)))
      (assert-eq? (tn) #f (eqv? (eof) (undef)))
      (assert-eq? (tn) #f (eqv? (undef) (eof)))
      (assert-eq? (tn) #f (eqv? '() (undef)))
      (assert-eq? (tn) #f (eqv? (undef) '()))
      (assert-eq? (tn) #f (eqv? #f (undef)))
      (assert-eq? (tn) #f (eqv? (undef) #f))))

(tn "eqv? integer")
(assert-eq? (tn) #t (eqv? 0 0))
(assert-eq? (tn) #t (eqv? 1 1))
(assert-eq? (tn) #t (eqv? 3 3))
(assert-eq? (tn) #t (eqv? -1 -1))
(assert-eq? (tn) #t (eqv? -3 -3))

(assert-eq? (tn) #f (eqv? 0 1))
(assert-eq? (tn) #f (eqv? 1 0))
(assert-eq? (tn) #f (eqv? 1 3))
(assert-eq? (tn) #f (eqv? 3 1))
(assert-eq? (tn) #f (eqv? -1 1))
(assert-eq? (tn) #f (eqv? 1 -1))
(assert-eq? (tn) #f (eqv? -3 3))
(assert-eq? (tn) #f (eqv? 3 -3))
(assert-eq? (tn) #f (eqv? -1 -3))
(assert-eq? (tn) #f (eqv? -3 -1))

(tn "eqv? symbol")
(assert-eq? (tn) #t (eqv? 'symbol 'symbol))
(assert-eq? (tn) #f (eqv? 'symbol1 'symbol2))
(if (and (provided? "sigscheme")
         (provided? "strict-r5rs")
         case-insensitive-symbol?)
    (begin
      (assert-eq? (tn) #t (eqv? 'symbol 'SYMBOL))
      (assert-eq? (tn) #t (eqv? 'SYMBOL 'symbol))
      (assert-eq? (tn) #t (eqv? 'symbol 'Symbol))
      (assert-eq? (tn) #t (eqv? 'Symbol 'symbol))
      (assert-eq? (tn) #t (eqv? 'symbol 'syMBoL))
      (assert-eq? (tn) #t (eqv? 'syMBoL 'symbol)))
    (begin
      (assert-eq? (tn) #f (eqv? 'symbol 'SYMBOL))
      (assert-eq? (tn) #f (eqv? 'SYMBOL 'symbol))
      (assert-eq? (tn) #f (eqv? 'symbol 'Symbol))
      (assert-eq? (tn) #f (eqv? 'Symbol 'symbol))
      (assert-eq? (tn) #f (eqv? 'symbol 'syMBoL))
      (assert-eq? (tn) #f (eqv? 'syMBoL 'symbol))))

(tn "eqv? singlebyte char")
(assert-eq? (tn) #t (eqv? #\a #\a))
(assert-eq? (tn) #f (eqv? #\a #\b))
(assert-eq? (tn) #f (eqv? #\b #\a))
(assert-eq? (tn) #t (eqv? #\b #\b))

(let ((c1 #\a)
      (c2 #\b))
  (assert-eq? (tn) #t (eqv? c1 c1))
  (assert-eq? (tn) #t (eqv? c2 c2)))

(tn "eqv? multibyte char")
(assert-eq? (tn) #t (eqv? #\あ #\あ))
(assert-eq? (tn) #f (eqv? #\あ #\い))
(assert-eq? (tn) #f (eqv? #\い #\あ))
(assert-eq? (tn) #t (eqv? #\い #\い))

(let ((c1 #\あ)
      (c2 #\い))
  (assert-eq? (tn) #t (eqv? c1 c1))
  (assert-eq? (tn) #t (eqv? c2 c2)))

(tn "eqv? singlebyte string")
(if (provided? "sigscheme")
    (begin
      (assert-eq? (tn) #f (eqv? "" ""))
      (assert-eq? (tn) #f (eqv? "a" "a"))
      (assert-eq? (tn) #f (eqv? "b" "b"))
      (assert-eq? (tn) #f (eqv? "aBc12!" "aBc12!"))))
(let ((s1 "")
      (s2 "a")
      (s3 "b")
      (s4 "aBc12!"))
  (assert-eq? (tn) #t (eqv? s1 s1))
  (assert-eq? (tn) #t (eqv? s2 s2))
  (assert-eq? (tn) #t (eqv? s3 s3))
  (assert-eq? (tn) #t (eqv? s4 s4)))
(assert-eq? (tn) #f (eqv? "" "a"))
(assert-eq? (tn) #f (eqv? "a" ""))
(assert-eq? (tn) #f (eqv? "a" "b"))
(assert-eq? (tn) #f (eqv? "b" "a"))
(assert-eq? (tn) #f (eqv? "a" "A"))
(assert-eq? (tn) #f (eqv? "A" "a"))
(assert-eq? (tn) #f (eqv? "aBc123!" "aBc12!"))
(assert-eq? (tn) #f (eqv? "aBc12!" "aBc123!"))

(tn "eqv? multibyte string")
(if (provided? "sigscheme")
    (begin
      (assert-eq? (tn) #f (eqv? "あ" "あ"))
      (assert-eq? (tn) #f (eqv? "い" "い"))
      (assert-eq? (tn) #f (eqv? "あ0イう12!" "あ0イう12!"))))
(let ((s1 "あ")
      (s2 "い")
      (s3 "あ0イう12!"))
  (assert-eq? (tn) #t (eqv? s1 s1))
  (assert-eq? (tn) #t (eqv? s2 s2))
  (assert-eq? (tn) #t (eqv? s3 s3)))
(assert-eq? (tn) #f (eqv? "" "あ"))
(assert-eq? (tn) #f (eqv? "あ" ""))
(assert-eq? (tn) #f (eqv? "あ" "い"))
(assert-eq? (tn) #f (eqv? "い" "あ"))
(assert-eq? (tn) #f (eqv? "あ" "ア"))
(assert-eq? (tn) #f (eqv? "ア" "あ"))
(assert-eq? (tn) #f (eqv? "あ0イうぇ12!" "あ0イう12!"))
(assert-eq? (tn) #f (eqv? "あ0イう12!" "あ0イうぇ12!"))

(tn "eqv? procedure")
(assert-eq? (tn) #t (eqv? + +))
(assert-eq? (tn) #f (eqv? + -))
(assert-eq? (tn) #f (eqv? - +))
(assert-eq? (tn) #t (eqv? - -))
(let ((plus +))
  (assert-eq? (tn) #t (eqv? + plus))
  (assert-eq? (tn) #t (eqv? plus +))
  (assert-eq? (tn) #t (eqv? plus plus)))

(tn "eqv? syntax")
(assert-error (tn) (lambda () (eqv? if if)))
(assert-error (tn) (lambda () (eqv? if set!)))
(assert-error (tn) (lambda () (eqv? set! if)))
(assert-error (tn) (lambda () (eqv? set! set!)))
;; (define syntax if) is an invalid form

(tn "eqv? macro")
(if (symbol-bound? 'let-syntax)
    (let-syntax ((macro1 (syntax-rules ()
                           ((_) 'macro1-expanded)))
                 (macro2 (syntax-rules ()
                           ((_) 'macro2-expanded))))
      ;; syntactic keyword as value
      (assert-error (tn) (lambda () (eqv? macro1 macro1)))
      (assert-error (tn) (lambda () (eqv? macro2 macro1)))
      (assert-error (tn) (lambda () (eqv? macro1 macro2)))
      (assert-error (tn) (lambda () (eqv? macro2 macro2)))))

(tn "eqv? closure")
(let ((closure (lambda () #t)))
  (assert-eq? (tn) #t (eqv? closure closure))
  (if (provided? "sigscheme")
      (begin
        (assert-eq? (tn) #f (eqv? closure (lambda () #t)))
        (assert-eq? (tn) #f (eqv? (lambda () #t) closure))
        (assert-eq? (tn) #f (eqv? (lambda () #t) (lambda () #t))))))

(tn "eqv? stateful closure")
(let ((stateful (lambda ()
                  (let ((state 0))
                    (lambda ()
                      (set! state (+ state 1))
                      state)))))
  (assert-eq? (tn) #t (eqv? stateful stateful))
  (assert-eq? (tn) #f (eqv? (stateful) (stateful))))

(let ((may-be-optimized-out (lambda ()
                              (let ((state 0))
                                (lambda ()
                                  (set! state (+ state 1))
                                  0)))))
  (assert-eq? (tn) #t (eqv? may-be-optimized-out may-be-optimized-out))
  (if (provided? "sigscheme")
      (assert-eq? (tn) #f (eqv? (may-be-optimized-out) (may-be-optimized-out)))))

(letrec ((may-be-unified1 (lambda ()
                            (if (eqv? may-be-unified1
                                      may-be-unified2)
                                'optimized-out
                                'not-unified1)))
         (may-be-unified2 (lambda ()
                            (if (eqv? may-be-unified1
                                      may-be-unified2)
                                'optimized-out
                                'not-unified2))))
  (if (provided? "sigscheme")
      (begin
        (assert-eq? (tn) #f (eqv? may-be-unified1 may-be-unified2))
        (assert-eq? (tn) #f (eqv? (may-be-unified1) (may-be-unified2))))
      (begin
        ;; other implementations may pass this
        ;;(assert-eq? (tn) #t (eqv? may-be-unified1 may-be-unified2))
        ;;(assert-eq? (tn) #t (eqv? (may-be-unified1) (may-be-unified2)))
        )))

(tn "eqv? continuation")
(call-with-current-continuation
 (lambda (k1)
   (call-with-current-continuation
    (lambda (k2)
      (assert-eq? (tn) #t (eqv? k1 k1))
      (assert-eq? (tn) #f (eqv? k1 k2))
      (assert-eq? (tn) #f (eqv? k2 k1))
      (assert-eq? (tn) #t (eqv? k2 k2))
      (let ((cont k1))
        (assert-eq? (tn) #t (eqv? cont cont))
        (assert-eq? (tn) #t (eqv? cont k1))
        (assert-eq? (tn) #t (eqv? k1 cont))
        (assert-eq? (tn) #f (eqv? cont k2))
        (assert-eq? (tn) #f (eqv? k2 cont)))))))

(tn "eqv? port")
(assert-eq? (tn) #t (eqv? (current-output-port) (current-output-port)))
(assert-eq? (tn) #f (eqv? (current-input-port) (current-output-port)))
(assert-eq? (tn) #f (eqv? (current-output-port) (current-input-port)))
(assert-eq? (tn) #t (eqv? (current-input-port) (current-input-port)))
(let ((port (current-input-port)))
  (assert-eq? (tn) #t (eqv? port port))
  (assert-eq? (tn) #t (eqv? (current-input-port) port))
  (assert-eq? (tn) #t (eqv? port (current-input-port)))
  (assert-eq? (tn) #f (eqv? (current-output-port) port))
  (assert-eq? (tn) #f (eqv? port (current-output-port))))

(tn "eqv? pair")
(if (provided? "sigscheme")
    (begin
      (assert-eq? (tn) #f (eqv? '(#t . #t) '(#t . #t)))
      (assert-eq? (tn) #f (eqv? '(#f . #t) '(#f . #t)))
      (assert-eq? (tn) #f (eqv? '(#t . #f) '(#t . #f)))
      (assert-eq? (tn) #f (eqv? '(#f . #t) '(#t . #f)))
      (assert-eq? (tn) #f (eqv? '(#\a . #\a) '(#\a . #\a)))
      (assert-eq? (tn) #f (eqv? '(#\a . #\b) '(#\a . #\b)))
      (assert-eq? (tn) #f (eqv? '(#\b . #\a) '(#\b . #\a)))
      (assert-eq? (tn) #f (eqv? '(#\a . #\b) '(#\b . #\a)))
      (assert-eq? (tn) #f (eqv? '("a" . "a") '("a" . "a")))
      (assert-eq? (tn) #f (eqv? '("a" . "b") '("a" . "b")))
      (assert-eq? (tn) #f (eqv? '("b" . "a") '("b" . "a")))
      (assert-eq? (tn) #f (eqv? '("a" . "b") '("b" . "a")))))

(assert-eq? (tn) #f (eqv? (cons #t #t) (cons #t #t)))
(assert-eq? (tn) #f (eqv? (cons #f #t) (cons #f #t)))
(assert-eq? (tn) #f (eqv? (cons #t #f) (cons #t #f)))
(assert-eq? (tn) #f (eqv? (cons #f #t) (cons #t #f)))
(assert-eq? (tn) #f (eqv? (cons #\a #\a) (cons #\a #\a)))
(assert-eq? (tn) #f (eqv? (cons #\a #\b) (cons #\a #\b)))
(assert-eq? (tn) #f (eqv? (cons #\b #\a) (cons #\b #\a)))
(assert-eq? (tn) #f (eqv? (cons #\a #\b) (cons #\b #\a)))
(assert-eq? (tn) #f (eqv? (cons "a" "a") (cons "a" "a")))
(assert-eq? (tn) #f (eqv? (cons "a" "b") (cons "a" "b")))
(assert-eq? (tn) #f (eqv? (cons "b" "a") (cons "b" "a")))
(assert-eq? (tn) #f (eqv? (cons "a" "b") (cons "b" "a")))

(tn "eqv? list")
(if (provided? "sigscheme")
    (begin
      (assert-eq? (tn) #f (eqv? '(#f) '(#f)))
      (assert-eq? (tn) #f (eqv? '(#f) '(#t)))
      (assert-eq? (tn) #f (eqv? '(#t) '(#f)))
      (assert-eq? (tn) #f (eqv? '(#t) '(#t)))
      (assert-eq? (tn) #f (eqv? '((#f)) '((#f))))
      (assert-eq? (tn) #f (eqv? '((#f)) '((#t))))
      (assert-eq? (tn) #f (eqv? '((#t)) '((#f))))
      (assert-eq? (tn) #f (eqv? '((#t)) '((#t))))
      (assert-eq? (tn) #f (eqv? '(1) '(1)))
      (assert-eq? (tn) #f (eqv? '(1) '(0)))
      (assert-eq? (tn) #f (eqv? '(1 3 5 0 13)
                                '(1 3 5 0 13)))
      (assert-eq? (tn) #f (eqv? '(1 3 2 0 13)
                                '(1 3 5 0 13)))
      (assert-eq? (tn) #f (eqv? '(1 3 (5 0 13))
                                '(1 3 (5 0 13))))
      (assert-eq? (tn) #f (eqv? '(1 3 (2 0 13))
                                '(1 3 (5 0 13))))
      (assert-eq? (tn) #f (eqv? '((1)) '((1))))
      (assert-eq? (tn) #f (eqv? '((1)) '((0))))
      (assert-eq? (tn) #f (eqv? '((1) (3) (5) (0) (13))
                                '((1) (3) (5) (0) (13))))
      (assert-eq? (tn) #f (eqv? '((1) (3) (2) (0) (13))
                                '((1) (3) (5) (0) (13))))
      (assert-eq? (tn) #f (eqv? '(#\a) '(#\a)))
      (assert-eq? (tn) #f (eqv? '(#\a) '(#\b)))
      (assert-eq? (tn) #f (eqv? '(#\b) '(#\a)))
      (assert-eq? (tn) #f (eqv? '((#\a)) '((#\a))))
      (assert-eq? (tn) #f (eqv? '((#\a)) '((#\b))))
      (assert-eq? (tn) #f (eqv? '((#\b)) '((#\a))))))

(assert-eq? (tn) #f (eqv? (list #f) (list #f)))
(assert-eq? (tn) #f (eqv? (list #f) (list #t)))
(assert-eq? (tn) #f (eqv? (list #t) (list #f)))
(assert-eq? (tn) #f (eqv? (list #t) (list #t)))
(assert-eq? (tn) #f (eqv? (list (list #f)) (list (list #f))))
(assert-eq? (tn) #f (eqv? (list (list #f)) (list (list #t))))
(assert-eq? (tn) #f (eqv? (list (list #t)) (list (list #f))))
(assert-eq? (tn) #f (eqv? (list (list #t)) (list (list #t))))
(assert-eq? (tn) #f (eqv? (list 1) (list 1)))
(assert-eq? (tn) #f (eqv? (list 1) (list 0)))
(assert-eq? (tn) #f (eqv? (list 1 3 5 0 13)
                          (list 1 3 5 0 13)))
(assert-eq? (tn) #f (eqv? (list 1 3 2 0 13)
                          (list 1 3 5 0 13)))
(assert-eq? (tn) #f (eqv? (list 1 3 (list 5 0 13))
                          (list 1 3 (list 5 0 13))))
(assert-eq? (tn) #f (eqv? (list 1 3 (list 2 0 13))
                          (list 1 3 (list 5 0 13))))
(assert-eq? (tn) #f (eqv? (list (list 1)) (list (list 1))))
(assert-eq? (tn) #f (eqv? (list (list 1)) (list (list 0))))
(assert-eq? (tn) #f (eqv? (list (list 1) (list 3) (list 5) (list 0) (list 13))
                          (list (list 1) (list 3) (list 5) (list 0) (list 13))))
(assert-eq? (tn) #f (eqv? (list (list 1) (list 3) (list 2) (list 0) (list 13))
                          (list (list 1) (list 3) (list 5) (list 0) (list 13))))
(assert-eq? (tn) #f (eqv? (list #\a) (list #\a)))
(assert-eq? (tn) #f (eqv? (list #\a) (list #\b)))
(assert-eq? (tn) #f (eqv? (list #\b) (list #\a)))
(assert-eq? (tn) #f (eqv? (list (list #\a)) (list (list #\a))))
(assert-eq? (tn) #f (eqv? (list (list #\a)) (list (list #\b))))
(assert-eq? (tn) #f (eqv? (list (list #\b)) (list (list #\a))))

(if (provided? "sigscheme")
    (begin
      (assert-eq? (tn) #f (eqv? '("") '("")))
      (assert-eq? (tn) #f (eqv? '(("")) '((""))))
      (assert-eq? (tn) #f (eqv? '("aBc12!")
                                '("aBc12!")))
      (assert-eq? (tn) #f (eqv? '("あ0イう12!")
                                '("あ0イう12!")))
      (assert-eq? (tn) #f (eqv? '("a" "" "aB1" ("3c" "d") "a")
                                '("a" "" "aB1" ("3c" "d") "a")))
      (assert-eq? (tn) #f (eqv? '(("aBc12!"))
                                '(("aBc12!"))))
      (assert-eq? (tn) #f (eqv? '(("あ0イう12!"))
                                '(("あ0イう12!"))))))

(assert-eq? (tn) #f (eqv? (list "") (list "")))
(assert-eq? (tn) #f (eqv? (list (list "")) (list (list ""))))
(assert-eq? (tn) #f (eqv? (list "aBc12!")
                          (list "aBc12!")))
(assert-eq? (tn) #f (eqv? (list "あ0イう12!")
                          (list "あ0イう12!")))
(assert-eq? (tn) #f (eqv? (list "a" "" "aB1" (list "3c" "d") "a")
                          (list "a" "" "aB1" (list "3c" "d") "a")))
(assert-eq? (tn) #f (eqv? (list (list "aBc12!"))
                          (list (list "aBc12!"))))
(assert-eq? (tn) #f (eqv? (list (list "あ0イう12!"))
                          (list (list "あ0イう12!"))))

(if (provided? "sigscheme")
    (begin
      (assert-eq? (tn) #f (eqv? '("aBc123!")
                                '("aBc12!")))
      (assert-eq? (tn) #f (eqv? '("あ0イぅ12!")
                                '("あ0イう12!")))
      (assert-eq? (tn) #f (eqv? '("a" "" "aB1" ("3c" "e") "a")
                                '("a" "" "aB1" ("3c" "d") "a")))
      (assert-eq? (tn) #f (eqv? '(("aBc123!"))
                                '(("aBc12!"))))
      (assert-eq? (tn) #f (eqv? '(("あ0イぅ12!"))
                                '(("あ0イう12!"))))))

(assert-eq? (tn) #f (eqv? (list "aBc123!")
                          (list "aBc12!")))
(assert-eq? (tn) #f (eqv? (list "あ0イぅ12!")
                          (list "あ0イう12!")))
(assert-eq? (tn) #f (eqv? (list "a" "" "aB1" (list "3c" "e") "a")
                          (list "a" "" "aB1" (list "3c" "d") "a")))
(assert-eq? (tn) #f (eqv? (list (list "aBc123!"))
                          (list (list "aBc12!"))))
(assert-eq? (tn) #f (eqv? (list (list "あ0イぅ12!"))
                          (list (list "あ0イう12!"))))

(if (provided? "sigscheme")
    (begin
      (assert-eq? (tn) #f
                  (eqv? '(0 #\a "" #("vE" -1 (#\?)) 23 + "aBc" (-1 #\b ("Ls")) #t)
                        '(0 #\a "" #("vE" -1 (#\?)) 23 + "aBc" (-1 #\b ("Ls")) #t)))
      (assert-eq? (tn) #f
                  (eqv? '(0 #\a "" #("vE" -1 (#\?)) 23 + "aBc" (-1 #\b ("Ls")) #t)
                        '(0 #\a "" #("vE" -1 (#\?)) 23 + "aBc" (-1 #\b ("L")) #t)))
      (assert-eq? (tn) #f
                  (eqv? '(0 #\a "" #("vE" -1 (#\?)) 23 + "aBc" (-1 #\b ("Ls")) #t)
                        '(0 #\a "" ("vE" -1 (#\?)) 23 + "aBc" (-1 #\b ("Ls")) #t)))
      (assert-eq? (tn) #f
                  (eqv? '(0 #\a "" #("vE" -1 (#\?)) 23 + "aBc" (-1 #\b ("Ls")) #t)
                        '(0 #\a "" #("vE" -1 (#\?)) 23 + "aBc" #t)))
      (assert-eq? (tn) #f
                  (eqv? '(0 #\a "" #("vE" -1 (#\?)) 23 + "aBc" #t)
                        '(0 #\a "" #("vE" -1 (#\?)) 23 + "aBc" (-1 #\b ("Ls")) #t)))))

(assert-eq? (tn) #f
            (eqv? (list 0 #\a "" (vector "vE" -1 '(#\?)) 23 + "aBc" (list -1 #\b '("Ls")) #t)
                  (list 0 #\a "" (vector "vE" -1 '(#\?)) 23 + "aBc" (list -1 #\b '("Ls")) #t)))
(assert-eq? (tn) #f
            (eqv? (list 0 #\a "" (vector "vE" -1 '(#\?)) 23 + "aBc" (list -1 #\b '("Ls")) #t)
                  (list 0 #\a "" (vector "vE" -1 '(#\?)) 23 + "aBc" (list -1 #\b '("L")) #t)))
(assert-eq? (tn) #f
            (eqv? (list 0 #\a "" (vector "vE" -1 '(#\?)) 23 + "aBc" (list -1 #\b '("Ls")) #t)
                  (list 0 #\a "" (list "vE" -1 '(#\?))   23 + "aBc" (list -1 #\b '("Ls")) #t)))
(assert-eq? (tn) #f
            (eqv? (list 0 #\a "" (vector "vE" -1 '(#\?)) 23 + "aBc" (list -1 #\b '("Ls")) #t)
                  (list 0 #\a "" (vector "vE" -1 '(#\?)) 23 + "aBc" #t)))
(assert-eq? (tn) #f
            (eqv? (list 0 #\a "" (vector "vE" -1 '(#\?)) 23 + "aBc" #t)
                  (list 0 #\a "" (vector "vE" -1 '(#\?)) 23 + "aBc" (list -1 #\b '("Ls")) #t)))

(tn "eqv? empty vector")
(if (provided? "sigscheme")
    (assert-eq? (tn) #f (eqv? '#() '#())))
(assert-eq? (tn) #f (eqv? (vector) (vector)))

(let ((v1 '#())
      (v2 (vector)))
  (assert-eq? (tn) #t (eqv? v1 v1))
  (assert-eq? (tn) #t (eqv? v2 v2))
  (assert-eq? (tn) #f (eqv? v1 v2)))

(tn "eqv? vector")
(if (provided? "sigscheme")
    (begin
      (assert-eq? (tn) #f (eqv? '#(#f) '#(#f)))
      (assert-eq? (tn) #f (eqv? '#(#f) '#(#t)))
      (assert-eq? (tn) #f (eqv? '#(#t) '#(#f)))
      (assert-eq? (tn) #f (eqv? '#(#t) '#(#t)))
      (assert-eq? (tn) #f (eqv? '#(#(#f)) '#(#(#f))))
      (assert-eq? (tn) #f (eqv? '#(#(#f)) '#(#(#t))))
      (assert-eq? (tn) #f (eqv? '#(#(#t)) '#(#(#f))))
      (assert-eq? (tn) #f (eqv? '#(#(#t)) '#(#(#t))))
      (assert-eq? (tn) #f (eqv? '#(1) '#(1)))
      (assert-eq? (tn) #f (eqv? '#(1) '#(0)))
      (assert-eq? (tn) #f (eqv? '#(1 3 5 0 13)
                                '#(1 3 5 0 13)))
      (assert-eq? (tn) #f (eqv? '#(1 3 2 0 13)
                                '#(1 3 5 0 13)))
      (assert-eq? (tn) #f (eqv? '#(1 3 #(5 0 13))
                                '#(1 3 #(5 0 13))))
      (assert-eq? (tn) #f (eqv? '#(1 3 #(2 0 13))
                                '#(1 3 #(5 0 13))))
      (assert-eq? (tn) #f (eqv? '#(#(1)) '#(#(1))))
      (assert-eq? (tn) #f (eqv? '#(#(1)) '#(#(0))))
      (assert-eq? (tn) #f (eqv? '#(#(1) #(3) #(5) #(0) #(13))
                                '#(#(1) #(3) #(5) #(0) #(13))))
      (assert-eq? (tn) #f (eqv? '#(#(1) #(3) #(2) #(0) #(13))
                                '#(#(1) #(3) #(5) #(0) #(13))))
      (assert-eq? (tn) #f (eqv? '#(#\a) '#(#\a)))
      (assert-eq? (tn) #f (eqv? '#(#\a) '#(#\b)))
      (assert-eq? (tn) #f (eqv? '#(#\b) '#(#\a)))
      (assert-eq? (tn) #f (eqv? '#(#(#\a)) '#(#(#\a))))
      (assert-eq? (tn) #f (eqv? '#(#(#\a)) '#(#(#\b))))
      (assert-eq? (tn) #f (eqv? '#(#(#\b)) '#(#(#\a))))))

(assert-eq? (tn) #f (eqv? (vector #f) (vector #f)))
(assert-eq? (tn) #f (eqv? (vector #f) (vector #t)))
(assert-eq? (tn) #f (eqv? (vector #t) (vector #f)))
(assert-eq? (tn) #f (eqv? (vector #t) (vector #t)))
(assert-eq? (tn) #f (eqv? (vector (vector #f)) (vector (vector #f))))
(assert-eq? (tn) #f (eqv? (vector (vector #f)) (vector (vector #t))))
(assert-eq? (tn) #f (eqv? (vector (vector #t)) (vector (vector #f))))
(assert-eq? (tn) #f (eqv? (vector (vector #t)) (vector (vector #t))))
(assert-eq? (tn) #f (eqv? (vector 1) (vector 1)))
(assert-eq? (tn) #f (eqv? (vector 1) (vector 0)))
(assert-eq? (tn) #f (eqv? (vector 1 3 5 0 13)
                          (vector 1 3 5 0 13)))
(assert-eq? (tn) #f (eqv? (vector 1 3 2 0 13)
                          (vector 1 3 5 0 13)))
(assert-eq? (tn) #f (eqv? (vector 1 3 (vector 5 0 13))
                          (vector 1 3 (vector 5 0 13))))
(assert-eq? (tn) #f (eqv? (vector 1 3 (vector 2 0 13))
                          (vector 1 3 (vector 5 0 13))))
(assert-eq? (tn) #f (eqv? (vector (vector 1)) (vector (vector 1))))
(assert-eq? (tn) #f (eqv? (vector (vector 1)) (vector (vector 0))))
(assert-eq? (tn) #f (eqv? (vector (vector 1) (vector 3) (vector 5) (vector 0) (vector 13))
                          (vector (vector 1) (vector 3) (vector 5) (vector 0) (vector 13))))
(assert-eq? (tn) #f (eqv? (vector (vector 1) (vector 3) (vector 2) (vector 0) (vector 13))
                          (vector (vector 1) (vector 3) (vector 5) (vector 0) (vector 13))))
(assert-eq? (tn) #f (eqv? (vector #\a) (vector #\a)))
(assert-eq? (tn) #f (eqv? (vector #\a) (vector #\b)))
(assert-eq? (tn) #f (eqv? (vector #\b) (vector #\a)))
(assert-eq? (tn) #f (eqv? (vector (vector #\a)) (vector (vector #\a))))
(assert-eq? (tn) #f (eqv? (vector (vector #\a)) (vector (vector #\b))))
(assert-eq? (tn) #f (eqv? (vector (vector #\b)) (vector (vector #\a))))

(if (provided? "sigscheme")
    (begin
      (assert-eq? (tn) #f (eqv? '#("") '#("")))
      (assert-eq? (tn) #f (eqv? '#(#("")) '#(#(""))))
      (assert-eq? (tn) #f (eqv? '#("aBc12!")
                                '#("aBc12!")))
      (assert-eq? (tn) #f (eqv? '#("あ0イう12!")
                                '#("あ0イう12!")))
      (assert-eq? (tn) #f (eqv? '#("a" "" "aB1" #("3c" "d") "a")
                                '#("a" "" "aB1" #("3c" "d") "a")))
      (assert-eq? (tn) #f (eqv? '#(#("aBc12!"))
                                '#(#("aBc12!"))))
      (assert-eq? (tn) #f (eqv? '#(#("あ0イう12!"))
                                '#(#("あ0イう12!"))))))

(assert-eq? (tn) #f (eqv? (vector "") (vector "")))
(assert-eq? (tn) #f (eqv? (vector (vector "")) (vector (vector ""))))
(assert-eq? (tn) #f (eqv? (vector "aBc12!")
                          (vector "aBc12!")))
(assert-eq? (tn) #f (eqv? (vector "あ0イう12!")
                          (vector "あ0イう12!")))
(assert-eq? (tn) #f (eqv? (vector "a" "" "aB1" (vector "3c" "d") "a")
                          (vector "a" "" "aB1" (vector "3c" "d") "a")))
(assert-eq? (tn) #f (eqv? (vector (vector "aBc12!"))
                          (vector (vector "aBc12!"))))
(assert-eq? (tn) #f (eqv? (vector (vector "あ0イう12!"))
                          (vector (vector "あ0イう12!"))))

(if (provided? "sigscheme")
    (begin
      (assert-eq? (tn) #f (eqv? '#("aBc123!")
                                '#("aBc12!")))
      (assert-eq? (tn) #f (eqv? '#("あ0イぅ12!")
                                '#("あ0イう12!")))
      (assert-eq? (tn) #f (eqv? '#("a" "" "aB1" #("3c" "e") "a")
                                '#("a" "" "aB1" #("3c" "d") "a")))
      (assert-eq? (tn) #f (eqv? '#(#("aBc123!"))
                                '#(#("aBc12!"))))
      (assert-eq? (tn) #f (eqv? '#(#("あ0イぅ12!"))
                                '#(#("あ0イう12!"))))))

(assert-eq? (tn) #f (eqv? (vector "aBc123!")
                          (vector "aBc12!")))
(assert-eq? (tn) #f (eqv? (vector "あ0イぅ12!")
                          (vector "あ0イう12!")))
(assert-eq? (tn) #f (eqv? (vector "a" "" "aB1" (vector "3c" "e") "a")
                          (vector "a" "" "aB1" (vector "3c" "d") "a")))
(assert-eq? (tn) #f (eqv? (vector (vector "aBc123!"))
                          (vector (vector "aBc12!"))))
(assert-eq? (tn) #f (eqv? (vector (vector "あ0イぅ12!"))
                          (vector (vector "あ0イう12!"))))

(if (provided? "sigscheme")
    (begin
      (assert-eq? (tn) #f
                  (eqv? '#(0 #\a "" #("vE" -1 (#\?)) 23 + "aBc" (-1 #\b ("Ls")) #t)
                        '#(0 #\a "" #("vE" -1 (#\?)) 23 + "aBc" (-1 #\b ("Ls")) #t)))
      (assert-eq? (tn) #f
                  (eqv? '#(0 #\a "" #("vE" -1 (#\?)) 23 + "aBc" (-1 #\b ("Ls")) #t)
                        '#(0 #\a "" #("vE" -1 (#\?)) 23 + "aBc" (-1 #\b ("L")) #t)))
      (assert-eq? (tn) #f
                  (eqv? '#(0 #\a "" #("vE" -1 (#\?)) 23 + "aBc" (-1 #\b ("Ls")) #t)
                        '#(0 #\a "" ("vE" -1 (#\?)) 23 + "aBc" (-1 #\b ("Ls")) #t)))
      (assert-eq? (tn) #f
                  (eqv? '#(0 #\a "" #("vE" -1 (#\?)) 23 + "aBc" (-1 #\b ("Ls")) #t)
                        '#(0 #\a "" #("vE" -1 (#\?)) 23 + "aBc" #t)))
      (assert-eq? (tn) #f
                  (eqv? '#(0 #\a "" #("vE" -1 (#\?)) 23 + "aBc" #t)
                        '#(0 #\a "" #("vE" -1 (#\?)) 23 + "aBc" (-1 #\b ("Ls")) #t)))))

(assert-eq? (tn) #f
            (eqv? (vector 0 #\a "" (vector "vE" -1 '(#\?)) 23 + "aBc" (vector -1 #\b '("Ls")) #t)
                  (vector 0 #\a "" (vector "vE" -1 '(#\?)) 23 + "aBc" (vector -1 #\b '("Ls")) #t)))
(assert-eq? (tn) #f
            (eqv? (vector 0 #\a "" (vector "vE" -1 '(#\?)) 23 + "aBc" (vector -1 #\b '("Ls")) #t)
                  (vector 0 #\a "" (vector "vE" -1 '(#\?)) 23 + "aBc" (vector -1 #\b '("L")) #t)))
(assert-eq? (tn) #f
            (eqv? (vector 0 #\a "" (vector "vE" -1 '(#\?)) 23 + "aBc" (vector -1 #\b '("Ls")) #t)
                  (vector 0 #\a "" (list "vE" -1 '(#\?))   23 + "aBc" (vector -1 #\b '("Ls")) #t)))
(assert-eq? (tn) #f
            (eqv? (vector 0 #\a "" (vector "vE" -1 '(#\?)) 23 + "aBc" (vector -1 #\b '("Ls")) #t)
                  (vector 0 #\a "" (vector "vE" -1 '(#\?)) 23 + "aBc" #t)))
(assert-eq? (tn) #f
            (eqv? (vector 0 #\a "" (vector "vE" -1 '(#\?)) 23 + "aBc" #t)
                  (vector 0 #\a "" (vector "vE" -1 '(#\?)) 23 + "aBc" (vector -1 #\b '("Ls")) #t)))


(total-report)
