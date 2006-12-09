#! /usr/bin/env sscm -C UTF-8
;; -*- buffer-file-coding-system: utf-8 -*-

;;  Filename : test-bool.scm
;;  About    : unit tests for boolean
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

;; To sense boolean values accurately, these tests use '(assert-true (if <exp>
;; #t #f))' form to test a boolean expression instead of '(assert-true <exp>)'
;; of '(assert-equal? #t <exp>)'.  -- YamaKen 2006-09-07
(tn "R5RS upper-case boolean literal")
(if (provided? "sigscheme")
    (begin
      ;; not supported by SigScheme
      (assert-parse-error (tn) "#F")
      (assert-parse-error (tn) "#T"))
    (begin
      (assert-false (tn) (if (string-read "#F") #t #f))
      (assert-true  (tn) (if (string-read "#T") #t #f))))

(tn "boolean self-evaluation")
(assert-true   (tn) (eq? #f '#f))
(assert-true   (tn) (eq? #t '#t))

(tn "boolean values")
(assert-false  (tn) (if #f #t #f))
(assert-true   (tn) (if #t #t #f))
(if (and (provided? "sigscheme")
         (provided? "siod-bugs"))
    (begin
      (assert-false (tn) '())
      (assert-true  (tn) (eq? #f '())))
    (begin
      (assert-true  (tn) '())
      (assert-false (tn) (eq? #f '()))))
(if (provided? "sigscheme")
    (begin
      (assert-true   (tn) (if (eof) #t #f))
      (assert-true   (tn) (if (undef) #t #f))))
(assert-true   (tn) (if 0 #t #f))
(assert-true   (tn) (if 1 #t #f))
(assert-true   (tn) (if 3 #t #f))
(assert-true   (tn) (if -1 #t #f))
(assert-true   (tn) (if -3 #t #f))
(assert-true   (tn) (if 'symbol #t #f))
(assert-true   (tn) (if 'SYMBOL #t #f))
(assert-true   (tn) (if #\a #t #f))
(assert-true   (tn) (if #\あ #t #f))
(assert-true   (tn) (if "" #t #f))
(assert-true   (tn) (if " " #t #f))
(assert-true   (tn) (if "a" #t #f))
(assert-true   (tn) (if "A" #t #f))
(assert-true   (tn) (if "aBc12!" #t #f))
(assert-true   (tn) (if "あ" #t #f))
(assert-true   (tn) (if "あ0イう12!" #t #f))
(assert-true   (tn) (if + #t #f))
(assert-true   (tn) (if (lambda () #t) #t #f))

;; syntactic keywords should not be appeared as operand
(if sigscheme?
    (begin
      ;; pure syntactic keyword
      (assert-error (tn) (lambda () (if else #t #f)))
      ;; expression keyword
      (assert-error (tn) (lambda () (if do #t #f)))))

(call-with-current-continuation
 (lambda (k)
   (assert-true   (tn) (if k #t #f))))
(assert-true   (tn) (if (current-output-port) #t #f))
(assert-true   (tn) (if '(#t . #t) #t #f))
(assert-true   (tn) (if (cons #t #t) #t #f))
(assert-true   (tn) (if '(0 1 2) #t #f))
(assert-true   (tn) (if (list 0 1 2) #t #f))
(assert-true   (tn) (if '#() #t #f))
(assert-true   (tn) (if (vector) #t #f))
(assert-true   (tn) (if '#(0 1 2) #t #f))
(assert-true   (tn) (if (vector 0 1 2) #t #f))

(tn "not")
;; 'not' must return exact #t
;; > R5RS: 6.3 Other data types
;; > `Not' returns #t if obj is false, and returns #f otherwise.
(assert-eq? (tn) #t (not #f))
(assert-eq? (tn) #f (not #t))
(if (and (provided? "sigscheme")
         (provided? "siod-bugs"))
    (assert-eq? (tn) #t (not '()))
    (assert-eq? (tn) #f (not '())))
(if (provided? "sigscheme")
    (begin
      (assert-eq? (tn) #f (not (eof)))
      (assert-eq? (tn) #f (not (undef)))))
(assert-eq? (tn) #f (not 0))
(assert-eq? (tn) #f (not 1))
(assert-eq? (tn) #f (not 3))
(assert-eq? (tn) #f (not -1))
(assert-eq? (tn) #f (not -3))
(assert-eq? (tn) #f (not 'symbol))
(assert-eq? (tn) #f (not 'SYMBOL))
(assert-eq? (tn) #f (not #\a))
(assert-eq? (tn) #f (not #\あ))
(assert-eq? (tn) #f (not ""))
(assert-eq? (tn) #f (not " "))
(assert-eq? (tn) #f (not "a"))
(assert-eq? (tn) #f (not "A"))
(assert-eq? (tn) #f (not "aBc12!"))
(assert-eq? (tn) #f (not "あ"))
(assert-eq? (tn) #f (not "あ0イう12!"))
(assert-eq? (tn) #f (not +))
(assert-eq? (tn) #f (not (lambda () #t)))

;; syntactic keywords should not be appeared as operand
(if sigscheme?
    (begin
      ;; pure syntactic keyword
      (assert-error (tn) (lambda () (not else)))
      ;; expression keyword
      (assert-error (tn) (lambda () (not do)))))

(call-with-current-continuation
 (lambda (k)
   (assert-eq? (tn) #f (not k))))
(assert-eq? (tn) #f (not (current-output-port)))
(assert-eq? (tn) #f (not '(#t . #t)))
(assert-eq? (tn) #f (not (cons #t #t)))
(assert-eq? (tn) #f (not '(0 1 2)))
(assert-eq? (tn) #f (not (list 0 1 2)))
(assert-eq? (tn) #f (not '#()))
(assert-eq? (tn) #f (not (vector)))
(assert-eq? (tn) #f (not '#(0 1 2)))
(assert-eq? (tn) #f (not (vector 0 1 2)))

(tn "boolean?")
(assert-eq? (tn) #t (boolean? #f))
(assert-eq? (tn) #t (boolean? #t))
(if (and (provided? "sigscheme")
         (provided? "siod-bugs"))
    (assert-eq? (tn) #t (boolean? '()))
    (assert-eq? (tn) #f (boolean? '())))
(if (provided? "sigscheme")
    (begin
      (assert-eq? (tn) #f (boolean? (eof)))
      (assert-eq? (tn) #f (boolean? (undef)))))
(assert-eq? (tn) #f (boolean? 0))
(assert-eq? (tn) #f (boolean? 1))
(assert-eq? (tn) #f (boolean? 3))
(assert-eq? (tn) #f (boolean? -1))
(assert-eq? (tn) #f (boolean? -3))
(assert-eq? (tn) #f (boolean? 'symbol))
(assert-eq? (tn) #f (boolean? 'SYMBOL))
(assert-eq? (tn) #f (boolean? #\a))
(assert-eq? (tn) #f (boolean? #\あ))
(assert-eq? (tn) #f (boolean? ""))
(assert-eq? (tn) #f (boolean? " "))
(assert-eq? (tn) #f (boolean? "a"))
(assert-eq? (tn) #f (boolean? "A"))
(assert-eq? (tn) #f (boolean? "aBc12!"))
(assert-eq? (tn) #f (boolean? "あ"))
(assert-eq? (tn) #f (boolean? "あ0イう12!"))
(assert-eq? (tn) #f (boolean? +))
(assert-eq? (tn) #f (boolean? (lambda () #t)))

;; syntactic keywords should not be appeared as operand
(if sigscheme?
    (begin
      ;; pure syntactic keyword
      (assert-error (tn) (lambda () (boolean? else)))
      ;; expression keyword
      (assert-error (tn) (lambda () (boolean? do)))))

(call-with-current-continuation
 (lambda (k)
   (assert-eq? (tn) #f (boolean? k))))
(assert-eq? (tn) #f (boolean? (current-output-port)))
(assert-eq? (tn) #f (boolean? '(#t . #t)))
(assert-eq? (tn) #f (boolean? (cons #t #t)))
(assert-eq? (tn) #f (boolean? '(0 1 2)))
(assert-eq? (tn) #f (boolean? (list 0 1 2)))
(assert-eq? (tn) #f (boolean? '#()))
(assert-eq? (tn) #f (boolean? (vector)))
(assert-eq? (tn) #f (boolean? '#(0 1 2)))
(assert-eq? (tn) #f (boolean? (vector 0 1 2)))

(total-report)
