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

;; To sense boolean values accurately, these tests use '(assert-true (if <exp>
;; #t #f))' form to test a boolean expression instead of '(assert-true <exp>)'
;; of '(assert-equal? #t <exp>)'.  -- YamaKen 2006-09-07

(load "test/unittest.scm")

(define tn test-name)

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

(call-with-current-continuation
 (lambda (k)
   (assert-true   (tn) (if k #t #f))))
(assert-true   (tn) (if (current-output-port) #t #f))
(assert-true   (tn) (if '(#t . #t) #t #f))
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
(assert-true   (tn) (eq? #t (not #f)))
(assert-true   (tn) (eq? #f (not #t)))
(if (and (provided? "sigscheme")
         (provided? "siod-bugs"))
    (assert-true   (tn) (eq? #t (not '())))
    (assert-true   (tn) (eq? #f (not '()))))
(if (provided? "sigscheme")
    (begin
      (assert-true   (tn) (eq? #f (not (eof))))
      (assert-true   (tn) (eq? #f (not (undef))))))
(assert-true   (tn) (eq? #f (not 0)))
(assert-true   (tn) (eq? #f (not 1)))
(assert-true   (tn) (eq? #f (not 3)))
(assert-true   (tn) (eq? #f (not -1)))
(assert-true   (tn) (eq? #f (not -3)))
(assert-true   (tn) (eq? #f (not 'symbol)))
(assert-true   (tn) (eq? #f (not 'SYMBOL)))
(assert-true   (tn) (eq? #f (not #\a)))
(assert-true   (tn) (eq? #f (not #\あ)))
(assert-true   (tn) (eq? #f (not "")))
(assert-true   (tn) (eq? #f (not " ")))
(assert-true   (tn) (eq? #f (not "a")))
(assert-true   (tn) (eq? #f (not "A")))
(assert-true   (tn) (eq? #f (not "aBc12!")))
(assert-true   (tn) (eq? #f (not "あ")))
(assert-true   (tn) (eq? #f (not "あ0イう12!")))
(assert-true   (tn) (eq? #f (not +)))
(assert-true   (tn) (eq? #f (not (lambda () #t))))

;; syntactic keywords should not be appeared as operand

(call-with-current-continuation
 (lambda (k)
   (assert-true   (tn) (eq? #f (not k)))))
(assert-true   (tn) (eq? #f (not (current-output-port))))
(assert-true   (tn) (eq? #f (not '(#t . #t))))
(assert-true   (tn) (eq? #f (not '(0 1 2))))
(assert-true   (tn) (eq? #f (not (list 0 1 2))))
(assert-true   (tn) (eq? #f (not '#())))
(assert-true   (tn) (eq? #f (not (vector))))
(assert-true   (tn) (eq? #f (not '#(0 1 2))))
(assert-true   (tn) (eq? #f (not (vector 0 1 2))))

(tn "boolean?")
(assert-true   (tn) (boolean? #f))
(assert-true   (tn) (boolean? #t))
(if (and (provided? "sigscheme")
         (provided? "siod-bugs"))
    (assert-true   (tn) (boolean? '()))
    (assert-false  (tn) (boolean? '())))
(if (provided? "sigscheme")
    (begin
      (assert-false  (tn) (boolean? (eof)))
      (assert-false  (tn) (boolean? (undef)))))
(assert-false  (tn) (boolean? 0))
(assert-false  (tn) (boolean? 1))
(assert-false  (tn) (boolean? 3))
(assert-false  (tn) (boolean? -1))
(assert-false  (tn) (boolean? -3))
(assert-false  (tn) (boolean? 'symbol))
(assert-false  (tn) (boolean? 'SYMBOL))
(assert-false  (tn) (boolean? #\a))
(assert-false  (tn) (boolean? #\あ))
(assert-false  (tn) (boolean? ""))
(assert-false  (tn) (boolean? " "))
(assert-false  (tn) (boolean? "a"))
(assert-false  (tn) (boolean? "A"))
(assert-false  (tn) (boolean? "aBc12!"))
(assert-false  (tn) (boolean? "あ"))
(assert-false  (tn) (boolean? "あ0イう12!"))
(assert-false  (tn) (boolean? +))
(assert-false  (tn) (boolean? (lambda () #t)))

;; syntactic keywords should not be appeared as operand

(call-with-current-continuation
 (lambda (k)
   (assert-false  (tn) (boolean? k))))
(assert-false  (tn) (boolean? (current-output-port)))
(assert-false  (tn) (boolean? '(#t . #t)))
(assert-false  (tn) (boolean? '(0 1 2)))
(assert-false  (tn) (boolean? (list 0 1 2)))
(assert-false  (tn) (boolean? '#()))
(assert-false  (tn) (boolean? (vector)))
(assert-false  (tn) (boolean? '#(0 1 2)))
(assert-false  (tn) (boolean? (vector 0 1 2)))

(total-report)
