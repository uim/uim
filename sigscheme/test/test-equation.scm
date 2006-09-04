#! /usr/bin/env sscm -C EUC-JP
;; -*- buffer-file-coding-system: euc-jp -*-

;;  Filename : test-equation.scm
;;  About    : unit test for equations
;;
;;  Copyright (C) 2005-2006 Kazuki Ohta <mover AT hct.zaq.ne.jp>
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

;; THIS TEST HAS BEEN DEPRECATED. USE test-{eq,eqv,equal}.scm INSTEAD.
;;   -- YamaKen 2006-09-04

(load "test/unittest.scm")

(define closure1 (lambda (x) x))
(define closure2 (lambda (x) x))

;; check eqv?
(assert-true"eqv? #1" (eqv? #t #t))
(assert-true"eqv? #2" (eqv? #f #f))
(assert-true"eqv? #3" (eqv? 'abc 'abc))
(assert-true"eqv? #3"  (string=? (symbol->string 'obj)
			     (symbol->string 'obj)))
(assert-true"eqv? #4" (eqv? -1 -1))
(assert-true"eqv? #4" (eqv? 0 0))
(assert-true"eqv? #5" (eqv? #\a #\a))
(assert-true"eqv? #5" (eqv? #\あ #\あ))

(let ((f (lambda (x) x + 1)))
  (assert-true"eqv? #6" (eqv? f f)))
(let ((f (lambda (x) x + 1))
      (g (lambda (x) x + 2)))
  (assert-true"eqv? #6" (not (eqv? f g))))
(let ((s1 "abc")
      (s2 "abc"))
  (assert-true"eqv? #6" (not (eqv? s1 s2))))
(assert-true"eqv? #6" (not (eqv? (cons 1 2) (cons 1 2))))
(assert-true"eqv? #6" (not (eqv? #f 'nil)))

(assert-true"eqv? #7" (not (eqv? #t #f)))
(assert-true"eqv? #7" (not (eqv? "abc" 'abc)))
(assert-true"eqv? #7" (not (eqv? 'ab 'ba)))
(assert-true"eqv? #7" (not (eqv? #\a #\b)))
(assert-true"eqv? #7" (not (eqv? #\あ #\い)))
(assert-true"eqv? #7" (not (eqv? '() '(())
			          )))

(assert-true  "eqv? #8 procedures" (eqv? + +))
(assert-false "eqv? #8 procedures" (eqv? + -))
(assert-false "eqv? #8 procedures" (eqv? + closure1))
(assert-true  "eqv? #8 procedures" (eqv? closure1 closure1))
(assert-false "eqv? #8 procedures" (eqv? closure1 closure2))

;; TODO: add tests for port and continuation

;; check eq?
;; FIXME: rewrite assert-equal? with assert
(assert-equal? "eq? check empty list" '() '())

(define pair1 (cons 'a 'b))
(define pair2 pair1)
(assert-equal? "eq? check cons" pair1 pair2)

(define str1 (string #\a))
(define str2 str1)
(assert-equal? "eq? check cons" str1 str2)

(assert-equal? "eq? check func" + +)

(assert-true  "eq? #5 procedures" (eq? + +))
(assert-false "eq? #5 procedures" (eq? + -))
(assert-false "eq? #5 procedures" (eq? + closure1))
(assert-true  "eq? #5 procedures" (eq? closure1 closure1))
(assert-false "eq? #5 procedures" (eq? closure1 closure2))

;; TODO: add tests for port and continuation

;; check equal?
(assert-true"basic equal? test1" (equal? 'a 'a))
(assert-true"basic equal? test2" (equal? '(a) '(a)))
(assert-true"basic equal? test3" (equal? '(a (b) c)
				     '(a (b) c)))
(assert-true"basic equal? test4" (equal? "abc" "abc"))
(assert-true"basic equal? test5" (equal? 2 2))
(assert-true"basic equal? test6" (equal? (make-vector 5 'a)
				     (make-vector 5 'a)))

(assert-true  "equal? #3 procedures" (equal? + +))
(assert-false "equal? #3 procedures" (equal? + -))
(assert-false "equal? #3 procedures" (equal? + closure1))
(assert-true  "equal? #3 procedures" (equal? closure1 closure1))
(assert-false "equal? #3 procedures" (equal? closure1 closure2))

;; TODO: add tests for port and continuation

(total-report)
