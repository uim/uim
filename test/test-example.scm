;;; Copyright (c) 2004-2013 uim Project https://github.com/uim/uim
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
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ``AS
;;; IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
;;; THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;;; PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR
;;; CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
;;; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
;;; OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
;;; ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;;

;; These tests are passed at revision 6605 (new repository)

(define-module test.test-example
  (use test.unit.test-case)
  (use test.uim-test))
(select-module test.test-example)

;; setup procedure is evaluated before each test to prepare
;; common testing environment for each test.
(define (setup)
  ;; (uim-test-setup) setup inferior uim-sh process up.
  ;; Don't omit it.
  (uim-test-setup)

  ;; User specific setup expressions are here.

  ;; The expressions are defined in gosh, not uim-sh.
  (use srfi-1)

  ;; The procedure 'uim-eval' can send an expression to inferior uim-sh
  ;; process.
  (uim-eval '(require "util.scm"))

  (uim-eval '(begin
               (define even?
                 (lambda (x)
                   (= (remainder x 2) 0)))
               (define odd?
                 (lambda (x)
                   (= (remainder x 2) 1)))))
  )

;; setup procedure is evaluated after each test to clean up
;; testing environment for each test.
(define (teardown)
  ;; (uim-test-teardown) tear inferior uim-sh process down.
  ;; Don't omit it.
  (uim-test-teardown)

  ;; User specific teardown expressions are here.
  ;; Normally, no expressions are needed.
  )

;; Define test. Inferior uim-sh process is newly prepared for each
;; test.
(define (test-+)
  ;; assert-uim-equal accepts <expect> <expression>. <expression> is sent to
  ;; the inferior uim-sh process, and receives the evaluated result.
  ;; Then gosh compares the two values.
  (assert-uim-equal 5 '(+ 2 3))
  (assert-uim-equal -1 '(+ 2 -3))

  ;; The last #f is just for inhibiting an optimization of gosh.
  ;; gosh optimize the last return location. So, backtrace for the last
  ;; assertion ("(assert-uim-equal -1 '(+ 2 -3))" in this test procedure)
  ;; is omitted when the last assertion is failed.
  #f)

;; another test in the testcase
(define (test--)
  (assert-uim-equal -1 '(- 2 3))
  (assert-uim-equal 5 '(- 2 -3))

  ;; When <extected> and the actual evaluated values are different,
  ;; it is countted as failure and reported.
  (assert-uim-equal 1 '(- 0 -1))

  #f)

(define (test-/)
  ;; 'assert-uim-error' asserts that the procedure passed causes an
  ;; error.
  (assert-uim-error '(/ 5 0))
  #f)

;; The procedures are defined in gosh, not uim-sh.
(define my-even?
  (lambda (x)
    (= (remainder x 2) 0)))
(define my-odd?
  (lambda (x)
    (= (remainder x 2) 1)))

;; Inferior uim-sh process is newly prepared and the setup procedure
;; is evaluated.
(define (test-even?)
  ;; Boolean tests must be used 'assert-uim-true' or
  ;; 'assert-uim-false' instead of 'assert-uim-equal' with
  ;; #t or #f. This is required to adapt SIOD's false value
  ;; '()' to #f.
  (assert-uim-true  '(even? 2))
  (assert-uim-false '(even? 3))
  #f)

;; Another uim-sh process is newly prepared and the setup procedure
;; is evaluated.
(define (test-odd?)
  (assert-uim-false '(odd? 2))
  (assert-uim-true  '(odd? 3))
  #f)

(define (test-odd?-#2)
  ;; another way to test the even?. This focuses on difference of
  ;; implementation's own behavior rather than correctness of
  ;; user-procedure definition.

  ;; Boolean tests without 'assert-uim-true' nor
  ;; 'assert-uim-false' must be wrapped into 'uim-bool'.
  ;; This is required to adapt SIOD's false value '()' to
  ;; #f. It will be unified into 'assert-uim-equal' once the
  ;; R5RS-compliant SigScheme has become default
  ;; implementation for uim.
  (assert-equal (my-odd? 2)
                (uim-bool '(odd? 2)))
  (assert-equal (my-odd? 3)
                (uim-bool '(odd? 3)))
  #f)

(define (test-difference-of-Gauche-and-uim)
  ;; developer can write complex tests using Gauche's stable and
  ;; wide-variety libraries. This is an useless example using 'every'
  ;; from SRFI-1.
  (assert-true (every (lambda (x)
                        (every (lambda (gosh-op uim-op)
                                 (equal? (gosh-op x)
                                         (uim-bool (list uim-op x))))
                               (list my-even? my-odd?)
                               '(even? odd?)))
                      '(-3 -2 -1 0 1 2 3)))
  #f)

(provide "test/test-example")
