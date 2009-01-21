#!/usr/bin/env gosh

;;; Copyright (c) 2004-2009 uim Project http://code.google.com/p/uim/
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

;; These tests are passed at revision 5329 (new repository)

(use test.unit)

(require "test/uim-test-utils")

;; group related tests into a testcase
(define-uim-test-case "testcase integer operations"
  ;; Define test. Inferior uim-sh process is newly prepared for each
  ;; test.
  ("test +"
   ;; assert-equal accepts <expect> <test>. The procedure 'uim' sends
   ;; an expression to the inferior uim-sh process, and receives the
   ;; evaluated result. Then gosh compares the two values.
   (assert-equal 5
		 (uim '(+ 2 3)))
   (assert-equal -1
		 (uim '(+ 2 -3))))

  ;; another test in the testcase
  ("test -"
   (assert-equal -1
		 (uim '(- 2 3)))
   (assert-equal 5
		 (uim '(- 2 -3)))

   ;; When <extected> and the actual evaluated values are different,
   ;; it is countted as failure and reported.
   (assert-equal 1
		 (uim '(- 0 -1))))

  ("test /"
   ;; 'assert-error' asserts that the procedure passed causes an
   ;; error.
   (assert-error (lambda ()
		   (uim '(/ 5 0))))))


;; The procedures are defined in gosh, not uim-sh.
(define my-even?
  (lambda (x)
    (= (remainder x 2) 0)))
(define my-odd?
  (lambda (x)
    (= (remainder x 2) 1)))

;; one more testcase
(define-uim-test-case "testcase integer predicates"
  ;; Optional setup procedure is evaluated for each test to prepare
  ;; common testing environment for each test.
  (setup
   (lambda ()
     ;; The expressions are defined in gosh, not uim-sh.
     (use srfi-1)

     ;; The procedure 'uim' can send an expression to inferior uim-sh
     ;; process.
     (uim '(require "util.scm"))

     ;; Some Scheme objects that the external form is not defined in
     ;; R5RS such as procedure cannot parsed by gosh. So such forms
     ;; must wrapped into a block ending with a safe object.
     (uim '(begin
	     (define even?
	       (lambda (x)
		 (= (remainder x 2) 0)))
	     (define odd?
	       (lambda (x)
		 (= (remainder x 2) 1)))
	     #f))))

  ;; Inferior uim-sh process is newly prepared and the setup procedure
  ;; is evaluated.
  ("test even?"
   ;; Boolean tests must be wrapped into 'uim-bool' instead of
   ;; 'uim'. This is required to adapt SIOD's false value '()' to
   ;; #f. It will be unified into 'uim' once the R5RS-compliant
   ;; SigScheme has become default implementation for uim.
   (assert-true  (uim-bool '(even? 2)))
   (assert-false (uim-bool '(even? 3))))

  ;; Another uim-sh process is newly prepared and the setup procedure
  ;; is evaluated.
  ("test odd?"
   (assert-false (uim-bool '(odd? 2)))
   (assert-true  (uim-bool '(odd? 3))))

  ("test odd? #2"
   ;; another way to test the even?. This focuses on difference of
   ;; implementation's own behavior rather than correctness of
   ;; user-procedure definition.
   (assert-equal (my-odd? 2)
		 (uim-bool '(odd? 2)))
   (assert-equal (my-odd? 3)
		 (uim-bool '(odd? 3))))

  ("test difference of Gauche and uim"
   ;; developer can write complex tests using Gauche's stable and
   ;; wide-variety libraries. This is an useless example using 'every'
   ;; from SRFI-1.
   (assert-true (every (lambda (x)
			 (every (lambda (gosh-op uim-op)
				  (equal? (gosh-op x)
					  (uim-bool (list uim-op x))))
				(list my-even? my-odd?)
				'(even? odd?)))
		       '(-3 -2 -1 0 1 2 3)))))
