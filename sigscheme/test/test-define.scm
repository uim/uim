;;  FileName : test-define.scm
;;  About    : unit test for R5RS 'define'
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

(load "./test/unittest.scm")

; invalid form
(assert-error "define invalid form #1"
	      (lambda ()
		(define)))
(assert-error "define invalid form #2"
	      (lambda ()
		(define a)))
(assert-error "define invalid form #3"
	      (lambda ()
		(define 1 1)))
(assert-error "define invalid form #4"
	      (lambda ()
		(define a 1 'excessive)))
(assert-error "define invalid form #5"
	      (lambda ()
		(define a . 2)))
(assert-error "define invalid form #6"
	      (lambda ()
		(define (f x) . x)))

; basic define
(define val1 3)
(assert-equal? "basic define check" 3 val1)

; redefine
(define val1 5)
(assert-equal? "redefine check" 5 val1)

; define lambda
(define (what? x)
  "DEADBEEF" x)
(assert-equal? "func define" 10 (what? 10))

(define what2?
  (lambda (x)
    "DEADBEEF" x))
(assert-equal? "func define" 10 (what2? 10))

(define (nullarg)
  "nullarg")
(assert-equal? "nullarg test" "nullarg" (nullarg))

(define (add x y)
  (+ x y))
(assert-equal? "func define" 10 (add 2 8))

; tests for dot list arguments
(define (dotarg1 . a)
  a)
(assert-equal? "dot arg test 1" '(1 2) (dotarg1 1 2))

(define (dotarg2 a . b)
  a)
(assert-equal? "dot arg test 2" 1 (dotarg2 1 2))

(define (dotarg3 a . b)
  b)
(assert-equal? "dot arg test 3" '(2) (dotarg3 1 2))
(assert-equal? "dot arg test 4" '(2 3) (dotarg3 1 2 3))


(define (dotarg4 a b . c)
  b)
(assert-equal? "dot arg test 5" 2 (dotarg4 1 2 3))

(define (dotarg5 a b . c)
  c)
(assert-equal? "dot arg test 6" '(3 4) (dotarg5 1 2 3 4))

; test for internal define
(define (idefine-o a)
  (define (idefine-i c)
    (+ c 3))
  (idefine-i a))

(assert-equal? "internal define1" 5 (idefine-o 2))

(define (idefine0 a)
  (define (idefine1 . args)
    (apply +  args))
  (define (idefine2 c)
    (+ c 2))
  (+ (idefine1 1 2 3 4 5) (idefine2 a)))

(assert-equal? "internal define2" 17 (idefine0 0))

; set!
(define (set-dot a . b)
  (set! b '(1 2))
  b)

(assert-equal? "set dot test" '(1 2) (set-dot '()))

(total-report)
