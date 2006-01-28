;;  FileName : test-exp.scm
;;  About    : unit test for R5RS expressions
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

(use srfi-23)

(define *test-track-progress* #f)
(define tn test-name)

(define tee #t)
(define ef #f)


;; lambda
(assert-equal? "basic lambda test1" 8 ((lambda (x) (+ x x)) 4))
(define reverse-subtract
  (lambda (x y) (- y x)))
(assert-equal? "basic lambda test2" 3 (reverse-subtract 7 10))
(define add4
  (let ((x 4))
    (lambda (y) (+ x y))))
(assert-equal? "basic lambda test3" 10 (add4 6))
(assert-equal? "basic lambda test4" '(3 4 5 6) ((lambda x x) 3 4 5 6))
(assert-equal? "basic lambda test5" '(5) ((lambda (x y . z) z) 3 4 5))
(assert-equal? "basic lambda test6" '(5 6) ((lambda (x y . z) z) 3 4 5 6))
(assert-equal? "basic lambda test7" 1 ((lambda (x . y) x) 1))
(assert-equal? "basic lambda test8" '() ((lambda (x . y) y) 1))
(assert-equal? "basic lambda test9" 1 ((lambda (x y . z) x) 1 2))
(assert-equal? "basic lambda test10" 2 ((lambda (x y . z) y) 1 2))
(assert-equal? "basic lambda test11" '() ((lambda (x y . z) z) 1 2))

(if (and (provided? "sigscheme")
         (provided? "strict-argcheck"))
    (begin
      (tn "lambda invalid formals: boolean as an arg")
      (assert-error (tn) (lambda () (lambda (#t) #t)))
      (assert-error (tn) (lambda () (lambda (x #t) #t)))
      (assert-error (tn) (lambda () (lambda (#t x) #t)))
      (assert-error (tn) (lambda () (lambda (x . #t) #t)))
      (assert-error (tn) (lambda () (lambda (#t . x) #t)))
      (assert-error (tn) (lambda () (lambda (x y #t) #t)))
      (assert-error (tn) (lambda () (lambda (x y . #t) #t)))
      (assert-error (tn) (lambda () (lambda (x #t y) #t)))
      (assert-error (tn) (lambda () (lambda (x #t . y) #t)))
      (tn "lambda invalid formals: intger as an arg")
      (assert-error (tn) (lambda () (lambda (1) #t)))
      (assert-error (tn) (lambda () (lambda (x 1) #t)))
      (assert-error (tn) (lambda () (lambda (1 x) #t)))
      (assert-error (tn) (lambda () (lambda (x . 1) #t)))
      (assert-error (tn) (lambda () (lambda (1 . x) #t)))
      (assert-error (tn) (lambda () (lambda (x y 1) #t)))
      (assert-error (tn) (lambda () (lambda (x y . 1) #t)))
      (assert-error (tn) (lambda () (lambda (x 1 y) #t)))
      (assert-error (tn) (lambda () (lambda (x 1 . y) #t)))
      (tn "lambda invalid formals: null as an arg")
      (assert-error (tn) (lambda () (lambda (()) #t)))
      (assert-error (tn) (lambda () (lambda (x ()) #t)))
      (assert-error (tn) (lambda () (lambda (() x) #t)))
      (assert-true  (tn)            (lambda (x . ()) #t))
      (assert-error (tn) (lambda () (lambda (() . x) #t)))
      (assert-error (tn) (lambda () (lambda (x y ()) #t)))
      (assert-true  (tn)            (lambda (x y . ()) #t))
      (assert-error (tn) (lambda () (lambda (x () y) #t)))
      (assert-error (tn) (lambda () (lambda (x () . y) #t)))
      (tn "lambda invalid formals: pair as an arg")
      (assert-error (tn) (lambda () (lambda ((a)) #t)))
      (assert-error (tn) (lambda () (lambda (x (a)) #t)))
      (assert-error (tn) (lambda () (lambda ((a) x) #t)))
      (assert-true  (tn)            (lambda (x . (a)) #t))
      (assert-error (tn) (lambda () (lambda ((a) . x) #t)))
      (assert-error (tn) (lambda () (lambda (x y (a)) #t)))
      (assert-true  (tn)            (lambda (x y . (a)) #t))
      (assert-error (tn) (lambda () (lambda (x (a) y) #t)))
      (assert-error (tn) (lambda () (lambda (x (a) . y) #t)))
      (tn "lambda invalid formals: char as an arg")
      (assert-error (tn) (lambda () (lambda (#\a) #t)))
      (assert-error (tn) (lambda () (lambda (x #\a) #t)))
      (assert-error (tn) (lambda () (lambda (#\a x) #t)))
      (assert-error (tn) (lambda () (lambda (x . #\a) #t)))
      (assert-error (tn) (lambda () (lambda (#\a . x) #t)))
      (assert-error (tn) (lambda () (lambda (x y #\a) #t)))
      (assert-error (tn) (lambda () (lambda (x y . #\a) #t)))
      (assert-error (tn) (lambda () (lambda (x #\a y) #t)))
      (assert-error (tn) (lambda () (lambda (x #\a . y) #t)))
      (tn "lambda invalid formals: string as an arg")
      (assert-error (tn) (lambda () (lambda ("a") #t)))
      (assert-error (tn) (lambda () (lambda (x "a") #t)))
      (assert-error (tn) (lambda () (lambda ("a" x) #t)))
      (assert-error (tn) (lambda () (lambda (x . "a") #t)))
      (assert-error (tn) (lambda () (lambda ("a" . x) #t)))
      (assert-error (tn) (lambda () (lambda (x y "a") #t)))
      (assert-error (tn) (lambda () (lambda (x y . "a") #t)))
      (assert-error (tn) (lambda () (lambda (x "a" y) #t)))
      (assert-error (tn) (lambda () (lambda (x "a" . y) #t)))
      (tn "lambda invalid formals: vector as an arg")
      (assert-error (tn) (lambda () (lambda (#(a)) #t)))
      (assert-error (tn) (lambda () (lambda (x #(a)) #t)))
      (assert-error (tn) (lambda () (lambda (#(a) x) #t)))
      (assert-error (tn) (lambda () (lambda (x . #(a)) #t)))
      (assert-error (tn) (lambda () (lambda (#(a) . x) #t)))
      (assert-error (tn) (lambda () (lambda (x y #(a)) #t)))
      (assert-error (tn) (lambda () (lambda (x y . #(a)) #t)))
      (assert-error (tn) (lambda () (lambda (x #(a) y) #t)))
      (assert-error (tn) (lambda () (lambda (x #(a) . y) #t)))))

;;
;; if
;;
(assert-error "if invalid form #1"
	      (lambda ()
		(if)))
(assert-error "if invalid form #2"
	      (lambda ()
		(if #t)))
(assert-error "if invalid form #3"
	      (lambda ()
		(if #t 'true 'false 'excessive)))
(assert-error "if invalid form #4"
	      (lambda ()
		(if #f 'true 'false 'excessive)))

(assert-equal? "if test1" 'true  (if #t 'true 'false))
(assert-equal? "if test2" 'true  (if #t 'true))
(assert-equal? "if test3" 'false (if #f 'true 'false))
;; check that does not cause error
(assert-equal? "if test4" (if #f 'true) (if #f 'true))
;; check that <test> is evaluated
(assert-equal? "if test5" 'true  (if tee 'true 'false))
(assert-equal? "if test6" 'false (if ef 'true 'false))


;;
;; cond
;;
(assert-error  "cond invalid form #1"
               (lambda ()
                 (cond)))
(assert-error  "cond invalid form #2"
               (lambda ()
                 (cond
                  ())))
(assert-error  "cond invalid form #3"
               (lambda ()
                 (cond
                  ()
                  (else #t))))
;; 'else' followed by another caluse
(assert-error  "cond invalid form #4"
               (lambda ()
                 (cond
                  (else #t)
                  (#t))))
;; not specified in R5RS, but SigScheme should cause error
(if (provided? "sigscheme")
    (assert-error  "cond invalid form #5"
                   (lambda ()
                     (cond
                      (else)))))
(assert-error  "cond invalid form #6"
               (lambda ()
                 (cond
                  (#t =>))))
(assert-error  "cond invalid form #7"
               (lambda ()
                 (cond
                  (#t =>)
                  (else #t))))
(assert-error  "cond invalid form #8"
               (lambda ()
                 (cond
                  (else =>))))
;; not a procedure
(assert-error  "cond invalid form #9"
               (lambda ()
                 (cond
                  (#t => #t))))
(assert-error  "cond invalid form #10"
               (lambda ()
                 (cond
                  (#t => #f))))
;; procedure but argument number mismatch
(assert-error  "cond invalid form #11"
               (lambda ()
                 (cond
                  (#t => eq?))))
;; not a procedure but a syntax
(assert-error  "cond invalid form #12"
               (lambda ()
                 (cond
                  (#t => delay))))
;; '=>' is not applicable at 'else' clause
(assert-error  "cond invalid form #13"
               (lambda ()
                 (cond
                  (else => values))))

;; not specified in R5RS, but SigScheme surely returns #<undef>
(if (provided? "sigscheme")
    (assert-equal?  "cond unspecified behavior #1"
                    (undef)
                    (cond
                     (#f))))
(if (provided? "sigscheme")
    (assert-equal?  "cond unspecified behavior #2"
                    (undef)
                    (cond
                     ((even? 3) #f)
                     ((positive? -1) #f))))

;; R5RS: If the selected <clause> contains only the <test> and no
;; <expression>s, then the value of the <test> is returned as the result.
(assert-equal?  "cond"
                #t
                (cond
                 (#t)))
(assert-equal?  "cond"
                3
                (cond
                 (#f)
                 (3)))
(assert-equal?  "cond"
                3
                (cond
                 (#f)
                 (3)
                 (4)))

(assert-equal? "cond"
               'greater
               (cond
                ((> 3 2) 'greater)
                ((< 3 2) 'less)))
(assert-equal? "cond"
               'equal
               (cond
                ((> 3 3) 'greater)
                ((< 3 3) 'less)
                (else 'equal)))
(assert-equal? "cond"
               #t
               (cond
                ((> 3 2))
                ((< 3 4) 'less)
                (else 'equal)))
(assert-equal? "cond"
               2
               (cond
                ((assv 'b '((a 1) (b 2))) => cadr)
                (else #f)))
(assert-equal? "cond"
               #f
               (cond
                ((assv 'c '((a 1) (b 2))) => cadr)
                (else #f)))
(assert-equal? "cond"
               'greater1
               (cond
                ((> 3 2) 'greater0 'greater1)
                (else #f)))

;;
;; case
;;
(assert-error  "case invalid form #1"
               (lambda ()
                 (case)))
(assert-error  "case invalid form #2"
               (lambda ()
                 (case 'key)))
(assert-error  "case invalid form #3"
               (lambda ()
                 (case 'key
                   ())))
(assert-error  "case invalid form #4"
               (lambda ()
                 (case 'key
                   (1))))
(assert-error  "case invalid form #5"
               (lambda ()
                 (case 'key
                   ((1 . 2)))))

(if (provided? "sigscheme")
    (begin
      ;; improper clause does not cause error if not evaled
      (assert-equal? "case invalid form #6"
                     (undef)
                     (case 'key
                       ((1) . 2)))
      (assert-equal?  "case invalid form #7"
                      (undef)
                      (case 'key
                        ((1) #t . 2)))
      ;; causes error when evaled
      (assert-error  "case invalid form #6"
                     (lambda ()
                       (case 1
                         ((1) . 2))))
      (assert-error  "case invalid form #7"
                     (lambda ()
                       (case 1
                         ((1) #t . 2))))))

(assert-error  "case invalid form #8"
               (lambda ()
                 (case 'key
                  ()
                  (else #t))))
;; 'else' followed by another caluse
(assert-error  "case invalid form #9"
               (lambda ()
                 (case 'key
                  (else #t)
                  (#t))))
;; not specified in R5RS, but SigScheme should cause error
(if (provided? "sigscheme")
    (assert-error  "case invalid form #10"
                   (lambda ()
                     (case 'key
                      (else)))))
(assert-error  "case invalid form #11"
               (lambda ()
                 (case 'key
                  (#t =>))))
(assert-error  "case invalid form #12"
               (lambda ()
                 (case 'key
                  (#t =>)
                  (else #t))))
(assert-error  "case invalid form #13"
               (lambda ()
                 (case 'key
                  (else =>))))
(assert-error  "case invalid form #14"
               (lambda ()
                 (case 'key
                  (else => symbol?))))
(assert-error  "case invalid form #15"
               (lambda ()
                 (case 'key
                  (else => #t))))
;; not a procedure
(assert-error  "case invalid form #16"
               (lambda ()
                 (case 'key
                  (#t => #t))))
(assert-error  "case invalid form #17"
               (lambda ()
                 (case 'key
                  (#t => #f))))
;; procedure but argument number mismatch
(assert-error  "case invalid form #18"
               (lambda ()
                 (case 'key
                  (#t => eq?))))
;; not a procedure but a syntax
(assert-error  "case invalid form #19"
               (lambda ()
                 (case 'key
                  (#t => delay))))

;; not specified in R5RS, but SigScheme surely returns #<undef>
(if (provided? "sigscheme")
    (assert-equal?  "case unspecified behavior #1"
                    (undef)
                    (case 'key
                      ((#f)))))
(if (provided? "sigscheme")
    (assert-equal?  "case unspecified behavior #2"
                    (undef)
                    (case 'key
                      ((foo) #f)
                      ((bar) #f))))

;; R5RS: If the selected <clause> contains only the <test> and no
;; <expression>s, then the value of the <test> is returned as the result.
(assert-equal?  "case"
                'key
                (case 'key
                  ((key))))
(assert-equal?  "case"
                'key
                (case 'key
                  ((#f))
                  ((key))))
(assert-equal?  "case"
                'key
                (case 'key
                  ((#f))
                  ((key))
                  ((foo))))
(assert-equal? "case"
               'odd
               (case 3
                 ((1 3 5) 'odd)
                 ((2 4 6) 'even)))
(assert-equal? "case"
               'unknown
               (case 0
                 ((1 3 5) 'odd)
                 ((2 4 6) 'even)
                 (else 'unknown)))
(assert-equal? "case"
               'odd
               (case (+ 1 2)
                 ((1 3 5) 'odd)
                 ((2 4 6) 'even)
                 (else 'unknown)))
(assert-equal? "case"
               3
               (case 3
                 ((1 3 5))
                 ((2 4 6) 'even)
                 (else 'unknown)))
(assert-equal? "case"
               -3
               (case 3
                 ((1 3 5) => -)
                 ((2 4 6) 'even)
                 (else 'unknown)))
(assert-equal? "case"
               'unknown
               (case 0
                 ((1 3 5) => -)
                 ((2 4 6) 'even)
                 (else 'unknown)))
(assert-equal? "case"
               'second
               (case 3
                 ((1 3 5) 'first 'second)
                 ((2 4 6) 'even)
                 (else 'unknown)))

(assert-equal? "basic case check1" 'case1 (case 1
					 ((1) 'case1)
					 ((2) 'case2)))

(assert-equal? "basic case check2" 'case2 (case 2
					 ((1) 'case1)
					 ((2) 'case2)))

(assert-equal? "basic case check3" #t (case (* 2 3)
			      ((2 3 4 7)   #f)
			      ((1 4 6 8 9) #t)))

(assert-equal? "basic case else"  'caseelse (case 3
					   ((1) 'case1)
					   ((2) 'case2)
					   (else
					    'caseelse)))

;;
;; and
;;
(assert-equal? "and test 1" #t (and (= 2 2) (> 2 1)))
(assert-equal? "and test 2" #f (and (= 2 2) (< 2 1)))
(assert-equal? "and test 3" '(f g) (and 1 2 'c '(f g)))
(assert-equal? "and test 4" #t (and))
(assert-equal? "and test 5" #f (and #t #f))

;;
;; or
;;
(assert-equal? "or test1" #t (or (= 2 2) (> 2 1)))
(assert-equal? "or test2" #t (or (= 2 2) (< 2 1)))
(assert-equal? "or test3" #f (or #f #f #f))
(assert-equal? "or test4" '(b c) (or (memq 'b '(a b c))
				     (/ 3 0)))
;;
;; let
;;
(assert-error  "let invalid form #1"
               (lambda ()
                 (let)))
(assert-error  "let invalid form #2"
               (lambda ()
                 (let a)))
(assert-error  "let invalid form #3"
               (lambda ()
                 (let (a 1))))
(if (provided? "siod-bugs")
    (assert-equal? "let invalid form #4"
                   (undef)
                   (let ((a))))
    (assert-error  "let invalid form #4"
                   (lambda ()
                     (let ((a))))))
(assert-error  "let invalid form #5"
               (lambda ()
                 (let ((a 1 'excessive)))))
(assert-error  "let invalid form #6"
               (lambda ()
                 (let ((a 1) . (b 2)))))
(assert-error  "let invalid form #7"
               (lambda ()
                 (let ((a . 1)))))
(assert-error  "let invalid form #8"
               (lambda ()
                 (let ((a  1)) . a)))
(assert-error  "let invalid form #9"
               (lambda ()
                 (let (1) #t)))

(assert-equal? "basic let test1" 0 (let ((n 0))
				 n))
(assert-equal? "basic let test2" 1 (let ((n 0))
				  (set! n 1)
                                  n))
(assert-equal? "basic let test3" 1 (let ((n 0))
				  (set! n (+ n 1))
                                  n))
(assert-equal? "basic let test4" 3 (let ((n1 2)
                                         (n2 1))
                                     (+ n1 n2)))
(define count
  (let ((n 0))
    (lambda ()
      (set! n (+ n 1))
      n)))

(assert-equal? "lexical scope test1" 1 (count))
(assert-equal? "lexical scope test2" 2 (count))

(define a 3)
(define (lexical-test)
  (let ((a 1))
    (assert-equal? "lexical scope test3" 1 a)
    (let* ((a 2))
      (assert-equal? "lexical scope test4" 2 a))
    (assert-equal? "lexical scope test5" 1 a)))
(lexical-test)

(assert-equal? "named let test"
               '((6 1 3) (-5 -2))
               (let loop ((numbers '(3 -2 1 6 -5))
                          (nonneg '())
                          (neg '()))
                 (cond ((null? numbers) (list nonneg neg))
                       ((>= (car numbers) 0)
                        (loop (cdr numbers)
                              (cons (car numbers) nonneg)
                              neg))
                       ((< (car numbers) 0)
                        (loop (cdr numbers)
                              nonneg
                              (cons (car numbers) neg))))))

;;
;; let*
;;
(assert-error  "let* invalid form #1"
               (lambda ()
                 (let*)))
(assert-error  "let* invalid form #2"
               (lambda ()
                 (let* a)))
(assert-error  "let* invalid form #3"
               (lambda ()
                 (let* (a 1))))
(if (provided? "siod-bugs")
    (assert-equal? "let* invalid form #4"
                   (undef)
                   (let* ((a))))
    (assert-error  "let* invalid form #4"
                   (lambda ()
                     (let* ((a))))))
(assert-error  "let* invalid form #5"
               (lambda ()
                 (let* ((a 1 'excessive)))))
(assert-error  "let* invalid form #6"
               (lambda ()
                 (let* ((a 1) . (b 2)))))
(assert-error  "let* invalid form #7"
               (lambda ()
                 (let* ((a . 1)))))
(assert-error  "let* invalid form #8"
               (lambda ()
                 (let* ((a  1)) . a)))
(assert-error  "let invalid form #9"
               (lambda ()
                 (let* (1) #t)))

(assert-equal? "basic let* test1" 70 (let ((x 2) (y 3))
				    (let* ((x 7)
					   (z (+ x y)))
				      (* z x))))

;;
;; letrec
;;
(assert-error  "letrec invalid form #1"
               (lambda ()
                 (letrec)))
(assert-error  "letrec invalid form #2"
               (lambda ()
                 (letrec a)))
(assert-error  "letrec invalid form #3"
               (lambda ()
                 (letrec (a 1))))
(assert-error  "letrec invalid form #4"
               (lambda ()
                 (letrec ((a)))))
(assert-error  "letrec invalid form #5"
               (lambda ()
                 (letrec ((a 1 'excessive)))))
(assert-error  "letrec invalid form #6"
               (lambda ()
                 (letrec ((a 1) . (b 2)))))
(assert-error  "letrec invalid form #7"
               (lambda ()
                 (letrec ((a . 1)))))
(assert-error  "letrec invalid form #8"
               (lambda ()
                 (letrec ((a  1)) . a)))
(assert-error  "let invalid form #9"
               (lambda ()
                 (letrec (1) #t)))

(assert-equal? "basic letrec test1"
               #t
               (letrec ((even?
                         (lambda (n)
                           (if (zero? n)
                               #t
                               (odd? (- n 1)))))
                        (odd?
                         (lambda (n)
                           (if (zero? n)
                               #f
                               (even? (- n 1))))))
                 (even? 88)))

(assert-equal? "basic letrec test2"
               "aiueo"
               (letrec ((a (lambda () b))
                        (b "aiueo"))
                 (a)))

(define mularg-apply
  (letrec ((apply-2 apply)
	   (append-to-last
	    (lambda (lst)
	      (if (null? (cdr lst))
		  (car lst)
		  (cons (car lst) (append-to-last (cdr lst)))))))
    (lambda args
      (apply-2 (car args) (append-to-last (cdr args))))))
(assert-equal? "basic letrec test3" '((1) . 2) (mularg-apply cons '(1) '(2)))
(assert-equal? "basic letrec test4" '(1 2) (mularg-apply cons 1 '((2))))
;; SigScheme dependent behavior
(assert-error  "basic letrec test5" (lambda ()
                                      (letrec ((letrec-a 1)
                                               (letrec-b letrec-a))
                                        letrec-b)))
;;
;; begin
;;
(define x 0)
(assert-equal? "basic begin test1" 6 (begin
				    (set! x 5)
				    (+ x 1)))
(assert-equal? "basic begin test2" 0 (begin
				    0))
(assert-equal? "basic begin test3" 1 (begin
				    0
				    1))
(assert-equal? "basic begin test4" 1 (begin
				    (define n 0)
				    (set! n 1)
                                    n))

;;
;; do
;;
(if (or (symbol-bound? 'v)
        (symbol-bound? 'w)
        (symbol-bound? 'i)
        (symbol-bound? 'evaled))
    (error "global variables for 'do' tests are tainted"))

(tn "do invalid form")
(assert-error  (tn) (lambda ()
                      (do)))
(assert-error  (tn) (lambda ()
                      (do v)))
(assert-error  (tn) (lambda ()
                      (do (v 1))))
(assert-error  (tn) (lambda ()
                      (do ((v 1))
                          )))
(assert-error  (tn) (lambda ()
                      (do ((v))
                          'eval)))
(assert-error  (tn) (lambda ()
                      (do ((v 1))
                          'unknow-value)))
(assert-error  (tn) (lambda ()
                      (do ((v 1 2 'excessive))
                          'eval)))
(tn "do invalid form: no test")
(assert-error  (tn) (lambda ()
                      (do ((v 1))
                          ()
                        'eval)))
(tn "do invalid form: non-list test form")
(assert-error  (tn) (lambda ()
                      (do ((v 1))
                          'test
                        'eval)))
(assert-error  (tn) (lambda ()
                      (do ((v 1))
                          1
                        'eval)))
(tn "do invalid form: non-list bindings form")
(assert-error  (tn) (lambda ()
                      (do 'bindings
                          (#t #t)
                        'eval)))
(assert-error  (tn) (lambda ()
                      (do 1
                          (#t #t)
                        'eval)))
(tn "do invalid form: non-symbol variable name")
(assert-error  (tn) (lambda ()
                      (do ((1 1))
                          (#t #t)
                        #t)))
(assert-error  (tn) (lambda ()
                      (do ((#t 1))
                          (#t #t)
                        #t)))
(assert-error  (tn) (lambda ()
                      (do (("a" 1))
                          (#t #t)
                        #t)))
(tn "do invalid form: duplicate variable name")
(assert-error  (tn) (lambda ()
                      (do ((v 1)
                           (v 2))
                          (#t #t)
                        #t)))
(assert-error  (tn) (lambda ()
                      (do ((v 1)
                           (w 0)
                           (v 2))
                          (#t #t)
                        #t)))
(tn "do invalid form: improper binding")
(assert-error  (tn) (lambda ()
                      (do ((v . 1))
                          (#t #t)
                        #t)))
(assert-error  (tn) (lambda ()
                      (do ((v  1 . v))
                          (#t #t)
                        #t)))
(assert-error  (tn) (lambda ()
                      (do ((v  1) . 1)
                          (#t #t)
                        #t)))
(tn "do invalid form: improper exps")
(assert-error  (tn) (lambda ()
                      (do ((v  1))
                          (#t . #t)
                        #t)))
(assert-error  (tn) (lambda ()
                      (do ((v  1))
                          (#t #t . #t)
                        #t)))
(tn "do invalid form: improper commands")
(assert-error  (tn) (lambda ()
                      (do ((v  1))
                          (#t #t)
                        #t . #t)))
(assert-error  (tn) (lambda ()
                      (do ((v  1 (+ v 1)))
                          ((= v 2) #t)
                        #t . #t)))

(tn "do valid form: no bindings")
(assert-true   (tn) (lambda ()
                      (do ()
                          (#t #t)
                        'foo)))
(assert-true   (tn) (lambda ()
                      (do ()
                          (#t)
                        'foo)))
(assert-true   (tn) (lambda ()
                      (do ()
                          (#t #t)
                        )))
(assert-true   (tn) (lambda ()
                      (do ()
                          (#t)
                        )))
(tn "do valid form: no commands")
(assert-true   (tn) (lambda ()
                      (do ((v 1))
                          (#t #t)
                        )))
(assert-true   (tn) (lambda ()
                      (do ((v 1))
                          (#t)
                        )))
(tn "do valid form: no exps")
(if (provided? "sigscheme")
    (assert-equal? (tn)
                   (undef)
                   (do ((v 1))
                       (#t)
                     'foo)))

(tn "do inter-iteration variable isolation")
(assert-equal? (tn)
               '(2 1 0)
               (do ((v '() (cons i v))
                    (i 0 (+ i 1)))
                   ((= i 3) v)
                 ))
(assert-equal? (tn)
               '(2 1 0)
               (do ((i 0 (+ i 1))
                    (v '() (cons i v)))
                   ((= i 3) v)
                 ))

(tn "do initialize-time variable isolation")  
(assert-error (tn) (lambda () (do ((v 1)
                                   (w v))
                                  (#t #t)
                                )))
(assert-error (tn) (lambda () (do ((w v)
                                   (v 1))
                                  (#t #t)
                                )))

(tn "do exp is evaluated exactly once")
(assert-equal? (tn)
               '(+ v w)
               (do ((v 1)
                    (w 2))
                   (#t '(+ v w))
                 ))

(tn "do iteration count")
(assert-equal? (tn)
               0
               (do ((i 0 (+ i 1))
                    (evaled 0))
                   (#t evaled)
                 (set! evaled (+ evaled 1))))
(assert-equal? (tn)
               0
               (do ((i 0 (+ i 1))
                    (evaled 0))
                   ((= i 0) evaled)
                 (set! evaled (+ evaled 1))))
(assert-equal? (tn)
               1
               (do ((i 0 (+ i 1))
                    (evaled 0))
                   ((= i 1) evaled)
                 (set! evaled (+ evaled 1))))
(assert-equal? (tn)
               2
               (do ((i 0 (+ i 1))
                    (evaled 0))
                   ((= i 2) evaled)
                 (set! evaled (+ evaled 1))))

(tn "do variable update")
(assert-equal? (tn)
               10
               (do ((v 1)
                    (w 2))
                   (#t (set! v (+ v 1))
                       (set! w (+ w v))
                       (set! v (+ v w))
                       (+ w v))
                 ))
(assert-equal? (tn)
               16
               (do ((i 0 (+ i 1))
                    (v 1)
                    (w 2))
                   ((= i 1)
                    (set! v (+ v 1))
                    (set! w (+ w v))
                    (set! v (+ v w))
                    (+ w v))
                 (set! v 3)))
(assert-equal? (tn)
               20
               (do ((i 0 (+ i 1))
                    (v 1)
                    (w 2))
                   ((= i 1)
                    (set! v (+ v 1))
                    (set! w (+ w v))
                    (set! v (+ v w))
                    (+ w v))
                 (set! v 3)
                 (set! w 4)))

(assert-equal? "do test1" '#(0 1 2 3 4) (do ((vec (make-vector 5))
					     (i 0 (+ i 1)))
					    ((= i 5) vec)
					  (vector-set! vec i i)))
(assert-equal? "do test2" 25 (let ((x '(1 3 5 7 9)))
			       (do ((x x (cdr x))
				    (sum 0 (+ sum (car x))))
				   ((null? x) sum))))

(define (expt-do x n)
  (do ((i 0 (+ i 1))
       (y 1))
      ((= i n) y)
    (set! y (* x y))))
(assert-equal? "do test3" 1024 (expt-do 2 10))

(define (nreverse rev-it)
  (do ((reved '() rev-it)
       (rev-cdr (cdr rev-it) (cdr rev-cdr))
       (rev-it rev-it rev-cdr))
      ((begin
	 (set-cdr! rev-it reved)
	 (null? rev-cdr))
       rev-it)))
(assert-equal? "do test4" '(c b a) (nreverse (list 'a 'b 'c)))
(assert-equal? "do test5"
               '((5 6) (3 4) (1 2))
               (nreverse (list '(1 2) '(3 4) '(5 6))))

;; scm_s_do() has been changed as specified in R5RS. -- YamaKen 2006-01-11
;; R5RS: If no <expression>s are present, then the value of the `do' expression
;; is unspecified.
;;(assert-equal? "do test6" 1  (do ((a 1)) (a) 'some))
;;(assert-equal? "do test7" #t (do ((a 1)) (#t) 'some))
(if (provided? "sigscheme")
    (begin
      (assert-equal? "do test6" (undef) (do ((a 1)) (a) 'some))
      (assert-equal? "do test7" (undef) (do ((a 1)) (#t) 'some))))
;; (do ((a 1)) 'eval) => (do ((a 1)) (quote eval))
(assert-equal? "do test8" eval (do ((a 1)) 'eval))

;;
;; procedure?
;;
(assert-true  "procedure? #1" (procedure? even?))
(assert-true  "procedure? #2" (procedure? (lambda (x) x)))
(assert-true  "procedure? #3" (procedure? (call-with-current-continuation
                                           (lambda (c)
                                             c))))
(assert-false "procedure? #4" (procedure? if))
(assert-false "procedure? #5" (procedure? quote))

;;
;; call-with-values
;;
;; from R5RS
(assert-equal? "call-with-values #1"
               5
	       (call-with-values (lambda () (values 4 5))
		 (lambda (a b) b)))
(assert-equal? "call-with-values #2"
               4
	       (call-with-values (lambda () (values 4))
		 (lambda (x) x)))
(assert-equal? "call-with-values #3"
               'ok
	       (call-with-values (lambda () (values))
		 (lambda () 'ok)))
(assert-equal? "call-with-values #4" -1 (call-with-values * -))

(assert-equal? "call-with-values #5"
               5
	       (apply call-with-values (list (lambda () (values 4 5))
                                             (lambda (a b) b))))
(assert-equal? "call-with-values #6"
               4
	       (apply call-with-values (list (lambda () (values 4))
                                             (lambda (x) x))))
(assert-equal? "call-with-values #7"
               'ok
	       (apply call-with-values (list (lambda () (values))
                                             (lambda () 'ok))))
(assert-equal? "call-with-values #8" -1 (apply call-with-values (list * -)))

;; test whether the variable properly bound
(assert-equal? "call-with-values #9"
               1
               ((lambda (n)
                  (call-with-values
                      (lambda () (values 2 3 n))
                    (lambda (dummy1 dummy2 n2)
                      n2)))
                1))

;;
;; values
;;
(assert-true   "values #1" (number? (values 5)))
(assert-false  "values #2" (number? (values 'five)))
(assert-equal? "values #3"
               '((eval-counter 1) (eval-counter 1))
               (call-with-values
                   (lambda () (values (eval-counter 0) (eval-counter 0)))
                 (lambda x x)))

;; not asserted, just make sure we don't blow up
(begin (values 1 2 3) 'ignore)

(assert-error "multiple values at invalid place"
              (lambda ()
                (write (values))))

;;
;; dynamic-wind
;;
(define dynwind-res '())
(define append-sym!
  (lambda (sym)
    (set! dynwind-res (append dynwind-res (list sym)))))

(set! dynwind-res '())
(assert-equal? "dynamic-wind #1"
               '(before thunk after)
               (begin
                 (dynamic-wind
                     (lambda ()
                       (append-sym! 'before))
                     (lambda ()
                       (append-sym! 'thunk))
                     (lambda ()
                       (append-sym! 'after)))
                 dynwind-res))
                   
(set! dynwind-res '())
(assert-equal? "dynamic-wind #2"
               '(before1 thunk1 before2 thunk2 after2 after1)
               (begin
                 (dynamic-wind
                     (lambda ()
                       (append-sym! 'before1))
                     (lambda ()
                       (append-sym! 'thunk1)
                       (dynamic-wind
                           (lambda ()
                             (append-sym! 'before2))
                           (lambda ()
                             (append-sym! 'thunk2))
                           (lambda ()
                             (append-sym! 'after2))))
                     (lambda ()
                       (append-sym! 'after1)))
                 dynwind-res))

;; current implementation does not support this yet
(set! dynwind-res '())
(assert-equal? "dynamic-wind #3"
               '(before thunk after)
               (begin
                 (call/cc
                  (lambda (k)
                    (dynamic-wind
                        (lambda ()
                          (append-sym! 'before))
                        (lambda ()
                          (append-sym! 'thunk)
                          (k #f))
                        (lambda ()
                          (append-sym! 'after)))))
                 dynwind-res))

;; current implementation does not support this yet
(set! dynwind-res '())
(assert-equal? "dynamic-wind #4"
               '(before1 thunk1 before2 thunk2 after2 after1)
               (begin
                 (call/cc
                  (lambda (k)
                    (dynamic-wind
                        (lambda ()
                          (append-sym! 'before1))
                        (lambda ()
                          (append-sym! 'thunk1)
                          (dynamic-wind
                              (lambda ()
                                (append-sym! 'before2))
                              (lambda ()
                                (append-sym! 'thunk2)
                                (k #f))
                              (lambda ()
                                (append-sym! 'after2))))
                        (lambda ()
                          (append-sym! 'after1)))))
                 dynwind-res))

(total-report)
