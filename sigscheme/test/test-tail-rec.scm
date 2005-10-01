;;  FileName : test-tail-rec.scm
;;  About    : unit test for the proper tail recursion
;;
;;  Copyright (C) 2005      by Kazuki Ohta (mover@hct.zaq.ne.jp)
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

;; FILE HISTORY:
;; 2005-08-07 contributed by YamaKen (yamaken AT bp.iij4u.or.jp)

;; This file is provided to test the proper tail recursion functionality. See
;; "3.5 Proper tail recursion" of R5RS for the accurate specification.
;;
;; This test must be run as follows to take effect.
;;
;; $ (ulimit -s 128 && ulimit -d 2048 && ./sscm test/test-tail-rec.scm || echo 'exploded')
;;
;; And compare the result with another R5RS implementation (gosh).
;;
;; $ (ulimit -s 128 && ulimit -d 2048 && gosh -I. test/test-tail-rec.scm || echo 'exploded')

(load "test/unittest.scm")

(define test-and?          #t)  ;; #t is required to conform to R5RS
(define test-or?           #t)  ;; #t is required to conform to R5RS
(define test-improper-and? #f)  ;; R5RS compliant implementation explodes if #t
(define test-improper-or?  #f)  ;; R5RS compliant implementation explodes if #t

(define KB 1024)
(define heap-limit (* 2048 KB))  ;; specify this by ulimit -d
(define cell-size 8)  ;; 16 is actual size of current SigScheme implementation
(define explosive-count (/ heap-limit cell-size))

(define assert-orig assert)
(define assert
  (lambda (msg exp)
    ;; current assert implementation cannot print msg before exp has
    ;; been evaluated
    (display msg)
    (assert-orig msg exp)
    (display " ...OK\n")))


(define rec-by-if-consequent
  (lambda (cnt)
    (if (not (zero? cnt))
	(rec-by-if-consequent (- cnt 1))
	'succeeded)))

(define rec-by-if-consequent-with-begin
  (lambda (cnt)
    (if (not (zero? cnt))
	(begin
	  (+ 1 2)  ;; dummy
	  (rec-by-if-consequent-with-begin (- cnt 1)))
	'succeeded)))

(define rec-by-if-consequent-with-let
  (lambda (cnt)
    (if (not (zero? cnt))
	(let ((dummy (+ 1 2))
	      (dummy2 (+ 3 4)))
	  (rec-by-if-consequent-with-let (- cnt 1)))
	'succeeded)))

(define rec-by-if-consequent-with-let*
  (lambda (cnt)
    (if (not (zero? cnt))
	(let* ((dummy (+ 1 2))
	       (dummy2 (+ dummy 3)))
	  (rec-by-if-consequent-with-let* (- cnt 1)))
	'succeeded)))

(define rec-by-if-consequent-with-letrec
  (lambda (cnt)
    (if (not (zero? cnt))
	(letrec ((dummy (lambda () dummy2))
		 (dummy2 (lambda () dummy)))
	  (rec-by-if-consequent-with-letrec (- cnt 1)))
	'succeeded)))

(define rec-by-if-alternate
  (lambda (cnt)
    (if (zero? cnt)
	'succeeded
	(rec-by-if-alternate (- cnt 1)))))

(define rec-by-if-alternate-with-begin
  (lambda (cnt)
    (if (not (zero? cnt))
	(begin
	  (+ 1 2)  ;; dummy
	  (rec-by-if-alternate-with-begin (- cnt 1)))
	'succeeded)))

(define rec-by-if-alternate-with-let
  (lambda (cnt)
    (if (not (zero? cnt))
	(let ((dummy (+ 1 2))
	      (dummy2 (+ 3 4)))
	  (rec-by-if-alternate-with-let (- cnt 1)))
	'succeeded)))

(define rec-by-if-alternate-with-let*
  (lambda (cnt)
    (if (not (zero? cnt))
	(let* ((dummy (+ 1 2))
	       (dummy2 (+ dummy 3)))
	  (rec-by-if-alternate-with-let* (- cnt 1)))
	'succeeded)))

(define rec-by-if-alternate-with-letrec
  (lambda (cnt)
    (if (not (zero? cnt))
	(letrec ((dummy (lambda () dummy2))
		 (dummy2 (lambda () dummy)))
	  (rec-by-if-alternate-with-letrec (- cnt 1)))
	'succeeded)))

(define rec-by-cond-1st
  (lambda (cnt)
    (cond
     ((positive? cnt)
      (rec-by-cond-1st (- cnt 1)))
     ((zero? cnt)
      'succeeded)
     ((negative? cnt)
      'dummy)
     (else
      'dummy))))

(define rec-by-cond-1st-with-begin
  (lambda (cnt)
    (cond
     ((positive? cnt)
      (begin
	(+ 1 2)	;; dummy
	(rec-by-cond-1st-with-begin (- cnt 1))))
     ((zero? cnt)
      'succeeded)
     ((negative? cnt)
      'dummy)
     (else
      'dummy))))

(define rec-by-cond-1st-with-let
  (lambda (cnt)
    (cond
     ((positive? cnt)
      (let ((dummy (+ 1 2))
	    (dummy2 (+ 3 4)))
	(rec-by-cond-1st-with-let (- cnt 1))))
     ((zero? cnt)
      'succeeded)
     ((negative? cnt)
      'dummy)
     (else
      'dummy))))

(define rec-by-cond-1st-with-let*
  (lambda (cnt)
    (cond
     ((positive? cnt)
      (let* ((dummy (+ 1 2))
	     (dummy2 (+ dummy 3)))
	(rec-by-cond-1st-with-let* (- cnt 1))))
     ((zero? cnt)
      'succeeded)
     ((negative? cnt)
      'dummy)
     (else
      'dummy))))

(define rec-by-cond-1st-with-letrec
  (lambda (cnt)
    (cond
     ((positive? cnt)
      (letrec ((dummy (lambda () dummy2))
	       (dummy2 (lambda () dummy)))
	(rec-by-cond-1st-with-letrec (- cnt 1))))
     ((zero? cnt)
      'succeeded)
     ((negative? cnt)
      'dummy)
     (else
      'dummy))))

(define rec-by-cond-2nd
  (lambda (cnt)
    (cond
     ((zero? cnt)
      'succeeded)
     ((positive? cnt)
      (rec-by-cond-2nd (- cnt 1)))
     ((negative? cnt)
      'dummy)
     (else
      'dummy))))

(define rec-by-cond-3rd
  (lambda (cnt)
    (cond
     ((zero? cnt)
      'succeeded)
     ((negative? cnt)
      'dummy)
     ((positive? cnt)
      (rec-by-cond-3rd (- cnt 1)))
     (else
      'dummy))))

(define rec-by-cond-last
  (lambda (cnt)
    (cond
     ((zero? cnt)
      'succeeded)
     ((negative? cnt)
      'dummy)
     ((positive? cnt)
      (rec-by-cond-last (- cnt 1))))))

(define rec-by-cond-else
  (lambda (cnt)
    (cond
     ((zero? cnt)
      'succeeded)
     ((negative? cnt)
      'dummy)
     ((not (positive? cnt))
      'dummy)
     (else
      (rec-by-cond-else (- cnt 1))))))

(define rec-by-cond-else-with-begin
  (lambda (cnt)
    (cond
     ((zero? cnt)
      'succeeded)
     ((negative? cnt)
      'dummy)
     ((not (positive? cnt))
      'dummy)
     (else
      (begin
	(+ 1 2)	;; dummy
	(rec-by-cond-else (- cnt 1)))))))

(define rec-by-cond-else-with-let
  (lambda (cnt)
    (cond
     ((zero? cnt)
      'succeeded)
     ((negative? cnt)
      'dummy)
     ((not (positive? cnt))
      'dummy)
     (else
      (let ((dummy (+ 1 2))
	    (dummy2 (+ 3 4)))
	(rec-by-cond-else (- cnt 1)))))))

(define rec-by-cond-else-with-let*
  (lambda (cnt)
    (cond
     ((zero? cnt)
      'succeeded)
     ((negative? cnt)
      'dummy)
     ((not (positive? cnt))
      'dummy)
     (else
      (let* ((dummy (+ 1 2))
	     (dummy2 (+ dummy 3)))
	(rec-by-cond-else (- cnt 1)))))))

(define rec-by-cond-else-with-letrec
  (lambda (cnt)
    (cond
     ((zero? cnt)
      'succeeded)
     ((negative? cnt)
      'dummy)
     ((not (positive? cnt))
      'dummy)
     (else
      (letrec ((dummy (lambda () dummy2))
	       (dummy2 (lambda () dummy)))
	(rec-by-cond-else (- cnt 1)))))))


(define rec-by-case-1st
  (lambda (cnt)
    (case (if (positive? cnt)
	      1
	      cnt)
      ((1)
       (rec-by-case-1st (- cnt 1)))
      ((0)
       'succeeded)
      ((-1)
       'dummy)
      (else
       'dummy))))

(define rec-by-case-1st-with-begin
  (lambda (cnt)
    (case (if (positive? cnt)
	      1
	      cnt)
      ((1)
       (begin
	 (+ 1 2) ;; dummy
	 (rec-by-case-1st-with-begin (- cnt 1))))
      ((0)
       'succeeded)
      ((-1)
       'dummy)
      (else
       'dummy))))

(define rec-by-case-1st-with-let
  (lambda (cnt)
    (case (if (positive? cnt)
	      1
	      cnt)
      ((1)
       (let ((dummy (+ 1 2))
	     (dummy2 (+ 3 4)))
	 (rec-by-case-1st-with-let (- cnt 1))))
      ((0)
       'succeeded)
      ((-1)
       'dummy)
      (else
       'dummy))))

(define rec-by-case-1st-with-let*
  (lambda (cnt)
    (case (if (positive? cnt)
	      1
	      cnt)
      ((1)
       (let* ((dummy (+ 1 2))
	      (dummy2 (+ dummy 3)))
	 (rec-by-case-1st-with-let* (- cnt 1))))
      ((0)
       'succeeded)
      ((-1)
       'dummy)
      (else
       'dummy))))

(define rec-by-case-1st-with-letrec
  (lambda (cnt)
    (case (if (positive? cnt)
	      1
	      cnt)
      ((1)
       (letrec ((dummy (lambda () dummy2))
		(dummy2 (lambda () dummy)))
	 (rec-by-case-1st-with-letrec (- cnt 1))))
      ((0)
       'succeeded)
      ((-1)
       'dummy)
      (else
       'dummy))))

(define rec-by-case-2nd
  (lambda (cnt)
    (case (if (positive? cnt)
	      1
	      cnt)
      ((0)
       'succeeded)
      ((1)
       (rec-by-case-2nd (- cnt 1)))
      ((-1)
       'dummy)
      (else
       'dummy))))

(define rec-by-case-3rd
  (lambda (cnt)
    (case (if (positive? cnt)
	      1
	      cnt)
      ((0)
       'succeeded)
      ((-1)
       'dummy)
      ((1)
       (rec-by-case-3rd (- cnt 1)))
      (else
       'dummy))))

(define rec-by-case-last
  (lambda (cnt)
    (case (if (positive? cnt)
	      1
	      cnt)
      ((0)
       'succeeded)
      ((-1)
       'dummy)
      ((1)
       (rec-by-case-last (- cnt 1))))))

(define rec-by-case-else
  (lambda (cnt)
    (case (if (positive? cnt)
	      1
	      cnt)
      ((0)
       'succeeded)
      ((-1)
       'dummy)
      ((2)
       'dummy)
      (else
       (rec-by-case-else (- cnt 1))))))

(define rec-by-case-else-with-begin
  (lambda (cnt)
    (case (if (positive? cnt)
	      1
	      cnt)
      ((0)
       'succeeded)
      ((-1)
       'dummy)
      ((2)
       'dummy)
      (else
       (begin
	 (+ 1 2) ;; dummy
	 (rec-by-case-else (- cnt 1)))))))

(define rec-by-case-else-with-let
  (lambda (cnt)
    (case (if (positive? cnt)
	      1
	      cnt)
      ((0)
       'succeeded)
      ((-1)
       'dummy)
      ((2)
       'dummy)
      (else
       (let ((dummy (+ 1 2))
	     (dummy2 (+ 3 4)))
	 (rec-by-case-else (- cnt 1)))))))

(define rec-by-case-else-with-let*
  (lambda (cnt)
    (case (if (positive? cnt)
	      1
	      cnt)
      ((0)
       'succeeded)
      ((-1)
       'dummy)
      ((2)
       'dummy)
      (else
       (let* ((dummy (+ 1 2))
	      (dummy2 (+ dummy 3)))
	 (rec-by-case-else (- cnt 1)))))))

(define rec-by-case-else-with-letrec
  (lambda (cnt)
    (case (if (positive? cnt)
	      1
	      cnt)
      ((0)
       'succeeded)
      ((-1)
       'dummy)
      ((2)
       'dummy)
      (else
       (letrec ((dummy (lambda () dummy2))
		(dummy2 (lambda () dummy)))
	 (rec-by-case-else (- cnt 1)))))))

(define rec-by-and-tail
  (lambda (cnt)
    (and (not (zero? cnt))
	 (rec-by-and-tail (- cnt 1)))))

(define rec-by-and-tail-with-begin
  (lambda (cnt)
    (and (not (zero? cnt))
	 (begin
	   (+ 1 2)  ;; dummy
	   (rec-by-and-tail-with-begin (- cnt 1))))))

(define rec-by-and-tail-with-let
  (lambda (cnt)
    (and (not (zero? cnt))
	 (let ((dummy (+ 1 2))
	       (dummy2 (+ 3 4)))
	   (rec-by-and-tail-with-let (- cnt 1))))))

(define rec-by-and-tail-with-let*
  (lambda (cnt)
    (and (not (zero? cnt))
	 (let* ((dummy (+ 1 2))
		(dummy2 (+ dummy 3)))
	   (rec-by-and-tail-with-let* (- cnt 1))))))

(define rec-by-and-tail-with-letrec
  (lambda (cnt)
    (and (not (zero? cnt))
	 (letrec ((dummy (lambda () dummy2))
		  (dummy2 (lambda () dummy)))
	   (rec-by-and-tail-with-letrec (- cnt 1))))))

(define improper-rec-by-and-tail
  (lambda (cnt)
    (and (not (zero? cnt))
	 (improper-rec-by-and-tail (- cnt 1)))
    'succeeded))

(define rec-by-or-tail
  (lambda (cnt)
    (or (zero? cnt)
	(rec-by-or-tail (- cnt 1)))))

(define rec-by-or-tail-with-begin
  (lambda (cnt)
    (or (zero? cnt)
	(begin
	  (+ 1 2)  ;; dummy
	  (rec-by-or-tail-with-begin (- cnt 1))))))

(define rec-by-or-tail-with-let
  (lambda (cnt)
    (or (zero? cnt)
	(let ((dummy (+ 1 2))
	      (dummy2 (+ 3 4)))
	  (rec-by-or-tail-with-let (- cnt 1))))))

(define rec-by-or-tail-with-let*
  (lambda (cnt)
    (or (zero? cnt)
	(let* ((dummy (+ 1 2))
	       (dummy2 (+ dummy 3)))
	  (rec-by-or-tail-with-let* (- cnt 1))))))

(define rec-by-or-tail-with-letrec
  (lambda (cnt)
    (or (zero? cnt)
	(letrec ((dummy (lambda () dummy2))
		 (dummy2 (lambda () dummy)))
	  (rec-by-or-tail-with-letrec (- cnt 1))))))

(define improper-rec-by-or-tail
  (lambda (cnt)
    (or (zero? cnt)
	(improper-rec-by-or-tail (- cnt 1)))
    'succeeded))

(define rec-even?
  (lambda (n)
    (if (zero? n)
	#t
	(rec-odd? (- n 1)))))

(define rec-odd?
  (lambda (n)
    (if (zero? n)
	#f
	(rec-even? (- n 1)))))

(define rec-even-with-begin?
  (lambda (n)
    (if (zero? n)
	#t
	(begin
	  (+ 1 2)  ;; dummy
	  (rec-odd-with-begin? (- n 1))))))

(define rec-odd-with-begin?
  (lambda (n)
    (if (zero? n)
	#f
	(begin
	  (+ 1 2)  ;; dummy
	  (rec-even-with-begin? (- n 1))))))

(define rec-even-with-let?
  (lambda (n)
    (if (zero? n)
	#t
	(let ((dummy (+ 1 2))
	      (dummy2 (+ 3 4)))
	  (rec-odd-with-let? (- n 1))))))

(define rec-odd-with-let?
  (lambda (n)
    (if (zero? n)
	#f
	(let ((dummy (+ 1 2))
	      (dummy2 (+ 3 4)))
	  (rec-even-with-let? (- n 1))))))

(define rec-even-with-let*?
  (lambda (n)
    (if (zero? n)
	#t
	(let* ((dummy (+ 1 2))
	       (dummy2 (+ dummy 3)))
	  (rec-odd-with-let*? (- n 1))))))

(define rec-odd-with-let*?
  (lambda (n)
    (if (zero? n)
	#f
	(let* ((dummy (+ 1 2))
	       (dummy2 (+ dummy 3)))
	  (rec-even-with-let*? (- n 1))))))

(define rec-even-with-letrec?
  (lambda (n)
    (if (zero? n)
	#t
	(letrec ((dummy (lambda () dummy2))
		 (dummy2 (lambda () dummy)))
	  (rec-odd-with-letrec? (- n 1))))))

(define rec-odd-with-letrec?
  (lambda (n)
    (if (zero? n)
	#f
	(letrec ((dummy (lambda () dummy2))
		 (dummy2 (lambda () dummy)))
	  (rec-even-with-letrec? (- n 1))))))

(define rec-continuation
  (lambda (n)
    (if (zero? n)
	'succeeded
        (call-with-current-continuation
         (lambda (cont)
           (rec-continuation (- n 1)))))))

(define rec-call-with-values
  (lambda (n)
    (if (zero? n)
	'succeeded
        (call-with-values
            (lambda () (values 2 3 n))
          (lambda (dummy1 dummy2 n)
            (rec-call-with-values (- n 1)))))))

(define rec-receive
  (lambda (n)
    (if (zero? n)
	'succeeded
        (receive (dummy1 dummy2 n) (values 2 3 n)
          (rec-receive (- n 1))))))

(define rec-proper-infinite
  (lambda (cnt)
    (rec-proper-infinite (+ cnt 1))))

(define rec-improper-infinite
  (lambda (cnt)
    (if (zero? cnt)
	(error "explicit explosion of improper infinite tail recursion failed")
	(rec-improper-infinite (- cnt 1)))
    'dummy))


;; if
(assert-equal? "proper tail recursion by if-consequent"
	       'succeeded
	       (rec-by-if-consequent explosive-count))
(assert-equal? "proper tail recursion by if-consequent with begin"
	       'succeeded
	       (rec-by-if-consequent-with-begin explosive-count))
(assert-equal? "proper tail recursion by if-consequent with let"
	       'succeeded
	       (rec-by-if-consequent-with-let explosive-count))
(assert-equal? "proper tail recursion by if-consequent with let*"
	       'succeeded
	       (rec-by-if-consequent-with-let* explosive-count))
(assert-equal? "proper tail recursion by if-consequent with letrec"
	       'succeeded
	       (rec-by-if-consequent-with-letrec explosive-count))
(assert-equal? "proper tail recursion by if-alternate"
	       'succeeded
	       (rec-by-if-alternate explosive-count))
(assert-equal? "proper tail recursion by if-alternate with begin"
	       'succeeded
	       (rec-by-if-alternate-with-begin explosive-count))
(assert-equal? "proper tail recursion by if-alternate with let"
	       'succeeded
	       (rec-by-if-alternate-with-let explosive-count))
(assert-equal? "proper tail recursion by if-alternate with let*"
	       'succeeded
	       (rec-by-if-alternate-with-let* explosive-count))
(assert-equal? "proper tail recursion by if-alternate with letrec"
	       'succeeded
	       (rec-by-if-alternate-with-letrec explosive-count))

;; cond
(assert-equal? "proper tail recursion by 1st clause of cond"
	       'succeeded
	       (rec-by-cond-1st explosive-count))
(assert-equal? "proper tail recursion by 1st clause of cond with begin"
	       'succeeded
	       (rec-by-cond-1st-with-begin explosive-count))
(assert-equal? "proper tail recursion by 1st clause of cond with let"
	       'succeeded
	       (rec-by-cond-1st-with-let explosive-count))
(assert-equal? "proper tail recursion by 1st clause of cond with let*"
	       'succeeded
	       (rec-by-cond-1st-with-let* explosive-count))
(assert-equal? "proper tail recursion by 1st clause of cond with letrec"
	       'succeeded
	       (rec-by-cond-1st-with-letrec explosive-count))
(assert-equal? "proper tail recursion by 2nd clause of cond"
	       'succeeded
	       (rec-by-cond-2nd explosive-count))
(assert-equal? "proper tail recursion by 3rd clause of cond"
	       'succeeded
	       (rec-by-cond-3rd explosive-count))
(assert-equal? "proper tail recursion by last clause of cond"
	       'succeeded
	       (rec-by-cond-last explosive-count))
(assert-equal? "proper tail recursion by cond-else"
	       'succeeded
	       (rec-by-cond-else explosive-count))
(assert-equal? "proper tail recursion by cond-else with begin"
	       'succeeded
	       (rec-by-cond-else-with-begin explosive-count))
(assert-equal? "proper tail recursion by cond-else with let"
	       'succeeded
	       (rec-by-cond-else-with-let explosive-count))
(assert-equal? "proper tail recursion by cond-else with let*"
	       'succeeded
	       (rec-by-cond-else-with-let* explosive-count))
(assert-equal? "proper tail recursion by cond-else with letrec"
	       'succeeded
	       (rec-by-cond-else-with-letrec explosive-count))

;; case
(assert-equal? "proper tail recursion by 1st clause of case"
	       'succeeded
	       (rec-by-case-1st explosive-count))
(assert-equal? "proper tail recursion by 1st clause of case with begin"
	       'succeeded
	       (rec-by-case-1st-with-begin explosive-count))
(assert-equal? "proper tail recursion by 1st clause of case with let"
	       'succeeded
	       (rec-by-case-1st-with-let explosive-count))
(assert-equal? "proper tail recursion by 1st clause of case with let*"
	       'succeeded
	       (rec-by-case-1st-with-let* explosive-count))
(assert-equal? "proper tail recursion by 1st clause of case with letrec"
	       'succeeded
	       (rec-by-case-1st-with-letrec explosive-count))
(assert-equal? "proper tail recursion by 2nd clause of case"
	       'succeeded
	       (rec-by-case-2nd explosive-count))
(assert-equal? "proper tail recursion by 3rd clause of case"
	       'succeeded
	       (rec-by-case-3rd explosive-count))
(assert-equal? "proper tail recursion by last clause of case"
	       'succeeded
	       (rec-by-case-last explosive-count))
(assert-equal? "proper tail recursion by case-else"
	       'succeeded
	       (rec-by-case-else explosive-count))
(assert-equal? "proper tail recursion by case-else with begin"
	       'succeeded
	       (rec-by-case-else-with-begin explosive-count))
(assert-equal? "proper tail recursion by case-else with let"
	       'succeeded
	       (rec-by-case-else-with-let explosive-count))
(assert-equal? "proper tail recursion by case-else with let*"
	       'succeeded
	       (rec-by-case-else-with-let* explosive-count))
(assert-equal? "proper tail recursion by case-else with letrec"
	       'succeeded
	       (rec-by-case-else-with-letrec explosive-count))

;; and
(if test-and?
    (begin
      (assert-equal? "proper tail recursion by and-tail"
		     'succeeded
		     (or (rec-by-and-tail explosive-count)
			 'succeeded))
      (assert-equal? "proper tail recursion by and-tail with begin"
		     'succeeded
		     (or (rec-by-and-tail-with-begin explosive-count)
			 'succeeded))
      (assert-equal? "proper tail recursion by and-tail with let"
		     'succeeded
		     (or (rec-by-and-tail-with-let explosive-count)
			 'succeeded))
      (assert-equal? "proper tail recursion by and-tail with let*"
		     'succeeded
		     (or (rec-by-and-tail-with-let* explosive-count)
			 'succeeded))
      (assert-equal? "proper tail recursion by and-tail with letrec"
		     'succeeded
		     (or (rec-by-and-tail-with-letrec explosive-count)
			 'succeeded))))
;; improper and: intentionally explodes
(if test-improper-and?
    (assert-equal? "improper tail recursion by and-tail"
		   'succeeded
		   (improper-rec-by-and-tail explosive-count)))

;; or
(if test-or?
    (begin
      (assert-equal? "proper tail recursion by or-tail"
		     'succeeded
		     (and (rec-by-or-tail explosive-count)
			  'succeeded))
      (assert-equal? "proper tail recursion by or-tail with begin"
		     'succeeded
		     (and (rec-by-or-tail-with-begin explosive-count)
			  'succeeded))
      (assert-equal? "proper tail recursion by or-tail with let"
		     'succeeded
		     (and (rec-by-or-tail-with-let explosive-count)
			  'succeeded))
      (assert-equal? "proper tail recursion by or-tail with let*"
		     'succeeded
		     (and (rec-by-or-tail-with-let* explosive-count)
			  'succeeded))
      (assert-equal? "proper tail recursion by or-tail with letrec"
		     'succeeded
		     (and (rec-by-or-tail-with-letrec explosive-count)
			  'succeeded))))
;; improper or: intentionally explodes
(if test-improper-or?
    (assert-equal? "improper tail recursion by or-tail"
		   'succeeded
		   (improper-rec-by-or-tail explosive-count)))

;; do
(assert-equal? "iteration by do"
	       'succeeded
	       (do ((cnt explosive-count (- cnt 1))
		    (dummy 0 (+ dummy 1)))
		   ((zero? cnt)
		    'succeeded)
		 (+ cnt dummy)))
(assert-equal? "proper tail recursion with do"
	       'succeeded
	       (let loop ((loop-cnt explosive-count))
		 (if (zero? loop-cnt)
		     'succeeded
		     (do ((cnt 3 (- cnt 1))
			  (dummy 0 (+ dummy 1)))
			 ((zero? cnt)
			  (loop (- loop-cnt 1)))
		       (+ cnt dummy)))))

;; flip-flop procs
(assert-equal? "proper tail recursion by flip-flop procs"
	       'succeeded
	       (and (rec-even? explosive-count)
		    'succeeded))
(assert-equal? "proper tail recursion by flip-flop procs with begin"
	       'succeeded
	       (and (rec-even-with-begin? explosive-count)
		    'succeeded))
(assert-equal? "proper tail recursion by flip-flop procs with let"
	       'succeeded
	       (and (rec-even-with-let? explosive-count)
		    'succeeded))
(assert-equal? "proper tail recursion by flip-flop procs with let*"
	       'succeeded
	       (and (rec-even-with-let*? explosive-count)
		    'succeeded))
(assert-equal? "proper tail recursion by flip-flop procs with letrec"
	       'succeeded
	       (and (rec-even-with-letrec? explosive-count)
		    'succeeded))

;; flip-flop procs in letrec
(assert-equal? "proper tail recursion by flip-flop procs defined by letrec"
	       'succeeded
	       (letrec ((my-even? (lambda (n)
				    (if (zero? n)
					#t
					(my-odd? (- n 1)))))
			(my-odd? (lambda (n)
				   (if (zero? n)
				       #f
				       (my-even? (- n 1))))))
		 (and (my-even? explosive-count)
		      'succeeded)))

;; named let
(assert-equal? "proper tail recursion by named let"
	       'succeeded
	       (let loop ((cnt explosive-count))
		 (if (zero? cnt)
		     'succeeded
		     (loop (- cnt 1)))))
(assert-equal? "proper tail recursion by named let with begin"
	       'succeeded
	       (let loop ((cnt explosive-count))
		 (if (zero? cnt)
		     'succeeded
		     (begin
		       (+ 1 2)  ;; dummy
		       (loop (- cnt 1))))))
(assert-equal? "proper tail recursion by named let with let"
	       'succeeded
	       (let loop ((cnt explosive-count))
		 (if (zero? cnt)
		     'succeeded
		     (let ((dummy (+ 1 2))
			   (dummy2 (+ 3 4)))
		       (loop (- cnt 1))))))
(assert-equal? "proper tail recursion by named let with let*"
	       'succeeded
	       (let loop ((cnt explosive-count))
		 (if (zero? cnt)
		     'succeeded
		     (let* ((dummy (+ 1 2))
			    (dummy2 (+ dummy 3)))
		       (loop (- cnt 1))))))
(assert-equal? "proper tail recursion by named let with letrec"
	       'succeeded
	       (let loop ((cnt explosive-count))
		 (if (zero? cnt)
		     'succeeded
		     (letrec ((dummy (+ 1 2))
			      (dummy2 (+ 3 4)))
		       (loop (- cnt 1))))))

;; call/cc

;; SigScheme cannot run this test as proper tail recursion. The stack will
;; grown.
;;(assert-equal? "proper tail recursion by call/cc"
;;               'succeeded
;;               (rec-continuation explosive-count))

;; call-with-values
(assert-equal? "proper tail recursion by call-with-values"
               'succeeded
               (rec-call-with-values explosive-count))

;; receive
(assert-equal? "proper tail recursion by receive"
               'succeeded
               (rec-receive explosive-count))

;; This test is succeeded if [OK]-exploded message sequence has been
;; printed as follows.
;;
;; [OK]
;;
;; check intentional 'exploded' message printed below
;; <system dependent ulimit exceeded error message>
;; exploded
(total-report)
(display "check intentional 'correctly exploded' message printed below")
(newline)

;; test whether the explosive-count is actually explosive
(assert-equal? "improper infinite tail recursion"
	       'succeeded
	       (rec-improper-infinite explosive-count))

;; infinite loop
(assert-equal? "proper infinite tail recursion"
	       'succeeded
	       (rec-proper-infinite 0))
