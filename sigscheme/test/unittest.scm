;;  FileName : unittest.scm
;;  About    : Simple unit test library
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

(define total-err-num  0)
(define total-test-num 0)
(define test-filename "unspecified")

(define total-report
  (lambda ()
    (begin
      (if (= total-err-num 0)
	  (begin
	    (display "OK : ")
	    (display total-test-num)
	    (display " testcase(s) passed")
	    (newline))
	  (begin
	    (display "FAILED : ")
	    (display total-err-num)
	    (display " testcase(s) out of ")
	    (display total-test-num)
	    (display " testcase(s) failed")
	    (newline))))))

(define report-error
  (lambda (errmsg)
    (begin
      (display "error : ")
      (display errmsg)
      (newline))))

(define assert
  (lambda (msg exp)
    (set! total-test-num (+ total-test-num 1))
    (if exp
	#t
	(begin
	  (set! total-err-num (+ total-err-num 1))
	  (report-error msg)
	  #f))))

(define assert-true assert)

(define assert-false
  (lambda (msg exp)
    (assert msg (not exp))))

(define assert-eq?
  (lambda (msg a b)
    (if (not (assert msg (eq? a b)))
	(begin
	  (display "assert-eq? : we expect ")
	  (write a)
	  (display " but got ")
	  (write b)
	  (newline)))))

(define assert-equal?
  (lambda (msg a b)
    (if (not (assert msg (equal? a b)))
	(begin
	  (display "assert-equal? : we expect ")
	  (write a)
	  (display " but got ")
	  (write b)
	  (newline)))))

(define (eval-counter n)
  (list 'eval-counter (+ n 1)))

;; dummy definition to eval args for assert-error. real implementation needed.
(define assert-error
  (lambda (msg exp)
    #f))
