;;  Filename : test-continuation.scm
;;  About    : unit test for continuation
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

(if (not (symbol-bound? 'call-with-current-continuation))
    (test-skip "R5RS continuation is not enabled"))

(define tn test-name)
(define call/cc call-with-current-continuation)

(tn "call/cc invalid forms")
;; no procedure
(assert-error  (tn) (lambda ()
                      (call-with-current-continuation)))
;; not a procedure
(assert-error  (tn) (lambda ()
                      (call-with-current-continuation #t)))
;; excessive
(assert-error  (tn) (lambda ()
                      (call-with-current-continuation procedure? #t)))

(tn "call/cc")
;; not applicable
(assert-error  (tn) (lambda ()
                      (call-with-current-continuation +)))

(assert-equal? (tn)
               -3
               (call-with-current-continuation
                (lambda (exit)
                  (for-each (lambda (x)
                              (if (negative? x)
                                  (exit x)))
                            '(54 0 37 -3 245 19))
                  #t)))

(define list-length
  (lambda (obj)
    (call-with-current-continuation
     (lambda (return)
       (letrec ((re
		 (lambda (obj1)
		   (cond ((null? obj1) 0)
			 ((pair? obj1)
			  (+ (re (cdr obj1)) 1))
			 (else
			  (return #f))))))
      (re obj))))))

(assert-equal? (tn) 4  (list-length '(1 2 3 4)))
(assert-equal? (tn) #f (list-length '(a b . c)))

;; function written in C as proc
(assert-true   (tn) (call/cc procedure?))

;; another continuation as proc
(assert-true   (tn) (procedure? (call/cc (lambda (c) (call/cc c)))))

(assert-equal? (tn) 'ret-call/cc
	       (call-with-current-continuation
		(lambda (k)
		  'ret-call/cc)))

(assert-equal? (tn) 'ret-call/cc
	       (call-with-current-continuation
		(lambda (k)
		  (k 'ret-call/cc))))

;; Call an expired continuation. Current SigScheme cause an error due to its
;; setjmp/longjmp implementation.
(assert-error  (tn)
               (lambda ()
                 (let ((res (call-with-current-continuation
                             (lambda (k)
                               k))))
                   (if (procedure? res)
                       (res 'succeeded)
                       res))))

;; "6.4 Control features" of R5RS:
;; The escape procedure accepts the same number of arguments as the
;; continuation to the original call to call-with-current-continuation.
;; Except for continuations created by the `call-with-values' procedure, all
;; continuations take exactly one value.
(assert-error (tn)
              (lambda ()
                (call-with-current-continuation
                 (lambda (k)
                   (k (values 1 2))))))

(assert-error (tn)
              (lambda ()
                (call-with-current-continuation
                 (lambda (k)
                   (k (values))))))

;; one value is OK
(assert-equal? (tn)
               1
               (call-with-current-continuation
                (lambda (k)
                  (k (values 1)))))

(tn "call/cc SigScheme-specific behavior")
(if (and (provided? "sigscheme")
         (provided? "nested-continuation-only"))
    ;; expired continuation
    (assert-error  (tn) (lambda ()
                          ((call/cc (lambda (c) c))
                           procedure?)))
    (assert-true   (tn) ((call/cc (lambda (c) c))
                         procedure?)))

(total-report)
