;;  Filename : test-values.scm
;;  About    : unit tests for multiple values
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


;;
;; values
;;

;; These tests use explicit equivalence predicates instead of assert-equal?, to
;; avoid being affected by multiple-values -specific behavior.

(tn "values invalid forms")
;; Normal continuations accept exactly one value only.
(assert-error  (tn) (lambda () (eq? '() (values))))
(assert-error  (tn) (lambda () (eq? '() (apply values '()))))
(assert-error  (tn) (lambda () (eq? '() (values . 1))))
(assert-error  (tn) (lambda () (eq? '() (values 1 2))))
(assert-error  (tn) (lambda () (eq? '() (apply values '(1 2)))))
(assert-error  (tn) (lambda () (eq? '() (values 1 . 2))))

(tn "values disallowed places")
;; top-level variable
(assert-error  (tn) (lambda () (eval '(define foo (values 1 2 3))
                                     (interaction-environment))))
(define foo 1)
(assert-error  (tn) (lambda () (eval '(set! foo (values 1 2 3))
                                     (interaction-environment))))
;; internal variable
(assert-error  (tn) (lambda () (define bar (values 1 2 3))))
;; others
(assert-error  (tn) (lambda () (let ((bar (values 1 2 3))) #t)))
(assert-error  (tn) (lambda () (let* ((bar (values 1 2 3))) #t)))
(assert-error  (tn) (lambda () (letrec ((bar (values 1 2 3))) #t)))
(assert-error  (tn) (lambda () (if (values 1 2 3) #t)))
(assert-error  (tn) (lambda () (and (values 1 2 3) #t)))
(assert-error  (tn) (lambda () (or (values 1 2 3) #t)))
(assert-error  (tn) (lambda () (cond ((values 1 2 3) #t) (else #t))))
(assert-error  (tn) (lambda () (case (values 1 2 3) (else #t))))
(assert-error  (tn) (lambda () (begin (values 1 2 3) #t)))
(assert-error  (tn) (lambda () ((lambda () (values 1 2 3) #t))))

(tn "values")
;; Exactly one value.
(assert-true   (tn) (eqv? 1 (values 1)))
(assert-true   (tn) (eqv? 1 (apply values '(1))))
(assert-true   (tn) (eq? '() (values '())))
(assert-true   (tn) (eq? '() (apply values '(()))))
(assert-true   (tn) (eq? #f (values #f)))
(assert-true   (tn) (eq? #f (apply values '(#f))))

;; Returning multiple values in top-level is allowed (SigScheme-specific).
;; These forms test whether evaluations are passed without blowing up.
(values)
(values 1 2 3)
(apply values '())
(apply values '(1 2 3))
(begin
  (values))
(begin
  (values 1 2 3))
(begin
  (apply values '()))
(begin
  (apply values '(1 2 3)))

;;
;; call-with-values
;;

(tn "call-with-values invalid forms")
(assert-error  (tn) (lambda ()
                      (call-with-values)))
(assert-error  (tn) (lambda ()
                      (call-with-values even?)))
(assert-error  (tn) (lambda ()
                      (call-with-values even? #t)))
(assert-error  (tn) (lambda ()
                      (call-with-values #t even?)))

(tn "call-with-values")
(assert-equal? (tn)
               -1
               (call-with-values * -))

(assert-equal? (tn)
               'ok
               (call-with-values
                   (lambda () (values))
                 (lambda () 'ok)))
(assert-equal? (tn)
               '()
               (call-with-values
                   (lambda () (values))
                 (lambda args args)))
(assert-equal? (tn)
               'ok
               (call-with-values
                   (lambda () (apply values '()))
                 (lambda () 'ok)))
(assert-equal? (tn)
               '()
               (call-with-values
                   (lambda () (apply values '()))
                 (lambda args args)))

(assert-equal? (tn)
               1
               (call-with-values
                   (lambda () (values 1))
                 (lambda (x) x)))
(assert-equal? (tn)
               '(1)
               (call-with-values
                   (lambda () (values 1))
                 (lambda args args)))
(assert-equal? (tn)
               1
               (call-with-values
                   (lambda () (apply values '(1)))
                 (lambda (x) x)))
(assert-equal? (tn)
               '(1)
               (call-with-values
                   (lambda () (apply values '(1)))
                 (lambda args args)))

(assert-equal? (tn)
               '(1 2)
               (call-with-values
                   (lambda () (values 1 2))
                 (lambda (x y) (list x y))))
(assert-equal? (tn)
               '(1 2)
               (call-with-values
                   (lambda () (values 1 2))
                 (lambda args args)))
(assert-equal? (tn)
               '(1 2)
               (call-with-values
                   (lambda () (apply values '(1 2)))
                 (lambda (x y) (list x y))))
(assert-equal? (tn)
               '(1 2)
               (call-with-values
                   (lambda () (apply values '(1 2)))
                 (lambda args args)))

(tn "call-with-values by apply")
(assert-equal? (tn)
               -1
               (apply call-with-values (list * -)))

(assert-equal? (tn)
               'ok
               (apply call-with-values
                      (list (lambda () (values))
                            (lambda () 'ok))))
(assert-equal? (tn)
               '()
               (apply call-with-values
                      (list (lambda () (values))
                            (lambda args args))))
(assert-equal? (tn)
               'ok
               (apply call-with-values
                      (list (lambda () (apply values '()))
                            (lambda () 'ok))))
(assert-equal? (tn)
               '()
               (apply call-with-values
                      (list (lambda () (apply values '()))
                            (lambda args args))))

(assert-equal? (tn)
               1
               (apply call-with-values
                      (list (lambda () (values 1))
                            (lambda (x) x))))
(assert-equal? (tn)
               '(1)
               (apply call-with-values
                      (list (lambda () (values 1))
                            (lambda args args))))
(assert-equal? (tn)
               1
               (apply call-with-values
                      (list (lambda () (apply values '(1)))
                            (lambda (x) x))))
(assert-equal? (tn)
               '(1)
               (apply call-with-values
                      (list (lambda () (apply values '(1)))
                            (lambda args args))))

(assert-equal? (tn)
               '(1 2)
               (apply call-with-values
                      (list (lambda () (values 1 2))
                            (lambda (x y) (list x y)))))
(assert-equal? (tn)
               '(1 2)
               (apply call-with-values
                      (list (lambda () (values 1 2))
                            (lambda args args))))
(assert-equal? (tn)
               '(1 2)
               (apply call-with-values
                      (list (lambda () (apply values '(1 2)))
                            (lambda (x y) (list x y)))))
(assert-equal? (tn)
               '(1 2)
               (apply call-with-values
                      (list (lambda () (apply values '(1 2)))
                            (lambda args args))))

(tn "call-with-values misc")
;; test whether the variable is properly bound
(assert-equal? (tn)
               1
               ((lambda (n)
                  (call-with-values
                      (lambda () (values 2 3 n))
                    (lambda (dummy1 dummy2 n2)
                      n2)))
                1))


(total-report)
