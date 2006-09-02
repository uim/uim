;;  Filename : test-eval.scm
;;  About    : unit test for evaluation
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

(define tn test-name)

(tn "eval")
(assert-equal? (tn) 3 (eval '(+ 1 2)
                            (interaction-environment)))

(assert-equal? (tn) 3 (eval '((lambda (x y) (+ x y)) 1 2)
                            (interaction-environment)))

(tn "eval with invalid environment specifiers")
(assert-error  (tn) (lambda ()
                      (eval '(+ 1 2) 3)))
(assert-error  (tn) (lambda ()
                      (eval '(+ 1 2) 'symbol)))
(assert-error  (tn) (lambda ()
                      (eval '(+ 1 2) "string")))
(assert-error  (tn) (lambda ()
                      (eval '(+ 1 2) #\a)))

;; R5RS: 4.1.3 Procedure calls
;; > Procedure calls may return any number of values (see values in section see
;; > section 6.4 Control features). With the exception of `values' the
;; > procedures available in the initial environment return one value or, for
;; > procedures such as `apply', pass on the values returned by a call to one
;; > of their arguments.
;; SigScheme apply this specification for 'eval' also.  -- YamaKen 2006-09-02
(tn "eval that returns multiple values")
(call-with-values
    (lambda ()
      (eval '(values 1 2 3)
            (interaction-environment)))
  (lambda vals
    (assert-equal? (tn) '(1 2 3) vals)))
(call-with-values
    (lambda ()
      (eval '(apply values '(1 2 3))
            (interaction-environment)))
  (lambda vals
    (assert-equal? (tn) '(1 2 3) vals)))

(tn "scheme-report-environment")
(if (provided? "sigscheme")
    (begin
      (assert-error  (tn) (lambda ()
                            (eval '(+ 1 2)
                                  (scheme-report-environment 4))))
      (assert-error  (tn) (lambda ()
                            (eval '(+ 1 2)
                                  (scheme-report-environment 6))))))
(assert-error  (tn) (lambda ()
                      (eval '(+ 1 2)
                            (scheme-report-environment 'symbol))))
(assert-error  (tn) (lambda ()
                      (eval '(+ 1 2)
                            (scheme-report-environment "string"))))
(assert-error  (tn) (lambda ()
                      (eval '(+ 1 2)
                            (scheme-report-environment #\a))))
(assert-equal? (tn) 3 (eval '(+ 1 2)
                            (scheme-report-environment 5)))
;; R5RS: 6.5 Eval
;; `eval' is not allowed to create new bindings in the environments associated
;; with `null-environment' or `scheme-report-environment'.
;; FIXME: SigScheme does not support this yet.
(if (and #f (provided? "sigscheme"))
    (begin
      (assert-error  (tn) (lambda ()
                            ;; Although other implementations evaluate this
                            ;; expression without error (with or without 'foo'
                            ;; defined), SigScheme will adopt this behavior.
                            (eval '(define foo 1)
                                  (scheme-report-environment 5))))
      (assert-error  (tn) (lambda ()
                            (eval 'use
                                  (scheme-report-environment 5))))))

(tn "null-environment")
(if (and #f (provided? "sigscheme"))
    (begin
      (assert-error  (tn) (lambda ()
                            (eval '(+ 1 2)
                                  (null-environment 4))))
      (assert-error  (tn) (lambda ()
                            (eval '(+ 1 2)
                                  (null-environment 6))))))
(assert-error  (tn) (lambda ()
                      (eval '(+ 1 2)
                            (null-environment 'symbol))))
(assert-error  (tn) (lambda ()
                      (eval '(+ 1 2)
                            (null-environment "string"))))
(assert-error  (tn) (lambda ()
                      (eval '(+ 1 2)
                            (null-environment #\a))))
(assert-equal? (tn) 3 (eval '(+ 1 2)
                            (null-environment 5)))
;; R5RS: 6.5 Eval
;; `eval' is not allowed to create new bindings in the environments associated
;; with `null-environment' or `scheme-report-environment'.
;; FIXME: SigScheme does not support this yet.
(if (and #f (provided? "sigscheme"))
    (begin
      (assert-error  (tn) (lambda ()
                            ;; Although other implementations evaluate this
                            ;; expression without error (with or without 'foo'
                            ;; defined), SigScheme will adopt this behavior.
                            (eval '(define foo 1)
                                  (null-environment 5))))
      (assert-error  (tn) (lambda ()
                            (eval 'use
                                  (null-environment 5))))
      (assert-error  (tn) (lambda ()
                            (eval 'procedure?
                                  (null-environment 5))))))

(if (provided? "sigscheme")
    (begin
      (tn "eval with handmade env")
      ;; single frame
      (assert-equal? (tn) 10 (eval '(+ x y)
                                   '(((x y) . (4 6)))))
      ;; 2 frames
      (assert-equal? (tn) 15 (eval '(+ x y z)
                                   '(((x y) . (4 6))
                                     ((z)   . (5)))))
      ;; 3 frames
      (assert-equal? (tn) 14 (eval '(+ x y z v w)
                                   '(((x y) . (4 6))
                                     ((v w) . (0 -1))
                                     ((z)   . (5)))))
      ;; dotted arg as formals
      (assert-equal? (tn) 44 (eval '(apply + lst)
                                   '(((x y . lst) . (4 6 8 10 12 14))
                                     ((z)  . (5)))))
      ;; symbol as formals
      (assert-equal? (tn) 54 (eval '(apply + lst)
                                   '((lst . (4 6 8 10 12 14))
                                     ((z) . (5)))))

      (tn "eval with invalid handmade env")
      ;; improper frame list
      (assert-error  (tn) (lambda ()
                            (eval '(+ 1 2)
                                  '(((x y) . (4 6))
                                    . #t))))
      ;; actuals shortage
      (assert-error  (tn) (lambda ()
                            (eval '(+ 1 2)
                                  '(((x y z) . (4 6))))))
      ;; actuals shortage #2
      (assert-error  (tn) (lambda ()
                            (eval '(+ 1 2)
                                  '(((x y . z) . (4))))))
      ;; superfluous actuals
      (assert-error  (tn) (lambda ()
                            (eval '(+ 1 2)
                                  '(((x y) . (4 6 8))))))
      ;; dotted actuals
      (assert-error  (tn) (lambda ()
                            (eval '(+ 1 2)
                                  '(((x y) . (4 . 6))))))
      ;; dotted actuals #2
      (assert-error  (tn) (lambda ()
                            (eval '(+ 1 2)
                                  '(((x y) . (4 6 . 8))))))
      ;; dotted actuals #3
      (assert-error  (tn) (lambda ()
                            (eval '(+ 1 2)
                                  '(((x . y) . (4 6 . 8))))))
      ;; not a symbol in formals
      (assert-error  (tn) (lambda ()
                            (eval '(+ 1 2)
                                  '(((x 3) . (4 6))))))
      ;; not a list as actuals
      (assert-error  (tn) (lambda ()
                            (eval '(+ 1 2)
                                  '(((x) . 4)
                                    ((y) . 6)))))
      ;; not a list as both formals and actuals
      (assert-error  (tn) (lambda ()
                            (eval '(+ 1 2)
                                  '((x . 4)
                                    (y . 6)))))))

(total-report)
