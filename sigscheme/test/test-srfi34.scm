;;  Filename : test-srfi34.scm
;;  About    : unit test for SRFI-34
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

(if (not (provided? "srfi-34"))
    (test-skip "SRFI-34 is not enabled"))

(use srfi-8)

(set! *test-track-progress* #f)

(define tn test-name)

(define my-assert-error
  (lambda (test-name proc)
    (assert-error test-name (lambda ()
                              (guard (var
                                      ;; not an error but user-raised object
                                      ((eq? var 'obj)
                                       #f))
                                (proc))))))

;;
;; raise
;;

(tn "raise")
;; no arg
(assert-error (tn) (lambda ()
                     (raise)))
;; multiple values are not allowed
(assert-error (tn) (lambda ()
                     (raise (values 1 2 3))))
;; no guard or raw exception handler
(assert-error (tn) (lambda ()
                     (raise 'exception)))

;;
;; with-exception-handler
;;

(tn "with-exception-handler invalid form")

;; handler is not a procedure
(my-assert-error (tn) (lambda ()
                        (with-exception-handler
                            'a-handler-must-not-return
                          (lambda ()
                            (+ 1 (raise 'obj))))))

;; handler is a procedure but takes 2 arguments
(my-assert-error (tn) (lambda ()
                        (with-exception-handler
                            eq?
                          (lambda ()
                            (+ 1 (raise 'obj))))))

;; thunk is not a procedure
(my-assert-error (tn) (lambda ()
                        (with-exception-handler
                            (lambda (x)
                              'a-handler-must-not-return)
                          'an-error)))

;; thunk is a procedure but takes an argument
(my-assert-error (tn) (lambda ()
                        (with-exception-handler
                            (lambda (x)
                              'a-handler-must-not-return)
                          (lambda (dummy)
                            (+ 1 (raise 'obj))))))

(tn "with-exception-handler")

;; Although the behavior when a handler returned is not specified in SRFI-34,
;; SigScheme should produce an error to prevent being misused by users.
(if sigscheme?
    (my-assert-error (tn) (lambda ()
                            (with-exception-handler
                                (lambda (x)
                                  'a-handler-must-not-return)
                              (lambda ()
                                (+ 1 (raise 'obj)))))))

(assert-error (tn) (lambda ()
                     (with-exception-handler
                         (lambda (x)
                           (assert-equal? (tn) 'an-error x)
                           'a-handler-must-not-return)
                       (lambda ()
                         (+ 1 (raise 'an-error))))))

(assert-equal? (tn)
               6
	       (with-exception-handler
                   (lambda (x)
                     'not-reaches-here)
                 (lambda ()
                   (+ 1 2 3))))

(assert-equal? (tn)
               'success
	       (with-exception-handler
                   (lambda (x)
                     'not-reaches-here)
                 (lambda ()
                   'success)))

;; multiple values are allowed for thunk
(assert-equal? (tn)
               '(1 2 3)
               (receive vals
                   (with-exception-handler
                       (lambda (x)
                         'not-reaches-here)
                     (lambda ()
                       (values 1 2 3)))
                 vals))


;;
;; guard
;;

(tn "guard")
(assert-equal? (tn)
               'exception
	       (guard (condition
		       (else
			(assert-equal? (tn) 'an-error condition)
			'exception))
                 (+ 1 (raise 'an-error))))

(assert-equal? (tn)
               3
	       (guard (condition
		       (else
			'exception))
                 (+ 1 2)))

(assert-equal? (tn)
               'success
	       (guard (condition
		       (else
			'exception))
                 'success))

(assert-equal? (tn)
               'exception
	       (guard (condition
		       (else
			'exception))
                 (+ 1 (raise 'error))))

(assert-equal? (tn)
               42
               (guard (condition
                       ((assq 'a condition) => cdr)
                       ((assq 'b condition))
                       (else
                        (display condition)
                        (newline)))
                 (raise (list (cons 'a 42)))))

(assert-equal? (tn)
               '(b . 23)
               (guard (condition
                       ((assq 'a condition) => cdr)
                       ((assq 'b condition))
                       (else
                        (display condition)
                        (newline)))
                 (raise (list (cons 'b 23)))))

;; not matched against => and fall through to else
(assert-equal? (tn)
               'else
               (guard (condition
                       ((assv condition '((a 1) (b 2))) => cadr)
                       (else 'else))
                 (raise 'c)))

;;
;; handler part of guard
;;

(tn "guard handler invalid form")

(my-assert-error (tn) (lambda ()
                        (guard (var)
                          (raise 'obj))))

(my-assert-error (tn) (lambda ()
                        (guard (var
                                ())
                          (raise 'obj))))

(my-assert-error (tn) (lambda ()
                        (guard (var
                                ()
                                (else #t))
                          (raise 'obj))))

;; 'else' followed by another caluse
(my-assert-error (tn) (lambda ()
                        (guard (var
                                (else #t)
                                (#t))
                          (raise 'obj))))

;; not specified in R5RS 'case', but SigScheme should cause error
(if (provided? "sigscheme")
    (my-assert-error (tn) (lambda ()
                            (guard (var
                                    (else))
                              (raise 'obj)))))

(my-assert-error (tn) (lambda ()
                        (guard (var
                                (#t =>))
                          (raise 'obj))))

(my-assert-error (tn) (lambda ()
                        (guard (var
                                (#t =>)
                                (else #t))
                          (raise 'obj))))

(my-assert-error (tn) (lambda ()
                        (guard (var
                                (else =>))
                          (raise 'obj))))

;; not a procedure
(my-assert-error (tn) (lambda ()
                        (guard (var
                                (#t => #t))
                          (raise 'obj))))

;; not a procedure but #f
(my-assert-error (tn) (lambda ()
                        (guard (var
                                (#t => #f))
                          (raise 'obj))))

;; procedure but argument number mismatch
(my-assert-error (tn) (lambda ()
                        (guard (var
                                (#t => eq?))
                          (raise 'obj))))

;; not a procedure but a syntax
(my-assert-error (tn) (lambda ()
                        (guard (var
                                (#t => delay))
                          (raise 'obj))))

(tn "guard namespace taintlessness")

(assert-false (tn) (guard (var
                           (#f var))
                     (symbol-bound? 'lex-env)))

(assert-false (tn) (guard (var
                           (#f var))
                     (symbol-bound? 'cond-catch)))

(assert-false (tn) (guard (var
                           (#f var))
                     (symbol-bound? 'body)))

(assert-false (tn) (guard (var
                           (#f var))
                     (symbol-bound? 'condition)))

(assert-false (tn) (guard (var
                           (#f var))
                     (symbol-bound? 'guard-k)))

(assert-false (tn) (guard (var
                           (#f var))
                     (symbol-bound? 'handler-k)))

(assert-false (tn) (guard (var
                           (#f var))
                     (symbol-bound? 'var)))

(tn "guard handler namespace taintlessness")

(assert-false (tn) (guard (var
                           (else
                            (symbol-bound? 'lex-env)))
                     (raise 'err)))

(assert-false (tn) (guard (var
                           (else
                            (symbol-bound? 'cond-catch)))
                     (raise 'err)))

(assert-false (tn) (guard (var
                           (else
                            (symbol-bound? 'body)))
                     (raise 'err)))

(assert-false (tn) (guard (var
                           (else
                            (symbol-bound? 'condition)))
                     (raise 'err)))

(assert-false (tn) (guard (var
                           (else
                            (symbol-bound? 'guard-k)))
                     (raise 'err)))

(assert-false (tn) (guard (var
                           (else
                            (symbol-bound? 'handler-k)))
                     (raise 'err)))

(tn "guard handler condition variable")

(assert-equal? (tn)
               'err
               (guard (var
                       (else var))
                 (raise 'err)))

;; the variable can be modified
(assert-equal? (tn)
               'ERR
               (guard (var
                       (#t
                        (set! var 'ERR)
                        var))
                 (raise 'err)))

;; the variable does not affect outer environment
(define var 'global-var)
(assert-equal? (tn)
               'outer
               (let ((var 'outer))
                 (guard (var
                         (#t
                          (set! var 'ERR)))
                   (raise 'err))
                 var))

;; the variable does not affect global one
(define var 'global-var)
(assert-equal? (tn)
               'global-var
               (begin
                 (guard (var
                         (#t
                          (set! var 'ERR)))
                   (raise 'err))
                 var))

(tn "guard evaluation count exactness")

(assert-equal? (tn)
               7
               (guard (var
                       (else var))
                 (+ 3 4)))

(assert-equal? (tn)
               7
               (guard (var
                       (else var))
                 (raise (+ 3 4))))

(assert-equal? (tn)
               7
               (guard (var
                       (else (+ 3 4)))
                 (raise 'err)))

(assert-equal? (tn)
               7
               (let ((a 3)
                     (b 4))
                 (guard (var
                         (else var))
                   (+ a b))))

(assert-equal? (tn)
               7
               (let ((a 3)
                     (b 4))
                 (guard (var
                         (else var))
                   (raise (+ a b)))))

(assert-equal? (tn)
               7
               (let ((a 3)
                     (b 4))
                 (guard (var
                         (else (+ a b)))
                   (raise 'err))))

(assert-equal? (tn)
               (list + 3 4)  ;; not 7
               (let ((a 3)
                     (b 4))
                 (guard (var
                         (else var))
                   (list + a b))))

(assert-equal? (tn)
               (list + 3 4)  ;; not 7
               (let ((a 3)
                     (b 4))
                 (guard (var
                         (else var))
                   (raise (list + a b)))))

(assert-equal? (tn)
               (list + 3 4)  ;; not 7
               (let ((a 3)
                     (b 4))
                 (guard (var
                         (else (list + a b)))
                   (raise 'err))))

(tn "guard with multiple values")

(assert-equal? (tn)
               '(1 2)
               (receive vals
                   (guard (var
                           (else var))
                     (values 1 2))
                 vals))

(assert-equal? (tn)
               '(1 2)
               (receive vals
                   (guard (var
                           (else (values 1 2)))
                     (raise 'err))
                 vals))

(if (provided? "sigscheme")
    (assert-error (tn)
                  (lambda ()
                    (guard (var
                            ((not (%%error-object? var))
                             var))
                      (raise (values 1 2))))))

(tn "guard handler reraise")

(assert-equal?  (tn)
                'reraised
                (guard (var
                        ((eq? var 'error)
                         'reraised))
                  (guard (var
                          (#f))
                    (raise 'error))))
(assert-equal?  (tn)
                'reraised
                (guard (var
                        ((eq? var 'error)
                         'reraised))
                  (guard (var
                          ((even? 3) #f)
                          ((positive? -1) #f))
                    (raise 'error))))
(assert-error (tn) (lambda ()
                     (guard (condition
                             ((positive? condition) 'positive)
                             ((negative? condition) 'negative))
                       (raise 0))))

(tn "guard handler tested value as result")

;; R5RS: If the selected <clause> contains only the <test> and no
;; <expression>s, then the value of the <test> is returned as the result.
(assert-equal?  (tn)
                #t
                (guard (var
                        (#t))
                  (raise 'error)))
(assert-equal?  (tn)
                3
                (guard (var
                        (#f)
                        (3))
                  (raise 'error)))
(assert-equal?  (tn)
                3
                (guard(var
                       (#f)
                       (3)
                       (4))
                  (raise 'error)))

;;
;; mixed use of with-exception-handler and guard
;;

(tn "mixed exception handling")

(assert-equal? (tn)
               'guard-ret
	       (with-exception-handler (lambda (x)
					 (k 'with-exception-ret))
                 (lambda ()
                   (guard (condition
                           (else
                            'guard-ret))
                     (raise 1)))))

(assert-error (tn)
              (lambda ()
                (with-exception-handler (lambda (x)
                                          'with-exception-ret
                                          ;; a exception handler must not
                                          ;; return (as specified in SRFI-34)
                                          )
                  (lambda ()
                    (guard (condition
                            ((negative? condition)
                             'guard-ret))
                      (raise 1))))))

(assert-equal? (tn)
               'positive
               (call-with-current-continuation
                (lambda (k)
                  (with-exception-handler (lambda (x)
                                            (k 'zero))
                    (lambda ()
                      (guard (condition
                              ((positive? condition) 'positive)
                              ((negative? condition) 'negative))
                        (raise 1)))))))

(assert-equal? (tn)
               'negative
               (call-with-current-continuation
                (lambda (k)
                  (with-exception-handler (lambda (x)
                                            (k 'zero))
                    (lambda ()
                      (guard (condition
                              ((positive? condition) 'positive)
                              ((negative? condition) 'negative))
                        (raise -1)))))))

(assert-equal? (tn)
               'zero
               (call-with-current-continuation
                (lambda (k)
                  (with-exception-handler (lambda (x)
                                            (k 'zero))
                    (lambda ()
                      (guard (condition
                              ((positive? condition) 'positive)
                              ((negative? condition) 'negative))
                        (raise 0)))))))

(total-report)
