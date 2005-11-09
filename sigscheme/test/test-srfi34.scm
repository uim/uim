;;  FileName : test-srfi34.scm
;;  About    : unit test for SRFI-34
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

(load "./test/unittest.scm")

(cond-expand
 (sigscheme
  (use srfi-34))
 (else #t))

(set! *test-track-progress* #t)

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

;; no guard or raw exception handler
(assert-error "raise #1" #f (lambda ()
                              (raise 'exception)))

;;
;; with-exception-handler
;;

;; handler is not a procedure
(my-assert-error "with-exception-handler invalid form #1"
                 (lambda ()
                   (with-exception-handler
                       'a-handler-must-not-return
                     (lambda ()
                       (+ 1 (raise 'obj))))))

;; FAILED: infinite loop with a message "Error: missing argument(s)"
;; handler a procedure but takes 2 arguments
;;(my-assert-error "with-exception-handler invalid form #2"
;;                 (lambda ()
;;                   (with-exception-handler
;;                       eq?
;;                     (lambda ()
;;                       (+ 1 (raise 'obj))))))

;; thunk is not a procedure
(my-assert-error "with-exception-handler invalid form #3"
                 (lambda ()
                   (with-exception-handler
                       (lambda (x)
                         'a-handler-must-not-return)
                     'an-error)))

;; handler a procedure but takes an argument
(my-assert-error "with-exception-handler invalid form #4"
                 (lambda ()
                   (with-exception-handler
                       (lambda (x)
                         'a-handler-must-not-return)
                     (lambda (dummy)
                       (+ 1 (raise 'obj))))))

;; FAILED
;; Although the behavior when a handler returned is not specified in SRFI-34,
;; SigScheme should produce an error to prevent being misused by users.
(if (provided? "sigscheme")
    (my-assert-error "with-exception-handler #1"
                     (lambda ()
                       (with-exception-handler
                           (lambda (x)
                             'a-handler-must-not-return)
                         (lambda ()
                           (+ 1 (raise 'obj)))))))

;; SEGV
;;(assert-error "with-exception-handler #3"
;;              (lambda ()
;;                (with-exception-handler
;;                    (lambda (x)
;;                      (assert-equal? "with-exception-handler #2" 'an-error x)
;;                      'a-handler-must-not-return)
;;                  (lambda ()
;;                    (+ 1 (raise 'an-error))))))

(assert-equal? "with-exception-handler #4"
               6
	       (with-exception-handler
                   (lambda (x)
                     'not-reaches-here)
                 (lambda ()
                   (+ 1 2 3))))

(assert-equal? "with-exception-handler #5"
               'success
	       (with-exception-handler
                   (lambda (x)
                     'not-reaches-here)
                 (lambda ()
                   'success)))


;; guard
(assert-equal? "guard #1"
               'exception
	       (guard (condition
		       (else
			(assert-equal? "guard #2" 'an-error condition)
			'exception))
                 (+ 1 (raise 'an-error))))

(assert-equal? "guard #3"
               3
	       (guard (condition
		       (else
			'exception))
                 (+ 1 2)))

(assert-equal? "guard #4"
               'success
	       (guard (condition
		       (else
			'exception))
                 'success))

(assert-equal? "guard #5"
               'exception
	       (guard (condition
		       (else
			'exception))
                 (+ 1 (raise 'error))))

(assert-equal? "guard #6"
               42
               (guard (condition
                       ((assq 'a condition) => cdr)
                       ((assq 'b condition))
                       (else
                        (display condition)
                        (newline)))
                 (raise (list (cons 'a 42)))))

(assert-equal? "guard #7"
               '(b . 23)
               (guard (condition
                       ((assq 'a condition) => cdr)
                       ((assq 'b condition))
                       (else
                        (display condition)
                        (newline)))
                 (raise (list (cons 'b 23)))))

;; not matched against => and fall through to else
(assert-equal? "guard #8"
               #f
               (guard (condition
                       ((assv condition '((a 1) (b 2))) => cadr)
                       (else #f))
                 (raise 'c)))

;;
;; handler part of guard
;;

;; SEGV
;;(my-assert-error  "guard handler invalid form #1"
;;                  (lambda ()
;;                    (guard (var)
;;                      (raise 'obj))))

;; FAILED
(my-assert-error  "guard handler invalid form #2"
                  (lambda ()
                    (guard (var
                            ())
                      (raise 'obj))))

;; FAILED
(my-assert-error  "guard handler invalid form #3"
                  (lambda ()
                    (guard (var
                            ()
                            (else #t))
                      (raise 'obj))))

;; SEGV
;; 'else' followed by another caluse
;;(my-assert-error  "guard handler invalid form #4"
;;                  (lambda ()
;;                    (guard (var
;;                            (else #t)
;;                            (#t))
;;                      (raise 'obj))))

;; SEGV
;; not specified in R5RS 'case', but SigScheme should cause error
;;(if (provided? "sigscheme")
;;    (my-assert-error  "guard handler invalid form #5"
;;                      (lambda ()
;;                        (guard (var
;;                                (else))
;;                          (raise 'obj)))))

;; SEGV
;;(my-assert-error  "guard handler invalid form #6"
;;                  (lambda ()
;;                    (guard (var
;;                            (#t =>))
;;                      (raise 'obj))))

;; SEGV
;;(my-assert-error  "guard handler invalid form #7"
;;                  (lambda ()
;;                    (guard (var
;;                            (#t =>)
;;                            (else #t))
;;                      (raise 'obj))))

;; SEGV
;;(my-assert-error  "guard handler invalid form #8"
;;                  (lambda ()
;;                    (guard (var
;;                            (else =>))
;;                      (raise 'obj))))

;; FAILED
;; not a procedure
(my-assert-error  "guard handler invalid form #9"
                  (lambda ()
                    (guard (var
                            (#t => #t))
                      (raise 'obj))))

;; FAILED
;; not a procedure but #f
(my-assert-error  "guard handler invalid form #10"
                  (lambda ()
                    (guard (var
                            (#t => #f))
                      (raise 'obj))))

;; FAILED
;; procedure but argument number mismatch
(my-assert-error  "guard handler invalid form #11"
                  (lambda ()
                    (guard (var
                            (#t => eq?))
                      (raise 'obj))))

;; FAILED
;; not a procedure but a syntax
(my-assert-error  "guard handler invalid form #12"
                  (lambda ()
                    (guard (var
                            (#t => delay))
                      (raise 'obj))))

(assert-equal?  "guard handler reraise #1"
                'reraised
                (guard (var
                        ((eq? var 'error)
                         'reraised))
                  (guard (var
                          (#f))
                    (raise 'error))))
(assert-equal?  "guard handler reraise #2"
                'reraised
                (guard (var
                        ((eq? var 'error)
                         'reraised))
                  (guard (var
                          ((even? 3) #f)
                          ((positive? -1) #f))
                    (raise 'error))))

;; R5RS: If the selected <clause> contains only the <test> and no
;; <expression>s, then the value of the <test> is returned as the result.
(assert-equal?  "guard handler tested value as result #1"
                #t
                (guard (var
                        (#t))
                  (raise 'error)))
(assert-equal?  "guard handler tested value as result #2"
                3
                (guard (var
                        (#f)
                        (3))
                  (raise 'error)))
(assert-equal?  "guard handler tested value as result #3"
                3
                (guard(var
                       (#f)
                       (3)
                       (4))
                  (raise 'error)))

;;
;; mixed use of with-exception-handler and guard
;;

;; SEGV
;;(assert-equal? "mixed exception handling #1"
;;               'guard-ret
;;	       (with-exception-handler (lambda (x)
;;					 (k 'with-exception-ret))
;;                 (lambda ()
;;                   (guard (condition
;;                           (else
;;                            'guard-ret))
;;                     (raise 1)))))

(assert-equal? "mixed exception handling #2"
               'with-exception-ret
	       (with-exception-handler (lambda (x)
					 'with-exception-ret)
                 (lambda ()
                   (guard (condition
                           ((negative? condition)
                            'guard-ret))
                     (raise 1)))))

;; SEGV
;;(assert-equal? "mixed exception handling #3"
;;               'positive
;;               (call-with-current-continuation
;;                (lambda (k)
;;                  (with-exception-handler (lambda (x)
;;                                            (k 'zero))
;;                    (lambda ()
;;                      (guard (condition
;;                              ((positive? condition) 'positive)
;;                              ((negative? condition) 'negative))
;;                        (raise 1)))))))

;; SEGV
;;(assert-equal? "mixed exception handling #4"
;;               'negative
;;               (call-with-current-continuation
;;                (lambda (k)
;;                  (with-exception-handler (lambda (x)
;;                                            (k 'zero))
;;                    (lambda ()
;;                      (guard (condition
;;                              ((positive? condition) 'positive)
;;                              ((negative? condition) 'negative))
;;                        (raise -1)))))))

(assert-equal? "mixed exception handling #5"
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
