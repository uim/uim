;;  Filename : test-srfi34-2.scm
;;  About    : unit test for SRFI-34 taken from "Examples" section of SRFI-34
;;
;;  Copyright (C) 2005-2006 YAMAMOTO Kengo <yamaken AT bp.iij4u.or.jp>
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

;; All tests in this file are passed against r2143 (new repository)

;;(set! *test-track-progress* #t)

;; these tests are ported from "Examples" section of SRFI-34

(define print-expected
  (lambda (expected)
    (display " expected print: ")
    (display expected)
    (newline)
    (display "   actual print: ")))

;;PRINTS: condition: an-error
(print-expected "condition: an-error")
(assert-equal? "Examples of SRFI-34 document #1"
               'exception
               (call-with-current-continuation
                (lambda (k)
                  (with-exception-handler (lambda (x)
                                            (display "condition: ")
                                            (write x)
                                            (newline)
                                            (k 'exception))
                    (lambda ()
                      (+ 1 (raise 'an-error)))))))

;;PRINTS: something went wrong
;; Then behaves in an unspecified way. Although the behavior when a handler
;; returned is not specified in SRFI-34, SigScheme should produce an error to
;; prevent being misused by users.
(print-expected "something went wrong")
(assert-error "Examples of SRFI-34 document #2"
              (lambda ()
                (call-with-current-continuation
                 (lambda (k)
                   (with-exception-handler (lambda (x)
                                             (display "something went wrong")
                                             (newline)
                                             'dont-care)
                     (lambda ()
                       (+ 1 (raise 'an-error))))))))

;;PRINTS: condition: an-error
(print-expected "condition: an-error")
(assert-equal? "Examples of SRFI-34 document #3"
               'exception
               (guard (condition
                       (else
                        (display "condition: ")
                        (write condition)
                        (newline)
                        'exception))
                 (+ 1 (raise 'an-error))))

;;PRINTS: something went wrong
(print-expected "something went wrong")
(assert-equal? "Examples of SRFI-34 document #4"
               'dont-care
               (guard (condition
                       (else
                        (display "something went wrong")
                        (newline)
                        'dont-care))
                 (+ 1 (raise 'an-error))))

(assert-equal? "Examples of SRFI-34 document #5"
               'positive
               (call-with-current-continuation
                (lambda (k)
                  (with-exception-handler (lambda (x)
                                            (display "reraised ") (write x) (newline)
                                            (k 'zero))
                    (lambda ()
                      (guard (condition
                              ((positive? condition) 'positive)
                              ((negative? condition) 'negative))
                        (raise 1)))))))

(assert-equal? "Examples of SRFI-34 document #6"
               'negative
               (call-with-current-continuation
                (lambda (k)
                  (with-exception-handler (lambda (x)
                                            (display "reraised ") (write x) (newline)
                                            (k 'zero))
                    (lambda ()
                      (guard (condition
                              ((positive? condition) 'positive)
                              ((negative? condition) 'negative))
                        (raise -1)))))))

;;PRINTS: reraised 0
(print-expected "reraised 0")
(assert-equal? "Examples of SRFI-34 document #7"
               'zero
               (call-with-current-continuation
                (lambda (k)
                  (with-exception-handler (lambda (x)
                                            (display "reraised ") (write x) (newline)
                                            (k 'zero))
                    (lambda ()
                      (guard (condition
                              ((positive? condition) 'positive)
                              ((negative? condition) 'negative))
                        (raise 0)))))))

(assert-equal? "Examples of SRFI-34 document #8"
               42
               (guard (condition
                       ((assq 'a condition) => cdr)
                       ((assq 'b condition)))
                 (raise (list (cons 'a 42)))))

(assert-equal? "Examples of SRFI-34 document #9"
               '(b . 23)
               (guard (condition
                       ((assq 'a condition) => cdr)
                       ((assq 'b condition)))
                 (raise (list (cons 'b 23)))))

(total-report)
