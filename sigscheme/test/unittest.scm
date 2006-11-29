;;  Filename : unittest.scm
;;  About    : Simple unit test library
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

;;;; defining a syntax as value is invalid
;;(if (provided? "sigscheme")
;;    (eval '(begin
;;             (define cond-expand cond)
;;             (define sigscheme #t))
;;          (interaction-environment)))
;;
;;(cond-expand
;; (sigscheme
;;  (use srfi-34))
;; (else #t))

(define *test-track-progress* #f)  ;; for locationg SEGV point
(define *total-testsuites* 1)  ;; TODO: introduce test suites and defaults to 0
(define *total-testcases* 1)   ;; TODO: introduce testcase and defaults to 0
(define *total-tests* 1)       ;; TODO: introduce test group and defaults to 0
(define *total-failures*  0)
(define *total-assertions* 0)
(define *total-errors* 0) ;; TODO: recover unintended error and increment this
(define test-filename "unspecified")

(define total-report
  (lambda ()
    (let ((header (if (zero? *total-failures*)
                      "OK: "
                      "FAILED: "))
          (total-successes (- *total-assertions* *total-failures*)))
      (for-each display
                (list
                 header
                 *total-tests*      " tests, "
                 *total-assertions* " assertions, "
                 total-successes    " successes, "
                 *total-failures*   " failures, "
                 *total-errors*     " errors"))
      (newline))))

(define report-error
  (lambda (err-msg)
    (begin
      (display "failed: ")
      (display err-msg)
      (newline))))

(define report-inequality
  (lambda (expected actual)
    (display " expected: <")
    (write expected)
    (display ">")
    (newline)
    (display "   actual: <")
    (write actual)
    (display ">")
    (newline)))

(define assert
  (let ((+ +))  ;; protect from redefinition
    (lambda (test-name err-msg exp)
      (set! *total-assertions* (+ *total-assertions* 1))
      (if *test-track-progress*
          (begin
            (display "done: ")
            (display test-name)
            (newline)))
      (if exp
          #t
          (begin
            (set! *total-failures* (+ *total-failures* 1))
            (report-error err-msg)
            #f)))))

(define test-skip
  (lambda (reason)
    (display "SKIP: ")
    (display reason)
    (newline)
    (exit 0)))

;;
;; assertions for test writers
;;

(define assert-fail
  (lambda (test-name err-msg)
    (assert test-name err-msg #f)))

(define assert-true
  (lambda (test-name exp)
    (assert test-name test-name exp)))

(define assert-false
  (lambda (test-name exp)
    (assert test-name test-name (not exp))))

(define assert-eq?
  (lambda (test-name expected actual)
    (or (assert test-name test-name (eq? expected actual))
        (report-inequality expected actual))))

(define assert-equal?
  (lambda (test-name expected actual)
    (or (assert test-name test-name (equal? expected actual))
        (report-inequality expected actual))))

(define assert-error
  (lambda (test-name proc)
    (or (procedure? proc)
        (error "assert-error: procedure required but got" proc))
    (let ((errored (guard (err
                           (else
                            #t))
                     (proc)
                     #f))
          (err-msg (string-append "no error has occurred in test "
                                  test-name)))
      (assert test-name err-msg errored))))

(define assert-parse-error
  (lambda (test-name str)
    (assert-error test-name (lambda ()
                              (string-read str)))))

(define assert-parseable
  (lambda (test-name str)
    (assert-true test-name (guard (err
                                   (else
                                    #f))
                             (lambda ()
                               (string-read str)
                               #t)))))

;;
;; misc
;;

;; SigScheme and Gauche surely returns #<undef>
(define undef
  (lambda ()
    (for-each values '())))

;; SigScheme and Gauche surely returns #<eof>
(define eof
  (lambda ()
    (string-read "")))

(define obj->literal
  (lambda (obj)
    (use srfi-6)
    (let ((port (open-output-string)))
      (write obj port)
      (get-output-string port))))

(define string-read
  (lambda (str)
    (use srfi-6)
    (let ((port (open-input-string str)))
      (read port))))

(define string-eval
  (lambda (str)
    (eval (string-read str)
          (interaction-environment))))

(define test-name
  (let ((name "anonymous test")
        (serial 0)
        (+ +))  ;; protect from redefinition
    (lambda args
      (if (null? args)
          (begin
            (set! serial (+ serial 1))
            (string-append name " #" (number->string serial)))
          (begin
            (set! name (car args))
            (set! serial 0)
            #f)))))

;;
;; implementation information
;;

(define sigscheme? (provided? "sigscheme"))

(define fixnum-bits (and (symbol-bound? 'fixnum-width)
                         (fixnum-width)))
