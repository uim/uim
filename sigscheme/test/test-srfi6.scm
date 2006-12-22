;;  Filename : test-srfi6.scm
;;  About    : unit test for SRFI-6 Basic String Ports
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

(if (not (provided? "srfi-6"))
    (test-skip "SRFI-6 is not enabled"))

(use srfi-6)

(define tn test-name)

;;
;; open-input-string
;;

(tn "open-input-string invalid forms")
(assert-error  (tn) (lambda () (open-input-string)))
(assert-error  (tn) (lambda () (open-input-string '())))
(assert-error  (tn) (lambda () (open-input-string (current-input-port))))
(assert-error  (tn) (lambda () (open-input-string "" "")))

;; immutable
(define p
  (open-input-string "(a . (b . (c . ()))) 34"))

(tn "open-input-string immutable")
(assert-true   (tn) (input-port? p))
(assert-equal? (tn) '(a b c) (read p))
(assert-equal? (tn) 34 (read p))
(assert-true   (tn) (eof-object? (read p)))
(assert-true   (tn) (eof-object? (read-char (open-input-string ""))))

;; mutable
(define p2
  (open-input-string (string-copy "(a . (b . (c . ()))) 34")))

(tn "open-input-string mutable")
(assert-true   (tn) (input-port? p2))
(assert-equal? (tn) '(a b c) (read p2))
(assert-equal? (tn) 34 (read p2))
(assert-true   (tn) (eof-object? (read p2)))
(assert-true   (tn) (eof-object? (read-char
                                  (open-input-string (string-copy "")))))

;;
;; open-output-string and get-output-string
;;

(tn "open-output-string invalid forms")
(assert-error  (tn) (lambda () (open-output-string '())))
(assert-error  (tn) (lambda () (open-output-string (current-input-port))))
(assert-error  (tn) (lambda () (open-output-string "")))

(tn "get-output-string invalid forms")
(assert-error  (tn) (lambda () (get-output-string)))
(assert-error  (tn) (lambda () (get-output-string (current-output-port))))

(tn "output string")
(assert-equal? (tn)
               "a(b c)"
               (let ((q (open-output-string))
                     (x '(a b c)))
                 (write (car x) q)
                 (write (cdr x) q)
                 (get-output-string q)))
(assert-equal? (tn)
               "aB"
               (let ((q (open-output-string)))
                 (write-char #\a q)
                 (write-char #\B q)
                 (get-output-string q)))
(assert-equal? (tn)
               ""
               (get-output-string (open-output-string)))

(total-report)
