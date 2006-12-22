;;  Filename : test-srfi38.scm
;;  About    : unit test for SRFI-38
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

(if (not (provided? "srfi-38"))
    (test-skip "SRFI-38 is not enabled"))

(define tn test-name)

(use srfi-38)
(use srfi-6)

(tn "write/ss short-name alias")
(if sigscheme?
    (begin
      (assert-true   (tn) (symbol-bound? 'write/ss))
      (assert-true   (tn) (string-eval
                           "(eq? write/ss write-with-shared-structure)"))))

(tn "write/ss invalid form")
(assert-error  (tn) (lambda ()
                      (write-with-shared-structure)))
(assert-error  (tn) (lambda ()
                      (write-with-shared-structure #f . (current-output-port))))
(assert-error  (tn) (lambda ()
                      (write-with-shared-structure #f (current-output-port) . #t)))
(if sigscheme?
    (assert-error  (tn) (lambda ()
                          (write-with-shared-structure
                           #f (current-output-port) #t . #t))))

(tn "write/ss with implicit port")
(print-expected "\"abc\"")
(write-with-shared-structure "abc")
(newline)

(tn "write/ss with explicit port arg")
(let* ((outs (open-output-string))
       (s "abc")
       (convolution `(,s 1 #(,s b) ,(list 2) () ,s)))
  ; go crazy with mutators
  (set-car! (cdr convolution) convolution)
  (vector-set! (caddr convolution) 1 (cddr convolution))
  (set-cdr! (cadddr convolution) (cdr convolution))
  (write-with-shared-structure convolution outs)
  (assert-equal? (tn)
                 "#1=(#2=\"abc\" . #3=(#1# . #4=(#(#2# #4#) (2 . #3#) () #2#)))"
                 (get-output-string outs)))

(let* ((outs (open-output-string))
       (a-pair (cons 'kar 'kdr))
            (convolution (eval (list 'lambda () a-pair) (scheme-report-environment 5))))
       (set-cdr! a-pair convolution)
       (write-with-shared-structure convolution outs)
       (assert-equal? (tn)
                      "#1=#<closure (() (kar . #1#))>"
                      (get-output-string outs)))

(tn "write/ss with explicit port arg and optarg")
(let ((p (open-output-string)))
  (write-with-shared-structure "abc" p #t)
  (assert-equal? (tn) "\"abc\"" (get-output-string p)))
(if sigscheme?
    (let ((p (open-output-string)))
      (write-with-shared-structure "abc" p #t #t)  ;; accepts 2+ optarg
      (assert-equal? (tn) "\"abc\"" (get-output-string p))))

(total-report)
