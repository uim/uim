;;  Filename : test-exp.scm
;;  About    : unit test for R5RS expressions
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

(use srfi-6)


;; open-input-string
;;;; immutable
(define p
  (open-input-string "(a . (b . (c . ()))) 34"))

(assert-true   "open-input-string immutable" (input-port? p))
(assert-equal? "open-input-string immutable" '(a b c) (read p))
(assert-equal? "open-input-string immutable" 34 (read p))
;;;; mutable
(define p2
  (open-input-string (string-copy "(a . (b . (c . ()))) 34")))

(assert-true   "open-input-string mutable" (input-port? p2))
(assert-equal? "open-input-string mutable" '(a b c) (read p2))
(assert-equal? "open-input-string mutable" 34 (read p2))

;; open-output-string and get-output-string
(assert-equal? "output string test 1" "a(b c)" (let ((q (open-output-string))
						     (x '(a b c)))
						 (write (car x) q)
						 (write (cdr x) q)
						 (get-output-string q)))
(assert-equal? "output string test 2" "" (get-output-string
                                          (open-output-string)))

(total-report)
