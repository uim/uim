;;  Filename : test-misc.scm
;;  About    : unit tests for miscellaneous procedures
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

(tn "procedure?")
(assert-false  (tn) (procedure? #f))
(assert-false  (tn) (procedure? #t))
(assert-false  (tn) (procedure? '()))
(if (provided? "sigscheme")
    (begin
      (assert-false  (tn) (procedure? (eof)))
      (assert-false  (tn) (procedure? (undef)))))
(assert-false  (tn) (procedure? 0))
(assert-false  (tn) (procedure? 1))
(assert-false  (tn) (procedure? 3))
(assert-false  (tn) (procedure? -1))
(assert-false  (tn) (procedure? -3))
(assert-false  (tn) (procedure? 'symbol))
(assert-false  (tn) (procedure? 'SYMBOL))
(assert-false  (tn) (procedure? #\a))
(assert-false  (tn) (procedure? #\あ))
(assert-false  (tn) (procedure? ""))
(assert-false  (tn) (procedure? " "))
(assert-false  (tn) (procedure? "a"))
(assert-false  (tn) (procedure? "A"))
(assert-false  (tn) (procedure? "aBc12!"))
(assert-false  (tn) (procedure? "あ"))
(assert-false  (tn) (procedure? "あ0イう12!"))
(assert-true   (tn) (procedure? car))
(assert-false  (tn) (procedure? 'car))
(assert-true   (tn) (procedure? +))
(assert-true   (tn) (procedure? (lambda () #t)))
(assert-false  (tn) (procedure? '(lambda () #t)))

;; syntactic keywords should not be appeared as operand

(call-with-current-continuation
 (lambda (k)
   (assert-true   (tn) (procedure? k))))
(assert-true   (tn) (call-with-current-continuation procedure?))
(assert-false  (tn) (procedure? (current-output-port)))
(assert-false  (tn) (procedure? '(#t . #t)))
(assert-false  (tn) (procedure? (cons #t #t)))
(assert-false  (tn) (procedure? '(0 1 2)))
(assert-false  (tn) (procedure? (list 0 1 2)))
(assert-false  (tn) (procedure? '#()))
(assert-false  (tn) (procedure? (vector)))
(assert-false  (tn) (procedure? '#(0 1 2)))
(assert-false  (tn) (procedure? (vector 0 1 2)))


(total-report)
