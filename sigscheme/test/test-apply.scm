;;  Filename : test-apply.scm
;;  About    : unit test for R5RS apply
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

;; check apply
(assert-equal? "apply check1" #t (apply = '(1 1 1)))
(assert-equal? "apply check2" 6  (apply + `(1 2 ,(+ 1 2))))
(assert-equal? "apply check3" '(3) (apply cddr '((1 2 3))))
(assert-equal? "apply check4" #t (apply equal? '((1 2) (1 2))))
(assert-equal? "apply check5" "iu" (apply substring '("aiueo" 1 3)))

(assert-equal? "apply check6" 4  (apply (lambda (x y) (+ x y)) '(1 3)))
(assert-equal? "apply check7" 4  (apply (lambda (x y) (+ x y)) '(1 3)))
(assert-equal? "apply check8" '(1 2 3) (apply (lambda x x) '(1 2 3)))
(assert-equal? "apply check9" 1 (apply (lambda (x) x) '(1)))
(assert-equal? "apply check10" '(1) (apply (lambda x x) '(1)))

(assert-equal? "apply check11" 2 (apply (lambda x x 2) '(1)))

(assert-equal? "apply check12" '() (apply (lambda (a . b) b) '(1)))
(assert-equal? "apply check13" '(2) (apply (lambda (a . b) b) '(1 2)))
(assert-equal? "apply check13" '() (apply (lambda (a b . c) c) '(1 2)))

(define (dotarg-2 x . y)
  (+ x (car y)))

(assert-equal? "sequence dot-arg func apply check" 4 (apply dotarg-2 '(1 3)))
(assert-equal? "sequence dot-arg func apply check" 4 (apply dotarg-2 '(1 3)))

(define compose
  (lambda (f g)
    (lambda args
      (f (apply g args)))))
(assert-equal? "apply check5" "100" ((compose number->string *) 4 25))

(total-report)
