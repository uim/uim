;;  FileName : test-srfi8.scm
;;  About    : unit test for SRFI-8
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

(use srfi-8)

(receive (a b c)
	 (values #f #t '())
	 (assert-equal? "receive test 1" #f a)
	 (assert-equal? "receive test 2" #t b)
	 (assert-equal? "receive test 3" '() c))

(assert-equal? "receive test4"
               5
	       (receive (a b) (values 4 5)
                 b))

(assert-true "receive test5"
	     (receive args (values)
		      (null? args)))

(assert-true "receive test6"
	     (receive () (values)
		      #t))

(define var 'global)
(receive (a b c var)
	 (values 'a 6 var 'local)
	 (assert-equal? "receive test 7" 'a a)
	 (assert-equal? "receive test 8" 6 b)
	 (assert-equal? "receive test 9" 'global c)
	 (assert-equal? "receive test 10" 'local var))

(tn "receive symbol formals (variadic_0)")
(assert-equal? (tn) '()        (receive args (values)       args))
(assert-equal? (tn) '(0)       (receive args 0              args))
(assert-equal? (tn) '(0)       (receive args (values 0)     args))
(assert-equal? (tn) '(0)       (receive args (values 0)     args))
(assert-equal? (tn) '(0 1)     (receive args (values 0 1)   args))
(assert-equal? (tn) '(0 1 2)   (receive args (values 0 1 2) args))

(tn "receive dotted formals variadic_1")
(assert-error  (tn) (lambda () (receive (x . rest) (values)    (list x rest))))
(assert-equal? (tn) '(0 ())    (receive (x . rest) 0           (list x rest)))
(assert-equal? (tn) '(0 ())    (receive (x . rest) (values 0)  (list x rest)))
(assert-equal? (tn) '(0 ())    (receive (x . rest) (values 0)  (list x rest)))
(assert-equal? (tn) '(0 (1))   (receive (x . rest) (values 0 1) (list x rest)))
(assert-equal? (tn) '(0 (1 2)) (receive (x . rest) (values 0 1 2)
                                 (list x rest)))

(tn "receive dotted formals variadic_2")
(assert-error  (tn) (lambda ()
               (receive (x y . rest) (values)         (list x y rest))))
(assert-error  (tn) (lambda ()
               (receive (x y . rest) 0                (list x y rest))))
(assert-error  (tn) (lambda ()
               (receive (x y . rest) (values 0)       (list x y rest))))
(assert-error  (tn) (lambda ()
               (receive (x y . rest) (values 0)       (list x y rest))))
(assert-equal? (tn) '(0 1 ())
               (receive (x y . rest) (values 0 1)     (list x y rest)))
(assert-equal? (tn) '(0 1 (2))
               (receive (x y . rest) (values 0 1 2)   (list x y rest)))
(assert-equal? (tn) '(0 1 (2 3))
               (receive (x y . rest) (values 0 1 2 3) (list x y rest)))

(if (and (provided? "sigscheme")
         (provided? "strict-argcheck"))
    (begin
      (tn "receive invalid formals: boolean as an arg")
      (assert-error (tn) (lambda () (receive (#t) #t #t)))
      (assert-error (tn) (lambda () (receive (x #t) #t #t)))
      (assert-error (tn) (lambda () (receive (#t x) #t #t)))
      (assert-error (tn) (lambda () (receive (x . #t) #t #t)))
      (assert-error (tn) (lambda () (receive (#t . x) #t #t)))
      (assert-error (tn) (lambda () (receive (x y #t) #t #t)))
      (assert-error (tn) (lambda () (receive (x y . #t) #t #t)))
      (assert-error (tn) (lambda () (receive (x #t y) #t #t)))
      (assert-error (tn) (lambda () (receive (x #t . y) #t #t)))
      (tn "receive invalid formals: intger as an arg")
      (assert-error (tn) (lambda () (receive (1) #t #t)))
      (assert-error (tn) (lambda () (receive (x 1) #t #t)))
      (assert-error (tn) (lambda () (receive (1 x) #t #t)))
      (assert-error (tn) (lambda () (receive (x . 1) #t #t)))
      (assert-error (tn) (lambda () (receive (1 . x) #t #t)))
      (assert-error (tn) (lambda () (receive (x y 1) #t #t)))
      (assert-error (tn) (lambda () (receive (x y . 1) #t #t)))
      (assert-error (tn) (lambda () (receive (x 1 y) #t #t)))
      (assert-error (tn) (lambda () (receive (x 1 . y) #t #t)))
      (tn "receive invalid formals: null as an arg")
      (assert-error (tn) (lambda () (receive (()) #t #t)))
      (assert-error (tn) (lambda () (receive (x ()) #t #t)))
      (assert-error (tn) (lambda () (receive (() x) #t #t)))
      (assert-true  (tn)            (receive (x . ()) #t x))
      (assert-error (tn) (lambda () (receive (() . x) #t #t)))
      (assert-error (tn) (lambda () (receive (x y ()) #t #t)))
      (assert-error (tn) (lambda () (receive (x y . ()) #t x)))
      (assert-error (tn) (lambda () (receive (x () y) #t #t)))
      (assert-error (tn) (lambda () (receive (x () . y) #t #t)))
      (tn "receive invalid formals: pair as an arg")
      (assert-error (tn) (lambda () (receive ((a)) #t #t)))
      (assert-error (tn) (lambda () (receive (x (a)) #t #t)))
      (assert-error (tn) (lambda () (receive ((a) x) #t #t)))
      (assert-error (tn) (lambda () (receive (x . (a)) #t x)))
      (assert-error (tn) (lambda () (receive ((a) . x) #t #t)))
      (assert-error (tn) (lambda () (receive (x y (a)) #t #t)))
      (assert-true  (tn) (lambda () (receive (x y . (a)) #t x)))
      (assert-error (tn) (lambda () (receive (x (a) y) #t #t)))
      (assert-error (tn) (lambda () (receive (x (a) . y) #t #t)))
      (tn "receive invalid formals: char as an arg")
      (assert-error (tn) (lambda () (receive (#\a) #t #t)))
      (assert-error (tn) (lambda () (receive (x #\a) #t #t)))
      (assert-error (tn) (lambda () (receive (#\a x) #t #t)))
      (assert-error (tn) (lambda () (receive (x . #\a) #t #t)))
      (assert-error (tn) (lambda () (receive (#\a . x) #t #t)))
      (assert-error (tn) (lambda () (receive (x y #\a) #t #t)))
      (assert-error (tn) (lambda () (receive (x y . #\a) #t #t)))
      (assert-error (tn) (lambda () (receive (x #\a y) #t #t)))
      (assert-error (tn) (lambda () (receive (x #\a . y) #t #t)))
      (tn "receive invalid formals: string as an arg")
      (assert-error (tn) (lambda () (receive ("a") #t #t)))
      (assert-error (tn) (lambda () (receive (x "a") #t #t)))
      (assert-error (tn) (lambda () (receive ("a" x) #t #t)))
      (assert-error (tn) (lambda () (receive (x . "a") #t #t)))
      (assert-error (tn) (lambda () (receive ("a" . x) #t #t)))
      (assert-error (tn) (lambda () (receive (x y "a") #t #t)))
      (assert-error (tn) (lambda () (receive (x y . "a") #t #t)))
      (assert-error (tn) (lambda () (receive (x "a" y) #t #t)))
      (assert-error (tn) (lambda () (receive (x "a" . y) #t #t)))
      (tn "receive invalid formals: vector as an arg")
      (assert-error (tn) (lambda () (receive (#(a)) #t #t)))
      (assert-error (tn) (lambda () (receive (x #(a)) #t #t)))
      (assert-error (tn) (lambda () (receive (#(a) x) #t #t)))
      (assert-error (tn) (lambda () (receive (x . #(a)) #t #t)))
      (assert-error (tn) (lambda () (receive (#(a) . x) #t #t)))
      (assert-error (tn) (lambda () (receive (x y #(a)) #t #t)))
      (assert-error (tn) (lambda () (receive (x y . #(a)) #t #t)))
      (assert-error (tn) (lambda () (receive (x #(a) y) #t #t)))
      (assert-error (tn) (lambda () (receive (x #(a) . y) #t #t)))))

(total-report)
