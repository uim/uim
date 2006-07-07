;;  Filename : test-lambda.scm
;;  About    : unit test for R5RS lambda
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

(define *test-track-progress* #f)
(define tn test-name)


(if (and (provided? "sigscheme")
         (provided? "strict-argcheck"))
    (begin
      (tn "lambda invalid formals: boolean as an arg")
      (assert-error (tn) (lambda () (lambda (#t) #t)))
      (assert-error (tn) (lambda () (lambda (x #t) #t)))
      (assert-error (tn) (lambda () (lambda (#t x) #t)))
      (assert-error (tn) (lambda () (lambda (x . #t) #t)))
      (assert-error (tn) (lambda () (lambda (#t . x) #t)))
      (assert-error (tn) (lambda () (lambda (x y #t) #t)))
      (assert-error (tn) (lambda () (lambda (x y . #t) #t)))
      (assert-error (tn) (lambda () (lambda (x #t y) #t)))
      (assert-error (tn) (lambda () (lambda (x #t . y) #t)))
      (tn "lambda invalid formals: intger as an arg")
      (assert-error (tn) (lambda () (lambda (1) #t)))
      (assert-error (tn) (lambda () (lambda (x 1) #t)))
      (assert-error (tn) (lambda () (lambda (1 x) #t)))
      (assert-error (tn) (lambda () (lambda (x . 1) #t)))
      (assert-error (tn) (lambda () (lambda (1 . x) #t)))
      (assert-error (tn) (lambda () (lambda (x y 1) #t)))
      (assert-error (tn) (lambda () (lambda (x y . 1) #t)))
      (assert-error (tn) (lambda () (lambda (x 1 y) #t)))
      (assert-error (tn) (lambda () (lambda (x 1 . y) #t)))
      (tn "lambda invalid formals: null as an arg")
      (assert-error (tn) (lambda () (lambda (()) #t)))
      (assert-error (tn) (lambda () (lambda (x ()) #t)))
      (assert-error (tn) (lambda () (lambda (() x) #t)))
      (assert-true  (tn)            (lambda (x . ()) #t))
      (assert-error (tn) (lambda () (lambda (() . x) #t)))
      (assert-error (tn) (lambda () (lambda (x y ()) #t)))
      (assert-true  (tn)            (lambda (x y . ()) #t))
      (assert-error (tn) (lambda () (lambda (x () y) #t)))
      (assert-error (tn) (lambda () (lambda (x () . y) #t)))
      (tn "lambda invalid formals: pair as an arg")
      (assert-error (tn) (lambda () (lambda ((a)) #t)))
      (assert-error (tn) (lambda () (lambda (x (a)) #t)))
      (assert-error (tn) (lambda () (lambda ((a) x) #t)))
      (assert-true  (tn)            (lambda (x . (a)) #t))
      (assert-error (tn) (lambda () (lambda ((a) . x) #t)))
      (assert-error (tn) (lambda () (lambda (x y (a)) #t)))
      (assert-true  (tn)            (lambda (x y . (a)) #t))
      (assert-error (tn) (lambda () (lambda (x (a) y) #t)))
      (assert-error (tn) (lambda () (lambda (x (a) . y) #t)))
      (tn "lambda invalid formals: char as an arg")
      (assert-error (tn) (lambda () (lambda (#\a) #t)))
      (assert-error (tn) (lambda () (lambda (x #\a) #t)))
      (assert-error (tn) (lambda () (lambda (#\a x) #t)))
      (assert-error (tn) (lambda () (lambda (x . #\a) #t)))
      (assert-error (tn) (lambda () (lambda (#\a . x) #t)))
      (assert-error (tn) (lambda () (lambda (x y #\a) #t)))
      (assert-error (tn) (lambda () (lambda (x y . #\a) #t)))
      (assert-error (tn) (lambda () (lambda (x #\a y) #t)))
      (assert-error (tn) (lambda () (lambda (x #\a . y) #t)))
      (tn "lambda invalid formals: string as an arg")
      (assert-error (tn) (lambda () (lambda ("a") #t)))
      (assert-error (tn) (lambda () (lambda (x "a") #t)))
      (assert-error (tn) (lambda () (lambda ("a" x) #t)))
      (assert-error (tn) (lambda () (lambda (x . "a") #t)))
      (assert-error (tn) (lambda () (lambda ("a" . x) #t)))
      (assert-error (tn) (lambda () (lambda (x y "a") #t)))
      (assert-error (tn) (lambda () (lambda (x y . "a") #t)))
      (assert-error (tn) (lambda () (lambda (x "a" y) #t)))
      (assert-error (tn) (lambda () (lambda (x "a" . y) #t)))
      (tn "lambda invalid formals: vector as an arg")
      (assert-error (tn) (lambda () (lambda (#(a)) #t)))
      (assert-error (tn) (lambda () (lambda (x #(a)) #t)))
      (assert-error (tn) (lambda () (lambda (#(a) x) #t)))
      (assert-error (tn) (lambda () (lambda (x . #(a)) #t)))
      (assert-error (tn) (lambda () (lambda (#(a) . x) #t)))
      (assert-error (tn) (lambda () (lambda (x y #(a)) #t)))
      (assert-error (tn) (lambda () (lambda (x y . #(a)) #t)))
      (assert-error (tn) (lambda () (lambda (x #(a) y) #t)))
      (assert-error (tn) (lambda () (lambda (x #(a) . y) #t)))))

(tn "lambda")
;; empty args
(assert-equal? (tn)
               'result
               ((lambda () 'result)))
(assert-error  (tn)
               (lambda ()
                 ((lambda () 'result) 0)))
;; variable args
(assert-equal? (tn)
               '()
               ((lambda args args)))
(assert-equal? (tn)
               '(2)
               ((lambda args args) (+ 1 1)))
(assert-equal? (tn)
               '(2 3)
               ((lambda args args) (+ 1 1) (+ 2 1)))
(assert-equal? (tn)
               '(2 3 4)
               ((lambda args args) (+ 1 1) (+ 2 1) (+ 3 1)))
;; 1 arg + rest
(assert-error  (tn)
               (lambda ()
                 ((lambda (var1 . rest)
                    (list var1 rest)))))
(assert-equal? (tn)
               '(2 ())
               ((lambda (var1 . rest)
                  (list var1 rest))
                (+ 1 1)))
(assert-equal? (tn)
               '(2 (3))
               ((lambda (var1 . rest)
                  (list var1 rest))
                (+ 1 1) (+ 2 1)))
(assert-equal? (tn)
               '(2 (3 4))
               ((lambda (var1 . rest)
                  (list var1 rest))
                (+ 1 1) (+ 2 1) (+ 3 1)))
;; 2 arg + rest
(assert-error  (tn)
               (lambda ()
                 ((lambda (var1 var2 . rest)
                    (list var1 var2 rest)))))
(assert-error  (tn)
               (lambda ()
                 ((lambda (var1 var2 . rest)
                    (list var1 var2 rest))
                  (+ 1 1))))
(assert-equal? (tn)
               '(2 3 ())
               ((lambda (var1 var2 . rest)
                  (list var1 var2 rest))
                (+ 1 1) (+ 2 1)))
(assert-equal? (tn)
               '(2 3 (4))
               ((lambda (var1 var2 . rest)
                  (list var1 var2 rest))
                (+ 1 1) (+ 2 1) (+ 3 1)))
;; duplicate variable name: SigScheme does not perform the check
;;(assert-error  (tn)
;;               (lambda ()
;;                 ((lambda (var1 var2) 'result) 0 1)))
;; masked variable name
(assert-equal? (tn)
               '(4 5 3)
               ((lambda (var1 var2 var3)
                  ((lambda (var1 var2)
                     (list var1 var2 var3))
                   4 5))
                1 2 3))
(assert-equal? (tn)
               '(1 2 3)
               ((lambda (var1 var2 var3)
                  ((lambda (var1 var2)
                     'dymmy)
                   4 5)
                  (list var1 var2 var3))
                1 2 3))
(assert-equal? (tn)
               '(1 2 9)
               ((lambda (var1 var2 var3)
                  ((lambda (var1 var2)
                     (set! var3 (+ var1 var2)))
                   4 5)
                  (list var1 var2 var3))
                1 2 3))
(assert-equal? (tn)
               '(1 2 30)
               ((lambda (var1 var2 var3)
                  ((lambda (var1 var2)
                     (set! var1 10)
                     (set! var2 20)
                     (set! var3 (+ var1 var2)))
                   4 5)
                  (list var1 var2 var3))
                1 2 3))

(tn "lambda lexical scope")
(define count-let
  ((lambda (count-let)  ;; intentionally same name
     (lambda ()
       (set! count-let (+ count-let 1))
       count-let))
   0))
(assert-true   (tn) (procedure? count-let))
(assert-equal? (tn) 1 (count-let))
(assert-equal? (tn) 2 (count-let))
(assert-equal? (tn) 3 (count-let))

(tn "let internal definitions lacking sequence part")
;; at least one <expression> is required
(assert-error  (tn)
               (lambda ()
                 ((lambda ()
                   (define var1 1)))))
(assert-error  (tn)
               (lambda ()
                 ((lambda ()
                   (define (proc1) 1)))))
(assert-error  (tn)
               (lambda ()
                 ((lambda ()
                   (define var1 1)
                   (define var2 2)))))
(assert-error  (tn)
               (lambda ()
                 ((lambda ()
                   (define (proc1) 1)
                   (define (proc2) 2)))))
(assert-error  (tn)
               (lambda ()
                 ((lambda ()
                   (define var1 1)
                   (define (proc2) 2)))))
(assert-error  (tn)
               (lambda ()
                 ((lambda ()
                   (define (proc1) 1)
                   (define var2 2)))))
(assert-error  (tn)
               (lambda ()
                 ((lambda ()
                   (begin)))))
(assert-error  (tn)
               (lambda ()
                 ((lambda ()
                   (begin
                     (define var1 1))))))
(assert-error  (tn)
               (lambda ()
                 ((lambda ()
                   (begin
                     (define (proc1) 1))))))
(assert-error  (tn)
               (lambda ()
                 ((lambda ()
                   (begin
                     (define var1 1)
                     (define var2 2))))))
(assert-error  (tn)
               (lambda ()
                 ((lambda ()
                   (begin
                     (define (proc1) 1)
                     (define (proc2) 2))))))
(assert-error  (tn)
               (lambda ()
                 ((lambda ()
                   (begin
                     (define var1 1)
                     (define (proc2) 2))))))
(assert-error  (tn)
               (lambda ()
                 ((lambda ()
                   (begin
                     (define (proc1) 1)
                     (define var2 2))))))
;; appending a non-definition expression into a begin block is invalid
(assert-error  (tn)
               (lambda ()
                 ((lambda ()
                   (begin
                     (define var1 1)
                     'val)))))
(assert-error  (tn)
               (lambda ()
                 ((lambda ()
                   (begin
                     (define (proc1) 1)
                     'val)))))
(assert-error  (tn)
               (lambda ()
                 ((lambda ()
                   (begin
                     (define var1 1)
                     (define var2 2)
                     'val)))))
(assert-error  (tn)
               (lambda ()
                 ((lambda ()
                   (begin
                     (define (proc1) 1)
                     (define (proc2) 2)
                     'val)))))
(assert-error  (tn)
               (lambda ()
                 ((lambda ()
                   (begin
                     (define var1 1)
                     (define (proc2) 2)
                     'val)))))
(assert-error  (tn)
               (lambda ()
                 ((lambda ()
                   (begin
                     (define (proc1) 1)
                     (define var2 2)
                     'val)))))

(tn "let internal definitions cross reference")
;; R5RS: 5.2.2 Internal definitions
;; Just as for the equivalent `letrec' expression, it must be possible to
;; evaluate each <expression> of every internal definition in a <body> without
;; assigning or referring to the value of any <variable> being defined.
(assert-error  (tn)
               (lambda ()
                 ((lambda ()
                   (define var1 1)
                   (define var2 var1)
                   'val))))
(assert-error  (tn)
               (lambda ()
                 ((lambda ()
                   (define var1 var2)
                   (define var2 2)
                   'val))))
(assert-error  (tn)
               (lambda ()
                 ((lambda ()
                   (define var1 var1)
                   'val))))
(assert-equal? (tn)
               '(0 0 0 0 0)
               ((lambda (var0)
                 (define var1 var0)
                 (define var2 var0)
                 (begin
                   (define var3 var0)
                   (begin
                     (define var4 var0)))
                 (define var5 var0)
                 (list var1 var2 var3 var4 var5)) 0))
(assert-equal? (tn)
               '(#f #f #f #f #f #f)
               ((lambda (var0)
                 (define var1 (symbol-bound? 'var1))
                 (define var2 (symbol-bound? 'var1))
                 (begin
                   (define var3 (symbol-bound? 'var1))
                   (begin
                     (define var4 (symbol-bound? 'var1))))
                 (define var5 (symbol-bound? 'var1))
                 (list var0 var1 var2 var3 var4 var5))
                (symbol-bound? 'var1)))
(assert-equal? (tn)
               '(#f #f #f #f #f #f)
               ((lambda (var0)
                 (define var1 (symbol-bound? 'var2))
                 (define var2 (symbol-bound? 'var2))
                 (begin
                   (define var3 (symbol-bound? 'var2))
                   (begin
                     (define var4 (symbol-bound? 'var2))))
                 (define var5 (symbol-bound? 'var2))
                 (list var0 var1 var2 var3 var4 var5))
                (symbol-bound? 'var2)))
(assert-equal? (tn)
               '(#f #f #f #f #f #f)
               ((lambda (var0)
                 (define var1 (symbol-bound? 'var3))
                 (define var2 (symbol-bound? 'var3))
                 (begin
                   (define var3 (symbol-bound? 'var3))
                   (begin
                     (define var4 (symbol-bound? 'var3))))
                 (define var5 (symbol-bound? 'var3))
                 (list var0 var1 var2 var3 var4 var5))
                (symbol-bound? 'var3)))
(assert-equal? (tn)
               '(#f #f #f #f #f #f)
               ((lambda (var0)
                 (define var1 (symbol-bound? 'var4))
                 (define var2 (symbol-bound? 'var4))
                 (begin
                   (define var3 (symbol-bound? 'var4))
                   (begin
                     (define var4 (symbol-bound? 'var4))))
                 (define var5 (symbol-bound? 'var4))
                 (list var0 var1 var2 var3 var4 var5))
                (symbol-bound? 'var4)))
(assert-equal? (tn)
               '(#f #f #f #f #f #f)
               ((lambda (var0)
                 (define var1 (symbol-bound? 'var5))
                 (define var2 (symbol-bound? 'var5))
                 (begin
                   (define var3 (symbol-bound? 'var5))
                   (begin
                     (define var4 (symbol-bound? 'var5))))
                 (define var5 (symbol-bound? 'var5))
                 (list var0 var1 var2 var3 var4 var5))
                (symbol-bound? 'var5)))
;; outer region cannot refer internal variable
(assert-error  (tn)
               (lambda ()
                 ((lambda (var0)
                    (define var1 (lambda () 1))
                    (eq? (var0) var0))
                  (lambda () var1))))
;; defining procedure can refer other (and self) variables as if letrec
(assert-equal? (tn)
               '(#t #t #t #t #t)
               ((lambda (var0)
                  (define var1 (lambda () var0))
                  (define var2 (lambda () var0))
                  (begin
                    (define var3 (lambda () var0))
                    (begin
                      (define var4 (lambda () var0))))
                  (define var5 (lambda () var0))
                  (list (eq? (var1) var0)
                        (eq? (var2) var0)
                        (eq? (var3) var0)
                        (eq? (var4) var0)
                        (eq? (var5) var0)))
                (lambda () 0)))
(assert-equal? (tn)
               '(#t #t #t #t #t)
               ((lambda ()
                 (define var1 (lambda () var1))
                 (define var2 (lambda () var1))
                 (begin
                   (define var3 (lambda () var1))
                   (begin
                     (define var4 (lambda () var1))))
                 (define var5 (lambda () var1))
                 (list (eq? (var1) var1)
                       (eq? (var2) var1)
                       (eq? (var3) var1)
                       (eq? (var4) var1)
                       (eq? (var5) var1)))))
(assert-equal? (tn)
               '(#t #t #t #t #t)
               ((lambda ()
                 (define var1 (lambda () var2))
                 (define var2 (lambda () var2))
                 (begin
                   (define var3 (lambda () var2))
                   (begin
                     (define var4 (lambda () var2))))
                 (define var5 (lambda () var2))
                 (list (eq? (var1) var2)
                       (eq? (var2) var2)
                       (eq? (var3) var2)
                       (eq? (var4) var2)
                       (eq? (var5) var2)))))
(assert-equal? (tn)
               '(#t #t #t #t #t)
               ((lambda ()
                 (define var1 (lambda () var3))
                 (define var2 (lambda () var3))
                 (begin
                   (define var3 (lambda () var3))
                   (begin
                     (define var4 (lambda () var3))))
                 (define var5 (lambda () var3))
                 (list (eq? (var1) var3)
                       (eq? (var2) var3)
                       (eq? (var3) var3)
                       (eq? (var4) var3)
                       (eq? (var5) var3)))))
(assert-equal? (tn)
               '(#t #t #t #t #t)
               ((lambda ()
                 (define var1 (lambda () var4))
                 (define var2 (lambda () var4))
                 (begin
                   (define var3 (lambda () var4))
                   (begin
                     (define var4 (lambda () var4))))
                 (define var5 (lambda () var4))
                 (list (eq? (var1) var4)
                       (eq? (var2) var4)
                       (eq? (var3) var4)
                       (eq? (var4) var4)
                       (eq? (var5) var4)))))
(assert-equal? (tn)
               '(#t #t #t #t #t)
               ((lambda ()
                 (define var1 (lambda () var5))
                 (define var2 (lambda () var5))
                 (begin
                   (define var3 (lambda () var5))
                   (begin
                     (define var4 (lambda () var5))))
                 (define var5 (lambda () var5))
                 (list (eq? (var1) var5)
                       (eq? (var2) var5)
                       (eq? (var3) var5)
                       (eq? (var4) var5)
                       (eq? (var5) var5)))))

(tn "let internal definitions valid forms")
;; valid internal definitions
(assert-equal? (tn)
               '(1)
               ((lambda ()
                 (define var1 1)
                 (list var1))))
(assert-equal? (tn)
               '(1)
               ((lambda ()
                 (define (proc1) 1)
                 (list (proc1)))))
(assert-equal? (tn)
               '(1 2)
               ((lambda ()
                 (define var1 1)
                 (define var2 2)
                 (list var1 var2))))
(assert-equal? (tn)
               '(1 2)
               ((lambda ()
                  (define (proc1) 1)
                  (define (proc2) 2)
                  (list (proc1) (proc2)))))
(assert-equal? (tn)
               '(1 2)
               ((lambda ()
                  (define var1 1)
                  (define (proc2) 2)
                  (list var1 (proc2)))))
(assert-equal? (tn)
               '(1 2)
               ((lambda ()
                  (define (proc1) 1)
                  (define var2 2)
                  (list (proc1) var2))))
;; SigScheme accepts '(begin)' as valid internal definition '(begin
;; <definition>*)' as defined in "7.1.6 Programs and definitions" of R5RS
;; although it is rejected as expression '(begin <sequence>)' as defined in
;; "7.1.3 Expressions".
(assert-equal? (tn)
               1
               ((lambda ()
                  (begin)
                  1)))
(assert-equal? (tn)
               1
               ((lambda ()
                  (begin)
                  (define var1 1)
                  (begin)
                  1)))
(assert-equal? (tn)
               '(1)
               ((lambda ()
                  (begin
                    (define var1 1))
                  (list var1))))
(assert-equal? (tn)
               '(1)
               ((lambda ()
                 (begin
                   (define (proc1) 1))
                 (list (proc1)))))
(assert-equal? (tn)
               '(1 2)
               ((lambda ()
                 (begin
                   (define var1 1)
                   (define var2 2))
                 (list var1 var2))))
(assert-equal? (tn)
               '(1 2)
               ((lambda ()
                 (begin
                   (define (proc1) 1)
                   (define (proc2) 2))
                 (list (proc1) (proc2)))))
(assert-equal? (tn)
               '(1 2)
               ((lambda ()
                 (begin
                   (define var1 1)
                   (define (proc2) 2))
                 (list var1 (proc2)))))
(assert-equal? (tn)
               '(1 2)
               ((lambda ()
                 (begin
                   (define (proc1) 1)
                   (define var2 2))
                 (list (proc1) var2))))
(assert-equal? (tn)
               '(1 2 3 4 5 6)
               ((lambda ()
                 (begin
                   (define (proc1) 1)
                   (define var2 2)
                   (begin
                     (define (proc3) 3)
                     (define var4 4)
                     (begin
                       (define (proc5) 5)
                       (define var6 6))))
                 (list (proc1) var2
                       (proc3) var4
                       (proc5) var6))))
;; begin block and single definition mixed
(assert-equal? (tn)
               '(1 2 3 4 5 6)
               ((lambda ()
                 (begin)
                 (define (proc1) 1)
                 (begin
                   (define var2 2)
                   (begin
                     (define (proc3) 3)
                     (begin)
                     (define var4 4)))
                 (begin)
                 (define (proc5) 5)
                 (begin
                   (begin
                     (begin
                       (begin)))
                   (define var6 6)
                   (begin))
                 (begin)
                 (list (proc1) var2
                       (proc3) var4
                       (proc5) var6))))

(tn "let internal definitions invalid begin blocks")
;; appending a non-definition expression into a begin block is invalid
(assert-error  (tn)
               (lambda ()
                 ((lambda ()
                    (begin
                      (define var1 1)
                      'val)
                    (list var1)))))
(assert-error  (tn)
               (lambda ()
                 ((lambda ()
                   (begin
                     (define (proc1) 1)
                     'val)
                   (list (proc1))))))
(assert-error  (tn)
               (lambda ()
                 ((lambda ()
                   (begin
                     (define var1 1)
                     (define var2 2)
                     'val)
                   (list var1 var2)))))
(assert-error  (tn)
               (lambda ()
                 ((lambda ()
                   (begin
                     (define (proc1) 1)
                     (define (proc2) 2)
                     'val)
                   (list (proc1) (proc2))))))
(assert-error  (tn)
               (lambda ()
                 ((lambda ()
                   (begin
                     (define var1 1)
                     (define (proc2) 2)
                     'val)
                   (list var1 (proc2))))))
(assert-error  (tn)
               (lambda ()
                 ((lambda ()
                   (begin
                     (define (proc1) 1)
                     (define var2 2)
                     'val)
                   (list (proc1) var2)))))
(assert-error  (tn)
               (lambda ()
                 ((lambda ()
                   (begin
                     (define (proc1) 1)
                     (define var2 2)
                     (begin
                       (define (proc3) 3)
                       (define var4 4)
                       (begin
                         (define (proc5) 5)
                         (define var6 6)
                         'val)))
                   (list (proc1) var2
                         (proc3) var4
                         (proc5) var6)))))

(tn "let internal definitions invalid placement")
;; a non-definition expression prior to internal definition is invalid
(assert-error  (tn)
               (lambda ()
                 ((lambda ()
                   'val
                   (define var1 1)))))
(assert-error  (tn)
               (lambda ()
                 ((lambda ()
                   'val
                   (define (proc1) 1)))))
(assert-error  (tn)
               (lambda ()
                 ((lambda ()
                   'val
                   (define var1 1)
                   (define var2 2)))))
(assert-error  (tn)
               (lambda ()
                 ((lambda ()
                   'val
                   (define (proc1) 1)
                   (define (proc2) 2)))))
(assert-error  (tn)
               (lambda ()
                 ((lambda ()
                   'val
                   (define var1 1)
                   (define (proc2) 2)))))
(assert-error  (tn)
               (lambda ()
                 ((lambda ()
                   'val
                   (define (proc1) 1)
                   (define var2 2)))))
(assert-error  (tn)
               (lambda ()
                 ((lambda ()
                   'val
                   (begin)))))
(assert-error  (tn)
               (lambda ()
                 ((lambda ()
                   'val
                   (begin
                     (define var1 1))))))
(assert-error  (tn)
               (lambda ()
                 ((lambda ()
                   'val
                   (begin
                     (define (proc1) 1))))))
(assert-error  (tn)
               (lambda ()
                 ((lambda ()
                   'val
                   (begin
                     (define var1 1)
                     (define var2 2))))))
(assert-error  (tn)
               (lambda ()
                 ((lambda ()
                   'val
                   (begin
                     (define (proc1) 1)
                     (define (proc2) 2))))))
(assert-error  (tn)
               (lambda ()
                 ((lambda ()
                   'val
                   (begin
                     (define var1 1)
                     (define (proc2) 2))))))
(assert-error  (tn)
               (lambda ()
                 ((lambda ()
                   'val
                   (begin
                     (define (proc1) 1)
                     (define var2 2))))))
(assert-error  (tn)
               (lambda ()
                 ((lambda ()
                   'val
                   (begin
                     (define (proc1) 1)
                     (define var2 2)
                     (begin
                       (define (proc3) 3)
                       (define var4 4)
                       (begin
                         (define (proc5) 5)
                         (define var6 6))))))))
(assert-error  (tn)
               (lambda ()
                 ((lambda ()
                   (begin
                     (define (proc1) 1)
                     (define var2 2)
                     'val
                     (begin
                       (define (proc3) 3)
                       (define var4 4)
                       (begin
                         (define (proc5) 5)
                         (define var6 6))))))))
;; a non-definition expression prior to internal definition is invalid even if
;; expression(s) is following the internal definition
(assert-error  (tn)
               (lambda ()
                 ((lambda ()
                   'val
                   (define var1 1)
                   'val))))
(assert-error  (tn)
               (lambda ()
                 ((lambda ()
                   'val
                   (define (proc1) 1)
                   'val))))
(assert-error  (tn)
               (lambda ()
                 ((lambda ()
                   'val
                   (define var1 1)
                   (define var2 2)
                   'val))))
(assert-error  (tn)
               (lambda ()
                 ((lambda ()
                   'val
                   (define (proc1) 1)
                   (define (proc2) 2)
                   'val))))
(assert-error  (tn)
               (lambda ()
                 ((lambda ()
                   'val
                   (define var1 1)
                   (define (proc2) 2)
                   'val))))
(assert-error  (tn)
               (lambda ()
                 ((lambda ()
                   'val
                   (define (proc1) 1)
                   (define var2 2)
                   'val))))
(assert-error  (tn)
               (lambda ()
                 ((lambda ()
                   'val
                   (begin)
                   'val))))
(assert-error  (tn)
               (lambda ()
                 ((lambda ()
                   'val
                   (begin
                     (define var1 1))
                   'val))))
(assert-error  (tn)
               (lambda ()
                 ((lambda ()
                   'val
                   (begin
                     (define (proc1) 1))
                   'val))))
(assert-error  (tn)
               (lambda ()
                 ((lambda ()
                   'val
                   (begin
                     (define var1 1)
                     (define var2 2))
                   'val))))
(assert-error  (tn)
               (lambda ()
                 ((lambda ()
                   'val
                   (begin
                     (define (proc1) 1)
                     (define (proc2) 2))
                   'val))))
(assert-error  (tn)
               (lambda ()
                 ((lambda ()
                   'val
                   (begin
                     (define var1 1)
                     (define (proc2) 2))
                   'val))))
(assert-error  (tn)
               (lambda ()
                 ((lambda ()
                   'val
                   (begin
                     (define (proc1) 1)
                     (define var2 2))
                   'val))))
(assert-error  (tn)
               (lambda ()
                 ((lambda ()
                   'val
                   (begin
                     (define (proc1) 1)
                     (define var2 2)
                     (begin
                       (define (proc3) 3)
                       (define var4 4)
                       (begin
                         (define (proc5) 5)
                         (define var6 6))))
                   (list (proc1) var2
                         (proc3) var4
                         (proc5) var6)))))


(total-report)
