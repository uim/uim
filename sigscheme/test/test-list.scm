;;  Filename : test-list.scm
;;  About    : unit test for list operations
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

(load "test/unittest.scm")

(define tn test-name)

(define elm0 (lambda () #f))
(define elm1 (lambda () #f))
(define elm2 (lambda () #f))
(define elm3 (lambda () #f))
(define nil  ())
(define cdr3 (cons elm3 nil))
(define cdr2 (cons elm2 cdr3))
(define cdr1 (cons elm1 cdr2))
(define cdr0 (cons elm0 cdr1))
(define lst cdr0)


(tn "pair?")
(assert-true   (tn) (pair? '(a . b)))
(assert-true   (tn) (pair? '(a b c)))
(assert-equal? (tn) #f (pair? '()))
(assert-equal? (tn) #f (pair? '#(a b)))

(tn "cons")
(assert-equal? (tn) '(a) (cons 'a '()))
(assert-equal? (tn) '((a) b c d) (cons '(a) '(b c d)))
(assert-equal? (tn) '(a . 3) (cons 'a 3))
(assert-equal? (tn) '((a b) . c) (cons '(a b) 'c))

(tn "car")
(assert-equal? (tn) 'a (car '(a b c)))
(assert-equal? (tn) '(a) (car '((a) b c)))
(assert-equal? (tn) 1 (car '(1 . 2)))

(tn "cdr")
(assert-equal? (tn) '(b c d) (cdr '((a) b c d)))
(assert-equal? (tn) 2 (cdr '(1 . 2)))

(tn "set-car!")
(define my-set-car!
  (lambda (kons x)
    (set-car! kons x)
    kons))
(assert-error  (tn) (lambda()  (my-set-car! (list) 2)))
(assert-equal? (tn) '(2)       (my-set-car! (list 0) 2))
(assert-equal? (tn) '(2 . 1)   (my-set-car! (cons 0 1) 2))
(assert-equal? (tn) '(2 1)     (my-set-car! (list 0 1) 2))
(assert-equal? (tn) '(2 1 . 2) (my-set-car! (cons 0 '(1 . 2)) 2))
(if (and (provided? "sigscheme")
         (provided? "const-list-literal"))
    (begin
      (assert-error  (tn) (lambda () (my-set-car! '(0) 2)))
      (assert-error  (tn) (lambda () (my-set-car! '(0 . 1) 2)))
      (assert-error  (tn) (lambda () (my-set-car! '(0 1) 2)))
      (assert-error  (tn) (lambda () (my-set-car! '(0 1 . 2) 2))))
    (begin
      (assert-equal? (tn) '(2)       (my-set-car! '(0) 2))
      (assert-equal? (tn) '(2 . 1)   (my-set-car! '(0 . 1) 2))
      (assert-equal? (tn) '(2 1)     (my-set-car! '(0 1) 2))
      (assert-equal? (tn) '(2 1 . 2) (my-set-car! '(0 1 . 2) 2))))

(tn "set-cdr!")
(define my-set-cdr!
  (lambda (kons x)
    (set-cdr! kons x)
    kons))
(assert-error  (tn) (lambda()  (my-set-cdr! (list) 2)))
(assert-equal? (tn) '(0 . 2)   (my-set-cdr! (list 0) 2))
(assert-equal? (tn) '(0 . 2)   (my-set-cdr! (cons 0 1) 2))
(assert-equal? (tn) '(0 . 2)   (my-set-cdr! (list 0 1) 2))
(assert-equal? (tn) '(0 . 2)   (my-set-cdr! (cons 0 '(1 . 2)) 2))
(if (and (provided? "sigscheme")
         (provided? "const-list-literal"))
    (begin
      (assert-error  (tn) (lambda () (my-set-cdr! '(0) 2)))
      (assert-error  (tn) (lambda () (my-set-cdr! '(0 . 1) 2)))
      (assert-error  (tn) (lambda () (my-set-cdr! '(0 1) 2)))
      (assert-error  (tn) (lambda () (my-set-cdr! '(0 1 . 2) 2))))
    (begin
      (assert-equal? (tn) '(0 . 2) (my-set-cdr! '(0) 2))
      (assert-equal? (tn) '(0 . 2) (my-set-cdr! '(0 . 1) 2))
      (assert-equal? (tn) '(0 . 2) (my-set-cdr! '(0 1) 2))
      (assert-equal? (tn) '(0 . 2) (my-set-cdr! '(0 1 . 2) 2))))

(tn "null?")
(assert-true   (tn) (null? '()))
(assert-equal? (tn) #f (null? "aiueo"))

(tn "list?")
(assert-true   (tn) (list? '(a b c)))
(assert-true   (tn) (list? '()))
(assert-false  (tn) (list? '(a . b)))
(assert-false  (tn) (list? '(a b . c)))
(assert-false  (tn) (let ((x (list 'a)))
                      (set-cdr! x x)
                      (list? x)))

(tn "list?")
(assert-equal? (tn) '(a 7 c) (list 'a (+ 3 4) 'c))
(assert-equal? (tn) '() (list))

(tn "length")
(assert-equal? (tn) 3 (length '(a b c)))
(assert-equal? (tn) 3 (length '(a (b) (c d e))))
(assert-equal? (tn) 0 (length '()))

(tn "append")
(assert-equal? (tn) '(x y) (append '(x) '(y)))
(assert-equal? (tn) '(a b c d) (append '(a) '(b c d)))
(assert-equal? (tn) '(a (b) (c)) (append '(a (b)) '((c))))
(define w '(n o))
(define x '(d o))
(define y '(car))
(define z '(why))
(assert-equal? (tn) '(n o d o car why . ta) (append w x y () z 'ta))
(assert-equal? (tn) '(n o) w)	; test non-destructiveness
(assert-eq?    (tn) x (cdr (append '((Calpis hosi-)) x))) ; share last

(tn "reverse")
(assert-equal? (tn) '(c b a) (reverse '(a b c)))
(assert-equal? (tn) '((e (f)) d (b c) a) (reverse '(a (b c) d (e (f)))))

(tn "list-tail")
(assert-equal? (tn) '(a b c) (list-tail '(a b c) 0))
(assert-equal? (tn) '(b c) (list-tail '(a b c) 1))
(assert-equal? (tn) '(c) (list-tail '(a b c) 2))
(assert-equal? (tn) '() (list-tail '(a b c) 3))
(assert-error  (tn) (lambda () (list-tail '(a b c) 4)))
(assert-equal? (tn) '() (list-tail '() 0))
(assert-error  (tn) (lambda () (list-tail '() 1)))
(assert-error  (tn) (lambda () (list-tail '() -1)))
(assert-eq?    (tn) cdr0 (list-tail lst 0))
(assert-eq?    (tn) cdr1 (list-tail lst 1))
(assert-eq?    (tn) cdr2 (list-tail lst 2))
(assert-eq?    (tn) cdr3 (list-tail lst 3))
(assert-eq?    (tn) nil  (list-tail lst 4))
(assert-error  (tn) (lambda () (list-tail lst 5)))
(assert-error  (tn) (lambda () (list-tail lst -1)))

(tn "list-ref")
(assert-equal? (tn) 'c (list-ref '(a b c d) 2))
(assert-eq?    (tn) elm0 (list-ref lst 0))
(assert-eq?    (tn) elm1 (list-ref lst 1))
(assert-eq?    (tn) elm2 (list-ref lst 2))
(assert-eq?    (tn) elm3 (list-ref lst 3))
(assert-error  (tn) (lambda () (list-ref lst 4)))

(tn "memq")
(assert-equal? (tn) '(a b c) (memq 'a '(a b c)))
(assert-equal? (tn) '(b c) (memq 'b '(a b c)))
(assert-equal? (tn) #f (memq 'a '(b c d)))
(assert-equal? (tn) #f (memq (list 'a) '(b (a) c)))

(tn "member")
(assert-equal? (tn) '((a) c) (member (list 'a) '(b (a) c)))

(tn "assq")
(define e '((a 1) (b 2) (c 3)))
(assert-equal? (tn) '(a 1) (assq 'a e))
(assert-equal? (tn) '(b 2) (assq 'b e))
(assert-equal? (tn) #f (assq 'd e))
(assert-equal? (tn) #f (assq (list 'a) '(((a)) ((b)) ((c)))))

(tn "assoc")
(assert-equal? (tn) '((a)) (assoc (list 'a) '(((a)) ((b)) ((c)))))

(define lst1 (list 1))
(set-cdr! lst1 lst1)
(define lst2 (list 1 2))
(set-cdr! (list-tail lst2 1) lst2)
(define lst3 (list 1 2 3))
(set-cdr! (list-tail lst3 2) lst3)
(define lst4 (list 1 2 3 4))
(set-cdr! (list-tail lst4 3) lst4)
(if (provided? "sigscheme")
    (begin
      (use sscm)
      (tn "length* proper list")
      (assert-equal? (tn) 0 (length* '()))
      (assert-equal? (tn) 1 (length* '(1)))
      (assert-equal? (tn) 2 (length* '(1 2)))
      (assert-equal? (tn) 3 (length* '(1 2 3)))
      (assert-equal? (tn) 4 (length* '(1 2 3 4)))
      (tn "length* improper list")
      (assert-equal? (tn) -1 (length* 1))
      (assert-equal? (tn) -2 (length* '(1 . 2)))
      (assert-equal? (tn) -3 (length* '(1 2 . 3)))
      (assert-equal? (tn) -4 (length* '(1 2 3 . 4)))
      (assert-equal? (tn) -5 (length* '(1 2 3 4 . 5)))
      (tn "length* circular list")
      (assert-false (tn) (length* lst1))
      (assert-false (tn) (length* lst2))
      (assert-false (tn) (length* lst3))
      (assert-false (tn) (length* lst4))))

(total-report)
