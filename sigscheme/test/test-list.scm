#! /usr/bin/env sscm -C UTF-8
;; -*- buffer-file-coding-system: utf-8 -*-

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
(define nil  '())
(define cdr3 (cons elm3 nil))
(define cdr2 (cons elm2 cdr3))
(define cdr1 (cons elm1 cdr2))
(define cdr0 (cons elm0 cdr1))
(define lst cdr0)
;; circular lists
(define clst1 (list 1))
(set-cdr! clst1 clst1)
(define clst2 (list 1 2))
(set-cdr! (list-tail clst2 1) clst2)
(define clst3 (list 1 2 3))
(set-cdr! (list-tail clst3 2) clst3)
(define clst4 (list 1 2 3 4))
(set-cdr! (list-tail clst4 3) clst4)


(tn "null?")
(if (and (provided? "sigscheme")
         (provided? "siod-bugs"))
    (assert-eq? (tn) #t (null? #f))
    (assert-eq? (tn) #f (null? #f)))
(assert-eq? (tn) #f (null? #t))
(assert-eq? (tn) #t (null? '()))
(if (provided? "sigscheme")
    (begin
      (assert-eq? (tn) #f (null? (eof)))
      (assert-eq? (tn) #f (null? (undef)))))
(assert-eq? (tn) #f (null? 0))
(assert-eq? (tn) #f (null? 1))
(assert-eq? (tn) #f (null? 3))
(assert-eq? (tn) #f (null? -1))
(assert-eq? (tn) #f (null? -3))
(assert-eq? (tn) #f (null? 'symbol))
(assert-eq? (tn) #f (null? 'SYMBOL))
(assert-eq? (tn) #f (null? #\a))
(assert-eq? (tn) #f (null? #\あ))
(assert-eq? (tn) #f (null? ""))
(assert-eq? (tn) #f (null? " "))
(assert-eq? (tn) #f (null? "a"))
(assert-eq? (tn) #f (null? "A"))
(assert-eq? (tn) #f (null? "aBc12!"))
(assert-eq? (tn) #f (null? "あ"))
(assert-eq? (tn) #f (null? "あ0イう12!"))
(assert-eq? (tn) #f (null? +))
(assert-eq? (tn) #f (null? (lambda () #t)))

;; syntactic keywords should not be appeared as operand

(call-with-current-continuation
 (lambda (k)
   (assert-eq? (tn) #f (null? k))))
(assert-eq? (tn) #f (null? (current-output-port)))
(assert-eq? (tn) #f (null? '(#t . #t)))
(assert-eq? (tn) #f (null? (cons #t #t)))
(assert-eq? (tn) #f (null? '(0 1 2)))
(assert-eq? (tn) #f (null? (list 0 1 2)))
;; improper lists
(assert-eq? (tn) #f (null? '(0 . 1)))
(assert-eq? (tn) #f (null? '(0 1 . 2)))
(assert-eq? (tn) #f (null? '(0 1 2 . 3)))
;; circular lists
(assert-eq? (tn) #f (null? clst1))
(assert-eq? (tn) #f (null? clst2))
(assert-eq? (tn) #f (null? clst3))
(assert-eq? (tn) #f (null? clst4))
(assert-eq? (tn) #f (null? '#()))
(assert-eq? (tn) #f (null? (vector)))
(assert-eq? (tn) #f (null? '#(0 1 2)))
(assert-eq? (tn) #f (null? (vector 0 1 2)))

(tn "list?")
(if (and (provided? "sigscheme")
         (provided? "siod-bugs"))
    (assert-eq? (tn) #t (list? #f))
    (assert-eq? (tn) #f (list? #f)))
(assert-eq? (tn) #f (list? #t))
(assert-eq? (tn) #t (list? '()))
(if (provided? "sigscheme")
    (begin
      (assert-eq? (tn) #f (list? (eof)))
      (assert-eq? (tn) #f (list? (undef)))))
(assert-eq? (tn) #f (list? 0))
(assert-eq? (tn) #f (list? 1))
(assert-eq? (tn) #f (list? 3))
(assert-eq? (tn) #f (list? -1))
(assert-eq? (tn) #f (list? -3))
(assert-eq? (tn) #f (list? 'symbol))
(assert-eq? (tn) #f (list? 'SYMBOL))
(assert-eq? (tn) #f (list? #\a))
(assert-eq? (tn) #f (list? #\あ))
(assert-eq? (tn) #f (list? ""))
(assert-eq? (tn) #f (list? " "))
(assert-eq? (tn) #f (list? "a"))
(assert-eq? (tn) #f (list? "A"))
(assert-eq? (tn) #f (list? "aBc12!"))
(assert-eq? (tn) #f (list? "あ"))
(assert-eq? (tn) #f (list? "あ0イう12!"))
(assert-eq? (tn) #f (list? +))
(assert-eq? (tn) #f (list? (lambda () #t)))

;; syntactic keywords should not be appeared as operand

(call-with-current-continuation
 (lambda (k)
   (assert-eq? (tn) #f (list? k))))
(assert-eq? (tn) #f (list? (current-output-port)))
(assert-eq? (tn) #f (list? '(#t . #t)))
(assert-eq? (tn) #f (list? (cons #t #t)))
(assert-eq? (tn) #t (list? '(0 1 2)))
(assert-eq? (tn) #t (list? (list 0 1 2)))
;; improper lists
(assert-eq? (tn) #f (list? '(0 . 1)))
(assert-eq? (tn) #f (list? '(0 1 . 2)))
(assert-eq? (tn) #f (list? '(0 1 2 . 3)))
;; circular lists
(assert-eq? (tn) #f (list? clst1))
(assert-eq? (tn) #f (list? clst2))
(assert-eq? (tn) #f (list? clst3))
(assert-eq? (tn) #f (list? clst4))
(assert-eq? (tn) #f (list? '#()))
(assert-eq? (tn) #f (list? (vector)))
(assert-eq? (tn) #f (list? '#(0 1 2)))
(assert-eq? (tn) #f (list? (vector 0 1 2)))

(tn "list? from R5RS examples")
(assert-eq? (tn) #t (list? '(a b c)))
(assert-eq? (tn) #t (list? '()))
(assert-eq? (tn) #f (list? '(a . b)))
(assert-eq? (tn) #f (list? '(a b . c)))
(assert-eq? (tn) #f (let ((x (list 'a)))
                      (set-cdr! x x)
                      (list? x)))

(tn "list")
(assert-equal? (tn) '() (list))
(assert-equal? (tn) '(a) (list 'a))
(assert-equal? (tn) '(7) (list (+ 3 4)))
(assert-equal? (tn) '(7 a c) (list (+ 3 4) 'a 'c))
(assert-equal? (tn) '(a 7 c) (list 'a (+ 3 4) 'c))
(assert-equal? (tn) '(a c 7) (list 'a 'c (+ 3 4)))

(tn "length proper lists")
(assert-equal? (tn) 0 (length '()))
(assert-equal? (tn) 1 (length '(1)))
(assert-equal? (tn) 2 (length '(1 2)))
(assert-equal? (tn) 3 (length '(1 2 3)))
(assert-equal? (tn) 4 (length '(1 2 3 4)))
(tn "length improper lists")
(assert-error  (tn) (lambda () (length #t)))
(assert-error  (tn) (lambda () (length '(#t . #t))))
(assert-error  (tn) (lambda () (length '(#t #t . #t))))
(assert-error  (tn) (lambda () (length '(#t #t #t . #t))))
(assert-error  (tn) (lambda () (length '(#t #t #t #t . #t))))
(assert-error  (tn) (lambda () (length 0)))
(assert-error  (tn) (lambda () (length '(1 . 2))))
(assert-error  (tn) (lambda () (length '(1 2 . 3))))
(assert-error  (tn) (lambda () (length '(1 2 3 . 4))))
(assert-error  (tn) (lambda () (length '(1 2 3 4 . 5))))
(tn "length circular lists")
(assert-error  (tn) (lambda () (length clst1)))
(assert-error  (tn) (lambda () (length clst2)))
(assert-error  (tn) (lambda () (length clst3)))
(assert-error  (tn) (lambda () (length clst4)))
(tn "length from R5RS examples")
(assert-equal? (tn) 3 (length '(a b c)))
(assert-equal? (tn) 3 (length '(a (b) (c d e))))
(assert-equal? (tn) 0 (length '()))

(tn "append")
(assert-equal? (tn) '() (append))
(assert-equal? (tn) '() (append '()))
(assert-equal? (tn) '() (append '() '()))
(assert-equal? (tn) '() (append '() '() '()))
(assert-equal? (tn) '(a) (append '(a) '() '()))
(assert-equal? (tn) '(a) (append '() '(a) '()))
(assert-equal? (tn) '(a) (append '() '() '(a)))
(assert-equal? (tn) 'a (append 'a))
(assert-error  (tn) (lambda () (append 'a 'b)))
(assert-error  (tn) (lambda () (append 'a '(b))))
(assert-error  (tn) (lambda () (append 'a '())))
(assert-equal? (tn) '(a . b) (append '(a . b)))
(assert-error  (tn) (lambda () (append '(a . b) '())))
(assert-error  (tn) (lambda () (append '() '(a . b) '())))
(assert-equal? (tn) '(a . b) (append '() '() '(a . b)))
(assert-equal? (tn) '(1 2 3 a . b) (append '(1) '(2 3) '(a . b)))
(assert-equal? (tn) 7 (append (+ 3 4)))
(assert-equal? (tn) '(+ 3 4) (append '(+ 3 4)))

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

(tn "append from R5RS examples")
(assert-equal? (tn) '(x y)       (append '(x) '(y)))
(assert-equal? (tn) '(a b c d)   (append '(a) '(b c d)))
(assert-equal? (tn) '(a (b) (c)) (append '(a (b)) '((c))))
(assert-equal? (tn) '(a b c . d) (append '(a b) '(c . d)))
(assert-equal? (tn) 'a           (append '() 'a))

(tn "reverse")
(assert-equal? (tn) '() (reverse '()))
(assert-error  (tn) (lambda () (reverse)))
(assert-error  (tn) (lambda () (reverse '(a . b))))
(assert-error  (tn) (lambda () (reverse 'a)))
(assert-error  (tn) (lambda () (reverse '() '())))
(assert-error  (tn) (lambda () (reverse '(a) '())))
(assert-error  (tn) (lambda () (reverse '() '(a))))

(tn "reverse from R5RS examples")
(assert-equal? (tn) '(c b a) (reverse '(a b c)))
(assert-equal? (tn) '((e (f)) d (b c) a) (reverse '(a (b c) d (e (f)))))

(tn "list-tail")
(assert-equal? (tn) '(a b c)   (list-tail '(a b c) 0))
(assert-equal? (tn) '(b c)     (list-tail '(a b c) 1))
(assert-equal? (tn) '(c)       (list-tail '(a b c) 2))
(assert-equal? (tn) '()        (list-tail '(a b c) 3))
(assert-error  (tn) (lambda () (list-tail '(a b c) 4)))
(assert-error  (tn) (lambda () (list-tail '(a b c) -1)))
(assert-equal? (tn) '()        (list-tail '() 0))
(assert-error  (tn) (lambda () (list-tail '() 1)))
(assert-error  (tn) (lambda () (list-tail '() -1)))
(assert-eq?    (tn) cdr0       (list-tail lst 0))
(assert-eq?    (tn) cdr1       (list-tail lst 1))
(assert-eq?    (tn) cdr2       (list-tail lst 2))
(assert-eq?    (tn) cdr3       (list-tail lst 3))
(assert-eq?    (tn) nil        (list-tail lst 4))
(assert-error  (tn) (lambda () (list-tail lst 5)))
(assert-error  (tn) (lambda () (list-tail lst -1)))

(tn "list-tail improper list")
(assert-equal? (tn) '(a b c . d) (list-tail '(a b c . d) 0))
(assert-equal? (tn) '(b c . d)   (list-tail '(a b c . d) 1))
(assert-equal? (tn) '(c . d)     (list-tail '(a b c . d) 2))
(assert-equal? (tn) 'd           (list-tail '(a b c . d) 3))
(assert-error  (tn) (lambda ()   (list-tail '(a b c . d) 4)))
(assert-error  (tn) (lambda ()   (list-tail '(a b c . d) -1)))
(assert-equal? (tn) 'a           (list-tail 'a 0))
(assert-error  (tn) (lambda ()   (list-tail 'a 1)))
(assert-error  (tn) (lambda ()   (list-tail 'a -1)))

(tn "list-ref")
(assert-equal? (tn) 'a         (list-ref '(a b c d) 0))
(assert-equal? (tn) 'b         (list-ref '(a b c d) 1))
(assert-equal? (tn) 'c         (list-ref '(a b c d) 2))
(assert-equal? (tn) 'd         (list-ref '(a b c d) 3))
(assert-error  (tn) (lambda () (list-ref '(a b c d) 4)))
(assert-error  (tn) (lambda () (list-ref '(a b c d) -1)))
(assert-error  (tn) (lambda () (list-ref '() 0)))
(assert-error  (tn) (lambda () (list-ref '() 1)))
(assert-error  (tn) (lambda () (list-ref '() -1)))
(assert-eq?    (tn) elm0       (list-ref lst 0))
(assert-eq?    (tn) elm1       (list-ref lst 1))
(assert-eq?    (tn) elm2       (list-ref lst 2))
(assert-eq?    (tn) elm3       (list-ref lst 3))
(assert-error  (tn) (lambda () (list-ref lst 4)))
(assert-error  (tn) (lambda () (list-ref lst -1)))

(tn "list-ref improper list")
(assert-equal? (tn) 'a         (list-ref '(a b c . d) 0))
(assert-equal? (tn) 'b         (list-ref '(a b c . d) 1))
(assert-equal? (tn) 'c         (list-ref '(a b c . d) 2))
(assert-error  (tn) (lambda () (list-ref '(a b c . d) 3)))
(assert-error  (tn) (lambda () (list-ref '(a b c . d) 4)))
(assert-error  (tn) (lambda () (list-ref '(a b c . d) -1)))
(assert-error  (tn) (lambda () (list-ref 'a 0)))
(assert-error  (tn) (lambda () (list-ref 'a 1)))
(assert-error  (tn) (lambda () (list-ref 'a -1)))

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
      (assert-false (tn) (length* clst1))
      (assert-false (tn) (length* clst2))
      (assert-false (tn) (length* clst3))
      (assert-false (tn) (length* clst4))))

(total-report)
