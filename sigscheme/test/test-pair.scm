#! /usr/bin/env sscm -C UTF-8
;; -*- buffer-file-coding-system: utf-8 -*-

;;  Filename : test-pair.scm
;;  About    : unit tests for pairs
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

(load "./test/unittest.scm")

(define tn test-name)
(define *test-track-progress* #f)

(tn "pair?")
(assert-eq? (tn) #f (pair? #f))
(assert-eq? (tn) #f (pair? #t))
(assert-eq? (tn) #f (pair? '()))
(if (provided? "sigscheme")
    (begin
      (assert-eq? (tn) #f (pair? (eof)))
      (assert-eq? (tn) #f (pair? (undef)))))
(assert-eq? (tn) #f (pair? 0))
(assert-eq? (tn) #f (pair? 1))
(assert-eq? (tn) #f (pair? 3))
(assert-eq? (tn) #f (pair? -1))
(assert-eq? (tn) #f (pair? -3))
(assert-eq? (tn) #f (pair? 'symbol))
(assert-eq? (tn) #f (pair? 'SYMBOL))
(assert-eq? (tn) #f (pair? #\a))
(assert-eq? (tn) #f (pair? #\あ))
(assert-eq? (tn) #f (pair? ""))
(assert-eq? (tn) #f (pair? " "))
(assert-eq? (tn) #f (pair? "a"))
(assert-eq? (tn) #f (pair? "A"))
(assert-eq? (tn) #f (pair? "aBc12!"))
(assert-eq? (tn) #f (pair? "あ"))
(assert-eq? (tn) #f (pair? "あ0イう12!"))
(assert-eq? (tn) #f (pair? +))
(assert-eq? (tn) #f (pair? (lambda () #t)))

;; syntactic keywords should not be appeared as operand
(if sigscheme?
    (begin
      ;; pure syntactic keyword
      (assert-error (tn) (lambda () (pair? else)))
      ;; expression keyword
      (assert-error (tn) (lambda () (pair? do)))))

(call-with-current-continuation
 (lambda (k)
   (assert-eq? (tn) #f (pair? k))))
(assert-eq? (tn) #f (pair? (current-output-port)))
(assert-eq? (tn) #t (pair? '(#t . #t)))
(assert-eq? (tn) #t (pair? (cons #t #t)))
(assert-eq? (tn) #t (pair? '(0 1 2)))
(assert-eq? (tn) #t (pair? (list 0 1 2)))
(assert-eq? (tn) #f (pair? '#()))
(assert-eq? (tn) #f (pair? (vector)))
(assert-eq? (tn) #f (pair? '#(0 1 2)))
(assert-eq? (tn) #f (pair? (vector 0 1 2)))

(tn "cons")
(assert-equal? (tn) '(a) (cons 'a '()))
(assert-equal? (tn) '((a) b c d) (cons '(a) '(b c d)))
(assert-equal? (tn) '(a . 3) (cons 'a 3))
(assert-equal? (tn) '((a b) . c) (cons '(a b) 'c))
(assert-equal? (tn) '(a b c) (cons 'a '(b c)))

(tn "car")
(assert-equal? (tn) 'a (car '(a b c)))
(assert-equal? (tn) '(a) (car '((a) b c)))
(assert-equal? (tn) 1 (car '(1 . 2)))
(if (and (provided? "sigscheme")
         (provided? "siod-bugs"))
    (assert-equal? (tn) '() (car '()))
    (assert-error  (tn) (lambda (car '()))))

(tn "car various type objects")
(if (and (provided? "sigscheme")
         (provided? "siod-bugs"))
    (assert-equal? (tn) '() (car #f))
    (assert-error  (tn) (lambda (car #f))))
(assert-error  (tn) (lambda () (car #t)))
(if (provided? "sigscheme")
    (begin
      (assert-error  (tn) (lambda () (car (eof))))
      (assert-error  (tn) (lambda () (car (undef))))))
(assert-error  (tn) (lambda () (car 0)))
(assert-error  (tn) (lambda () (car 1)))
(assert-error  (tn) (lambda () (car 3)))
(assert-error  (tn) (lambda () (car -1)))
(assert-error  (tn) (lambda () (car -3)))
(assert-error  (tn) (lambda () (car 'symbol)))
(assert-error  (tn) (lambda () (car 'SYMBOL)))
(assert-error  (tn) (lambda () (car #\a)))
(assert-error  (tn) (lambda () (car #\あ)))
(assert-error  (tn) (lambda () (car "")))
(assert-error  (tn) (lambda () (car " ")))
(assert-error  (tn) (lambda () (car "a")))
(assert-error  (tn) (lambda () (car "A")))
(assert-error  (tn) (lambda () (car "aBc12!")))
(assert-error  (tn) (lambda () (car "あ")))
(assert-error  (tn) (lambda () (car "あ0イう12!")))
(assert-error  (tn) (lambda () (car +)))
(assert-error  (tn) (lambda () (car (lambda () #t))))

;; syntactic keywords should not be appeared as operand
(if sigscheme?
    (begin
      ;; pure syntactic keyword
      (assert-error (tn) (lambda () (car else)))
      ;; expression keyword
      (assert-error (tn) (lambda () (car do)))))

(call-with-current-continuation
 (lambda (k)
   (assert-error  (tn) (lambda () (car k)))))
(assert-error  (tn) (lambda () (car (current-output-port))))
(assert-equal? (tn) #t (car '(#t . #t)))
(assert-equal? (tn) #t (car (cons #t #t)))
(assert-equal? (tn) 0 (car '(0 1 2)))
(assert-equal? (tn) 0 (car (list 0 1 2)))
(assert-error  (tn) (lambda () (car '#())))
(assert-error  (tn) (lambda () (car (vector))))
(assert-error  (tn) (lambda () (car '#(0 1 2))))
(assert-error  (tn) (lambda () (car (vector 0 1 2))))


(tn "cdr")
(assert-equal? (tn) '(b c d) (cdr '((a) b c d)))
(assert-equal? (tn) 2 (cdr '(1 . 2)))
(if (and (provided? "sigscheme")
         (provided? "siod-bugs"))
    (assert-equal? (tn) '() (cdr '()))
    (assert-error  (tn) (lambda (cdr '()))))

(tn "cdr various type objects")
(if (and (provided? "sigscheme")
         (provided? "siod-bugs"))
    (assert-equal? (tn) '() (cdr #f))
    (assert-error  (tn) (lambda (cdr #f))))
(assert-error  (tn) (lambda () (cdr #t)))
(if (provided? "sigscheme")
    (begin
      (assert-error  (tn) (lambda () (cdr (eof))))
      (assert-error  (tn) (lambda () (cdr (undef))))))
(assert-error  (tn) (lambda () (cdr 0)))
(assert-error  (tn) (lambda () (cdr 1)))
(assert-error  (tn) (lambda () (cdr 3)))
(assert-error  (tn) (lambda () (cdr -1)))
(assert-error  (tn) (lambda () (cdr -3)))
(assert-error  (tn) (lambda () (cdr 'symbol)))
(assert-error  (tn) (lambda () (cdr 'SYMBOL)))
(assert-error  (tn) (lambda () (cdr #\a)))
(assert-error  (tn) (lambda () (cdr #\あ)))
(assert-error  (tn) (lambda () (cdr "")))
(assert-error  (tn) (lambda () (cdr " ")))
(assert-error  (tn) (lambda () (cdr "a")))
(assert-error  (tn) (lambda () (cdr "A")))
(assert-error  (tn) (lambda () (cdr "aBc12!")))
(assert-error  (tn) (lambda () (cdr "あ")))
(assert-error  (tn) (lambda () (cdr "あ0イう12!")))
(assert-error  (tn) (lambda () (cdr +)))
(assert-error  (tn) (lambda () (cdr (lambda () #t))))

;; syntactic keywords should not be appeared as operand
(if sigscheme?
    (begin
      ;; pure syntactic keyword
      (assert-error (tn) (lambda () (cdr else)))
      ;; expression keyword
      (assert-error (tn) (lambda () (cdr do)))))

(call-with-current-continuation
 (lambda (k)
   (assert-error  (tn) (lambda () (cdr k)))))
(assert-error  (tn) (lambda () (cdr (current-output-port))))
(assert-equal? (tn) #t (cdr '(#t . #t)))
(assert-equal? (tn) #t (cdr (cons #t #t)))
(assert-equal? (tn) '(1 2) (cdr '(0 1 2)))
(assert-equal? (tn) '(1 2) (cdr (list 0 1 2)))
(assert-error  (tn) (lambda () (cdr '#())))
(assert-error  (tn) (lambda () (cdr (vector))))
(assert-error  (tn) (lambda () (cdr '#(0 1 2))))
(assert-error  (tn) (lambda () (cdr (vector 0 1 2))))


(tn "set-car!")
(define my-set-car!
  (lambda (kons x)
    (set-car! kons x)
    kons))
(assert-error  (tn) (lambda()  (set-car! '() 2)))
(assert-error  (tn) (lambda()  (set-car! (list) 2)))
(if (and (provided? "sigscheme")
         (provided? "compat-siod"))
    (assert-equal? (tn) 2 (set-car! (list 0) 2))
    (undef))

(assert-equal? (tn) '(2)       (my-set-car! (list 0) 2))
(assert-equal? (tn) '(2 . 1)   (my-set-car! (cons 0 1) 2))
(assert-equal? (tn) '(2 1)     (my-set-car! (list 0 1) 2))
(assert-equal? (tn) '(2 1 . 2) (my-set-car! (cons 0 '(1 . 2)) 2))
(if (and (provided? "sigscheme")
         (provided? "const-list-literal"))
    (begin
      (assert-error  (tn) (lambda () (set-car! '(0) 2)))
      (assert-error  (tn) (lambda () (set-car! '(0 . 1) 2)))
      (assert-error  (tn) (lambda () (set-car! '(0 1) 2)))
      (assert-error  (tn) (lambda () (set-car! '(0 1 . 2) 2))))
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
(assert-error  (tn) (lambda()  (set-cdr! '() 2)))
(assert-error  (tn) (lambda()  (set-cdr! (list) 2)))
(if (and (provided? "sigscheme")
         (provided? "compat-siod"))
    (assert-equal? (tn) 2 (set-cdr! (list 0) 2))
    (undef))

(assert-equal? (tn) '(0 . 2)   (my-set-cdr! (list 0) 2))
(assert-equal? (tn) '(0 . 2)   (my-set-cdr! (cons 0 1) 2))
(assert-equal? (tn) '(0 . 2)   (my-set-cdr! (list 0 1) 2))
(assert-equal? (tn) '(0 . 2)   (my-set-cdr! (cons 0 '(1 . 2)) 2))
(if (and (provided? "sigscheme")
         (provided? "const-list-literal"))
    (begin
      (assert-error  (tn) (lambda () (set-cdr! '(0) 2)))
      (assert-error  (tn) (lambda () (set-cdr! '(0 . 1) 2)))
      (assert-error  (tn) (lambda () (set-cdr! '(0 1) 2)))
      (assert-error  (tn) (lambda () (set-cdr! '(0 1 . 2) 2))))
    (begin
      (assert-equal? (tn) '(0 . 2) (my-set-cdr! '(0) 2))
      (assert-equal? (tn) '(0 . 2) (my-set-cdr! '(0 . 1) 2))
      (assert-equal? (tn) '(0 . 2) (my-set-cdr! '(0 1) 2))
      (assert-equal? (tn) '(0 . 2) (my-set-cdr! '(0 1 . 2) 2))))


(define tree (cons (cons (cons '(0  . 1)
                               '(2  . 3))
                         (cons '(4  . 5)
                               '(6  . 7)))
                   (cons (cons '(8  . 9)
                               '(10 . 11))
                         (cons '(12 . 13)
                               '(14 . 15)))))
(assert-equal? "caar"   '((0 . 1)   . (2 . 3))   (caar tree))
(assert-equal? "cadr"   '((8 . 9)   . (10 . 11)) (cadr tree))
(assert-equal? "cdar"   '((4 . 5)   . (6 . 7))   (cdar tree))
(assert-equal? "cddr"   '((12 . 13) . (14 . 15)) (cddr tree))
(assert-equal? "caddr"  '(12 . 13) (caddr tree))
(assert-equal? "cdddr"  '(14 . 15) (cdddr tree))
(if (or (not (provided? "sigscheme"))
        (provided? "deep-cadrs"))
    (begin
      (assert-equal? "caaar"  '(0  . 1)  (caaar tree))
      (assert-equal? "caadr"  '(8  . 9)  (caadr tree))
      (assert-equal? "cadar"  '(4  . 5)  (cadar tree))
      (assert-equal? "cdaar"  '(2  . 3)  (cdaar tree))
      (assert-equal? "cdadr"  '(10 . 11) (cdadr tree))
      (assert-equal? "cddar"  '(6  . 7)  (cddar tree))
      (assert-equal? "caaaar" 0  (caaaar tree))
      (assert-equal? "caaadr" 8  (caaadr tree))
      (assert-equal? "caadar" 4  (caadar tree))
      (assert-equal? "caaddr" 12 (caaddr tree))
      (assert-equal? "cadaar" 2  (cadaar tree))
      (assert-equal? "cadadr" 10 (cadadr tree))
      (assert-equal? "caddar" 6  (caddar tree))
      (assert-equal? "cadddr" 14 (cadddr tree))
      (assert-equal? "cdaaar" 1  (cdaaar tree))
      (assert-equal? "cdaadr" 9  (cdaadr tree))
      (assert-equal? "cdadar" 5  (cdadar tree))
      (assert-equal? "cdaddr" 13 (cdaddr tree))
      (assert-equal? "cddaar" 3  (cddaar tree))
      (assert-equal? "cddadr" 11 (cddadr tree))
      (assert-equal? "cdddar" 7  (cdddar tree))
      (assert-equal? "cddddr" 15 (cddddr tree))))

(if (and (provided? "sigscheme")
         (provided? "compat-siod"))
    (begin
      (assert-equal? "caar empty list"   '() (caar '()))
      (assert-equal? "cadr empty list"   '() (cadr '()))
      (assert-equal? "cdar empty list"   '() (cdar '()))
      (assert-equal? "cddr empty list"   '() (cddr '()))
      (assert-equal? "caddr empty list"  '() (caddr '()))
      (assert-equal? "cdddr empty list"  '() (cdddr '()))
      (if (or (not (provided? "sigscheme"))
              (provided? "deep-cadrs"))
          (begin
            (assert-equal? "caaar empty list"  '() (caaar '()))
            (assert-equal? "caadr empty list"  '() (caadr '()))
            (assert-equal? "cadar empty list"  '() (cadar '()))
            (assert-equal? "cdaar empty list"  '() (cdaar '()))
            (assert-equal? "cdadr empty list"  '() (cdadr '()))
            (assert-equal? "cddar empty list"  '() (cddar '()))
            (assert-equal? "caaaar empty list" '() (caaaar '()))
            (assert-equal? "caaadr empty list" '() (caaadr '()))
            (assert-equal? "caadar empty list" '() (caadar '()))
            (assert-equal? "caaddr empty list" '() (caaddr '()))
            (assert-equal? "cadaar empty list" '() (cadaar '()))
            (assert-equal? "cadadr empty list" '() (cadadr '()))
            (assert-equal? "caddar empty list" '() (caddar '()))
            (assert-equal? "cadddr empty list" '() (cadddr '()))
            (assert-equal? "cdaaar empty list" '() (cdaaar '()))
            (assert-equal? "cdaadr empty list" '() (cdaadr '()))
            (assert-equal? "cdadar empty list" '() (cdadar '()))
            (assert-equal? "cdaddr empty list" '() (cdaddr '()))
            (assert-equal? "cddaar empty list" '() (cddaar '()))
            (assert-equal? "cddadr empty list" '() (cddadr '()))
            (assert-equal? "cdddar empty list" '() (cdddar '()))
            (assert-equal? "cddddr empty list" '() (cddddr '())))))
    (begin
      (assert-error  "caar empty list"   (lambda () (caar '())))
      (assert-error  "cadr empty list"   (lambda () (cadr '())))
      (assert-error  "cdar empty list"   (lambda () (cdar '())))
      (assert-error  "cddr empty list"   (lambda () (cddr '())))
      (assert-error  "caddr empty list"  (lambda () (caddr '())))
      (assert-error  "cdddr empty list"  (lambda () (cdddr '())))
      (if (or (not (provided? "sigscheme"))
              (provided? "deep-cadrs"))
          (begin
            (assert-error  "caaar empty list"  (lambda () (caaar '())))
            (assert-error  "caadr empty list"  (lambda () (caadr '())))
            (assert-error  "cadar empty list"  (lambda () (cadar '())))
            (assert-error  "cdaar empty list"  (lambda () (cdaar '())))
            (assert-error  "cdadr empty list"  (lambda () (cdadr '())))
            (assert-error  "cddar empty list"  (lambda () (cddar '())))
            (assert-error  "caaaar empty list" (lambda () (caaaar '())))
            (assert-error  "caaadr empty list" (lambda () (caaadr '())))
            (assert-error  "caadar empty list" (lambda () (caadar '())))
            (assert-error  "caaddr empty list" (lambda () (caaddr '())))
            (assert-error  "cadaar empty list" (lambda () (cadaar '())))
            (assert-error  "cadadr empty list" (lambda () (cadadr '())))
            (assert-error  "caddar empty list" (lambda () (caddar '())))
            (assert-error  "cadddr empty list" (lambda () (cadddr '())))
            (assert-error  "cdaaar empty list" (lambda () (cdaaar '())))
            (assert-error  "cdaadr empty list" (lambda () (cdaadr '())))
            (assert-error  "cdadar empty list" (lambda () (cdadar '())))
            (assert-error  "cdaddr empty list" (lambda () (cdaddr '())))
            (assert-error  "cddaar empty list" (lambda () (cddaar '())))
            (assert-error  "cddadr empty list" (lambda () (cddadr '())))
            (assert-error  "cdddar empty list" (lambda () (cdddar '())))
            (assert-error  "cddddr empty list" (lambda () (cddddr '())))))))

(assert-error  "caar insufficient list"   (lambda () (caar '(0 . 1))))
(assert-error  "cadr insufficient list"   (lambda () (cadr '(0 . 1))))
(assert-error  "cdar insufficient list"   (lambda () (cdar '(0 . 1))))
(assert-error  "cddr insufficient list"   (lambda () (cddr '(0 . 1))))
(assert-error  "caddr insufficient list"  (lambda () (caddr '(0 . 1))))
(assert-error  "cdddr insufficient list"  (lambda () (cdddr '(0 . 1))))
(if (or (not (provided? "sigscheme"))
        (provided? "deep-cadrs"))
    (begin
      (assert-error  "caaar insufficient list"  (lambda () (caaar '(0 . 1))))
      (assert-error  "caadr insufficient list"  (lambda () (caadr '(0 . 1))))
      (assert-error  "cadar insufficient list"  (lambda () (cadar '(0 . 1))))
      (assert-error  "cdaar insufficient list"  (lambda () (cdaar '(0 . 1))))
      (assert-error  "cdadr insufficient list"  (lambda () (cdadr '(0 . 1))))
      (assert-error  "cddar insufficient list"  (lambda () (cddar '(0 . 1))))
      (assert-error  "caaaar insufficient list" (lambda () (caaaar '(0 . 1))))
      (assert-error  "caaadr insufficient list" (lambda () (caaadr '(0 . 1))))
      (assert-error  "caadar insufficient list" (lambda () (caadar '(0 . 1))))
      (assert-error  "caaddr insufficient list" (lambda () (caaddr '(0 . 1))))
      (assert-error  "cadaar insufficient list" (lambda () (cadaar '(0 . 1))))
      (assert-error  "cadadr insufficient list" (lambda () (cadadr '(0 . 1))))
      (assert-error  "caddar insufficient list" (lambda () (caddar '(0 . 1))))
      (assert-error  "cadddr insufficient list" (lambda () (cadddr '(0 . 1))))
      (assert-error  "cdaaar insufficient list" (lambda () (cdaaar '(0 . 1))))
      (assert-error  "cdaadr insufficient list" (lambda () (cdaadr '(0 . 1))))
      (assert-error  "cdadar insufficient list" (lambda () (cdadar '(0 . 1))))
      (assert-error  "cdaddr insufficient list" (lambda () (cdaddr '(0 . 1))))
      (assert-error  "cddaar insufficient list" (lambda () (cddaar '(0 . 1))))
      (assert-error  "cddadr insufficient list" (lambda () (cddadr '(0 . 1))))
      (assert-error  "cdddar insufficient list" (lambda () (cdddar '(0 . 1))))
      (assert-error  "cddddr insufficient list" (lambda () (cddddr '(0 . 1))))))

(assert-error  "caar invalid object"   (lambda () (caar #t)))
(assert-error  "cadr invalid object"   (lambda () (cadr #t)))
(assert-error  "cdar invalid object"   (lambda () (cdar #t)))
(assert-error  "cddr invalid object"   (lambda () (cddr #t)))
(assert-error  "caddr invalid object"  (lambda () (caddr #t)))
(assert-error  "cdddr invalid object"  (lambda () (cdddr #t)))
(if (or (not (provided? "sigscheme"))
        (provided? "deep-cadrs"))
    (begin
      (assert-error  "caaar invalid object"  (lambda () (caaar #t)))
      (assert-error  "caadr invalid object"  (lambda () (caadr #t)))
      (assert-error  "cadar invalid object"  (lambda () (cadar #t)))
      (assert-error  "cdaar invalid object"  (lambda () (cdaar #t)))
      (assert-error  "cdadr invalid object"  (lambda () (cdadr #t)))
      (assert-error  "cddar invalid object"  (lambda () (cddar #t)))
      (assert-error  "caaaar invalid object" (lambda () (caaaar #t)))
      (assert-error  "caaadr invalid object" (lambda () (caaadr #t)))
      (assert-error  "caadar invalid object" (lambda () (caadar #t)))
      (assert-error  "caaddr invalid object" (lambda () (caaddr #t)))
      (assert-error  "cadaar invalid object" (lambda () (cadaar #t)))
      (assert-error  "cadadr invalid object" (lambda () (cadadr #t)))
      (assert-error  "caddar invalid object" (lambda () (caddar #t)))
      (assert-error  "cadddr invalid object" (lambda () (cadddr #t)))
      (assert-error  "cdaaar invalid object" (lambda () (cdaaar #t)))
      (assert-error  "cdaadr invalid object" (lambda () (cdaadr #t)))
      (assert-error  "cdadar invalid object" (lambda () (cdadar #t)))
      (assert-error  "cdaddr invalid object" (lambda () (cdaddr #t)))
      (assert-error  "cddaar invalid object" (lambda () (cddaar #t)))
      (assert-error  "cddadr invalid object" (lambda () (cddadr #t)))
      (assert-error  "cdddar invalid object" (lambda () (cdddar #t)))
      (assert-error  "cddddr invalid object" (lambda () (cddddr #t)))))


(total-report)
