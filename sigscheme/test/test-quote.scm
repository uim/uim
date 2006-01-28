;;  FileName : test-quote.scm
;;  About    : unit test for quote and quasiquote
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
(define *test-track-progress* #f)

(assert-true "quasiquote check #1" (equal? '(1 2 3) `(1 2 3)))
(assert-true "quasiquote check #2" (equal? '(5) `(,(+ 2 3))))
(assert-true "unquote check" (equal? `(1 2 3) `(1 ,(+ 1 1) ,(+ 1 2))))
(assert-true "unquote-splicing check" (equal? `(1 2 3) `(1 ,@(cdr '(1 2)) 3)))
(assert-true "mixed check" (equal? '(a 3 c 7 8 9) `(a ,(+ 1 2) c ,@(cdr '(6 7 8 9)))))
(assert-equal? "nested quasiquote check #1"
               '(a `(b c ,() 0) 1)
               `(a `(b c ,(,@() ,@()) 0) 1))

(assert-equal? "nested quasiquote check #2"
               '(0 1)
               `(0 . ,(list 1)))

(assert-equal? "nested quasiquote check #3"
               '(0 . 1)
               `(0 . ,'1))

(assert-equal? "nested quasiquote check #4"
               '(0 quasiquote (unquote 1))
               `(0 . `,,(+ 1)))

(assert-true "vector quasiquote check #1"
	(equal?
	 '#(#(a b c d) e)
	 `#(,@() #(a ,@(list 'b 'c) d) e)))
(assert-equal? "vector quasiquote check #2" '(1 . #(2 3)) `(1 . #(,(+ 1 1) 3)))
(assert-equal? "vector quasiquote check #3"
               '(0 . #(1 2 3 4 5 6))
               `(0 . #(1 ,2 ,@(list 3 4) 5 ,6 ,@())))
(assert-equal? "vector quasiquote check #3"
               '#(a b)
               `#(,@(list 'a 'b)))

(tn "quasiquote reference test of R5RS")
(if (not (symbol-bound? 'sqrt))
    (define sqrt
      (lambda (x)
        (cdr (assv x '((4  . 2)
                       (9  . 3)
                       (16 . 4)))))))
(assert-equal? (tn)
               '(list 3 4)
               `(list ,(+ 1 2) 4))
(assert-equal? (tn)
               '(list a (quote a))
               (let ((name 'a)) `(list ,name ',name)))
(assert-equal? (tn)
               '(a 3 4 5 6 b)
               `(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b))
;; Commented out since the test seems to wrong. Even if the interpretation for
;; the quote after foo (foo') may varied by implementation, at least the
;; quasiquote before foo (`foo) must be remained.
;;
;; SigScheme: (((quasiquote foo') 7) . cons)
;; Gauche:    ((`foo '7) . cons)
;; Guile:     (((quasiquote foo') 7) . cons)
;; Bigloo:    (((quasiquote foo') 7) . cons)
;; Scheme48:  (((quasiquote foo) '7) . cons)
;; SCM:       (((quasiquote foo\') 7) . cons)
;; PLT:       read: illegal use of backquote
;;(assert-equal? (tn)
;;               '((foo 7) . cons)
;;               `((`foo' ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons))))
(assert-equal? (tn)
               '#(10 5 2 4 3 8)
               `#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8))
(assert-equal? (tn)
               '(a `(b ,(+ 1 2) ,(foo 4 d) e) f)
               `(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f))
(assert-equal? (tn)
               '(a `(b ,x ,'y d) e)
               (let ((name1 'x)
                     (name2 'y))
                 `(a `(b ,,name1 ,',name2 d) e)))
(assert-equal? (tn)
               '(list 3 4)
               (quasiquote (list (unquote (+ 1 2)) 4)))
(assert-equal? (tn)
               '`(list ,(+ 1 2) 4)
               '(quasiquote (list (unquote (+ 1 2)) 4)))

(tn "quasiquote valid form")
(assert-equal? (tn) ''1      `'1)
(assert-equal? (tn) '`1      ``1)
(assert-equal? (tn) 1        `,1)
(assert-equal? (tn) ''1      `',1)
(assert-equal? (tn) '(quote 1)      `'1)
(assert-equal? (tn) '(quasiquote 1) ``1)
(assert-equal? (tn) '(quote 1)      `',1)
(assert-equal? (tn) '('1)    `('1))
(assert-equal? (tn) '(`1)    `(`1))
(assert-equal? (tn) '(1)     `(,1))
(assert-equal? (tn) '('1)    `(',1))
(assert-equal? (tn) '((quote 1))      `('1))
(assert-equal? (tn) '((quasiquote 1)) `(`1))
(assert-equal? (tn) '(1)              `(,1))
(assert-equal? (tn) '((quote 1))      `(',1))
(assert-equal? (tn) '()      `(,@()))
(assert-equal? (tn) '()      `(,@(list)))
(assert-equal? (tn) '(1)     `(,@(list 1)))
(assert-equal? (tn) '(1 2)   `(,@(list 1 2)))
(assert-equal? (tn) '(1 2 3) `(,@(list 1 2 3)))

(tn "quasiquote nested")
(assert-equal? (tn)
               '((quasiquote q) q)
               `(`q ,`q))
(assert-equal? (tn)
               '((quasiquote q) (q (quasiquote q)))
               `(`q ,`(q `q)))
(assert-equal? (tn)
               '((quasiquote q) (q q))
               `(`q ,`(q ,`q)))
(assert-equal? (tn)
               '((quasiquote q) (q q (quasiquote q)))
               `(`q ,`(q ,`q `q)))
(assert-equal? (tn)
               '((quasiquote q) (q q (quasiquote (unquote q))))
               `(`q ,`(q ,`q `,q)))
(assert-equal? (tn)
               '((quasiquote q) (q q (quasiquote (unquote (quasiquote q)))))
               `(`q ,`(q ,`q `,`q)))
(assert-equal? (tn)
               '((quasiquote q) (q q (quasiquote (unquote q))))
               `(`q ,`(q ,`q `,,`q)))

;; R5RS allows these forms to be an error
(tn "quasiquote implementation-dependent form")
(if (provided? "sigscheme")
    (begin
      (assert-error  (tn) (lambda () `((quasiquote))))
      (assert-error  (tn) (lambda () `((quasiquote . 0))))
      (assert-error  (tn) (lambda () `((quasiquote 0 1))))
      (assert-error  (tn) (lambda () `((quasiquote 0 . 1))))
      (assert-error  (tn) (lambda () `(0 quasiquote)))
      (assert-error  (tn) (lambda () `(0 . (quasiquote))))
      (assert-error  (tn) (lambda () `(0 quasiquote 2 3)))
      (assert-error  (tn) (lambda () `(0 . (quasiquote 2 3))))
      (assert-error  (tn) (lambda () `(0 quasiquote 2 3 4)))
      (assert-error  (tn) (lambda () `(0 . (quasiquote 2 3 4))))
      (assert-error  (tn) (lambda () `(0 quasiquote . 0)))
      (assert-error  (tn) (lambda () `(0 . (quasiquote . 0))))
      (assert-error  (tn) (lambda () `(0 quasiquote 2 3 . 0)))
      (assert-error  (tn) (lambda () `(0 . (quasiquote 2 3 . 0))))
      (assert-error  (tn) (lambda () `(0 quasiquote 2 3 4 . 0)))
      (assert-error  (tn) (lambda () `(0 . (quasiquote 2 3 4 . 0))))))
(tn "unquote implementation-dependent form")
(if (provided? "sigscheme")
    (begin
      (assert-error  (tn) (lambda () `((unquote))))
      (assert-error  (tn) (lambda () `((unquote . 0))))
      (assert-error  (tn) (lambda () `((unquote 0 1))))
      (assert-error  (tn) (lambda () `((unquote 0 . 1))))
      (assert-error  (tn) (lambda () `(0 unquote)))
      (assert-error  (tn) (lambda () `(0 . (unquote))))
      (assert-error  (tn) (lambda () `(0 unquote 2 3)))
      (assert-error  (tn) (lambda () `(0 . (unquote 2 3))))
      (assert-error  (tn) (lambda () `(0 unquote 2 3 4)))
      (assert-error  (tn) (lambda () `(0 . (unquote 2 3 4))))
      (assert-error  (tn) (lambda () `(0 unquote . 0)))
      (assert-error  (tn) (lambda () `(0 . (unquote . 0))))
      (assert-error  (tn) (lambda () `(0 unquote 2 3 . 0)))
      (assert-error  (tn) (lambda () `(0 . (unquote 2 3 . 0))))
      (assert-error  (tn) (lambda () `(0 unquote 2 3 4 . 0)))
      (assert-error  (tn) (lambda () `(0 . (unquote 2 3 4 . 0))))))
(tn "unquote-splicing implementation-dependent form")
(if (provided? "sigscheme")
    (begin
      (assert-error  (tn) (lambda () `(0 unquote-splicing)))
      (assert-error  (tn) (lambda () `(0 . (unquote-splicing))))
      (assert-error  (tn) (lambda () `(0 unquote-splicing 2 3)))
      (assert-error  (tn) (lambda () `(0 . (unquote-splicing 2 3))))
      (assert-error  (tn) (lambda () `(0 unquote-splicing 2 3 4)))
      (assert-error  (tn) (lambda () `(0 . (unquote-splicing 2 3 4))))
      (assert-error  (tn) (lambda () `(0 unquote-splicing . 0)))
      (assert-error  (tn) (lambda () `(0 . (unquote-splicing . 0))))
      (assert-error  (tn) (lambda () `(0 unquote-splicing 2 3 . 0)))
      (assert-error  (tn) (lambda () `(0 . (unquote-splicing 2 3 . 0))))
      (assert-error  (tn) (lambda () `(0 unquote-splicing 2 3 4 . 0)))
      (assert-error  (tn) (lambda () `(0 . (unquote-splicing 2 3 4 . 0))))
      (assert-error  (tn) (lambda () `((unquote-splicing))))
      (assert-error  (tn) (lambda () `((unquote-splicing . 0))))
      (assert-error  (tn) (lambda () `((unquote-splicing 0 1))))
      (assert-error  (tn) (lambda () `((unquote-splicing 0 . 1))))))

(tn "quasiquote dotted list")
(assert-equal? (tn) '(0 . '1)         `(0 . '1))
(assert-equal? (tn) '(0 . `1)         `(0 . `1))
(assert-equal? (tn) '(0 . 1)          `(0 . ,1))
(assert-equal? (tn) '(0 . (quote 1))  `(0 . '1))
(assert-equal? (tn) '(0 . (quasiquote 1)) `(0 . `1))
(assert-equal? (tn) '(0 . #(1))       `(0 . ,'#(1)))
(assert-equal? (tn) '(0 . #(1))       `(0 . ,`#(1)))
(assert-equal? (tn) '(0 . #(1 3))     `(0 . ,`#(1 ,(+ 1 2))))
(assert-equal? (tn) '(0 . #(1 -1 -2)) `(0 . ,`#(1 ,@(list (- 1) (- 2)))))
(assert-error  (tn) (lambda ()        `(0 . ,@())))
(assert-error  (tn) (lambda ()        `(0 . ,@(list))))
(assert-error  (tn) (lambda ()        `(0 . ,@(list 1))))
(assert-error  (tn) (lambda ()        `(0 . ,@(list 1 2))))
(assert-error  (tn) (lambda ()        `(0 . ,@(list 1 2 3))))
(assert-error  (tn) (lambda ()        `(0 . ,@#t)))
(assert-error  (tn) (lambda ()        `(0 . ,@1)))
(assert-error  (tn) (lambda ()        `(0 . ,@#\a)))
(assert-error  (tn) (lambda ()        `(0 . ,@"str")))
(assert-error  (tn) (lambda ()        `(0 . ,@'sym)))
(assert-error  (tn) (lambda ()        `(0 . ,@sym)))
(assert-error  (tn) (lambda ()        `(0 . ,@var)))
(assert-error  (tn) (lambda ()        `(0 . ,@(lambda () #f))))
(assert-error  (tn) (lambda ()        `(0 . ,@(+ 1 2))))
(assert-error  (tn) (lambda ()        `(0 . ,@#(1 2))))
(assert-error  (tn) (lambda ()        `(0 . (unquote-splicing ()))))
(assert-error  (tn) (lambda ()        `(0 . (unquote-splicing (list)))))
(assert-error  (tn) (lambda ()        `(0 . (unquote-splicing (list 1)))))
(assert-error  (tn) (lambda ()        `(0 . (unquote-splicing (list 1 2)))))
(assert-error  (tn) (lambda ()        `(0 . (unquote-splicing (list 1 2 3)))))
(assert-error  (tn) (lambda ()        `(0 . (unquote-splicing #t))))
(assert-error  (tn) (lambda ()        `(0 . (unquote-splicing 1))))
(assert-error  (tn) (lambda ()        `(0 . (unquote-splicing #\a))))
(assert-error  (tn) (lambda ()        `(0 . (unquote-splicing "str"))))
(assert-error  (tn) (lambda ()        `(0 . (unquote-splicing 'sym))))
(assert-error  (tn) (lambda ()        `(0 . (unquote-splicing sym))))
(assert-error  (tn) (lambda ()        `(0 . (unquote-splicing var))))
(assert-error  (tn) (lambda ()        `(0 . (unquote-splicing (lambda () #f)))))
(assert-error  (tn) (lambda ()        `(0 . (unquote-splicing (+ 1 2)))))
(assert-error  (tn) (lambda ()        `(0 . (unquote-splicing #(1 2)))))

(tn "unquote invalid form")

(tn "unquote-splicing invalid form")
(define sym 'sym)
(define var 3)
(assert-error (tn) (lambda () `,@()))
(assert-error (tn) (lambda () `,@(list)))
(assert-error (tn) (lambda () `,@(list 1)))
(assert-error (tn) (lambda () `,@(list 1 2)))
(assert-error (tn) (lambda () `,@(list 1 2 3)))
(assert-error (tn) (lambda () `,@#t))
(assert-error (tn) (lambda () `,@1))
(assert-error (tn) (lambda () `,@1))
(assert-error (tn) (lambda () `,@#\a))
(assert-error (tn) (lambda () `,@"str"))
(assert-error (tn) (lambda () `,@'sym))
(assert-error (tn) (lambda () `,@sym))
(assert-error (tn) (lambda () `,@var))
(assert-error (tn) (lambda () `,@(lambda () #f)))
(assert-error (tn) (lambda () `,@(+ 1 2)))
(assert-error (tn) (lambda () `,@#(1 2)))
(assert-error (tn) (lambda () `(,@#t)))
(assert-error (tn) (lambda () `(,@1)))
(assert-error (tn) (lambda () `(,@#\a)))
(assert-error (tn) (lambda () `(,@"str")))
(assert-error (tn) (lambda () `(,@'sym)))
(assert-error (tn) (lambda () `(,@sym)))
(assert-error (tn) (lambda () `(,@var)))
(assert-error (tn) (lambda () `(,@(lambda () #f))))
(assert-error (tn) (lambda () `(,@(+ 1 2))))
(assert-error (tn) (lambda () `(,@#(1 2))))

(total-report)
