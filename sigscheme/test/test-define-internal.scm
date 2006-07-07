;;  Filename : test-define-internal.scm
;;  About    : unit test for R5RS internal definitions
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

;; internal definitions in 'let' variants are writtin at test-let.scm

(load "./test/unittest.scm")

(define tn test-name)
(define *test-track-progress* #f)

(tn "internal defintions")
(assert-equal? (tn)
               14
               (let ((x 5))
                 (+ (let ()
                      (define x 6)
                      (+ x 3))
                    x)))
(assert-equal? (tn)
               14
               (let ((x 5))
                 (+ (let* ()
                      (define x 6)
                      (+ x 3))
                    x)))
(assert-equal? (tn)
               14
               (let ((x 5))
                 (+ (letrec ()
                      (define x 6)
                      (+ x 3))
                    x)))
(assert-equal? (tn)
               14
               (let ((x 5))
                 (+ ((lambda ()
                       (define x 6)
                       (+ x 3)))
                    x)))
(assert-equal? (tn)
               14
               (let ((x 5))
                 (+ (let ()
                      (define (f)
                        (define x 6)
                        (+ x 3))
                      (f))
                    x)))

(tn "internal defintions: letrec-like behavior")
(assert-equal? (tn)
               45
               (let ((x 5))
                 (define foo (lambda (y) (bar x y)))
                 (define bar (lambda (a b) (+ (* a b) a)))
                 (foo (+ x 3))))
(assert-equal? (tn)
               45
               (let ((x 5))
                 (define bar (lambda (a b) (+ (* a b) a)))
                 (define foo (lambda (y) (bar x y)))
                 (foo (+ x 3))))
(assert-error (tn)
               (lambda ()
                 (let ((x 5))
                   (define foo bar)
                   (define bar (lambda (a b) (+ (* a b) a)))
                   (foo x (+ x 3)))))
(assert-error  (tn)
               (lambda ()
                 (let ((x 5))
                   (define bar (lambda (a b) (+ (* a b) a)))
                   (define foo bar)
                   (foo x (+ x 3)))))
(assert-error  (tn)
               (lambda ()
                 (let ()
                   (define foo 1)
                   (define bar (+ foo 1))
                   (+ foo bar))))
(assert-error  (tn)
               (lambda ()
                 (let ()
                   (define bar (+ foo 1))
                   (define foo 1)
                   (+ foo bar))))
(assert-error  (tn)
               (lambda ()
                 (let ((foo 3))
                   (define foo 1)
                   (define bar (+ foo 1))
                   (+ foo bar))))
(assert-error  (tn)
               (lambda ()
                 (let ((foo 3))
                   (define bar (+ foo 1))
                   (define foo 1)
                   (+ foo bar))))

(tn "internal defintions: non-beginning of block")
(assert-error  (tn)
               (lambda ()
                 (let ()
                   (define foo 1)
                   (set! foo 5)
                   (define bar 2)
                   (+ foo bar))))
(assert-error  (tn)
               (lambda ()
                 (let* ()
                   (define foo 1)
                   (set! foo 5)
                   (define bar 2)
                   (+ foo bar))))
(assert-error  (tn)
               (lambda ()
                 (letrec ()
                   (define foo 1)
                   (set! foo 5)
                   (define bar 2)
                   (+ foo bar))))
(assert-error  (tn)
               (lambda ()
                 ((lambda ()
                    (define foo 1)
                    (set! foo 5)
                    (define bar 2)
                    (+ foo bar)))))
(assert-error  (tn)
               (lambda ()
                 (define (f)
                   (define foo 1)
                   (set! foo 5)
                   (define bar 2)
                   (+ foo bar))
                 (f)))

(tn "internal defintions: non-beginning of block (in begin)")
(assert-error  (tn)
               (lambda ()
                 (let ()
                   (define foo 1)
                   (set! foo 5)
                   (begin
                     (define bar 2))
                   (+ foo bar))))
(assert-error  (tn)
               (lambda ()
                 (let* ()
                   (define foo 1)
                   (set! foo 5)
                   (begin
                     (define bar 2))
                   (+ foo bar))))
(assert-error  (tn)
               (lambda ()
                 (letrec ()
                   (define foo 1)
                   (set! foo 5)
                   (begin
                     (define bar 2))
                   (+ foo bar))))
(assert-error  (tn)
               (lambda ()
                 ((lambda ()
                    (define foo 1)
                    (set! foo 5)
                    (begin
                      (define bar 2))
                    (+ foo bar)))))
(assert-error  (tn)
               (lambda ()
                 (define (f)
                   (define foo 1)
                   (set! foo 5)
                   (begin
                     (define bar 2))
                   (+ foo bar))
                 (f)))

(tn "internal defintions: non-beginning of block (in eval)")
(assert-equal? (tn)
               7
               (let ()
                 (define foo 1)
                 (set! foo 5)
                 (eval '(define bar 2)
                       (interaction-environment))
                 (+ foo bar)))
(assert-equal? (tn)
               7
               (let* ()
                 (define foo 1)
                 (set! foo 5)
                 (eval '(define bar 2)
                       (interaction-environment))
                 (+ foo bar)))
(assert-equal? (tn)
               7
               (letrec ()
                 (define foo 1)
                 (set! foo 5)
                 (eval '(define bar 2)
                       (interaction-environment))
                 (+ foo bar)))
(assert-equal? (tn)
               7
               ((lambda ()
                  (define foo 1)
                  (set! foo 5)
                  (eval '(define bar 2)
                        (interaction-environment))
                  (+ foo bar))))
(assert-equal? (tn)
               7
               (let ()
                 (define (f)
                   (define foo 1)
                   (set! foo 5)
                   (eval '(define bar 2)
                         (interaction-environment))
                   (+ foo bar))
                 (f)))

;; As specified as follows in R5RS, definitions in following forms are invalid.
;;
;; 5.2 Definitions
;;
;; Definitions are valid in some, but not all, contexts where expressions are
;; allowed. They are valid only at the top level of a <program> and at the
;; beginning of a <body>.
;;
;; 5.2.2 Internal definitions
;;
;; Definitions may occur at the beginning of a <body> (that is, the body of a
;; lambda, let, let*, letrec, let-syntax, or letrec-syntax expression or that
;; of a definition of an appropriate form).
;;
;; Wherever an internal definition may occur (begin <definition1> ...) is
;; equivalent to the sequence of definitions that form the body of the begin.
(tn "definition in do")
(assert-error (tn)
              (lambda ()
                (do ((i 0 (+ i 1)))
                    ((= i 1) (+ x 3))
                  (define x 6))))
(assert-error (tn)
              (lambda ()
                (do ((i 0 (+ i 1)))
                    ((= i 1) (+ x 3))
                  (begin
                    (define x 6)))))
(assert-equal? (tn)
               9
               (do ((i 0 (+ i 1)))
                   ((= i 1) (+ x 3))
                 (eval '(define x 6)
                       (interaction-environment))))
(tn "definition in if")
(assert-error  (tn)
               (lambda ()
                 (if #t
                     (define x 6))))
(assert-error  (tn)
               (lambda ()
                 (if #t
                     (begin
                       (define x 6)))))
(assert-equal? (tn)
               'x
               (if #t
                   (eval '(define x 6)
                         (interaction-environment))))


(tn "func-form define internal definitions lacking sequence part")
;; at least one <expression> is required
(define (f)
  (define var1 1))
(assert-error  (tn) (lambda () (f)))
(define (f)
  (define (proc1) 1))
(assert-error  (tn) (lambda () (f)))
(define (f)
  (define var1 1)
  (define var2 2))
(assert-error  (tn) (lambda () (f)))
(define (f)
  (define (proc1) 1)
  (define (proc2) 2))
(assert-error  (tn) (lambda () (f)))
(define (f)
  (define var1 1)
  (define (proc2) 2))
(assert-error  (tn) (lambda () (f)))
(define (f)
  (define (proc1) 1)
  (define var2 2))
(assert-error  (tn) (lambda () (f)))
(define (f)
  (begin))
(assert-error  (tn) (lambda () (f)))
(define (f)
  (begin
    (define var1 1)))
(assert-error  (tn) (lambda () (f)))
(define (f)
  (begin
    (define (proc1) 1)))
(assert-error  (tn) (lambda () (f)))
(define (f)
  (begin
    (define var1 1)
    (define var2 2)))
(assert-error  (tn) (lambda () (f)))
(define (f)
  (begin
    (define (proc1) 1)
    (define (proc2) 2)))
(assert-error  (tn) (lambda () (f)))
(define (f)
  (begin
    (define var1 1)
    (define (proc2) 2)))
(assert-error  (tn) (lambda () (f)))
(define (f)
  (begin
    (define (proc1) 1)
    (define var2 2)))
(assert-error  (tn) (lambda () (f)))
;; appending a non-definition expression into a begin block is invalid
(define (f)
  (begin
    (define var1 1)
    'val))
(assert-error  (tn) (lambda () (f)))
(define (f)
  (begin
    (define (proc1) 1)
    'val))
(assert-error  (tn) (lambda () (f)))
(define (f)
  (begin
    (define var1 1)
    (define var2 2)
    'val))
(assert-error  (tn) (lambda () (f)))
(define (f)
  (begin
    (define (proc1) 1)
    (define (proc2) 2)
    'val))
(assert-error  (tn) (lambda () (f)))
(define (f)
  (begin
    (define var1 1)
    (define (proc2) 2)
    'val))
(assert-error  (tn) (lambda () (f)))
(define (f)
  (begin
    (define (proc1) 1)
    (define var2 2)
    'val))
(assert-error  (tn) (lambda () (f)))

(tn "func-form define internal definitions cross reference")
;; R5RS: 5.2.2 Internal definitions
;; Just as for the equivalent `letrec' expression, it must be possible to
;; evaluate each <expression> of every internal definition in a <body> without
;; assigning or referring to the value of any <variable> being defined.
(define (f)
  (define var1 1)
  (define var2 var1)
  'val)
(assert-error  (tn) (lambda () (f)))
(define (f)
  (define var1 var2)
  (define var2 2)
  'val)
(assert-error  (tn) (lambda () (f)))
(define (f)
  (define var1 var1)
  'val)
(assert-error  (tn) (lambda () (f)))
(define (f var0)
  (define var1 var0)
  (define var2 var0)
  (begin
    (define var3 var0)
    (begin
      (define var4 var0)))
  (define var5 var0)
  (list var1 var2 var3 var4 var5))
(assert-equal? (tn)
               '(0 0 0 0 0)
               (f 0))
(assert-equal? (tn)
               '(#f #f #f #f #f #f)
               (list (symbol-bound? 'var0)
                     (symbol-bound? 'var1)
                     (symbol-bound? 'var2)
                     (symbol-bound? 'var3)
                     (symbol-bound? 'var4)
                     (symbol-bound? 'var5)))
(define (f var0)
  (define var1 (symbol-bound? 'var1))
  (define var2 (symbol-bound? 'var1))
  (begin
    (define var3 (symbol-bound? 'var1))
    (begin
      (define var4 (symbol-bound? 'var1))))
  (define var5 (symbol-bound? 'var1))
  (list var0 var1 var2 var3 var4 var5))
(assert-equal? (tn)
               '(#f #f #f #f #f #f)
               (f #f))
(define (f var0)
  (define var1 (symbol-bound? 'var3))
  (define var2 (symbol-bound? 'var3))
  (begin
    (define var3 (symbol-bound? 'var3))
    (begin
      (define var4 (symbol-bound? 'var3))))
  (define var5 (symbol-bound? 'var3))
  (list var0 var1 var2 var3 var4 var5))
(assert-equal? (tn)
               '(#f #f #f #f #f #f)
               (f #f))
(define (f var0)
  (define var1 (symbol-bound? 'var4))
  (define var2 (symbol-bound? 'var4))
  (begin
    (define var3 (symbol-bound? 'var4))
    (begin
      (define var4 (symbol-bound? 'var4))))
  (define var5 (symbol-bound? 'var4))
  (list var0 var1 var2 var3 var4 var5))
(assert-equal? (tn)
               '(#f #f #f #f #f #f)
               (f #f))
(define (f var0)
  (define var1 (symbol-bound? 'var5))
  (define var2 (symbol-bound? 'var5))
  (begin
    (define var3 (symbol-bound? 'var5))
    (begin
      (define var4 (symbol-bound? 'var5))))
  (define var5 (symbol-bound? 'var5))
  (list var0 var1 var2 var3 var4 var5))
(assert-equal? (tn)
               '(#f #f #f #f #f #f)
               (f #f))
;; defining procedure can refer other (and self) variables as if letrec
(define (f var0)
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
(assert-equal? (tn)
               '(#t #t #t #t #t)
               (f (lambda () 0)))
(define (f)
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
        (eq? (var5) var1)))
(assert-equal? (tn)
               '(#t #t #t #t #t)
               (f))
(define (f)
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
        (eq? (var5) var2)))
(assert-equal? (tn)
               '(#t #t #t #t #t)
               (f))
(define (f)
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
        (eq? (var5) var4)))
(assert-equal? (tn)
               '(#t #t #t #t #t)
               (f))
(define (f)
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
        (eq? (var5) var5)))
(assert-equal? (tn)
               '(#t #t #t #t #t)
               (f))

(tn "func-form define internal definitions valid forms")
;; valid internal definitions
(define (f)
  (define var1 1)
  (list var1))
(assert-equal? (tn)
               '(1)
               (f))
(define (f)
  (define (proc1) 1)
  (list (proc1)))
(assert-equal? (tn)
               '(1)
               (f))
(define (f)
  (define var1 1)
  (define var2 2)
  (list var1 var2))
(assert-equal? (tn)
               '(1 2)
               (f))
(define (f)
  (define (proc1) 1)
  (define (proc2) 2)
  (list (proc1) (proc2)))
(assert-equal? (tn)
               '(1 2)
               (f))
(define (f)
  (define var1 1)
  (define (proc2) 2)
  (list var1 (proc2)))
(assert-equal? (tn)
               '(1 2)
               (f))
(define (f)
  (define (proc1) 1)
  (define var2 2)
  (list (proc1) var2))
(assert-equal? (tn)
               '(1 2)
               (f))
;; SigScheme accepts '(begin)' as valid internal definition '(begin
;; <definition>*)' as defined in "7.1.6 Programs and definitions" of R5RS
;; although it is rejected as expression '(begin <sequence>)' as defined in
;; "7.1.3 Expressions".
(define (f)
  (begin)
  1)
(assert-equal? (tn)
               1
               (f))
(define (f)
  (begin)
  (define var1 1)
  (begin)
  1)
(assert-equal? (tn)
               1
               (f))
(define (f)
  (begin
    (define var1 1))
  (list var1))
(assert-equal? (tn)
               '(1)
               (f))
(define (f)
  (begin
    (define (proc1) 1))
  (list (proc1)))
(assert-equal? (tn)
               '(1)
               (f))
(define (f)
  (begin
    (define var1 1)
    (define var2 2))
  (list var1 var2))
(assert-equal? (tn)
               '(1 2)
               (f))
(define (f)
  (begin
    (define (proc1) 1)
    (define (proc2) 2))
  (list (proc1) (proc2)))
(assert-equal? (tn)
               '(1 2)
               (f))
(define (f)
  (begin
    (define var1 1)
    (define (proc2) 2))
  (list var1 (proc2)))
(assert-equal? (tn)
               '(1 2)
               (f))
(define (f)
  (begin
    (define (proc1) 1)
    (define var2 2))
  (list (proc1) var2))
(assert-equal? (tn)
               '(1 2)
               (f))
(define (f)
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
        (proc5) var6))
(assert-equal? (tn)
               '(1 2 3 4 5 6)
               (f))
;; begin block and single definition mixed
(define (f)
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
        (proc5) var6))
(assert-equal? (tn)
               '(1 2 3 4 5 6)
               (f))

(tn "func-form define internal definitions invalid begin blocks")
(define (f)
  (begin
    (define var1 1)
    'val)
  (list var1))
(assert-error  (tn) (lambda () (f)))
(define (f)
  (begin
    (define (proc1) 1)
    'val)
  (list (proc1)))
(assert-error  (tn) (lambda () (f)))
(define (f)
  (begin
    (define var1 1)
    (define var2 2)
    'val)
  (list var1 var2))
(assert-error  (tn) (lambda () (f)))
(define (f)
  (begin
    (define (proc1) 1)
    (define (proc2) 2)
    'val)
  (list (proc1) (proc2)))
(assert-error  (tn) (lambda () (f)))
(define (f)
  (begin
    (define var1 1)
    (define (proc2) 2)
    'val)
  (list var1 (proc2)))
(assert-error  (tn) (lambda () (f)))
(define (f)
  (begin
    (define (proc1) 1)
    (define var2 2)
    'val)
  (list (proc1) var2))
(assert-error  (tn) (lambda () (f)))
(define (f)
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
        (proc5) var6))

(tn "func-form define internal definitions invalid placement")
;; a non-definition expression prior to internal definition is invalid
(assert-error  (tn) (lambda () (f)))
(define (f)
  'val
  (define var1 1))
(assert-error  (tn) (lambda () (f)))
(define (f)
  'val
  (define (proc1) 1))
(assert-error  (tn) (lambda () (f)))
(define (f)
  'val
  (define var1 1)
  (define var2 2))
(assert-error  (tn) (lambda () (f)))
(define (f)
  'val
  (define (proc1) 1)
  (define (proc2) 2))
(assert-error  (tn) (lambda () (f)))
(define (f)
  'val
  (define var1 1)
  (define (proc2) 2))
(assert-error  (tn) (lambda () (f)))
(define (f)
  'val
  (define (proc1) 1)
  (define var2 2))
(assert-error  (tn) (lambda () (f)))
(define (f)
  'val
  (begin))
(assert-error  (tn) (lambda () (f)))
(define (f)
  'val
  (begin
    (define var1 1)))
(assert-error  (tn) (lambda () (f)))
(define (f)
  'val
  (begin
    (define (proc1) 1)))
(assert-error  (tn) (lambda () (f)))
(define (f)
  'val
  (begin
    (define var1 1)
    (define var2 2)))
(assert-error  (tn) (lambda () (f)))
(define (f)
  'val
  (begin
    (define (proc1) 1)
    (define (proc2) 2)))
(assert-error  (tn) (lambda () (f)))
(define (f)
  'val
  (begin
    (define var1 1)
    (define (proc2) 2)))
(assert-error  (tn) (lambda () (f)))
(define (f)
  'val
  (begin
    (define (proc1) 1)
    (define var2 2)))
(assert-error  (tn) (lambda () (f)))
(define (f)
  'val
  (begin
    (define (proc1) 1)
    (define var2 2)
    (begin
      (define (proc3) 3)
      (define var4 4)
      (begin
        (define (proc5) 5)
        (define var6 6)))))
(assert-error  (tn) (lambda () (f)))
(define (f)
  (begin
    (define (proc1) 1)
    (define var2 2)
    'val
    (begin
      (define (proc3) 3)
      (define var4 4)
      (begin
        (define (proc5) 5)
        (define var6 6)))))
(assert-error  (tn) (lambda () (f)))
;; a non-definition expression prior to internal definition is invalid even if
;; expression(s) is following the internal definition
(define (f)
  'val
  (define var1 1)
  'val)
(assert-error  (tn) (lambda () (f)))
(define (f)
  'val
  (define (proc1) 1)
  'val)
(assert-error  (tn) (lambda () (f)))
(define (f)
  'val
  (define var1 1)
  (define var2 2)
  'val)
(assert-error  (tn) (lambda () (f)))
(define (f)
  'val
  (define (proc1) 1)
  (define (proc2) 2)
  'val)
(assert-error  (tn) (lambda () (f)))
(define (f)
  'val
  (define var1 1)
  (define (proc2) 2)
  'val)
(assert-error  (tn) (lambda () (f)))
(define (f)
  'val
  (define (proc1) 1)
  (define var2 2)
  'val)
(assert-error  (tn) (lambda () (f)))
(define (f)
  'val
  (begin)
  'val)
(assert-error  (tn) (lambda () (f)))
(define (f)
  'val
  (begin
    (define var1 1))
  'val)
(assert-error  (tn) (lambda () (f)))
(define (f)
  'val
  (begin
    (define (proc1) 1))
  'val)
(assert-error  (tn) (lambda () (f)))
(define (f)
  'val
  (begin
    (define var1 1)
    (define var2 2))
  'val)
(assert-error  (tn) (lambda () (f)))
(define (f)
  'val
  (begin
    (define (proc1) 1)
    (define (proc2) 2))
  'val)
(assert-error  (tn) (lambda () (f)))
(define (f)
  'val
  (begin
    (define var1 1)
    (define (proc2) 2))
  'val)
(assert-error  (tn) (lambda () (f)))
(define (f)
  'val
  (begin
    (define (proc1) 1)
    (define var2 2))
  'val)
(assert-error  (tn) (lambda () (f)))
(define (f)
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
        (proc5) var6))
(assert-error  (tn) (lambda () (f)))


(total-report)
